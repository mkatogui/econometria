# ============================================================
# Liquidación de Divisas - Multi-Horizon Forecasting
# SARIMA, AR(1), ETS, Theta, NNETAR, LSTM, Combination
# + Diebold-Mariano tests
# ============================================================

# --- Configuration ---
# Reducir mensajes de TensorFlow al cargar LSTM (opcional)
Sys.setenv(TF_CPP_MIN_LOG_LEVEL = "3")  # 0=all, 1=no INFO, 2=no WARNING, 3=no ERROR

SERIES_START  <- c(2002, 6)
WINDOW_SIZE   <- 179
N_EVAL        <- 47
MAX_HORIZON   <- 12
SEED          <- 1988

TRAIN_RATIO   <- 0.75
N_CV_FOLDS    <- 3
EPOCHS_TUNE   <- 100
EPOCHS_FINAL  <- 200
ES_PATIENCE   <- 10

set.seed(SEED)
# Nota: La reproducibilidad es clave. Se asegura el uso de set.seed() antes de cada proceso estocástico.

# --- Libraries ---
library(tidyverse)
library(forecast)
library(tseries)


# ============================================================
# HELPER FUNCTIONS
# ============================================================

#' Multi-horizon rolling window evaluation.
#' Slides a fixed-size window and produces h-step-ahead forecasts.
#' Returns an N_EVAL x MAX_HORIZON matrix of predictions.
rolling_multi_horizon <- function(ts_data, window_size, n_eval, max_h,
                                  fit_predict_fn, label = "") {
  results <- matrix(NA_real_, nrow = n_eval, ncol = max_h)
  for (i in seq_len(n_eval)) {
    x <- ts(ts_data[i:(i + window_size - 1)], frequency = 12)
    results[i, ] <- tryCatch(
      fit_predict_fn(x, max_h),
      error = function(e) rep(NA_real_, max_h)
    )
    if (i %% 10 == 0 || i == n_eval) {
      message(sprintf("  [%s] %d/%d", label, i, n_eval))
    }
  }
  results
}

#' Build the matrix of actual values aligned with the rolling windows.
build_actuals_matrix <- function(ts_data, window_size, n_eval, max_h) {
  mat <- matrix(NA_real_, nrow = n_eval, ncol = max_h)
  for (i in seq_len(n_eval)) {
    idx_start <- i + window_size
    idx_end   <- idx_start + max_h - 1
    if (idx_end <= length(ts_data)) {
      mat[i, ] <- ts_data[idx_start:idx_end]
    }
  }
  mat
}

#' Compute RMSE, MAE, MAPE, SMAPE, MASE and Hit Rate per horizon for a single model.
#' MASE uses the naive seasonal (m=12) in-sample MAE as the scaling factor,
#' computed per rolling window and averaged across windows.
#' Hit Rate measures the percentage of correct directional predictions (up/down).
compute_metrics <- function(actual_mat, pred_mat, model_name,
                            ts_data = NULL, window_size = NULL) {
  n_h  <- ncol(actual_mat)
  n_w  <- nrow(actual_mat)
  m    <- 12L
  out  <- data.frame(
    Modelo    = model_name,
    Horizonte = paste0(1:n_h, "m"),
    RMSE = NA_real_, MAE = NA_real_,
    MAPE = NA_real_, SMAPE = NA_real_, MASE = NA_real_,
    HitRate = NA_real_,
    stringsAsFactors = FALSE
  )

  # Pre-compute per-window naive seasonal MAE (denominator for MASE)
  naive_mae <- rep(NA_real_, n_w)
  if (!is.null(ts_data) && !is.null(window_size)) {
    for (i in seq_len(n_w)) {
      win <- ts_data[i:(i + window_size - 1)]
      # In-sample naive seasonal errors: y_t - y_{t-m} for t > m
      if (length(win) > m) {
        naive_err <- abs(win[(m + 1):length(win)] - win[1:(length(win) - m)])
        naive_mae[i] <- mean(naive_err)
      }
    }
  }

  for (h in seq_len(n_h)) {
    a <- actual_mat[, h]; p <- pred_mat[, h]
    ok <- !is.na(a) & !is.na(p)
    if (sum(ok) < 2) next
    a <- a[ok]; p <- p[ok]; e <- a - p
    out$RMSE[h]  <- sqrt(mean(e^2))
    out$MAE[h]   <- mean(abs(e))
    out$MAPE[h]  <- mean(abs(e / a)) * 100
    out$SMAPE[h] <- mean(2 * abs(e) / (abs(a) + abs(p))) * 100

    # --- Hit Rate (Directional accuracy) ---
    # We compare the sign of the change from the last known value in the window
    if (!is.null(ts_data) && !is.null(window_size)) {
      last_vals <- sapply(seq_along(ok)[ok], function(i) ts_data[i + window_size - 1])
      actual_change <- a - last_vals
      pred_change   <- p - last_vals
      out$HitRate[h] <- mean(sign(actual_change) == sign(pred_change), na.rm = TRUE) * 100
    }

    # MASE: average of per-window scaled errors
    nm <- naive_mae[ok]
    ok_mase <- !is.na(nm) & nm > 0
    if (sum(ok_mase) >= 2) {
      out$MASE[h] <- mean(abs(e[ok_mase]) / nm[ok_mase])
    }
  }
  out
}

#' Format ARIMA order as readable string.
format_order <- function(ord) {
  if (length(ord) >= 6) {
    sprintf("(%d,%d,%d)(%d,%d,%d)[12]",
            ord[1], ord[2], ord[3], ord[4], ord[5], ord[6])
  } else {
    sprintf("(%d,%d,%d)", ord[1], ord[2], ord[3])
  }
}


# ============================================================
# 1. DATA LOADING & CLEANING
# ============================================================

filenames <- list.files(pattern = "\\.xls$")
stopifnot("No .xls file found" = length(filenames) >= 1)

df <- readxl::read_xls(filenames[1], sheet = 2)
df <- df[, colSums(is.na(df)) < nrow(df)]
df <- df[rowSums(is.na(df)) < ncol(df), ]
df <- df[4:15, ]
colnames(df) <- c("month", 2002:2022)
df$month <- 1:12

df_long <- pivot_longer(df, !month, names_to = "year", values_to = "liquidacion") %>%
  arrange(year, month) %>%
  filter(!is.na(liquidacion))

ts_liq <- ts(df_long$liquidacion / 1e6, start = SERIES_START, frequency = 12)

cat("=== Serie temporal ===\n")
cat(sprintf("  Observaciones: %d | Inicio: %d-%02d | Fin: %d-%02d\n",
            length(ts_liq),
            start(ts_liq)[1], start(ts_liq)[2],
            end(ts_liq)[1], end(ts_liq)[2]))


# ============================================================
# 2. EXPLORATION & STATIONARITY
# ============================================================

cat("\n=== ADF Test: Serie en Niveles ===\n")
print(adf.test(ts_liq))

plot(ts_liq, ylab = "Millones de U$S",
     main = "Liquidación de divisas informadas")
abline(v = time(ts_liq)[which.max(ts_liq)], col = "red", lty = 2)

diff_log_liq <- diff(log(ts_liq), 6)
cat("\n=== ADF Test: Diferencia Logarítmica (lag 6) ===\n")
print(adf.test(diff_log_liq))
ts.plot(diff_log_liq, main = "Diferencia logarítmica (lag 6)")

decomp <- decompose(ts_liq)
plot(decomp)

# --- Agricultural price index regression (if second file exists) ---
if (length(filenames) >= 2) {
  tryCatch({
    cat("\n=== Índice de Precios Agrícola vs Tendencia ===\n")
    indice_precios <- readxl::read_xls(filenames[2], sheet = 2,
      col_types = c("date", "numeric", "numeric", "numeric", "numeric"))
    indice_agro <- as.numeric(indice_precios[[3]][73:(nrow(indice_precios) - 6)])
    tendencia   <- as.numeric(decomp$trend[8:(length(decomp$trend) - 6)])

    ok <- !is.na(tendencia) & !is.na(indice_agro[seq_along(tendencia)])
    if (sum(ok) > 10) {
      data_reg <- data.frame(
        indice    = indice_agro[seq_along(tendencia)][ok],
        tendencia = tendencia[ok]
      )
      reg <- lm(log(tendencia) ~ log(indice), data = data_reg)
      cat("Regresión log-log: tendencia ~ índice agrícola\n")
      print(summary(reg))
    }
  }, error = function(e) {
    cat("  No se pudo cargar el índice de precios:", conditionMessage(e), "\n")
  })
}


# ============================================================
# 3. MODEL SELECTION & DIAGNOSTICS (full sample)
# ============================================================

sarima_full <- auto.arima(ts_liq, approximation = FALSE, stepwise = FALSE)
cat("\n=== SARIMA (búsqueda exhaustiva) ===\n")
summary(sarima_full)
checkresiduals(sarima_full)
ord_sarima <- arimaorder(sarima_full)

ets_full <- ets(ts_liq)
cat("\n=== ETS ===\n")
summary(ets_full)
checkresiduals(ets_full)

nnetar_full <- nnetar(ts_liq)
cat("\n=== NNETAR ===\n")
print(nnetar_full)

par(mfrow = c(2, 2))
plot(forecast(sarima_full, MAX_HORIZON), main = paste("SARIMA", format_order(ord_sarima)))
plot(forecast(ets_full, MAX_HORIZON), main = paste("ETS:", ets_full$method))
plot(forecast(nnetar_full, MAX_HORIZON), main = "NNETAR")
plot(thetaf(ts_liq, MAX_HORIZON), main = "Theta")
par(mfrow = c(1, 1))


# ============================================================
# 4. MULTI-HORIZON ROLLING EVALUATION (47 windows × 12 horizons)
# ============================================================

actuals_mat <- build_actuals_matrix(ts_liq, WINDOW_SIZE, N_EVAL, MAX_HORIZON)

# --- SARIMA (fixed order from exhaustive auto.arima) ---
message(sprintf("\n>>> SARIMA%s", format_order(ord_sarima)))
pred_sarima <- rolling_multi_horizon(
  ts_liq, WINDOW_SIZE, N_EVAL, MAX_HORIZON, label = "SARIMA",
  fit_predict_fn = function(x, h) {
    tryCatch({
      mod <- Arima(x, order = ord_sarima[1:3],
                   seasonal = list(order = ord_sarima[4:6], period = 12))
      as.numeric(forecast(mod, h)$mean)
    }, error = function(e) as.numeric(forecast(auto.arima(x), h)$mean))
  }
)

# --- AR(1) benchmark ---
message("\n>>> AR(1)")
pred_ar1 <- rolling_multi_horizon(
  ts_liq, WINDOW_SIZE, N_EVAL, MAX_HORIZON, label = "AR(1)",
  fit_predict_fn = function(x, h) {
    as.numeric(forecast(Arima(x, order = c(1, 0, 0)), h)$mean)
  }
)

# --- ETS ---
message("\n>>> ETS")
pred_ets <- rolling_multi_horizon(
  ts_liq, WINDOW_SIZE, N_EVAL, MAX_HORIZON, label = "ETS",
  fit_predict_fn = function(x, h) {
    tryCatch(
      as.numeric(forecast(ets(x), h)$mean),
      error = function(e) rep(NA_real_, h)
    )
  }
)

# --- Theta (M3 competition winner) ---
message("\n>>> Theta")
pred_theta <- rolling_multi_horizon(
  ts_liq, WINDOW_SIZE, N_EVAL, MAX_HORIZON, label = "Theta",
  fit_predict_fn = function(x, h) {
    as.numeric(thetaf(x, h)$mean)
  }
)

# --- NNETAR (neural network autoregression, pure R) ---
message("\n>>> NNETAR")
pred_nnetar <- rolling_multi_horizon(
  ts_liq, WINDOW_SIZE, N_EVAL, MAX_HORIZON, label = "NNETAR",
  fit_predict_fn = function(x, h) {
    tryCatch(
      as.numeric(forecast(nnetar(x), h)$mean),
      error = function(e) rep(NA_real_, h)
    )
  }
)


# ============================================================
# 5. LSTM: HYPERPARAMETER TUNING + FORECASTING
# ============================================================
# Grid search over (units, lookback, dropout, learning_rate)
# using expanding-window time-series CV (N_CV_FOLDS folds).
# Each fold uses fold-specific min-max scaling to prevent leakage.
# Early stopping (patience = ES_PATIENCE) avoids over-training.
# Best configuration is retrained on the full training portion
# and used for the 47 rolling-window multi-horizon predictions.

tuning_results <- NULL

lstm_ok <- tryCatch({
  library(reticulate)

  tf    <- import("tensorflow")
  tf$get_logger()$setLevel("ERROR")
  keras <- tf$keras
  np    <- import("numpy")
  tf$random$set_seed(as.integer(SEED))

  diffed    <- diff(ts_liq, differences = 1)
  n_diff    <- length(diffed)
  n_train_d <- round(n_diff * TRAIN_RATIO)

  # Global scaling anchored on training portion (used for final model)
  g_min     <- min(diffed[1:n_train_d])
  g_max     <- max(diffed[1:n_train_d])
  scale_v   <- function(v) 2 * (v - g_min) / (g_max - g_min) - 1
  unscale_v <- function(v) (v + 1) / 2 * (g_max - g_min) + g_min

  # ---- 5a. Hyperparameter grid ----
  hp_grid <- expand.grid(
    units    = c(4L, 8L, 16L, 32L),
    lookback = c(6L, 12L, 24L),
    dropout  = c(0.0, 0.1, 0.2),
    lr       = c(1e-3, 1e-4),
    stringsAsFactors = FALSE
  )

  cat(sprintf("\n%s\n", strrep("=", 65)))
  cat("  LSTM HYPERPARAMETER TUNING (Grid Search + TS-CV)\n")
  cat(sprintf("%s\n", strrep("=", 65)))
  cat(sprintf("  Grid size         : %d combinations\n", nrow(hp_grid)))
  cat(sprintf("  CV method          : Expanding window (%d folds)\n", N_CV_FOLDS))
  cat(sprintf("  CV horizon         : %d months (recursive)\n", MAX_HORIZON))
  cat(sprintf("  Max epochs (tune)  : %d (early stopping, patience %d)\n",
              EPOCHS_TUNE, ES_PATIENCE))
  cat(sprintf("  Max epochs (final) : %d (early stopping, patience 15)\n\n",
              EPOCHS_FINAL))

  # ---- 5b. Expanding-window fold boundaries ----
  min_train_cv <- round(n_train_d * 0.50)
  max_train_cv <- n_train_d - MAX_HORIZON
  fold_ends    <- round(seq(min_train_cv, max_train_cv, length.out = N_CV_FOLDS))

  cat("  Fold structure (differenced series):\n")
  for (f in seq_len(N_CV_FOLDS)) {
    cat(sprintf("    Fold %d: train [1:%d]  →  validate [%d:%d]\n",
                f, fold_ends[f], fold_ends[f] + 1, fold_ends[f] + MAX_HORIZON))
  }
  cat("\n")

  # ---- 5c. CV scoring function ----
  # Returns mean MAE on the differenced series across folds.
  # Each fold: build model → train → recursive 12-step prediction → MAE.
  cv_score <- function(units, lookback, dropout, lr) {
    fold_maes <- numeric(N_CV_FOLDS)

    for (f in seq_len(N_CV_FOLDS)) {
      train_end <- fold_ends[f]
      val_end   <- min(train_end + MAX_HORIZON, n_train_d)
      fold_train <- diffed[1:train_end]
      fold_val   <- diffed[(train_end + 1):val_end]

      # Fold-specific scaling (prevents leakage from validation data)
      f_min     <- min(fold_train)
      f_max     <- max(fold_train)
      f_scale   <- function(v) 2 * (v - f_min) / (f_max - f_min) - 1
      f_unscale <- function(v) (v + 1) / 2 * (f_max - f_min) + f_min

      n_samples <- length(fold_train) - lookback
      if (n_samples < 10) { fold_maes[f] <- Inf; next }

      X <- matrix(0, nrow = n_samples, ncol = lookback)
      y <- numeric(n_samples)
      for (t in seq_len(n_samples)) {
        X[t, ] <- fold_train[t:(t + lookback - 1)]
        y[t]   <- fold_train[t + lookback]
      }

      X_s <- f_scale(X)
      y_s <- f_scale(y)
      dim(X_s) <- c(nrow(X_s), lookback, 1)

      mdl <- keras$Sequential(list(
        keras$layers$Input(shape = c(as.integer(lookback), 1L)),
        keras$layers$LSTM(as.integer(units), activation = "tanh"),
        keras$layers$Dropout(dropout),
        keras$layers$Dense(1L)
      ))
      mdl$compile(
        loss      = "mae",
        optimizer = keras$optimizers$Adam(learning_rate = lr)
      )

      es <- keras$callbacks$EarlyStopping(
        monitor              = "val_loss",
        patience             = as.integer(ES_PATIENCE),
        restore_best_weights = TRUE
      )
      mdl$fit(
        np$array(X_s), np$array(y_s),
        epochs           = as.integer(EPOCHS_TUNE),
        batch_size       = 16L,
        validation_split = 0.1,
        callbacks        = list(es),
        verbose          = 0L
      )

      # Recursive multi-step prediction on validation
      input_seq <- fold_train[(train_end - lookback + 1):train_end]
      preds     <- numeric(length(fold_val))
      for (h in seq_along(fold_val)) {
        x_in   <- np$array(array(f_scale(input_seq), dim = c(1, lookback, 1)))
        pred_s <- mdl$predict(x_in, verbose = 0L)
        pred_d <- f_unscale(as.numeric(pred_s[1, 1]))
        preds[h]  <- pred_d
        input_seq <- c(input_seq[-1], pred_d)
      }

      fold_maes[f] <- mean(abs(fold_val - preds))
      keras$backend$clear_session()
    }
    mean(fold_maes)
  }

  # ---- 5d. Run grid search ----
  t_start      <- Sys.time()
  hp_grid$cv_mae <- NA_real_
  best_mae     <- Inf

  for (row in seq_len(nrow(hp_grid))) {
    hp <- hp_grid[row, ]
    set.seed(SEED)
    tf$random$set_seed(as.integer(SEED))

    score <- tryCatch(
      cv_score(hp$units, hp$lookback, hp$dropout, hp$lr),
      error = function(e) Inf
    )
    hp_grid$cv_mae[row] <- round(score, 2)

    is_new_best <- score < best_mae
    if (is_new_best) best_mae <- score

    cat(sprintf("  [%3d/%d] units=%2d  lb=%2d  do=%.1f  lr=%.4f  →  MAE=%7.2f %s\n",
                row, nrow(hp_grid),
                hp$units, hp$lookback, hp$dropout, hp$lr, score,
                if (is_new_best) "***" else ""))
  }

  t_elapsed <- difftime(Sys.time(), t_start, units = "mins")

  # ---- 5e. Report best configuration ----
  best_idx <- which.min(hp_grid$cv_mae)
  best_hp  <- hp_grid[best_idx, ]

  cat(sprintf("\n%s\n", strrep("=", 65)))
  cat("  TUNING RESULTS\n")
  cat(sprintf("%s\n", strrep("=", 65)))
  cat(sprintf("  Time elapsed : %.1f minutes\n", as.numeric(t_elapsed)))
  cat(sprintf("  Best config  : #%d of %d\n", best_idx, nrow(hp_grid)))
  cat(sprintf("    Units      : %d\n", best_hp$units))
  cat(sprintf("    Lookback   : %d months\n", best_hp$lookback))
  cat(sprintf("    Dropout    : %.1f\n", best_hp$dropout))
  cat(sprintf("    LR         : %.4f\n", best_hp$lr))
  cat(sprintf("    CV MAE     : %.2f\n\n", best_hp$cv_mae))

  cat("  Top 10 Configurations:\n")
  top10 <- head(hp_grid[order(hp_grid$cv_mae), ], 10)
  print(top10, row.names = FALSE)
  cat("\n")

  tuning_results <<- hp_grid[order(hp_grid$cv_mae), ]

  # ---- 5f. Train final model with best hyperparameters ----
  BEST_UNITS    <- best_hp$units
  BEST_LOOKBACK <- best_hp$lookback
  BEST_DROPOUT  <- best_hp$dropout
  BEST_LR       <- best_hp$lr

  n_samples   <- n_train_d - BEST_LOOKBACK
  X_train     <- matrix(0, nrow = n_samples, ncol = BEST_LOOKBACK)
  y_train_vec <- numeric(n_samples)
  for (t in seq_len(n_samples)) {
    X_train[t, ]   <- diffed[t:(t + BEST_LOOKBACK - 1)]
    y_train_vec[t] <- diffed[t + BEST_LOOKBACK]
  }

  X_train_s <- scale_v(X_train)
  y_train_s <- scale_v(y_train_vec)
  dim(X_train_s) <- c(nrow(X_train_s), BEST_LOOKBACK, 1)

  set.seed(SEED)
  tf$random$set_seed(as.integer(SEED))

  final_model <- keras$Sequential(list(
    keras$layers$Input(shape = c(as.integer(BEST_LOOKBACK), 1L)),
    keras$layers$LSTM(as.integer(BEST_UNITS), activation = "tanh"),
    keras$layers$Dropout(BEST_DROPOUT),
    keras$layers$Dense(1L)
  ))
  final_model$compile(
    loss      = "mae",
    optimizer = keras$optimizers$Adam(learning_rate = BEST_LR)
  )

  cat("=== Final LSTM Architecture (tuned) ===\n")
  final_model$summary()

  es_final <- keras$callbacks$EarlyStopping(
    monitor              = "val_loss",
    patience             = 15L,
    restore_best_weights = TRUE
  )

  message("\n>>> Training final LSTM with tuned hyperparameters...")
  final_model$fit(
    np$array(X_train_s), np$array(y_train_s),
    epochs           = as.integer(EPOCHS_FINAL),
    batch_size       = 16L,
    validation_split = 0.1,
    callbacks        = list(es_final),
    verbose          = 0L
  )
  message("  Final training completed.")

  # ---- 5g. Rolling-window multi-horizon predictions ----
  pred_lstm <- matrix(NA_real_, nrow = N_EVAL, ncol = MAX_HORIZON)

  for (i in seq_len(N_EVAL)) {
    window_end <- i + WINDOW_SIZE - 1
    diff_end   <- window_end - 1
    if (diff_end < BEST_LOOKBACK) next

    input_seq  <- diffed[(diff_end - BEST_LOOKBACK + 1):diff_end]
    last_level <- ts_liq[window_end]

    for (h in seq_len(MAX_HORIZON)) {
      x_in   <- np$array(array(scale_v(input_seq), dim = c(1, BEST_LOOKBACK, 1)))
      pred_s <- final_model$predict(x_in, verbose = 0L)
      pred_d <- unscale_v(as.numeric(pred_s[1, 1]))

      last_level      <- last_level + pred_d
      pred_lstm[i, h] <- last_level
      input_seq       <- c(input_seq[-1], pred_d)
    }
    if (i %% 10 == 0 || i == N_EVAL) {
      message(sprintf("  [LSTM pred] %d/%d", i, N_EVAL))
    }
  }

  TRUE
}, error = function(e) {
  cat("\n=== LSTM Omitido ===\n")
  cat("  Error:", conditionMessage(e), "\n")
  cat("  Para instalar: reticulate::py_install('tensorflow')\n")
  pred_lstm <<- matrix(NA_real_, nrow = N_EVAL, ncol = MAX_HORIZON)
  FALSE
})


# ============================================================
# 6. FORECAST COMBINATION & METRICS
# ============================================================

stat_models <- list(pred_sarima, pred_ar1, pred_ets, pred_theta, pred_nnetar)
pred_combo  <- Reduce("+", stat_models) / length(stat_models)

all_models <- list(
  "SARIMA"      = pred_sarima,
  "AR(1)"       = pred_ar1,
  "ETS"         = pred_ets,
  "Theta"       = pred_theta,
  "NNETAR"      = pred_nnetar,
  "Combinación" = pred_combo
)

if (lstm_ok) {
  all_models[["LSTM"]] <- pred_lstm
  all_models[["Combi+LSTM"]] <- Reduce("+", c(stat_models, list(pred_lstm))) /
                                 (length(stat_models) + 1)
}

# --- Per-horizon metrics ---
all_metrics <- do.call(rbind, lapply(names(all_models), function(nm) {
  compute_metrics(actuals_mat, all_models[[nm]], nm,
                  ts_data = ts_liq, window_size = WINDOW_SIZE)
}))

# --- Summary (average across horizons) ---
summary_metrics <- all_metrics %>%
  group_by(Modelo) %>%
  summarise(
    RMSE  = round(mean(RMSE, na.rm = TRUE), 2),
    MAE   = round(mean(MAE, na.rm = TRUE), 2),
    MAPE  = round(mean(MAPE, na.rm = TRUE), 2),
    SMAPE = round(mean(SMAPE, na.rm = TRUE), 2),
    MASE  = round(mean(MASE, na.rm = TRUE), 3),
    HitRate = round(mean(HitRate, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(MAPE)

cat("\n")
cat(strrep("=", 65), "\n")
cat("  RESUMEN: Métricas promedio (horizontes 1–12)\n")
cat(strrep("=", 65), "\n\n")
print(as.data.frame(summary_metrics), row.names = FALSE)
cat(sprintf("\n  Mejor modelo (MAPE promedio): %s (%.2f%%)\n",
            summary_metrics$Modelo[1], summary_metrics$MAPE[1]))

# --- MAPE detail per horizon ---
cat("\n=== MAPE por Horizonte ===\n")
mape_wide <- all_metrics %>%
  select(Modelo, Horizonte, MAPE) %>%
  mutate(MAPE = round(MAPE, 2)) %>%
  pivot_wider(names_from = Modelo, values_from = MAPE)
print(as.data.frame(mape_wide), row.names = FALSE)

# --- SMAPE detail per horizon ---
cat("\n=== SMAPE por Horizonte ===\n")
smape_wide <- all_metrics %>%
  select(Modelo, Horizonte, SMAPE) %>%
  mutate(SMAPE = round(SMAPE, 2)) %>%
  pivot_wider(names_from = Modelo, values_from = SMAPE)
print(as.data.frame(smape_wide), row.names = FALSE)

# --- MASE detail per horizon ---
cat("\n=== MASE por Horizonte (ref: naive seasonal m=12) ===\n")
cat("  MASE < 1 → mejor que naive estacional\n\n")
mase_wide <- all_metrics %>%
  select(Modelo, Horizonte, MASE) %>%
  mutate(MASE = round(MASE, 3)) %>%
  pivot_wider(names_from = Modelo, values_from = MASE)
print(as.data.frame(mase_wide), row.names = FALSE)

# --- Hit Rate detail per horizon ---
cat("\n=== Hit Rate por Horizonte (Precisión Direccional %) ===\n")
hit_wide <- all_metrics %>%
  select(Modelo, Horizonte, HitRate) %>%
  mutate(HitRate = round(HitRate, 2)) %>%
  pivot_wider(names_from = Modelo, values_from = HitRate)
print(as.data.frame(hit_wide), row.names = FALSE)


# ============================================================
# 7. DIEBOLD-MARIANO TESTS (per horizon, SARIMA as baseline)
# ============================================================

cat("\n=== Diebold-Mariano Tests (SARIMA como baseline) ===\n")
cat("H0: Igual capacidad predictiva | Significancia: 5%\n")
cat("Nota: dm.test() aplica la corrección de Harvey-Leybourne-Newbold (HLN) para muestras finitas.\n\n")

dm_all <- data.frame()
for (h in c(1, 3, 6, 12)) {
  sarima_err <- actuals_mat[, h] - pred_sarima[, h]

  for (nm in setdiff(names(all_models), "SARIMA")) {
    other_err <- actuals_mat[, h] - all_models[[nm]][, h]
    ok <- !is.na(sarima_err) & !is.na(other_err)
    if (sum(ok) < 10) next

    dm <- tryCatch(
      dm.test(sarima_err[ok], other_err[ok], alternative = "two.sided", h = 1),
      error = function(e) NULL
    )
    if (!is.null(dm)) {
      conclusion <- if (dm$p.value < 0.05) {
        if (dm$statistic > 0) paste(nm, "mejor") else "SARIMA mejor"
      } else "="
      dm_all <- rbind(dm_all, data.frame(
        Horizonte  = paste0(h, "m"),
        Comparación = paste("vs", nm),
        DM_stat    = round(dm$statistic, 3),
        p_value    = round(dm$p.value, 4),
        Resultado  = conclusion,
        stringsAsFactors = FALSE
      ))
    }
  }
}
print(dm_all, row.names = FALSE)


# ============================================================
# 8. VISUALIZATION
# ============================================================

# Sample of rolling windows
windows_to_plot <- c(1, 10, 20, 30, 40, 47)
leg_labels <- c("Actual", "SARIMA", "ETS", "Theta", "NNETAR",
                "Combinación", if (lstm_ok) "LSTM")
leg_cols   <- c("black", "blue", "red", "darkgreen", "grey50",
                "purple", if (lstm_ok) "orange")
leg_lty    <- c(1, 2, 2, 2, 2, 1, if (lstm_ok) 1)
leg_lwd    <- c(2, 1, 1, 1, 1, 2, if (lstm_ok) 2)

par(mfrow = c(2, 3), oma = c(0, 0, 0, 0))
for (idx in seq_along(windows_to_plot)) {
  w <- windows_to_plot[idx]
  y_range <- range(actuals_mat[w, ], pred_sarima[w, ], pred_ets[w, ],
                   pred_combo[w, ], na.rm = TRUE)
  if (lstm_ok) y_range <- range(y_range, pred_lstm[w, ], na.rm = TRUE)

  plot(1:MAX_HORIZON, actuals_mat[w, ], type = "l", lwd = 2,
       xlab = "Horizonte (meses)", ylab = "Millones USD",
       ylim = y_range, main = sprintf("Ventana %d", w))
  lines(1:MAX_HORIZON, pred_sarima[w, ], col = "blue", lty = 2)
  lines(1:MAX_HORIZON, pred_ets[w, ], col = "red", lty = 2)
  lines(1:MAX_HORIZON, pred_theta[w, ], col = "darkgreen", lty = 2)
  lines(1:MAX_HORIZON, pred_nnetar[w, ], col = "grey50", lty = 2)
  lines(1:MAX_HORIZON, pred_combo[w, ], col = "purple", lwd = 2)
  if (lstm_ok) lines(1:MAX_HORIZON, pred_lstm[w, ], col = "orange", lwd = 2)

  if (idx == 1) {
    legend("topleft", legend = leg_labels, col = leg_cols,
           lty = leg_lty, lwd = leg_lwd, cex = 0.6, bg = "white")
  }
}
par(mfrow = c(1, 1))

# MAPE by horizon — bar chart
mape_long <- all_metrics %>%
  mutate(h = as.integer(gsub("m", "", Horizonte)))
ggplot(mape_long, aes(x = h, y = MAPE, color = Modelo)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Horizonte (meses)", y = "MAPE (%)",
       title = "MAPE por horizonte de predicción") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")


# ============================================================
# 9. EXPORT RESULTS
# ============================================================

tryCatch({
  wb <- list(
    Resumen        = as.data.frame(summary_metrics),
    MAPE_Detalle   = as.data.frame(mape_wide),
    SMAPE_Detalle  = as.data.frame(smape_wide),
    MASE_Detalle   = as.data.frame(mase_wide),
    HitRate_Detalle = as.data.frame(hit_wide),
    DM_Tests       = dm_all,
    Metricas_Full  = as.data.frame(all_metrics)
  )
  if (!is.null(tuning_results)) {
    wb[["LSTM_Tuning"]] <- as.data.frame(tuning_results)
  }
  openxlsx::write.xlsx(wb, file = "Tabla.Resultados.xlsx", overwrite = TRUE)
  cat("\n  Resultados exportados a Tabla.Resultados.xlsx\n")
  if (!is.null(tuning_results)) {
    cat("  (incluye hoja LSTM_Tuning con las 72 configuraciones evaluadas)\n")
  }
}, error = function(e) {
  cat("\n  Export a Excel no disponible:", conditionMessage(e), "\n")
  cat("  Instalar: install.packages('openxlsx')\n")
})

cat("\n", strrep("=", 65), "\n")
cat("  ANÁLISIS COMPLETADO\n")
cat(strrep("=", 65), "\n")
