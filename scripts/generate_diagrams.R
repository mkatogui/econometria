library(grid)

# ============================================================
# FIGURA 1: REPRESENTACIÓN DE UNA NEURONA
# ============================================================
generate_fig1 <- function() {
  png("figures/figura1.png", width=800, height=450, res=120)
  grid.newpage()
  
  x_in <- 0.20; x_w <- 0.35; x_sum <- 0.55; x_act <- 0.75; x_out <- 0.9
  y_top <- 0.8; y_mid <- 0.5; y_bot <- 0.2
  
  # "Entrada" bracket
  grid.text("Entrada", x=0.08, y=y_mid, gp=gpar(fontsize=12))
  grid.lines(x=c(0.14, 0.14), y=c(y_top, y_bot), gp=gpar(lwd=1.5))
  grid.lines(x=c(0.13, 0.14), y=c(y_top, y_top), gp=gpar(lwd=1.5))
  grid.lines(x=c(0.13, 0.14), y=c(y_bot, y_bot), gp=gpar(lwd=1.5))
  grid.lines(x=c(0.12, 0.14), y=c(y_mid, y_mid), gp=gpar(lwd=1.5))
  
  # Inputs
  grid.text(expression(x[1]), x=x_in, y=y_top, gp=gpar(fontsize=14))
  grid.text(expression(x[2]), x=x_in, y=y_mid, gp=gpar(fontsize=14))
  grid.text("...", x=x_in, y=0.35, gp=gpar(fontsize=14))
  grid.text(expression(x[n]), x=x_in, y=y_bot, gp=gpar(fontsize=14))
  
  # Weights
  r_w <- 0.04
  for(y in c(y_top, y_mid, y_bot)) {
    grid.circle(x=x_w, y=y, r=r_w, gp=gpar(fill="white", lwd=1.5))
  }
  grid.text(expression(omega[1]), x=x_w, y=y_top, gp=gpar(fontsize=14))
  grid.text(expression(omega[2]), x=x_w, y=y_mid, gp=gpar(fontsize=14))
  grid.text(expression(omega[n]), x=x_w, y=y_bot, gp=gpar(fontsize=14))
  grid.text("Pesos", x=x_w, y=y_bot-0.1, gp=gpar(fontsize=12))
  
  # Arrows
  ar <- arrow(length=unit(0.1, "inches"), type="closed")
  grid.segments(x0=x_in+0.04, y0=y_top, x1=x_w-r_w, y1=y_top, arrow=ar)
  grid.segments(x0=x_in+0.04, y0=y_mid, x1=x_w-r_w, y1=y_mid, arrow=ar)
  grid.segments(x0=x_in+0.04, y0=y_bot, x1=x_w-r_w, y1=y_bot, arrow=ar)
  
  # Sum
  r_sum <- 0.12
  grid.circle(x=x_sum, y=y_mid, r=r_sum, gp=gpar(fill="white", lwd=2))
  grid.text(expression(Sigma), x=x_sum, y=y_mid, gp=gpar(fontsize=35))
  grid.text("Suma", x=x_sum, y=y_mid-0.18, gp=gpar(fontsize=12))
  
  grid.segments(x0=x_w+r_w, y0=y_top, x1=x_sum-0.09, y1=y_mid+0.07, arrow=ar)
  grid.segments(x0=x_w+r_w, y0=y_mid, x1=x_sum-r_sum, y1=y_mid, arrow=ar)
  grid.segments(x0=x_w+r_w, y0=y_bot, x1=x_sum-0.09, y1=y_mid-0.07, arrow=ar)
  
  # Bias
  grid.circle(x=x_sum, y=0.9, r=0.05, gp=gpar(fill="white", lwd=1.5))
  grid.text("b", x=x_sum, y=0.9, gp=gpar(fontsize=16, fontface="italic"))
  grid.segments(x0=x_sum, y0=0.85, x1=x_sum, y1=y_mid+r_sum, arrow=ar)
  
  # Activation
  grid.rect(x=x_act, y=y_mid, width=0.12, height=0.12, gp=gpar(fill="white", lwd=1.5))
  grid.text(expression(varphi("·")), x=x_act, y=y_mid, gp=gpar(fontsize=16))
  grid.segments(x0=x_sum+r_sum, y0=y_mid, x1=x_act-0.06, y1=y_mid, arrow=ar)
  
  # Output
  grid.text(expression(y), x=x_out, y=y_mid, gp=gpar(fontsize=18, fontface="italic"))
  grid.segments(x0=x_act+0.06, y0=y_mid, x1=x_out-0.04, y1=y_mid, arrow=ar)
  
  dev.off()
}

# ============================================================
# FIGURA 2: RED NEURONAL DE TRES CAPAS
# ============================================================
generate_fig2 <- function() {
  png("figures/figura2.png", width=800, height=450, res=120)
  grid.newpage()
  
  x_inp <- 0.4; x_hid <- 0.6; x_out <- 0.8
  y_in <- c(0.7, 0.5, 0.3); y_hid <- c(0.8, 0.6, 0.4, 0.2); y_res <- 0.5
  
  # Connections
  for (yi in y_in) for (yh in y_hid) grid.segments(x0=x_inp, y0=yi, x1=x_hid, y1=yh, gp=gpar(col="gray70"))
  for (yh in y_hid) grid.segments(x0=x_hid, y0=yh, x1=x_out, y1=y_res, gp=gpar(col="gray70"))
  
  # Nodes
  r_node <- 0.03
  for (yi in y_in) grid.circle(x=x_inp, y=yi, r=r_node, gp=gpar(fill="black"))
  for (yh in y_hid) grid.circle(x=x_hid, y=yh, r=r_node, gp=gpar(fill="black"))
  grid.circle(x=x_out, y=y_res, r=r_node, gp=gpar(fill="black"))
  
  # Labels
  grid.text("Capa de\nEntrada", x=x_inp, y=0.08, gp=gpar(fontsize=10))
  grid.text("Capa\nOculta", x=x_hid, y=0.08, gp=gpar(fontsize=10))
  grid.text("Capa de\nSalida", x=x_out, y=0.08, gp=gpar(fontsize=10))
  
  dev.off()
}

# ============================================================
# FIGURA 3: RED NEURONAL RECURRENTE (RNN)
# ============================================================
generate_fig3 <- function() {
  png("figures/figura3.png", width=800, height=400, res=120)
  grid.newpage()
  
  # RNN Folded
  grid.rect(x=0.2, y=0.5, width=0.1, height=0.1, gp=gpar(fill="white", lwd=2))
  grid.text("RNN", x=0.2, y=0.5, gp=gpar(fontface="bold"))
  grid.segments(x0=0.2, y0=0.3, x1=0.2, y1=0.45, arrow=arrow(length=unit(0.1, "inches"), type="closed"))
  grid.text(expression(x[t]), x=0.2, y=0.25)
  grid.segments(x0=0.2, y0=0.55, x1=0.2, y1=0.7, arrow=arrow(length=unit(0.1, "inches"), type="closed"))
  grid.text(expression(h[t]), x=0.2, y=0.75)
  
  # Loop arrow
  grid.bezier(x=c(0.25, 0.3, 0.3, 0.25), y=c(0.5, 0.55, 0.45, 0.5), arrow=arrow(length=unit(0.1, "inches"), type="closed"))
  
  grid.text("=", x=0.4, y=0.5, gp=gpar(fontsize=20))
  
  # Unfolded RNN
  steps <- c(0.55, 0.7, 0.85)
  for(i in 1:3) {
    grid.rect(x=steps[i], y=0.5, width=0.08, height=0.08, gp=gpar(fill="white", lwd=1.5))
    grid.text("A", x=steps[i], y=0.5)
    grid.segments(x0=steps[i], y0=0.35, x1=steps[i], y1=0.46, arrow=arrow(length=unit(0.08, "inches"), type="closed"))
    grid.text(paste0("x", i-1), x=steps[i], y=0.3)
    grid.segments(x0=steps[i], y0=0.54, x1=steps[i], y1=0.65, arrow=arrow(length=unit(0.08, "inches"), type="closed"))
    grid.text(paste0("h", i-1), x=steps[i], y=0.7)
    if(i < 3) grid.segments(x0=steps[i]+0.04, y0=0.5, x1=steps[i+1]-0.04, y1=0.5, arrow=arrow(length=unit(0.08, "inches"), type="closed"))
  }
  grid.text("...", x=0.95, y=0.5)
  
  dev.off()
}

# ============================================================
# FIGURA 4: CÉLULA LSTM
# ============================================================
generate_fig4 <- function() {
  png("figures/figura4.png", width=800, height=500, res=120)
  grid.newpage()
  
  # Box for LSTM Cell
  grid.rect(x=0.5, y=0.5, width=0.7, height=0.6, gp=gpar(fill="gray95", lwd=2))
  grid.text("LSTM Cell", x=0.8, y=0.75, gp=gpar(fontface="italic", col="gray50"))
  
  # State line (top)
  grid.lines(x=c(0.1, 0.9), y=c(0.7, 0.7), gp=gpar(lwd=2, col="blue"))
  grid.text(expression(c[t-1]), x=0.05, y=0.7, gp=gpar(col="blue"))
  grid.text(expression(c[t]), x=0.95, y=0.7, gp=gpar(col="blue"))
  
  # Hidden state line (bottom)
  grid.lines(x=c(0.1, 0.9), y=c(0.2, 0.2), gp=gpar(lwd=2, col="red"))
  grid.text(expression(h[t-1]), x=0.05, y=0.2, gp=gpar(col="red"))
  grid.text(expression(h[t]), x=0.95, y=0.2, gp=gpar(col="red"))
  
  # Gates (Simplified)
  grid.rect(x=0.3, y=0.4, width=0.08, height=0.08, gp=gpar(fill="white"))
  grid.text("forget", x=0.3, y=0.4, gp=gpar(fontsize=8))
  
  grid.rect(x=0.5, y=0.4, width=0.08, height=0.08, gp=gpar(fill="white"))
  grid.text("input", x=0.5, y=0.4, gp=gpar(fontsize=8))
  
  grid.rect(x=0.7, y=0.4, width=0.08, height=0.08, gp=gpar(fill="white"))
  grid.text("output", x=0.7, y=0.4, gp=gpar(fontsize=8))
  
  # Input vector
  grid.segments(x0=0.5, y0=0.05, x1=0.5, y1=0.15, arrow=arrow(length=unit(0.1, "inches"), type="closed"))
  grid.text(expression(x[t]), x=0.5, y=0.02)
  
  dev.off()
}

# Ejecutar todas
dir.create("figures", showWarnings = FALSE)
generate_fig1()
generate_fig2()
generate_fig3()
generate_fig4()

cat("Todas las figuras han sido regeneradas en la carpeta 'figures/'.\n")
