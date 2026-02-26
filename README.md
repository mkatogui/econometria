# Tesis: Predicción de la liquidación de divisas del sector agroexportador

Comparación de capacidad predictiva de modelos de series temporales (SARIMA, AR(1), ETS, Theta, NNETAR, LSTM) y combinaciones de pronósticos para la liquidación de divisas del sector agroexportador argentino. El documento se genera en **PDF** (formato principal) y **HTML** (lectura en navegador).

---

## Requisitos

- **R** con paquetes: `rmarkdown`, `knitr`, `openxlsx`, `readxl`, y las dependencias de `coding.R` (p. ej. `tidyverse`, `forecast`, etc.).
- **LaTeX** (MiKTeX, TeX Live o TinyTeX) para compilar el PDF.

### Si aparece el error «"pdflatex" not found»

R Markdown necesita `pdflatex` para generar el PDF. Instala TinyTeX desde R:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

Si faltan paquetes LaTeX durante la compilación:

```r
tinytex::tlmgr_install(c("babel-spanish", "booktabs", "natbib"))  # según los avisos
```

---

## Cómo generar el documento

### Datos y tablas de resultados

Las tablas del **Capítulo 4** y del **Anexo** (Resumen, MAPE, sMAPE, MASE, Diebold-Mariano, etc.) se rellenan desde el archivo `Tabla.Resultados.xlsx`. Ese archivo **no se genera automáticamente** al hacer Knit; debe crearse ejecutando el script de análisis:

```r
setwd("ruta/a/econometria")   # carpeta del proyecto
source("coding.R")
```

`coding.R` requiere los datos de entrada en formato `.xls` (según el script). Si **no** existe `Tabla.Resultados.xlsx`, el documento se compila igual, pero las tablas de resultados pueden quedar vacías.

### Generar el PDF

1. **Desde RStudio**: abra `thesis.Rmd`, elija salida **PDF** y pulse **Knit** (o `Ctrl+Shift+K`).
2. **Desde R (consola)**:
   ```r
   setwd("ruta/a/econometria")
   rmarkdown::render("thesis.Rmd", output_format = "pdf_document")
   ```
   El PDF se guarda en la misma carpeta: `thesis.pdf`.

### Citas y referencias en el PDF

Si en el PDF aparecen "(?)" en las citas o en "Tabla X", hay que ejecutar BibTeX. Con `keep_tex: true` se genera `thesis.tex`. Desde la carpeta del proyecto:

```r
tinytex::latexmk("thesis.tex")
```

O en la terminal:

```bash
pdflatex thesis && bibtex thesis && pdflatex thesis && pdflatex thesis
```

Luego vuelva a abrir `thesis.pdf`.

### Generar el HTML

1. **Desde RStudio**: abra `thesis.Rmd`, elija salida **HTML** y pulse **Knit**.
2. **Desde R**:
   ```r
   rmarkdown::render("thesis.Rmd", output_format = "html_document")
   ```

Se genera `thesis.html`. Para que se vea correctamente:

- La carpeta **`thesis_files/`** (Bootstrap, figuras generadas por R, etc.) debe estar junto a `thesis.html`.
- El archivo **`thesis-html.css`** debe estar en la misma carpeta (estilos propios del HTML).
- Las figuras 2.1–2.4 (redes neuronales) usan `figures/figura1.png` … `figures/figura4.png`. Si existe la carpeta **`figures/`** con esos archivos, las imágenes se mostrarán; si no, esas cuatro figuras aparecerán rotas en el HTML.

Al compartir el HTML, incluya `thesis.html`, `thesis_files/`, `thesis-html.css` y, si aplica, `figures/`.

---

## Salida PDF vs HTML

| Aspecto        | PDF                         | HTML                                      |
|----------------|-----------------------------|-------------------------------------------|
| Uso            | Versión principal, entrega | Lectura en navegador                      |
| Portada        | Título, autor, fecha        | Título, autor, fecha, abstract            |
| Estructura     | Secciones 1–6 + Anexo       | Igual; TOC flotante, secciones numeradas  |
| Tablas/figuras | Desde Excel y chunks R      | Igual; figuras en `thesis_files/` o `figures/` |
| Citas          | Estilo autor-año (BibTeX)   | Estilo autor-año (si se procesan)         |
| Math           | LaTeX nativo                | MathJax (requiere conexión para cargar)   |

---

## Estructura del proyecto

| Elemento | Descripción |
|----------|-------------|
| `thesis.Rmd` | Documento principal (Markdown + R). Única fuente para PDF y HTML. |
| `coding.R` | Script de análisis: estimación de modelos, métricas, test de Diebold-Mariano, export a Excel. |
| `Tabla.Resultados.xlsx` | Generado por `coding.R`. Alimenta las tablas del Capítulo 4 y del Anexo. |
| `thesis-html.css` | Hoja de estilos para el HTML (tipografía, ancho máximo, tablas, TOC). |
| `references.bib` | Base de datos bibliográfica para citas. |
| `thesis_files/` | Carpeta generada al compilar HTML (Bootstrap, jQuery, figuras R, etc.). |
| `figures/` | Opcional. Incluir `figura1.png` … `figura4.png` para las figuras del Marco teórico en HTML. |

---

## Estructura del documento (tesis)

1. **Introducción** — Pregunta de investigación, objetivo, contribución, roadmap.
2. **Marco teórico** — SARIMA/ARIMA, ETS, Theta, NNETAR, LSTM, métricas, test de Diebold-Mariano.
3. **Metodología y datos** — Fuente de datos (CIARA-CEC), ventana rodante (47 evaluaciones, 179 obs, 12 meses), benchmark naive, supuestos.
4. **Resultados y discusión** — Métricas por modelo, errores acumulados, test DM, robustez, literatura.
5. **Limitaciones** — Univariado, tamaño muestral, periodo, estabilidad estructural, benchmark.
6. **Conclusiones** — Síntesis y extensiones.
7. **Anexo** — Tablas detalladas (sMAPE, RMSE, etc.), figuras auxiliares, nota de reproducibilidad.

El **abstract** está definido en el YAML de `thesis.Rmd`; en PDF puede no mostrarse si la portada es personalizada; en HTML sí aparece bajo el título.
