# Generación de la tesis en PDF

## Requisitos

- R con paquetes: `rmarkdown`, `knitr`, `openxlsx`, y las dependencias de `coding.R` (p. ej. `tidyverse`, `forecast`, etc.).
- LaTeX instalado (MiKTeX, TeX Live o TinyTeX) para compilar el PDF.

### Si aparece el error «"pdflatex" not found»

R Markdown necesita el ejecutable `pdflatex` para generar el PDF. Si no tienes LaTeX instalado (o no está en el PATH), instala **TinyTeX** desde R:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

Cierra y vuelve a abrir RStudio, luego vuelve a hacer Knit. Si faltan paquetes LaTeX durante la compilación, TinyTeX los instalará al vuelo; si no, ejecuta:

```r
tinytex::tlmgr_install(c("babel-spanish", "booktabs", "natbib"))  # según los avisos
```

## Cómo generar el PDF

**Para que las tablas del Capítulo 4 y del Anexo muestren datos reales**, debe existir `Tabla.Resultados.xlsx` en el directorio del proyecto. Ese archivo se genera ejecutando el script `coding.R` (requiere los datos de entrada en formato .xls según indica el script). Si el archivo no existe, el PDF se compila pero las tablas de resultados pueden aparecer vacías.

1. **Desde RStudio**: abra `thesis.Rmd` y pulse **Knit** (o `Ctrl+Shift+K`). Si tiene configurado el proyecto para ejecutar `coding.R` antes del Knit, se generará `Tabla.Resultados.xlsx` y las tablas del capítulo 4 se poblarán con los resultados. En caso contrario, ejecute primero `source("coding.R")` desde R en la carpeta del proyecto, luego Knit.

2. **Sin re-ejecutar el análisis**: en el primer chunk de `thesis.Rmd` cambie `RUN_ANALYSIS <- TRUE` por `RUN_ANALYSIS <- FALSE`. Asegúrese de tener ya generado `Tabla.Resultados.xlsx` en el mismo directorio (ejecutando `coding.R` una vez). El PDF se generará usando esos datos.

3. **Desde R (consola)**:
   ```r
   setwd("ruta/a/econometria")   # carpeta donde están thesis.Rmd y coding.R
   rmarkdown::render("thesis.Rmd")
   ```
   El PDF quedará en la misma carpeta: `thesis.pdf`.

4. **Citas y referencias (evitar los "?")**: Tras el Knit, el PDF puede mostrar "(?)" en las citas y en "Tabla X" porque LaTeX aún no ha ejecutado BibTeX. El Knit copia `Thesis_LaTeX/references.bib` a `references.bib` en la carpeta del proyecto. Desde esa misma carpeta (donde está `thesis.tex`), ejecute **una vez** la secuencia completa:
   ```r
   setwd("ruta/a/econometria")   # la carpeta de thesis.tex
   tinytex::latexmk("thesis.tex")
   ```
   O en la terminal (en la carpeta del proyecto):
   ```bash
   pdflatex thesis && bibtex thesis && pdflatex thesis && pdflatex thesis
   ```
   Vuelva a abrir `thesis.pdf`: las citas (Autor, año) y la referencia "Tabla X" deberían verse correctamente, sin "(?)".

## Salida HTML vs PDF

- **PDF**: formato principal de la tesis (portada, resumen, índice, referencias cruzadas y citas completas). Use *Knit → PDF* o `rmarkdown::render("thesis.Rmd", output_format = "pdf_document")`.
- **HTML**: versión legible en navegador; el resumen y los capítulos se sanean para mostrar títulos, negritas y referencias de forma aproximada. Las citas aparecen como claves (@autor2020). Para la versión definitiva use PDF.

## Estructura

- `thesis.Rmd`: documento principal que combina LaTeX y R.
- `coding.R`: script de análisis (modelos, métricas, export a Excel).
- `Thesis_LaTeX/`: capítulos en `.tex`, `references.bib` y `preamble.tex`.
- `Tabla.Resultados.xlsx`: generado por `coding.R`; las tablas del capítulo 4 se rellenan desde aquí.
