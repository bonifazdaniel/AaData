# Bonifaz-INCMNSZ
### Asistente de análisis de datos paso a paso (R Shiny)

Aplicación pensada para investigadores del **Instituto Nacional de Ciencias
Médicas y Nutrición Salvador Zubirán** que no tienen experiencia programando
en R. La aplicación guía al usuario, pregunta tras pregunta, para:

1. Cargar un archivo de datos (CSV o Excel) — o usar un set de datos de
   ejemplo de un estudio de **artritis reumatoide en un modelo murino**.
2. Revisar una vista previa y un resumen de los datos.
3. Limpiar los datos (quitar datos faltantes, duplicados, columnas vacías, etc.).
4. Elegir y correr un análisis estadístico (descriptivos, comparación de
   grupos, correlación, frecuencias, o evolución en el tiempo), con los
   resultados explicados en lenguaje sencillo.
5. Generar gráficas (histograma, boxplot, violín, dispersión, barras,
   barras con error estándar, mapa de calor, línea de tiempo, líneas
   individuales por sujeto, y curva de supervivencia Kaplan-Meier) sin
   escribir código.
6. Descargar los datos trabajados, un **reporte completo en PDF** (con el
   resumen de los datos, los resultados de cada análisis ejecutado, las
   gráficas generadas, y la bitácora de la sesión) y la bitácora en texto.

## Requisitos

- [R](https://cran.r-project.org) (versión 4.0 o superior recomendada)
- Opcional pero recomendado: [RStudio](https://posit.co/download/rstudio-desktop/)
- Conexión a internet la primera vez que se ejecuta (para instalar paquetes)

## Cómo ejecutar la aplicación

**Opción 1 — Con RStudio (más fácil):**
1. Abre el archivo `app.R` en RStudio.
2. Da clic en el botón **"Run App"** (arriba a la derecha del editor).

**Opción 2 — Desde la consola de R:**
```r
shiny::runApp("app.R")
```

La primera vez que se ejecute, el script instalará automáticamente los
paquetes necesarios: `shiny`, `DT`, `ggplot2`, `dplyr`, `tidyr`, `readxl`,
`writexl`, `gridExtra` (para el reporte en PDF) y `survival` (para la
curva de Kaplan-Meier). Esto puede tardar unos minutos.

## Formato recomendado de tus datos

- Un archivo CSV o Excel donde cada **columna es una variable** (por ejemplo:
  `Grupo`, `Dia`, `Puntaje_clinico`, `Peso_g`, `IL6_pg_mL`) y cada **fila es
  una observación** (por ejemplo, un ratón en un día determinado).
- Si tu archivo CSV proviene de Excel en español, es común que use **punto y
  coma (;)** como separador de columnas y **coma (,)** como separador
  decimal — la aplicación te permite seleccionar esto en el paso de carga.
- Si los acentos se ven mal al cargar el archivo, cambia la codificación a
  "Latin1" en el paso de carga.

## Datos de ejemplo incluidos

La aplicación incluye un botón para generar un conjunto de datos simulado
(reproducible) de un estudio con 3 grupos de ratones — **Control**, **AR**
(artritis reumatoide inducida) y **AR+Tratamiento** — medidos en los días
0, 7, 14, 21 y 28, con las variables:

- `Puntaje_clinico`: puntaje de severidad de artritis.
- `Peso_g`: peso corporal en gramos.
- `IL6_pg_mL`: nivel de la citocina IL-6 en plasma.
- `Dia_evento_artritis`: día (simulado) en que el ratón desarrolla artritis
  clínica manifiesta (o 28 si no la desarrolló).
- `Evento_artritis`: 1 si el evento ocurrió, 0 si el ratón quedó censurado.
  Estas dos últimas columnas sirven para probar la curva de supervivencia
  (Kaplan-Meier).

Este set de datos es útil para explorar todas las funciones de la
aplicación (comparación de grupos, correlación, evolución en el tiempo,
gráficas) sin necesidad de subir tu propio archivo.

## Sobre el reporte en PDF

En el paso 7 (Descargar) puedes generar un reporte en PDF con:

- Portada con fecha y tamaño de los datos analizados.
- Resumen estadístico de todas las variables.
- El resultado **más reciente** de cada tipo de análisis que hayas ejecutado
  en el paso 5 (descriptivo, comparación de grupos, correlación,
  frecuencias, evolución en el tiempo).
- La gráfica **más reciente de cada tipo** que hayas generado en el paso 6.
- La bitácora completa de la sesión.

Si quieres que el reporte incluya, por ejemplo, la comparación de dos
variables numéricas distintas, ejecuta cada análisis o gráfica que quieras
conservar justo antes de descargar el PDF (solo se guarda la versión más
reciente de cada tipo).

## Notas importantes

- Esta herramienta es un **apoyo** para agilizar análisis exploratorios y
  gráficas; no sustituye la asesoría de un bioestadístico, especialmente
  para el diseño experimental, el cálculo de tamaño de muestra, o el
  análisis de estudios complejos (medidas repetidas, modelos mixtos, etc.).
- Todos los cálculos y gráficas se generan localmente en tu computadora;
  ningún dato se envía a servidores externos.
