# Bonifaz-INCMNSZ — versión Python (Shiny for Python)

Port de la aplicación desde R (Shiny) a Python (Shiny for Python). Se eligió
**Shiny for Python** porque conserva casi la misma estructura reactiva que la
versión en R (pasos, estado reactivo, observadores, UI dinámica), así que el
port es prácticamente 1:1 y no obliga a reaprender otro framework.

## Cómo ejecutar

1. (Recomendado) crea un entorno virtual:
   ```bash
   python -m venv .venv
   # Windows:
   .venv\Scripts\activate
   # macOS / Linux:
   source .venv/bin/activate
   ```
2. Instala las dependencias:
   ```bash
   pip install -r requirements.txt
   ```
3. Ejecuta la app:
   ```bash
   shiny run --reload app.py
   ```
4. Abre en el navegador la dirección que aparece en la terminal
   (por defecto http://127.0.0.1:8000).

## Qué incluye esta base (ya funcionando)

- El asistente de **7 pasos** con barra de progreso y navegación
  (Atrás / Siguiente), con la misma lógica de estado que la versión R.
- **Carga de datos**: set de ejemplo (artritis reumatoide en ratones) y subir
  archivo propio (CSV o Excel), con opciones de separador y decimal para CSV.
- **Vista previa** y resumen por variable.

Los pasos 4-7 (limpieza, análisis, gráficas y descargas) están como secciones
"por portar": la estructura ya está lista para recibirlas.

## Mapeo de conceptos R → Python

| R (Shiny)                        | Python (Shiny for Python)              |
|----------------------------------|----------------------------------------|
| `reactiveValues()`               | `reactive.value(...)`                  |
| `observeEvent(input$x, {...})`   | `@reactive.effect` + `@reactive.event(input.x)` |
| `renderUI` / `uiOutput`          | `@render.ui` / `ui.output_ui`          |
| `renderTable` / `DTOutput`       | `@render.data_frame` / `ui.output_data_frame` |
| `renderPlot`                     | `@render.plot`                         |
| `conditionalPanel`               | `ui.panel_conditional`                 |
| `downloadHandler`                | `@render.download`                     |
| data.frame                       | `pandas.DataFrame`                     |
| ggplot2                          | `plotnine` (misma gramática de gráficas) |
| `t.test`, `aov`, `cor.test`      | `scipy.stats`, `statsmodels`           |
| `survival` (Kaplan-Meier)        | `lifelines`                            |

## Hoja de ruta del port (siguientes fases)

1. **Paso 4 — Limpieza**: NA, duplicados, columnas vacías, estandarizar texto,
   texto→número, redondeo, filtro por rango, detección de outliers (IQR),
   limpiar nombres de columnas.
2. **Paso 5 — Análisis**: descriptivos, comparación de grupos (t / ANOVA /
   Mann-Whitney / Kruskal-Wallis), pareado, d de Cohen, correlación y matriz,
   frecuencias, contingencia (chi²/Fisher), ANOVA de dos vías, modelo mixto,
   regresión lineal y logística, normalidad (Shapiro-Wilk).
3. **Paso 6 — Gráficas** con plotnine: histograma, boxplot, violín, dispersión,
   barras, barras con EE, mapa de calor, línea de tiempo, spaghetti,
   Kaplan-Meier.
4. **Paso 7 — Descargas**: datos (CSV/Excel), reporte PDF (reportlab) y bitácora.
5. **Importación avanzada** de Excel con encabezados en varias filas.

Cada fase se irá agregando y probando de forma incremental, igual que se hizo
en la versión R.
