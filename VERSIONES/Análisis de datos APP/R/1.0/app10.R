########################################################################
# Bonifaz-INCMNSZ
# Asistente de Análisis de Datos paso a paso (Shiny)
# Instituto Nacional de Ciencias Médicas y Nutrición Salvador Zubirán
#
# Objetivo: permitir que investigadores SIN experiencia en programación
# puedan cargar, limpiar, analizar y graficar sus datos respondiendo
# preguntas sencillas, sin escribir código.
#
# CÓMO EJECUTAR ESTA APLICACIÓN:
#   1. Instala R (https://cran.r-project.org) y, opcionalmente, RStudio.
#   2. Abre este archivo (app.R) en RStudio y da clic en "Run App"
#      (o ejecuta en la consola de R:  shiny::runApp("app.R") )
#   3. La primera vez, el script instalará automáticamente los paquetes
#      necesarios (requiere conexión a internet).
#
# Paquetes utilizados: shiny, DT, ggplot2, dplyr, tidyr, readxl, writexl,
#                      gridExtra (para armar el reporte en PDF)
########################################################################

## ---- 1. Verificación e instalación de paquetes necesarios ----------
paquetes_requeridos <- c("shiny", "DT", "ggplot2", "dplyr", "tidyr",
                          "readxl", "writexl", "gridExtra")

paquetes_faltantes <- paquetes_requeridos[
  !(paquetes_requeridos %in% rownames(installed.packages()))
]

if (length(paquetes_faltantes) > 0) {
  message("Instalando paquetes necesarios: ",
          paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes, repos = "https://cloud.r-project.org")
}

invisible(lapply(paquetes_requeridos, library, character.only = TRUE))
library(grid)  # paquete base de R (no requiere instalación), usado para armar el PDF

options(shiny.maxRequestSize = 30 * 1024^2)  # permite subir archivos de hasta 30 MB

## ---- 2. Funciones auxiliares ----------------------------------------

# Identifica columnas numéricas de un data.frame
obtener_vars_numericas <- function(df) {
  names(df)[sapply(df, is.numeric)]
}

# Identifica columnas categóricas (texto, factor, o numéricas con pocos
# valores distintos, útiles como variable de agrupación)
obtener_vars_categoricas <- function(df) {
  es_categorica <- sapply(df, function(x) {
    is.character(x) || is.factor(x) ||
      (is.numeric(x) && length(unique(na.omit(x))) <= 10)
  })
  names(df)[es_categorica]
}

# Formatea un valor p en lenguaje claro
interpretar_p <- function(p, alfa = 0.05) {
  if (is.na(p)) return("No fue posible calcular un valor p con estos datos.")
  if (p < alfa) {
    sprintf(paste0("El valor de p es %.4f, que es MENOR a 0.05. ",
                    "Esto sugiere que SÍ existe una diferencia ",
                    "estadísticamente significativa."), p)
  } else {
    sprintf(paste0("El valor de p es %.4f, que es MAYOR a 0.05. ",
                    "Esto sugiere que NO hay evidencia suficiente de una ",
                    "diferencia estadísticamente significativa."), p)
  }
}

# Genera un conjunto de datos de ejemplo: estudio de artritis reumatoide (AR)
# inducida en un modelo murino (ratones), con 3 grupos experimentales
# evaluados a lo largo del tiempo.
generar_datos_ejemplo <- function() {
  set.seed(2024)
  n_por_grupo <- 8
  grupos <- c("Control", "AR", "AR+Tratamiento")
  dias <- c(0, 7, 14, 21, 28)

  info_ratones <- data.frame(
    Raton_ID = 1:(n_por_grupo * length(grupos)),
    Grupo = rep(grupos, each = n_por_grupo)
  )

  datos <- merge(info_ratones, data.frame(Dia = dias))

  datos$Puntaje_clinico <- with(datos, {
    base <- ifelse(Grupo == "Control", 0.05 * Dia,
             ifelse(Grupo == "AR", 0.35 * Dia, 0.15 * Dia))
    pmax(0, round(base + rnorm(nrow(datos), 0, 1), 1))
  })

  datos$Peso_g <- with(datos, {
    base_w <- 22 - ifelse(Grupo == "Control", 0,
                    ifelse(Grupo == "AR", 0.08 * Dia, 0.03 * Dia))
    round(base_w + rnorm(nrow(datos), 0, 0.8), 1)
  })

  datos$IL6_pg_mL <- with(datos, {
    base_il6 <- ifelse(Grupo == "Control", 20,
                 ifelse(Grupo == "AR", 20 + 3 * Dia, 20 + 1.2 * Dia))
    round(pmax(5, base_il6 + rnorm(nrow(datos), 0, 8)), 1)
  })

  datos$Grupo <- factor(datos$Grupo, levels = grupos)
  datos <- datos[order(datos$Raton_ID, datos$Dia),
                 c("Raton_ID", "Grupo", "Dia", "Puntaje_clinico",
                   "Peso_g", "IL6_pg_mL")]
  rownames(datos) <- NULL
  datos
}

## ---- 3. Definición de los pasos del asistente ------------------------

NOMBRES_PASOS <- c(
  "Bienvenida",
  "Cargar datos",
  "Vista previa",
  "Limpieza",
  "Análisis",
  "Graficar",
  "Descargar"
)
TOTAL_PASOS <- length(NOMBRES_PASOS)

## ---- 4. Interfaz de usuario (UI) -------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #f7f8fa; }
      .titulo-app {
        background-color: #7a1f3d;
        color: white;
        padding: 18px 24px;
        border-radius: 6px;
        margin-bottom: 18px;
      }
      .titulo-app h2 { margin: 0; font-weight: 600; }
      .titulo-app p { margin: 4px 0 0 0; font-size: 14px; opacity: 0.9; }
      .paso-barra {
        display: flex;
        justify-content: space-between;
        margin-bottom: 22px;
        flex-wrap: wrap;
      }
      .paso-item {
        flex: 1;
        text-align: center;
        padding: 8px 4px;
        font-size: 12.5px;
        border-bottom: 4px solid #d9d9d9;
        color: #999;
      }
      .paso-activo {
        border-bottom: 4px solid #7a1f3d;
        color: #7a1f3d;
        font-weight: 700;
      }
      .paso-completo {
        border-bottom: 4px solid #b98ca0;
        color: #7a1f3d;
      }
      .caja {
        background-color: white;
        padding: 24px;
        border-radius: 8px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.08);
        margin-bottom: 20px;
      }
      .ayuda {
        font-size: 12.5px;
        color: #666;
        font-style: italic;
      }
      .btn-nav { min-width: 110px; }
      footer.pie {
        text-align: center;
        color: #999;
        font-size: 12px;
        margin-top: 30px;
        margin-bottom: 10px;
      }
    "))
  ),

  div(class = "titulo-app",
      h2("Bonifaz-INCMNSZ"),
      p("Asistente de análisis de datos paso a paso — Instituto Nacional de Ciencias Médicas y Nutrición Salvador Zubirán"),
      div(style = "float:right; margin-top:-38px;",
          actionButton("btn_restart", "Reiniciar", class = "btn-sm btn-light"))
  ),

  uiOutput("barra_progreso"),
  uiOutput("cuerpo_asistente"),

  tags$footer(class = "pie",
    "Bonifaz-INCMNSZ · Herramienta de apoyo para investigación clínica y básica (ej. modelos de artritis reumatoide en ratones) · No sustituye la asesoría de un bioestadístico. Realizado por Daniel Bonifaz-Calvo Ibarrola julio 2026"
  )
)

## ---- 5. Lógica del servidor ------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    paso = 1,
    datos_crudos = NULL,
    datos_limpios = NULL,
    limpieza_aplicada = FALSE,
    ultima_grafica = NULL,
    bitacora = character(0),
    resultados = list(),   # guarda el resultado más reciente de cada tipo de análisis
    graficas = list()      # guarda la gráfica más reciente de cada tipo generado
  )

  # Etiquetas legibles para cada tipo de gráfica (usadas en el reporte PDF)
  ETIQUETAS_GRAFICA <- c(
    histograma = "Histograma",
    boxplot = "Diagrama de caja",
    dispersion = "Diagrama de dispersión",
    barras = "Gráfica de barras",
    linea_tiempo = "Evolución en el tiempo"
  )

  agregar_bitacora <- function(texto) {
    rv$bitacora <- c(rv$bitacora,
                      paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M"), "] ", texto))
  }

  # Datos actuales a usar en análisis/gráficas: limpios si existen, si no crudos
  datos_actuales <- reactive({
    if (!is.null(rv$datos_limpios)) rv$datos_limpios else rv$datos_crudos
  })

  observeEvent(input$btn_restart, { session$reload() })

  ## ---- Barra de progreso ----
  output$barra_progreso <- renderUI({
    items <- lapply(seq_along(NOMBRES_PASOS), function(i) {
      clase <- if (i == rv$paso) "paso-item paso-activo"
               else if (i < rv$paso) "paso-item paso-completo"
               else "paso-item"
      div(class = clase, paste0(i, ". ", NOMBRES_PASOS[i]))
    })
    div(class = "paso-barra", items)
  })

  ## ---- Navegación genérica ----
  ir_siguiente <- function() { rv$paso <- min(TOTAL_PASOS, rv$paso + 1) }
  ir_atras <- function() { rv$paso <- max(1, rv$paso - 1) }

  botones_nav <- function(mostrar_siguiente = TRUE, id_siguiente = "btn_siguiente",
                           texto_siguiente = "Siguiente \u2192") {
    div(style = "margin-top: 20px; display:flex; justify-content: space-between;",
        if (rv$paso > 1) actionButton("btn_atras", "\u2190 Atrás", class = "btn-nav btn-default")
        else div(),
        if (mostrar_siguiente) actionButton(id_siguiente, texto_siguiente, class = "btn-nav btn-primary")
        else div()
    )
  }

  observeEvent(input$btn_atras, { ir_atras() })
  observeEvent(input$btn_siguiente, { ir_siguiente() })

  ## =====================================================================
  ## CUERPO PRINCIPAL: se redibuja según el paso actual
  ## =====================================================================
  output$cuerpo_asistente <- renderUI({

    if (rv$paso == 1) {
      ## ---------------- PASO 1: BIENVENIDA ----------------
      div(class = "caja",
        h3("¡Bienvenido(a)!"),
        p("Esta aplicación te guiará, paso a paso, para analizar y graficar tus datos ",
          "sin necesidad de escribir código en R."),
        p("Está pensada para investigadores del área clínica y de ciencias básicas ",
          "(por ejemplo, estudios con modelos murinos de artritis reumatoide), pero ",
          "funciona con cualquier tabla de datos (archivo CSV o Excel)."),
        tags$ul(
          tags$li("Paso 1: Bienvenida (aquí estás)"),
          tags$li("Paso 2: Cargar tu archivo de datos"),
          tags$li("Paso 3: Revisar una vista previa"),
          tags$li("Paso 4: Limpiar los datos (opcional)"),
          tags$li("Paso 5: Elegir y correr un análisis estadístico"),
          tags$li("Paso 6: Generar gráficas"),
          tags$li("Paso 7: Descargar tus resultados")
        ),
        p(class = "ayuda", "Puedes ir hacia atrás en cualquier momento sin perder tu información."),
        botones_nav(texto_siguiente = "Comenzar \u2192")
      )

    } else if (rv$paso == 2) {
      ## ---------------- PASO 2: CARGAR DATOS ----------------
      div(class = "caja",
        h3("¿Deseas subir un archivo?"),
        p("Puedes subir un archivo CSV o Excel (.xlsx) con tus datos. Cada columna debe ",
          "representar una variable (por ejemplo: Grupo, Día, Puntaje clínico, Peso, etc.) ",
          "y cada fila una observación (por ejemplo, un ratón en un día determinado)."),

        radioButtons("tipo_carga", NULL,
                     choices = c("Subir mi propio archivo" = "propio",
                                 "Usar datos de ejemplo (estudio de artritis reumatoide en ratones)" = "ejemplo"),
                     selected = "propio"),

        conditionalPanel(
          condition = "input.tipo_carga == 'propio'",
          fileInput("archivo", "Selecciona tu archivo (.csv, .xlsx o .xls)",
                    accept = c(".csv", ".xlsx", ".xls")),
          conditionalPanel(
            condition = "input.archivo != null",
            fluidRow(
              column(4, radioButtons("csv_sep", "Separador de columnas (solo CSV)",
                                      choices = c("Coma ( , )" = ",",
                                                  "Punto y coma ( ; )" = ";",
                                                  "Tabulador" = "\t"),
                                      selected = ",")),
              column(4, radioButtons("csv_dec", "Separador decimal (solo CSV)",
                                      choices = c("Punto ( . )" = ".",
                                                  "Coma ( , )" = ","),
                                      selected = ".")),
              column(4, radioButtons("csv_enc", "Codificación de texto (solo CSV)",
                                      choices = c("UTF-8" = "UTF-8",
                                                  "Latin1 (Windows/Excel en español)" = "Latin1"),
                                      selected = "UTF-8"))
            )
          ),
          p(class = "ayuda",
            "Consejo: si tu archivo viene de Excel en español y al subirlo ves las letras ",
            "con acentos mal (ej. 'artritis' se ve como 'artrÃ­tis'), cambia la codificación a 'Latin1'. ",
            "Si tu CSV viene de Excel en configuración regional en español, prueba con 'Punto y coma'.")
        ),

        conditionalPanel(
          condition = "input.tipo_carga == 'ejemplo'",
          p("Se usará un conjunto de datos simulado de un estudio con 3 grupos de ratones ",
            "(Control, AR, AR+Tratamiento) medidos en los días 0, 7, 14, 21 y 28, con las ",
            "variables: Puntaje clínico, Peso (g) e IL-6 (pg/mL)."),
          actionButton("cargar_ejemplo", "Cargar datos de ejemplo", class = "btn-info")
        ),

        uiOutput("mensaje_carga"),
        botones_nav(mostrar_siguiente = FALSE),
        uiOutput("boton_siguiente_carga")
      )

    } else if (rv$paso == 3) {
      ## ---------------- PASO 3: VISTA PREVIA ----------------
      req(rv$datos_crudos)
      df <- rv$datos_crudos
      div(class = "caja",
        h3("Vista previa de tus datos"),
        p(sprintf("Tu archivo tiene %d filas (observaciones) y %d columnas (variables).",
                  nrow(df), ncol(df))),
        DTOutput("tabla_preview"),
        h4("Resumen por variable"),
        verbatimTextOutput("resumen_preview"),
        botones_nav()
      )

    } else if (rv$paso == 4) {
      ## ---------------- PASO 4: LIMPIEZA ----------------
      req(rv$datos_crudos)
      div(class = "caja",
        h3("¿Quieres limpiar tus datos?"),
        p("Selecciona las opciones de limpieza que quieras aplicar. Si no necesitas ",
          "limpiar nada, simplemente da clic en 'Siguiente'."),
        checkboxGroupInput("opciones_limpieza", NULL,
          choices = c(
            "Eliminar filas con datos faltantes (NA)" = "quitar_na",
            "Eliminar columnas vacías (100% de datos faltantes)" = "quitar_col_vacia",
            "Eliminar filas duplicadas" = "quitar_duplicados",
            "Quitar espacios en blanco al inicio/final del texto" = "trim_texto"
          )),
        actionButton("aplicar_limpieza", "Aplicar limpieza", class = "btn-info"),
        actionButton("sin_limpieza", "No necesito limpiar, usar datos originales", class = "btn-default"),
        uiOutput("resultado_limpieza"),
        botones_nav()
      )

    } else if (rv$paso == 5) {
      ## ---------------- PASO 5: ANÁLISIS ----------------
      req(datos_actuales())
      df <- datos_actuales()
      vars_num <- obtener_vars_numericas(df)
      vars_cat <- obtener_vars_categoricas(df)

      div(class = "caja",
        h3("¿Qué tipo de análisis quieres hacer?"),
        radioButtons("tipo_analisis", NULL,
          choices = c(
            "Estadística descriptiva (promedios, medianas, etc.)" = "descriptivo",
            "Comparar grupos (2 o más grupos)" = "comparar",
            "Relación entre dos variables (correlación)" = "correlacion",
            "Tabla de frecuencias (variable categórica)" = "frecuencias",
            "Evolución en el tiempo (ej. puntaje clínico a lo largo de los días)" = "longitudinal"
          )),
        hr(),

        conditionalPanel(condition = "input.tipo_analisis == 'descriptivo'",
          selectInput("desc_var", "Variable numérica a describir:", choices = vars_num),
          selectizeInput("desc_var_grupo", "Agrupar por (opcional):",
                         choices = c("(Sin agrupar)" = "", vars_cat)),
          actionButton("run_desc", "Calcular", class = "btn-primary"),
          tableOutput("out_desc")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'comparar'",
          selectInput("comp_grupo", "Variable de grupo (categórica):", choices = vars_cat),
          selectInput("comp_num", "Variable numérica a comparar:", choices = vars_num),
          actionButton("run_comp", "Comparar grupos", class = "btn-primary"),
          verbatimTextOutput("out_comp"),
          tableOutput("out_comp_tabla")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'correlacion'",
          selectInput("corr_var1", "Primera variable numérica:", choices = vars_num),
          selectInput("corr_var2", "Segunda variable numérica:", choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          actionButton("run_corr", "Calcular correlación", class = "btn-primary"),
          verbatimTextOutput("out_corr")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'frecuencias'",
          selectInput("freq_var", "Variable categórica:", choices = vars_cat),
          actionButton("run_freq", "Calcular frecuencias", class = "btn-primary"),
          tableOutput("out_freq")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'longitudinal'",
          selectInput("long_tiempo", "Variable de tiempo (ej. Día):", choices = vars_num),
          selectInput("long_grupo", "Variable de grupo:", choices = vars_cat),
          selectInput("long_resp", "Variable de respuesta (ej. Puntaje clínico):", choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          actionButton("run_long", "Calcular evolución en el tiempo", class = "btn-primary"),
          tableOutput("out_long"),
          p(class = "ayuda", "Consejo: en el paso 6 (Graficar) podrás visualizar esta evolución en una línea de tiempo.")
        ),

        botones_nav()
      )

    } else if (rv$paso == 6) {
      ## ---------------- PASO 6: GRAFICAR ----------------
      req(datos_actuales())
      df <- datos_actuales()
      vars_num <- obtener_vars_numericas(df)
      vars_cat <- obtener_vars_categoricas(df)

      div(class = "caja",
        h3("¿Quieres graficar tus datos?"),
        radioButtons("tipo_grafica", NULL,
          choices = c(
            "Histograma (distribución de una variable)" = "histograma",
            "Diagrama de caja (comparar grupos)" = "boxplot",
            "Diagrama de dispersión (relación entre 2 variables)" = "dispersion",
            "Gráfica de barras (frecuencias)" = "barras",
            "Evolución en el tiempo (línea con promedio \u00b1 error estándar)" = "linea_tiempo"
          )),
        hr(),

        conditionalPanel(condition = "input.tipo_grafica == 'histograma'",
          selectInput("hist_var", "Variable numérica:", choices = vars_num)
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'boxplot'",
          selectInput("box_grupo", "Variable de grupo:", choices = vars_cat),
          selectInput("box_num", "Variable numérica:", choices = vars_num)
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'dispersion'",
          selectInput("disp_x", "Variable X:", choices = vars_num),
          selectInput("disp_y", "Variable Y:", choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          selectizeInput("disp_color", "Colorear por (opcional):",
                         choices = c("(Ninguno)" = "", vars_cat))
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'barras'",
          selectInput("barras_var", "Variable categórica:", choices = vars_cat)
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'linea_tiempo'",
          selectInput("lt_tiempo", "Variable de tiempo:", choices = vars_num),
          selectInput("lt_grupo", "Variable de grupo:", choices = vars_cat),
          selectInput("lt_resp", "Variable de respuesta:", choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1])
        ),

        actionButton("generar_grafica", "Generar gráfica", class = "btn-primary"),
        br(), br(),
        plotOutput("grafica_principal", height = "420px"),
        uiOutput("boton_descargar_grafica"),
        botones_nav()
      )

    } else if (rv$paso == 7) {
      ## ---------------- PASO 7: DESCARGAR ----------------
      div(class = "caja",
        h3("Descarga tus resultados"),
        p("Descarga tus datos (originales o limpios), un reporte completo en PDF, ",
          "y una bitácora con un resumen de las acciones y análisis que realizaste ",
          "en esta sesión."),
        fluidRow(
          column(4,
            h4("Datos"),
            downloadButton("descargar_csv", "Descargar datos (.csv)", class = "btn-info"),
            br(), br(),
            downloadButton("descargar_xlsx", "Descargar datos (.xlsx)", class = "btn-info")
          ),
          column(4,
            h4("Reporte en PDF"),
            p(class = "ayuda",
              "Incluye: resumen de los datos, el resultado más reciente de cada ",
              "análisis que hayas ejecutado, las gráficas que hayas generado, y la bitácora."),
            downloadButton("descargar_pdf", "Descargar reporte completo (.pdf)", class = "btn-danger")
          ),
          column(4,
            h4("Bitácora de la sesión"),
            downloadButton("descargar_reporte", "Descargar bitácora (.txt)", class = "btn-info")
          )
        ),
        hr(),
        h4("Bitácora en pantalla"),
        verbatimTextOutput("vista_bitacora"),
        botones_nav(mostrar_siguiente = FALSE)
      )
    }
  })

  ## =====================================================================
  ## PASO 2: Carga de datos
  ## =====================================================================

  output$mensaje_carga <- renderUI({
    if (!is.null(rv$datos_crudos)) {
      div(style = "color: #1a7a1a; margin-top: 10px; font-weight: 600;",
          sprintf("\u2713 Datos cargados correctamente: %d filas x %d columnas.",
                  nrow(rv$datos_crudos), ncol(rv$datos_crudos)))
    } else {
      div(style = "color: #888; margin-top: 10px;", "Aún no se han cargado datos.")
    }
  })

  output$boton_siguiente_carga <- renderUI({
    if (!is.null(rv$datos_crudos)) {
      div(style = "text-align: right; margin-top: -46px;",
          actionButton("btn_siguiente_desde_carga", "Siguiente \u2192", class = "btn-nav btn-primary"))
    }
  })

  observeEvent(input$btn_siguiente_desde_carga, { ir_siguiente() })

  observeEvent(input$archivo, {
    req(input$archivo)
    ext <- tolower(tools::file_ext(input$archivo$name))
    df <- tryCatch({
      if (ext == "csv") {
        sep <- if (is.null(input$csv_sep)) "," else input$csv_sep
        dec <- if (is.null(input$csv_dec)) "." else input$csv_dec
        enc <- if (is.null(input$csv_enc)) "UTF-8" else input$csv_enc
        read.csv(input$archivo$datapath, sep = sep, dec = dec,
                  fileEncoding = enc, stringsAsFactors = FALSE,
                  na.strings = c("NA", "", "NaN", "na", "N/A"))
      } else if (ext %in% c("xlsx", "xls")) {
        as.data.frame(readxl::read_excel(input$archivo$datapath))
      } else {
        stop("Formato de archivo no soportado. Usa .csv, .xlsx o .xls")
      }
    }, error = function(e) {
      showNotification(paste("Error al leer el archivo:", e$message),
                        type = "error", duration = 8)
      NULL
    })

    if (!is.null(df)) {
      rv$datos_crudos <- df
      rv$datos_limpios <- NULL
      agregar_bitacora(sprintf("Se cargó el archivo '%s' (%d filas, %d columnas).",
                                input$archivo$name, nrow(df), ncol(df)))
    }
  })

  observeEvent(input$cargar_ejemplo, {
    rv$datos_crudos <- generar_datos_ejemplo()
    rv$datos_limpios <- NULL
    agregar_bitacora("Se cargaron los datos de ejemplo (estudio de artritis reumatoide en ratones).")
  })

  ## =====================================================================
  ## PASO 3: Vista previa
  ## =====================================================================

  output$tabla_preview <- renderDT({
    req(rv$datos_crudos)
    datatable(rv$datos_crudos, options = list(pageLength = 6, scrollX = TRUE))
  })

  output$resumen_preview <- renderPrint({
    req(rv$datos_crudos)
    summary(rv$datos_crudos)
  })

  ## =====================================================================
  ## PASO 4: Limpieza
  ## =====================================================================

  observeEvent(input$sin_limpieza, {
    rv$datos_limpios <- rv$datos_crudos
    rv$limpieza_aplicada <- FALSE
    agregar_bitacora("El usuario decidió continuar sin aplicar limpieza a los datos.")
    showNotification("Se usarán los datos originales sin cambios.", type = "message")
  })

  observeEvent(input$aplicar_limpieza, {
    req(rv$datos_crudos)
    df <- rv$datos_crudos
    filas_antes <- nrow(df)
    cols_antes <- ncol(df)
    acciones <- c()

    if ("trim_texto" %in% input$opciones_limpieza) {
      df[] <- lapply(df, function(x) {
        if (is.character(x)) trimws(x) else x
      })
      acciones <- c(acciones, "se quitaron espacios en blanco al inicio/final del texto")
    }

    if ("quitar_col_vacia" %in% input$opciones_limpieza) {
      cols_vacias <- sapply(df, function(x) all(is.na(x)))
      if (any(cols_vacias)) df <- df[, !cols_vacias, drop = FALSE]
      acciones <- c(acciones, sprintf("se eliminaron %d columna(s) totalmente vacías", sum(cols_vacias)))
    }

    if ("quitar_duplicados" %in% input$opciones_limpieza) {
      n_antes <- nrow(df)
      df <- df[!duplicated(df), , drop = FALSE]
      acciones <- c(acciones, sprintf("se eliminaron %d fila(s) duplicada(s)", n_antes - nrow(df)))
    }

    if ("quitar_na" %in% input$opciones_limpieza) {
      n_antes <- nrow(df)
      df <- df[complete.cases(df), , drop = FALSE]
      acciones <- c(acciones, sprintf("se eliminaron %d fila(s) con datos faltantes", n_antes - nrow(df)))
    }

    rv$datos_limpios <- df
    rv$limpieza_aplicada <- TRUE

    if (length(acciones) == 0) {
      agregar_bitacora("Se dio clic en 'Aplicar limpieza' pero no se seleccionó ninguna opción.")
    } else {
      agregar_bitacora(paste0("Limpieza aplicada: ", paste(acciones, collapse = "; "), "."))
    }

    output$resultado_limpieza <- renderUI({
      div(style = "margin-top: 15px; padding: 12px; background-color: #eef7ee; border-radius: 6px;",
          h4("Resultado de la limpieza"),
          p(sprintf("Antes: %d filas x %d columnas.", filas_antes, cols_antes)),
          p(sprintf("Después: %d filas x %d columnas.", nrow(df), ncol(df))),
          if (length(acciones) > 0) tags$ul(lapply(acciones, tags$li))
          else p("No se aplicó ningún cambio (no se seleccionó ninguna opción).")
      )
    })
  })

  ## =====================================================================
  ## PASO 5: Análisis
  ## =====================================================================

  # --- Descriptivos ---
  observeEvent(input$run_desc, {
    df <- datos_actuales()
    req(df, input$desc_var)

    calcular_resumen <- function(x) {
      x <- x[!is.na(x)]
      data.frame(
        n = length(x),
        Media = round(mean(x), 2),
        Mediana = round(median(x), 2),
        DE = round(sd(x), 2),
        Minimo = round(min(x), 2),
        Maximo = round(max(x), 2)
      )
    }

    if (!is.null(input$desc_var_grupo) && input$desc_var_grupo != "") {
      resumen <- do.call(rbind, lapply(split(df[[input$desc_var]], df[[input$desc_var_grupo]]),
                                        calcular_resumen))
      resumen <- cbind(Grupo = rownames(resumen), resumen)
      rownames(resumen) <- NULL
    } else {
      resumen <- calcular_resumen(df[[input$desc_var]])
    }

    output$out_desc <- renderTable(resumen)
    rv$resultados$descriptivo <- list(
      titulo = sprintf("Estadística descriptiva: %s%s", input$desc_var,
                        if (!is.null(input$desc_var_grupo) && input$desc_var_grupo != "")
                          paste0(" (agrupado por ", input$desc_var_grupo, ")") else ""),
      tabla = resumen
    )
    agregar_bitacora(sprintf("Estadística descriptiva calculada para '%s'%s.",
                              input$desc_var,
                              if (!is.null(input$desc_var_grupo) && input$desc_var_grupo != "")
                                paste0(" agrupado por '", input$desc_var_grupo, "'") else ""))
  })

  # --- Comparación de grupos ---
  observeEvent(input$run_comp, {
    df <- datos_actuales()
    req(df, input$comp_grupo, input$comp_num)

    df_sub <- df[!is.na(df[[input$comp_num]]) & !is.na(df[[input$comp_grupo]]), ]
    grupos <- unique(df_sub[[input$comp_grupo]])
    n_grupos <- length(grupos)

    resultado_texto <- NULL
    tabla_resultado <- NULL

    if (n_grupos < 2) {
      resultado_texto <- "Se necesitan al menos 2 grupos distintos para poder comparar. Verifica la variable de grupo seleccionada."
    } else {
      valores_por_grupo <- split(df_sub[[input$comp_num]], df_sub[[input$comp_grupo]])
      p_normalidad <- sapply(valores_por_grupo, function(x) {
        if (length(x) >= 3 && length(x) <= 5000) tryCatch(shapiro.test(x)$p.value, error = function(e) NA)
        else NA
      })
      es_normal <- all(!is.na(p_normalidad) & p_normalidad > 0.05)

      formula_comp <- as.formula(paste0("`", input$comp_num, "` ~ `", input$comp_grupo, "`"))

      if (n_grupos == 2) {
        if (es_normal) {
          prueba <- t.test(formula_comp, data = df_sub)
          nombre_prueba <- "prueba t de Student"
        } else {
          prueba <- wilcox.test(formula_comp, data = df_sub)
          nombre_prueba <- "prueba de Wilcoxon (Mann-Whitney)"
        }
        resultado_texto <- paste0(
          "Se compararon 2 grupos: ", paste(grupos, collapse = " vs. "), ".\n",
          "Se usó la ", nombre_prueba,
          " (", if (es_normal) "los datos se comportan de forma aproximadamente normal"
                else "los datos NO se comportan de forma normal, por lo que se usó una prueba no paramétrica",
          ").\n\n", interpretar_p(prueba$p.value)
        )
      } else {
        if (es_normal) {
          modelo <- aov(formula_comp, data = df_sub)
          p_valor <- summary(modelo)[[1]][["Pr(>F)"]][1]
          nombre_prueba <- "ANOVA de una vía"
          posthoc <- TukeyHSD(modelo)[[1]]
          tabla_resultado <- data.frame(Comparacion = rownames(posthoc), round(as.data.frame(posthoc), 4))
        } else {
          kt <- kruskal.test(formula_comp, data = df_sub)
          p_valor <- kt$p.value
          nombre_prueba <- "prueba de Kruskal-Wallis"
          ph <- pairwise.wilcox.test(df_sub[[input$comp_num]], df_sub[[input$comp_grupo]],
                                       p.adjust.method = "BH")
          tabla_resultado <- as.data.frame(as.table(ph$p.value))
          names(tabla_resultado) <- c("Grupo_1", "Grupo_2", "p_ajustada")
          tabla_resultado <- tabla_resultado[!is.na(tabla_resultado$p_ajustada), ]
        }
        resultado_texto <- paste0(
          "Se compararon ", n_grupos, " grupos: ", paste(grupos, collapse = ", "), ".\n",
          "Se usó ", nombre_prueba,
          " (", if (es_normal) "los datos se comportan de forma aproximadamente normal"
                else "los datos NO se comportan de forma normal, por lo que se usó una prueba no paramétrica",
          ").\n\n", interpretar_p(p_valor),
          "\n\nSi el resultado general es significativo, revisa la tabla de comparaciones ",
          "por pares (post-hoc) para ver entre qué grupos específicos hay diferencia."
        )
      }
    }

    output$out_comp <- renderText(resultado_texto)
    output$out_comp_tabla <- renderTable(tabla_resultado)
    rv$resultados$comparacion <- list(
      titulo = sprintf("Comparación de grupos: %s según %s", input$comp_num, input$comp_grupo),
      texto = resultado_texto,
      tabla = tabla_resultado
    )
    agregar_bitacora(sprintf("Comparación de grupos: '%s' según '%s'.", input$comp_num, input$comp_grupo))
  })

  # --- Correlación ---
  observeEvent(input$run_corr, {
    df <- datos_actuales()
    req(df, input$corr_var1, input$corr_var2)

    df_sub <- df[!is.na(df[[input$corr_var1]]) & !is.na(df[[input$corr_var2]]), ]

    if (input$corr_var1 == input$corr_var2) {
      texto <- "Por favor selecciona dos variables distintas para calcular la correlación."
    } else {
      p1 <- tryCatch(shapiro.test(df_sub[[input$corr_var1]])$p.value, error = function(e) NA)
      p2 <- tryCatch(shapiro.test(df_sub[[input$corr_var2]])$p.value, error = function(e) NA)
      normal <- !is.na(p1) && !is.na(p2) && p1 > 0.05 && p2 > 0.05
      metodo <- if (normal) "pearson" else "spearman"

      prueba <- cor.test(df_sub[[input$corr_var1]], df_sub[[input$corr_var2]], method = metodo)

      fuerza <- abs(prueba$estimate)
      descripcion_fuerza <- if (fuerza < 0.3) "débil" else if (fuerza < 0.6) "moderada" else "fuerte"
      direccion <- if (prueba$estimate > 0) "positiva (cuando una variable sube, la otra tiende a subir)"
                   else "negativa (cuando una variable sube, la otra tiende a bajar)"

      texto <- paste0(
        "Se calculó la correlación de ", if (normal) "Pearson" else "Spearman",
        " (", if (normal) "datos aproximadamente normales" else "datos no normales, método no paramétrico", ").\n\n",
        sprintf("Coeficiente de correlación = %.3f\n", prueba$estimate),
        interpretar_p(prueba$p.value),
        "\n\n", sprintf("La relación es de intensidad %s y dirección %s.", descripcion_fuerza, direccion)
      )
    }

    output$out_corr <- renderText(texto)
    rv$resultados$correlacion <- list(
      titulo = sprintf("Correlación entre %s y %s", input$corr_var1, input$corr_var2),
      texto = texto
    )
    agregar_bitacora(sprintf("Correlación calculada entre '%s' y '%s'.", input$corr_var1, input$corr_var2))
  })

  # --- Frecuencias ---
  observeEvent(input$run_freq, {
    df <- datos_actuales()
    req(df, input$freq_var)

    tabla <- as.data.frame(table(df[[input$freq_var]], useNA = "ifany"))
    names(tabla) <- c("Categoria", "Frecuencia")
    tabla$Porcentaje <- round(100 * tabla$Frecuencia / sum(tabla$Frecuencia), 1)

    output$out_freq <- renderTable(tabla)
    rv$resultados$frecuencias <- list(
      titulo = sprintf("Tabla de frecuencias: %s", input$freq_var),
      tabla = tabla
    )
    agregar_bitacora(sprintf("Tabla de frecuencias calculada para '%s'.", input$freq_var))
  })

  # --- Evolución en el tiempo ---
  observeEvent(input$run_long, {
    df <- datos_actuales()
    req(df, input$long_tiempo, input$long_grupo, input$long_resp)

    resumen <- df %>%
      dplyr::filter(!is.na(.data[[input$long_resp]])) %>%
      dplyr::group_by(.data[[input$long_tiempo]], .data[[input$long_grupo]]) %>%
      dplyr::summarise(
        n = dplyr::n(),
        Promedio = round(mean(.data[[input$long_resp]], na.rm = TRUE), 2),
        Error_estandar = round(sd(.data[[input$long_resp]], na.rm = TRUE) / sqrt(dplyr::n()), 2),
        .groups = "drop"
      )

    output$out_long <- renderTable(resumen)
    rv$resultados$longitudinal <- list(
      titulo = sprintf("Evolución de %s a lo largo de %s, por %s",
                        input$long_resp, input$long_tiempo, input$long_grupo),
      tabla = resumen
    )
    agregar_bitacora(sprintf("Evolución en el tiempo calculada: '%s' a lo largo de '%s', por '%s'.",
                              input$long_resp, input$long_tiempo, input$long_grupo))
  })

  ## =====================================================================
  ## PASO 6: Graficar
  ## =====================================================================

  observeEvent(input$generar_grafica, {
    df <- datos_actuales()
    req(df, input$tipo_grafica)

    grafica <- tryCatch({
      if (input$tipo_grafica == "histograma") {
        req(input$hist_var)
        ggplot(df, aes(x = .data[[input$hist_var]])) +
          geom_histogram(bins = 20, fill = "#7a1f3d", color = "white", alpha = 0.9) +
          labs(title = paste("Histograma de", input$hist_var),
               x = input$hist_var, y = "Frecuencia") +
          theme_minimal(base_size = 14)

      } else if (input$tipo_grafica == "boxplot") {
        req(input$box_grupo, input$box_num)
        ggplot(df, aes(x = factor(.data[[input$box_grupo]]), y = .data[[input$box_num]],
                       fill = factor(.data[[input$box_grupo]]))) +
          geom_boxplot(alpha = 0.85) +
          labs(title = paste(input$box_num, "por", input$box_grupo),
               x = input$box_grupo, y = input$box_num, fill = input$box_grupo) +
          theme_minimal(base_size = 14) +
          theme(legend.position = "none")

      } else if (input$tipo_grafica == "dispersion") {
        req(input$disp_x, input$disp_y)
        if (!is.null(input$disp_color) && input$disp_color != "") {
          ggplot(df, aes(x = .data[[input$disp_x]], y = .data[[input$disp_y]],
                        color = factor(.data[[input$disp_color]]))) +
            geom_point(size = 2.6, alpha = 0.85) +
            labs(title = paste(input$disp_y, "vs.", input$disp_x),
                 x = input$disp_x, y = input$disp_y, color = input$disp_color) +
            theme_minimal(base_size = 14)
        } else {
          ggplot(df, aes(x = .data[[input$disp_x]], y = .data[[input$disp_y]])) +
            geom_point(size = 2.6, alpha = 0.85, color = "#7a1f3d") +
            labs(title = paste(input$disp_y, "vs.", input$disp_x),
                 x = input$disp_x, y = input$disp_y) +
            theme_minimal(base_size = 14)
        }

      } else if (input$tipo_grafica == "barras") {
        req(input$barras_var)
        ggplot(df, aes(x = factor(.data[[input$barras_var]]))) +
          geom_bar(fill = "#7a1f3d", alpha = 0.9) +
          labs(title = paste("Frecuencias de", input$barras_var),
               x = input$barras_var, y = "Conteo") +
          theme_minimal(base_size = 14)

      } else if (input$tipo_grafica == "linea_tiempo") {
        req(input$lt_tiempo, input$lt_grupo, input$lt_resp)
        resumen <- df %>%
          dplyr::filter(!is.na(.data[[input$lt_resp]])) %>%
          dplyr::group_by(.data[[input$lt_tiempo]], .data[[input$lt_grupo]]) %>%
          dplyr::summarise(
            Promedio = mean(.data[[input$lt_resp]], na.rm = TRUE),
            EE = sd(.data[[input$lt_resp]], na.rm = TRUE) / sqrt(dplyr::n()),
            .groups = "drop"
          )
        ggplot(resumen, aes(x = .data[[input$lt_tiempo]], y = Promedio,
                            color = factor(.data[[input$lt_grupo]]),
                            group = factor(.data[[input$lt_grupo]]))) +
          geom_line(linewidth = 1) +
          geom_point(size = 2.6) +
          geom_errorbar(aes(ymin = Promedio - EE, ymax = Promedio + EE), width = 0.4) +
          labs(title = paste("Evolución de", input$lt_resp, "en el tiempo"),
               x = input$lt_tiempo, y = paste0(input$lt_resp, " (promedio \u00b1 EE)"),
               color = input$lt_grupo) +
          theme_minimal(base_size = 14)
      }
    }, error = function(e) {
      showNotification(paste("No se pudo generar la gráfica:", e$message), type = "error", duration = 8)
      NULL
    })

    rv$ultima_grafica <- grafica
    if (!is.null(grafica)) {
      tmp <- rv$graficas
      tmp[[input$tipo_grafica]] <- list(
        plot = grafica,
        etiqueta = ETIQUETAS_GRAFICA[[input$tipo_grafica]]
      )
      rv$graficas <- tmp
      agregar_bitacora(sprintf("Gráfica generada: %s.", input$tipo_grafica))
    }
  })

  output$grafica_principal <- renderPlot({
    req(rv$ultima_grafica)
    rv$ultima_grafica
  })

  output$boton_descargar_grafica <- renderUI({
    req(rv$ultima_grafica)
    downloadButton("descargar_grafica_png", "Descargar gráfica (.png)", class = "btn-info")
  })

  output$descargar_grafica_png <- downloadHandler(
    filename = function() paste0("grafica_bonifaz_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = rv$ultima_grafica, width = 9, height = 6, dpi = 150)
    }
  )

  ## =====================================================================
  ## PASO 7: Descargar
  ## =====================================================================

  output$descargar_csv <- downloadHandler(
    filename = function() paste0("datos_bonifaz_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(datos_actuales(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$descargar_xlsx <- downloadHandler(
    filename = function() paste0("datos_bonifaz_", Sys.Date(), ".xlsx"),
    content = function(file) {
      writexl::write_xlsx(datos_actuales(), file)
    }
  )

  # --- Funciones auxiliares para armar el reporte en PDF ---

  # Escribe una o varias páginas de texto monoespaciado (con paginación automática)
  pagina_texto_pdf <- function(titulo, lineas, lineas_por_pagina = 48) {
    if (length(lineas) == 0) lineas <- ""
    bloques <- split(lineas, ceiling(seq_along(lineas) / lineas_por_pagina))
    for (bloque in bloques) {
      grid::grid.newpage()
      grid::grid.text(titulo, x = 0.03, y = 0.97, just = c("left", "top"),
                       gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#7a1f3d"))
      grid::grid.text(paste(bloque, collapse = "\n"), x = 0.03, y = 0.92,
                       just = c("left", "top"),
                       gp = grid::gpar(fontsize = 8.5, fontfamily = "mono"))
    }
  }

  # Dibuja una página con una tabla (data.frame) y un título arriba
  pagina_tabla_pdf <- function(titulo, df) {
    if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
    tema <- gridExtra::ttheme_default(base_size = 9)
    tabla_grob <- gridExtra::tableGrob(df, rows = NULL, theme = tema)
    gridExtra::grid.arrange(
      tabla_grob,
      top = grid::textGrob(titulo, gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#7a1f3d"))
    )
  }

  # Dibuja una página con una gráfica de ggplot2 y un título arriba
  pagina_grafica_pdf <- function(titulo, grafica) {
    if (is.null(grafica)) return(invisible(NULL))
    gridExtra::grid.arrange(
      grafica,
      top = grid::textGrob(titulo, gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#7a1f3d"))
    )
  }

  output$descargar_pdf <- downloadHandler(
    filename = function() paste0("reporte_bonifaz_", Sys.Date(), ".pdf"),
    content = function(file) {
      df_actual <- datos_actuales()
      res <- rv$resultados

      grDevices::pdf(file, width = 8.5, height = 11)
      on.exit(grDevices::dev.off(), add = TRUE)

      ## ---- Portada ----
      grid::grid.newpage()
      grid::grid.text("Bonifaz-INCMNSZ", x = 0.5, y = 0.72,
                       gp = grid::gpar(fontsize = 28, fontface = "bold", col = "#7a1f3d"))
      grid::grid.text("Reporte de análisis de datos", x = 0.5, y = 0.65,
                       gp = grid::gpar(fontsize = 16))
      grid::grid.text("Instituto Nacional de Ciencias Médicas y Nutrición Salvador Zubirán",
                       x = 0.5, y = 0.58, gp = grid::gpar(fontsize = 11))
      grid::grid.text(paste("Generado:", format(Sys.time(), "%d/%m/%Y %H:%M")),
                       x = 0.5, y = 0.50, gp = grid::gpar(fontsize = 10, col = "gray40"))
      if (!is.null(df_actual)) {
        grid::grid.text(sprintf("Datos analizados: %d filas x %d columnas",
                                 nrow(df_actual), ncol(df_actual)),
                         x = 0.5, y = 0.45, gp = grid::gpar(fontsize = 10, col = "gray40"))
      }

      ## ---- Resumen de los datos ----
      if (!is.null(df_actual)) {
        resumen_lineas <- capture.output(summary(df_actual))
        pagina_texto_pdf("Resumen de los datos", resumen_lineas)
      }

      ## ---- Resultados de análisis guardados (el más reciente de cada tipo) ----
      if (!is.null(res$descriptivo)) {
        pagina_tabla_pdf(res$descriptivo$titulo, res$descriptivo$tabla)
      }
      if (!is.null(res$comparacion)) {
        pagina_texto_pdf(res$comparacion$titulo, strsplit(res$comparacion$texto, "\n")[[1]])
        if (!is.null(res$comparacion$tabla) && nrow(res$comparacion$tabla) > 0) {
          pagina_tabla_pdf(paste(res$comparacion$titulo, "- comparaciones por pares"),
                            res$comparacion$tabla)
        }
      }
      if (!is.null(res$correlacion)) {
        pagina_texto_pdf(res$correlacion$titulo, strsplit(res$correlacion$texto, "\n")[[1]])
      }
      if (!is.null(res$frecuencias)) {
        pagina_tabla_pdf(res$frecuencias$titulo, res$frecuencias$tabla)
      }
      if (!is.null(res$longitudinal)) {
        pagina_tabla_pdf(res$longitudinal$titulo, res$longitudinal$tabla)
      }

      ## ---- Gráficas guardadas (la más reciente de cada tipo generado) ----
      for (g in rv$graficas) {
        pagina_grafica_pdf(g$etiqueta, g$plot)
      }

      ## ---- Bitácora de la sesión ----
      if (length(rv$bitacora) > 0) {
        pagina_texto_pdf("Bitácora de la sesión", rv$bitacora)
      }

      ## ---- Aviso si aún no hay nada que reportar ----
      if (length(res) == 0 && length(rv$graficas) == 0) {
        pagina_texto_pdf("Sin resultados", c(
          "Aún no se ha ejecutado ningún análisis ni se ha generado ninguna gráfica.",
          "Regresa a los pasos 5 (Análisis) y 6 (Graficar) para generarlos antes de",
          "descargar el reporte completo."
        ))
      }
    }
  )

  output$descargar_reporte <- downloadHandler(
    filename = function() paste0("bitacora_bonifaz_", Sys.Date(), ".txt"),
    content = function(file) {
      encabezado <- c(
        "BONIFAZ-INCMNSZ - Bitacora de sesion de analisis de datos",
        paste("Generado:", Sys.time()),
        strrep("-", 60), ""
      )
      writeLines(c(encabezado, rv$bitacora), file, useBytes = TRUE)
    }
  )

  output$vista_bitacora <- renderText({
    if (length(rv$bitacora) == 0) "Aún no se han registrado acciones en esta sesión."
    else paste(rv$bitacora, collapse = "\n")
  })

}

## ---- 6. Lanzar la aplicación ------------------------------------------
shinyApp(ui = ui, server = server)
