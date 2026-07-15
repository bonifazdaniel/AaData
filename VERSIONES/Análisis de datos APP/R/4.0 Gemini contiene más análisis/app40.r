########################################################################
# Bonifaz-INCMNSZ
# Asistente de Análisis de Datos paso a paso (Shiny)
# Instituto Nacional de Ciencias Médicas y Nutrición Salvador Zubirán
#
# Objetivo: permitir que investigadores carguen, limpien, analicen y grafiquen sus datos respondiendo
# preguntas sencillas, sin escribir código.
########################################################################

## ---- 1. Verificación e instalación de paquetes necesarios ----------
paquetes_requeridos <- c("shiny", "DT", "ggplot2", "dplyr", "tidyr",
                          "readxl", "writexl", "gridExtra", "survival")

paquetes_faltantes <- paquetes_requeridos[
  !(paquetes_requeridos %in% rownames(installed.packages()))
]

if (length(paquetes_faltantes) > 0) {
  message("Instalando paquetes necesarios: ",
          paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes, repos = "https://cloud.r-project.org")
}

invisible(lapply(paquetes_requeridos, library, character.only = TRUE))
library(grid)  

options(shiny.maxRequestSize = 30 * 1024^2)  

## ---- 2. Funciones auxiliares ----------------------------------------

obtener_vars_numericas <- function(df) {
  names(df)[sapply(df, is.numeric)]
}

obtener_vars_categoricas <- function(df) {
  es_categorica <- sapply(df, function(x) {
    is.character(x) || is.factor(x) ||
      (is.numeric(x) && length(unique(na.omit(x))) <= 10)
  })
  names(df)[es_categorica]
}

obtener_vars_todas <- function(df) {
  names(df)
}

interpretar_p <- function(p, alfa = 0.05) {
  if (is.na(p)) return("No fue posible calcular un valor p con estos datos.")
  if (p < alfa) {
    sprintf(paste0("El valor de p es %.4f, que es MENOR a 0.05. ",
                    "Esto sugiere que SÍ existe una diferencia o asociación ",
                    "estadísticamente significativa."), p)
  } else {
    sprintf(paste0("El valor de p es %.4f, que es MAYOR a 0.05. ",
                    "Esto sugiere que NO hay evidencia suficiente de una ",
                    "diferencia o asociación estadísticamente significativa."), p)
  }
}

generar_datos_ejemplo <- function() {
  set.seed(2024)
  n_por_grupo <- 8
  grupos <- c("Control", "AR", "AR+Tratamiento")
  dias <- c(0, 7, 14, 21, 28)

  info_ratones <- data.frame(
    Raton_ID = 1:(n_por_grupo * length(grupos)),
    Grupo = rep(grupos, each = n_por_grupo)
  )

  info_ratones$Dia_evento_artritis <- ifelse(
    info_ratones$Grupo == "Control",
    28,
    pmin(28, pmax(3, round(rnorm(
      nrow(info_ratones),
      mean = ifelse(info_ratones$Grupo == "AR", 14, 22),
      sd = ifelse(info_ratones$Grupo == "AR", 4, 5)
    ))))
  )
  info_ratones$Evento_artritis <- ifelse(
    info_ratones$Grupo == "Control", 0,
    ifelse(info_ratones$Dia_evento_artritis < 28, 1, 0)
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
                   "Peso_g", "IL6_pg_mL", "Dia_evento_artritis", "Evento_artritis")]
  rownames(datos) <- NULL
  datos
}

## ---- 3. Definición de los pasos del asistente ------------------------
NOMBRES_PASOS <- c("Bienvenida", "Cargar datos", "Vista previa", "Limpieza", "Análisis", "Graficar", "Descargar")
TOTAL_PASOS <- length(NOMBRES_PASOS)

## ---- 4. Interfaz de usuario (UI) -------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #f7f8fa; }
      .titulo-app { background-color: #7a1f3d; color: white; padding: 18px 24px; border-radius: 6px; margin-bottom: 18px; }
      .titulo-app h2 { margin: 0; font-weight: 600; }
      .titulo-app p { margin: 4px 0 0 0; font-size: 14px; opacity: 0.9; }
      .paso-barra { display: flex; justify-content: space-between; margin-bottom: 22px; flex-wrap: wrap; }
      .paso-item { flex: 1; text-align: center; padding: 8px 4px; font-size: 12.5px; border-bottom: 4px solid #d9d9d9; color: #999; }
      .paso-activo { border-bottom: 4px solid #7a1f3d; color: #7a1f3d; font-weight: 700; }
      .paso-completo { border-bottom: 4px solid #b98ca0; color: #7a1f3d; }
      .caja { background-color: white; padding: 24px; border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,0.08); margin-bottom: 20px; }
      .ayuda { font-size: 12.5px; color: #666; font-style: italic; }
      .btn-nav { min-width: 110px; }
      footer.pie { text-align: center; color: #999; font-size: 12px; margin-top: 30px; margin-bottom: 10px; }
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
    "Bonifaz-INCMNSZ · Herramienta de apoyo para investigación clínica y básica · No sustituye la asesoría de un bioestadístico."
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
    resultados = list(),   
    graficas = list()      
  )

  ETIQUETAS_GRAFICA <- c(
    histograma = "Histograma", boxplot = "Diagrama de caja", dispersion = "Diagrama de dispersión",
    barras = "Gráfica de barras", linea_tiempo = "Evolución en el tiempo", mapa_calor = "Mapa de calor",
    violin = "Gráfica de violín", barras_error = "Barras con error estándar (media \u00b1 EE)",
    spaghetti = "Líneas individuales por sujeto", supervivencia = "Curva de supervivencia (Kaplan-Meier)"
  )

  agregar_bitacora <- function(texto) {
    rv$bitacora <- c(rv$bitacora, paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M"), "] ", texto))
  }

  datos_actuales <- reactive({
    if (!is.null(rv$datos_limpios)) rv$datos_limpios else rv$datos_crudos
  })

  observeEvent(input$btn_restart, { session$reload() })

  output$barra_progreso <- renderUI({
    items <- lapply(seq_along(NOMBRES_PASOS), function(i) {
      clase <- if (i == rv$paso) "paso-item paso-activo" else if (i < rv$paso) "paso-item paso-completo" else "paso-item"
      div(class = clase, paste0(i, ". ", NOMBRES_PASOS[i]))
    })
    div(class = "paso-barra", items)
  })

  ir_siguiente <- function() { rv$paso <- min(TOTAL_PASOS, rv$paso + 1) }
  ir_atras <- function() { rv$paso <- max(1, rv$paso - 1) }

  botones_nav <- function(mostrar_siguiente = TRUE, id_siguiente = "btn_siguiente", texto_siguiente = "Siguiente \u2192") {
    div(style = "margin-top: 20px; display:flex; justify-content: space-between;",
        if (rv$paso > 1) actionButton("btn_atras", "\u2190 Atrás", class = "btn-nav btn-default") else div(),
        if (mostrar_siguiente) actionButton(id_siguiente, texto_siguiente, class = "btn-nav btn-primary") else div()
    )
  }

  observeEvent(input$btn_atras, { ir_atras() })
  observeEvent(input$btn_siguiente, { ir_siguiente() })

  output$cuerpo_asistente <- renderUI({

    if (rv$paso == 1) {
      div(class = "caja",
        h3("¡Bienvenido(a)!"),
        p("Esta aplicación te guiará, paso a paso, para analizar y graficar tus datos sin necesidad de escribir código en R."),
        tags$ul(
          tags$li("Paso 1: Bienvenida (aquí estás)"), tags$li("Paso 2: Cargar tu archivo de datos"),
          tags$li("Paso 3: Revisar una vista previa"), tags$li("Paso 4: Limpiar los datos (opcional)"),
          tags$li("Paso 5: Elegir y correr un análisis estadístico"), tags$li("Paso 6: Generar gráficas"),
          tags$li("Paso 7: Descargar tus resultados")
        ),
        botones_nav(texto_siguiente = "Comenzar \u2192")
      )

    } else if (rv$paso == 2) {
      div(class = "caja",
        h3("¿Deseas subir un archivo?"),
        radioButtons("tipo_carga", NULL, choices = c("Subir mi propio archivo" = "propio", "Usar datos de ejemplo" = "ejemplo"), selected = "propio"),
        conditionalPanel(
          condition = "input.tipo_carga == 'propio'",
          fileInput("archivo", "Selecciona tu archivo (.csv, .xlsx o .xls)", accept = c(".csv", ".xlsx", ".xls")),
          conditionalPanel(
            condition = "input.archivo != null",
            fluidRow(
              column(4, radioButtons("csv_sep", "Separador de columnas (solo CSV)", choices = c("Coma ( , )" = ",", "Punto y coma ( ; )" = ";", "Tabulador" = "\t"), selected = ",")),
              column(4, radioButtons("csv_dec", "Separador decimal (solo CSV)", choices = c("Punto ( . )" = ".", "Coma ( , )" = ","), selected = ".")),
              column(4, radioButtons("csv_enc", "Codificación de texto (solo CSV)", choices = c("UTF-8" = "UTF-8", "Latin1" = "Latin1"), selected = "UTF-8"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.tipo_carga == 'ejemplo'",
          actionButton("cargar_ejemplo", "Cargar datos de ejemplo", class = "btn-info")
        ),
        uiOutput("mensaje_carga"),
        botones_nav(mostrar_siguiente = FALSE),
        uiOutput("boton_siguiente_carga")
      )

    } else if (rv$paso == 3) {
      req(rv$datos_crudos)
      div(class = "caja",
        h3("Vista previa de tus datos"),
        p(sprintf("Tu archivo tiene %d filas y %d columnas.", nrow(rv$datos_crudos), ncol(rv$datos_crudos))),
        DTOutput("tabla_preview"),
        h4("Resumen por variable"),
        verbatimTextOutput("resumen_preview"),
        botones_nav()
      )

    } else if (rv$paso == 4) {
      req(rv$datos_crudos)
      df_actual_vars <- rv$datos_crudos
      vars_num <- obtener_vars_numericas(df_actual_vars)
      vars_todas <- obtener_vars_todas(df_actual_vars)
      
      div(class = "caja",
        h3("¿Quieres limpiar tus datos?"),
        p("Selecciona las opciones de limpieza que quieras aplicar. Si no necesitas limpiar nada, simplemente da clic en 'Siguiente'."),
        
        fluidRow(
          column(6,
            h4("Limpieza Básica Estándar"),
            checkboxGroupInput("opciones_limpieza", NULL,
              choices = c(
                "Eliminar filas con datos faltantes (NA)" = "quitar_na",
                "Eliminar columnas vacías (100% NA)" = "quitar_col_vacia",
                "Eliminar filas duplicadas" = "quitar_duplicados",
                "Quitar espacios en blanco al inicio/final del texto" = "trim_texto"
              ))
          ),
          column(6,
            h4("Opciones Avanzadas"),
            checkboxGroupInput("opciones_limpieza_nuevas", NULL,
              choices = c(
                "Limpiar nombres de columnas (Quitar acentos, espacios y caracteres)" = "limpiar_nombres",
                "Convertir texto a números (Forzar columnas detectadas como texto)" = "texto_a_numero",
                "Estandarizar texto en variables categóricas (Todo a MAYÚSCULAS)" = "estandarizar_texto",
                "Redondear valores numéricos" = "redondear_num",
                "Filtrar valores fuera de un rango válido" = "filtrar_rango",
                "Detectar y tratar valores atípicos (Outliers mediante Rango Intercuartílico)" = "tratar_outliers"
              ))
          )
        ),
        
        conditionalPanel(
          condition = "input.opciones_limpieza_nuevas.includes('texto_a_numero')",
          hr(), selectInput("cols_a_num", "Selecciona las columnas de texto a forzar como numéricas:", choices = vars_todas, multiple = TRUE)
        ),
        conditionalPanel(
          condition = "input.opciones_limpieza_nuevas.includes('redondear_num')",
          hr(), fluidRow(
            column(6, selectInput("cols_a_redondear", "Columnas numéricas a redondear:", choices = vars_num, multiple = TRUE)),
            column(6, numericInput("num_decimales", "Número de decimales de redondeo:", value = 2, min = 0, max = 10))
          )
        ),
        conditionalPanel(
          condition = "input.opciones_limpieza_nuevas.includes('filtrar_rango')",
          hr(), fluidRow(
            column(4, selectInput("col_rango", "Columna a filtrar:", choices = vars_num)),
            column(4, numericInput("rango_min", "Valor Mínimo Permitido:", value = 0)),
            column(4, numericInput("rango_max", "Valor Máximo Permitido:", value = 100))
          )
        ),
        conditionalPanel(
          condition = "input.opciones_limpieza_nuevas.includes('tratar_outliers')",
          hr(), fluidRow(
            column(6, selectInput("cols_outliers", "Columnas para evaluar outliers:", choices = vars_num, multiple = TRUE)),
            column(6, radioButtons("metodo_outliers", "Tratamiento de outliers detectados:",
                                   choices = c("Convertir a vacíos (NA)" = "convertir_na", "Eliminar filas completas" = "eliminar_filas"), selected = "convertir_na"))
          )
        ),
        
        hr(),
        actionButton("aplicar_limpieza", "Aplicar limpieza", class = "btn-info"),
        actionButton("sin_limpieza", "No necesito limpiar, usar datos originales", class = "btn-default"),
        uiOutput("resultado_limpieza"),
        botones_nav()
      )

    } else if (rv$paso == 5) {
      ## ---------------- PASO 5: ANÁLISIS (Con las 4 nuevas extensiones) ----------------
      req(datos_actuales())
      df <- datos_actuales()
      vars_num <- obtener_vars_numericas(df)
      vars_cat <- obtener_vars_categoricas(df)

      div(class = "caja",
        h3("¿Qué tipo de análisis quieres hacer?"),
        radioButtons("tipo_analisis", NULL,
          choices = c(
            "Estadística descriptiva (promedios, medianas, etc.)" = "descriptivo",
            "Prueba de Normalidad Formal (Shapiro-Wilk)" = "normalidad",
            "Comparar grupos independientes (2 o más grupos)" = "comparar",
            "Comparar datos pareados / repetidos (Antes vs. Después)" = "pareados",
            "Relación entre dos variables (correlación)" = "correlacion",
            "Asociación de variables categóricas (Chi-cuadrado / Fisher)" = "contingencia",
            "Tabla de frecuencias simple (variable categórica)" = "frecuencias",
            "Evolución promedio en el tiempo" = "longitudinal",
            "Modelo de Regresión Lineal Simple" = "regresion"
          )),
        hr(),

        conditionalPanel(condition = "input.tipo_analisis == 'descriptivo'",
          selectInput("desc_var", "Variable numérica a describir:", choices = vars_num),
          selectizeInput("desc_var_grupo", "Agrupar por (opcional):", choices = c("(Sin agrupar)" = "", vars_cat)),
          actionButton("run_desc", "Calcular Descriptivos", class = "btn-primary"),
          tableOutput("out_desc")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'normalidad'",
          selectInput("norm_var", "Variable numérica a evaluar con Shapiro-Wilk:", choices = vars_num),
          actionButton("run_norm", "Evaluar Normalidad", class = "btn-primary"),
          verbatimTextOutput("out_norm")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'comparar'",
          selectInput("comp_grupo", "Variable de grupo (categórica):", choices = vars_cat),
          selectInput("comp_num", "Variable numérica a comparar:", choices = vars_num),
          actionButton("run_comp", "Comparar grupos independientes", class = "btn-primary"),
          verbatimTextOutput("out_comp"),
          tableOutput("out_comp_tabla")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'pareados'",
          selectInput("pareada_antes", "Variable numérica base (Antes):", choices = vars_num),
          selectInput("pareada_despues", "Variable numérica de seguimiento (Después):", choices = vars_num),
          actionButton("run_pareados", "Efectuar Contraste Pareado", class = "btn-primary"),
          verbatimTextOutput("out_pareados")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'correlacion'",
          selectInput("corr_var1", "Primera variable numérica:", choices = vars_num),
          selectInput("corr_var2", "Segunda variable numérica:", choices = vars_num, selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          actionButton("run_corr", "Calcular correlación", class = "btn-primary"),
          verbatimTextOutput("out_corr")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'contingencia'",
          selectInput("chi_var1", "Primera variable categórica:", choices = vars_cat),
          selectInput("chi_var2", "Segunda variable categórica:", choices = vars_cat, selected = if (length(vars_cat) > 1) vars_cat[2] else vars_cat[1]),
          actionButton("run_contingencia", "Analizar Asociación", class = "btn-primary"),
          verbatimTextOutput("out_contingencia")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'frecuencias'",
          selectInput("freq_var", "Variable categórica:", choices = vars_cat),
          actionButton("run_freq", "Calcular frecuencias", class = "btn-primary"),
          tableOutput("out_freq")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'longitudinal'",
          selectInput("long_tiempo", "Variable de tiempo (ej. Día):", choices = vars_num),
          selectInput("long_grupo", "Variable de grupo:", choices = vars_cat),
          selectInput("long_resp", "Variable de respuesta:", choices = vars_num, selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          actionButton("run_long", "Calcular evolución en el tiempo", class = "btn-primary"),
          tableOutput("out_long")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'regresion'",
          selectInput("reg_x", "Variable Independiente / Predictora (X):", choices = vars_num),
          selectInput("reg_y", "Variable Dependiente / Resultado (Y):", choices = vars_num, selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          actionButton("run_regresion", "Ajustar Modelo Lineal", class = "btn-primary"),
          verbatimTextOutput("out_regresion")
        ),
        botones_nav()
      )

    } else if (rv$paso == 6) {
      req(datos_actuales())
      df <- datos_actuales()
      vars_num <- obtener_vars_numericas(df)
      vars_cat <- obtener_vars_categoricas(df)
      vars_todas <- obtener_vars_todas(df)

      div(class = "caja",
        h3("¿Quieres graficar tus datos?"),
        radioButtons("tipo_grafica", NULL,
          choices = c(
            "Histograma" = "histograma", "Diagrama de caja" = "boxplot", "Gráfica de violín" = "violin",
            "Diagrama de dispersión" = "dispersion", "Gráfica de barras" = "barras",
            "Barras con error estándar" = "barras_error", "Mapa de calor" = "mapa_calor",
            "Evolución en el tiempo" = "linea_tiempo", "Líneas individuales" = "spaghetti", "Supervivencia (Kaplan-Meier)" = "supervivencia"
          )),
        hr(),

        conditionalPanel(condition = "input.tipo_grafica == 'histograma'", selectInput("hist_var", "Variable numérica:", choices = vars_num)),
        conditionalPanel(condition = "input.tipo_grafica == 'boxplot'", selectInput("box_grupo", "Variable de grupo:", choices = vars_cat), selectInput("box_num", "Variable numérica:", choices = vars_num)),
        conditionalPanel(condition = "input.tipo_grafica == 'violin'", selectInput("violin_grupo", "Variable de grupo:", choices = vars_cat), selectInput("violin_num", "Variable numérica:", choices = vars_num)),
        conditionalPanel(condition = "input.tipo_grafica == 'dispersion'", selectInput("disp_x", "Variable X:", choices = vars_num), selectInput("disp_y", "Variable Y:", choices = vars_num, selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]), selectizeInput("disp_color", "Colorear por (opcional):", choices = c("(Ninguno)" = "", vars_cat))),
        conditionalPanel(condition = "input.tipo_grafica == 'barras'", selectInput("barras_var", "Variable categórica:", choices = vars_cat)),
        conditionalPanel(condition = "input.tipo_grafica == 'barras_error'", selectInput("barras_error_grupo", "Variable de grupo:", choices = vars_cat), selectInput("barras_error_num", "Variable numérica:", choices = vars_num)),
        conditionalPanel(condition = "input.tipo_grafica == 'mapa_calor'", selectInput("heat_fila", "Variable para las filas:", choices = vars_cat), selectInput("heat_columna", "Variable para las columnas:", choices = vars_cat, selected = if (length(vars_cat) > 1) vars_cat[2] else vars_cat[1]), selectInput("heat_valor", "Variable numérica a promediar:", choices = vars_num)),
        conditionalPanel(condition = "input.tipo_grafica == 'linea_tiempo'", selectInput("lt_tiempo", "Variable de tiempo:", choices = vars_num), selectInput("lt_grupo", "Variable de grupo:", choices = vars_cat), selectInput("lt_resp", "Variable de respuesta:", choices = vars_num, selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1])),
        conditionalPanel(condition = "input.tipo_grafica == 'spaghetti'", selectInput("sp_id", "Variable ID de sujeto:", choices = vars_todas), selectInput("sp_tiempo", "Variable de tiempo:", choices = vars_num), selectInput("sp_resp", "Variable de respuesta:", choices = vars_num, selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]), selectizeInput("sp_grupo", "Colorear por grupo (opcional):", choices = c("(Ninguno)" = "", vars_cat))),
        conditionalPanel(condition = "input.tipo_grafica == 'supervivencia'", selectInput("km_id", "Variable ID de sujeto:", choices = vars_todas), selectInput("km_tiempo", "Variable de tiempo hasta evento:", choices = vars_num), selectInput("km_evento", "Variable de evento (1/0):", choices = vars_num), selectizeInput("km_grupo", "Comparar por grupo:", choices = c("(Ninguno)" = "", vars_cat))),

        actionButton("generar_grafica", "Generar gráfica", class = "btn-primary"),
        br(), br(),
        plotOutput("grafica_principal", height = "420px"),
        uiOutput("texto_resultado_grafica"),
        uiOutput("boton_descargar_grafica"),
        botones_nav()
      )

    } else if (rv$paso == 7) {
      div(class = "caja",
        h3("Descarga tus resultados"),
        fluidRow(
          column(4, h4("Datos"), downloadButton("descargar_csv", "Descargar datos (.csv)", class = "btn-info"), br(), br(), downloadButton("descargar_xlsx", "Descargar datos (.xlsx)", class = "btn-info")),
          column(4, h4("Reporte en PDF"), downloadButton("descargar_pdf", "Descargar reporte completo (.pdf)", class = "btn-danger")),
          column(4, h4("Bitácora de la sesión"), downloadButton("descargar_reporte", "Descargar bitácora (.txt)", class = "btn-info"))
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
      div(style = "color: #1a7a1a; margin-top: 10px; font-weight: 600;", sprintf("\u2713 Datos cargados correctamente: %d filas x %d columnas.", nrow(rv$datos_crudos), ncol(rv$datos_crudos)))
    } else {
      div(style = "color: #888; margin-top: 10px;", "Aún no se han cargado datos.")
    }
  })

  output$boton_siguiente_carga <- renderUI({
    if (!is.null(rv$datos_crudos)) {
      div(style = "text-align: right; margin-top: -46px;", actionButton("btn_siguiente_desde_carga", "Siguiente \u2192", class = "btn-nav btn-primary"))
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
        read.csv(input$archivo$datapath, sep = sep, dec = dec, fileEncoding = enc, stringsAsFactors = FALSE, na.strings = c("NA", "", "NaN", "na", "N/A"))
      } else if (ext %in% c("xlsx", "xls")) {
        as.data.frame(readxl::read_excel(input$archivo$datapath))
      } else {
        stop("Formato no soportado.")
      }
    }, error = function(e) {
      showNotification(paste("Error al leer:", e$message), type = "error", duration = 8)
      NULL
    })

    if (!is.null(df)) {
      rv$datos_crudos <- df
      rv$datos_limpios <- NULL
      agregar_bitacora(sprintf("Se cargó el archivo '%s' (%d filas, %d columnas).", input$archivo$name, nrow(df), ncol(df)))
    }
  })

  observeEvent(input$cargar_ejemplo, {
    rv$datos_crudos <- generar_datos_ejemplo()
    rv$datos_limpios <- NULL
    agregar_bitacora("Se cargaron los datos de ejemplo.")
  })

  output$tabla_preview <- renderDT({ req(rv$datos_crudos); datatable(rv$datos_crudos, options = list(pageLength = 6, scrollX = TRUE)) })
  output$resumen_preview <- renderPrint({ req(rv$datos_crudos); summary(rv$datos_crudos) })

  ## =====================================================================
  ## PASO 4: Limpieza
  ## =====================================================================
  observeEvent(input$sin_limpieza, {
    rv$datos_limpios <- rv$datos_crudos
    rv$limpieza_aplicada <- FALSE
    agregar_bitacora("El usuario decidió continuar sin aplicar limpieza.")
    showNotification("Se usarán los datos originales sin cambios.", type = "message")
  })

  observeEvent(input$aplicar_limpieza, {
    req(rv$datos_crudos)
    df <- rv$datos_crudos
    filas_antes <- nrow(df)
    cols_antes <- ncol(df)
    acciones <- c()

    if ("trim_texto" %in% input$opciones_limpieza) {
      df[] <- lapply(df, function(x) if (is.character(x)) trimws(x) else x)
      acciones <- c(acciones, "Se removieron espacios al inicio/final del texto")
    }

    if ("limpiar_nombres" %in% input$opciones_limpieza_nuevas) {
      viejos_nombres <- names(df)
      nuevos_nombres <- iconv(viejos_nombres, to = "ASCII//TRANSLIT") 
      nuevos_nombres <- gsub("[^[:alnum:]_]", "_", nuevos_nombres)   
      nuevos_nombres <- gsub("_+", "_", nuevos_nombres)              
      nuevos_nombres <- gsub("(^_|_$)", "", nuevos_nombres)          
      names(df) <- nuevos_nombres
      acciones <- c(acciones, "Nombres de columnas estandarizados")
    }

    if ("texto_a_numero" %in% input$opciones_limpieza_nuevas && !is.null(input$cols_a_num)) {
      cols_interes <- intersect(input$cols_a_num, names(df))
      for(col in cols_interes) {
        df[[col]] <- as.numeric(df[[col]])
      }
      acciones <- c(acciones, paste("Columnas forzadas a numéricas:", paste(cols_interes, collapse = ", ")))
    }

    if ("estandarizar_texto" %in% input$opciones_limpieza_nuevas) {
      cols_char <- names(df)[sapply(df, is.character)]
      for(col in cols_char) {
        df[[col]] <- toupper(df[[col]])
      }
      acciones <- c(acciones, "Variables de texto convertidas a MAYÚSCULAS")
    }

    if ("filtrar_rango" %in% input$opciones_limpieza_nuevas && !is.null(input$col_rango)) {
      col_r <- input$col_rango
      if (col_r %in% names(df) && is.numeric(df[[col_r]])) {
        min_v <- input$rango_min
        max_v <- input$rango_max
        df <- df[!is.na(df[[col_r]]) & df[[col_r]] >= min_v & df[[col_r]] <= max_v, , drop = FALSE]
        acciones <- c(acciones, sprintf("Filtrados valores en '%s' fuera del rango [%.2f, %.2f]", col_r, min_v, max_v))
      }
    }

    if ("tratar_outliers" %in% input$opciones_limpieza_nuevas && !is.null(input$cols_outliers)) {
      cols_out <- intersect(input$cols_outliers, names(df))
      total_outliers <- 0
      for (col in cols_out) {
        if (is.numeric(df[[col]])) {
          qnt <- quantile(df[[col]], probs = c(.25, .75), na.rm = TRUE)
          H <- 1.5 * IQR(df[[col]], na.rm = TRUE)
          limite_inf <- qnt[1] - H
          limite_sup <- qnt[2] + H
          atipicos_idx <- which(df[[col]] < limite_inf | df[[col]] > limite_sup)
          total_outliers <- total_outliers + length(atipicos_idx)
          if (length(atipicos_idx) > 0) {
            if (input$metodo_outliers == "convertir_na") {
              df[atipicos_idx, col] <- NA
            } else if (input$metodo_outliers == "eliminar_filas") {
              df <- df[-atipicos_idx, , drop = FALSE]
            }
          }
        }
      }
      acciones <- c(acciones, sprintf("Detectados %d outliers (Tratamiento: %s)", total_outliers, input$metodo_outliers))
    }

    if ("redondear_num" %in% input$opciones_limpieza_nuevas && !is.null(input$cols_a_redondear)) {
      cols_red <- intersect(input$cols_a_redondear, names(df))
      dec <- input$num_decimales
      for(col in cols_red) {
        if (is.numeric(df[[col]])) df[[col]] <- round(df[[col]], digits = dec)
      }
      acciones <- c(acciones, paste("Columnas redondeadas a", dec, "decimales"))
    }

    if ("quitar_col_vacia" %in% input$opciones_limpieza) {
      cols_vacias <- sapply(df, function(x) all(is.na(x)))
      if (any(cols_vacias)) df <- df[, !cols_vacias, drop = FALSE]
      acciones <- c(acciones, sprintf("Se eliminaron %d columna(s) vacías", sum(cols_vacias)))
    }
    if ("quitar_duplicados" %in% input$opciones_limpieza) {
      n_antes <- nrow(df)
      df <- df[!duplicated(df), , drop = FALSE]
      acciones <- c(acciones, sprintf("Se eliminaron %d fila(s) duplicadas", n_antes - nrow(df)))
    }
    if ("quitar_na" %in% input$opciones_limpieza) {
      n_antes <- nrow(df)
      df <- df[complete.cases(df), , drop = FALSE]
      acciones <- c(acciones, sprintf("Se eliminaron %d fila(s) con NA", n_antes - nrow(df)))
    }

    rv$datos_limpios <- df
    rv$limpieza_aplicada <- TRUE
    agregar_bitacora(paste0("Limpieza ejecutada: ", paste(acciones, collapse = "; "), "."))

    output$resultado_limpieza <- renderUI({
      div(style = "margin-top: 15px; padding: 12px; background-color: #eef7ee; border-radius: 6px;",
          h4("Resumen del Procesamiento de Limpieza"),
          p(sprintf("Antes: %d filas x %d columnas. Después: %d filas x %d columnas.", filas_antes, cols_antes, nrow(df), ncol(df))),
          if (length(acciones) > 0) tags$ul(lapply(acciones, tags$li)) else p("Sin modificaciones.")
      )
    })
  })

  ## =====================================================================
  ## PASO 5: Lógica Estadística de Servidor
  ## =====================================================================
  
  # --- Descriptivos ---
  observeEvent(input$run_desc, {
    df <- datos_actuales()
    req(df, input$desc_var)
    calcular_resumen <- function(x) {
      x <- x[!is.na(x)]
      data.frame(n = length(x), Media = round(mean(x), 2), Mediana = round(median(x), 2), DE = round(sd(x), 2), Minimo = round(min(x), 2), Maximo = round(max(x), 2))
    }
    if (!is.null(input$desc_var_grupo) && input$desc_var_grupo != "") {
      resumen <- do.call(rbind, lapply(split(df[[input$desc_var]], df[[input$desc_var_grupo]]), calcular_resumen))
      resumen <- cbind(Grupo = rownames(resumen), resumen)
      rownames(resumen) <- NULL
    } else {
      resumen <- calcular_resumen(df[[input$desc_var]])
    }
    output$out_desc <- renderTable(resumen)
    rv$resultados$descriptivo <- list(titulo = sprintf("Estadística descriptiva: %s", input$desc_var), tabla = resumen)
    agregar_bitacora(sprintf("Estadística descriptiva procesada para '%s'.", input$desc_var))
  })

  # --- [NUEVO] Shapiro-Wilk ---
  observeEvent(input$run_norm, {
    df <- datos_actuales()
    req(df, input$norm_var)
    valores <- na.omit(df[[input$norm_var]])
    
    if (length(valores) >= 3 && length(valores) <= 5000) {
      prueba <- shapiro.test(valores)
      p_val <- prueba$p.value
      texto <- paste0(
        "Prueba de Normalidad de Shapiro-Wilk para la variable: '", input$norm_var, "'\n\n",
        interpretar_p(p_val), "\n",
        if(p_val > 0.05) "Conclusión: Los datos se comportan de forma aproximadamente NORMAL (Paramétricos)." 
        else "Conclusión: Los datos NO se comportan de forma normal (No Paramétricos)."
      )
    } else {
      texto <- "Error: El tamaño de la muestra debe estar entre 3 y 5000 observaciones para Shapiro-Wilk."
    }
    output$out_norm <- renderText(texto)
    rv$resultados$normalidad <- list(titulo = paste("Normalidad:", input$norm_var), texto = texto)
    agregar_bitacora(sprintf("Prueba de normalidad ejecutada para '%s'.", input$norm_var))
  })

  # --- Comparación de Grupos Independientes ---
  observeEvent(input$run_comp, {
    df <- datos_actuales()
    req(df, input$comp_grupo, input$comp_num)
    df_sub <- df[!is.na(df[[input$comp_num]]) & !is.na(df[[input$comp_grupo]]), ]
    grupos <- unique(df_sub[[input$comp_grupo]])
    n_grupos <- length(grupos)
    resultado_texto <- NULL; tabla_resultado <- NULL

    if (n_grupos < 2) {
      resultado_texto <- "Faltan grupos suficientes para realizar contrastes estadísticos independientes."
    } else {
      valores_por_grupo <- split(df_sub[[input$comp_num]], df_sub[[input$comp_grupo]])
      p_normalidad <- sapply(valores_por_grupo, function(x) if (length(x) >= 3 && length(x) <= 5000) tryCatch(shapiro.test(x)$p.value, error = function(e) NA) else NA)
      es_normal <- all(!is.na(p_normalidad) & p_normalidad > 0.05)
      formula_comp <- as.formula(paste0("`", input$comp_num, "` ~ `", input$comp_grupo, "`"))

      if (n_grupos == 2) {
        prueba <- if (es_normal) t.test(formula_comp, data = df_sub) else wilcox.test(formula_comp, data = df_sub)
        resultado_texto <- paste0("Comparación de 2 grupos con ", if(es_normal)"T-Student"else"Wilcoxon (Mann-Whitney)", ".\n\n", interpretar_p(prueba$p.value))
      } else {
        if (es_normal) {
          modelo <- aov(formula_comp, data = df_sub)
          p_valor <- summary(modelo)[[1]][["Pr(>F)"]][1]
          posthoc <- TukeyHSD(modelo)[[1]]
          tabla_resultado <- data.frame(Comparacion = rownames(posthoc), round(as.data.frame(posthoc), 4))
        } else {
          kt <- kruskal.test(formula_comp, data = df_sub)
          p_valor <- kt$p.value
          ph <- pairwise.wilcox.test(df_sub[[input$comp_num]], df_sub[[input$comp_grupo]], p.adjust.method = "BH")
          tabla_resultado <- as.data.frame(as.table(ph$p.value))
          names(tabla_resultado) <- c("Grupo_1", "Grupo_2", "p_ajustada")
          tabla_resultado <- tabla_resultado[!is.na(tabla_resultado$p_ajustada), ]
        }
        resultado_texto <- paste0("Comparación de ", n_grupos, " grupos mediante ", if(es_normal)"ANOVA"else"Kruskal-Wallis", ".\n\n", interpretar_p(p_valor))
      }
    }
    output$out_comp <- renderText(resultado_texto)
    output$out_comp_tabla <- renderTable(tabla_resultado)
    rv$resultados$comparacion <- list(titulo = sprintf("Comparación independiente: %s", input$comp_num), texto = resultado_texto, tabla = tabla_resultado)
    agregar_bitacora(sprintf("Contraste de grupos independientes para '%s'.", input$comp_num))
  })

  # --- [NUEVO] Datos Pareados ---
  observeEvent(input$run_pareados, {
    df <- datos_actuales()
    req(df, input$pareada_antes, input$pareada_despues)
    
    pares <- na.omit(data.frame(antes = df[[input$pareada_antes]], despues = df[[input$pareada_despues]]))
    
    if (nrow(pares) >= 3) {
      diferencia <- pares$despues - pares$antes
      es_normal <- tryCatch(shapiro.test(diferencia)$p.value > 0.05, error = function(e) FALSE)
      
      if (es_normal) {
        prueba <- t.test(pares$antes, pares$despues, paired = TRUE)
        tipo_p <- "Prueba t de Student para muestras pareadas (Diferencias Normales)"
      } else {
        prueba <- wilcox.test(pares$antes, pares$despues, paired = TRUE)
        tipo_p <- "Prueba de Rangos con Signo de Wilcoxon para muestras pareadas (No paramétrica)"
      }
      
      texto <- paste0(
        "Análisis de Datos Pareados (Antes vs. Después):\n",
        "Variables: ", input$pareada_antes, " vs. ", input$pareada_despues, "\n",
        "Muestras emparejadas válidas: n = ", nrow(pares), "\n",
        "Método utilizado: ", tipo_p, "\n\n",
        interpretar_p(prueba$p.value)
      )
    } else {
      texto <- "Error: Datos insuficientes (se requieren al menos 3 filas emparejadas sin vacíos)."
    }
    output$out_pareados <- renderText(texto)
    rv$resultados$pareados <- list(titulo = "Análisis Pareado", texto = texto)
    agregar_bitacora(sprintf("Análisis de datos pareados entre '%s' y '%s'.", input$pareada_antes, input$pareada_despues))
  })

  # --- Correlación ---
  observeEvent(input$run_corr, {
    df <- datos_actuales()
    req(df, input$corr_var1, input$corr_var2)
    df_sub <- df[!is.na(df[[input$corr_var1]]) & !is.na(df[[input$corr_var2]]), ]
    if (input$corr_var1 == input$corr_var2) {
      texto <- "Por favor selecciona dos variables distintas."
    } else {
      prueba <- cor.test(df_sub[[input$corr_var1]], df_sub[[input$corr_var2]], method = "spearman")
      texto <- sprintf("Coeficiente de correlación de Spearman r = %.3f\n\n%s", prueba$estimate, interpretar_p(prueba$p.value))
    }
    output$out_corr <- renderText(texto)
    rv$resultados$correlacion <- list(titulo = "Correlación", texto = texto)
    agregar_bitacora(sprintf("Correlación calculada entre '%s' y '%s'.", input$corr_var1, input$corr_var2))
  })

  # --- [NUEVO] Tablas de Contingencia (Chi-Cuadrado / Fisher) ---
  observeEvent(input$run_contingencia, {
    df <- datos_actuales()
    req(df, input$chi_var1, input$chi_var2)
    
    tabla_c <- table(df[[input$chi_var1]], df[[input$chi_var2]])
    
    if (nrow(tabla_c) >= 2 && ncol(tabla_c) >= 2) {
      test_chisq <- chisq.test(tabla_c)
      usar_fisher <- any(test_chisq$expected < 5)
      
      if (usar_fisher) {
        prueba <- tryCatch(fisher.test(tabla_c), error = function(e) { 
          return(tryCatch(fisher.test(tabla_c, simulate.p.value = TRUE), error = function(ex) NULL)) 
        })
        nombre_test <- "Prueba Exacta de Fisher (Detectadas celdas esperadas < 5)"
      } else {
        prueba <- test_chisq
        nombre_test <- "Prueba de Chi-cuadrado de Pearson"
      }
      
      if(!is.null(prueba)) {
        texto <- paste0(
          "Análisis de Asociación Categórica:\n",
          "Variables: ", input$chi_var1, " x ", input$chi_var2, "\n",
          "Prueba ejecutada: ", nombre_test, "\n\n",
          interpretar_p(prueba$p.value), "\n\nTabla de frecuencias observadas:\n"
        )
        # Formatear matriz en pantalla
        matriz_texto <- capture.output(print(tabla_c))
        texto <- paste(c(texto, matriz_texto), collapse = "\n")
      } else {
        texto <- "No se pudo resolver el test estadístico exacto con la distribución actual."
      }
    } else {
      texto <- "Error: Ambas variables deben tener al menos 2 categorías distintas presentes."
    }
    
    output$out_contingencia <- renderText(texto)
    rv$resultados$contingencia <- list(titulo = "Asociación Categórica", texto = texto)
    agregar_bitacora(sprintf("Prueba de asociación categórica realizada entre '%s' y '%s'.", input$chi_var1, input$chi_var2))
  })

  # --- Frecuencias simples ---
  observeEvent(input$run_freq, {
    df <- datos_actuales()
    req(df, input$freq_var)
    tabla <- as.data.frame(table(df[[input$freq_var]], useNA = "ifany"))
    names(tabla) <- c("Categoria", "Frecuencia")
    tabla$Porcentaje <- round(100 * tabla$Frecuencia / sum(tabla$Frecuencia), 1)
    output$out_freq <- renderTable(tabla)
    rv$resultados$frecuencias <- list(titulo = "Frecuencias", tabla = tabla)
    agregar_bitacora(sprintf("Frecuencias obtenidas para '%s'.", input$freq_var))
  })

  # --- Evolución temporal ---
  observeEvent(input$run_long, {
    df <- datos_actuales()
    req(df, input$long_tiempo, input$long_grupo, input$long_resp)
    resumen <- df %>% dplyr::filter(!is.na(.data[[input$long_resp]])) %>%
      dplyr::group_by(.data[[input$long_tiempo]], .data[[input$long_grupo]]) %>%
      dplyr::summarise(n = dplyr::n(), Promedio = round(mean(.data[[input$long_resp]], na.rm = TRUE), 2), Error_estandar = round(sd(.data[[input$long_resp]], na.rm = TRUE) / sqrt(dplyr::n()), 2), .groups = "drop")
    output$out_long <- renderTable(resumen)
    rv$resultados$longitudinal <- list(titulo = "Evolución Temporal", tabla = resumen)
    agregar_bitacora(sprintf("Evolución calculada para '%s'.", input$long_resp))
  })

  # --- [NUEVO] Regresión Lineal Simple ---
  observeEvent(input$run_regresion, {
    df <- datos_actuales()
    req(df, input$reg_x, input$reg_y)
    
    if (input$reg_x != input$reg_y) {
      formula_reg <- as.formula(paste0("`", input$reg_y, "` ~ `", input$reg_x, "`"))
      modelo <- lm(formula_reg, data = df)
      sum_mod <- summary(modelo)
      p_val <- sum_mod$coefficients[2, 4]
      r2 <- sum_mod$r.squared
      
      texto <- paste0(
        "Modelo de Regresión Lineal Simple Ajustado:\n",
        "Variable de respuesta (Y): ", input$reg_y, "\n",
        "Variable predictora  (X): ", input$reg_x, "\n\n",
        sprintf("Ecuación estimada: %s = %.3f + (%.3f * %s)\n", input$reg_y, sum_mod$coefficients[1,1], sum_mod$coefficients[2,1], input$reg_x),
        sprintf("Coeficiente de Determinación (R²): %.4f (Explica el %.1f%% de la varianza del resultado)\n\n", r2, r2 * 100),
        "Significancia del predictor:\n",
        interpretar_p(p_val)
      )
    } else {
      texto <- "Error: La variable independiente (X) y la variable dependiente (Y) deben ser diferentes."
    }
    output$out_regresion <- renderText(texto)
    rv$resultados$regresion <- list(titulo = paste("Regresión Lineal:", input$reg_y), texto = texto)
    agregar_bitacora(sprintf("Modelo de regresión lineal simple ajustado para Y=%s e X=%s.", input$reg_y, input$reg_x))
  })

  ## =====================================================================
  ## PASO 6: Graficar
  ## =====================================================================
  observeEvent(input$generar_grafica, {
    df <- datos_actuales()
    req(df, input$tipo_grafica)
    rv$km_texto <- NULL

    grafica <- tryCatch({
      if (input$tipo_grafica == "histograma") {
        req(input$hist_var)
        ggplot(df, aes(x = .data[[input$hist_var]])) + geom_histogram(bins = 20, fill = "#7a1f3d", color = "white", alpha = 0.9) + labs(title = paste("Histograma de", input$hist_var)) + theme_minimal(base_size = 14)
      } else if (input$tipo_grafica == "boxplot") {
        req(input$box_grupo, input$box_num)
        ggplot(df, aes(x = factor(.data[[input$box_grupo]]), y = .data[[input$box_num]], fill = factor(.data[[input$box_grupo]]))) + geom_boxplot(alpha = 0.85) + theme_minimal(base_size = 14)
      } else if (input$tipo_grafica == "violin") {
        req(input$violin_grupo, input$violin_num)
        ggplot(df, aes(x = factor(.data[[input$violin_grupo]]), y = .data[[input$violin_num]], fill = factor(.data[[input$violin_grupo]]))) + geom_violin(alpha = 0.8, trim = FALSE) + geom_boxplot(width = 0.12, fill = "white", alpha = 0.7) + theme_minimal(base_size = 14)
      } else if (input$tipo_grafica == "dispersion") {
        req(input$disp_x, input$disp_y)
        ggplot(df, aes(x = .data[[input$disp_x]], y = .data[[input$disp_y]])) + geom_point(size = 2.6, color = "#7a1f3d") + theme_minimal(base_size = 14)
      } else if (input$tipo_grafica == "barras") {
        req(input$barras_var)
        ggplot(df, aes(x = factor(.data[[input$barras_var]]))) + geom_bar(fill = "#7a1f3d") + theme_minimal(base_size = 14)
      } else if (input$tipo_grafica == "barras_error") {
        req(input$barras_error_grupo, input$barras_error_num)
        resumen <- df %>% dplyr::filter(!is.na(.data[[input$barras_error_num]])) %>% dplyr::group_by(.data[[input$barras_error_grupo]]) %>% dplyr::summarise(Promedio = mean(.data[[input$barras_error_num]], na.rm = TRUE), EE = sd(.data[[input$barras_error_num]], na.rm = TRUE) / sqrt(dplyr::n()), .groups = "drop")
        ggplot(resumen, aes(x = factor(.data[[input$barras_error_grupo]]), y = Promedio, fill = factor(.data[[input$barras_error_grupo]]))) + geom_col() + geom_errorbar(aes(ymin = Promedio - EE, ymax = Promedio + EE), width = 0.25) + theme_minimal(base_size = 14)
      } else if (input$tipo_grafica == "mapa_calor") {
        req(input$heat_fila, input$heat_columna, input$heat_valor)
        resumen <- df %>% dplyr::filter(!is.na(.data[[input$heat_valor]])) %>% dplyr::group_by(.data[[input$heat_fila]], .data[[input$heat_columna]]) %>% dplyr::summarise(Promedio = mean(.data[[input$heat_valor]], na.rm = TRUE), .groups = "drop")
        ggplot(resumen, aes(x = factor(.data[[input$heat_columna]]), y = factor(.data[[input$heat_fila]]), fill = Promedio)) + geom_tile() + scale_fill_gradient(low = "#f7ecef", high = "#7a1f3d") + theme_minimal(base_size = 14)
      } else if (input$tipo_grafica == "linea_tiempo") {
        req(input$lt_tiempo, input$lt_grupo, input$lt_resp)
        resumen <- df %>% dplyr::filter(!is.na(.data[[input$lt_resp]])) %>% dplyr::group_by(.data[[input$lt_tiempo]], .data[[input$lt_grupo]]) %>% dplyr::summarise(Promedio = mean(.data[[input$lt_resp]], na.rm = TRUE), EE = sd(.data[[input$lt_resp]], na.rm = TRUE) / sqrt(dplyr::n()), .groups = "drop")
        ggplot(resumen, aes(x = .data[[input$lt_tiempo]], y = Promedio, color = factor(.data[[input$lt_grupo]]), group = factor(.data[[input$lt_grupo]]))) + geom_line() + geom_point() + geom_errorbar(aes(ymin = Promedio - EE, ymax = Promedio + EE), width = 0.4) + theme_minimal(base_size = 14)
      } else if (input$tipo_grafica == "spaghetti") {
        req(input$sp_id, input$sp_tiempo, input$sp_resp)
        ggplot(df, aes(x = .data[[input$sp_tiempo]], y = .data[[input$sp_resp]], group = .data[[input$sp_id]])) + geom_line(alpha = 0.5, color = "#7a1f3d") + theme_minimal(base_size = 14)
      } else if (input$tipo_grafica == "supervivencia") {
        req(input$km_id, input$km_tiempo, input$km_evento)
        df_sub <- df %>% dplyr::distinct(.data[[input$km_id]], .keep_all = TRUE)
        formula_km <- as.formula(paste0("survival::Surv(`", input$km_tiempo, "`, `", input$km_evento, "`) ~ 1"))
        ajuste <- survival::survfit(formula_km, data = df_sub)
        df_km <- data.frame(Tiempo = ajuste$time, Supervivencia = ajuste$surv, Grupo = "Todos")
        ggplot(df_km, aes(x = Tiempo, y = Supervivencia)) + geom_step(color = "#7a1f3d") + ylim(0, 1) + theme_minimal(base_size = 14)
      }
    }, error = function(e) {
      showNotification(paste("Error gráfico:", e$message), type = "error")
      NULL
    })

    rv$ultima_grafica <- grafica
    if (!is.null(grafica)) {
      tmp <- rv$graficas
      tmp[[input$tipo_grafica]] <- list(plot = grafica, etiqueta = ETIQUETAS_GRAFICA[[input$tipo_grafica]])
      rv$graficas <- tmp
    }
  })

  output$grafica_principal <- renderPlot({ req(rv$ultima_grafica); rv$ultima_grafica })
  output$boton_descargar_grafica <- renderUI({ req(rv$ultima_grafica); downloadButton("descargar_grafica_png", "Descargar gráfica (.png)", class = "btn-info") })

  output$descargar_grafica_png <- downloadHandler(
    filename = function() paste0("grafica_", Sys.Date(), ".png"),
    content = function(file) { ggsave(file, plot = rv$ultima_grafica, width = 9, height = 6) }
  )

  ## =====================================================================
  ## PASO 7: Descargas e Impresor PDF
  ## =====================================================================
  output$descargar_csv <- downloadHandler(filename = function() paste0("datos_", Sys.Date(), ".csv"), content = function(file) { write.csv(datos_actuales(), file, row.names = FALSE) })
  output$descargar_xlsx <- downloadHandler(filename = function() paste0("datos_", Sys.Date(), ".xlsx"), content = function(file) { writexl::write_xlsx(datos_actuales(), file) })

  pagina_texto_pdf <- function(titulo, lineas, lineas_por_pagina = 48) {
    if (length(lineas) == 0) lineas <- ""
    bloques <- split(lineas, ceiling(seq_along(lineas) / lineas_por_pagina))
    for (bloque in bloques) {
      grid::grid.newpage()
      grid::grid.text(titulo, x = 0.03, y = 0.97, just = c("left", "top"), gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#7a1f3d"))
      grid::grid.text(paste(bloque, collapse = "\n"), x = 0.03, y = 0.92, just = c("left", "top"), gp = grid::gpar(fontsize = 8.5, fontfamily = "mono"))
    }
  }

  output$descargar_pdf <- downloadHandler(
    filename = function() paste0("reporte_", Sys.Date(), ".pdf"),
    content = function(file) {
      df_actual <- datos_actuales()
      grDevices::pdf(file, width = 8.5, height = 11)
      on.exit(grDevices::dev.off(), add = TRUE)

      grid::grid.newpage()
      grid::grid.text("Bonifaz-INCMNSZ", x = 0.5, y = 0.72, gp = grid::gpar(fontsize = 28, fontface = "bold", col = "#7a1f3d"))
      grid::grid.text("Reporte Clínico Integrado", x = 0.5, y = 0.65, gp = grid::gpar(fontsize = 16))

      if (!is.null(df_actual)) {
        resumen_lineas <- capture.output(summary(df_actual))
        pagina_texto_pdf("Resumen de Datos", resumen_lineas)
      }
      
      # Adjuntar bitácora al reporte PDF
      if (length(rv$bitacora) > 0) {
        pagina_texto_pdf("Bitácora de Procesamiento Clínico", rv$bitacora)
      }
      
      for (g in rv$graficas) {
        gridExtra::grid.arrange(g$plot, top = grid::textGrob(g$etiqueta, gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#7a1f3d")))
      }
    }
  )

  output$descargar_reporte <- downloadHandler(filename = function() paste0("bitacora_", Sys.Date(), ".txt"), content = function(file) { writeLines(rv$bitacora, file) })
  output$vista_bitacora <- renderText({ if (length(rv$bitacora) == 0) "Sin registros." else paste(rv$bitacora, collapse = "\n") })
}

shinyApp(ui = ui, server = server)