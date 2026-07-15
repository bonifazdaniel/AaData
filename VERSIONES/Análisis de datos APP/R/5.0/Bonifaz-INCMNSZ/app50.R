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
#                      gridExtra (para armar el reporte en PDF),
#                      survival (para la curva de Kaplan-Meier)
########################################################################

## ---- 1. Verificación e instalación de paquetes necesarios ----------
paquetes_requeridos <- c("shiny", "DT", "ggplot2", "dplyr", "tidyr",
                          "readxl", "writexl", "gridExtra", "survival", "nlme")

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

# Devuelve todas las columnas del data.frame (usada, por ejemplo, para elegir
# la variable de identificación de sujeto/ratón, que puede tener muchos
# valores distintos y por eso no siempre aparece como "categórica")
obtener_vars_todas <- function(df) {
  names(df)
}

# Rellena hacia la derecha los huecos (NA/"") de un vector de encabezados,
# reconstruyendo las celdas combinadas horizontales (categoría, subcategoría).
rellenar_derecha <- function(x) {
  ultimo <- NA
  for (i in seq_along(x)) {
    if (is.na(x[i]) || x[i] == "") x[i] <- ultimo else ultimo <- x[i]
  }
  x
}

# Construye un nombre por columna combinando varias filas de encabezado.
# mat_head: matriz de caracteres (n_filas_encabezado x n_columnas).
# Para cada columna toma como nombre base el ÚLTIMO valor no vacío de su
# bloque de encabezado (la etiqueta más específica); si ese nombre se repite
# en otras columnas, le antepone los valores de las filas de arriba hasta
# que sea único. Así los nombres son cortos pero sin ambigüedad, y funciona
# aunque haya celdas combinadas en vertical.
construir_nombres <- function(mat_head) {
  mat_head <- apply(mat_head, c(1, 2), function(v) {
    v <- trimws(as.character(v)); if (is.na(v) || v == "NA") "" else v
  })
  if (is.null(dim(mat_head))) mat_head <- matrix(mat_head, nrow = 1)
  n_col <- ncol(mat_head)

  no_vacios <- lapply(seq_len(n_col), function(j) {
    vals <- mat_head[, j]; vals[vals != ""]
  })
  base <- vapply(no_vacios, function(v) if (length(v) == 0) "columna" else v[length(v)], character(1))

  # niveles de prefijo disponibles por columna (de más cercano a más lejano hacia arriba)
  max_niveles <- max(vapply(no_vacios, length, integer(1)), 1L)
  nivel <- 1L
  while (nivel < max_niveles && any(duplicated(base))) {
    dups <- base %in% base[duplicated(base)]
    for (j in which(dups)) {
      v <- no_vacios[[j]]
      idx <- length(v) - nivel   # valor 'nivel' posiciones arriba del base
      if (idx >= 1) base[j] <- paste(v[idx], base[j], sep = " - ")
    }
    nivel <- nivel + 1L
  }
  make.unique(base, sep = " ")
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

# Quita los acentos y la eñe de un texto (á->a, é->e, ñ->n, etc.)
quitar_acentos <- function(x) {
  chartr("áéíóúÁÉÍÓÚñÑüÜ", "aeiouAEIOUnNuU", x)
}

# Convierte a numérico las columnas de texto que son numéricas salvo por
# marcadores de dato faltante comunes ("-", "ND", "N/A", "s/d", etc.). Tolera
# la coma decimal. Deja intactas las columnas de texto real (ej. "Femenino").
convertir_columnas_numericas <- function(df) {
  tokens_na <- c("", "-", "--", "na", "n/a", "n.a.", "nd", "n.d.", "s/d", "sd",
                 "sin dato", "sindato", ".", "nan", "null", "?")
  for (nom in names(df)) {
    x <- df[[nom]]
    if (!is.character(x)) next
    limpio <- trimws(x)
    es_na <- tolower(limpio) %in% tokens_na
    convertido <- suppressWarnings(as.numeric(gsub(",", ".", limpio)))
    validas <- !es_na
    # Convertir solo si hay valores válidos y TODOS ellos son numéricos
    if (any(validas) && !any(is.na(convertido[validas]))) {
      convertido[es_na] <- NA
      df[[nom]] <- convertido
    }
  }
  df
}

# Limpia un vector de nombres de columnas: quita acentos, espacios y símbolos,
# y reemplaza los espacios por guion bajo para evitar problemas al referirlas.
limpiar_nombres_columnas <- function(nombres) {
  n <- quitar_acentos(nombres)
  n <- trimws(n)
  n <- gsub("[[:space:]]+", "_", n)        # espacios -> guion bajo
  n <- gsub("[^A-Za-z0-9_]", "", n)         # quita cualquier otro símbolo
  n <- gsub("_+", "_", n)                    # colapsa guiones bajos repetidos
  n <- gsub("^_|_$", "", n)                  # quita guion bajo al inicio/final
  n[n == ""] <- "columna"                    # evita nombres vacíos
  make.unique(n, sep = "_")                  # evita nombres duplicados
}

# Estandariza un vector de texto categórico: quita espacios sobrantes y unifica
# variantes que solo difieren en mayúsculas/minúsculas o espacios. Para cada
# grupo de valores equivalentes conserva la forma más frecuente (así "control",
# "Control " y "CONTROL" se vuelven todos "Control", sin dañar "AR+Tratamiento").
estandarizar_texto_vector <- function(x) {
  original <- trimws(x)
  original <- gsub("[[:space:]]+", " ", original)  # colapsa espacios internos
  clave <- tolower(original)
  no_na <- !is.na(clave)
  if (!any(no_na)) return(original)
  # forma canónica por clave = variante original más frecuente
  tabla <- as.matrix(table(clave[no_na], original[no_na]))
  canonico <- setNames(colnames(tabla)[max.col(tabla, ties.method = "first")], rownames(tabla))
  resultado <- original
  resultado[no_na] <- canonico[clave[no_na]]
  resultado
}

# Intenta convertir un vector de texto a numérico, tolerando la coma decimal
# (ej. "3,5" -> 3.5). Devuelve NULL si el texto no es realmente numérico.
intentar_texto_a_numero <- function(x) {
  if (!is.character(x)) return(NULL)
  limpio <- trimws(x)
  limpio <- gsub(",", ".", limpio)           # coma decimal -> punto
  limpio[limpio == ""] <- NA
  no_vacios <- !is.na(limpio)
  if (!any(no_vacios)) return(NULL)
  convertido <- suppressWarnings(as.numeric(limpio))
  # Solo aceptamos la conversión si TODOS los valores no vacíos son numéricos
  if (any(is.na(convertido[no_vacios]))) return(NULL)
  convertido
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

  # Día (simulado) en que cada ratón desarrolla artritis clínica manifiesta,
  # y si el evento ocurrió (1) o el ratón quedó censurado sin desarrollarla (0).
  # Estas dos columnas sirven para probar la curva de Kaplan-Meier.
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
    "Bonifaz-INCMNSZ · Herramienta de apoyo para investigación clínica y básica (ej. modelos de artritis reumatoide en ratones) · No sustituye la asesoría de un bioestadístico."
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
    linea_tiempo = "Evolución en el tiempo",
    mapa_calor = "Mapa de calor",
    violin = "Gráfica de violín",
    barras_error = "Barras con error estándar (media \u00b1 EE)",
    spaghetti = "Líneas individuales por sujeto",
    supervivencia = "Curva de supervivencia (Kaplan-Meier)"
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
                                 "Importación avanzada (Excel con encabezados en varias filas)" = "avanzado",
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
          condition = "input.tipo_carga == 'avanzado'",
          p("Usa esta opción para archivos de Excel donde los encabezados ocupan ",
            strong("varias filas"), " (categoría, subcategoría, variable), hay ",
            strong("celdas combinadas"), ", columnas o filas de margen, o los datos no empiezan en la primera fila. ",
            "La app combinará los encabezados en un solo nombre por columna y dejará una tabla lista para analizar."),
          fileInput("archivo_av", "Selecciona tu archivo de Excel (.xlsx o .xls)",
                    accept = c(".xlsx", ".xls")),
          uiOutput("panel_avanzado")
        ),

        conditionalPanel(
          condition = "input.tipo_carga == 'ejemplo'",
          p("Se usará un conjunto de datos simulado de un estudio con 3 grupos de ratones ",
            "(Control, AR, AR+Tratamiento) medidos en los días 0, 7, 14, 21 y 28, con las ",
            "variables: Puntaje clínico, Peso (g) e IL-6 (pg/mL)."),
          actionButton("cargar_ejemplo", "Cargar datos de ejemplo", class = "btn-info")
        ),

        uiOutput("mensaje_carga"),
        div(style = "margin-top: 20px; display:flex; justify-content: space-between;",
            actionButton("btn_atras", "\u2190 Atrás", class = "btn-nav btn-default"),
            actionButton("btn_next_paso2", "Siguiente \u2192", class = "btn-nav btn-primary"))
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
      df <- rv$datos_crudos
      vars_num <- obtener_vars_numericas(df)
      div(class = "caja",
        h3("¿Quieres limpiar tus datos?"),
        p("Selecciona las opciones de limpieza que quieras aplicar. Si no necesitas ",
          "limpiar nada, simplemente da clic en 'Siguiente'. Puedes marcar varias a la vez."),

        checkboxGroupInput("opciones_limpieza", NULL,
          choices = c(
            "Eliminar filas con datos faltantes (NA)" = "quitar_na",
            "Eliminar columnas vacías (100% de datos faltantes)" = "quitar_col_vacia",
            "Eliminar filas duplicadas" = "quitar_duplicados",
            "Quitar espacios en blanco al inicio/final del texto" = "trim_texto",
            "Estandarizar texto en variables categóricas (unificar mayúsculas/minúsculas y espacios)" = "estandarizar_texto",
            "Convertir a números las columnas de texto que en realidad son numéricas" = "texto_a_numero",
            "Redondear valores numéricos" = "redondear",
            "Filtrar valores fuera de un rango válido" = "filtrar_rango",
            "Detectar valores atípicos (outliers)" = "detectar_outliers",
            "Limpiar nombres de columnas (quitar acentos, espacios y símbolos)" = "limpiar_nombres"
          )),

        # --- Parámetros para 'redondear' ---
        conditionalPanel(
          condition = "input.opciones_limpieza && input.opciones_limpieza.indexOf('redondear') > -1",
          div(style = "margin-left: 25px; margin-bottom: 10px;",
            numericInput("redondear_decimales", "Número de decimales:", value = 2, min = 0, max = 6, step = 1)
          )
        ),

        # --- Parámetros para 'filtrar_rango' ---
        conditionalPanel(
          condition = "input.opciones_limpieza && input.opciones_limpieza.indexOf('filtrar_rango') > -1",
          div(style = "margin-left: 25px; margin-bottom: 10px; padding: 10px; background-color: #f6f6f9; border-radius: 6px;",
            p(class = "ayuda", "Se eliminarán las filas cuyo valor en la columna elegida quede fuera del rango [mínimo, máximo]."),
            selectInput("rango_col", "Columna numérica:", choices = vars_num),
            fluidRow(
              column(6, numericInput("rango_min", "Valor mínimo permitido:", value = 0)),
              column(6, numericInput("rango_max", "Valor máximo permitido:", value = 100))
            )
          )
        ),

        # --- Parámetros para 'detectar_outliers' ---
        conditionalPanel(
          condition = "input.opciones_limpieza && input.opciones_limpieza.indexOf('detectar_outliers') > -1",
          div(style = "margin-left: 25px; margin-bottom: 10px; padding: 10px; background-color: #f6f6f9; border-radius: 6px;",
            p(class = "ayuda", "Un valor atípico es un dato muy alejado del resto (posible error de captura). Se detectan con el rango intercuartílico (regla de 1.5 \u00d7 IQR) en las columnas numéricas."),
            radioButtons("outlier_accion", "¿Qué hacer con los valores atípicos?",
              choices = c(
                "Solo marcarlos como faltante (NA) sin borrar la fila" = "marcar",
                "Eliminar la fila completa que contenga un valor atípico" = "eliminar"
              ), selected = "marcar")
          )
        ),

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
      vars_todas <- obtener_vars_todas(df)

      div(class = "caja",
        h3("¿Qué tipo de análisis quieres hacer?"),
        radioButtons("tipo_analisis", NULL,
          choices = c(
            "Estadística descriptiva (promedios, medianas, etc.)" = "descriptivo",
            "Comparar grupos (2 o más grupos)" = "comparar",
            "Comparación pareada (antes vs. después, mismo sujeto)" = "pareada",
            "Tamaño del efecto (d de Cohen, 2 grupos)" = "cohen",
            "Relación entre dos variables (correlación)" = "correlacion",
            "Matriz de correlación (varias variables a la vez)" = "matriz_corr",
            "Tabla de frecuencias (variable categórica)" = "frecuencias",
            "Tabla de contingencia (asociación entre 2 categóricas)" = "contingencia",
            "Evolución en el tiempo (ej. puntaje clínico a lo largo de los días)" = "longitudinal",
            "ANOVA de dos vías (grupo \u00d7 tiempo, con interacción)" = "anova2",
            "ANOVA de medidas repetidas / modelo mixto" = "mixto",
            "Regresión lineal (predecir una variable numérica)" = "reg_lineal",
            "Regresión logística (predecir un desenlace sí/no)" = "reg_logistica",
            "Prueba de normalidad (Shapiro-Wilk) con recomendación" = "normalidad"
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

        conditionalPanel(condition = "input.tipo_analisis == 'pareada'",
          p(class = "ayuda", "Compara el mismo sujeto en dos momentos (ej. Día 0 vs. Día 28). Se emparejan las observaciones por el identificador de sujeto."),
          selectInput("par_id", "Identificador de sujeto (ej. Raton_ID):", choices = vars_todas),
          selectInput("par_cond", "Variable que distingue los dos momentos (ej. Dia):", choices = vars_todas),
          fluidRow(
            column(6, selectInput("par_m1", "Momento 1 (antes):", choices = NULL)),
            column(6, selectInput("par_m2", "Momento 2 (después):", choices = NULL))
          ),
          selectInput("par_resp", "Variable de respuesta (numérica):", choices = vars_num),
          actionButton("run_par", "Comparar (pareado)", class = "btn-primary"),
          verbatimTextOutput("out_par")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'cohen'",
          p(class = "ayuda", "El tamaño del efecto (d de Cohen) indica qué tan grande es la diferencia entre dos grupos, más allá de si es significativa."),
          selectInput("cohen_grupo", "Variable de grupo (debe tener 2 categorías):", choices = vars_cat),
          selectInput("cohen_num", "Variable numérica:", choices = vars_num),
          actionButton("run_cohen", "Calcular tamaño del efecto", class = "btn-primary"),
          verbatimTextOutput("out_cohen")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'correlacion'",
          selectInput("corr_var1", "Primera variable numérica:", choices = vars_num),
          selectInput("corr_var2", "Segunda variable numérica:", choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          actionButton("run_corr", "Calcular correlación", class = "btn-primary"),
          verbatimTextOutput("out_corr")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'matriz_corr'",
          p(class = "ayuda", "Calcula la correlación entre todas las variables numéricas que elijas (mínimo 2)."),
          selectizeInput("mcorr_vars", "Variables numéricas:", choices = vars_num, multiple = TRUE,
                         selected = if (length(vars_num) >= 2) vars_num[1:2] else vars_num),
          radioButtons("mcorr_metodo", "Método:",
            choices = c("Pearson (relación lineal)" = "pearson",
                        "Spearman (por rangos, no paramétrico)" = "spearman"), selected = "pearson"),
          actionButton("run_mcorr", "Calcular matriz", class = "btn-primary"),
          tableOutput("out_mcorr")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'frecuencias'",
          selectInput("freq_var", "Variable categórica:", choices = vars_cat),
          actionButton("run_freq", "Calcular frecuencias", class = "btn-primary"),
          tableOutput("out_freq")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'contingencia'",
          p(class = "ayuda", "Evalúa si dos variables categóricas están asociadas. Se usa chi-cuadrada, o la prueba exacta de Fisher cuando los conteos esperados son pequeños."),
          selectInput("cont_var1", "Primera variable categórica:", choices = vars_cat),
          selectInput("cont_var2", "Segunda variable categórica:", choices = vars_cat,
                     selected = if (length(vars_cat) > 1) vars_cat[2] else vars_cat[1]),
          actionButton("run_cont", "Analizar asociación", class = "btn-primary"),
          verbatimTextOutput("out_cont"),
          tableOutput("out_cont_tabla")
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

        conditionalPanel(condition = "input.tipo_analisis == 'anova2'",
          p(class = "ayuda", "Evalúa el efecto de dos factores (ej. grupo y tiempo) y su interacción sobre una variable numérica."),
          selectInput("a2_factor1", "Primer factor (ej. Grupo):", choices = vars_cat),
          selectInput("a2_factor2", "Segundo factor (ej. Día):", choices = vars_todas,
                     selected = if (length(vars_cat) > 1) vars_cat[2] else vars_cat[1]),
          selectInput("a2_resp", "Variable de respuesta (numérica):", choices = vars_num),
          actionButton("run_a2", "Calcular ANOVA de dos vías", class = "btn-primary"),
          verbatimTextOutput("out_a2"),
          tableOutput("out_a2_tabla")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'mixto'",
          p(class = "ayuda", "Análisis correcto cuando el mismo sujeto se mide en varios momentos (los datos no son independientes). Modela grupo, tiempo y su interacción, con el sujeto como efecto aleatorio."),
          selectInput("mix_id", "Identificador de sujeto (ej. Raton_ID):", choices = vars_todas),
          selectInput("mix_grupo", "Variable de grupo:", choices = vars_cat),
          selectInput("mix_tiempo", "Variable de tiempo (ej. Día):", choices = vars_todas,
                     selected = if (length(vars_num) > 0) vars_num[1] else vars_todas[1]),
          selectInput("mix_resp", "Variable de respuesta (numérica):", choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          actionButton("run_mix", "Calcular modelo mixto", class = "btn-primary"),
          verbatimTextOutput("out_mix"),
          tableOutput("out_mix_tabla")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'reg_lineal'",
          p(class = "ayuda", "Modela cómo una o más variables predicen una respuesta numérica."),
          selectInput("rl_resp", "Variable de respuesta (a predecir, numérica):", choices = vars_num),
          selectizeInput("rl_pred", "Variable(s) predictora(s):", choices = vars_todas, multiple = TRUE),
          actionButton("run_rl", "Ajustar regresión lineal", class = "btn-primary"),
          verbatimTextOutput("out_rl"),
          tableOutput("out_rl_tabla")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'reg_logistica'",
          p(class = "ayuda", "Predice un desenlace binario (sí/no, 1/0) a partir de otras variables. La respuesta debe tener exactamente 2 categorías."),
          selectInput("rlog_resp", "Variable de desenlace (2 categorías, ej. Evento_artritis):", choices = vars_todas),
          selectizeInput("rlog_pred", "Variable(s) predictora(s):", choices = vars_todas, multiple = TRUE),
          actionButton("run_rlog", "Ajustar regresión logística", class = "btn-primary"),
          verbatimTextOutput("out_rlog"),
          tableOutput("out_rlog_tabla")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'normalidad'",
          p(class = "ayuda", "Revisa si una variable numérica sigue una distribución normal y te recomienda qué tipo de prueba conviene usar."),
          selectInput("norm_var", "Variable numérica:", choices = vars_num),
          selectizeInput("norm_grupo", "Evaluar por grupo (opcional):",
                         choices = c("(Todo junto)" = "", vars_cat)),
          actionButton("run_norm", "Evaluar normalidad", class = "btn-primary"),
          verbatimTextOutput("out_norm")
        ),

        botones_nav()
      )

    } else if (rv$paso == 6) {
      ## ---------------- PASO 6: GRAFICAR ----------------
      req(datos_actuales())
      df <- datos_actuales()
      vars_num <- obtener_vars_numericas(df)
      vars_cat <- obtener_vars_categoricas(df)
      vars_todas <- obtener_vars_todas(df)

      div(class = "caja",
        h3("¿Quieres graficar tus datos?"),
        radioButtons("tipo_grafica", NULL,
          choices = c(
            "Histograma (distribución de una variable)" = "histograma",
            "Diagrama de caja (comparar grupos)" = "boxplot",
            "Gráfica de violín (distribución por grupo)" = "violin",
            "Diagrama de dispersión (relación entre 2 variables)" = "dispersion",
            "Gráfica de barras (frecuencias)" = "barras",
            "Barras con error estándar (media \u00b1 EE por grupo)" = "barras_error",
            "Mapa de calor (promedio cruzando dos variables)" = "mapa_calor",
            "Evolución en el tiempo (línea con promedio \u00b1 error estándar)" = "linea_tiempo",
            "Líneas individuales por sujeto (spaghetti plot)" = "spaghetti",
            "Curva de supervivencia (Kaplan-Meier)" = "supervivencia"
          )),
        hr(),

        conditionalPanel(condition = "input.tipo_grafica == 'histograma'",
          selectInput("hist_var", "Variable numérica:", choices = vars_num)
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'boxplot'",
          selectInput("box_grupo", "Variable de grupo:", choices = vars_cat),
          selectInput("box_num", "Variable numérica:", choices = vars_num)
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'violin'",
          selectInput("violin_grupo", "Variable de grupo:", choices = vars_cat),
          selectInput("violin_num", "Variable numérica:", choices = vars_num),
          p(class = "ayuda", "Muestra, además del promedio, la forma completa de la distribución en cada grupo (más informativo que el boxplot cuando hay pocos datos).")
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
        conditionalPanel(condition = "input.tipo_grafica == 'barras_error'",
          selectInput("barras_error_grupo", "Variable de grupo:", choices = vars_cat),
          selectInput("barras_error_num", "Variable numérica:", choices = vars_num),
          p(class = "ayuda", "Formato clásico en artículos biomédicos: la altura de la barra es el promedio y la línea vertical es el error estándar.")
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'mapa_calor'",
          selectInput("heat_fila", "Variable para las filas (categórica):", choices = vars_cat),
          selectInput("heat_columna", "Variable para las columnas (ej. tiempo o grupo):", choices = vars_cat,
                     selected = if (length(vars_cat) > 1) vars_cat[2] else vars_cat[1]),
          selectInput("heat_valor", "Variable numérica a promediar:", choices = vars_num),
          p(class = "ayuda", "Ejemplo: filas = Grupo, columnas = Día, valor = Puntaje clínico. Cada celda muestra el promedio de esa combinación.")
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'linea_tiempo'",
          selectInput("lt_tiempo", "Variable de tiempo:", choices = vars_num),
          selectInput("lt_grupo", "Variable de grupo:", choices = vars_cat),
          selectInput("lt_resp", "Variable de respuesta:", choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1])
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'spaghetti'",
          selectInput("sp_id", "Variable de identificación de sujeto (ej. Raton_ID):", choices = vars_todas),
          selectInput("sp_tiempo", "Variable de tiempo:", choices = vars_num),
          selectInput("sp_resp", "Variable de respuesta:", choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          selectizeInput("sp_grupo", "Colorear por grupo (opcional):",
                         choices = c("(Ninguno)" = "", vars_cat)),
          p(class = "ayuda", "Muestra la trayectoria individual de cada sujeto, útil para detectar variabilidad o valores atípicos que el promedio del grupo puede ocultar.")
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'supervivencia'",
          selectInput("km_id", "Variable de identificación de sujeto:", choices = vars_todas),
          selectInput("km_tiempo", "Variable de tiempo hasta el evento:", choices = vars_num),
          selectInput("km_evento", "Variable de evento (1 = ocurrió, 0 = censurado):", choices = vars_num),
          selectizeInput("km_grupo", "Comparar por grupo (opcional):",
                         choices = c("(Ninguno)" = "", vars_cat)),
          p(class = "ayuda", "Si tu tabla tiene varias filas por sujeto (ej. una fila por día), se usará solo la primera fila de cada sujeto para este análisis.")
        ),

        actionButton("generar_grafica", "Generar gráfica", class = "btn-primary"),
        br(), br(),
        plotOutput("grafica_principal", height = "420px"),
        uiOutput("texto_resultado_grafica"),
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

  output$boton_siguiente_carga <- renderUI({ NULL })

  observeEvent(input$btn_next_paso2, {
    if (is.null(rv$datos_crudos)) {
      showNotification(
        "Primero carga tus datos (sube un archivo, usa la importación avanzada, o carga los datos de ejemplo) antes de continuar.",
        type = "warning", duration = 6)
    } else {
      ir_siguiente()
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
        convertir_columnas_numericas(as.data.frame(readxl::read_excel(input$archivo$datapath)))
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

  ## ---- Importación avanzada (Excel con encabezados en varias filas) ----

  # Genera todo el panel avanzado en el servidor una vez que hay archivo subido.
  # (Se hace desde el servidor para no depender de condiciones del lado del cliente.)
  output$panel_avanzado <- renderUI({
    req(input$archivo_av)
    hojas <- tryCatch(readxl::excel_sheets(input$archivo_av$datapath),
                      error = function(e) NULL)
    if (is.null(hojas)) {
      return(div(style = "color:#b00020; margin-top:10px;",
                 "No se pudo leer el archivo de Excel. Verifica que sea un .xlsx o .xls válido."))
    }
    tagList(
      selectInput("hoja_av", "¿Qué hoja quieres analizar?", choices = hojas, selected = hojas[1]),
      h4("Vista previa del archivo (tal cual, con números de fila y columna)"),
      p(class = "ayuda", "Fíjate en qué fila(s) están los nombres y en qué fila empiezan los datos para configurar los valores de abajo."),
      div(style = "overflow-x:auto; border:1px solid #eee; border-radius:6px;",
          tableOutput("preview_crudo_av")),
      fluidRow(
        column(3, numericInput("fila_head_ini", "Primera fila de encabezados", value = 1, min = 1, step = 1)),
        column(3, numericInput("n_filas_head", "¿Cuántas filas de encabezado?", value = 1, min = 1, step = 1)),
        column(3, numericInput("fila_datos_ini", "Primera fila de datos", value = 2, min = 1, step = 1)),
        column(3, numericInput("col_ini", "Primera columna con datos", value = 1, min = 1, step = 1))
      ),
      checkboxInput("rellenar_merges",
                    "Rellenar celdas combinadas de los encabezados (recomendado)", value = TRUE),
      actionButton("construir_av", "Construir tabla para analizar", class = "btn-info"),
      p(class = "ayuda",
        "Nota: en este modo, las columnas de fecha pueden requerir revisión posterior. ",
        "El objetivo principal es dejar listas las variables numéricas y de grupo.")
    )
  })

  # Vista previa "cruda" de la hoja elegida (sin tratar ninguna fila como encabezado)
  output$preview_crudo_av <- renderTable({
    req(input$archivo_av, input$hoja_av)
    crudo <- tryCatch(
      readxl::read_excel(input$archivo_av$datapath, sheet = input$hoja_av,
                         col_names = FALSE, col_types = "text", n_max = 12,
                         .name_repair = "minimal"),
      error = function(e) NULL)
    req(crudo)
    crudo <- as.data.frame(crudo)
    n_col_mostrar <- min(ncol(crudo), 12)
    crudo <- crudo[, seq_len(n_col_mostrar), drop = FALSE]
    colnames(crudo) <- paste0("Col ", seq_len(n_col_mostrar))
    crudo <- cbind(Fila = seq_len(nrow(crudo)), crudo)
    crudo
  }, striped = TRUE, bordered = TRUE, na = "")

  # Construir la tabla plana a partir de la configuración del usuario
  observeEvent(input$construir_av, {
    req(input$archivo_av, input$hoja_av)
    ruta <- input$archivo_av$datapath; hoja <- input$hoja_av
    fh <- as.integer(input$fila_head_ini)
    nh <- as.integer(input$n_filas_head)
    fd <- as.integer(input$fila_datos_ini)
    ci <- as.integer(input$col_ini)

    resultado <- tryCatch({
      # 1) Leer el bloque de encabezados como texto
      head_raw <- readxl::read_excel(ruta, sheet = hoja, col_names = FALSE,
                                     col_types = "text", skip = fh - 1, n_max = nh,
                                     .name_repair = "minimal")
      head_raw <- as.data.frame(head_raw)
      # 2) Leer el bloque de datos dejando que readxl deduzca los tipos
      datos <- readxl::read_excel(ruta, sheet = hoja, col_names = FALSE,
                                  skip = fd - 1, .name_repair = "minimal")
      datos <- as.data.frame(datos)

      # Alinear al mismo número de columnas
      ncol_comun <- min(ncol(head_raw), ncol(datos))
      head_raw <- head_raw[, seq_len(ncol_comun), drop = FALSE]
      datos <- datos[, seq_len(ncol_comun), drop = FALSE]

      # Recortar columnas de margen a la izquierda
      if (ci > 1 && ci <= ncol_comun) {
        head_raw <- head_raw[, ci:ncol_comun, drop = FALSE]
        datos <- datos[, ci:ncol_comun, drop = FALSE]
      }

      # 3) Rellenar celdas combinadas de los encabezados (hacia la derecha)
      mat_head <- as.matrix(head_raw)
      if (isTRUE(input$rellenar_merges)) {
        mat_head <- t(apply(mat_head, 1, rellenar_derecha))
      }

      # 4) Construir nombres y quitar columnas/filas totalmente vacías
      nombres <- construir_nombres(mat_head)
      names(datos) <- nombres
      col_vacias <- sapply(datos, function(x) all(is.na(x)))
      datos <- datos[, !col_vacias, drop = FALSE]
      fila_vacia <- apply(datos, 1, function(fila) all(is.na(fila)))
      datos <- datos[!fila_vacia, , drop = FALSE]

      # Convertir a número las columnas que son numéricas salvo por marcadores
      # de dato faltante ("-", "ND", etc.), para que puedan graficarse y analizarse.
      datos <- convertir_columnas_numericas(datos)

      datos
    }, error = function(e) {
      showNotification(paste("No se pudo construir la tabla:", e$message),
                       type = "error", duration = 10); NULL
    })

    if (!is.null(resultado) && ncol(resultado) > 0) {
      rv$datos_crudos <- resultado
      rv$datos_limpios <- NULL
      agregar_bitacora(sprintf(
        "Importación avanzada del archivo '%s' (hoja '%s'): %d filas x %d columnas. Encabezados: filas %d-%d; datos desde fila %d; primera columna %d.",
        input$archivo_av$name, hoja, nrow(resultado), ncol(resultado),
        fh, fh + nh - 1, fd, ci))
      showNotification(sprintf("Tabla construida: %d filas x %d columnas. Ya puedes continuar.",
                               nrow(resultado), ncol(resultado)),
                       type = "message", duration = 6)
    }
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

    if ("estandarizar_texto" %in% input$opciones_limpieza) {
      cols_texto <- names(df)[sapply(df, is.character)]
      if (length(cols_texto) > 0) {
        df[cols_texto] <- lapply(df[cols_texto], estandarizar_texto_vector)
      }
      acciones <- c(acciones, sprintf("se estandarizó el texto de %d columna(s) categórica(s)", length(cols_texto)))
    }

    if ("texto_a_numero" %in% input$opciones_limpieza) {
      convertidas <- c()
      for (nom in names(df)) {
        nuevo <- intentar_texto_a_numero(df[[nom]])
        if (!is.null(nuevo)) {
          df[[nom]] <- nuevo
          convertidas <- c(convertidas, nom)
        }
      }
      acciones <- c(acciones, if (length(convertidas) > 0)
        sprintf("se convirtieron a número %d columna(s): %s", length(convertidas), paste(convertidas, collapse = ", "))
        else "no se encontraron columnas de texto que fueran realmente numéricas")
    }

    if ("redondear" %in% input$opciones_limpieza) {
      dec <- if (is.null(input$redondear_decimales) || is.na(input$redondear_decimales)) 2
             else as.integer(input$redondear_decimales)
      cols_num <- names(df)[sapply(df, is.numeric)]
      df[cols_num] <- lapply(df[cols_num], function(x) round(x, dec))
      acciones <- c(acciones, sprintf("se redondearon %d columna(s) numérica(s) a %d decimal(es)", length(cols_num), dec))
    }

    if ("filtrar_rango" %in% input$opciones_limpieza) {
      col <- input$rango_col
      if (!is.null(col) && col %in% names(df) && is.numeric(df[[col]])) {
        vmin <- input$rango_min; vmax <- input$rango_max
        n_antes <- nrow(df)
        dentro <- is.na(df[[col]]) | (df[[col]] >= vmin & df[[col]] <= vmax)
        df <- df[dentro, , drop = FALSE]
        acciones <- c(acciones, sprintf("se eliminaron %d fila(s) con '%s' fuera del rango [%s, %s]",
                                          n_antes - nrow(df), col, vmin, vmax))
      } else {
        acciones <- c(acciones, "no se aplicó el filtro por rango (columna no válida)")
      }
    }

    if ("detectar_outliers" %in% input$opciones_limpieza) {
      cols_num <- names(df)[sapply(df, is.numeric)]
      accion <- if (is.null(input$outlier_accion)) "marcar" else input$outlier_accion
      total_outliers <- 0
      es_outlier_matriz <- matrix(FALSE, nrow = nrow(df), ncol = length(cols_num))
      for (j in seq_along(cols_num)) {
        x <- df[[cols_num[j]]]
        q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
        iqr <- q[2] - q[1]
        lim_inf <- q[1] - 1.5 * iqr
        lim_sup <- q[2] + 1.5 * iqr
        fuera <- !is.na(x) & (x < lim_inf | x > lim_sup)
        es_outlier_matriz[, j] <- fuera
        total_outliers <- total_outliers + sum(fuera)
        if (accion == "marcar") {
          x[fuera] <- NA
          df[[cols_num[j]]] <- x
        }
      }
      if (accion == "eliminar") {
        filas_con_outlier <- apply(es_outlier_matriz, 1, any)
        n_antes <- nrow(df)
        df <- df[!filas_con_outlier, , drop = FALSE]
        acciones <- c(acciones, sprintf("se detectaron %d valor(es) atípico(s) y se eliminaron %d fila(s)",
                                          total_outliers, n_antes - nrow(df)))
      } else {
        acciones <- c(acciones, sprintf("se detectaron %d valor(es) atípico(s) y se marcaron como faltantes (NA)",
                                          total_outliers))
      }
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

    # Se aplica al final para no romper las referencias por nombre de columna
    # que usan las demás operaciones (ej. el filtro por rango).
    if ("limpiar_nombres" %in% input$opciones_limpieza) {
      nombres_antes <- names(df)
      names(df) <- limpiar_nombres_columnas(names(df))
      cambiados <- sum(nombres_antes != names(df))
      acciones <- c(acciones, sprintf("se limpiaron los nombres de columnas (%d cambiado(s))", cambiados))
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
  observeEvent(input$run_desc, { tryCatch({
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
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Comparación de grupos ---
  observeEvent(input$run_comp, { tryCatch({
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
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Correlación ---
  observeEvent(input$run_corr, { tryCatch({
    df <- datos_actuales()
    req(df, input$corr_var1, input$corr_var2)

    texto <- NULL
    if (input$corr_var1 == input$corr_var2) {
      texto <- "Por favor selecciona dos variables distintas para calcular la correlación."
    } else {
      x1 <- suppressWarnings(as.numeric(df[[input$corr_var1]]))
      x2 <- suppressWarnings(as.numeric(df[[input$corr_var2]]))
      completos <- !is.na(x1) & !is.na(x2)
      x1 <- x1[completos]; x2 <- x2[completos]

      if (length(x1) < 3) {
        texto <- paste0("No hay suficientes pares de datos numéricos para calcular la correlación ",
                         "(se necesitan al menos 3). Verifica que ambas variables sean numéricas.")
      } else if (sd(x1) == 0 || sd(x2) == 0) {
        texto <- "Una de las variables no varía (todos sus valores son iguales), así que no se puede calcular la correlación."
      } else {
        p1 <- if (length(x1) <= 5000) tryCatch(shapiro.test(x1)$p.value, error = function(e) NA) else NA
        p2 <- if (length(x2) <= 5000) tryCatch(shapiro.test(x2)$p.value, error = function(e) NA) else NA
        normal <- !is.na(p1) && !is.na(p2) && p1 > 0.05 && p2 > 0.05
        metodo <- if (normal) "pearson" else "spearman"

        prueba <- tryCatch(cor.test(x1, x2, method = metodo), error = function(e) NULL)
        if (is.null(prueba)) {
          texto <- "No fue posible calcular la correlación con estos datos."
        } else {
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
      }
    }

    output$out_corr <- renderText(texto)
    rv$resultados$correlacion <- list(
      titulo = sprintf("Correlación entre %s y %s", input$corr_var1, input$corr_var2),
      texto = texto
    )
    agregar_bitacora(sprintf("Correlación calculada entre '%s' y '%s'.", input$corr_var1, input$corr_var2))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Frecuencias ---
  observeEvent(input$run_freq, { tryCatch({
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
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Evolución en el tiempo ---
  observeEvent(input$run_long, { tryCatch({
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
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Comparación pareada: actualizar los momentos según la variable elegida ---
  observeEvent(input$par_cond, {
    df <- datos_actuales()
    req(df, input$par_cond)
    if (input$par_cond %in% names(df)) {
      valores <- sort(unique(as.character(df[[input$par_cond]])))
      updateSelectInput(session, "par_m1", choices = valores,
                        selected = if (length(valores) >= 1) valores[1] else NULL)
      updateSelectInput(session, "par_m2", choices = valores,
                        selected = if (length(valores) >= 2) valores[2] else NULL)
    }
  })

  # --- Comparación pareada (antes vs. después) ---
  observeEvent(input$run_par, { tryCatch({
    df <- datos_actuales()
    req(df, input$par_id, input$par_cond, input$par_m1, input$par_m2, input$par_resp)

    if (input$par_m1 == input$par_m2) {
      texto <- "Elige dos momentos distintos para comparar."
    } else {
      d1 <- df[as.character(df[[input$par_cond]]) == input$par_m1, c(input$par_id, input$par_resp)]
      d2 <- df[as.character(df[[input$par_cond]]) == input$par_m2, c(input$par_id, input$par_resp)]
      names(d1) <- c("id", "v1"); names(d2) <- c("id", "v2")
      emparejado <- merge(d1, d2, by = "id")
      emparejado <- emparejado[!is.na(emparejado$v1) & !is.na(emparejado$v2), ]

      if (nrow(emparejado) < 2) {
        texto <- "No hay suficientes sujetos con datos en ambos momentos para hacer la comparación pareada."
      } else {
        difs <- emparejado$v2 - emparejado$v1
        p_norm <- if (length(difs) >= 3 && length(difs) <= 5000)
          tryCatch(shapiro.test(difs)$p.value, error = function(e) NA) else NA
        normal <- !is.na(p_norm) && p_norm > 0.05
        if (normal) {
          prueba <- t.test(emparejado$v2, emparejado$v1, paired = TRUE)
          nombre <- "prueba t pareada"
        } else {
          prueba <- wilcox.test(emparejado$v2, emparejado$v1, paired = TRUE)
          nombre <- "prueba de Wilcoxon pareada (no paramétrica)"
        }
        texto <- paste0(
          "Comparación pareada de '", input$par_resp, "': ",
          input$par_m1, " vs. ", input$par_m2, ".\n",
          "Sujetos emparejados: ", nrow(emparejado), ".\n",
          "Cambio promedio (después - antes): ", round(mean(difs), 3), ".\n",
          "Se usó la ", nombre, ".\n\n", interpretar_p(prueba$p.value)
        )
      }
    }
    output$out_par <- renderText(texto)
    rv$resultados$pareada <- list(
      titulo = sprintf("Comparación pareada: %s (%s vs. %s)", input$par_resp, input$par_m1, input$par_m2),
      texto = texto)
    agregar_bitacora(sprintf("Comparación pareada de '%s': %s vs. %s.",
                              input$par_resp, input$par_m1, input$par_m2))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Tamaño del efecto (d de Cohen) ---
  observeEvent(input$run_cohen, { tryCatch({
    df <- datos_actuales()
    req(df, input$cohen_grupo, input$cohen_num)
    df_sub <- df[!is.na(df[[input$cohen_num]]) & !is.na(df[[input$cohen_grupo]]), ]
    grupos <- unique(as.character(df_sub[[input$cohen_grupo]]))

    if (length(grupos) != 2) {
      texto <- sprintf("La variable de grupo debe tener exactamente 2 categorías (tiene %d). Elige otra variable o filtra los datos.",
                        length(grupos))
    } else {
      x1 <- df_sub[[input$cohen_num]][as.character(df_sub[[input$cohen_grupo]]) == grupos[1]]
      x2 <- df_sub[[input$cohen_num]][as.character(df_sub[[input$cohen_grupo]]) == grupos[2]]
      n1 <- length(x1); n2 <- length(x2)
      sp <- sqrt(((n1 - 1) * var(x1) + (n2 - 1) * var(x2)) / (n1 + n2 - 2))
      d <- (mean(x1) - mean(x2)) / sp
      se_d <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
      ic_bajo <- d - 1.96 * se_d; ic_alto <- d + 1.96 * se_d
      magnitud <- if (abs(d) < 0.2) "insignificante" else if (abs(d) < 0.5) "pequeño"
                  else if (abs(d) < 0.8) "mediano" else "grande"
      texto <- paste0(
        "Tamaño del efecto entre '", grupos[1], "' y '", grupos[2], "' para '", input$cohen_num, "'.\n\n",
        "d de Cohen = ", round(d, 3), " (efecto ", magnitud, ").\n",
        "Intervalo de confianza al 95%: [", round(ic_bajo, 3), ", ", round(ic_alto, 3), "].\n\n",
        "Referencia: |d| ~ 0.2 pequeño, ~ 0.5 mediano, ~ 0.8 o más grande."
      )
    }
    output$out_cohen <- renderText(texto)
    rv$resultados$cohen <- list(
      titulo = sprintf("Tamaño del efecto (d de Cohen): %s por %s", input$cohen_num, input$cohen_grupo),
      texto = texto)
    agregar_bitacora(sprintf("Tamaño del efecto (d de Cohen) calculado para '%s' por '%s'.",
                              input$cohen_num, input$cohen_grupo))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Matriz de correlación ---
  observeEvent(input$run_mcorr, { tryCatch({
    df <- datos_actuales()
    req(df, input$mcorr_vars)
    if (length(input$mcorr_vars) < 2) {
      output$out_mcorr <- renderTable(data.frame(Aviso = "Elige al menos 2 variables numéricas."))
      return()
    }
    sub <- df[, input$mcorr_vars, drop = FALSE]
    sub <- as.data.frame(lapply(sub, function(x) suppressWarnings(as.numeric(x))))
    sub <- sub[complete.cases(sub), , drop = FALSE]
    if (nrow(sub) < 3) {
      output$out_mcorr <- renderTable(data.frame(
        Aviso = "No hay suficientes filas con datos numéricos completos (mínimo 3)."))
      return()
    }
    m <- tryCatch(round(cor(sub, method = input$mcorr_metodo), 3), error = function(e) NULL)
    if (is.null(m)) {
      output$out_mcorr <- renderTable(data.frame(
        Aviso = "No fue posible calcular la matriz (revisa que las variables sean numéricas)."))
      return()
    }
    tabla <- cbind(Variable = rownames(m), as.data.frame(m))
    rownames(tabla) <- NULL
    output$out_mcorr <- renderTable(tabla)
    rv$resultados$matriz_corr <- list(
      titulo = sprintf("Matriz de correlación (%s)", input$mcorr_metodo),
      tabla = tabla)
    agregar_bitacora(sprintf("Matriz de correlación (%s) calculada para: %s.",
                              input$mcorr_metodo, paste(input$mcorr_vars, collapse = ", ")))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Tabla de contingencia (chi-cuadrada / Fisher) ---
  observeEvent(input$run_cont, { tryCatch({
    df <- datos_actuales()
    req(df, input$cont_var1, input$cont_var2)
    if (input$cont_var1 == input$cont_var2) {
      output$out_cont <- renderText("Elige dos variables categóricas distintas.")
      output$out_cont_tabla <- renderTable(NULL)
      return()
    }
    tabla_cont <- table(df[[input$cont_var1]], df[[input$cont_var2]])
    esperados <- tryCatch(suppressWarnings(chisq.test(tabla_cont)$expected), error = function(e) NULL)
    usar_fisher <- !is.null(esperados) && any(esperados < 5)

    if (usar_fisher) {
      prueba <- tryCatch(fisher.test(tabla_cont, simulate.p.value = TRUE, B = 10000),
                          error = function(e) NULL)
      nombre <- "prueba exacta de Fisher (algunos conteos esperados eran pequeños)"
    } else {
      prueba <- suppressWarnings(chisq.test(tabla_cont))
      nombre <- "prueba de chi-cuadrada"
    }
    texto <- if (is.null(prueba)) "No fue posible calcular la prueba con estos datos." else paste0(
      "Asociación entre '", input$cont_var1, "' y '", input$cont_var2, "'.\n",
      "Se usó la ", nombre, ".\n\n", interpretar_p(prueba$p.value),
      "\n\nUn resultado significativo indica que las dos variables NO son independientes (están asociadas).")
    tabla_df <- as.data.frame.matrix(tabla_cont)
    tabla_df <- cbind(" " = rownames(tabla_df), tabla_df); rownames(tabla_df) <- NULL

    output$out_cont <- renderText(texto)
    output$out_cont_tabla <- renderTable(tabla_df)
    rv$resultados$contingencia <- list(
      titulo = sprintf("Tabla de contingencia: %s vs. %s", input$cont_var1, input$cont_var2),
      texto = texto, tabla = tabla_df)
    agregar_bitacora(sprintf("Tabla de contingencia: '%s' vs. '%s'.", input$cont_var1, input$cont_var2))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- ANOVA de dos vías ---
  observeEvent(input$run_a2, { tryCatch({
    df <- datos_actuales()
    req(df, input$a2_factor1, input$a2_factor2, input$a2_resp)
    df_sub <- df[!is.na(df[[input$a2_resp]]), ]
    df_sub[[input$a2_factor1]] <- factor(df_sub[[input$a2_factor1]])
    df_sub[[input$a2_factor2]] <- factor(df_sub[[input$a2_factor2]])

    formula_a2 <- as.formula(paste0("`", input$a2_resp, "` ~ `", input$a2_factor1,
                                      "` * `", input$a2_factor2, "`"))
    modelo <- tryCatch(aov(formula_a2, data = df_sub), error = function(e) NULL)
    if (is.null(modelo)) {
      output$out_a2 <- renderText("No fue posible ajustar el modelo con estos datos.")
      output$out_a2_tabla <- renderTable(NULL)
      return()
    }
    resumen <- summary(modelo)[[1]]
    tabla <- data.frame(
      Termino = trimws(rownames(resumen)),
      gl = resumen[["Df"]],
      F = round(resumen[["F value"]], 3),
      valor_p = signif(resumen[["Pr(>F)"]], 4)
    )
    p_int <- resumen[["Pr(>F)"]][3]
    texto <- paste0(
      "ANOVA de dos vías: efecto de '", input$a2_factor1, "', de '", input$a2_factor2,
      "' y su interacción sobre '", input$a2_resp, "'.\n\n",
      if (!is.na(p_int) && p_int < 0.05)
        "La interacción es significativa: el efecto de un factor DEPENDE del nivel del otro (ej. el tratamiento cambia la progresión en el tiempo)."
      else
        "La interacción no es significativa: los efectos de ambos factores son, en su mayoría, independientes entre sí.",
      "\nRevisa la tabla para el valor p de cada término (un valor p < 0.05 indica un efecto significativo).")
    output$out_a2 <- renderText(texto)
    output$out_a2_tabla <- renderTable(tabla)
    rv$resultados$anova2 <- list(
      titulo = sprintf("ANOVA de dos vías: %s ~ %s * %s", input$a2_resp, input$a2_factor1, input$a2_factor2),
      texto = texto, tabla = tabla)
    agregar_bitacora(sprintf("ANOVA de dos vías: '%s' ~ '%s' * '%s'.",
                              input$a2_resp, input$a2_factor1, input$a2_factor2))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- ANOVA de medidas repetidas / modelo mixto ---
  observeEvent(input$run_mix, { tryCatch({
    df <- datos_actuales()
    req(df, input$mix_id, input$mix_grupo, input$mix_tiempo, input$mix_resp)
    df_sub <- df[!is.na(df[[input$mix_resp]]), ]
    df_sub[[input$mix_grupo]] <- factor(df_sub[[input$mix_grupo]])
    df_sub[[input$mix_tiempo]] <- factor(df_sub[[input$mix_tiempo]])
    df_sub[[input$mix_id]] <- factor(df_sub[[input$mix_id]])

    formula_fija <- as.formula(paste0("`", input$mix_resp, "` ~ `", input$mix_grupo,
                                       "` * `", input$mix_tiempo, "`"))
    formula_random <- as.formula(paste0("~1 | `", input$mix_id, "`"))
    modelo <- tryCatch(
      nlme::lme(formula_fija, random = formula_random, data = df_sub, method = "REML"),
      error = function(e) NULL)

    if (is.null(modelo)) {
      output$out_mix <- renderText(paste0(
        "No fue posible ajustar el modelo mixto con estos datos. ",
        "Verifica que cada sujeto tenga varias mediciones y que no falten combinaciones."))
      output$out_mix_tabla <- renderTable(NULL)
      return()
    }
    an <- anova(modelo)
    tabla <- data.frame(
      Termino = trimws(rownames(an)),
      gl_num = an[["numDF"]],
      F = round(an[["F-value"]], 3),
      valor_p = signif(an[["p-value"]], 4)
    )
    texto <- paste0(
      "Modelo mixto (medidas repetidas) para '", input$mix_resp, "', con '",
      input$mix_id, "' como efecto aleatorio.\n",
      "Efectos fijos: '", input$mix_grupo, "', '", input$mix_tiempo, "' y su interacción.\n\n",
      "Revisa la tabla: un valor p < 0.05 en un término indica un efecto significativo. ",
      "Este análisis toma en cuenta que las mediciones del mismo sujeto están correlacionadas.")
    output$out_mix <- renderText(texto)
    output$out_mix_tabla <- renderTable(tabla)
    rv$resultados$mixto <- list(
      titulo = sprintf("Modelo mixto: %s ~ %s * %s (sujeto: %s)",
                        input$mix_resp, input$mix_grupo, input$mix_tiempo, input$mix_id),
      texto = texto, tabla = tabla)
    agregar_bitacora(sprintf("Modelo mixto ajustado para '%s' (sujeto aleatorio: '%s').",
                              input$mix_resp, input$mix_id))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Regresión lineal ---
  observeEvent(input$run_rl, { tryCatch({
    df <- datos_actuales()
    req(df, input$rl_resp, input$rl_pred)
    if (length(input$rl_pred) < 1) {
      output$out_rl <- renderText("Elige al menos una variable predictora.")
      output$out_rl_tabla <- renderTable(NULL); return()
    }
    predictores <- paste(sprintf("`%s`", input$rl_pred), collapse = " + ")
    formula_rl <- as.formula(paste0("`", input$rl_resp, "` ~ ", predictores))
    modelo <- tryCatch(lm(formula_rl, data = df), error = function(e) NULL)
    if (is.null(modelo)) {
      output$out_rl <- renderText("No fue posible ajustar la regresión con estos datos.")
      output$out_rl_tabla <- renderTable(NULL); return()
    }
    co <- summary(modelo)$coefficients
    tabla <- data.frame(
      Termino = rownames(co),
      Coeficiente = round(co[, 1], 4),
      Error_estandar = round(co[, 2], 4),
      valor_p = signif(co[, 4], 4)
    ); rownames(tabla) <- NULL
    r2 <- summary(modelo)$r.squared
    texto <- paste0(
      "Regresión lineal para predecir '", input$rl_resp, "'.\n",
      "R² = ", round(r2, 3), " (el modelo explica el ", round(100 * r2, 1), "% de la variación).\n\n",
      "Cada coeficiente indica cuánto cambia la respuesta por cada unidad de esa variable ",
      "(manteniendo las demás constantes). Un valor p < 0.05 indica un predictor significativo.")
    output$out_rl <- renderText(texto)
    output$out_rl_tabla <- renderTable(tabla)
    rv$resultados$reg_lineal <- list(
      titulo = sprintf("Regresión lineal: %s ~ %s", input$rl_resp, paste(input$rl_pred, collapse = " + ")),
      texto = texto, tabla = tabla)
    agregar_bitacora(sprintf("Regresión lineal ajustada: '%s' ~ %s.",
                              input$rl_resp, paste(input$rl_pred, collapse = " + ")))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Regresión logística ---
  observeEvent(input$run_rlog, { tryCatch({
    df <- datos_actuales()
    req(df, input$rlog_resp, input$rlog_pred)
    if (length(input$rlog_pred) < 1) {
      output$out_rlog <- renderText("Elige al menos una variable predictora.")
      output$out_rlog_tabla <- renderTable(NULL); return()
    }
    y_original <- df[[input$rlog_resp]]
    niveles <- unique(as.character(y_original[!is.na(y_original)]))
    if (length(niveles) != 2) {
      output$out_rlog <- renderText(sprintf(
        "El desenlace debe tener exactamente 2 categorías (tiene %d).", length(niveles)))
      output$out_rlog_tabla <- renderTable(NULL); return()
    }
    df2 <- df
    niveles_ord <- sort(niveles)
    df2$.y_bin <- ifelse(as.character(y_original) == niveles_ord[2], 1, 0)
    predictores <- paste(sprintf("`%s`", input$rlog_pred), collapse = " + ")
    formula_rlog <- as.formula(paste0(".y_bin ~ ", predictores))
    modelo <- tryCatch(glm(formula_rlog, data = df2, family = binomial), error = function(e) NULL)
    if (is.null(modelo)) {
      output$out_rlog <- renderText("No fue posible ajustar la regresión logística con estos datos.")
      output$out_rlog_tabla <- renderTable(NULL); return()
    }
    co <- summary(modelo)$coefficients
    tabla <- data.frame(
      Termino = rownames(co),
      OR = round(exp(co[, 1]), 3),
      Coeficiente = round(co[, 1], 4),
      valor_p = signif(co[, 4], 4)
    ); rownames(tabla) <- NULL
    texto <- paste0(
      "Regresión logística para predecir '", input$rlog_resp, "' (evento = '", niveles_ord[2], "').\n\n",
      "La columna OR es la razón de momios (odds ratio): un OR > 1 aumenta la probabilidad del evento, ",
      "y un OR < 1 la disminuye. Un valor p < 0.05 indica un predictor significativo.")
    output$out_rlog <- renderText(texto)
    output$out_rlog_tabla <- renderTable(tabla)
    rv$resultados$reg_logistica <- list(
      titulo = sprintf("Regresión logística: %s ~ %s", input$rlog_resp, paste(input$rlog_pred, collapse = " + ")),
      texto = texto, tabla = tabla)
    agregar_bitacora(sprintf("Regresión logística ajustada: '%s' ~ %s.",
                              input$rlog_resp, paste(input$rlog_pred, collapse = " + ")))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Prueba de normalidad (Shapiro-Wilk) ---
  observeEvent(input$run_norm, { tryCatch({
    df <- datos_actuales()
    req(df, input$norm_var)

    evaluar <- function(x, etiqueta) {
      x <- x[!is.na(x)]
      if (length(x) < 3) return(paste0("- ", etiqueta, ": muy pocos datos (n = ", length(x), ")."))
      if (length(x) > 5000) return(paste0("- ", etiqueta, ": demasiados datos para Shapiro-Wilk (n > 5000)."))
      p <- tryCatch(shapiro.test(x)$p.value, error = function(e) NA)
      if (is.na(p)) return(paste0("- ", etiqueta, ": no se pudo evaluar."))
      veredicto <- if (p > 0.05) "parece NORMAL (pruebas paramétricas: t, ANOVA, Pearson)"
                   else "NO parece normal (pruebas no paramétricas: Wilcoxon, Kruskal-Wallis, Spearman)"
      sprintf("- %s: p = %.4f -> %s", etiqueta, p, veredicto)
    }

    if (!is.null(input$norm_grupo) && input$norm_grupo != "") {
      grupos <- unique(as.character(df[[input$norm_grupo]]))
      lineas <- sapply(grupos, function(g) {
        evaluar(df[[input$norm_var]][as.character(df[[input$norm_grupo]]) == g], paste0("Grupo ", g))
      })
    } else {
      lineas <- evaluar(df[[input$norm_var]], input$norm_var)
    }
    texto <- paste0("Prueba de normalidad (Shapiro-Wilk) para '", input$norm_var, "':\n\n",
                     paste(lineas, collapse = "\n"),
                     "\n\nRegla: si p > 0.05, los datos son compatibles con una distribución normal.")
    output$out_norm <- renderText(texto)
    rv$resultados$normalidad <- list(
      titulo = sprintf("Prueba de normalidad (Shapiro-Wilk): %s", input$norm_var),
      texto = texto)
    agregar_bitacora(sprintf("Prueba de normalidad (Shapiro-Wilk) para '%s'.", input$norm_var))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste("No se pudo completar el an\u00e1lisis:", conditionMessage(e)),
                         type = "error", duration = 9)
    })
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

      } else if (input$tipo_grafica == "violin") {
        req(input$violin_grupo, input$violin_num)
        ggplot(df, aes(x = factor(.data[[input$violin_grupo]]), y = .data[[input$violin_num]],
                       fill = factor(.data[[input$violin_grupo]]))) +
          geom_violin(alpha = 0.8, trim = FALSE) +
          geom_boxplot(width = 0.12, fill = "white", alpha = 0.7, outlier.shape = NA) +
          labs(title = paste(input$violin_num, "por", input$violin_grupo),
               x = input$violin_grupo, y = input$violin_num) +
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

      } else if (input$tipo_grafica == "barras_error") {
        req(input$barras_error_grupo, input$barras_error_num)
        resumen <- df %>%
          dplyr::filter(!is.na(.data[[input$barras_error_num]])) %>%
          dplyr::group_by(.data[[input$barras_error_grupo]]) %>%
          dplyr::summarise(
            Promedio = mean(.data[[input$barras_error_num]], na.rm = TRUE),
            EE = sd(.data[[input$barras_error_num]], na.rm = TRUE) / sqrt(dplyr::n()),
            .groups = "drop"
          )
        ggplot(resumen, aes(x = factor(.data[[input$barras_error_grupo]]), y = Promedio,
                            fill = factor(.data[[input$barras_error_grupo]]))) +
          geom_col(alpha = 0.85) +
          geom_errorbar(aes(ymin = Promedio - EE, ymax = Promedio + EE), width = 0.25) +
          labs(title = paste(input$barras_error_num, "por", input$barras_error_grupo, "(media \u00b1 EE)"),
               x = input$barras_error_grupo, y = input$barras_error_num) +
          theme_minimal(base_size = 14) +
          theme(legend.position = "none")

      } else if (input$tipo_grafica == "mapa_calor") {
        req(input$heat_fila, input$heat_columna, input$heat_valor)
        resumen <- df %>%
          dplyr::filter(!is.na(.data[[input$heat_valor]])) %>%
          dplyr::group_by(.data[[input$heat_fila]], .data[[input$heat_columna]]) %>%
          dplyr::summarise(Promedio = mean(.data[[input$heat_valor]], na.rm = TRUE), .groups = "drop")
        ggplot(resumen, aes(x = factor(.data[[input$heat_columna]]), y = factor(.data[[input$heat_fila]]),
                            fill = Promedio)) +
          geom_tile(color = "white") +
          geom_text(aes(label = round(Promedio, 1)), color = "black", size = 4) +
          scale_fill_gradient(low = "#f7ecef", high = "#7a1f3d") +
          labs(title = paste("Mapa de calor de", input$heat_valor),
               x = input$heat_columna, y = input$heat_fila, fill = "Promedio") +
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

      } else if (input$tipo_grafica == "spaghetti") {
        req(input$sp_id, input$sp_tiempo, input$sp_resp)
        if (!is.null(input$sp_grupo) && input$sp_grupo != "") {
          ggplot(df, aes(x = .data[[input$sp_tiempo]], y = .data[[input$sp_resp]],
                        group = .data[[input$sp_id]], color = factor(.data[[input$sp_grupo]]))) +
            geom_line(alpha = 0.6) +
            geom_point(size = 1.6, alpha = 0.7) +
            labs(title = paste("Trayectorias individuales de", input$sp_resp),
                 x = input$sp_tiempo, y = input$sp_resp, color = input$sp_grupo) +
            theme_minimal(base_size = 14)
        } else {
          ggplot(df, aes(x = .data[[input$sp_tiempo]], y = .data[[input$sp_resp]],
                        group = .data[[input$sp_id]])) +
            geom_line(alpha = 0.5, color = "#7a1f3d") +
            geom_point(size = 1.6, alpha = 0.6, color = "#7a1f3d") +
            labs(title = paste("Trayectorias individuales de", input$sp_resp),
                 x = input$sp_tiempo, y = input$sp_resp) +
            theme_minimal(base_size = 14)
        }

      } else if (input$tipo_grafica == "supervivencia") {
        req(input$km_id, input$km_tiempo, input$km_evento)
        df_sub <- df %>% dplyr::distinct(.data[[input$km_id]], .keep_all = TRUE)
        df_sub <- df_sub[!is.na(df_sub[[input$km_tiempo]]) & !is.na(df_sub[[input$km_evento]]), ]

        con_grupo <- !is.null(input$km_grupo) && input$km_grupo != ""
        formula_km <- if (con_grupo) {
          as.formula(paste0("survival::Surv(`", input$km_tiempo, "`, `", input$km_evento,
                             "`) ~ `", input$km_grupo, "`"))
        } else {
          as.formula(paste0("survival::Surv(`", input$km_tiempo, "`, `", input$km_evento, "`) ~ 1"))
        }
        ajuste <- survival::survfit(formula_km, data = df_sub)

        if (!is.null(ajuste$strata)) {
          etiquetas_grupo <- rep(names(ajuste$strata), ajuste$strata)
          etiquetas_grupo <- sub("^[^=]*=", "", etiquetas_grupo)
        } else {
          etiquetas_grupo <- rep("Todos", length(ajuste$time))
        }

        df_km <- data.frame(Tiempo = ajuste$time, Supervivencia = ajuste$surv, Grupo = etiquetas_grupo)
        inicio <- data.frame(Tiempo = 0, Supervivencia = 1, Grupo = unique(df_km$Grupo))
        df_km <- rbind(inicio, df_km)
        df_km <- df_km[order(df_km$Grupo, df_km$Tiempo), ]

        if (con_grupo) {
          prueba_lr <- tryCatch(survival::survdiff(formula_km, data = df_sub), error = function(e) NULL)
          if (!is.null(prueba_lr)) {
            p_lr <- 1 - pchisq(prueba_lr$chisq, length(prueba_lr$n) - 1)
            rv$km_texto <- paste0("Prueba de rangos logarítmicos (log-rank) entre grupos.\n",
                                    interpretar_p(p_lr))
          }
        }

        ggplot(df_km, aes(x = Tiempo, y = Supervivencia, color = Grupo)) +
          geom_step(linewidth = 1) +
          ylim(0, 1) +
          labs(title = "Curva de supervivencia (Kaplan-Meier)",
               x = input$km_tiempo, y = "Probabilidad de no evento",
               color = if (con_grupo) input$km_grupo else NULL) +
          theme_minimal(base_size = 14)
      }
    }, error = function(e) {
      showNotification(paste("No se pudo generar la gráfica:", e$message), type = "error", duration = 8)
      NULL
    })

    # ggplot evalúa de forma diferida: forzamos la construcción para atrapar
    # aquí cualquier error de datos (ej. variable no numérica) en vez de fallar
    # al dibujar y dejar el área en blanco.
    if (!is.null(grafica)) {
      valida <- tryCatch({ ggplot2::ggplot_build(grafica); TRUE },
        error = function(e) {
          showNotification(paste("No se pudo generar la gráfica:", e$message,
                                 "\u2014 revisa que las variables elegidas sean del tipo correcto (ej. numéricas)."),
                           type = "error", duration = 9)
          FALSE
        })
      if (!valida) grafica <- NULL
    }

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

  output$texto_resultado_grafica <- renderUI({
    if (!is.null(input$tipo_grafica) && input$tipo_grafica == "supervivencia" && !is.null(rv$km_texto)) {
      div(style = "margin-top: 12px; padding: 12px; background-color: #f7ecef; border-radius: 6px;",
          strong("Resultado estadístico (log-rank):"), br(),
          HTML(gsub("\n", "<br>", rv$km_texto)))
    }
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
      for (r in res) {
        if (!is.null(r$texto)) {
          pagina_texto_pdf(r$titulo, strsplit(r$texto, "\n")[[1]])
          if (!is.null(r$tabla) && nrow(r$tabla) > 0) {
            pagina_tabla_pdf(r$titulo, r$tabla)
          }
        } else if (!is.null(r$tabla) && nrow(r$tabla) > 0) {
          pagina_tabla_pdf(r$titulo, r$tabla)
        }
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
