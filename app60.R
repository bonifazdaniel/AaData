########################################################################
# AaData
# Asistente de Análisis de Datos paso a paso (Shiny)
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
# Nota: 'httr' y 'jsonlite' solo se necesitan si activas la "IA avanzada".
# No son obligatorios; la app los verifica y avisa únicamente si los usas.

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

# Formatea un valor p en lenguaje claro (bilingüe)
interpretar_p <- function(p, idioma = "es", alfa = 0.05) {
  if (is.na(p)) {
    return(if (idioma == "en") "It was not possible to compute a p-value with these data."
           else "No fue posible calcular un valor p con estos datos.")
  }
  if (p < alfa) {
    if (idioma == "en")
      sprintf("The p-value is %.4f, which is LESS than 0.05. This suggests there IS a statistically significant difference.", p)
    else
      sprintf("El valor de p es %.4f, que es MENOR a 0.05. Esto sugiere que SÍ existe una diferencia estadísticamente significativa.", p)
  } else {
    if (idioma == "en")
      sprintf("The p-value is %.4f, which is GREATER than 0.05. This suggests there is NOT enough evidence of a statistically significant difference.", p)
    else
      sprintf("El valor de p es %.4f, que es MAYOR a 0.05. Esto sugiere que NO hay evidencia suficiente de una diferencia estadísticamente significativa.", p)
  }
}

# Quita los acentos y la eñe de un texto (á->a, é->e, ñ->n, etc.)
quitar_acentos <- function(x) {
  chartr("áéíóúÁÉÍÓÚñÑüÜ", "aeiouAEIOUnNuU", x)
}

# Copia un archivo subido a una ruta temporal CON la extensión correcta.
# Shiny guarda los archivos con un nombre temporal que a veces no conserva la
# extensión; sin ella, readxl intenta abrir el .xlsx como zip y falla. Con esto
# nos aseguramos de que readxl reconozca el formato.
preparar_ruta_excel <- function(datapath, nombre) {
  ext <- tolower(tools::file_ext(nombre))
  if (!ext %in% c("xlsx", "xls")) ext <- "xlsx"
  tmp <- tempfile(fileext = paste0(".", ext))
  file.copy(datapath, tmp, overwrite = TRUE)
  tmp
}

# Traduce errores técnicos de lectura de Excel a un mensaje claro para el usuario.
mensaje_error_lectura <- function(msg) {
  if (grepl("zip|cannot be opened|Failed to open|not a zip|libxls|corrupt|evaluation error",
            msg, ignore.case = TRUE)) {
    paste0("El archivo no parece ser un Excel válido (puede estar dañado, o ser un CSV/HTML ",
           "con extensión .xlsx). Sugerencia: ábrelo en Excel y usa 'Guardar como' \u2192 ",
           "Libro de Excel (.xlsx), o guárdalo como CSV y súbelo así.")
  } else {
    msg
  }
}

# ¿El archivo es en realidad una tabla HTML guardada como .xls/.xlsx?
# (Común en exportaciones de sistemas clínicos y de laboratorio.)
parece_html <- function(path) {
  txt <- tryCatch(tolower(paste(readLines(path, n = 40, warn = FALSE), collapse = " ")),
                  error = function(e) "")
  grepl("<table|<html|<!doctype html|<tr|<td|<tbody|xmlns", txt)
}

# Intenta leer una tabla HTML (usa rvest/xml2 o XML si están instalados).
leer_html_tabla <- function(path) {
  if (requireNamespace("rvest", quietly = TRUE) && requireNamespace("xml2", quietly = TRUE)) {
    return(tryCatch({
      tabs <- rvest::html_table(xml2::read_html(path), fill = TRUE)
      if (length(tabs) > 0) as.data.frame(tabs[[which.max(vapply(tabs, ncol, integer(1)))]])
      else NULL
    }, error = function(e) NULL))
  }
  if (requireNamespace("XML", quietly = TRUE)) {
    return(tryCatch({
      tabs <- XML::readHTMLTable(path, stringsAsFactors = FALSE)
      tabs <- Filter(Negate(is.null), tabs)
      if (length(tabs) > 0) tabs[[which.max(vapply(tabs, ncol, integer(1)))]] else NULL
    }, error = function(e) NULL))
  }
  NULL
}

# Intenta leer texto delimitado probando varios separadores y se queda con el
# que produce más columnas (útil cuando un "Excel" es en realidad texto).
leer_texto_auto <- function(path) {
  seps <- c(",", ";", "\t", "|")
  mejor <- NULL; ncmax <- 1
  for (s in seps) {
    d <- tryCatch(read.csv(path, sep = s, stringsAsFactors = FALSE, check.names = FALSE,
                           na.strings = c("NA", "", "NaN", "na", "N/A")),
                  error = function(e) NULL)
    if (!is.null(d) && ncol(d) > ncmax) { mejor <- d; ncmax <- ncol(d) }
  }
  mejor
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

## ---- Asistente inteligente (recomendaciones) ------------------------

# Detecta si un nombre de variable sugiere tiempo, identificador de sujeto, etc.
es_como_nombre <- function(nombre, patrones) {
  any(grepl(paste(patrones, collapse = "|"), nombre, ignore.case = TRUE))
}

# Construye un "perfil" de la tabla actual: tamaño, tipos, grupos, faltantes,
# si hay variable de tiempo o de identificación de sujeto, etc.
perfil_datos <- function(df) {
  n <- nrow(df); p <- ncol(df)
  vars_num <- obtener_vars_numericas(df)
  vars_cat <- obtener_vars_categoricas(df)
  # variables de grupo candidatas: categóricas con 2 a 10 niveles
  grupos_cand <- character(0)
  niveles_grupo <- integer(0)
  for (v in vars_cat) {
    k <- length(unique(stats::na.omit(df[[v]])))
    if (k >= 2 && k <= 10) { grupos_cand <- c(grupos_cand, v); niveles_grupo <- c(niveles_grupo, k) }
  }
  # variable de tiempo candidata
  tiempo_cand <- names(df)[vapply(names(df), function(nm)
    es_como_nombre(nm, c("dia", "día", "tiempo", "time", "semana", "week", "mes", "day", "visita")), logical(1))]
  tiempo_cand <- intersect(tiempo_cand, c(vars_num, vars_cat))
  # variable de identificación de sujeto
  id_cand <- names(df)[vapply(names(df), function(nm)
    es_como_nombre(nm, c("id", "raton", "ratón", "sujeto", "paciente", "folio", "muestra")), logical(1))]
  # faltantes
  pct_na <- round(100 * sum(is.na(df)) / (n * p), 1)
  cols_con_na <- sum(vapply(df, function(x) any(is.na(x)), logical(1)))
  # tamaño de muestra
  tamano <- if (n < 15) "muy pequeña" else if (n <= 40) "pequeña"
            else if (n <= 150) "moderada" else "grande"

  list(
    n = n, p = p, vars_num = vars_num, vars_cat = vars_cat,
    grupos_cand = grupos_cand, niveles_grupo = niveles_grupo,
    tiempo_cand = tiempo_cand, id_cand = id_cand,
    pct_na = pct_na, cols_con_na = cols_con_na,
    tamano = tamano,
    hay_texto = any(vapply(df, is.character, logical(1)))
  )
}

# Detecta la "intención" a partir del texto del propósito del usuario.
detectar_intencion <- function(texto) {
  t <- tolower(texto)
  list(
    comparar   = grepl("compar|diferenc|grupo|tratamiento|control|efecto|versus|vs", t),
    relacion   = grepl("relaci|correlaci|asociaci|depende|influye|predic|regres", t),
    tiempo     = grepl("tiempo|evoluci|progres|longitud|semana|d[ií]a|seguimiento|curva", t),
    predecir   = grepl("predic|regres|model|estimar|probabil|riesgo|factor", t),
    describir  = grepl("describ|resum|explor|caracteriz|general|panorama", t),
    frecuencia = grepl("frecuen|proporci|porcentaje|cuent|distribuci[óo]n de categor", t),
    superviv   = grepl("superviv|survival|kaplan|tiempo hasta|evento|mortal|reca[ií]da", t)
  )
}

# Genera recomendaciones LOCALES (sin enviar datos a ningún servidor).
recomendar_local <- function(df, proposito, idioma = "es") {
  pf <- perfil_datos(df)
  intn <- detectar_intencion(if (is.null(proposito)) "" else proposito)
  L <- function(es, en) if (idioma == "en") en else es

  tam_disp <- switch(pf$tamano,
                     "muy pequeña" = L("muy pequeña", "very small"),
                     "pequeña" = L("pequeña", "small"),
                     "moderada" = L("moderada", "moderate"),
                     "grande" = L("grande", "large"),
                     pf$tamano)

  encabezado <- sprintf(
    L("Con base en tus datos (%d observaciones, %d variables; %d numéricas y %d categóricas; muestra %s; %.1f%% de datos faltantes)%s:",
      "Based on your data (%d observations, %d variables; %d numeric and %d categorical; %s sample; %.1f%% missing data)%s:"),
    pf$n, pf$p, length(pf$vars_num), length(pf$vars_cat), tam_disp, pf$pct_na,
    if (nzchar(trimws(proposito)))
      sprintf(L(" y tu objetivo (\u201c%s\u201d)", " and your goal (\u201c%s\u201d)"), trimws(proposito)) else ""
  )

  # ---- Limpieza ----
  limp <- character(0)
  if (pf$pct_na > 0)
    limp <- c(limp, sprintf(
      L("Tienes datos faltantes en %d columna(s). Si la muestra es %s, evita borrar filas a la ligera: valora conservar el máximo de datos y solo quitar NA en las variables que vayas a analizar.",
        "You have missing data in %d column(s). If the sample is %s, avoid deleting rows carelessly: consider keeping as much data as possible and only removing NAs in the variables you will analyze."),
      pf$cols_con_na, tam_disp))
  if (pf$hay_texto)
    limp <- c(limp, L("Estandariza el texto de las variables categóricas (unifica mayúsculas/minúsculas y espacios) para que 'Control' y 'control' no se cuenten como grupos distintos.",
                       "Standardize the text of categorical variables (unify case and spacing) so that 'Control' and 'control' are not counted as different groups."))
  limp <- c(limp, L("Detecta valores atípicos (outliers): con muestras pequeñas un solo dato extremo puede distorsionar todo; revísalos antes de decidir si son errores de captura.",
                     "Detect outliers: with small samples a single extreme value can distort everything; review them before deciding whether they are data-entry errors."))
  limp <- c(limp, L("Revisa que las variables numéricas estén como número (no como texto) y limpia los nombres de columnas si traen acentos o símbolos.",
                     "Check that numeric variables are stored as numbers (not text) and clean column names if they contain accents or symbols."))
  if (pf$n < 15)
    limp <- c(limp, L("Ojo: tu muestra es muy pequeña; eliminar filas reduce mucho el poder estadístico. Prefiere marcar/imputar antes que borrar.",
                       "Note: your sample is very small; deleting rows greatly reduces statistical power. Prefer marking/imputing over deleting."))

  # ---- Análisis ----
  ana <- character(0)
  hay_grupo <- length(pf$grupos_cand) > 0
  dos_niveles <- hay_grupo && any(pf$niveles_grupo == 2)
  mas_niveles <- hay_grupo && any(pf$niveles_grupo > 2)
  no_param <- pf$tamano %in% c("muy pequeña", "pequeña")

  if (intn$describir || length(proposito) == 0 || !nzchar(trimws(proposito)))
    ana <- c(ana, L("Empieza con estadística descriptiva (medias, medianas, desviaciones) para conocer tus variables antes de cualquier prueba.",
                     "Start with descriptive statistics (means, medians, standard deviations) to get to know your variables before any test."))
  if (intn$comparar || hay_grupo) {
    if (dos_niveles)
      ana <- c(ana, sprintf(L("Para comparar 2 grupos%s usa %s.", "To compare 2 groups%s use %s."),
                            if (length(pf$grupos_cand)) sprintf(L(" (ej. %s)", " (e.g. %s)"), pf$grupos_cand[1]) else "",
                            if (no_param) L("la prueba de Mann-Whitney/Wilcoxon (no paramétrica, más segura con pocos datos)",
                                            "the Mann-Whitney/Wilcoxon test (non-parametric, safer with few data)")
                            else L("la prueba t de Student (verifica antes la normalidad)",
                                   "Student's t-test (check normality first)")))
    if (mas_niveles)
      ana <- c(ana, sprintf(L("Para comparar más de 2 grupos usa %s.", "To compare more than 2 groups use %s."),
                            if (no_param) L("Kruskal-Wallis (no paramétrica)", "Kruskal-Wallis (non-parametric)")
                            else L("ANOVA de una vía, con prueba post-hoc si sale significativa",
                                   "one-way ANOVA, with a post-hoc test if it is significant")))
    ana <- c(ana, L("Complementa el valor p con el tamaño del efecto (d de Cohen): con muestras pequeñas, una diferencia real puede no dar 'significativa' pero sí tener un efecto relevante.",
                     "Complement the p-value with the effect size (Cohen's d): with small samples, a real difference may not be 'significant' but can still have a relevant effect."))
  }
  if (intn$tiempo && length(pf$tiempo_cand) > 0) {
    if (length(pf$id_cand) > 0)
      ana <- c(ana, L("Como mides al mismo sujeto en varios momentos, el análisis correcto es un ANOVA de medidas repetidas / modelo mixto (toma en cuenta que las mediciones del mismo sujeto están correlacionadas).",
                       "Since you measure the same subject at several time points, the correct analysis is a repeated-measures ANOVA / mixed model (it accounts for the fact that measurements from the same subject are correlated)."))
    else
      ana <- c(ana, L("Para ver el cambio a lo largo del tiempo, resume la evolución (promedio ± error estándar por momento y grupo).",
                       "To see change over time, summarize the trend (mean ± standard error by time point and group)."))
  }
  if (intn$relacion)
    ana <- c(ana, sprintf(L("Para relación entre dos variables numéricas usa correlación de %s; si son varias, una matriz de correlación.",
                            "For the relationship between two numeric variables use %s correlation; if several, a correlation matrix."),
                          if (no_param) L("Spearman (por rangos, robusta con pocos datos)", "Spearman (rank-based, robust with few data)")
                          else "Pearson"))
  if (intn$predecir)
    ana <- c(ana, L("Si quieres predecir: regresión lineal para un desenlace numérico, o regresión logística para uno binario (sí/no). Con muestras pequeñas usa pocos predictores para no sobreajustar.",
                     "If you want to predict: linear regression for a numeric outcome, or logistic regression for a binary one (yes/no). With small samples use few predictors to avoid overfitting."))
  if (intn$frecuencia)
    ana <- c(ana, L("Para variables categóricas usa tablas de frecuencias; para ver asociación entre dos categóricas, tabla de contingencia (chi-cuadrada, o Fisher si hay conteos pequeños).",
                     "For categorical variables use frequency tables; to see the association between two categoricals, a contingency table (chi-square, or Fisher if there are small counts)."))
  if (intn$superviv && length(pf$id_cand) > 0)
    ana <- c(ana, L("Si mides tiempo hasta un evento, usa análisis de supervivencia (Kaplan-Meier y prueba de log-rank entre grupos).",
                     "If you measure time to an event, use survival analysis (Kaplan-Meier and the log-rank test between groups)."))
  if (length(ana) == 0)
    ana <- c(ana, L("Describe primero tus variables; luego, según lo que busques, compara grupos, mide relaciones o modela. Escribe tu objetivo arriba para una sugerencia más precisa.",
                     "First describe your variables; then, depending on your goal, compare groups, measure relationships or model. Type your goal above for a more precise suggestion."))
  if (no_param)
    ana <- c(ana, L("Recomendación general: con tu tamaño de muestra prefiere pruebas no paramétricas y reporta intervalos de confianza.",
                     "General recommendation: with your sample size, prefer non-parametric tests and report confidence intervals."))

  # ---- Gráficas ----
  graf <- character(0)
  if (intn$comparar || hay_grupo)
    graf <- c(graf, sprintf(L("Para comparar grupos: %s.", "To compare groups: %s."),
                            if (pf$tamano %in% c("muy pequeña","pequeña"))
                              L("gráfica de violín o boxplot MOSTRANDO los puntos individuales (con pocos datos conviene ver cada observación), o barras con error estándar",
                                "violin plot or boxplot SHOWING the individual points (with few data it helps to see each observation), or bars with standard error")
                            else L("boxplot o barras con error estándar (media ± EE)",
                                   "boxplot or bars with standard error (mean ± SE)")))
  if (intn$tiempo && length(pf$tiempo_cand) > 0) {
    graf <- c(graf, L("Para el tiempo: línea de tiempo con promedio ± error estándar por grupo.",
                       "For time: a time line with mean ± standard error by group."))
    if (length(pf$id_cand) > 0)
      graf <- c(graf, L("Añade una gráfica de líneas individuales por sujeto (spaghetti) para ver la trayectoria de cada uno y detectar variabilidad.",
                         "Add an individual-lines-per-subject plot (spaghetti) to see each subject's trajectory and detect variability."))
  }
  if (intn$relacion) {
    graf <- c(graf, L("Para relación entre dos variables: diagrama de dispersión.",
                       "For the relationship between two variables: a scatter plot."))
    if (length(pf$vars_num) >= 3)
      graf <- c(graf, L("Para ver muchas variables a la vez: mapa de calor de correlaciones.",
                         "To see many variables at once: a correlation heatmap."))
  }
  if (intn$frecuencia || (!intn$comparar && !intn$relacion && !intn$tiempo))
    graf <- c(graf, L("Para distribuciones: histograma (numéricas) o gráfica de barras (categóricas).",
                       "For distributions: a histogram (numeric) or a bar chart (categorical)."))
  if (intn$superviv && length(pf$id_cand) > 0)
    graf <- c(graf, L("Para tiempo hasta un evento: curva de supervivencia de Kaplan-Meier.",
                       "For time to an event: a Kaplan-Meier survival curve."))
  if (length(graf) == 0)
    graf <- c(graf, L("Empieza con histogramas para ver la forma de tus variables y boxplots para comparar entre grupos.",
                       "Start with histograms to see the shape of your variables and boxplots to compare between groups."))

  list(encabezado = encabezado, limpieza = limp, analisis = ana, graficas = graf)
}

# Construye un resumen (SOLO METADATOS, nunca datos crudos) para la IA opcional.
construir_resumen_ia <- function(df, proposito) {
  pf <- perfil_datos(df)
  descr_cols <- vapply(names(df), function(nm) {
    x <- df[[nm]]
    tipo <- if (is.numeric(x)) "numérica" else "categórica/texto"
    k <- length(unique(stats::na.omit(x)))
    na <- round(100 * mean(is.na(x)), 1)
    sprintf("- %s: %s, %d valores distintos, %.0f%% faltantes", nm, tipo, k, na)
  }, character(1))
  paste0(
    "Eres un bioestadístico que asesora a investigadores clínicos. Con base en el ",
    "PERFIL de un conjunto de datos (no se incluyen los datos crudos por privacidad), ",
    "recomienda de forma concreta y en español: (1) qué limpieza conviene, (2) qué ",
    "análisis estadístico usar y (3) qué gráficas hacer. Considera el tamaño de muestra ",
    "y el objetivo del usuario. Responde en 3 secciones breves con viñetas.\n\n",
    sprintf("PERFIL: %d observaciones, %d variables. Muestra %s. %.1f%% de datos faltantes.\n",
            pf$n, pf$p, pf$tamano, pf$pct_na),
    "Variables:\n", paste(descr_cols, collapse = "\n"),
    "\n\nObjetivo del usuario: ", if (nzchar(trimws(proposito))) proposito else "(no especificado)"
  )
}

# Llama a la API de Anthropic (opcional). Devuelve el texto o un mensaje de error.
llamar_anthropic <- function(prompt, api_key, modelo = "claude-3-5-sonnet-latest") {
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE))
    return("Para usar la IA avanzada instala los paquetes 'httr' y 'jsonlite' (install.packages(c('httr','jsonlite'))).")
  cuerpo <- list(
    model = modelo,
    max_tokens = 1024,
    messages = list(list(role = "user", content = prompt))
  )
  resp <- tryCatch(
    httr::POST(
      "https://api.anthropic.com/v1/messages",
      httr::add_headers(`x-api-key` = api_key,
                        `anthropic-version` = "2023-06-01",
                        `content-type` = "application/json"),
      body = jsonlite::toJSON(cuerpo, auto_unbox = TRUE),
      encode = "raw"
    ), error = function(e) NULL)
  if (is.null(resp)) return("No se pudo conectar con la API (revisa tu conexión a internet).")
  if (httr::status_code(resp) != 200)
    return(sprintf("La API respondió con un error (código %s). Revisa tu clave de API.", httr::status_code(resp)))
  datos <- tryCatch(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                                        simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(datos) || is.null(datos$content)) return("La API no devolvió texto utilizable.")
  textos <- vapply(datos$content, function(b) if (!is.null(b$text)) b$text else "", character(1))
  paste(textos, collapse = "\n")
}

## ---- 3. Definición de los pasos del asistente ------------------------

# Nombres de los pasos en español e inglés (para el selector de idioma).
NOMBRES_PASOS_I18N <- list(
  es = c("Bienvenida", "Cargar datos", "Vista previa", "Limpieza",
         "Análisis", "Graficar", "Descargar"),
  en = c("Welcome", "Load data", "Preview", "Cleaning",
         "Analysis", "Charts", "Download")
)
TOTAL_PASOS <- length(NOMBRES_PASOS_I18N$es)

# Diccionario de traducciones del marco (encabezado, navegación, bienvenida,
# pie de página). El resto de los pasos se irá traduciendo de forma incremental.
TRAD <- list(
  subtitulo = list(
    es = "Asistente de análisis de datos paso a paso",
    en = "Step-by-step data analysis assistant"),
  reiniciar = list(es = "Reiniciar", en = "Restart"),
  siguiente = list(es = "Siguiente \u2192", en = "Next \u2192"),
  atras     = list(es = "\u2190 Atrás", en = "\u2190 Back"),
  comenzar  = list(es = "Comenzar \u2192", en = "Start \u2192"),
  pie = list(
    es = "AaData · Asistente de análisis de datos paso a paso, para investigación clínica y básica (ej. modelos de artritis reumatoide en ratones) · No sustituye la asesoría de un bioestadístico. Por: Ing. Daniel Bonifaz-Calvo Ibarrola",
    en = "AaData · Step-by-step data analysis assistant, for clinical and basic research (e.g. mouse models of rheumatoid arthritis) · Not a substitute for a biostatistician's advice. By: Eng. Daniel Bonifaz-Calvo Ibarrola"),
  bienvenida_titulo = list(es = "¡Bienvenido(a)!", en = "Welcome!"),
  bienvenida_intro = list(
    es = "Esta aplicación te guiará, paso a paso, para analizar y graficar tus datos.",
    en = "This application will guide you, step by step, to analyze and chart your data."),
  paso1_item = list(
    es = c("Paso 1: Bienvenida (aquí estás)",
           "Paso 2: Cargar tu archivo de datos",
           "Paso 3: Revisar una vista previa",
           "Paso 4: Limpiar los datos (opcional)",
           "Paso 5: Elegir y correr un análisis estadístico",
           "Paso 6: Generar gráficas",
           "Paso 7: Descargar tus resultados"),
    en = c("Step 1: Welcome (you are here)",
           "Step 2: Load your data file",
           "Step 3: Review a preview",
           "Step 4: Clean the data (optional)",
           "Step 5: Choose and run a statistical analysis",
           "Step 6: Generate charts",
           "Step 7: Download your results")),
  falta_datos = list(
    es = "Primero carga tus datos (usa la importación avanzada, el set de ejemplo, o sube un archivo) antes de continuar.",
    en = "Please load your data first (use advanced import, the example dataset, or upload a file) before continuing."),
  puedes_atras = list(
    es = "Puedes ir hacia atrás en cualquier momento sin perder tu información.",
    en = "You can go back at any time without losing your information."),

  # ---- Paso 2: Cargar datos ----
  p2_titulo = list(es = "¿Deseas subir un archivo?", en = "Do you want to upload a file?"),
  p2_intro = list(
    es = "Puedes subir un archivo CSV o Excel (.xlsx) con tus datos. Cada columna debe representar una variable (por ejemplo: Grupo, Día, Puntaje clínico, Peso, etc.) y cada fila una observación (por ejemplo, un ratón en un día determinado).",
    en = "You can upload a CSV or Excel (.xlsx) file with your data. Each column should represent a variable (for example: Group, Day, Clinical score, Weight, etc.) and each row an observation (for example, a mouse on a given day)."),
  p2_opt_propio = list(es = "Subir mi propio archivo", en = "Upload my own file"),
  p2_opt_avanzado = list(es = "Importación avanzada (Excel con encabezados en varias filas)",
                          en = "Advanced import (Excel with multi-row headers)"),
  p2_opt_ejemplo = list(es = "Usar datos de ejemplo (estudio de artritis reumatoide en ratones)",
                         en = "Use example dataset (rheumatoid arthritis study in mice)"),
  p2_file_label = list(es = "Selecciona tu archivo (.csv, .xlsx o .xls)",
                        en = "Select your file (.csv, .xlsx or .xls)"),
  p2_csv_sep = list(es = "Separador de columnas (solo CSV)", en = "Column separator (CSV only)"),
  p2_sep_coma = list(es = "Coma ( , )", en = "Comma ( , )"),
  p2_sep_puntocoma = list(es = "Punto y coma ( ; )", en = "Semicolon ( ; )"),
  p2_sep_tab = list(es = "Tabulador", en = "Tab"),
  p2_csv_dec = list(es = "Separador decimal (solo CSV)", en = "Decimal separator (CSV only)"),
  p2_dec_punto = list(es = "Punto ( . )", en = "Period ( . )"),
  p2_dec_coma = list(es = "Coma ( , )", en = "Comma ( , )"),
  p2_csv_enc = list(es = "Codificación de texto (solo CSV)", en = "Text encoding (CSV only)"),
  p2_enc_latin = list(es = "Latin1 (Windows/Excel en español)", en = "Latin1 (Windows/Spanish Excel)"),
  p2_ayuda_csv = list(
    es = "Consejo: si tu archivo viene de Excel en español y al subirlo ves las letras con acentos mal (ej. 'artritis' se ve como 'artrÃ­tis'), cambia la codificación a 'Latin1'. Si tu CSV viene de Excel en configuración regional en español, prueba con 'Punto y coma'.",
    en = "Tip: if your file comes from Spanish Excel and accented letters look wrong after uploading, change the encoding to 'Latin1'. If your CSV comes from Excel with Spanish regional settings, try 'Semicolon'."),
  p2_av_intro = list(
    es = "Usa esta opción para archivos de Excel donde los encabezados ocupan <strong>varias filas</strong> (categoría, subcategoría, variable), hay <strong>celdas combinadas</strong>, columnas o filas de margen, o los datos no empiezan en la primera fila. La app combinará los encabezados en un solo nombre por columna y dejará una tabla lista para analizar.",
    en = "Use this option for Excel files where headers span <strong>several rows</strong> (category, subcategory, variable), there are <strong>merged cells</strong>, margin rows or columns, or the data does not start on the first row. The app will combine the headers into a single name per column and produce a table ready for analysis."),
  p2_av_file = list(es = "Selecciona tu archivo de Excel (.xlsx o .xls)",
                     en = "Select your Excel file (.xlsx or .xls)"),
  p2_ej_intro = list(
    es = "Se usará un conjunto de datos simulado de un estudio con 3 grupos de ratones (Control, AR, AR+Tratamiento) medidos en los días 0, 7, 14, 21 y 28, con las variables: Puntaje clínico, Peso (g) e IL-6 (pg/mL).",
    en = "A simulated dataset will be used from a study with 3 groups of mice (Control, RA, RA+Treatment) measured on days 0, 7, 14, 21 and 28, with the variables: Clinical score, Weight (g) and IL-6 (pg/mL)."),
  p2_ej_boton = list(es = "Cargar datos de ejemplo", en = "Load example dataset"),

  # panel de importación avanzada
  av_error_hoja = list(
    es = "No se pudo leer el archivo de Excel. Verifica que sea un .xlsx o .xls válido (si lo abriste en Excel, prueba 'Guardar como' \u2192 Libro de Excel .xlsx).",
    en = "The Excel file could not be read. Make sure it is a valid .xlsx or .xls (if you opened it in Excel, try 'Save As' \u2192 Excel Workbook .xlsx)."),
  av_hoja = list(es = "¿Qué hoja quieres analizar?", en = "Which sheet do you want to analyze?"),
  av_preview_titulo = list(es = "Vista previa del archivo (tal cual, con números de fila y columna)",
                            en = "File preview (as-is, with row and column numbers)"),
  av_preview_ayuda = list(
    es = "Fíjate en qué fila(s) están los nombres y en qué fila empiezan los datos para configurar los valores de abajo.",
    en = "Look at which row(s) contain the names and which row the data starts on to set the values below."),
  av_fila_head = list(es = "Primera fila de encabezados", en = "First header row"),
  av_n_head = list(es = "¿Cuántas filas de encabezado?", en = "How many header rows?"),
  av_fila_datos = list(es = "Primera fila de datos", en = "First data row"),
  av_col_ini = list(es = "Primera columna con datos", en = "First column with data"),
  av_rellenar = list(es = "Rellenar celdas combinadas de los encabezados (recomendado)",
                      en = "Fill merged header cells (recommended)"),
  av_construir = list(es = "Construir tabla para analizar", en = "Build table for analysis"),
  av_nota_fecha = list(
    es = "Nota: en este modo, las columnas de fecha pueden requerir revisión posterior. El objetivo principal es dejar listas las variables numéricas y de grupo.",
    en = "Note: in this mode, date columns may need later review. The main goal is to prepare the numeric and grouping variables."),

  # mensajes de carga / vista previa
  carga_ok = list(es = "\u2713 Datos cargados correctamente: %d filas \u00d7 %d columnas.",
                   en = "\u2713 Data loaded successfully: %d rows \u00d7 %d columns."),
  carga_none = list(es = "Aún no se han cargado datos.", en = "No data loaded yet."),
  prev_fila = list(es = "Fila", en = "Row"),
  prev_col = list(es = "Col", en = "Col"),
  notif_ejemplo = list(es = "Datos de ejemplo cargados.", en = "Example dataset loaded."),
  notif_no_columnas = list(
    es = "El archivo se leyó pero no tiene columnas. Revisa el formato o el separador (si es CSV).",
    en = "The file was read but has no columns. Check the format or the separator (if CSV)."),
  notif_html = list(es = "El archivo era una tabla HTML con extensión de Excel; se leyó correctamente.",
                     en = "The file was an HTML table with an Excel extension; it was read successfully."),
  notif_texto = list(es = "El archivo no era un Excel real; se leyó como texto delimitado.",
                      en = "The file was not a real Excel; it was read as delimited text."),
  notif_error_lectura = list(es = "No se pudo leer el archivo:", en = "The file could not be read:"),
  av_tabla_ok = list(es = "Tabla construida: %d filas x %d columnas. Ya puedes continuar.",
                      en = "Table built: %d rows x %d columns. You can continue now."),
  av_tabla_error = list(es = "No se pudo construir la tabla:", en = "The table could not be built:"),

  # ---- Paso 3: Vista previa ----
  p3_titulo = list(es = "Vista previa de tus datos", en = "Preview of your data"),
  p3_dimensiones = list(
    es = "Tu archivo tiene %d filas (observaciones) y %d columnas (variables).",
    en = "Your file has %d rows (observations) and %d columns (variables)."),
  p3_resumen = list(es = "Resumen por variable", en = "Summary by variable"),

  # ---- Paso 4: Limpieza ----
  p4_titulo = list(es = "¿Quieres limpiar tus datos?", en = "Do you want to clean your data?"),
  p4_intro = list(
    es = "Selecciona las opciones de limpieza que quieras aplicar. Si no necesitas limpiar nada, simplemente da clic en 'Siguiente'. Puedes marcar varias a la vez.",
    en = "Select the cleaning options you want to apply. If you don't need to clean anything, just click 'Next'. You can check several at once."),
  p4_op_quitar_na = list(es = "Eliminar filas con datos faltantes (NA)", en = "Remove rows with missing data (NA)"),
  p4_op_col_vacia = list(es = "Eliminar columnas vacías (100% de datos faltantes)", en = "Remove empty columns (100% missing data)"),
  p4_op_duplicados = list(es = "Eliminar filas duplicadas", en = "Remove duplicate rows"),
  p4_op_trim = list(es = "Quitar espacios en blanco al inicio/final del texto", en = "Trim leading/trailing whitespace from text"),
  p4_op_estandarizar = list(es = "Estandarizar texto en variables categóricas (unificar mayúsculas/minúsculas y espacios)",
                             en = "Standardize text in categorical variables (unify case and spacing)"),
  p4_op_texto_num = list(es = "Convertir a números las columnas de texto que en realidad son numéricas",
                          en = "Convert text columns that are actually numeric into numbers"),
  p4_op_redondear = list(es = "Redondear valores numéricos", en = "Round numeric values"),
  p4_op_rango = list(es = "Filtrar valores fuera de un rango válido", en = "Filter out values outside a valid range"),
  p4_op_outliers = list(es = "Detectar valores atípicos (outliers)", en = "Detect outliers"),
  p4_op_nombres = list(es = "Limpiar nombres de columnas (quitar acentos, espacios y símbolos)",
                        en = "Clean column names (remove accents, spaces and symbols)"),
  p4_decimales = list(es = "Número de decimales:", en = "Number of decimals:"),
  p4_rango_ayuda = list(
    es = "Se eliminarán las filas cuyo valor en la columna elegida quede fuera del rango [mínimo, máximo].",
    en = "Rows whose value in the chosen column falls outside the [minimum, maximum] range will be removed."),
  p4_rango_col = list(es = "Columna numérica:", en = "Numeric column:"),
  p4_rango_min = list(es = "Valor mínimo permitido:", en = "Minimum allowed value:"),
  p4_rango_max = list(es = "Valor máximo permitido:", en = "Maximum allowed value:"),
  p4_outlier_ayuda = list(
    es = "Un valor atípico es un dato muy alejado del resto (posible error de captura). Se detectan con el rango intercuartílico (regla de 1.5 \u00d7 IQR) en las columnas numéricas.",
    en = "An outlier is a value far from the rest (possible data-entry error). They are detected using the interquartile range (1.5 \u00d7 IQR rule) on numeric columns."),
  p4_outlier_pregunta = list(es = "¿Qué hacer con los valores atípicos?", en = "What to do with outliers?"),
  p4_outlier_marcar = list(es = "Solo marcarlos como faltante (NA) sin borrar la fila", en = "Only mark them as missing (NA) without deleting the row"),
  p4_outlier_eliminar = list(es = "Eliminar la fila completa que contenga un valor atípico", en = "Delete the entire row containing an outlier"),
  p4_aplicar = list(es = "Aplicar limpieza", en = "Apply cleaning"),
  p4_sin = list(es = "No necesito limpiar, usar datos originales", en = "No cleaning needed, use original data"),
  p4_sin_notif = list(es = "Se usarán los datos originales sin cambios.", en = "The original data will be used unchanged."),
  p4_result_titulo = list(es = "Resultado de la limpieza", en = "Cleaning result"),
  p4_result_resumen = list(
    es = "Filas: %d \u2192 %d · Columnas: %d \u2192 %d.",
    en = "Rows: %d \u2192 %d · Columns: %d \u2192 %d."),
  p4_result_acciones = list(es = "Acciones aplicadas:", en = "Actions applied:"),
  p4_result_sin_cambios = list(es = "No se seleccionó ninguna opción de limpieza.", en = "No cleaning option was selected."),
  p4_aplicada_notif = list(es = "Limpieza aplicada.", en = "Cleaning applied."),

  # ---- Paso 5: Análisis ----
  p5_titulo = list(es = "¿Qué tipo de análisis quieres hacer?", en = "What type of analysis do you want to do?"),
  p5_t_descriptivo = list(es = "Estadística descriptiva (promedios, medianas, etc.)", en = "Descriptive statistics (means, medians, etc.)"),
  p5_t_comparar = list(es = "Comparar grupos (2 o más grupos)", en = "Compare groups (2 or more groups)"),
  p5_t_pareada = list(es = "Comparación pareada (antes vs. después, mismo sujeto)", en = "Paired comparison (before vs. after, same subject)"),
  p5_t_cohen = list(es = "Tamaño del efecto (d de Cohen, 2 grupos)", en = "Effect size (Cohen's d, 2 groups)"),
  p5_t_correlacion = list(es = "Relación entre dos variables (correlación)", en = "Relationship between two variables (correlation)"),
  p5_t_matriz = list(es = "Matriz de correlación (varias variables a la vez)", en = "Correlation matrix (several variables at once)"),
  p5_t_frecuencias = list(es = "Tabla de frecuencias (variable categórica)", en = "Frequency table (categorical variable)"),
  p5_t_contingencia = list(es = "Tabla de contingencia (asociación entre 2 categóricas)", en = "Contingency table (association between 2 categoricals)"),
  p5_t_longitudinal = list(es = "Evolución en el tiempo (ej. puntaje clínico a lo largo de los días)", en = "Change over time (e.g. clinical score across days)"),
  p5_t_anova2 = list(es = "ANOVA de dos vías (grupo \u00d7 tiempo, con interacción)", en = "Two-way ANOVA (group \u00d7 time, with interaction)"),
  p5_t_mixto = list(es = "ANOVA de medidas repetidas / modelo mixto", en = "Repeated-measures ANOVA / mixed model"),
  p5_t_reg_lineal = list(es = "Regresión lineal (predecir una variable numérica)", en = "Linear regression (predict a numeric variable)"),
  p5_t_reg_logistica = list(es = "Regresión logística (predecir un desenlace sí/no)", en = "Logistic regression (predict a yes/no outcome)"),
  p5_t_normalidad = list(es = "Prueba de normalidad (Shapiro-Wilk) con recomendación", en = "Normality test (Shapiro-Wilk) with recommendation"),
  # controles comunes
  p5_desc_var = list(es = "Variable numérica a describir:", en = "Numeric variable to describe:"),
  p5_desc_grupo = list(es = "Agrupar por (opcional):", en = "Group by (optional):"),
  p5_sin_agrupar = list(es = "(Sin agrupar)", en = "(No grouping)"),
  p5_calcular = list(es = "Calcular", en = "Calculate"),
  p5_comp_grupo = list(es = "Variable de grupo (categórica):", en = "Grouping variable (categorical):"),
  p5_comp_num = list(es = "Variable numérica a comparar:", en = "Numeric variable to compare:"),
  p5_comp_boton = list(es = "Comparar grupos", en = "Compare groups"),
  p5_par_ayuda = list(es = "Compara el mismo sujeto en dos momentos (ej. Día 0 vs. Día 28). Se emparejan las observaciones por el identificador de sujeto.",
                       en = "Compares the same subject at two time points (e.g. Day 0 vs. Day 28). Observations are paired by the subject identifier."),
  p5_par_id = list(es = "Identificador de sujeto (ej. Raton_ID):", en = "Subject identifier (e.g. Mouse_ID):"),
  p5_par_cond = list(es = "Variable que distingue los dos momentos (ej. Dia):", en = "Variable distinguishing the two time points (e.g. Day):"),
  p5_par_m1 = list(es = "Momento 1 (antes):", en = "Time point 1 (before):"),
  p5_par_m2 = list(es = "Momento 2 (después):", en = "Time point 2 (after):"),
  p5_par_resp = list(es = "Variable de respuesta (numérica):", en = "Response variable (numeric):"),
  p5_par_boton = list(es = "Comparar (pareado)", en = "Compare (paired)"),
  p5_cohen_ayuda = list(es = "El tamaño del efecto (d de Cohen) indica qué tan grande es la diferencia entre dos grupos, más allá de si es significativa.",
                         en = "The effect size (Cohen's d) indicates how large the difference between two groups is, beyond whether it is significant."),
  p5_cohen_grupo = list(es = "Variable de grupo (debe tener 2 categorías):", en = "Grouping variable (must have 2 categories):"),
  p5_cohen_num = list(es = "Variable numérica:", en = "Numeric variable:"),
  p5_cohen_boton = list(es = "Calcular tamaño del efecto", en = "Calculate effect size"),
  p5_corr_var1 = list(es = "Primera variable numérica:", en = "First numeric variable:"),
  p5_corr_var2 = list(es = "Segunda variable numérica:", en = "Second numeric variable:"),
  p5_corr_boton = list(es = "Calcular correlación", en = "Calculate correlation"),
  p5_mcorr_ayuda = list(es = "Calcula la correlación entre todas las variables numéricas que elijas (mínimo 2).",
                         en = "Computes the correlation among all the numeric variables you choose (minimum 2)."),
  p5_mcorr_vars = list(es = "Variables numéricas:", en = "Numeric variables:"),
  p5_metodo = list(es = "Método:", en = "Method:"),
  p5_pearson = list(es = "Pearson (relación lineal)", en = "Pearson (linear relationship)"),
  p5_spearman = list(es = "Spearman (por rangos, no paramétrico)", en = "Spearman (rank-based, non-parametric)"),
  p5_mcorr_boton = list(es = "Calcular matriz", en = "Calculate matrix"),
  p5_freq_var = list(es = "Variable categórica:", en = "Categorical variable:"),
  p5_freq_boton = list(es = "Calcular frecuencias", en = "Calculate frequencies"),
  p5_cont_ayuda = list(es = "Evalúa si dos variables categóricas están asociadas. Se usa chi-cuadrada, o la prueba exacta de Fisher cuando los conteos esperados son pequeños.",
                        en = "Tests whether two categorical variables are associated. Chi-square is used, or Fisher's exact test when expected counts are small."),
  p5_cont_var1 = list(es = "Primera variable categórica:", en = "First categorical variable:"),
  p5_cont_var2 = list(es = "Segunda variable categórica:", en = "Second categorical variable:"),
  p5_cont_boton = list(es = "Analizar asociación", en = "Analyze association"),
  p5_long_tiempo = list(es = "Variable de tiempo (ej. Día):", en = "Time variable (e.g. Day):"),
  p5_long_grupo = list(es = "Variable de grupo:", en = "Grouping variable:"),
  p5_long_resp = list(es = "Variable de respuesta (ej. Puntaje clínico):", en = "Response variable (e.g. Clinical score):"),
  p5_long_boton = list(es = "Calcular evolución en el tiempo", en = "Calculate change over time"),
  p5_long_consejo = list(es = "Consejo: en el paso 6 (Graficar) podrás visualizar esta evolución en una línea de tiempo.",
                          en = "Tip: in step 6 (Charts) you can visualize this change as a time line."),
  p5_a2_ayuda = list(es = "Evalúa el efecto de dos factores (ej. grupo y tiempo) y su interacción sobre una variable numérica.",
                      en = "Evaluates the effect of two factors (e.g. group and time) and their interaction on a numeric variable."),
  p5_a2_f1 = list(es = "Primer factor (ej. Grupo):", en = "First factor (e.g. Group):"),
  p5_a2_f2 = list(es = "Segundo factor (ej. Día):", en = "Second factor (e.g. Day):"),
  p5_a2_resp = list(es = "Variable de respuesta (numérica):", en = "Response variable (numeric):"),
  p5_a2_boton = list(es = "Calcular ANOVA de dos vías", en = "Calculate two-way ANOVA"),
  p5_mix_ayuda = list(es = "Análisis correcto cuando el mismo sujeto se mide en varios momentos (los datos no son independientes). Modela grupo, tiempo y su interacción, con el sujeto como efecto aleatorio.",
                       en = "The correct analysis when the same subject is measured at several time points (data are not independent). Models group, time and their interaction, with subject as a random effect."),
  p5_mix_id = list(es = "Identificador de sujeto (ej. Raton_ID):", en = "Subject identifier (e.g. Mouse_ID):"),
  p5_mix_grupo = list(es = "Variable de grupo:", en = "Grouping variable:"),
  p5_mix_tiempo = list(es = "Variable de tiempo (ej. Día):", en = "Time variable (e.g. Day):"),
  p5_mix_resp = list(es = "Variable de respuesta (numérica):", en = "Response variable (numeric):"),
  p5_mix_boton = list(es = "Calcular modelo mixto", en = "Calculate mixed model"),
  p5_rl_ayuda = list(es = "Modela cómo una o más variables predicen una respuesta numérica.",
                      en = "Models how one or more variables predict a numeric response."),
  p5_rl_resp = list(es = "Variable de respuesta (a predecir, numérica):", en = "Response variable (to predict, numeric):"),
  p5_rl_pred = list(es = "Variable(s) predictora(s):", en = "Predictor variable(s):"),
  p5_rl_boton = list(es = "Ajustar regresión lineal", en = "Fit linear regression"),
  p5_rlog_ayuda = list(es = "Predice un desenlace binario (sí/no, 1/0) a partir de otras variables. La respuesta debe tener exactamente 2 categorías.",
                        en = "Predicts a binary outcome (yes/no, 1/0) from other variables. The response must have exactly 2 categories."),
  p5_rlog_resp = list(es = "Variable de desenlace (2 categorías, ej. Evento_artritis):", en = "Outcome variable (2 categories, e.g. Arthritis_event):"),
  p5_rlog_boton = list(es = "Ajustar regresión logística", en = "Fit logistic regression"),
  p5_norm_ayuda = list(es = "Revisa si una variable numérica sigue una distribución normal y te recomienda qué tipo de prueba conviene usar.",
                        en = "Checks whether a numeric variable follows a normal distribution and recommends which type of test to use."),
  p5_norm_var = list(es = "Variable numérica:", en = "Numeric variable:"),
  p5_norm_grupo = list(es = "Evaluar por grupo (opcional):", en = "Evaluate by group (optional):"),
  p5_todo_junto = list(es = "(Todo junto)", en = "(All together)"),
  p5_norm_boton = list(es = "Evaluar normalidad", en = "Evaluate normality"),
  # resultados del paso 5 (compartidos)
  p5_error = list(es = "No se pudo completar el análisis:", en = "The analysis could not be completed:"),
  p5_desc_n = list(es = "n", en = "n"),
  p5_desc_media = list(es = "Media", en = "Mean"),
  p5_desc_mediana = list(es = "Mediana", en = "Median"),
  p5_desc_de = list(es = "DE", en = "SD"),
  p5_desc_min = list(es = "Mínimo", en = "Minimum"),
  p5_desc_max = list(es = "Máximo", en = "Maximum"),
  p5_desc_grupo_col = list(es = "Grupo", en = "Group"),
  # comparación de grupos
  p5c_min2 = list(es = "Se necesitan al menos 2 grupos distintos para poder comparar. Verifica la variable de grupo seleccionada.",
                   en = "At least 2 distinct groups are needed to compare. Check the selected grouping variable."),
  p5c_t = list(es = "prueba t de Student", en = "Student's t-test"),
  p5c_wilcoxon = list(es = "prueba de Wilcoxon (Mann-Whitney)", en = "Wilcoxon (Mann-Whitney) test"),
  p5c_anova = list(es = "ANOVA de una vía", en = "one-way ANOVA"),
  p5c_kruskal = list(es = "prueba de Kruskal-Wallis", en = "Kruskal-Wallis test"),
  p5c_normal_si = list(es = "los datos se comportan de forma aproximadamente normal",
                        en = "the data are approximately normally distributed"),
  p5c_normal_no = list(es = "los datos NO se comportan de forma normal, por lo que se usó una prueba no paramétrica",
                        en = "the data are NOT normally distributed, so a non-parametric test was used"),
  p5c_comp2 = list(es = "Se compararon 2 grupos: %s.\nSe usó la %s (%s).\n\n",
                    en = "2 groups were compared: %s.\nThe %s was used (%s).\n\n"),
  p5c_compN = list(es = "Se compararon %d grupos: %s.\nSe usó %s (%s).\n\n",
                    en = "%d groups were compared: %s.\nThe %s was used (%s).\n\n"),
  p5c_posthoc = list(es = "\n\nSi el resultado general es significativo, revisa la tabla de comparaciones por pares (post-hoc) para ver entre qué grupos específicos hay diferencia.",
                      en = "\n\nIf the overall result is significant, check the pairwise (post-hoc) comparison table to see which specific groups differ."),
  p5c_titulo = list(es = "Comparación de grupos: %s según %s", en = "Group comparison: %s by %s"),

  # pareada
  p5p_dos_momentos = list(es = "Elige dos momentos distintos para comparar.", en = "Choose two different time points to compare."),
  p5p_insuf = list(es = "No hay suficientes sujetos con datos en ambos momentos para hacer la comparación pareada.",
                    en = "There are not enough subjects with data at both time points for the paired comparison."),
  p5p_t = list(es = "prueba t pareada", en = "paired t-test"),
  p5p_wilcoxon = list(es = "prueba de Wilcoxon pareada (no paramétrica)", en = "paired Wilcoxon test (non-parametric)"),
  p5p_texto = list(
    es = "Comparación pareada de '%s': %s vs. %s.\nSujetos emparejados: %d.\nCambio promedio (después - antes): %s.\nSe usó la %s.\n\n",
    en = "Paired comparison of '%s': %s vs. %s.\nPaired subjects: %d.\nMean change (after - before): %s.\nThe %s was used.\n\n"),
  p5p_titulo = list(es = "Comparación pareada: %s (%s vs. %s)", en = "Paired comparison: %s (%s vs. %s)"),
  # Cohen
  p5co_2cat = list(es = "La variable de grupo debe tener exactamente 2 categorías (tiene %d). Elige otra variable o filtra los datos.",
                    en = "The grouping variable must have exactly 2 categories (it has %d). Choose another variable or filter the data."),
  p5co_ins = list(es = "insignificante", en = "negligible"),
  p5co_peq = list(es = "pequeño", en = "small"),
  p5co_med = list(es = "mediano", en = "medium"),
  p5co_gra = list(es = "grande", en = "large"),
  p5co_texto = list(
    es = "Tamaño del efecto entre '%s' y '%s' para '%s'.\n\nd de Cohen = %s (efecto %s).\nIntervalo de confianza al 95%%: [%s, %s].\n\nReferencia: |d| ~ 0.2 pequeño, ~ 0.5 mediano, ~ 0.8 o más grande.",
    en = "Effect size between '%s' and '%s' for '%s'.\n\nCohen's d = %s (%s effect).\n95%% confidence interval: [%s, %s].\n\nReference: |d| ~ 0.2 small, ~ 0.5 medium, ~ 0.8 or more large."),
  p5co_titulo = list(es = "Tamaño del efecto (d de Cohen): %s por %s", en = "Effect size (Cohen's d): %s by %s"),
  # correlación
  p5cor_distintas = list(es = "Por favor selecciona dos variables distintas para calcular la correlación.",
                          en = "Please select two different variables to compute the correlation."),
  p5cor_insuf = list(es = "No hay suficientes pares de datos numéricos para calcular la correlación (se necesitan al menos 3). Verifica que ambas variables sean numéricas.",
                      en = "There are not enough numeric data pairs to compute the correlation (at least 3 are needed). Check that both variables are numeric."),
  p5cor_sinvar = list(es = "Una de las variables no varía (todos sus valores son iguales), así que no se puede calcular la correlación.",
                       en = "One of the variables does not vary (all its values are equal), so the correlation cannot be computed."),
  p5cor_nulo = list(es = "No fue posible calcular la correlación con estos datos.", en = "It was not possible to compute the correlation with these data."),
  p5cor_debil = list(es = "débil", en = "weak"),
  p5cor_moderada = list(es = "moderada", en = "moderate"),
  p5cor_fuerte = list(es = "fuerte", en = "strong"),
  p5cor_pos = list(es = "positiva (cuando una variable sube, la otra tiende a subir)", en = "positive (as one variable increases, the other tends to increase)"),
  p5cor_neg = list(es = "negativa (cuando una variable sube, la otra tiende a bajar)", en = "negative (as one variable increases, the other tends to decrease)"),
  p5cor_normal_si = list(es = "datos aproximadamente normales", en = "approximately normal data"),
  p5cor_normal_no = list(es = "datos no normales, método no paramétrico", en = "non-normal data, non-parametric method"),
  p5cor_texto = list(
    es = "Se calculó la correlación de %s (%s).\n\nCoeficiente de correlación = %.3f\n%s\n\nLa relación es de intensidad %s y dirección %s.",
    en = "%s correlation was computed (%s).\n\nCorrelation coefficient = %.3f\n%s\n\nThe relationship has %s strength and a %s direction."),
  p5cor_titulo = list(es = "Correlación entre %s y %s", en = "Correlation between %s and %s"),
  # matriz de correlación
  p5m_min2 = list(es = "Elige al menos 2 variables numéricas.", en = "Choose at least 2 numeric variables."),
  p5m_insuf = list(es = "No hay suficientes filas con datos numéricos completos (mínimo 3).", en = "There are not enough rows with complete numeric data (minimum 3)."),
  p5m_nulo = list(es = "No fue posible calcular la matriz (revisa que las variables sean numéricas).", en = "The matrix could not be computed (check that the variables are numeric)."),
  p5m_aviso_col = list(es = "Aviso", en = "Notice"),
  p5m_titulo = list(es = "Matriz de correlación (%s)", en = "Correlation matrix (%s)"),
  # frecuencias
  p5f_categoria = list(es = "Categoría", en = "Category"),
  p5f_frecuencia = list(es = "Frecuencia", en = "Frequency"),
  p5f_porcentaje = list(es = "Porcentaje", en = "Percentage"),
  p5f_titulo = list(es = "Tabla de frecuencias: %s", en = "Frequency table: %s"),

  # contingencia
  p5ct_distintas = list(es = "Elige dos variables categóricas distintas.", en = "Choose two different categorical variables."),
  p5ct_fisher = list(es = "prueba exacta de Fisher (algunos conteos esperados eran pequeños)", en = "Fisher's exact test (some expected counts were small)"),
  p5ct_chi = list(es = "prueba de chi-cuadrada", en = "chi-square test"),
  p5ct_nulo = list(es = "No fue posible calcular la prueba con estos datos.", en = "It was not possible to run the test with these data."),
  p5ct_texto = list(
    es = "Asociación entre '%s' y '%s'.\nSe usó la %s.\n\n%s\n\nUn resultado significativo indica que las dos variables NO son independientes (están asociadas).",
    en = "Association between '%s' and '%s'.\nThe %s was used.\n\n%s\n\nA significant result indicates that the two variables are NOT independent (they are associated)."),
  p5ct_titulo = list(es = "Tabla de contingencia: %s vs. %s", en = "Contingency table: %s vs. %s"),
  # evolución temporal
  p5l_n = list(es = "n", en = "n"),
  p5l_promedio = list(es = "Promedio", en = "Mean"),
  p5l_ee = list(es = "Error_estandar", en = "Std_error"),
  p5l_titulo = list(es = "Evolución de %s a lo largo de %s, por %s", en = "Change of %s over %s, by %s"),
  # ANOVA dos vías
  p5a2_nulo = list(es = "No fue posible ajustar el modelo con estos datos.", en = "The model could not be fitted with these data."),
  p5a2_termino = list(es = "Termino", en = "Term"),
  p5a2_gl = list(es = "gl", en = "df"),
  p5a2_p = list(es = "valor_p", en = "p_value"),
  p5a2_int_si = list(es = "La interacción es significativa: el efecto de un factor DEPENDE del nivel del otro (ej. el tratamiento cambia la progresión en el tiempo).",
                      en = "The interaction is significant: the effect of one factor DEPENDS on the level of the other (e.g. the treatment changes the progression over time)."),
  p5a2_int_no = list(es = "La interacción no es significativa: los efectos de ambos factores son, en su mayoría, independientes entre sí.",
                      en = "The interaction is not significant: the effects of both factors are mostly independent of each other."),
  p5a2_texto = list(
    es = "ANOVA de dos vías: efecto de '%s', de '%s' y su interacción sobre '%s'.\n\n%s\nRevisa la tabla para el valor p de cada término (un valor p < 0.05 indica un efecto significativo).",
    en = "Two-way ANOVA: effect of '%s', of '%s' and their interaction on '%s'.\n\n%s\nCheck the table for the p-value of each term (a p-value < 0.05 indicates a significant effect)."),
  p5a2_titulo = list(es = "ANOVA de dos vías: %s ~ %s * %s", en = "Two-way ANOVA: %s ~ %s * %s"),
  # modelo mixto
  p5mx_nulo = list(es = "No fue posible ajustar el modelo mixto con estos datos. Verifica que cada sujeto tenga varias mediciones y que no falten combinaciones.",
                    en = "The mixed model could not be fitted with these data. Check that each subject has several measurements and that no combinations are missing."),
  p5mx_termino = list(es = "Termino", en = "Term"),
  p5mx_gl = list(es = "gl_num", en = "num_df"),
  p5mx_p = list(es = "valor_p", en = "p_value"),
  p5mx_texto = list(
    es = "Modelo mixto (medidas repetidas) para '%s', con '%s' como efecto aleatorio.\nEfectos fijos: '%s', '%s' y su interacción.\n\nRevisa la tabla: un valor p < 0.05 en un término indica un efecto significativo. Este análisis toma en cuenta que las mediciones del mismo sujeto están correlacionadas.",
    en = "Mixed model (repeated measures) for '%s', with '%s' as a random effect.\nFixed effects: '%s', '%s' and their interaction.\n\nCheck the table: a p-value < 0.05 for a term indicates a significant effect. This analysis accounts for the fact that measurements from the same subject are correlated."),
  p5mx_titulo = list(es = "Modelo mixto: %s ~ %s * %s (sujeto: %s)", en = "Mixed model: %s ~ %s * %s (subject: %s)"),
  # regresión lineal
  p5rl_min1 = list(es = "Elige al menos una variable predictora.", en = "Choose at least one predictor variable."),
  p5rl_nulo = list(es = "No fue posible ajustar la regresión con estos datos.", en = "The regression could not be fitted with these data."),
  p5rl_termino = list(es = "Termino", en = "Term"),
  p5rl_coef = list(es = "Coeficiente", en = "Coefficient"),
  p5rl_ee = list(es = "Error_estandar", en = "Std_error"),
  p5rl_p = list(es = "valor_p", en = "p_value"),
  p5rl_texto = list(
    es = "Regresión lineal para predecir '%s'.\nR\u00b2 = %s (el modelo explica el %s%% de la variación).\n\nCada coeficiente indica cuánto cambia la respuesta por cada unidad de esa variable (manteniendo las demás constantes). Un valor p < 0.05 indica un predictor significativo.",
    en = "Linear regression to predict '%s'.\nR\u00b2 = %s (the model explains %s%% of the variation).\n\nEach coefficient indicates how much the response changes per unit of that variable (holding the others constant). A p-value < 0.05 indicates a significant predictor."),
  p5rl_titulo = list(es = "Regresión lineal: %s ~ %s", en = "Linear regression: %s ~ %s"),
  # regresión logística
  p5rg_min1 = list(es = "Elige al menos una variable predictora.", en = "Choose at least one predictor variable."),
  p5rg_2cat = list(es = "El desenlace debe tener exactamente 2 categorías (tiene %d).", en = "The outcome must have exactly 2 categories (it has %d)."),
  p5rg_nulo = list(es = "No fue posible ajustar la regresión logística con estos datos.", en = "The logistic regression could not be fitted with these data."),
  p5rg_termino = list(es = "Termino", en = "Term"),
  p5rg_coef = list(es = "Coeficiente", en = "Coefficient"),
  p5rg_p = list(es = "valor_p", en = "p_value"),
  p5rg_texto = list(
    es = "Regresión logística para predecir '%s' (evento = '%s').\n\nLa columna OR es la razón de momios (odds ratio): un OR > 1 aumenta la probabilidad del evento, y un OR < 1 la disminuye. Un valor p < 0.05 indica un predictor significativo.",
    en = "Logistic regression to predict '%s' (event = '%s').\n\nThe OR column is the odds ratio: an OR > 1 increases the probability of the event, and an OR < 1 decreases it. A p-value < 0.05 indicates a significant predictor."),
  p5rg_titulo = list(es = "Regresión logística: %s ~ %s", en = "Logistic regression: %s ~ %s"),
  # normalidad
  p5n_pocos = list(es = "- %s: muy pocos datos (n = %d).", en = "- %s: too few data (n = %d)."),
  p5n_muchos = list(es = "- %s: demasiados datos para Shapiro-Wilk (n > 5000).", en = "- %s: too many data for Shapiro-Wilk (n > 5000)."),
  p5n_noeval = list(es = "- %s: no se pudo evaluar.", en = "- %s: could not be evaluated."),
  p5n_normal = list(es = "parece NORMAL (pruebas paramétricas: t, ANOVA, Pearson)", en = "appears NORMAL (parametric tests: t, ANOVA, Pearson)"),
  p5n_nonormal = list(es = "NO parece normal (pruebas no paramétricas: Wilcoxon, Kruskal-Wallis, Spearman)", en = "does NOT appear normal (non-parametric tests: Wilcoxon, Kruskal-Wallis, Spearman)"),
  p5n_linea = list(es = "- %s: p = %.4f -> %s", en = "- %s: p = %.4f -> %s"),
  p5n_grupo = list(es = "Grupo %s", en = "Group %s"),
  p5n_texto = list(
    es = "Prueba de normalidad (Shapiro-Wilk) para '%s':\n\n%s\n\nRegla: si p > 0.05, los datos son compatibles con una distribución normal.",
    en = "Normality test (Shapiro-Wilk) for '%s':\n\n%s\n\nRule: if p > 0.05, the data are compatible with a normal distribution."),
  p5n_titulo = list(es = "Prueba de normalidad (Shapiro-Wilk): %s", en = "Normality test (Shapiro-Wilk): %s"),

  # ---- Paso 6: Graficar ----
  p6_titulo = list(es = "¿Quieres graficar tus datos?", en = "Do you want to chart your data?"),
  p6_t_histograma = list(es = "Histograma (distribución de una variable)", en = "Histogram (distribution of a variable)"),
  p6_t_boxplot = list(es = "Diagrama de caja (comparar grupos)", en = "Box plot (compare groups)"),
  p6_t_violin = list(es = "Gráfica de violín (distribución por grupo)", en = "Violin plot (distribution by group)"),
  p6_t_dispersion = list(es = "Diagrama de dispersión (relación entre 2 variables)", en = "Scatter plot (relationship between 2 variables)"),
  p6_t_barras = list(es = "Gráfica de barras (frecuencias)", en = "Bar chart (frequencies)"),
  p6_t_barras_error = list(es = "Barras con error estándar (media \u00b1 EE por grupo)", en = "Bars with standard error (mean \u00b1 SE by group)"),
  p6_t_mapa = list(es = "Mapa de calor (promedio cruzando dos variables)", en = "Heatmap (mean across two variables)"),
  p6_t_linea = list(es = "Evolución en el tiempo (línea con promedio \u00b1 error estándar)", en = "Change over time (line with mean \u00b1 standard error)"),
  p6_t_spaghetti = list(es = "Líneas individuales por sujeto (spaghetti plot)", en = "Individual lines per subject (spaghetti plot)"),
  p6_t_superv = list(es = "Curva de supervivencia (Kaplan-Meier)", en = "Survival curve (Kaplan-Meier)"),
  p6_var_num = list(es = "Variable numérica:", en = "Numeric variable:"),
  p6_var_grupo = list(es = "Variable de grupo:", en = "Grouping variable:"),
  p6_violin_ayuda = list(es = "Muestra, además del promedio, la forma completa de la distribución en cada grupo (más informativo que el boxplot cuando hay pocos datos).",
                          en = "Shows, in addition to the mean, the full shape of the distribution in each group (more informative than the boxplot when there are few data)."),
  p6_disp_x = list(es = "Variable X:", en = "X variable:"),
  p6_disp_y = list(es = "Variable Y:", en = "Y variable:"),
  p6_disp_color = list(es = "Colorear por (opcional):", en = "Color by (optional):"),
  p6_ninguno = list(es = "(Ninguno)", en = "(None)"),
  p6_barras_var = list(es = "Variable categórica:", en = "Categorical variable:"),
  p6_barras_error_ayuda = list(es = "Formato clásico en artículos biomédicos: la altura de la barra es el promedio y la línea vertical es el error estándar.",
                                en = "Classic format in biomedical papers: the bar height is the mean and the vertical line is the standard error."),
  p6_heat_fila = list(es = "Variable para las filas (categórica):", en = "Variable for rows (categorical):"),
  p6_heat_col = list(es = "Variable para las columnas (ej. tiempo o grupo):", en = "Variable for columns (e.g. time or group):"),
  p6_heat_valor = list(es = "Variable numérica a promediar:", en = "Numeric variable to average:"),
  p6_heat_ayuda = list(es = "Ejemplo: filas = Grupo, columnas = Día, valor = Puntaje clínico. Cada celda muestra el promedio de esa combinación.",
                        en = "Example: rows = Group, columns = Day, value = Clinical score. Each cell shows the mean of that combination."),
  p6_lt_tiempo = list(es = "Variable de tiempo:", en = "Time variable:"),
  p6_lt_resp = list(es = "Variable de respuesta:", en = "Response variable:"),
  p6_sp_id = list(es = "Variable de identificación de sujeto (ej. Raton_ID):", en = "Subject identifier variable (e.g. Mouse_ID):"),
  p6_sp_grupo = list(es = "Colorear por grupo (opcional):", en = "Color by group (optional):"),
  p6_sp_ayuda = list(es = "Muestra la trayectoria individual de cada sujeto, útil para detectar variabilidad o valores atípicos que el promedio del grupo puede ocultar.",
                      en = "Shows each subject's individual trajectory, useful for spotting variability or outliers that the group mean can hide."),
  p6_km_id = list(es = "Variable de identificación de sujeto:", en = "Subject identifier variable:"),
  p6_km_tiempo = list(es = "Variable de tiempo hasta el evento:", en = "Time-to-event variable:"),
  p6_km_evento = list(es = "Variable de evento (1 = ocurrió, 0 = censurado):", en = "Event variable (1 = occurred, 0 = censored):"),
  p6_km_grupo = list(es = "Comparar por grupo (opcional):", en = "Compare by group (optional):"),
  p6_km_ayuda = list(es = "Si tu tabla tiene varias filas por sujeto (ej. una fila por día), se usará solo la primera fila de cada sujeto para este análisis.",
                      en = "If your table has several rows per subject (e.g. one row per day), only the first row of each subject will be used for this analysis."),
  p6_generar = list(es = "Generar gráfica", en = "Generate chart")
  ,
  # títulos y ejes de las gráficas
  g_hist_title = list(es = "Histograma de %s", en = "Histogram of %s"),
  g_frecuencia = list(es = "Frecuencia", en = "Frequency"),
  g_por = list(es = "%s por %s", en = "%s by %s"),
  g_vs = list(es = "%s vs. %s", en = "%s vs. %s"),
  g_bars_title = list(es = "Frecuencias de %s", en = "Frequencies of %s"),
  g_conteo = list(es = "Conteo", en = "Count"),
  g_barserr_title = list(es = "%s por %s (media \u00b1 EE)", en = "%s by %s (mean \u00b1 SE)"),
  g_heat_title = list(es = "Mapa de calor de %s", en = "Heatmap of %s"),
  g_promedio = list(es = "Promedio", en = "Mean"),
  g_line_title = list(es = "Evolución de %s en el tiempo", en = "Change of %s over time"),
  g_line_y = list(es = "%s (promedio \u00b1 EE)", en = "%s (mean \u00b1 SE)"),
  g_spag_title = list(es = "Trayectorias individuales de %s", en = "Individual trajectories of %s"),
  g_km_title = list(es = "Curva de supervivencia (Kaplan-Meier)", en = "Survival curve (Kaplan-Meier)"),
  g_km_y = list(es = "Probabilidad de no evento", en = "Event-free probability"),
  g_km_todos = list(es = "Todos", en = "All"),
  g_logrank = list(es = "Prueba de rangos logarítmicos (log-rank) entre grupos.\n",
                    en = "Log-rank test between groups.\n"),
  g_error = list(es = "No se pudo generar la gráfica:", en = "The chart could not be generated:"),
  g_error_tipo = list(es = "\u2014 revisa que las variables elegidas sean del tipo correcto (ej. numéricas).",
                       en = "\u2014 check that the chosen variables are of the correct type (e.g. numeric)."),

  # ---- Paso 7: Descargar ----
  p7_titulo = list(es = "Descarga tus resultados", en = "Download your results"),
  p7_intro = list(
    es = "Descarga tus datos (originales o limpios), un reporte completo en PDF, y una bitácora con un resumen de las acciones y análisis que realizaste en esta sesión.",
    en = "Download your data (original or cleaned), a full PDF report, and a log summarizing the actions and analyses you performed in this session."),
  p7_h_datos = list(es = "Datos", en = "Data"),
  p7_csv = list(es = "Descargar datos (.csv)", en = "Download data (.csv)"),
  p7_xlsx = list(es = "Descargar datos (.xlsx)", en = "Download data (.xlsx)"),
  p7_h_pdf = list(es = "Reporte en PDF", en = "PDF report"),
  p7_pdf_ayuda = list(
    es = "Incluye: resumen de los datos, el resultado más reciente de cada análisis que hayas ejecutado, las gráficas que hayas generado, y la bitácora.",
    en = "Includes: data summary, the most recent result of each analysis you ran, the charts you generated, and the log."),
  p7_responsable = list(es = "Nombre del responsable del análisis:", en = "Name of the person responsible for the analysis:"),
  p7_orientacion = list(es = "Orientación del reporte:", en = "Report orientation:"),
  p7_vertical = list(es = "Vertical", en = "Portrait"),
  p7_horizontal = list(es = "Horizontal", en = "Landscape"),
  p7_pdf_boton = list(es = "Descargar reporte completo (.pdf)", en = "Download full report (.pdf)"),
  p7_h_bitacora = list(es = "Bitácora de la sesión", en = "Session log"),
  p7_txt = list(es = "Descargar bitácora (.txt)", en = "Download log (.txt)"),
  p7_bitacora_pantalla = list(es = "Bitácora en pantalla", en = "Log on screen")
  ,
  # ---- Asistente inteligente (interfaz) ----
  ia_titulo = list(es = "\U0001F4A1 Asistente inteligente (opcional)", en = "\U0001F4A1 Smart assistant (optional)"),
  ia_ayuda = list(
    es = "Escribe para qué usarás tus datos y te sugiero qué hacer, según el tamaño de tu muestra y tu objetivo. Las recomendaciones se calculan en tu computadora; no se envían tus datos.",
    en = "Describe what you'll use your data for and I'll suggest what to do, based on your sample size and goal. Recommendations are computed on your computer; your data is not sent."),
  ia_placeholder = list(
    es = "Ej.: quiero comparar el puntaje clínico entre los grupos Control, AR y AR+Tratamiento a lo largo de los días.",
    en = "E.g.: I want to compare the clinical score between the Control, RA and RA+Treatment groups across days."),
  ia_checkbox = list(
    es = "Usar IA en línea (avanzada) — requiere una clave de API de Anthropic; opcional.",
    en = "Use online AI (advanced) — requires an Anthropic API key; optional."),
  ia_apikey_label = list(es = "Clave de API de Anthropic (sk-ant-...):", en = "Anthropic API key (sk-ant-...):"),
  ia_api_ayuda = list(
    es = "Tus datos de pacientes NO se envían; solo nombres de variables, tipos, conteos y tu objetivo.",
    en = "Your patient data is NOT sent; only variable names, types, counts and your goal."),
  ia_boton = list(es = "Obtener recomendaciones", en = "Get recommendations"),
  ia_claude_win = list(es = "Descargar Claude para Windows", en = "Download Claude for Windows"),
  ia_claude_mac = list(es = "Descargar Claude para macOS", en = "Download Claude for macOS"),
  ia_claude_linux = list(es = "Descargar Claude para Linux", en = "Download Claude for Linux"),
  ia_claude_otro = list(es = "Descargar Claude", en = "Download Claude"),
  ia_claude_ayuda = list(es = "Aplicación de escritorio de Claude (opcional).", en = "Claude desktop app (optional)."),
  ia_falta_datos = list(es = "Primero carga tus datos (paso 2) para poder recomendarte.", en = "Load your data first (step 2) so I can make recommendations."),
  ia_consultando = list(es = "Consultando la IA avanzada...", en = "Querying the advanced AI..."),
  ia_error = list(es = "Error al consultar la IA:", en = "Error querying the AI:"),
  ia_reco_api = list(es = "Recomendación (IA avanzada)", en = "Recommendation (advanced AI)"),
  ia_sec_limpieza = list(es = "Limpieza recomendada", en = "Recommended cleaning"),
  ia_sec_analisis = list(es = "Análisis recomendado", en = "Recommended analysis"),
  ia_sec_graficas = list(es = "Gráficas recomendadas", en = "Recommended charts")
  ,
  # ---- Reporte PDF / bitácora ----
  pdf_subtitulo = list(es = "Reporte de análisis de datos", en = "Data analysis report"),
  pdf_responsable = list(es = "Análisis efectuado por: %s", en = "Analysis performed by: %s"),
  pdf_generado = list(es = "Generado:", en = "Generated:"),
  pdf_datos = list(es = "Datos analizados: %d filas x %d columnas", en = "Data analyzed: %d rows x %d columns"),
  pdf_resumen = list(es = "Resumen de los datos", en = "Data summary"),
  pdf_bitacora = list(es = "Bitácora de la sesión", en = "Session log"),
  pdf_sin_titulo = list(es = "Sin resultados", en = "No results"),
  pdf_sin_l1 = list(es = "Aún no se ha ejecutado ningún análisis ni se ha generado ninguna gráfica.",
                     en = "No analysis has been run and no chart has been generated yet."),
  pdf_sin_l2 = list(es = "Regresa a los pasos 5 (Análisis) y 6 (Graficar) para generarlos antes de",
                     en = "Go back to steps 5 (Analysis) and 6 (Charts) to generate them before"),
  pdf_sin_l3 = list(es = "descargar el reporte completo.", en = "downloading the full report."),
  txt_encabezado = list(es = "AADATA - Bitacora de sesion de analisis de datos",
                         en = "AADATA - Data analysis session log"),
  txt_generado = list(es = "Generado:", en = "Generated:"),
  bit_vacia = list(es = "Aún no se han registrado acciones en esta sesión.",
                    en = "No actions have been recorded in this session yet.")
)

# Devuelve el texto traducido de una clave según el idioma ("es" o "en").
tr <- function(clave, idioma) {
  v <- TRAD[[clave]]
  if (is.null(v)) return(clave)
  val <- v[[idioma]]
  if (is.null(val)) v[["es"]] else val
}

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

  uiOutput("encabezado_app"),

  uiOutput("barra_progreso"),
  uiOutput("cuerpo_asistente"),

  uiOutput("pie_app")
)

## ---- 5. Lógica del servidor ------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    paso = 1,
    idioma = "es",         # idioma de la interfaz: "es" o "en"
    datos_crudos = NULL,
    datos_limpios = NULL,
    limpieza_aplicada = FALSE,
    ultima_grafica = NULL,
    bitacora = character(0),
    resultados = list(),   # guarda el resultado más reciente de cada tipo de análisis
    graficas = list(),     # guarda la gráfica más reciente de cada tipo generado
    reco = NULL            # recomendaciones del asistente inteligente
  )

  # Etiquetas legibles para cada tipo de gráfica (usadas en el reporte PDF)
  ETIQUETAS_GRAFICA_I18N <- list(
    es = c(
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
    ),
    en = c(
      histograma = "Histogram",
      boxplot = "Box plot",
      dispersion = "Scatter plot",
      barras = "Bar chart",
      linea_tiempo = "Change over time",
      mapa_calor = "Heatmap",
      violin = "Violin plot",
      barras_error = "Bars with standard error (mean \u00b1 SE)",
      spaghetti = "Individual lines per subject",
      supervivencia = "Survival curve (Kaplan-Meier)"
    )
  )
  etiqueta_grafica <- function(tipo, idi) {
    v <- ETIQUETAS_GRAFICA_I18N[[idi]]
    if (is.null(v)) v <- ETIQUETAS_GRAFICA_I18N$es
    et <- v[[tipo]]
    if (is.null(et)) tipo else et
  }

  agregar_bitacora <- function(texto) {
    rv$bitacora <- c(rv$bitacora,
                      paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M"), "] ", texto))
  }

  # Datos actuales a usar en análisis/gráficas: limpios si existen, si no crudos
  datos_actuales <- reactive({
    if (!is.null(rv$datos_limpios)) rv$datos_limpios else rv$datos_crudos
  })

  observeEvent(input$btn_restart, { session$reload() })

  # Cambio de idioma
  observeEvent(input$idioma_es, { rv$idioma <- "es" })
  observeEvent(input$idioma_en, { rv$idioma <- "en" })

  ## ---- Encabezado (con selector de idioma) ----
  output$encabezado_app <- renderUI({
    idi <- rv$idioma
    clase_es <- if (idi == "es") "btn-sm btn-light" else "btn-sm btn-outline-light"
    clase_en <- if (idi == "en") "btn-sm btn-light" else "btn-sm btn-outline-light"
    div(class = "titulo-app",
        h2("AaData"),
        p(tr("subtitulo", idi)),
        div(style = "float:right; margin-top:-46px;",
            span(style = "margin-right:10px;",
                 actionButton("idioma_es", "ES", class = clase_es),
                 actionButton("idioma_en", "EN", class = clase_en)),
            actionButton("btn_restart", tr("reiniciar", idi), class = "btn-sm btn-light"))
    )
  })

  ## ---- Pie de página ----
  output$pie_app <- renderUI({
    tags$footer(class = "pie", tr("pie", rv$idioma))
  })

  ## ---- Barra de progreso ----
  output$barra_progreso <- renderUI({
    nombres <- NOMBRES_PASOS_I18N[[rv$idioma]]
    items <- lapply(seq_along(nombres), function(i) {
      clase <- if (i == rv$paso) "paso-item paso-activo"
               else if (i < rv$paso) "paso-item paso-completo"
               else "paso-item"
      div(class = clase, paste0(i, ". ", nombres[i]))
    })
    div(class = "paso-barra", items)
  })

  ## ---- Navegación genérica ----
  ir_siguiente <- function() { rv$paso <- min(TOTAL_PASOS, rv$paso + 1) }
  ir_atras <- function() { rv$paso <- max(1, rv$paso - 1) }

  botones_nav <- function(mostrar_siguiente = TRUE, id_siguiente = "btn_siguiente",
                           texto_siguiente = NULL) {
    idi <- rv$idioma
    txt_sig <- if (is.null(texto_siguiente)) tr("siguiente", idi) else texto_siguiente
    div(style = "margin-top: 20px; display:flex; justify-content: space-between;",
        if (rv$paso > 1) actionButton("btn_atras", tr("atras", idi), class = "btn-nav btn-default")
        else div(),
        if (mostrar_siguiente) actionButton(id_siguiente, txt_sig, class = "btn-nav btn-primary")
        else div()
    )
  }

  observeEvent(input$btn_atras, { ir_atras() })
  observeEvent(input$btn_siguiente, { ir_siguiente() })

  # Caja del Asistente inteligente, reutilizable en los pasos 4, 5 y 6.
  # 'foco' es "limpieza", "analisis" o "graficas": define qué recomendación resalta.
  caja_asistente <- function(foco) {
    idi <- rv$idioma
    salida_id <- paste0("ia_salida_", foco)
    div(style = "background:#fbf6f8; border:1px solid #eadfe4; border-radius:8px; padding:16px; margin-bottom:18px;",
      tags$div(style = "font-weight:700; color:#7a1f3d; margin-bottom:6px;",
               tr("ia_titulo", idi)),
      p(class = "ayuda", style = "margin-top:0;", tr("ia_ayuda", idi)),
      textAreaInput("ia_proposito", NULL, width = "100%", height = "70px",
                    placeholder = tr("ia_placeholder", idi)),
      checkboxInput("ia_usar_api", tr("ia_checkbox", idi), value = FALSE),
      conditionalPanel(
        condition = "input.ia_usar_api == true",
        passwordInput("ia_api_key", tr("ia_apikey_label", idi), width = "100%"),
        p(class = "ayuda", tr("ia_api_ayuda", idi))
      ),
      uiOutput("boton_descargar_claude"),
      actionButton("ia_recomendar", tr("ia_boton", idi), class = "btn-primary"),
      uiOutput(salida_id)
    )
  }

  # Botón para descargar la app de Claude (la página detecta el sistema operativo).
  output$boton_descargar_claude <- renderUI({
    idi <- rv$idioma
    so <- Sys.info()[["sysname"]]
    etiqueta <- if (so == "Windows") tr("ia_claude_win", idi)
                else if (so == "Darwin") tr("ia_claude_mac", idi)
                else if (so == "Linux") tr("ia_claude_linux", idi)
                else tr("ia_claude_otro", idi)
    div(style = "margin: 4px 0 10px 0;",
      tags$a(etiqueta, href = "https://claude.com/download", target = "_blank",
             class = "btn btn-default btn-sm"),
      span(class = "ayuda", style = "margin-left:8px;", tr("ia_claude_ayuda", idi))
    )
  })

  # Observador: genera las recomendaciones (local o vía API) al presionar el botón.
  observeEvent(input$ia_recomendar, {
    df <- datos_actuales()
    if (is.null(df)) {
      showNotification(tr("ia_falta_datos", rv$idioma), type = "warning", duration = 6); return()
    }
    proposito <- if (is.null(input$ia_proposito)) "" else input$ia_proposito

    if (isTRUE(input$ia_usar_api) && nzchar(trimws(if (is.null(input$ia_api_key)) "" else input$ia_api_key))) {
      showNotification(tr("ia_consultando", rv$idioma), type = "message", duration = 3)
      prompt <- construir_resumen_ia(df, proposito)
      texto <- tryCatch(llamar_anthropic(prompt, input$ia_api_key),
                        error = function(e) paste(tr("ia_error", rv$idioma), conditionMessage(e)))
      rv$reco <- list(modo = "api", texto = texto)
    } else {
      rv$reco <- c(list(modo = "local"), recomendar_local(df, proposito, rv$idioma))
    }
    agregar_bitacora(sprintf("Asistente inteligente consultado (%s).",
                              if (isTRUE(input$ia_usar_api)) "IA avanzada" else "local"))
  })

  # Render de la salida del asistente, enfocada según el paso.
  render_reco <- function(foco) {
    idi <- rv$idioma
    reco <- rv$reco
    if (is.null(reco)) return(NULL)
    contenedor <- function(...) div(style = "margin-top:14px; background:white; border-radius:6px; padding:14px; border:1px solid #eee;", ...)

    if (identical(reco$modo, "api")) {
      return(contenedor(
        tags$div(style = "font-weight:600; color:#7a1f3d; margin-bottom:6px;", tr("ia_reco_api", idi)),
        tags$div(style = "white-space:pre-wrap; font-size:14px;", reco$texto)
      ))
    }
    # modo local
    secciones <- list(
      limpieza = list(titulo = tr("ia_sec_limpieza", idi), items = reco$limpieza),
      analisis = list(titulo = tr("ia_sec_analisis", idi), items = reco$analisis),
      graficas = list(titulo = tr("ia_sec_graficas", idi), items = reco$graficas)
    )
    orden <- unique(c(foco, c("limpieza", "analisis", "graficas")))
    bloques <- lapply(orden, function(k) {
      s <- secciones[[k]]
      resaltar <- identical(k, foco)
      div(style = sprintf("margin-bottom:10px; %s", if (resaltar) "" else "opacity:0.75;"),
          tags$div(style = "font-weight:600; color:#7a1f3d;", s$titulo),
          tags$ul(lapply(s$items, function(x) tags$li(x))))
    })
    contenedor(
      tags$div(style = "font-size:13px; color:#555; margin-bottom:8px;", reco$encabezado),
      bloques
    )
  }

  output$ia_salida_limpieza <- renderUI({ render_reco("limpieza") })
  output$ia_salida_analisis <- renderUI({ render_reco("analisis") })
  output$ia_salida_graficas <- renderUI({ render_reco("graficas") })

  ## =====================================================================
  ## CUERPO PRINCIPAL: se redibuja según el paso actual
  ## =====================================================================
  output$cuerpo_asistente <- renderUI({

    if (rv$paso == 1) {
      ## ---------------- PASO 1: BIENVENIDA ----------------
      idi <- rv$idioma
      div(class = "caja",
        h3(tr("bienvenida_titulo", idi)),
        p(tr("bienvenida_intro", idi)),
        tags$ul(lapply(TRAD$paso1_item[[idi]], tags$li)),
        p(class = "ayuda", tr("puedes_atras", idi)),
        botones_nav(texto_siguiente = tr("comenzar", idi))
      )

    } else if (rv$paso == 2) {
      ## ---------------- PASO 2: CARGAR DATOS ----------------
      idi <- rv$idioma
      div(class = "caja",
        h3(tr("p2_titulo", idi)),
        p(tr("p2_intro", idi)),

        radioButtons("tipo_carga", NULL,
                     choices = stats::setNames(
                       c("propio", "avanzado", "ejemplo"),
                       c(tr("p2_opt_propio", idi), tr("p2_opt_avanzado", idi), tr("p2_opt_ejemplo", idi))),
                     selected = "propio"),

        conditionalPanel(
          condition = "input.tipo_carga == 'propio'",
          fileInput("archivo", tr("p2_file_label", idi),
                    accept = c(".csv", ".xlsx", ".xls")),
          conditionalPanel(
            condition = "input.archivo != null",
            fluidRow(
              column(4, radioButtons("csv_sep", tr("p2_csv_sep", idi),
                                      choices = stats::setNames(
                                        c(",", ";", "\t"),
                                        c(tr("p2_sep_coma", idi), tr("p2_sep_puntocoma", idi), tr("p2_sep_tab", idi))),
                                      selected = ",")),
              column(4, radioButtons("csv_dec", tr("p2_csv_dec", idi),
                                      choices = stats::setNames(
                                        c(".", ","),
                                        c(tr("p2_dec_punto", idi), tr("p2_dec_coma", idi))),
                                      selected = ".")),
              column(4, radioButtons("csv_enc", tr("p2_csv_enc", idi),
                                      choices = stats::setNames(
                                        c("UTF-8", "Latin1"),
                                        c("UTF-8", tr("p2_enc_latin", idi))),
                                      selected = "UTF-8"))
            )
          ),
          p(class = "ayuda", tr("p2_ayuda_csv", idi))
        ),

        conditionalPanel(
          condition = "input.tipo_carga == 'avanzado'",
          p(HTML(tr("p2_av_intro", idi))),
          fileInput("archivo_av", tr("p2_av_file", idi),
                    accept = c(".xlsx", ".xls")),
          uiOutput("panel_avanzado")
        ),

        conditionalPanel(
          condition = "input.tipo_carga == 'ejemplo'",
          p(tr("p2_ej_intro", idi)),
          actionButton("cargar_ejemplo", tr("p2_ej_boton", idi), class = "btn-info")
        ),

        uiOutput("mensaje_carga"),
        div(style = "margin-top: 20px; display:flex; justify-content: space-between;",
            actionButton("btn_atras", tr("atras", idi), class = "btn-nav btn-default"),
            actionButton("btn_next_paso2", tr("siguiente", idi), class = "btn-nav btn-primary"))
      )

    } else if (rv$paso == 3) {
      ## ---------------- PASO 3: VISTA PREVIA ----------------
      req(rv$datos_crudos)
      idi <- rv$idioma
      df <- rv$datos_crudos
      div(class = "caja",
        h3(tr("p3_titulo", idi)),
        p(sprintf(tr("p3_dimensiones", idi), nrow(df), ncol(df))),
        DTOutput("tabla_preview"),
        h4(tr("p3_resumen", idi)),
        verbatimTextOutput("resumen_preview"),
        botones_nav()
      )

    } else if (rv$paso == 4) {
      ## ---------------- PASO 4: LIMPIEZA ----------------
      req(rv$datos_crudos)
      idi <- rv$idioma
      df <- rv$datos_crudos
      vars_num <- obtener_vars_numericas(df)
      div(class = "caja",
        h3(tr("p4_titulo", idi)),
        caja_asistente("limpieza"),
        p(tr("p4_intro", idi)),

        checkboxGroupInput("opciones_limpieza", NULL,
          choices = stats::setNames(
            c("quitar_na", "quitar_col_vacia", "quitar_duplicados", "trim_texto",
              "estandarizar_texto", "texto_a_numero", "redondear", "filtrar_rango",
              "detectar_outliers", "limpiar_nombres"),
            c(tr("p4_op_quitar_na", idi), tr("p4_op_col_vacia", idi), tr("p4_op_duplicados", idi),
              tr("p4_op_trim", idi), tr("p4_op_estandarizar", idi), tr("p4_op_texto_num", idi),
              tr("p4_op_redondear", idi), tr("p4_op_rango", idi), tr("p4_op_outliers", idi),
              tr("p4_op_nombres", idi)))),

        # --- Parámetros para 'redondear' ---
        conditionalPanel(
          condition = "input.opciones_limpieza && input.opciones_limpieza.indexOf('redondear') > -1",
          div(style = "margin-left: 25px; margin-bottom: 10px;",
            numericInput("redondear_decimales", tr("p4_decimales", idi), value = 2, min = 0, max = 6, step = 1)
          )
        ),

        # --- Parámetros para 'filtrar_rango' ---
        conditionalPanel(
          condition = "input.opciones_limpieza && input.opciones_limpieza.indexOf('filtrar_rango') > -1",
          div(style = "margin-left: 25px; margin-bottom: 10px; padding: 10px; background-color: #f6f6f9; border-radius: 6px;",
            p(class = "ayuda", tr("p4_rango_ayuda", idi)),
            selectInput("rango_col", tr("p4_rango_col", idi), choices = vars_num),
            fluidRow(
              column(6, numericInput("rango_min", tr("p4_rango_min", idi), value = 0)),
              column(6, numericInput("rango_max", tr("p4_rango_max", idi), value = 100))
            )
          )
        ),

        # --- Parámetros para 'detectar_outliers' ---
        conditionalPanel(
          condition = "input.opciones_limpieza && input.opciones_limpieza.indexOf('detectar_outliers') > -1",
          div(style = "margin-left: 25px; margin-bottom: 10px; padding: 10px; background-color: #f6f6f9; border-radius: 6px;",
            p(class = "ayuda", tr("p4_outlier_ayuda", idi)),
            radioButtons("outlier_accion", tr("p4_outlier_pregunta", idi),
              choices = stats::setNames(c("marcar", "eliminar"),
                                        c(tr("p4_outlier_marcar", idi), tr("p4_outlier_eliminar", idi))),
              selected = "marcar")
          )
        ),

        actionButton("aplicar_limpieza", tr("p4_aplicar", idi), class = "btn-info"),
        actionButton("sin_limpieza", tr("p4_sin", idi), class = "btn-default"),
        uiOutput("resultado_limpieza"),
        botones_nav()
      )

    } else if (rv$paso == 5) {
      ## ---------------- PASO 5: ANÁLISIS ----------------
      req(datos_actuales())
      idi <- rv$idioma
      df <- datos_actuales()
      vars_num <- obtener_vars_numericas(df)
      vars_cat <- obtener_vars_categoricas(df)
      vars_todas <- obtener_vars_todas(df)

      div(class = "caja",
        h3(tr("p5_titulo", idi)),
        caja_asistente("analisis"),
        radioButtons("tipo_analisis", NULL,
          choices = stats::setNames(
            c("descriptivo", "comparar", "pareada", "cohen", "correlacion", "matriz_corr",
              "frecuencias", "contingencia", "longitudinal", "anova2", "mixto",
              "reg_lineal", "reg_logistica", "normalidad"),
            c(tr("p5_t_descriptivo", idi), tr("p5_t_comparar", idi), tr("p5_t_pareada", idi),
              tr("p5_t_cohen", idi), tr("p5_t_correlacion", idi), tr("p5_t_matriz", idi),
              tr("p5_t_frecuencias", idi), tr("p5_t_contingencia", idi), tr("p5_t_longitudinal", idi),
              tr("p5_t_anova2", idi), tr("p5_t_mixto", idi), tr("p5_t_reg_lineal", idi),
              tr("p5_t_reg_logistica", idi), tr("p5_t_normalidad", idi)))),
        hr(),

        conditionalPanel(condition = "input.tipo_analisis == 'descriptivo'",
          selectInput("desc_var", tr("p5_desc_var", idi), choices = vars_num),
          selectizeInput("desc_var_grupo", tr("p5_desc_grupo", idi),
                         choices = c(stats::setNames("", tr("p5_sin_agrupar", idi)), vars_cat)),
          actionButton("run_desc", tr("p5_calcular", idi), class = "btn-primary"),
          tableOutput("out_desc")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'comparar'",
          selectInput("comp_grupo", tr("p5_comp_grupo", idi), choices = vars_cat),
          selectInput("comp_num", tr("p5_comp_num", idi), choices = vars_num),
          actionButton("run_comp", tr("p5_comp_boton", idi), class = "btn-primary"),
          verbatimTextOutput("out_comp"),
          tableOutput("out_comp_tabla")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'pareada'",
          p(class = "ayuda", tr("p5_par_ayuda", idi)),
          selectInput("par_id", tr("p5_par_id", idi), choices = vars_todas),
          selectInput("par_cond", tr("p5_par_cond", idi), choices = vars_todas),
          fluidRow(
            column(6, selectInput("par_m1", tr("p5_par_m1", idi), choices = NULL)),
            column(6, selectInput("par_m2", tr("p5_par_m2", idi), choices = NULL))
          ),
          selectInput("par_resp", tr("p5_par_resp", idi), choices = vars_num),
          actionButton("run_par", tr("p5_par_boton", idi), class = "btn-primary"),
          verbatimTextOutput("out_par")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'cohen'",
          p(class = "ayuda", tr("p5_cohen_ayuda", idi)),
          selectInput("cohen_grupo", tr("p5_cohen_grupo", idi), choices = vars_cat),
          selectInput("cohen_num", tr("p5_cohen_num", idi), choices = vars_num),
          actionButton("run_cohen", tr("p5_cohen_boton", idi), class = "btn-primary"),
          verbatimTextOutput("out_cohen")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'correlacion'",
          selectInput("corr_var1", tr("p5_corr_var1", idi), choices = vars_num),
          selectInput("corr_var2", tr("p5_corr_var2", idi), choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          actionButton("run_corr", tr("p5_corr_boton", idi), class = "btn-primary"),
          verbatimTextOutput("out_corr")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'matriz_corr'",
          p(class = "ayuda", tr("p5_mcorr_ayuda", idi)),
          selectizeInput("mcorr_vars", tr("p5_mcorr_vars", idi), choices = vars_num, multiple = TRUE,
                         selected = if (length(vars_num) >= 2) vars_num[1:2] else vars_num),
          radioButtons("mcorr_metodo", tr("p5_metodo", idi),
            choices = stats::setNames(c("pearson", "spearman"),
                                      c(tr("p5_pearson", idi), tr("p5_spearman", idi))), selected = "pearson"),
          actionButton("run_mcorr", tr("p5_mcorr_boton", idi), class = "btn-primary"),
          tableOutput("out_mcorr")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'frecuencias'",
          selectInput("freq_var", tr("p5_freq_var", idi), choices = vars_cat),
          actionButton("run_freq", tr("p5_freq_boton", idi), class = "btn-primary"),
          tableOutput("out_freq")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'contingencia'",
          p(class = "ayuda", tr("p5_cont_ayuda", idi)),
          selectInput("cont_var1", tr("p5_cont_var1", idi), choices = vars_cat),
          selectInput("cont_var2", tr("p5_cont_var2", idi), choices = vars_cat,
                     selected = if (length(vars_cat) > 1) vars_cat[2] else vars_cat[1]),
          actionButton("run_cont", tr("p5_cont_boton", idi), class = "btn-primary"),
          verbatimTextOutput("out_cont"),
          tableOutput("out_cont_tabla")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'longitudinal'",
          selectInput("long_tiempo", tr("p5_long_tiempo", idi), choices = vars_num),
          selectInput("long_grupo", tr("p5_long_grupo", idi), choices = vars_cat),
          selectInput("long_resp", tr("p5_long_resp", idi), choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          actionButton("run_long", tr("p5_long_boton", idi), class = "btn-primary"),
          tableOutput("out_long"),
          p(class = "ayuda", tr("p5_long_consejo", idi))
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'anova2'",
          p(class = "ayuda", tr("p5_a2_ayuda", idi)),
          selectInput("a2_factor1", tr("p5_a2_f1", idi), choices = vars_cat),
          selectInput("a2_factor2", tr("p5_a2_f2", idi), choices = vars_todas,
                     selected = if (length(vars_cat) > 1) vars_cat[2] else vars_cat[1]),
          selectInput("a2_resp", tr("p5_a2_resp", idi), choices = vars_num),
          actionButton("run_a2", tr("p5_a2_boton", idi), class = "btn-primary"),
          verbatimTextOutput("out_a2"),
          tableOutput("out_a2_tabla")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'mixto'",
          p(class = "ayuda", tr("p5_mix_ayuda", idi)),
          selectInput("mix_id", tr("p5_mix_id", idi), choices = vars_todas),
          selectInput("mix_grupo", tr("p5_mix_grupo", idi), choices = vars_cat),
          selectInput("mix_tiempo", tr("p5_mix_tiempo", idi), choices = vars_todas,
                     selected = if (length(vars_num) > 0) vars_num[1] else vars_todas[1]),
          selectInput("mix_resp", tr("p5_mix_resp", idi), choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          actionButton("run_mix", tr("p5_mix_boton", idi), class = "btn-primary"),
          verbatimTextOutput("out_mix"),
          tableOutput("out_mix_tabla")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'reg_lineal'",
          p(class = "ayuda", tr("p5_rl_ayuda", idi)),
          selectInput("rl_resp", tr("p5_rl_resp", idi), choices = vars_num),
          selectizeInput("rl_pred", tr("p5_rl_pred", idi), choices = vars_todas, multiple = TRUE),
          actionButton("run_rl", tr("p5_rl_boton", idi), class = "btn-primary"),
          verbatimTextOutput("out_rl"),
          tableOutput("out_rl_tabla")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'reg_logistica'",
          p(class = "ayuda", tr("p5_rlog_ayuda", idi)),
          selectInput("rlog_resp", tr("p5_rlog_resp", idi), choices = vars_todas),
          selectizeInput("rlog_pred", tr("p5_rl_pred", idi), choices = vars_todas, multiple = TRUE),
          actionButton("run_rlog", tr("p5_rlog_boton", idi), class = "btn-primary"),
          verbatimTextOutput("out_rlog"),
          tableOutput("out_rlog_tabla")
        ),

        conditionalPanel(condition = "input.tipo_analisis == 'normalidad'",
          p(class = "ayuda", tr("p5_norm_ayuda", idi)),
          selectInput("norm_var", tr("p5_norm_var", idi), choices = vars_num),
          selectizeInput("norm_grupo", tr("p5_norm_grupo", idi),
                         choices = c(stats::setNames("", tr("p5_todo_junto", idi)), vars_cat)),
          actionButton("run_norm", tr("p5_norm_boton", idi), class = "btn-primary"),
          verbatimTextOutput("out_norm")
        ),

        botones_nav()
      )

    } else if (rv$paso == 6) {
      ## ---------------- PASO 6: GRAFICAR ----------------
      req(datos_actuales())
      idi <- rv$idioma
      df <- datos_actuales()
      vars_num <- obtener_vars_numericas(df)
      vars_cat <- obtener_vars_categoricas(df)
      vars_todas <- obtener_vars_todas(df)

      div(class = "caja",
        h3(tr("p6_titulo", idi)),
        caja_asistente("graficas"),
        radioButtons("tipo_grafica", NULL,
          choices = stats::setNames(
            c("histograma", "boxplot", "violin", "dispersion", "barras", "barras_error",
              "mapa_calor", "linea_tiempo", "spaghetti", "supervivencia"),
            c(tr("p6_t_histograma", idi), tr("p6_t_boxplot", idi), tr("p6_t_violin", idi),
              tr("p6_t_dispersion", idi), tr("p6_t_barras", idi), tr("p6_t_barras_error", idi),
              tr("p6_t_mapa", idi), tr("p6_t_linea", idi), tr("p6_t_spaghetti", idi),
              tr("p6_t_superv", idi)))),
        hr(),

        conditionalPanel(condition = "input.tipo_grafica == 'histograma'",
          selectInput("hist_var", tr("p6_var_num", idi), choices = vars_num)
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'boxplot'",
          selectInput("box_grupo", tr("p6_var_grupo", idi), choices = vars_cat),
          selectInput("box_num", tr("p6_var_num", idi), choices = vars_num)
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'violin'",
          selectInput("violin_grupo", tr("p6_var_grupo", idi), choices = vars_cat),
          selectInput("violin_num", tr("p6_var_num", idi), choices = vars_num),
          p(class = "ayuda", tr("p6_violin_ayuda", idi))
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'dispersion'",
          selectInput("disp_x", tr("p6_disp_x", idi), choices = vars_num),
          selectInput("disp_y", tr("p6_disp_y", idi), choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          selectizeInput("disp_color", tr("p6_disp_color", idi),
                         choices = c(stats::setNames("", tr("p6_ninguno", idi)), vars_cat))
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'barras'",
          selectInput("barras_var", tr("p6_barras_var", idi), choices = vars_cat)
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'barras_error'",
          selectInput("barras_error_grupo", tr("p6_var_grupo", idi), choices = vars_cat),
          selectInput("barras_error_num", tr("p6_var_num", idi), choices = vars_num),
          p(class = "ayuda", tr("p6_barras_error_ayuda", idi))
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'mapa_calor'",
          selectInput("heat_fila", tr("p6_heat_fila", idi), choices = vars_cat),
          selectInput("heat_columna", tr("p6_heat_col", idi), choices = vars_cat,
                     selected = if (length(vars_cat) > 1) vars_cat[2] else vars_cat[1]),
          selectInput("heat_valor", tr("p6_heat_valor", idi), choices = vars_num),
          p(class = "ayuda", tr("p6_heat_ayuda", idi))
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'linea_tiempo'",
          selectInput("lt_tiempo", tr("p6_lt_tiempo", idi), choices = vars_num),
          selectInput("lt_grupo", tr("p6_var_grupo", idi), choices = vars_cat),
          selectInput("lt_resp", tr("p6_lt_resp", idi), choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1])
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'spaghetti'",
          selectInput("sp_id", tr("p6_sp_id", idi), choices = vars_todas),
          selectInput("sp_tiempo", tr("p6_lt_tiempo", idi), choices = vars_num),
          selectInput("sp_resp", tr("p6_lt_resp", idi), choices = vars_num,
                     selected = if (length(vars_num) > 1) vars_num[2] else vars_num[1]),
          selectizeInput("sp_grupo", tr("p6_sp_grupo", idi),
                         choices = c(stats::setNames("", tr("p6_ninguno", idi)), vars_cat)),
          p(class = "ayuda", tr("p6_sp_ayuda", idi))
        ),
        conditionalPanel(condition = "input.tipo_grafica == 'supervivencia'",
          selectInput("km_id", tr("p6_km_id", idi), choices = vars_todas),
          selectInput("km_tiempo", tr("p6_km_tiempo", idi), choices = vars_num),
          selectInput("km_evento", tr("p6_km_evento", idi), choices = vars_num),
          selectizeInput("km_grupo", tr("p6_km_grupo", idi),
                         choices = c(stats::setNames("", tr("p6_ninguno", idi)), vars_cat)),
          p(class = "ayuda", tr("p6_km_ayuda", idi))
        ),

        actionButton("generar_grafica", tr("p6_generar", idi), class = "btn-primary"),
        br(), br(),
        plotOutput("grafica_principal", height = "420px"),
        uiOutput("texto_resultado_grafica"),
        uiOutput("boton_descargar_grafica"),
        botones_nav()
      )

    } else if (rv$paso == 7) {
      ## ---------------- PASO 7: DESCARGAR ----------------
      idi <- rv$idioma
      div(class = "caja",
        h3(tr("p7_titulo", idi)),
        p(tr("p7_intro", idi)),
        fluidRow(
          column(4,
            h4(tr("p7_h_datos", idi)),
            downloadButton("descargar_csv", tr("p7_csv", idi), class = "btn-info"),
            br(), br(),
            downloadButton("descargar_xlsx", tr("p7_xlsx", idi), class = "btn-info")
          ),
          column(4,
            h4(tr("p7_h_pdf", idi)),
            p(class = "ayuda", tr("p7_pdf_ayuda", idi)),
            textInput("responsable_analisis", tr("p7_responsable", idi),
                      placeholder = "Ej. Daniel Bonifaz-Calvo"),
            radioButtons("orientacion_pdf", tr("p7_orientacion", idi),
                         choices = stats::setNames(c("vertical", "horizontal"),
                                                   c(tr("p7_vertical", idi), tr("p7_horizontal", idi))),
                         selected = "vertical", inline = TRUE),
            downloadButton("descargar_pdf", tr("p7_pdf_boton", idi), class = "btn-danger")
          ),
          column(4,
            h4(tr("p7_h_bitacora", idi)),
            downloadButton("descargar_reporte", tr("p7_txt", idi), class = "btn-info")
          )
        ),
        hr(),
        h4(tr("p7_bitacora_pantalla", idi)),
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
          sprintf(tr("carga_ok", rv$idioma),
                  nrow(rv$datos_crudos), ncol(rv$datos_crudos)))
    } else {
      div(style = "color: #888; margin-top: 10px;", tr("carga_none", rv$idioma))
    }
  })

  output$boton_siguiente_carga <- renderUI({ NULL })

  observeEvent(input$btn_next_paso2, {
    if (is.null(rv$datos_crudos)) {
      showNotification(tr("falta_datos", rv$idioma), type = "warning", duration = 6)
    } else {
      ir_siguiente()
    }
  })

  observeEvent(input$btn_siguiente_desde_carga, { ir_siguiente() })

  observeEvent(input$archivo, {
    req(input$archivo)
    ext <- tolower(tools::file_ext(input$archivo$name))
    origen <- input$archivo$datapath
    nombre <- input$archivo$name

    leer_csv <- function(path) {
      sep <- if (is.null(input$csv_sep)) "," else input$csv_sep
      dec <- if (is.null(input$csv_dec)) "." else input$csv_dec
      enc <- if (is.null(input$csv_enc)) "UTF-8" else input$csv_enc
      # "UTF-8-BOM" quita automáticamente el BOM que suele poner Excel.
      fenc <- if (enc == "UTF-8") "UTF-8-BOM" else enc

      intento <- function(fe, s) tryCatch(
        read.csv(path, sep = s, dec = dec, fileEncoding = fe, stringsAsFactors = FALSE,
                 check.names = FALSE, na.strings = c("NA", "", "NaN", "na", "N/A")),
        error = function(e) NULL)

      # 1) Con la configuración elegida por el usuario.
      d <- intento(fenc, sep)
      # 2) Si falla o sale en una sola columna, probar la otra codificación.
      if (is.null(d) || ncol(d) <= 1) {
        otro <- if (enc == "UTF-8") "latin1" else "UTF-8-BOM"
        d2 <- intento(otro, sep)
        if (!is.null(d2) && (is.null(d) || ncol(d2) > ncol(d))) d <- d2
      }
      # 3) Si sigue en una sola columna, autodetectar el separador.
      if (is.null(d) || ncol(d) <= 1) {
        d3 <- leer_texto_auto(path)
        if (!is.null(d3) && (is.null(d) || ncol(d3) > ncol(d))) d <- d3
      }
      if (is.null(d)) stop("No se pudo leer el archivo CSV con la configuración indicada; prueba a cambiar el separador o la codificación.")
      d
    }
    leer_excel <- function() {
      as.data.frame(readxl::read_excel(preparar_ruta_excel(origen, nombre)))
    }

    # Intenta leer un archivo con extensión de Excel probando, en orden:
    # Excel real -> tabla HTML -> texto delimitado. Devuelve data.frame o error.
    leer_excel_o_alternativas <- function() {
      df1 <- tryCatch(leer_excel(), error = function(e) NULL)
      if (!is.null(df1) && ncol(df1) > 0) return(df1)
      if (parece_html(origen)) {
        df2 <- leer_html_tabla(origen)
        if (!is.null(df2) && ncol(df2) > 0) {
          showNotification(tr("notif_html", rv$idioma), type = "message", duration = 6)
          return(df2)
        }
      }
      df3 <- leer_texto_auto(origen)
      if (!is.null(df3) && ncol(df3) > 1) {
        showNotification(tr("notif_texto", rv$idioma), type = "message", duration = 6)
        return(df3)
      }
      stop("no-excel")  # activa el mensaje amigable
    }

    df <- tryCatch({
      if (ext == "csv") {
        leer_csv(origen)
      } else if (ext %in% c("xlsx", "xls")) {
        convertir_columnas_numericas(leer_excel_o_alternativas())
      } else {
        sig <- tryCatch(readBin(origen, what = "raw", n = 2), error = function(e) raw(0))
        es_zip_xlsx <- length(sig) >= 2 && sig[1] == as.raw(0x50) && sig[2] == as.raw(0x4b) # "PK"
        if (es_zip_xlsx) convertir_columnas_numericas(leer_excel_o_alternativas())
        else convertir_columnas_numericas(leer_csv(origen))
      }
    }, error = function(e) {
      showNotification(paste(tr("notif_error_lectura", rv$idioma), mensaje_error_lectura(e$message)),
                        type = "error", duration = 11)
      NULL
    })

    if (!is.null(df) && ncol(df) > 0) {
      rv$datos_crudos <- df
      rv$datos_limpios <- NULL
      agregar_bitacora(sprintf("Se cargó el archivo '%s' (%d filas, %d columnas).",
                                input$archivo$name, nrow(df), ncol(df)))
    } else if (!is.null(df) && ncol(df) == 0) {
      showNotification(tr("notif_no_columnas", rv$idioma), type = "warning", duration = 8)
    }
  })

  observeEvent(input$cargar_ejemplo, {
    rv$datos_crudos <- generar_datos_ejemplo()
    rv$datos_limpios <- NULL
    agregar_bitacora("Se cargaron los datos de ejemplo (estudio de artritis reumatoide en ratones).")
    showNotification(tr("notif_ejemplo", rv$idioma), type = "message", duration = 4)
  })

  ## ---- Importación avanzada (Excel con encabezados en varias filas) ----

  # Genera todo el panel avanzado en el servidor una vez que hay archivo subido.
  # (Se hace desde el servidor para no depender de condiciones del lado del cliente.)
  output$panel_avanzado <- renderUI({
    req(input$archivo_av)
    idi <- rv$idioma
    ruta_av <- preparar_ruta_excel(input$archivo_av$datapath, input$archivo_av$name)
    hojas <- tryCatch(readxl::excel_sheets(ruta_av), error = function(e) NULL)
    if (is.null(hojas)) {
      return(div(style = "color:#b00020; margin-top:10px;", tr("av_error_hoja", idi)))
    }
    tagList(
      selectInput("hoja_av", tr("av_hoja", idi), choices = hojas, selected = hojas[1]),
      h4(tr("av_preview_titulo", idi)),
      p(class = "ayuda", tr("av_preview_ayuda", idi)),
      div(style = "overflow-x:auto; border:1px solid #eee; border-radius:6px;",
          tableOutput("preview_crudo_av")),
      fluidRow(
        column(3, numericInput("fila_head_ini", tr("av_fila_head", idi), value = 1, min = 1, step = 1)),
        column(3, numericInput("n_filas_head", tr("av_n_head", idi), value = 1, min = 1, step = 1)),
        column(3, numericInput("fila_datos_ini", tr("av_fila_datos", idi), value = 2, min = 1, step = 1)),
        column(3, numericInput("col_ini", tr("av_col_ini", idi), value = 1, min = 1, step = 1))
      ),
      checkboxInput("rellenar_merges", tr("av_rellenar", idi), value = TRUE),
      actionButton("construir_av", tr("av_construir", idi), class = "btn-info"),
      p(class = "ayuda", tr("av_nota_fecha", idi))
    )
  })

  # Vista previa "cruda" de la hoja elegida (sin tratar ninguna fila como encabezado)
  output$preview_crudo_av <- renderTable({
    req(input$archivo_av, input$hoja_av)
    idi <- rv$idioma
    ruta_av <- preparar_ruta_excel(input$archivo_av$datapath, input$archivo_av$name)
    crudo <- tryCatch(
      readxl::read_excel(ruta_av, sheet = input$hoja_av,
                         col_names = FALSE, col_types = "text", n_max = 12,
                         .name_repair = "minimal"),
      error = function(e) NULL)
    req(crudo)
    crudo <- as.data.frame(crudo)
    n_col_mostrar <- min(ncol(crudo), 12)
    crudo <- crudo[, seq_len(n_col_mostrar), drop = FALSE]
    colnames(crudo) <- paste0(tr("prev_col", idi), " ", seq_len(n_col_mostrar))
    crudo <- cbind(stats::setNames(data.frame(seq_len(nrow(crudo))), tr("prev_fila", idi)), crudo)
    crudo
  }, striped = TRUE, bordered = TRUE, na = "")

  # Construir la tabla plana a partir de la configuración del usuario
  observeEvent(input$construir_av, {
    req(input$archivo_av, input$hoja_av)
    ruta <- preparar_ruta_excel(input$archivo_av$datapath, input$archivo_av$name); hoja <- input$hoja_av
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
      showNotification(paste(tr("av_tabla_error", rv$idioma), e$message),
                       type = "error", duration = 10); NULL
    })

    if (!is.null(resultado) && ncol(resultado) > 0) {
      rv$datos_crudos <- resultado
      rv$datos_limpios <- NULL
      agregar_bitacora(sprintf(
        "Importación avanzada del archivo '%s' (hoja '%s'): %d filas x %d columnas. Encabezados: filas %d-%d; datos desde fila %d; primera columna %d.",
        input$archivo_av$name, hoja, nrow(resultado), ncol(resultado),
        fh, fh + nh - 1, fd, ci))
      showNotification(sprintf(tr("av_tabla_ok", rv$idioma),
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
    showNotification(tr("p4_sin_notif", rv$idioma), type = "message")
  })

  observeEvent(input$aplicar_limpieza, {
    req(rv$datos_crudos)
    df <- rv$datos_crudos
    idi <- rv$idioma
    L <- function(es, en) if (idi == "en") en else es
    filas_antes <- nrow(df)
    cols_antes <- ncol(df)
    acciones <- c()

    if ("trim_texto" %in% input$opciones_limpieza) {
      df[] <- lapply(df, function(x) {
        if (is.character(x)) trimws(x) else x
      })
      acciones <- c(acciones, L("se quitaron espacios en blanco al inicio/final del texto",
                                 "leading/trailing whitespace was removed from text"))
    }

    if ("estandarizar_texto" %in% input$opciones_limpieza) {
      cols_texto <- names(df)[sapply(df, is.character)]
      if (length(cols_texto) > 0) {
        df[cols_texto] <- lapply(df[cols_texto], estandarizar_texto_vector)
      }
      acciones <- c(acciones, sprintf(L("se estandarizó el texto de %d columna(s) categórica(s)",
                                         "text was standardized in %d categorical column(s)"), length(cols_texto)))
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
        sprintf(L("se convirtieron a número %d columna(s): %s", "%d column(s) converted to numbers: %s"),
                length(convertidas), paste(convertidas, collapse = ", "))
        else L("no se encontraron columnas de texto que fueran realmente numéricas",
               "no text columns were found that were actually numeric"))
    }

    if ("redondear" %in% input$opciones_limpieza) {
      dec <- if (is.null(input$redondear_decimales) || is.na(input$redondear_decimales)) 2
             else as.integer(input$redondear_decimales)
      cols_num <- names(df)[sapply(df, is.numeric)]
      df[cols_num] <- lapply(df[cols_num], function(x) round(x, dec))
      acciones <- c(acciones, sprintf(L("se redondearon %d columna(s) numérica(s) a %d decimal(es)",
                                         "%d numeric column(s) rounded to %d decimal(s)"), length(cols_num), dec))
    }

    if ("filtrar_rango" %in% input$opciones_limpieza) {
      col <- input$rango_col
      if (!is.null(col) && col %in% names(df) && is.numeric(df[[col]])) {
        vmin <- input$rango_min; vmax <- input$rango_max
        n_antes <- nrow(df)
        dentro <- is.na(df[[col]]) | (df[[col]] >= vmin & df[[col]] <= vmax)
        df <- df[dentro, , drop = FALSE]
        acciones <- c(acciones, sprintf(L("se eliminaron %d fila(s) con '%s' fuera del rango [%s, %s]",
                                           "%d row(s) with '%s' outside the range [%s, %s] were removed"),
                                          n_antes - nrow(df), col, vmin, vmax))
      } else {
        acciones <- c(acciones, L("no se aplicó el filtro por rango (columna no válida)",
                                   "the range filter was not applied (invalid column)"))
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
        acciones <- c(acciones, sprintf(L("se detectaron %d valor(es) atípico(s) y se eliminaron %d fila(s)",
                                           "%d outlier(s) were detected and %d row(s) removed"),
                                          total_outliers, n_antes - nrow(df)))
      } else {
        acciones <- c(acciones, sprintf(L("se detectaron %d valor(es) atípico(s) y se marcaron como faltantes (NA)",
                                           "%d outlier(s) were detected and marked as missing (NA)"),
                                          total_outliers))
      }
    }

    if ("quitar_col_vacia" %in% input$opciones_limpieza) {
      cols_vacias <- sapply(df, function(x) all(is.na(x)))
      if (any(cols_vacias)) df <- df[, !cols_vacias, drop = FALSE]
      acciones <- c(acciones, sprintf(L("se eliminaron %d columna(s) totalmente vacías",
                                         "%d completely empty column(s) were removed"), sum(cols_vacias)))
    }

    if ("quitar_duplicados" %in% input$opciones_limpieza) {
      n_antes <- nrow(df)
      df <- df[!duplicated(df), , drop = FALSE]
      acciones <- c(acciones, sprintf(L("se eliminaron %d fila(s) duplicada(s)",
                                         "%d duplicate row(s) were removed"), n_antes - nrow(df)))
    }

    if ("quitar_na" %in% input$opciones_limpieza) {
      n_antes <- nrow(df)
      df <- df[complete.cases(df), , drop = FALSE]
      acciones <- c(acciones, sprintf(L("se eliminaron %d fila(s) con datos faltantes",
                                         "%d row(s) with missing data were removed"), n_antes - nrow(df)))
    }

    # Se aplica al final para no romper las referencias por nombre de columna
    # que usan las demás operaciones (ej. el filtro por rango).
    if ("limpiar_nombres" %in% input$opciones_limpieza) {
      nombres_antes <- names(df)
      names(df) <- limpiar_nombres_columnas(names(df))
      cambiados <- sum(nombres_antes != names(df))
      acciones <- c(acciones, sprintf(L("se limpiaron los nombres de columnas (%d cambiado(s))",
                                         "column names were cleaned (%d changed)"), cambiados))
    }

    rv$datos_limpios <- df
    rv$limpieza_aplicada <- TRUE

    if (length(acciones) == 0) {
      agregar_bitacora("Se dio clic en 'Aplicar limpieza' pero no se seleccionó ninguna opción.")
    } else {
      agregar_bitacora(paste0("Limpieza aplicada: ", paste(acciones, collapse = "; "), "."))
    }

    output$resultado_limpieza <- renderUI({
      idi <- rv$idioma
      div(style = "margin-top: 15px; padding: 12px; background-color: #eef7ee; border-radius: 6px;",
          h4(tr("p4_result_titulo", idi)),
          p(sprintf(tr("p4_result_resumen", idi), filas_antes, nrow(df), cols_antes, ncol(df))),
          if (length(acciones) > 0)
            tagList(p(strong(tr("p4_result_acciones", idi))), tags$ul(lapply(acciones, tags$li)))
          else p(tr("p4_result_sin_cambios", idi))
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
      idi <- rv$idioma
      d <- data.frame(
        n = length(x),
        Media = round(mean(x), 2),
        Mediana = round(median(x), 2),
        DE = round(sd(x), 2),
        Minimo = round(min(x), 2),
        Maximo = round(max(x), 2)
      )
      names(d) <- c(tr("p5_desc_n", idi), tr("p5_desc_media", idi), tr("p5_desc_mediana", idi),
                    tr("p5_desc_de", idi), tr("p5_desc_min", idi), tr("p5_desc_max", idi))
      d
    }

    if (!is.null(input$desc_var_grupo) && input$desc_var_grupo != "") {
      resumen <- do.call(rbind, lapply(split(df[[input$desc_var]], df[[input$desc_var_grupo]]),
                                        calcular_resumen))
      resumen <- cbind(Grupo = rownames(resumen), resumen)
      names(resumen)[1] <- tr("p5_desc_grupo_col", rv$idioma)
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
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
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

    idi <- rv$idioma
    resultado_texto <- NULL
    tabla_resultado <- NULL

    if (n_grupos < 2) {
      resultado_texto <- tr("p5c_min2", idi)
    } else {
      valores_por_grupo <- split(df_sub[[input$comp_num]], df_sub[[input$comp_grupo]])
      p_normalidad <- sapply(valores_por_grupo, function(x) {
        if (length(x) >= 3 && length(x) <= 5000) tryCatch(shapiro.test(x)$p.value, error = function(e) NA)
        else NA
      })
      es_normal <- all(!is.na(p_normalidad) & p_normalidad > 0.05)
      nota_normal <- if (es_normal) tr("p5c_normal_si", idi) else tr("p5c_normal_no", idi)

      formula_comp <- as.formula(paste0("`", input$comp_num, "` ~ `", input$comp_grupo, "`"))

      if (n_grupos == 2) {
        if (es_normal) {
          prueba <- t.test(formula_comp, data = df_sub)
          nombre_prueba <- tr("p5c_t", idi)
        } else {
          prueba <- wilcox.test(formula_comp, data = df_sub)
          nombre_prueba <- tr("p5c_wilcoxon", idi)
        }
        resultado_texto <- paste0(
          sprintf(tr("p5c_comp2", idi), paste(grupos, collapse = " vs. "), nombre_prueba, nota_normal),
          interpretar_p(prueba$p.value, idi)
        )
      } else {
        if (es_normal) {
          modelo <- aov(formula_comp, data = df_sub)
          p_valor <- summary(modelo)[[1]][["Pr(>F)"]][1]
          nombre_prueba <- tr("p5c_anova", idi)
          posthoc <- TukeyHSD(modelo)[[1]]
          tabla_resultado <- data.frame(Comparacion = rownames(posthoc), round(as.data.frame(posthoc), 4))
        } else {
          kt <- kruskal.test(formula_comp, data = df_sub)
          p_valor <- kt$p.value
          nombre_prueba <- tr("p5c_kruskal", idi)
          ph <- pairwise.wilcox.test(df_sub[[input$comp_num]], df_sub[[input$comp_grupo]],
                                       p.adjust.method = "BH")
          tabla_resultado <- as.data.frame(as.table(ph$p.value))
          names(tabla_resultado) <- c("Grupo_1", "Grupo_2", "p_ajustada")
          tabla_resultado <- tabla_resultado[!is.na(tabla_resultado$p_ajustada), ]
        }
        resultado_texto <- paste0(
          sprintf(tr("p5c_compN", idi), n_grupos, paste(grupos, collapse = ", "), nombre_prueba, nota_normal),
          interpretar_p(p_valor, idi),
          tr("p5c_posthoc", idi)
        )
      }
    }

    output$out_comp <- renderText(resultado_texto)
    output$out_comp_tabla <- renderTable(tabla_resultado)
    rv$resultados$comparacion <- list(
      titulo = sprintf(tr("p5c_titulo", idi), input$comp_num, input$comp_grupo),
      texto = resultado_texto,
      tabla = tabla_resultado
    )
    agregar_bitacora(sprintf("Comparación de grupos: '%s' según '%s'.", input$comp_num, input$comp_grupo))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Correlación ---
  observeEvent(input$run_corr, { tryCatch({
    df <- datos_actuales()
    req(df, input$corr_var1, input$corr_var2)

    idi <- rv$idioma
    texto <- NULL
    if (input$corr_var1 == input$corr_var2) {
      texto <- tr("p5cor_distintas", idi)
    } else {
      x1 <- suppressWarnings(as.numeric(df[[input$corr_var1]]))
      x2 <- suppressWarnings(as.numeric(df[[input$corr_var2]]))
      completos <- !is.na(x1) & !is.na(x2)
      x1 <- x1[completos]; x2 <- x2[completos]

      if (length(x1) < 3) {
        texto <- tr("p5cor_insuf", idi)
      } else if (sd(x1) == 0 || sd(x2) == 0) {
        texto <- tr("p5cor_sinvar", idi)
      } else {
        p1 <- if (length(x1) <= 5000) tryCatch(shapiro.test(x1)$p.value, error = function(e) NA) else NA
        p2 <- if (length(x2) <= 5000) tryCatch(shapiro.test(x2)$p.value, error = function(e) NA) else NA
        normal <- !is.na(p1) && !is.na(p2) && p1 > 0.05 && p2 > 0.05
        metodo <- if (normal) "pearson" else "spearman"

        prueba <- tryCatch(cor.test(x1, x2, method = metodo), error = function(e) NULL)
        if (is.null(prueba)) {
          texto <- tr("p5cor_nulo", idi)
        } else {
          fuerza <- abs(prueba$estimate)
          descripcion_fuerza <- if (fuerza < 0.3) tr("p5cor_debil", idi) else if (fuerza < 0.6) tr("p5cor_moderada", idi) else tr("p5cor_fuerte", idi)
          direccion <- if (prueba$estimate > 0) tr("p5cor_pos", idi) else tr("p5cor_neg", idi)
          nombre_metodo <- if (normal) "Pearson" else "Spearman"
          nota_normal <- if (normal) tr("p5cor_normal_si", idi) else tr("p5cor_normal_no", idi)
          texto <- paste0(
            sprintf(tr("p5cor_texto", idi), nombre_metodo, nota_normal, prueba$estimate,
                    interpretar_p(prueba$p.value, idi), descripcion_fuerza, direccion)
          )
        }
      }
    }

    output$out_corr <- renderText(texto)
    rv$resultados$correlacion <- list(
      titulo = sprintf(tr("p5cor_titulo", idi), input$corr_var1, input$corr_var2),
      texto = texto
    )
    agregar_bitacora(sprintf("Correlación calculada entre '%s' y '%s'.", input$corr_var1, input$corr_var2))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Frecuencias ---
  observeEvent(input$run_freq, { tryCatch({
    df <- datos_actuales()
    req(df, input$freq_var)
    idi <- rv$idioma

    x <- df[[input$freq_var]]
    req(!is.null(x))
    tb <- table(x, useNA = "ifany")
    tabla <- data.frame(
      Categoria = names(tb),
      Frecuencia = as.integer(tb),
      stringsAsFactors = FALSE
    )
    tabla$Porcentaje <- round(100 * tabla$Frecuencia / sum(tabla$Frecuencia), 1)
    names(tabla) <- c(tr("p5f_categoria", idi), tr("p5f_frecuencia", idi), tr("p5f_porcentaje", idi))

    output$out_freq <- renderTable(tabla)
    rv$resultados$frecuencias <- list(
      titulo = sprintf(tr("p5f_titulo", idi), input$freq_var),
      tabla = tabla
    )
    agregar_bitacora(sprintf("Tabla de frecuencias calculada para '%s'.", input$freq_var))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Evolución en el tiempo ---
  observeEvent(input$run_long, { tryCatch({
    df <- datos_actuales()
    req(df, input$long_tiempo, input$long_grupo, input$long_resp)

    idi <- rv$idioma
    resumen <- df %>%
      dplyr::filter(!is.na(.data[[input$long_resp]])) %>%
      dplyr::group_by(.data[[input$long_tiempo]], .data[[input$long_grupo]]) %>%
      dplyr::summarise(
        n = dplyr::n(),
        Promedio = round(mean(.data[[input$long_resp]], na.rm = TRUE), 2),
        Error_estandar = round(sd(.data[[input$long_resp]], na.rm = TRUE) / sqrt(dplyr::n()), 2),
        .groups = "drop"
      )
    names(resumen) <- c(input$long_tiempo, input$long_grupo, tr("p5l_n", idi),
                        tr("p5l_promedio", idi), tr("p5l_ee", idi))

    output$out_long <- renderTable(resumen)
    rv$resultados$longitudinal <- list(
      titulo = sprintf(tr("p5l_titulo", idi),
                        input$long_resp, input$long_tiempo, input$long_grupo),
      tabla = resumen
    )
    agregar_bitacora(sprintf("Evolución en el tiempo calculada: '%s' a lo largo de '%s', por '%s'.",
                              input$long_resp, input$long_tiempo, input$long_grupo))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
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

    idi <- rv$idioma
    if (input$par_m1 == input$par_m2) {
      texto <- tr("p5p_dos_momentos", idi)
    } else {
      d1 <- df[as.character(df[[input$par_cond]]) == input$par_m1, c(input$par_id, input$par_resp)]
      d2 <- df[as.character(df[[input$par_cond]]) == input$par_m2, c(input$par_id, input$par_resp)]
      names(d1) <- c("id", "v1"); names(d2) <- c("id", "v2")
      emparejado <- merge(d1, d2, by = "id")
      emparejado <- emparejado[!is.na(emparejado$v1) & !is.na(emparejado$v2), ]

      if (nrow(emparejado) < 2) {
        texto <- tr("p5p_insuf", idi)
      } else {
        difs <- emparejado$v2 - emparejado$v1
        p_norm <- if (length(difs) >= 3 && length(difs) <= 5000)
          tryCatch(shapiro.test(difs)$p.value, error = function(e) NA) else NA
        normal <- !is.na(p_norm) && p_norm > 0.05
        if (normal) {
          prueba <- t.test(emparejado$v2, emparejado$v1, paired = TRUE)
          nombre <- tr("p5p_t", idi)
        } else {
          prueba <- wilcox.test(emparejado$v2, emparejado$v1, paired = TRUE)
          nombre <- tr("p5p_wilcoxon", idi)
        }
        texto <- paste0(
          sprintf(tr("p5p_texto", idi), input$par_resp, input$par_m1, input$par_m2,
                  nrow(emparejado), as.character(round(mean(difs), 3)), nombre),
          interpretar_p(prueba$p.value, idi)
        )
      }
    }
    output$out_par <- renderText(texto)
    rv$resultados$pareada <- list(
      titulo = sprintf(tr("p5p_titulo", idi), input$par_resp, input$par_m1, input$par_m2),
      texto = texto)
    agregar_bitacora(sprintf("Comparación pareada de '%s': %s vs. %s.",
                              input$par_resp, input$par_m1, input$par_m2))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Tamaño del efecto (d de Cohen) ---
  observeEvent(input$run_cohen, { tryCatch({
    df <- datos_actuales()
    req(df, input$cohen_grupo, input$cohen_num)
    idi <- rv$idioma
    df_sub <- df[!is.na(df[[input$cohen_num]]) & !is.na(df[[input$cohen_grupo]]), ]
    grupos <- unique(as.character(df_sub[[input$cohen_grupo]]))

    if (length(grupos) != 2) {
      texto <- sprintf(tr("p5co_2cat", idi), length(grupos))
    } else {
      x1 <- df_sub[[input$cohen_num]][as.character(df_sub[[input$cohen_grupo]]) == grupos[1]]
      x2 <- df_sub[[input$cohen_num]][as.character(df_sub[[input$cohen_grupo]]) == grupos[2]]
      n1 <- length(x1); n2 <- length(x2)
      sp <- sqrt(((n1 - 1) * var(x1) + (n2 - 1) * var(x2)) / (n1 + n2 - 2))
      d <- (mean(x1) - mean(x2)) / sp
      se_d <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
      ic_bajo <- d - 1.96 * se_d; ic_alto <- d + 1.96 * se_d
      magnitud <- if (abs(d) < 0.2) tr("p5co_ins", idi) else if (abs(d) < 0.5) tr("p5co_peq", idi)
                  else if (abs(d) < 0.8) tr("p5co_med", idi) else tr("p5co_gra", idi)
      texto <- sprintf(tr("p5co_texto", idi), grupos[1], grupos[2], input$cohen_num,
                       as.character(round(d, 3)), magnitud,
                       as.character(round(ic_bajo, 3)), as.character(round(ic_alto, 3)))
    }
    output$out_cohen <- renderText(texto)
    rv$resultados$cohen <- list(
      titulo = sprintf(tr("p5co_titulo", idi), input$cohen_num, input$cohen_grupo),
      texto = texto)
    agregar_bitacora(sprintf("Tamaño del efecto (d de Cohen) calculado para '%s' por '%s'.",
                              input$cohen_num, input$cohen_grupo))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Matriz de correlación ---
  observeEvent(input$run_mcorr, { tryCatch({
    df <- datos_actuales()
    req(df, input$mcorr_vars)
    idi <- rv$idioma
    if (length(input$mcorr_vars) < 2) {
      output$out_mcorr <- renderTable(stats::setNames(data.frame(tr("p5m_min2", idi)), tr("p5m_aviso_col", idi)))
      return()
    }
    sub <- df[, input$mcorr_vars, drop = FALSE]
    sub <- as.data.frame(lapply(sub, function(x) suppressWarnings(as.numeric(x))))
    sub <- sub[complete.cases(sub), , drop = FALSE]
    if (nrow(sub) < 3) {
      output$out_mcorr <- renderTable(stats::setNames(data.frame(tr("p5m_insuf", idi)), tr("p5m_aviso_col", idi)))
      return()
    }
    m <- tryCatch(round(cor(sub, method = input$mcorr_metodo), 3), error = function(e) NULL)
    if (is.null(m)) {
      output$out_mcorr <- renderTable(stats::setNames(data.frame(tr("p5m_nulo", idi)), tr("p5m_aviso_col", idi)))
      return()
    }
    tabla <- cbind(Variable = rownames(m), as.data.frame(m))
    rownames(tabla) <- NULL
    output$out_mcorr <- renderTable(tabla)
    rv$resultados$matriz_corr <- list(
      titulo = sprintf(tr("p5m_titulo", idi), input$mcorr_metodo),
      tabla = tabla)
    agregar_bitacora(sprintf("Matriz de correlación (%s) calculada para: %s.",
                              input$mcorr_metodo, paste(input$mcorr_vars, collapse = ", ")))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Tabla de contingencia (chi-cuadrada / Fisher) ---
  observeEvent(input$run_cont, { tryCatch({
    df <- datos_actuales()
    req(df, input$cont_var1, input$cont_var2)
    idi <- rv$idioma
    if (input$cont_var1 == input$cont_var2) {
      output$out_cont <- renderText(tr("p5ct_distintas", idi))
      output$out_cont_tabla <- renderTable(NULL)
      return()
    }
    tabla_cont <- table(df[[input$cont_var1]], df[[input$cont_var2]])
    esperados <- tryCatch(suppressWarnings(chisq.test(tabla_cont)$expected), error = function(e) NULL)
    usar_fisher <- !is.null(esperados) && any(esperados < 5)

    if (usar_fisher) {
      prueba <- tryCatch(fisher.test(tabla_cont, simulate.p.value = TRUE, B = 10000),
                          error = function(e) NULL)
      nombre <- tr("p5ct_fisher", idi)
    } else {
      prueba <- suppressWarnings(chisq.test(tabla_cont))
      nombre <- tr("p5ct_chi", idi)
    }
    texto <- if (is.null(prueba)) tr("p5ct_nulo", idi) else
      sprintf(tr("p5ct_texto", idi), input$cont_var1, input$cont_var2, nombre,
              interpretar_p(prueba$p.value, idi))
    tabla_df <- as.data.frame.matrix(tabla_cont)
    tabla_df <- cbind(" " = rownames(tabla_df), tabla_df); rownames(tabla_df) <- NULL

    output$out_cont <- renderText(texto)
    output$out_cont_tabla <- renderTable(tabla_df)
    rv$resultados$contingencia <- list(
      titulo = sprintf(tr("p5ct_titulo", idi), input$cont_var1, input$cont_var2),
      texto = texto, tabla = tabla_df)
    agregar_bitacora(sprintf("Tabla de contingencia: '%s' vs. '%s'.", input$cont_var1, input$cont_var2))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
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
    idi <- rv$idioma
    modelo <- tryCatch(aov(formula_a2, data = df_sub), error = function(e) NULL)
    if (is.null(modelo)) {
      output$out_a2 <- renderText(tr("p5a2_nulo", idi))
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
    names(tabla) <- c(tr("p5a2_termino", idi), tr("p5a2_gl", idi), "F", tr("p5a2_p", idi))
    p_int <- resumen[["Pr(>F)"]][3]
    nota_int <- if (!is.na(p_int) && p_int < 0.05) tr("p5a2_int_si", idi) else tr("p5a2_int_no", idi)
    texto <- sprintf(tr("p5a2_texto", idi), input$a2_factor1, input$a2_factor2, input$a2_resp, nota_int)
    output$out_a2 <- renderText(texto)
    output$out_a2_tabla <- renderTable(tabla)
    rv$resultados$anova2 <- list(
      titulo = sprintf(tr("p5a2_titulo", idi), input$a2_resp, input$a2_factor1, input$a2_factor2),
      texto = texto, tabla = tabla)
    agregar_bitacora(sprintf("ANOVA de dos vías: '%s' ~ '%s' * '%s'.",
                              input$a2_resp, input$a2_factor1, input$a2_factor2))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
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
    idi <- rv$idioma
    modelo <- tryCatch(
      nlme::lme(formula_fija, random = formula_random, data = df_sub, method = "REML"),
      error = function(e) NULL)

    if (is.null(modelo)) {
      output$out_mix <- renderText(tr("p5mx_nulo", idi))
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
    names(tabla) <- c(tr("p5mx_termino", idi), tr("p5mx_gl", idi), "F", tr("p5mx_p", idi))
    texto <- sprintf(tr("p5mx_texto", idi), input$mix_resp, input$mix_id, input$mix_grupo, input$mix_tiempo)
    output$out_mix <- renderText(texto)
    output$out_mix_tabla <- renderTable(tabla)
    rv$resultados$mixto <- list(
      titulo = sprintf(tr("p5mx_titulo", idi),
                        input$mix_resp, input$mix_grupo, input$mix_tiempo, input$mix_id),
      texto = texto, tabla = tabla)
    agregar_bitacora(sprintf("Modelo mixto ajustado para '%s' (sujeto aleatorio: '%s').",
                              input$mix_resp, input$mix_id))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Regresión lineal ---
  observeEvent(input$run_rl, { tryCatch({
    df <- datos_actuales()
    req(df, input$rl_resp, input$rl_pred)
    idi <- rv$idioma
    if (length(input$rl_pred) < 1) {
      output$out_rl <- renderText(tr("p5rl_min1", idi))
      output$out_rl_tabla <- renderTable(NULL); return()
    }
    predictores <- paste(sprintf("`%s`", input$rl_pred), collapse = " + ")
    formula_rl <- as.formula(paste0("`", input$rl_resp, "` ~ ", predictores))
    modelo <- tryCatch(lm(formula_rl, data = df), error = function(e) NULL)
    if (is.null(modelo)) {
      output$out_rl <- renderText(tr("p5rl_nulo", idi))
      output$out_rl_tabla <- renderTable(NULL); return()
    }
    co <- summary(modelo)$coefficients
    tabla <- data.frame(
      Termino = rownames(co),
      Coeficiente = round(co[, 1], 4),
      Error_estandar = round(co[, 2], 4),
      valor_p = signif(co[, 4], 4)
    ); rownames(tabla) <- NULL
    names(tabla) <- c(tr("p5rl_termino", idi), tr("p5rl_coef", idi), tr("p5rl_ee", idi), tr("p5rl_p", idi))
    r2 <- summary(modelo)$r.squared
    texto <- sprintf(tr("p5rl_texto", idi), input$rl_resp, as.character(round(r2, 3)), as.character(round(100 * r2, 1)))
    output$out_rl <- renderText(texto)
    output$out_rl_tabla <- renderTable(tabla)
    rv$resultados$reg_lineal <- list(
      titulo = sprintf(tr("p5rl_titulo", idi), input$rl_resp, paste(input$rl_pred, collapse = " + ")),
      texto = texto, tabla = tabla)
    agregar_bitacora(sprintf("Regresión lineal ajustada: '%s' ~ %s.",
                              input$rl_resp, paste(input$rl_pred, collapse = " + ")))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Regresión logística ---
  observeEvent(input$run_rlog, { tryCatch({
    df <- datos_actuales()
    req(df, input$rlog_resp, input$rlog_pred)
    idi <- rv$idioma
    if (length(input$rlog_pred) < 1) {
      output$out_rlog <- renderText(tr("p5rg_min1", idi))
      output$out_rlog_tabla <- renderTable(NULL); return()
    }
    y_original <- df[[input$rlog_resp]]
    niveles <- unique(as.character(y_original[!is.na(y_original)]))
    if (length(niveles) != 2) {
      output$out_rlog <- renderText(sprintf(tr("p5rg_2cat", idi), length(niveles)))
      output$out_rlog_tabla <- renderTable(NULL); return()
    }
    df2 <- df
    niveles_ord <- sort(niveles)
    df2$.y_bin <- ifelse(as.character(y_original) == niveles_ord[2], 1, 0)
    predictores <- paste(sprintf("`%s`", input$rlog_pred), collapse = " + ")
    formula_rlog <- as.formula(paste0(".y_bin ~ ", predictores))
    modelo <- tryCatch(glm(formula_rlog, data = df2, family = binomial), error = function(e) NULL)
    if (is.null(modelo)) {
      output$out_rlog <- renderText(tr("p5rg_nulo", idi))
      output$out_rlog_tabla <- renderTable(NULL); return()
    }
    co <- summary(modelo)$coefficients
    tabla <- data.frame(
      Termino = rownames(co),
      OR = round(exp(co[, 1]), 3),
      Coeficiente = round(co[, 1], 4),
      valor_p = signif(co[, 4], 4)
    ); rownames(tabla) <- NULL
    names(tabla) <- c(tr("p5rg_termino", idi), "OR", tr("p5rg_coef", idi), tr("p5rg_p", idi))
    texto <- sprintf(tr("p5rg_texto", idi), input$rlog_resp, niveles_ord[2])
    output$out_rlog <- renderText(texto)
    output$out_rlog_tabla <- renderTable(tabla)
    rv$resultados$reg_logistica <- list(
      titulo = sprintf(tr("p5rg_titulo", idi), input$rlog_resp, paste(input$rlog_pred, collapse = " + ")),
      texto = texto, tabla = tabla)
    agregar_bitacora(sprintf("Regresión logística ajustada: '%s' ~ %s.",
                              input$rlog_resp, paste(input$rlog_pred, collapse = " + ")))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  # --- Prueba de normalidad (Shapiro-Wilk) ---
  observeEvent(input$run_norm, { tryCatch({
    df <- datos_actuales()
    req(df, input$norm_var)

    idi <- rv$idioma
    evaluar <- function(x, etiqueta) {
      x <- x[!is.na(x)]
      if (length(x) < 3) return(sprintf(tr("p5n_pocos", idi), etiqueta, length(x)))
      if (length(x) > 5000) return(sprintf(tr("p5n_muchos", idi), etiqueta))
      p <- tryCatch(shapiro.test(x)$p.value, error = function(e) NA)
      if (is.na(p)) return(sprintf(tr("p5n_noeval", idi), etiqueta))
      veredicto <- if (p > 0.05) tr("p5n_normal", idi) else tr("p5n_nonormal", idi)
      sprintf(tr("p5n_linea", idi), etiqueta, p, veredicto)
    }

    if (!is.null(input$norm_grupo) && input$norm_grupo != "") {
      grupos <- unique(as.character(df[[input$norm_grupo]]))
      lineas <- sapply(grupos, function(g) {
        evaluar(df[[input$norm_var]][as.character(df[[input$norm_grupo]]) == g], sprintf(tr("p5n_grupo", idi), g))
      })
    } else {
      lineas <- evaluar(df[[input$norm_var]], input$norm_var)
    }
    texto <- sprintf(tr("p5n_texto", idi), input$norm_var, paste(lineas, collapse = "\n"))
    output$out_norm <- renderText(texto)
    rv$resultados$normalidad <- list(
      titulo = sprintf(tr("p5n_titulo", idi), input$norm_var),
      texto = texto)
    agregar_bitacora(sprintf("Prueba de normalidad (Shapiro-Wilk) para '%s'.", input$norm_var))
  
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error"))
        showNotification(paste(tr("p5_error", rv$idioma), conditionMessage(e)),
                         type = "error", duration = 9)
    })
  })

  ## =====================================================================
  ## PASO 6: Graficar
  ## =====================================================================

  observeEvent(input$generar_grafica, {
    df <- datos_actuales()
    req(df, input$tipo_grafica)
    idi <- rv$idioma
    rv$km_texto <- NULL

    grafica <- tryCatch({
      if (input$tipo_grafica == "histograma") {
        req(input$hist_var)
        ggplot(df, aes(x = .data[[input$hist_var]])) +
          geom_histogram(bins = 20, fill = "#7a1f3d", color = "white", alpha = 0.9) +
          labs(title = sprintf(tr("g_hist_title", idi), input$hist_var),
               x = input$hist_var, y = tr("g_frecuencia", idi)) +
          theme_minimal(base_size = 14)

      } else if (input$tipo_grafica == "boxplot") {
        req(input$box_grupo, input$box_num)
        ggplot(df, aes(x = factor(.data[[input$box_grupo]]), y = .data[[input$box_num]],
                       fill = factor(.data[[input$box_grupo]]))) +
          geom_boxplot(alpha = 0.85) +
          labs(title = sprintf(tr("g_por", idi), input$box_num, input$box_grupo),
               x = input$box_grupo, y = input$box_num, fill = input$box_grupo) +
          theme_minimal(base_size = 14) +
          theme(legend.position = "none")

      } else if (input$tipo_grafica == "violin") {
        req(input$violin_grupo, input$violin_num)
        ggplot(df, aes(x = factor(.data[[input$violin_grupo]]), y = .data[[input$violin_num]],
                       fill = factor(.data[[input$violin_grupo]]))) +
          geom_violin(alpha = 0.8, trim = FALSE) +
          geom_boxplot(width = 0.12, fill = "white", alpha = 0.7, outlier.shape = NA) +
          labs(title = sprintf(tr("g_por", idi), input$violin_num, input$violin_grupo),
               x = input$violin_grupo, y = input$violin_num) +
          theme_minimal(base_size = 14) +
          theme(legend.position = "none")

      } else if (input$tipo_grafica == "dispersion") {
        req(input$disp_x, input$disp_y)
        if (!is.null(input$disp_color) && input$disp_color != "") {
          ggplot(df, aes(x = .data[[input$disp_x]], y = .data[[input$disp_y]],
                        color = factor(.data[[input$disp_color]]))) +
            geom_point(size = 2.6, alpha = 0.85) +
            labs(title = sprintf(tr("g_vs", idi), input$disp_y, input$disp_x),
                 x = input$disp_x, y = input$disp_y, color = input$disp_color) +
            theme_minimal(base_size = 14)
        } else {
          ggplot(df, aes(x = .data[[input$disp_x]], y = .data[[input$disp_y]])) +
            geom_point(size = 2.6, alpha = 0.85, color = "#7a1f3d") +
            labs(title = sprintf(tr("g_vs", idi), input$disp_y, input$disp_x),
                 x = input$disp_x, y = input$disp_y) +
            theme_minimal(base_size = 14)
        }

      } else if (input$tipo_grafica == "barras") {
        req(input$barras_var)
        ggplot(df, aes(x = factor(.data[[input$barras_var]]))) +
          geom_bar(fill = "#7a1f3d", alpha = 0.9) +
          labs(title = sprintf(tr("g_bars_title", idi), input$barras_var),
               x = input$barras_var, y = tr("g_conteo", idi)) +
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
          labs(title = sprintf(tr("g_barserr_title", idi), input$barras_error_num, input$barras_error_grupo),
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
          labs(title = sprintf(tr("g_heat_title", idi), input$heat_valor),
               x = input$heat_columna, y = input$heat_fila, fill = tr("g_promedio", idi)) +
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
          labs(title = sprintf(tr("g_line_title", idi), input$lt_resp),
               x = input$lt_tiempo, y = sprintf(tr("g_line_y", idi), input$lt_resp),
               color = input$lt_grupo) +
          theme_minimal(base_size = 14)

      } else if (input$tipo_grafica == "spaghetti") {
        req(input$sp_id, input$sp_tiempo, input$sp_resp)
        if (!is.null(input$sp_grupo) && input$sp_grupo != "") {
          ggplot(df, aes(x = .data[[input$sp_tiempo]], y = .data[[input$sp_resp]],
                        group = .data[[input$sp_id]], color = factor(.data[[input$sp_grupo]]))) +
            geom_line(alpha = 0.6) +
            geom_point(size = 1.6, alpha = 0.7) +
            labs(title = sprintf(tr("g_spag_title", idi), input$sp_resp),
                 x = input$sp_tiempo, y = input$sp_resp, color = input$sp_grupo) +
            theme_minimal(base_size = 14)
        } else {
          ggplot(df, aes(x = .data[[input$sp_tiempo]], y = .data[[input$sp_resp]],
                        group = .data[[input$sp_id]])) +
            geom_line(alpha = 0.5, color = "#7a1f3d") +
            geom_point(size = 1.6, alpha = 0.6, color = "#7a1f3d") +
            labs(title = sprintf(tr("g_spag_title", idi), input$sp_resp),
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
          etiquetas_grupo <- rep(tr("g_km_todos", idi), length(ajuste$time))
        }

        df_km <- data.frame(Tiempo = ajuste$time, Supervivencia = ajuste$surv, Grupo = etiquetas_grupo)
        inicio <- data.frame(Tiempo = 0, Supervivencia = 1, Grupo = unique(df_km$Grupo))
        df_km <- rbind(inicio, df_km)
        df_km <- df_km[order(df_km$Grupo, df_km$Tiempo), ]

        if (con_grupo) {
          prueba_lr <- tryCatch(survival::survdiff(formula_km, data = df_sub), error = function(e) NULL)
          if (!is.null(prueba_lr)) {
            p_lr <- 1 - pchisq(prueba_lr$chisq, length(prueba_lr$n) - 1)
            rv$km_texto <- paste0(tr("g_logrank", idi), interpretar_p(p_lr, idi))
          }
        }

        ggplot(df_km, aes(x = Tiempo, y = Supervivencia, color = Grupo)) +
          geom_step(linewidth = 1) +
          ylim(0, 1) +
          labs(title = tr("g_km_title", idi),
               x = input$km_tiempo, y = tr("g_km_y", idi),
               color = if (con_grupo) input$km_grupo else NULL) +
          theme_minimal(base_size = 14)
      }
    }, error = function(e) {
      showNotification(paste(tr("g_error", idi), e$message), type = "error", duration = 8)
      NULL
    })

    # ggplot evalúa de forma diferida: forzamos la construcción para atrapar
    # aquí cualquier error de datos (ej. variable no numérica) en vez de fallar
    # al dibujar y dejar el área en blanco.
    if (!is.null(grafica)) {
      valida <- tryCatch({ ggplot2::ggplot_build(grafica); TRUE },
        error = function(e) {
          showNotification(paste(tr("g_error", idi), e$message, tr("g_error_tipo", idi)),
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
        etiqueta = etiqueta_grafica(input$tipo_grafica, idi)
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
    filename = function() paste0("grafica_aadata_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = rv$ultima_grafica, width = 9, height = 6, dpi = 150)
    }
  )

  ## =====================================================================
  ## PASO 7: Descargar
  ## =====================================================================

  output$descargar_csv <- downloadHandler(
    filename = function() paste0("datos_aadata_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(datos_actuales(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$descargar_xlsx <- downloadHandler(
    filename = function() paste0("datos_aadata_", Sys.Date(), ".xlsx"),
    content = function(file) {
      writexl::write_xlsx(datos_actuales(), file)
    }
  )

  # --- Funciones auxiliares para armar el reporte en PDF ---

  # Pie de página del reporte (se dibuja al final de cada página)
  pie_pagina_pdf <- function() {
    grid::grid.text("Por: ing. Daniel Bonifaz-Calvo Ibarrola",
                     x = 0.5, y = 0.02, just = c("center", "bottom"),
                     gp = grid::gpar(fontsize = 9, col = "gray40", fontface = "italic"))
  }

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
      pie_pagina_pdf()
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
    pie_pagina_pdf()
  }

  # Dibuja una página con una gráfica de ggplot2 y un título arriba
  pagina_grafica_pdf <- function(titulo, grafica) {
    if (is.null(grafica)) return(invisible(NULL))
    gridExtra::grid.arrange(
      grafica,
      top = grid::textGrob(titulo, gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#7a1f3d"))
    )
    pie_pagina_pdf()
  }

  output$descargar_pdf <- downloadHandler(
    filename = function() paste0("reporte_aadata_", Sys.Date(), ".pdf"),
    content = function(file) {
      df_actual <- datos_actuales()
      res <- rv$resultados
      idi <- rv$idioma

      # Orientación elegida por el usuario: vertical (carta) u horizontal (apaisado).
      orientacion <- if (is.null(input$orientacion_pdf)) "vertical" else input$orientacion_pdf
      if (orientacion == "horizontal") {
        pdf_ancho <- 11; pdf_alto <- 8.5
      } else {
        pdf_ancho <- 8.5; pdf_alto <- 11
      }
      grDevices::pdf(file, width = pdf_ancho, height = pdf_alto)
      on.exit(grDevices::dev.off(), add = TRUE)

      ## ---- Portada ----
      grid::grid.newpage()
      grid::grid.text("AaData", x = 0.5, y = 0.72,
                       gp = grid::gpar(fontsize = 28, fontface = "bold", col = "#7a1f3d"))
      grid::grid.text(tr("pdf_subtitulo", idi), x = 0.5, y = 0.65,
                       gp = grid::gpar(fontsize = 16))
      responsable <- trimws(if (is.null(input$responsable_analisis)) "" else input$responsable_analisis)
      if (nzchar(responsable)) {
        grid::grid.text(sprintf(tr("pdf_responsable", idi), responsable),
                         x = 0.5, y = 0.58, gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#333333"))
      }
      grid::grid.text(paste(tr("pdf_generado", idi), format(Sys.time(), "%d/%m/%Y %H:%M")),
                       x = 0.5, y = 0.50, gp = grid::gpar(fontsize = 10, col = "gray40"))
      if (!is.null(df_actual)) {
        grid::grid.text(sprintf(tr("pdf_datos", idi),
                                 nrow(df_actual), ncol(df_actual)),
                         x = 0.5, y = 0.45, gp = grid::gpar(fontsize = 10, col = "gray40"))
      }
      pie_pagina_pdf()

      ## ---- Resumen de los datos ----
      if (!is.null(df_actual)) {
        resumen_lineas <- capture.output(summary(df_actual))
        pagina_texto_pdf(tr("pdf_resumen", idi), resumen_lineas)
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
        pagina_texto_pdf(tr("pdf_bitacora", idi), rv$bitacora)
      }

      ## ---- Aviso si aún no hay nada que reportar ----
      if (length(res) == 0 && length(rv$graficas) == 0) {
        pagina_texto_pdf(tr("pdf_sin_titulo", idi), c(
          tr("pdf_sin_l1", idi),
          tr("pdf_sin_l2", idi),
          tr("pdf_sin_l3", idi)
        ))
      }
    }
  )

  output$descargar_reporte <- downloadHandler(
    filename = function() paste0("bitacora_aadata_", Sys.Date(), ".txt"),
    content = function(file) {
      encabezado <- c(
        tr("txt_encabezado", rv$idioma),
        paste(tr("txt_generado", rv$idioma), Sys.time()),
        strrep("-", 60), ""
      )
      writeLines(c(encabezado, rv$bitacora), file, useBytes = TRUE)
    }
  )

  output$vista_bitacora <- renderText({
    if (length(rv$bitacora) == 0) tr("bit_vacia", rv$idioma)
    else paste(rv$bitacora, collapse = "\n")
  })

}

## ---- 6. Lanzar la aplicación ------------------------------------------
shinyApp(ui = ui, server = server)
