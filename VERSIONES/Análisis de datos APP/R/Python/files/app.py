########################################################################
# Bonifaz-INCMNSZ  ·  Versión Python (Shiny for Python)
# Asistente de análisis de datos paso a paso
# Instituto Nacional de Ciencias Médicas y Nutrición Salvador Zubirán
#
# Esta es la BASE del port desde R. Incluye ya funcionando:
#   - El asistente de 7 pasos con barra de progreso y navegación.
#   - Carga de datos: set de ejemplo (artritis reumatoide en ratones),
#     y subir archivo propio (CSV o Excel) con opciones de separador/decimal.
#   - Vista previa y resumen de los datos.
# Los pasos 4-7 (limpieza, análisis, gráficas, descargas) están como
# secciones "por portar", que iremos llenando igual que hicimos en R.
#
# CÓMO EJECUTAR:
#   1. Instala las dependencias (ver requirements.txt):
#        pip install -r requirements.txt
#   2. Ejecuta la app:
#        shiny run --reload app.py
#   3. Abre en el navegador la dirección que muestra la terminal
#      (por defecto http://127.0.0.1:8000).
########################################################################

from __future__ import annotations

import io

import numpy as np
import pandas as pd
from shiny import App, reactive, render, req, ui

# ----------------------------------------------------------------------
# 1. Utilidades y datos de ejemplo
# ----------------------------------------------------------------------

PASOS = [
    "Bienvenida",
    "Cargar datos",
    "Vista previa",
    "Limpieza",
    "Análisis",
    "Graficar",
    "Descargar",
]
TOTAL_PASOS = len(PASOS)

VINO = "#7a1f3d"


def generar_datos_ejemplo() -> pd.DataFrame:
    """Estudio simulado de artritis reumatoide (AR) en un modelo murino,
    con 3 grupos evaluados a lo largo del tiempo. Equivalente al de la app R."""
    rng = np.random.default_rng(2024)
    n_por_grupo = 8
    grupos = ["Control", "AR", "AR+Tratamiento"]
    dias = [0, 7, 14, 21, 28]

    filas = []
    raton_id = 1
    info = []
    for g in grupos:
        for _ in range(n_por_grupo):
            info.append((raton_id, g))
            raton_id += 1

    for rid, g in info:
        for d in dias:
            if g == "Control":
                base = 0.05 * d
            elif g == "AR":
                base = 0.35 * d
            else:
                base = 0.15 * d
            puntaje = max(0, round(base + rng.normal(0, 1), 1))

            if g == "Control":
                base_w = 22.0
            elif g == "AR":
                base_w = 22.0 - 0.08 * d
            else:
                base_w = 22.0 - 0.03 * d
            peso = round(base_w + rng.normal(0, 0.8), 1)

            if g == "Control":
                base_il6 = 20.0
            elif g == "AR":
                base_il6 = 20.0 + 3.0 * d
            else:
                base_il6 = 20.0 + 1.2 * d
            il6 = round(max(5.0, base_il6 + rng.normal(0, 8)), 1)

            filas.append(
                {
                    "Raton_ID": rid,
                    "Grupo": g,
                    "Dia": d,
                    "Puntaje_clinico": puntaje,
                    "Peso_g": peso,
                    "IL6_pg_mL": il6,
                }
            )

    df = pd.DataFrame(filas)
    df["Grupo"] = pd.Categorical(df["Grupo"], categories=grupos, ordered=False)
    return df


def resumen_datos(df: pd.DataFrame) -> pd.DataFrame:
    """Resumen por variable (equivalente sencillo a summary() de R)."""
    filas = []
    for col in df.columns:
        s = df[col]
        if pd.api.types.is_numeric_dtype(s):
            filas.append(
                {
                    "Variable": col,
                    "Tipo": "numérica",
                    "n": int(s.notna().sum()),
                    "Faltantes": int(s.isna().sum()),
                    "Media": round(float(s.mean()), 2) if s.notna().any() else np.nan,
                    "Mínimo": round(float(s.min()), 2) if s.notna().any() else np.nan,
                    "Máximo": round(float(s.max()), 2) if s.notna().any() else np.nan,
                }
            )
        else:
            filas.append(
                {
                    "Variable": col,
                    "Tipo": "categórica / texto",
                    "n": int(s.notna().sum()),
                    "Faltantes": int(s.isna().sum()),
                    "Media": np.nan,
                    "Mínimo": np.nan,
                    "Máximo": np.nan,
                }
            )
    return pd.DataFrame(filas)


# ----------------------------------------------------------------------
# 2. Interfaz de usuario
# ----------------------------------------------------------------------

estilos = ui.tags.style(
    f"""
    body {{ background-color: #f7f8fa; }}
    .titulo-app {{
        background-color: {VINO}; color: white;
        padding: 18px 24px; border-radius: 6px; margin-bottom: 18px;
    }}
    .titulo-app h2 {{ margin: 0; font-weight: 600; }}
    .titulo-app p {{ margin: 4px 0 0 0; font-size: 14px; opacity: 0.9; }}
    .paso-barra {{ display: flex; justify-content: space-between;
                   margin-bottom: 22px; flex-wrap: wrap; }}
    .paso-item {{ flex: 1; text-align: center; padding: 8px 4px;
                  font-size: 12.5px; border-bottom: 4px solid #d9d9d9; color: #999; }}
    .paso-activo {{ border-bottom: 4px solid {VINO}; color: {VINO}; font-weight: 700; }}
    .paso-completo {{ border-bottom: 4px solid #b98ca0; color: {VINO}; }}
    .caja {{ background-color: white; padding: 24px; border-radius: 8px;
             box-shadow: 0 1px 4px rgba(0,0,0,0.08); margin-bottom: 20px; }}
    .ayuda {{ font-size: 12.5px; color: #666; font-style: italic; }}
    footer.pie {{ text-align: center; color: #999; font-size: 12px;
                  margin-top: 30px; margin-bottom: 10px; }}
    """
)

app_ui = ui.page_fluid(
    estilos,
    ui.div(
        ui.h2("Bonifaz-INCMNSZ"),
        ui.p(
            "Asistente de análisis de datos paso a paso — Instituto Nacional "
            "de Ciencias Médicas y Nutrición Salvador Zubirán"
        ),
        class_="titulo-app",
    ),
    ui.output_ui("barra_progreso"),
    ui.output_ui("cuerpo_asistente"),
    ui.tags.footer(
        "Bonifaz-INCMNSZ · Herramienta de apoyo para investigación clínica y básica "
        "(ej. modelos de artritis reumatoide en ratones) · No sustituye la asesoría "
        "de un bioestadístico.",
        class_="pie",
    ),
)


# ----------------------------------------------------------------------
# 3. Lógica del servidor
# ----------------------------------------------------------------------

def server(input, output, session):
    paso = reactive.value(1)
    datos_crudos = reactive.value(None)     # pd.DataFrame | None
    datos_limpios = reactive.value(None)    # pd.DataFrame | None

    def datos_actuales():
        return datos_limpios() if datos_limpios() is not None else datos_crudos()

    # ---- Navegación ----
    def ir_siguiente():
        paso.set(min(TOTAL_PASOS, paso() + 1))

    def ir_atras():
        paso.set(max(1, paso() - 1))

    @reactive.effect
    @reactive.event(input.btn_atras)
    def _():
        ir_atras()

    @reactive.effect
    @reactive.event(input.btn_siguiente)
    def _():
        # En el paso 2 exigimos que ya haya datos cargados
        if paso() == 2 and datos_actuales() is None:
            ui.notification_show(
                "Primero carga tus datos (usa el set de ejemplo o sube un archivo) "
                "antes de continuar.",
                type="warning",
                duration=6,
            )
            return
        ir_siguiente()

    # ---- Barra de progreso ----
    @render.ui
    def barra_progreso():
        items = []
        for i, nombre in enumerate(PASOS, start=1):
            if i == paso():
                clase = "paso-item paso-activo"
            elif i < paso():
                clase = "paso-item paso-completo"
            else:
                clase = "paso-item"
            items.append(ui.div(f"{i}. {nombre}", class_=clase))
        return ui.div(*items, class_="paso-barra")

    def botones_nav(mostrar_siguiente=True):
        izquierda = (
            ui.input_action_button("btn_atras", "← Atrás", class_="btn-default")
            if paso() > 1
            else ui.div()
        )
        derecha = (
            ui.input_action_button("btn_siguiente", "Siguiente →", class_="btn-primary")
            if mostrar_siguiente
            else ui.div()
        )
        return ui.div(
            izquierda,
            derecha,
            style="margin-top:20px; display:flex; justify-content:space-between;",
        )

    # ---- Cuerpo principal según el paso ----
    @render.ui
    def cuerpo_asistente():
        p = paso()

        if p == 1:
            return ui.div(
                ui.h3("¡Bienvenido(a)!"),
                ui.p(
                    "Esta aplicación te guiará, paso a paso, para analizar y graficar "
                    "tus datos sin necesidad de escribir código en Python."
                ),
                ui.p(
                    "Está pensada para investigadores del área clínica y de ciencias "
                    "básicas (por ejemplo, estudios con modelos murinos de artritis "
                    "reumatoide), pero funciona con cualquier tabla de datos (CSV o Excel)."
                ),
                ui.tags.ul(
                    ui.tags.li("Paso 1: Bienvenida (aquí estás)"),
                    ui.tags.li("Paso 2: Cargar tu archivo de datos"),
                    ui.tags.li("Paso 3: Revisar una vista previa"),
                    ui.tags.li("Paso 4: Limpiar los datos (opcional)"),
                    ui.tags.li("Paso 5: Elegir y correr un análisis estadístico"),
                    ui.tags.li("Paso 6: Generar gráficas"),
                    ui.tags.li("Paso 7: Descargar tus resultados"),
                ),
                botones_nav(),
                class_="caja",
            )

        if p == 2:
            return ui.div(
                ui.h3("¿Deseas subir un archivo?"),
                ui.p(
                    "Puedes usar el set de datos de ejemplo o subir tu propio archivo "
                    "(CSV o Excel). Cada columna debe representar una variable y cada "
                    "fila una observación."
                ),
                ui.input_radio_buttons(
                    "tipo_carga",
                    None,
                    {
                        "ejemplo": "Usar datos de ejemplo (artritis reumatoide en ratones)",
                        "propio": "Subir mi propio archivo (CSV o Excel)",
                    },
                    selected="ejemplo",
                ),
                ui.panel_conditional(
                    "input.tipo_carga === 'ejemplo'",
                    ui.input_action_button(
                        "cargar_ejemplo", "Cargar datos de ejemplo", class_="btn-info"
                    ),
                ),
                ui.panel_conditional(
                    "input.tipo_carga === 'propio'",
                    ui.input_file(
                        "archivo",
                        "Selecciona tu archivo (.csv, .xlsx o .xls)",
                        accept=[".csv", ".xlsx", ".xls"],
                        multiple=False,
                    ),
                    ui.row(
                        ui.column(
                            6,
                            ui.input_radio_buttons(
                                "csv_sep",
                                "Separador de columnas (solo CSV)",
                                {",": "Coma ( , )", ";": "Punto y coma ( ; )", "\t": "Tabulador"},
                                selected=",",
                            ),
                        ),
                        ui.column(
                            6,
                            ui.input_radio_buttons(
                                "csv_dec",
                                "Separador decimal (solo CSV)",
                                {".": "Punto ( . )", ",": "Coma ( , )"},
                                selected=".",
                            ),
                        ),
                    ),
                ),
                ui.output_ui("mensaje_carga"),
                botones_nav(),
                class_="caja",
            )

        if p == 3:
            df = datos_crudos()
            if df is None:
                return ui.div(
                    ui.h3("Vista previa de tus datos"),
                    ui.p("Aún no has cargado datos. Regresa al paso 2."),
                    botones_nav(),
                    class_="caja",
                )
            return ui.div(
                ui.h3("Vista previa de tus datos"),
                ui.p(
                    f"Tu archivo tiene {df.shape[0]} filas (observaciones) y "
                    f"{df.shape[1]} columnas (variables)."
                ),
                ui.output_data_frame("tabla_preview"),
                ui.h4("Resumen por variable"),
                ui.output_data_frame("tabla_resumen"),
                botones_nav(),
                class_="caja",
            )

        # Pasos 4-7: por portar desde la versión R
        titulos = {4: "Limpieza", 5: "Análisis", 6: "Graficar", 7: "Descargar"}
        return ui.div(
            ui.h3(f"{p}. {titulos.get(p, '')}"),
            ui.p(
                "Esta sección se portará desde la versión en R en los siguientes pasos "
                "del desarrollo. La estructura (navegación, estado y datos) ya está lista "
                "para recibirla."
            ),
            ui.p(
                f"Datos disponibles: "
                f"{'sí' if datos_actuales() is not None else 'no'}.",
                class_="ayuda",
            ),
            botones_nav(mostrar_siguiente=(p < TOTAL_PASOS)),
            class_="caja",
        )

    # ---- Carga de datos de ejemplo ----
    @reactive.effect
    @reactive.event(input.cargar_ejemplo)
    def _():
        datos_crudos.set(generar_datos_ejemplo())
        datos_limpios.set(None)
        ui.notification_show("Datos de ejemplo cargados.", type="message", duration=4)

    # ---- Carga de archivo propio ----
    @reactive.effect
    @reactive.event(input.archivo)
    def _():
        archivos = input.archivo()
        if not archivos:
            return
        info = archivos[0]
        ruta = info["datapath"]
        nombre = info["name"].lower()
        try:
            if nombre.endswith(".csv"):
                sep = input.csv_sep() if input.csv_sep() else ","
                dec = input.csv_dec() if input.csv_dec() else "."
                df = pd.read_csv(ruta, sep=sep, decimal=dec)
            elif nombre.endswith((".xlsx", ".xls")):
                df = pd.read_excel(ruta)
            else:
                ui.notification_show(
                    "Formato no soportado. Usa .csv, .xlsx o .xls", type="error"
                )
                return
        except Exception as e:  # noqa: BLE001
            ui.notification_show(f"Error al leer el archivo: {e}", type="error", duration=8)
            return

        datos_crudos.set(df)
        datos_limpios.set(None)

    # ---- Mensaje de confirmación de carga ----
    @render.ui
    def mensaje_carga():
        df = datos_crudos()
        if df is not None:
            return ui.div(
                f"✓ Datos cargados correctamente: {df.shape[0]} filas × {df.shape[1]} columnas.",
                style="color:#1a7a1a; margin-top:10px; font-weight:600;",
            )
        return ui.div(
            "Aún no se han cargado datos.", style="color:#888; margin-top:10px;"
        )

    # ---- Tablas del paso 3 ----
    @render.data_frame
    def tabla_preview():
        df = datos_crudos()
        req(df is not None)
        return render.DataGrid(df.head(50), height="320px", summary=False)

    @render.data_frame
    def tabla_resumen():
        df = datos_crudos()
        req(df is not None)
        return render.DataGrid(resumen_datos(df), summary=False)


app = App(app_ui, server)
