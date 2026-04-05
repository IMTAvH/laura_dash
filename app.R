library(shiny)
library(DT)
library(bslib)
library(shinymanager)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(scales)
library(tidyr)
library(purrr)
library(reactable)
library(httr)
library(jsonlite)
library(tibble)

source("R/odk_auth.R")
source("R/entidad_odk.R")
source("R/formulario_odk.R")
source("R/utils.R")
source("R/mod_entities.R")
source("R/mod_preregistro.R")
source("R/mod_encuesta.R")

# =====================================================
# CONFIG
# =====================================================

tema_laura <- bs_theme(
  version = 5,
  
  # 🎨 Colores principales
  primary = "#2C7FB8",   # azul científico
  secondary = "#7FCDBB", # verde suave
  success = "#41AB5D",
  info = "#1D91C0",
  warning = "#FEB24C",
  danger = "#FC4E2A",
  
  # 🧱 Fondo
  bg = "#F8FAFC",
  fg = "#1F2937",
  
  # 🔤 Tipografía
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins"),
  
  # 📐 Estética moderna
  border_radius = "0.75rem",
  btn_border_radius = "0.75rem",
  
  # 📦 Espaciado
  spacer = "1rem"
)

cfg_entities <- list(
  id = "short_id",
  age = "age",
  consent = "consent",
  complete = "complete_EN",
  created = "__system.createdAt",
  updated = "__system.updatedAt",
  creator = "__system.creatorName",
  phone = "phone",
  email = "email"
)

cfg_preregistro <- list(
  id = "participantes.short_id",
  age = "participantes.edad",
  region = "participantes.region",
  phone = "participantes.mobile_phone",
  email = "participantes.correo",
  creator = "__system.submitterName",
  created = "__system.submissionDate",
  updated = NULL,
  details_ok = "participantes.are_details_correct"
)

cfg_encuesta <- list(
  id = "short_id",
  age = "Q1.1-age",
  created = "__system.submissionDate",
  creator = "__system.submitterName",
  menstruates_monthly = "Q7.2-menstruating_monthly",
  bleeding_days = "Q7.4-menstrual_bleeding_days",
  cycle_regularity = "Q7.5-menstrual_cycle_regularity",
  bleeding_perception = "Q7.7-bleeding_perception",
  pain = "Q7.13-menstruation_pain",
  pain_intensity = "Q7.13.1-menstruation_pain_intensity",
  pms_vars = c(
    "Q7.20.1-pms_irritability",
    "Q7.20.2-pms_anxiety",
    "Q7.20.3-pms_tearfulness",
    "Q7.20.4-pms_depressed_mood",
    "Q7.20.8-pms_concentration_difficulty",
    "Q7.20.9-pms_fatigue",
    "Q7.20.11-pms_insomnia",
    "Q7.20.14-pms_physical_symptoms"
  )
)

# =====================================================
# DESCARGA DE DATOS DESDE ODK
# =====================================================

# Entidades
df_entities <- obtener_df_entidad_participantes(
  dataset_name = Sys.getenv("ODK_ENTITY_NAME"),
  verbose = TRUE
)

# Formularios
df_preregistro <- obtener_df_formulario(
  xml_form_id = Sys.getenv("ODK_XML_FORM_ID_PREREGISTRO"),
  table = "Submissions",
  verbose = TRUE
)

df_encuesta <- obtener_df_formulario(
  xml_form_id = Sys.getenv("ODK_XML_FORM_ID_ENCUESTA"),
  table = "Submissions",
  verbose = TRUE
)


ui <- page_navbar(
  title = "Proyecto Laura - MVP",
  theme = tema_laura,

  nav_panel("Entidades", mod_entities_ui("entities")),
  nav_panel("Pre-registro", mod_preregistro_ui("preregistro")),
  nav_panel("Encuesta", mod_encuesta_ui("encuesta"))
)

ui <- secure_app(
  ui,
  theme = tema_laura,
  language = "es",
  enable_admin = TRUE,
  tags_top = tags$div(
    style = "text-align:center; padding:10px;",
    tags$h3("Proyecto Laura"),
    tags$p("Acceso restringido")
  )
)

server <- function(input, output, session) {
  # Autenticación con SQLite
  res_auth <- secure_server(
    check_credentials = check_credentials(
      db = "db/database.sqlite",
      passphrase = Sys.getenv("SQL_DB_PASS")
    )
  )
  mod_entities_server("entities", df_entities, cfg_entities)
  mod_preregistro_server("preregistro", df_preregistro, cfg_preregistro)
  mod_encuesta_server("encuesta", df_encuesta, cfg_encuesta)
}

shinyApp(ui, server)