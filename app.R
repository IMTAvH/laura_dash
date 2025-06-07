library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinymanager)
library(httr)
library(jsonlite)

source("R/entidad_odk.R")

ui <- page_sidebar(
  
  theme = bs_theme(bootswatch = "minty", version = 5),

  fillable = TRUE, 
  
  sidebar = sidebar(
    open = T, # panel izquierdo abierto al iniciar shiny
    width = NULL,  # auto o usa "300px", "25%" si deseas tamaño fijo
    tags$div(
      style = "text-align: center;",
      tags$img(src = "laura_logo.jpg", height = "180px"),
      tags$h4("Proyecto Laura - Lima", style = "margin-top: 8px; font-weight: 750; color: #007195;")
    ),
    # tags$hr(),
    actionButton(
      "actualizar_datos",
      label = "Actualizar datos",
      icon = icon("sync"),
      class = "btn btn-outline-primary btn-sm"
    ),
    tags$div(
      style = "font-size: 12px; color: #555;",
      uiOutput("ultima_actualizacion")
    ),
    # br(),
    
  ),
  
  layout_column_wrap(
    fluidRow(
      column(4, value_box(
        title = "Inscritas",
        value = textOutput("inscritas"),
        showcase = icon("user"),
        style = "background-color: #78B8C0; color: white;"
        # theme_color = "primary"
      )),
      column(4, value_box(
        title = "Consentimientos Informados",
        value = textOutput("consentimientos_informados"),
        showcase = icon("file-signature"),
        style = "background-color: #3E91A5; color: white;"
        # theme_color = "primary"
      )),
      column(4, value_box(
        title = "Encuestas Nacionales completas",
        value = textOutput("encuestas_nacionales"),
        showcase = icon("clipboard-check"), 
        style = "background-color: #007195; color: white;"
        # theme_color = "primary"
      )),
      
      card(
        full_screen = TRUE,
        class = "mb-4",
        card_header("Evolución diaria"),
        card_body(
          plotlyOutput("grafico_inscripciones")
        )
      )
    )
  )
)

# Wrap your UI with secure_app
ui <- secure_app(ui = ui, enable_admin = T)

server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "db/database.sqlite",
      # passphrase = key_get("R-shinymanager-key", "obiwankenobi")
      passphrase = Sys.getenv("SQL_DB_PASS")
    )
  )
  
  datos <- reactiveVal()
  ultima_fecha <- reactiveVal()
  
  cargar_datos <- function() {
    df <- tryCatch({
      obtener_df_entidad_participantes()
    }, error = function(e) {
      message("Error al obtener los datos: ", e$message)
      
      set.seed(2)  # plantas la semilla
      
      n = 50 # cantidad de registros
      
      # Crear df con datos simulados
      tibble::tibble(
        `__id` = as.character(1:n),
        label = paste0("99", sample(100000000:999999999, n)),
        `__system` = data.frame(
          createdAt = format(
            as.POSIXct(runif(n, as.numeric(as.POSIXct("2025-06-04")), as.numeric(Sys.time())),
                       origin = "1970-01-01", tz = "UTC"),
            format = "%Y-%m-%dT%H:%M:%OSZ"
          ),
          creatorId = rep("137", n),
          creatorName = rep("Encuesta Nacional en Salud Femenina - Preregistro", n),
          updates = sample(0:5, n, replace = TRUE),
          updatedAt = rep(NA_character_, n),
          version = rep(1, n),
          conflict = rep(NA, n),
          stringsAsFactors = FALSE
        ),
        phone = paste0("9", sample(100000000:999999999, n)),
        long_id = replicate(n, paste(sample(c(0:9, letters[1:6]), 64, replace = TRUE), collapse = "")),
        email = paste0("correo", 1:n, "@example.com"),
        short_id = substr(long_id, 1, 6),
        consent = sample(c("yes", "no", ""), n, replace = TRUE),
        complete_p1 = sample(c("yes", "no", ""), n, replace = TRUE),
        complete_p2 = sample(c("yes", "no", ""), n, replace = TRUE),
        complete_p3 = sample(c("yes", "no", ""), n, replace = TRUE)
      )
    })
    datos(df)
    ultima_fecha(Sys.time())
  }
  
  observeEvent(input$actualizar_datos, {
    message("Botón presionado: actualizando datos...")
    
    withProgress(message = "Actualizando datos...", value = 0.3, {
      cargar_datos()
      incProgress(0.6)
      Sys.sleep(0.5)  # opcional, solo para que se vea la barra
    })
  })
  
    # Cargar datos al iniciar
  cargar_datos()
  
  output$ultima_actualizacion <- renderUI({
    if (is.null(ultima_fecha())) {
      HTML("Última actualización:<br><em>No disponible aún</em>")
    } else {
      HTML(paste0(
        "Última actualización:<br>",
        format(ultima_fecha(), "%d/%m/%Y"), "<br>",
        format(ultima_fecha(), "%H:%M:%S")
      ))
    }
  })
  
  # Value box: total de registros
  output$inscritas <- renderText({
    datos() %>% 
      nrow()
  })
  
  output$consentimientos_informados <- renderText({
    datos() %>% 
      filter(consent=="yes") %>% 
      nrow()
  })
  
  output$encuestas_nacionales <- renderText({
    datos() %>% 
      filter(consent=="yes",complete_p1=="yes",complete_p2=="yes",complete_p3=="yes") %>% 
      nrow()
  })
  
  output$grafico_inscripciones <- renderPlotly({
    df_datos <- datos()
    
    df_plot <- df_datos %>%
      mutate(
        fecha = as.POSIXct(`__system`$createdAt, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
        fecha = as.Date(format(fecha, tz = "America/Lima"))
      ) %>%
      mutate(
        consentimiento = ifelse(consent == "yes", 1, 0),
        completa = ifelse(consent == "yes" & complete_p1 == "yes" & complete_p2 == "yes" & complete_p3 == "yes", 1, 0)
      ) %>%
      group_by(fecha) %>%
      summarise(
        `Inscritas` = n(),
        `Consentimiento informado` = sum(consentimiento),
        `Encuesta nacional completa` = sum(completa),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(
        cols = c("Inscritas", "Consentimiento informado", "Encuesta nacional completa"),
        names_to = "tipo", values_to = "n"
      ) %>% 
      mutate(tipo = factor(tipo, levels = c(
        "Inscritas",
        "Consentimiento informado",
        "Encuesta nacional completa"
      )))
    
    p <- ggplot(df_plot, aes(x = fecha, y = n, color = tipo, linetype = tipo)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(x = "Fecha", y = "Cantidad", color = "", linetype = "") +
      theme_minimal() +
      scale_color_manual(
        values = c(
          "Inscritas" = "#78B8C0",                  # Azul institucional
          "Consentimiento informado" = "#3E91A5",   # Azul intermedio
          "Encuesta nacional completa" = "#007195"  # Azul claro
        )
      ) +
      scale_linetype_manual(
        values = c(
          "Inscritas" = "dashed",
          "Consentimiento informado" = "dashed",
          "Encuesta nacional completa" = "solid"
        )
      )
    
    ggplotly(p) %>% 
      layout(
        legend = list(
          orientation = "h",         # horizontal
          x = 0.5,                   # centro horizontal
          xanchor = "center",       
          y = -0.2                   # debajo del gráfico
        )
      )
  })
  
  # Render de la tabla
  # output$tabla <- renderDT({
  #   datatable(df, options = list(pageLength = 10), class = "table table-hover")
  # })
}

shinyApp(ui, server)