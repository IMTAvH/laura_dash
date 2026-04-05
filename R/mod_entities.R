mod_entities_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("kpi_boxes")),
    br(),
    
    layout_sidebar(
      sidebar = sidebar(
        selectInput(ns("consent"), "Consentimiento", c("Todos", "Sí", "No")),
        selectInput(ns("complete"), "Complete EN", c("Todos", "Sí", "No")),
        open = "always",
        width = 280
      ),
      
      layout_column_wrap(
        width = 1/2,
        
        card(
          full_screen = TRUE,
          card_header("Registros en el tiempo"),
          card_body(
            plotOutput(ns("time_plot"), height = "300px")
          )
        ),
        
        card(
          full_screen = TRUE,
          card_header("Registros por creador"),
          card_body(
            plotOutput(ns("creator_plot"), height = "300px")
          )
        ),
        
        card(
          full_screen = TRUE,
          card_header("Disponibilidad de contacto"),
          card_body(
            plotOutput(ns("contact_plot"), height = "300px")
          )
        ),
        
        card(
          full_screen = TRUE,
          card_header("Resumen de datos faltantes"),
          card_body(
            DTOutput(ns("missing_tbl"))
          )
        ),
        
        card(
          full_screen = TRUE,
          card_header("Tabla de entidades"),
          card_body(
            DTOutput(ns("table"))
          )
        )
      )
    )
  )
}

mod_entities_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df0 <- reactive({
      req(nrow(data) > 0)
      
      data %>%
        dplyr::mutate(
          created_at   = parse_odk_datetime(safe_pull(., cfg$created)),
          creator      = safe_pull(., cfg$creator),
          age_num      = suppressWarnings(as.numeric(safe_pull(., cfg$age))),
          consent_std  = to_yes_no(safe_pull(., cfg$consent)),
          complete_std = to_yes_no(safe_pull(., cfg$complete)),
          has_phone    = has_value(safe_pull(., cfg$phone)),
          has_email    = has_value(safe_pull(., cfg$email))
        )
    })
    
    df <- reactive({
      x <- df0()
      
      if (input$consent != "Todos") {
        x <- x %>% dplyr::filter(consent_std == input$consent)
      }
      
      if (input$complete != "Todos") {
        x <- x %>% dplyr::filter(complete_std == input$complete)
      }
      
      x
    })
    
    output$kpi_boxes <- renderUI({
      d <- df()
      
      n_total <- nrow(d)
      pct_consent <- mean(d$consent_std == "Sí", na.rm = TRUE)
      pct_complete <- mean(d$complete_std == "Sí", na.rm = TRUE)
      age_mean <- mean(d$age_num, na.rm = TRUE)
      
      layout_column_wrap(
        width = 1/4,
        
        value_box(
          title = "Total de entidades",
          value = n_total,
          theme = "primary"
        ),
        
        value_box(
          title = "% con consentimiento",
          value = scales::percent(pct_consent, accuracy = 0.1),
          theme = "success"
        ),
        
        value_box(
          title = "% complete EN",
          value = scales::percent(pct_complete, accuracy = 0.1),
          theme = "info"
        ),
        
        value_box(
          title = "Edad promedio",
          value = if (is.nan(age_mean)) "NA" else round(age_mean, 1),
          theme = "warning"
        )
      )
    })
    
    output$time_plot <- renderPlot({
      plot_time_safe(df(), "created_at")
    })
    
    output$creator_plot <- renderPlot({
      plot_bar_safe(df(), "creator", "Creador")
    })
    
    output$contact_plot <- renderPlot({
      d <- tibble::tibble(
        indicador = c("Tiene teléfono", "Tiene email"),
        valor = c(
          mean(df()$has_phone == "Sí", na.rm = TRUE),
          mean(df()$has_email == "Sí", na.rm = TRUE)
        )
      )
      
      ggplot2::ggplot(d, ggplot2::aes(x = indicador, y = valor)) +
        ggplot2::geom_col() +
        ggplot2::scale_y_continuous(labels = scales::percent_format()) +
        ggplot2::labs(x = NULL, y = "%") +
        ggplot2::theme_minimal()
    })
    
    output$missing_tbl <- renderDT({
      DT::datatable(
        make_missing_tbl(df()),
        options = list(pageLength = 8, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    output$table <- renderDT({
      out <- df() %>%
        dplyr::transmute(
          id          = safe_pull(., cfg$id),
          age         = age_num,
          consent     = consent_std,
          complete_en = complete_std,
          creator     = creator,
          created_at  = created_at,
          phone       = safe_pull(., cfg$phone),
          email       = safe_pull(., cfg$email)
        )
      
      DT::datatable(
        out,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}