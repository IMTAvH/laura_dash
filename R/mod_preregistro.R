mod_preregistro_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Pre-registro",
    
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("details_ok"), "Datos confirmados", c("Todos", "Sí", "No"))
      ),
      
      mainPanel(
        fluidRow(
          column(4, h3(textOutput(ns("n")))),
          column(4, h3(textOutput(ns("pct_ok")))),
          column(4, h3(textOutput(ns("age_mean"))))
        ),
        
        fluidRow(
          column(6, plotOutput(ns("time_plot"))),
          column(6, plotOutput(ns("region_plot")))
        ),
        
        fluidRow(
          column(12, DTOutput(ns("table")))
        )
      )
    )
  )
}

mod_preregistro_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    df0 <- reactive({
      req(nrow(data) > 0)
      data %>%
        mutate(
          created_at = parse_odk_datetime(safe_pull(., cfg$created)),
          updated_at = parse_odk_datetime(safe_pull(., cfg$updated)),
          age_num = suppressWarnings(as.numeric(safe_pull(., cfg$age))),
          region_std = as.character(safe_pull(., cfg$region)),
          details_ok_std = to_yes_no(safe_pull(., cfg$details_ok)),
          has_email = has_value(safe_pull(., cfg$email)),
          has_phone = has_value(safe_pull(., cfg$phone))
        )
    })
    
    output$region_ui <- renderUI({
      x <- sort(unique(na.omit(df0()$region_std)))
      selectInput(session$ns("region"), "Región", choices = c("Todas", x), selected = "Todas")
    })
    
    df <- reactive({
      x <- df0()
      if (input$details_ok != "Todos") x <- x %>% filter(details_ok_std == input$details_ok)
      if (!is.null(input$region) && input$region != "Todas") x <- x %>% filter(region_std == input$region)
      x
    })
    
    output$n <- renderText(nrow(df()))
    output$pct_ok <- renderText(scales::percent(mean(df()$details_ok_std == "Sí", na.rm = TRUE), accuracy = 0.1))
    output$age_mean <- renderText(round(mean(df()$age_num, na.rm = TRUE), 1))
    output$pct_email <- renderText(scales::percent(mean(df()$has_email == "Sí", na.rm = TRUE), accuracy = 0.1))
    
    output$time_plot <- renderPlot({
      plot_time_safe(df(), "created_at")
    })
    
    output$region_plot <- renderPlot({
      plot_bar_safe(df(), "region_std", "Región")
    })
    
    output$motivation_tbl <- renderDT({
      var_mot <- "participantes.motivacion"
      if (!var_mot %in% names(df())) {
        return(datatable(data.frame(mensaje = "No se encontró la variable 'motivacion'"), rownames = FALSE))
      }
      
      out <- df() %>%
        transmute(
          id = safe_pull(., cfg$id),
          edad = age_num,
          region = region_std,
          motivacion = .data[[var_mot]]
        )
      
      datatable(out, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
    })
    
    output$missing_tbl <- renderDT({
      datatable(make_missing_tbl(df()), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    })
    
    output$table <- renderDT({
      out <- df() %>%
        transmute(
          id = safe_pull(., cfg$id),
          edad = age_num,
          region = region_std,
          telefono = safe_pull(., cfg$phone),
          correo = safe_pull(., cfg$email),
          datos_confirmados = details_ok_std,
          created_at,
          updated_at
        )
      
      datatable(out, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
  })
}