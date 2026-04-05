mod_encuesta_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Encuesta nacional",
    
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("menstruates"), "Menstrúa cada mes", c("Todos", "Sí", "No")),
        selectInput(ns("pain"), "Dolor menstrual", c("Todos", "Sí", "No"))
      ),
      
      mainPanel(
        fluidRow(
          column(3, h3(textOutput(ns("n")))),
          column(3, h3(textOutput(ns("pct_mens")))),
          column(3, h3(textOutput(ns("pct_pain")))),
          column(3, h3(textOutput(ns("bleeding_days_mean"))))
        ),
        
        fluidRow(
          column(6, plotOutput(ns("time_plot"))),
          column(6, plotOutput(ns("regularity_plot")))
        ),
        
        fluidRow(
          column(6, plotOutput(ns("pain_plot"))),
          column(6, plotOutput(ns("pms_plot")))
        ),
        
        fluidRow(
          column(12, DTOutput(ns("table")))
        )
      )
    )
  )
}

mod_encuesta_server <- function(id, data, cfg) {
  moduleServer(id, function(input, output, session) {
    
    df0 <- reactive({
      req(nrow(data) > 0)
      x <- data %>%
        mutate(
          created_at = parse_odk_datetime(safe_pull(., cfg$created)),
          creator = safe_pull(., cfg$creator),
          age_num = suppressWarnings(as.numeric(safe_pull(., cfg$age))),
          menstruates_std = to_yes_no(safe_pull(., cfg$menstruates_monthly)),
          pain_std = to_yes_no(safe_pull(., cfg$pain)),
          bleeding_days_num = suppressWarnings(as.numeric(safe_pull(., cfg$bleeding_days))),
          regularity_std = as.character(safe_pull(., cfg$cycle_regularity)),
          bleeding_std = as.character(safe_pull(., cfg$bleeding_perception)),
          pain_intensity_std = as.character(safe_pull(., cfg$pain_intensity))
        )
      
      pms_ok <- cfg$pms_vars[cfg$pms_vars %in% names(x)]
      
      if (length(pms_ok) > 0) {
        x <- x %>%
          rowwise() %>%
          mutate(
            pms_count = sum(to_yes_no(c_across(all_of(pms_ok))) == "Sí", na.rm = TRUE)
          ) %>%
          ungroup()
      } else {
        x$pms_count <- NA_real_
      }
      
      x
    })
    
    df <- reactive({
      x <- df0()
      if (input$menstruates != "Todos") x <- x %>% filter(menstruates_std == input$menstruates)
      if (input$pain != "Todos") x <- x %>% filter(pain_std == input$pain)
      x
    })
    
    output$n <- renderText(nrow(df()))
    output$pct_mens <- renderText(scales::percent(mean(df()$menstruates_std == "Sí", na.rm = TRUE), accuracy = 0.1))
    output$pct_pain <- renderText(scales::percent(mean(df()$pain_std == "Sí", na.rm = TRUE), accuracy = 0.1))
    output$bleeding_days_mean <- renderText(round(mean(df()$bleeding_days_num, na.rm = TRUE), 1))
    
    output$time_plot <- renderPlot({
      plot_time_safe(df(), "created_at")
    })
    
    output$regularity_plot <- renderPlot({
      plot_bar_safe(df(), "regularity_std", "Regularidad del ciclo")
    })
    
    output$bleeding_plot <- renderPlot({
      plot_bar_safe(df(), "bleeding_std", "Percepción del sangrado")
    })
    
    output$pain_plot <- renderPlot({
      plot_bar_safe(df(), "pain_std", "Dolor menstrual")
    })
    
    output$pms_plot <- renderPlot({
      req("pms_count" %in% names(df()))
      
      d <- df() %>%
        filter(!is.na(pms_count))
      
      if (nrow(d) == 0) {
        plot.new()
        title("Sin variables PMS disponibles")
      } else if (nrow(d) == 1) {
        ggplot(d, aes(x = factor(pms_count))) +
          geom_bar() +
          labs(x = "Número de síntomas PMS reportados", y = "N") +
          theme_minimal()
      } else {
        ggplot(d, aes(x = pms_count)) +
          geom_histogram(binwidth = 1, boundary = 0, closed = "left") +
          labs(x = "Número de síntomas PMS reportados", y = "N") +
          theme_minimal()
      }

    })
    
    output$missing_tbl <- renderDT({
      datatable(make_missing_tbl(df()), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    })
    
    output$table <- renderDT({
      out <- df() %>%
        transmute(
          id = safe_pull(., cfg$id),
          edad = age_num,
          created_at,
          menstrua_cada_mes = menstruates_std,
          dias_sangrado = bleeding_days_num,
          regularidad = regularity_std,
          sangrado = bleeding_std,
          dolor = pain_std,
          intensidad_dolor = pain_intensity_std,
          n_sintomas_pms = pms_count
        )
      
      datatable(out, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
  })
}