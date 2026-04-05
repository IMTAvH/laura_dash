safe_pull <- function(df, var) {
  if (is.null(var) || length(var) == 0 || !var %in% names(df)) {
    return(rep(NA, nrow(df)))
  }
  df[[var]]
}

parse_odk_datetime <- function(x) {
  if (length(x) == 0) return(as.POSIXct(rep(NA, 0), origin = "1970-01-01", tz = "UTC"))
  suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE))
}

to_yes_no <- function(x) {
  y <- stringr::str_to_lower(trimws(as.character(x)))
  dplyr::case_when(
    y %in% c("yes", "si", "sí", "1", "true") ~ "Sí",
    y %in% c("no", "0", "false", "") ~ "No",
    TRUE ~ NA_character_
  )
}

has_value <- function(x) {
  ifelse(is.na(x) | trimws(as.character(x)) == "", "No", "Sí")
}

make_missing_tbl <- function(df) {
  tibble::tibble(
    variable = names(df),
    n_missing = purrr::map_int(df, ~ sum(is.na(.x) | trimws(as.character(.x)) == "")),
    pct_missing = purrr::map_dbl(df, ~ mean(is.na(.x) | trimws(as.character(.x)) == ""))
  ) %>%
    dplyr::arrange(dplyr::desc(pct_missing)) %>%
    dplyr::mutate(
      pct_missing_label = scales::percent(pct_missing, accuracy = 0.1),
      alerta = dplyr::case_when(
        pct_missing == 0 ~ "OK",
        pct_missing < 0.2 ~ "Revisar",
        TRUE ~ "Alto"
      )
    )
}

plot_bar_safe <- function(df, var, xlab = NULL) {
  req(var %in% names(df))
  
  d <- df %>%
    mutate(.xvar = as.character(.data[[var]])) %>%
    count(.xvar, sort = TRUE)
  
  ggplot(d, aes(x = reorder(.xvar, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = ifelse(is.null(xlab), var, xlab), y = "N") +
    theme_minimal()
}

plot_time_safe <- function(df, var_date, xlab = "Fecha") {
  req(var_date %in% names(df))
  
  d <- df %>%
    mutate(fecha = as.Date(.data[[var_date]])) %>%
    filter(!is.na(fecha)) %>%
    count(fecha)
  
  # 👉 CASOS
  
  if (nrow(d) == 0) {
    plot.new()
    title("Sin datos disponibles")
    
  } else if (nrow(d) == 1) {
    ggplot(d, aes(x = fecha, y = n)) +
      geom_point(size = 3) +
      labs(x = xlab, y = "N") +
      theme_minimal()
    
  } else {
    ggplot(d, aes(x = fecha, y = n)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(x = xlab, y = "N") +
      theme_minimal()
  }
}

make_yes_count <- function(df, var) {
  if (!var %in% names(df)) return(NA_real_)
  mean(to_yes_no(df[[var]]) == "Sí", na.rm = TRUE)
}