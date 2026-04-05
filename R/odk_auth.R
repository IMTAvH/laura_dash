# R/odk_auth.R

library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(purrr)
library(stringr)

odk_get_config <- function() {
  cfg <- list(
    base_url   = Sys.getenv("ODK_BASE_URL"),
    project_id = Sys.getenv("ODK_PROJECT_ID"),
    username   = Sys.getenv("ODK_USERNAME"),
    password   = Sys.getenv("ODK_PASSWORD")
  )
  
  missing <- names(cfg)[cfg == ""]
  if (length(missing) > 0) {
    stop("Faltan variables de entorno ODK: ", paste(missing, collapse = ", "))
  }
  
  cfg$base_url <- sub("/+$", "", cfg$base_url)
  cfg
}

odk_auth <- function(cfg = odk_get_config()) {
  authenticate(cfg$username, cfg$password, type = "basic")
}

odk_accept_json <- function() {
  accept("application/json")
}

odk_stop_for_status <- function(res) {
  sc <- status_code(res)
  if (sc >= 200 && sc < 300) return(invisible(res))
  
  body_txt <- tryCatch(
    content(res, as = "text", encoding = "UTF-8"),
    error = function(e) "<sin cuerpo de respuesta>"
  )
  
  stop("❌ Error HTTP ", sc, ": ", body_txt, call. = FALSE)
}

odk_get_json <- function(url, query = list(), cfg = odk_get_config(), verbose = FALSE) {
  if (verbose) message("GET ", url)
  
  res <- GET(
    url = url,
    odk_auth(cfg),
    odk_accept_json(),
    query = query
  )
  
  odk_stop_for_status(res)
  
  txt <- content(res, as = "text", encoding = "UTF-8")
  fromJSON(txt, simplifyVector = FALSE)
}