# R/formulario_odk.R

library(dplyr)
library(tibble)
library(jsonlite)
library(purrr)

obtener_df_formulario <- function(
    xml_form_id,
    table = "Submissions",
    select = NULL,
    filter = NULL,
    top = NULL,
    skip = NULL,
    verbose = FALSE,
    flatten = TRUE
) {
  cfg <- odk_get_config()
  
  if (missing(xml_form_id) || is.null(xml_form_id) || xml_form_id == "") {
    stop("Debes indicar xml_form_id.", call. = FALSE)
  }
  
  url <- paste0(
    cfg$base_url,
    "/v1/projects/", cfg$project_id,
    "/forms/", xml_form_id, ".svc/", table
  )
  
  query <- list()
  if (!is.null(select)) query[["$select"]] <- paste(select, collapse = ",")
  if (!is.null(filter)) query[["$filter"]] <- filter
  if (!is.null(top))    query[["$top"]]    <- top
  if (!is.null(skip))   query[["$skip"]]   <- skip
  
  data_list <- odk_get_json(url = url, query = query, cfg = cfg, verbose = verbose)
  
  if (!"value" %in% names(data_list)) {
    stop("❌ La respuesta no contiene 'value'.", call. = FALSE)
  }
  
  df <- tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(data_list$value), flatten = FALSE))
  
  if (flatten) {
    df <- odk_flatten_df(df)
  }
  
  if (verbose) {
    message("✅ Formulario ", xml_form_id, " descargado: ", nrow(df), " registros.")
  }
  
  df
}