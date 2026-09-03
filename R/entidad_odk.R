# R/entidad_odk.R

library(dplyr)
library(tibble)
library(purrr)
library(tidyr)
library(jsonlite)

odk_flatten_df <- function(df, max_depth = 10) {
  # Aplana columnas tipo data.frame anidadas de forma recursiva, por ejemplo
  # __system, o grupos ODK anidados a varios niveles (grupo.subgrupo.campo).
  out <- as_tibble(df)
  depth <- 0

  repeat {
    nested_cols <- names(out)[purrr::map_lgl(out, is.data.frame)]
    if (length(nested_cols) == 0 || depth >= max_depth) break

    for (col in nested_cols) {
      nested <- as_tibble(out[[col]])
      out[[col]] <- NULL
      if (ncol(nested) == 0) next # grupo ODK vacío (solo notas/labels), no aporta columnas
      names(nested) <- paste0(col, ".", names(nested))
      out <- bind_cols(out, nested)
    }

    depth <- depth + 1
  }

  out
}

obtener_df_entidad_participantes <- function(
    dataset_name = Sys.getenv("ODK_ENTITY_NAME"),
    select = NULL,
    filter = NULL,
    top = NULL,
    skip = NULL,
    verbose = FALSE,
    flatten = TRUE
) {
  cfg <- odk_get_config()
  
  if (dataset_name == "") {
    stop("Falta ODK_ENTITY_NAME en variables de entorno.", call. = FALSE)
  }
  
  url <- paste0(
    cfg$base_url,
    "/v1/projects/", cfg$project_id,
    "/datasets/", dataset_name, ".svc/Entities"
  )
  
  query <- list()
  if (!is.null(select)) query[["$select"]] <- paste(select, collapse = ",")
  if (!is.null(filter)) query[["$filter"]] <- filter
  if (!is.null(top))    query[["$top"]]    <- top
  if (!is.null(skip))   query[["$skip"]]   <- skip
  
  data_list <- odk_get_json(url = url, query = query, cfg = cfg, verbose = verbose, simplify = TRUE)

  if (!"value" %in% names(data_list)) {
    stop("❌ La respuesta no contiene 'value'.", call. = FALSE)
  }

  df <- tibble::as_tibble(data_list$value)

  if (flatten) {
    df <- odk_flatten_df(df)
  }
  
  if (verbose) {
    message("✅ Entidades descargadas: ", nrow(df), " registros.")
  }
  
  df
}

