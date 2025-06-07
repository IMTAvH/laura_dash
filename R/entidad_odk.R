# R/entidad_odk.R
obtener_df_entidad_participantes <- function(verbose = F) {
  res <- GET(
    url = paste0(Sys.getenv("ODK_BASE_URL"), "v1/projects/", Sys.getenv("ODK_PROJECT_ID"), "/datasets/", Sys.getenv("ODK_ENTITY_NAME"), ".svc/Entities"),
    authenticate(Sys.getenv("ODK_USERNAME"), Sys.getenv("ODK_PASSWORD"), type = "basic"),
    accept("application/json")
  )
  if (status_code(res) == 200) {
    data_json <- content(res, as = "text", encoding = "UTF-8")
    data_list <- fromJSON(data_json)
    
    if (!"value" %in% names(data_list)) {
      stop("❌ La respuesta no contiene datos válidos.")
    }
    
    df <- as_tibble(data_list$value)
    if (verbose) message("✅ Datos descargados: ", nrow(df), " registros.")
    return(df)
  } else {
    stop("❌ Error HTTP ", status_code(res), ": ",
         content(res, as = "text", encoding = "UTF-8"))
  }
}

