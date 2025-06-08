library(shinymanager)

#### init the Sqlite Database
# Credentials data
credentials <- data.frame(
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_USER_PASS"), # password will automatically be hashed
  admin = c(T),
  stringsAsFactors = FALSE
)

# Create the database
create_db(
  credentials_data = credentials,
  sqlite_path = "db/database.sqlite", # will be created
  # passphrase = key_get("obiwankenobi")
  passphrase = Sys.getenv("SQL_DB_PASS")  # or just a word, without keyring
)
