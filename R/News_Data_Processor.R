#' @title News Data Processor
#' News Data Processor: A Class for Processing and storing News Data in a SQLite Database
#'
#' The R6 class `News_Data_Processor` is used to process and store news data in a SQLite database.
#' It creates a relational database to store contents of news articles, authors, quotes, and image captions.
#'
#' @importFrom R6 R6Class
#' @importFrom DBI dbConnect dbGetQuery dbWriteTable dbDisconnect

News_Data_Processor <- R6::R6Class("News_Data_Processor",
                                 private = list(
                                   db_connection = NULL
                                 ),
                                 public = list(
                                   #' @field path_to_db A character string specifying a user defined path to the database file.
                                   path_to_db = NULL,

                                   #' @description
                                   #' Initializes the News_Data_Processor object.
                                   #' If no path to the database file is specified, the database will be stored in the current working directory
                                   #' with an alert to the user about the destination.
                                   #' @param db_file A character string specifying the name of the database file.
                                   #' @param path_to_db A character string specifying a user defined path to the database file.
                                   #' @return An initialized News_Data_Processor object.
                                   initialize = function(db_file, path_to_db = NULL) {
                                     # check if path is specified
                                     if (is.null(path_to_db)) {
                                       message("The database will be stored in the current working directory: ", getwd())

                                       ## Save path to so it is available to other methods and in the next session ##

                                     } else {

                                       # check if path exists
                                       if (!dir.exists(dirname(path_to_db))) {
                                         # create directory if it does not exist
                                         dir.create(dirname(path_to_db), recursive = TRUE)
                                       } else {
                                         # if path_to_db is not null then append path to db_file
                                         if (!is.null(path_to_db)) {
                                           db_file <- paste0(path_to_db, db_file)
                                         }
                                       }
                                     }

                                     # append file extension if not present
                                     if (!grepl(".sqlite$", db_file)) {
                                       # append file extension
                                       db_file <- paste0(db_file, ".sqlite")
                                     }

                                     # Connect to the existing SQLite database
                                     private$db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_file)
                                     db_file = "test.sqlite"

                                     # Query the database for a list of all tables
                                     db_tables <- DBI::dbGetQuery(private$db_connection, "SELECT name FROM sqlite_master WHERE type='table'")

                                     # Check if SQLite database is empty
                                     if (length(tables$name) == 0) {
                                       # If database is empty, create tables
                                       # Define SQL statements for creating tables
                                       create_tables_sql <- c(
                                         "CREATE TABLE news_item (
                                          item_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          title TEXT,
                                          url TEXT,
                                          publication_date DATE,
                                          news_outlet TEXT,
                                          FOREIGN KEY(article_id) REFERENCES article(article_id)
                                        )",

                                        "CREATE TABLE article (
                                          article_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          item_id INTEGER,
                                          body_text TEXT,
                                          FOREIGN KEY(item_id) REFERENCES news_item(item_id)
                                        )",

                                        "CREATE TABLE author (
                                          author_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          article_id INTEGER,
                                          author_name TEXT,
                                          email TEXT UNIQUE
                                          FOREIGN KEY(article_id) REFERENCES article(article_id)
                                        )",

                                        "CREATE TABLE quote (
                                          quote_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          article_id INTEGER,
                                          quote_text TEXT,
                                          quote_author TEXT,
                                          FOREIGN KEY(article_id) REFERENCES article(article_id)
                                        )",

                                        "CREATE TABLE image_caption (
                                          caption_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          article_id INTEGER,
                                          caption_text TEXT,
                                          FOREIGN KEY(article_id) REFERENCES article(article_id)
                                        )",

                                        "CREATE TABLE organization (
                                          organization_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          organization_name TEXT UNIQUE
                                        )",

                                        "CREATE TABLE person (
                                          person_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          person_name TEXT
                                        )",

                                        "CREATE TABLE article_organization (
                                          article_organization_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          article_id INTEGER,
                                          organization_id INTEGER,
                                          FOREIGN KEY(article_id) REFERENCES article(article_id),
                                          FOREIGN KEY(organization_id) REFERENCES organization(organization_id)
                                        )",

                                        "CREATE TABLE article_person (
                                          article_person_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          article_id INTEGER,
                                          person_id INTEGER,
                                          FOREIGN KEY(article_id) REFERENCES article(article_id),
                                          FOREIGN KEY(person_id) REFERENCES person(person_id)
                                        )",

                                        "CREATE TABLE caption_organization (
                                          caption_organization_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          caption_id INTEGER,
                                          organization_id INTEGER,
                                          FOREIGN KEY(caption_id) REFERENCES image_caption(caption_id),
                                          FOREIGN KEY(organization_id) REFERENCES organization(organization_id)
                                        )",

                                        "CREATE TABLE caption_person (
                                          caption_person_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          caption_id INTEGER,
                                          person_id INTEGER,
                                          FOREIGN KEY(caption_id) REFERENCES image_caption(caption_id),
                                          FOREIGN KEY(person_id) REFERENCES person(id)
                                        )",

                                        "CREATE TABLE quote_organization (
                                          quote_organization_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          quote_id INTEGER,
                                          organization_id INTEGER,
                                          FOREIGN KEY(quote_id) REFERENCES quote(quote_id),
                                          FOREIGN KEY(organization_id) REFERENCES organization(organization_id)
                                        )",

                                        "CREATE TABLE quote_person (
                                          quote_person_id INTEGER PRIMARY KEY AUTOINCREMENT,
                                          quote_id INTEGER,
                                          person_id INTEGER,
                                          FOREIGN KEY(quote_id) REFERENCES quote(quote_id),
                                          FOREIGN KEY(person_id) REFERENCES person(id)
                                        )"
                                       )

                                       system.time({
                                         # Execute each SQL command to create tables
                                         purrr::walk(create_tables_sql, DBI::dbExecute, conn = private$db_connection)
                                       })

                                       } else {

                                         # Define table names that are required for the database
                                         required_table_names = c("article", "author", "quote", "image_caption", "organization", "person", "article_organization",
                                                                  "article_person", "caption_organization", "caption_person", "quote_organization", "quote_person",
                                                                  "news_item")

                                         # Check if all required tables exist
                                         if (!all(required_table_names %in% db_tables$name)) {
                                           # FIGURE OUT WHAT TO DO IF ALL TABLES ARE NOT PRESENT
                                         }
                                       #   # If tables exist, ask user if they want to overwrite the existing tables
                                       #   overwrite_tables <- readline("The database already contains tables. Do you want to overwrite the existing tables? (y/n) ")
                                       #   if (overwrite_tables == "y") {
                                       #     # If user wants to overwrite tables, drop all tables
                                       #     DBI::dbExecute(db_connection, "DROP TABLE IF EXISTS article")
                                       #     DBI::dbExecute(db_connection, "DROP TABLE IF EXISTS author")
                                       #     DBI::dbExecute(db_connection, "DROP TABLE IF EXISTS quote")
                                       #     DBI::dbExecute(db_connection, "DROP TABLE IF EXISTS image_caption")
                                       #     DBI::dbExecute(db_connection, "DROP TABLE IF EXISTS organization")
                                       #     DBI::dbExecute(db_connection, "DROP TABLE IF EXISTS person")
                                       #     DBI::dbExecute(db_connection, "DROP TABLE IF EXISTS article_organization")
                                       #     DBI::dbExecute(db_connection, "DROP TABLE IF EXISTS article_person")
                                       #   # If user does not want to overwrite tables, stop the script
                                       #   stop("The script was stopped by the user.")
                                       # }
                                     }

                                   },

                                   process_article = function(url) {
                                     # Scrape and process the article
                                     # This is just a placeholder for actual scraping logic
                                     scraped_data <- list(title = "Sample Article", content = "Sample Content")
                                     self$store_data(scraped_data)
                                   },

                                   store_data = function(article_data, news_item_data, author_data, quote_data, image_caption_data) {
                                     # Store data in the database
                                     DBI::dbWriteTable(private$db_connection, "articles", article_data, append = TRUE)
                                   },



                                   finalize = function() {
                                     # Close the database connection when the object is garbage collected
                                     DBI::dbDisconnect(private$db_connection)
                                   }
                                 )
)

# Usage
processor <- NewsDataProcessor$new("news_data.sqlite")
processor$process_article("http://example.com/news-article-1")
processor$process_article("http://example.com/news-article-2")
# The connection will be closed automatically when the object is garbage collected
