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
                                   #' I
                                   initialize = function(db_file) {
                                     private$db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_file)
                                     # check if db_file exists on drive

                                     # Check what tables are in the database

                                     # If the database is empty, create the necessary tables according to the schema
                                   },

                                   process_article = function(url) {
                                     # Scrape and process the article
                                     # This is just a placeholder for actual scraping logic
                                     scraped_data <- list(title = "Sample Article", content = "Sample Content")
                                     self$store_data(scraped_data)
                                   },

                                   store_data = function(article_data) {
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
