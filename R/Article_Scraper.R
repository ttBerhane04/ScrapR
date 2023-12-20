#' Article_Scraper: A class for scraping the contents of articles from Danish news outlets
#'
#' This class retrieves information and text from articles on dr.dk and tv2.dk.
#' It
#'
#' @importFrom rvest html_elements html_element html_text
#' @importFrom magrittr %>%
#' @export


Article_Scraper <- R6::R6Class(
  classname = "Article_Scraper",
  public = list(
    #' @field soup An xml2 object representing the HTML source of a web page.
    soup = NULL,
    #' @field news_outlet A character string specifying the news outlet; accepts either 'dr' or 'tv2'.
    news_outlet = NULL,

    #' @description
    #' Initializes the Article_Scraper object.
    #' @param soup An xml2 object representing the HTML source of a web page.
    #' @param news_outlet A character string specifying the news outlet; accepts either 'dr' or 'tv2'.
    #' @return An initialized Article_Scraper object.
    initialize = function(soup, news_outlet) {
      self$soup <- soup
      self$news_outlet <- news_outlet
    },

    #' @description
    #' Extracts the article body text
    #' @return A character string containing the article body text.
    get_article_text = function() {

      if (self$news_outlet == "dr") {

        # Parse article body
        body_element <- self$soup %>% rvest::html_elements("div.dre-speech")

        if (length(body_element) > 0) {
          article_text <- body_element %>% rvest::html_text(trim = TRUE)
        }
      } else if (self$news_outlet == "tv2") {

        # Parse article body
        body_element <- self$soup %>% rvest::html_elements("div.tc_richcontent") %>% rvest::html_elements("p")

        if (length(body_element) > 0) {
          article_text <- body_element  %>% rvest::html_text()
        }
      }

      if (length(article_text) > 0) {
        article_text <- article_text %>% paste(collapse = " ")
      } else {
        article_text <- character(0)
      }
      return(article_text)
    },

    #' @description
    #' Extracts quotes from the article where text is emphazised and the author is specified.
    #' @return A list of lists containing the quote text and author.
    get_article_quotes = function() {
      # set empty list for quotes
      quote_list <- list()

      if (self$news_outlet == "dr") {
        # retrieve quote elements from article
        quote_elements <- self$soup %>% rvest::html_elements("blockquote.dre-block-quote")

        if (length(quote_elements) > 0) {
          # loop through quote elements
          for (element in quote_elements) {
            text_element <- element %>% rvest::html_element("div.dre-block-quote__body")
            cite_element <- element %>% rvest::html_element("cite.dre-block-quote__author")
            quote_text <- if (!is.null(text_element)) rvest::html_text(text_element, trim = TRUE) else ""
            quote_author <- if (!is.null(cite_element)) rvest::html_text(cite_element, trim = TRUE) else ""
            quote_list[[length(quote_list) + 1]] <- list(quote_text = quote_text, quote_author = quote_author)
          }
        }
      } else if (self$news_outlet == "tv2") {
        # retrieve quote elements from article
        quote_elements <- self$soup %>% rvest::html_elements("blockquote")

        if (length(quote_elements) > 0) {
          # loop through quote elements
          for (element in quote_elements) {
            text_element <- element %>% rvest::html_element("p")
            cite_element <- element %>% rvest::html_element("cite")
            quote_text <- if (!is.null(text_element)) rvest::html_text(text_element, trim = TRUE) else ""
            quote_author <- if (!is.null(cite_element)) rvest::html_text(cite_element, trim = TRUE) else ""
            quote_list[[length(quote_list) + 1]] <- list(quote_text = quote_text, quote_author = quote_author)
          }
        }
      }
    },

    #' @description
    #' Extracts image captions from the article.
    #' @return A character vector containing the image captions.
    get_article_image_captions = function() {
      article_captions <- character(0)

      if (self$news_outlet == "dr") {

        caption_elements <- self$soup %>% rvest::html_elements("span.dre-caption")

        if (length(caption_elements) > 0) {
          article_captions <- caption_elements %>% rvest::html_text(trim = TRUE)
        }
      } else if (self$news_outlet == "tv2") {

        caption_elements <- self$soup %>% rvest::html_elements("figcaption.tc_caption")

        if (length(caption_elements) > 0) {
          article_captions <- caption_elements %>% rvest::html_text(trim = TRUE)
        }
      }
      return(article_captions)
    },

    #' @description
    #' Extracts the article author(s).
    #' @return A character vector containing the article author(s).
    get_author = function() {

      author <- character(0)

      if (self$news_outlet == "dr") {

        author_element <- self$soup %>% rvest::html_elements("a.dre-byline__contribution-name")

        if (length(author_element) > 0) {
          author <- author_element %>% rvest::html_element("span") %>% html_text(trim = TRUE)
        }
      } else if (self$news_outlet == "tv2") {

        author_element <- self$soup %>% rvest::html_elements("span.tc_byline__author__info")

        if (length(author_element) > 0) {
          author <- author_element %>% rvest::html_element("strong") %>% html_text(trim = TRUE)
        }
      }

      return(author)
    }
  )
)

