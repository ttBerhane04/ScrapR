#' Extract News Elements from a Web Page Source
#'
#' This function extracts news-related HTML elements from the provided web page source,
#' specifically targeting Danish news outlets 'dr' and 'tv2'.
#' It uses XPath to identify relevant elements within the HTML structure.
#'
#' @param outlet A character string specifying the news outlet; accepts either 'dr' or 'tv2'.
#' @param page_source A character string or xml2 object representing the HTML source of a web page.
#'
#' @return An object of class `xml_nodeset` containing the extracted news elements.
#' If the page source is NULL or the outlet is not recognized, the function stops with an error message.
#'
#' @examples
#' \dontrun{
#' # Assuming valid HTML content in 'html_content' variable
#' get_news_elements(outlet = 'dr', page_source = html_content)
#'}
#'
#' @importFrom magrittr %>%
#' @importFrom rvest html_nodes
#' @importFrom xml2 read_html


get_news_elements = function(outlet = c('dr','tv2'), page_source = NULL) {
  # check if page_source is NULL
  if (is.null(page_source)) {
    stop("Error: page_source must be provided")
  }

  # Convert page_source to xml
  if (is.character(page_source)) {
    page_source <- xml2::read_html(page_source)
  }

  if (outlet == 'dr') {
    # get news elements from dr.dk
    news_elements = page_source %>%
      rvest::html_elements(xpath = "//div[contains(@class, 'dre-teaser-content')]")
  } else if (outlet == 'tv2') {
    # get news elements from tv2.dk
    news_elements = page_source %>%
      rvest::html_elements(xpath = "//article[contains(@class, 'tc_teaser')]")
  } else {
    stop("Error: outlet must be either 'dr' or 'tv2'")
  }
}
