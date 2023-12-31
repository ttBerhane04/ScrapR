#' Retrieve News Items
#'
#' Retrieves news items from specified news outlets and topics, with options for headless browsing and handling cookie prompts.
#'
#' @param news_outlet A string specifying the news outlet, either "dr" or "tv2".
#' @param topic A string specifying the topic to retrieve news from. Valid topics depend on the news outlet.
#'  For dr.dk, valid topics are "indland", "udland", "penge", and "politik".
#'  For tv2.dk, valid topics are "krimi", "politik", "samfund", "udland", "business", "penge", "tech", and "klima".
#' @param last_crawl A Date object or string specifying the date of the last crawl to set a cut-off date for how far back to scrape.
#' @param headless_browsing Logical indicating if the browsing should be headless. Default is TRUE.
#' @param handle_cookie_prompt Logical indicating if the cookie prompt should be automatically handled. Default is TRUE.
#' @param verbose Logical indicating whether to print messages to the console.
#' @param debug_output Logical indicating whether to return additional object for debuging purposes
#' @return A data frame of news items retrieved from the specified news outlet and topic.
#'
#' @examples
#' \dontrun{
#' news_items <- retrieve_news_items("tv2", "klima", last_crawl = "2023-01-01", headless_browsing = FALSE)
#' }
#' @importFrom magrittr %>%
#' @export
#'
retrieve_news_items = function(news_outlet,
                               topic,
                               last_crawl = NULL,
                               headless_browsing = NULL,
                               handle_cookie_prompt = TRUE,
                               verbose = FALSE,
                               debug_output = FALSE) {

  # Make sure magrittr is loaded
  requireNamespace("magrittr", quietly = TRUE)

  # Set headless
  headless_browsing <- ifelse(is.null(headless_browsing), TRUE, headless_browsing)

  # set handle cookie prompt
  if (!is.logical(handle_cookie_prompt)) {
    # message user
    message("handle_cookie_prompt must be TRUE or FALSE. Setting handle_cookie_prompt to TRUE.")
    handle_cookie_prompt <- TRUE
  }

  # load an instance of the web driver
  remote_browser <- web_driver$new()

  # Initialize Rselenuim
  remote_browser$initialize_driver(headless = headless_browsing)

  if (news_outlet == "dr") {

    if (!topic %in% c('indland','udland', 'penge','politik')) {
      stop("Invalid topic. Please choose between 'indland','udland', 'penge','politik'.")
    }
    # Select news outlet and topic
    remote_browser$go_to_dr(topic, handle_cookie = handle_cookie_prompt)

  } else if (news_outlet == "tv2") {

    if (!topic %in% c('krimi', 'politik', 'samfund','udland', 'business', 'penge', 'tech', 'klima')) {
      stop("Invalid topic. Please choose between 'krimi', 'politik', 'samfund','udland', 'business', 'penge', 'tech', 'klima'.")
    }
    # Select news outlet and topic
    remote_browser$go_to_tv2(topic, handle_cookie = handle_cookie_prompt)
  }

  # If handle_cookie_prompt == FALSE, then use handle_cookie_consent method to handle cookie prompt with user input
  if (isFALSE(handle_cookie_prompt)) {
    remote_browser$handle_cookie_consent(user_prompt = TRUE)
  }

  # Get news articles
  news_items = remote_browser$get_news_articles(last_update = last_crawl, verbose_ = verbose, debug_ = debug_output)

  # Quit RSelenium
  remote_browser$quit_RSelenium()

  message(paste0("Finished crawling ", topic, " from ", toupper(news_outlet), ".\n\n"))

  return(news_items)

}
