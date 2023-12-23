# Define a class that contains a webdriver object
#' web_driver: An R6Class for Web Scraping and Navigation
#'
#' This class provides methods for web scraping and navigation using RSelenium.
#' It includes functionalities like initializing a web driver, navigating to URLs, handling cookie consents, and scraping news articles.
#'
#' @examples
#' \dontrun{
#' # load an instance of the web driver
#' remote_browser <- web_driver$new()
#'
#' # Initialize Rselenuim
#' remote_browser$initialize_driver(headless = FALSE)
#'
#' # Print to see what topics are available from tv2.dk
#' # print(remote_browser$tv2)
#'
#' # Select news outlet and topic
#' remote_browser$go_to_tv2('klima')
#'
#' # Get news articles
#' tv2_news_klima = remote_browser$get_news_articles(last_update = "2023-11-20")
#'
#' # Quit RSelenium
#' remote_browser$quit_RSelenium()
#' }
#'
#'
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @importFrom RSelenium rsDriver
#' @importFrom rvest html_elements html_text html_attr
#' @importFrom dplyr bind_rows if_else
#' @importFrom lubridate is.Date
#' @export

web_driver <- R6::R6Class("web_driver",
                          public = list(
                            #' @field rD Remote driver object.
                            rD = NULL,
                            #' @field server RSelenium server object.
                            server = NULL,
                            #' @field client RSelenium client object.
                            client = NULL,
                            #' @field headless Logical indicating if the browser is run in headless mode.
                            headless = NULL,
                            #' @field outlet Current news outlet being scraped.
                            outlet = NULL,
                            #' @field url_ Current URL navigated to.
                            url_ = NULL,
                            #' @field page_source HTML source of the current page.
                            page_source = NULL,
                            #' @field show_more_lgl Logical indicating if a 'show more' button was found.
                            show_more_lgl = NULL,
                            #' @field show_more_id ID of the 'show more' button.
                            show_more_id = NULL,
                            #' @field show_more_class Class of the 'show more' button.
                            show_more_class = NULL,
                            #' @field tv2 List of TV2 news categories and their URLs.
                            tv2 = list('krimi' = "https://nyheder.tv2.dk/krimi",
                                       'politik' = 'https://nyheder.tv2.dk/politik',
                                       'samfund' = "https://nyheder.tv2.dk/samfund",
                                       'udland' = "https://nyheder.tv2.dk/udland",
                                       'business' = "https://nyheder.tv2.dk/business",
                                       'penge' = "https://nyheder.tv2.dk/penge",
                                       'tech' = "https://nyheder.tv2.dk/tech",
                                       'klima' = "https://nyheder.tv2.dk/klima"),
                            #' @field dr List of DR news categories and their URLs.
                            dr = list('indland' = "https://www.dr.dk/nyheder/indland",
                                      'udland' = "https://www.dr.dk/nyheder/udland",
                                      'penge' = "https://www.dr.dk/nyheder/penge",
                                      'politik' = "https://www.dr.dk/nyheder/politik"),

                            #' @description
                            #' Initialize WebDriver
                            #'
                            #' This method initializes the RSelenium web driver in either headless or normal mode.
                            #' @param headless Logical, default is TRUE. Indicates whether to run the webdriver in headless mode.
                            #' @return None.
                            initialize_driver = function(headless = TRUE) {

                              # start RSelenium with specified browser
                              if (isTRUE(headless)) {
                                self$headless = headless
                                self$rD = RSelenium::rsDriver(browser = "firefox",
                                                              verbose = FALSE,
                                                              check = FALSE,
                                                              port = as.integer(netstat::free_port(random = TRUE)),
                                                              chromever = NULL,
                                                              extraCapabilities = list( "moz:firefoxOptions" = list(args = list('--headless')) ) ) }
                              else {
                                self$headless = FALSE
                                self$rD = RSelenium::rsDriver(browser = "firefox",
                                                              verbose = FALSE,
                                                              check = FALSE,
                                                              port = as.integer(netstat::free_port(random = TRUE)),
                                                              chromever = NULL ) }

                              # check if RSelenium is running
                              if (is.null(self$rD$client)) {
                                stop("RSelenium could not be started.")
                              }

                              # assign server and client (and set timeout for pages to load)
                              self$client <- self$rD$client
                              self$client$setTimeout(type = "page load", milliseconds = 10000)
                              self$server <- self$rD$server
                            },

                            #' @description
                            #' Quit RSelenium
                            #'
                            #' Shuts down the RSeleium server and closes the client.
                            #' @return None.
                            quit_RSelenium = function() {
                              # close server and client
                              if (!is.null(self$server)) {
                                self$client$close()
                                self$server$stop()
                              }
                            },

                            #' @description
                            #' Restart WebDriver
                            #'
                            #' Restarts the RSelenium web driver if it is not alive, maintaining the current URL.
                            #' @return None.
                            restart_driver = function() {
                              # check if driver is still alive
                              if (isTRUE(self$server$process$is_alive())) {
                                # if driver is still alive, open client
                                self$client$open()
                                # navigate to URL again
                                self$client$navigate(self$url_)
                              } else {
                                # if driver is not alive, re-initialize driver
                                self$initialize_driver()
                                # navigate to URL again
                                self$client$navigate(self$url_)
                              }



                            },

                            #' @description
                            #' Go to URL
                            #'
                            #' Navigates to a specified URL and checks for common HTTP error responses.
                            #' @param url The URL to navigate to.
                            #' @return None, but stops with a message if an error or invalid URL is encountered.
                            go_to_url = function(url) {
                              # Check if url is provided
                              if (is.null(url)) {
                                stop("No url provided.")
                              }

                              # Store url
                              self$url_ = url

                              # check site response for http error codes
                              response <- httr::GET(url)
                              # Check response for multiple error types
                              if (response$status_code == 404) {
                                stop("Page not found.")
                              } else if (response$status_code == 403) {
                                stop("Access denied.")
                              } else if (response$status_code == 500) {
                                stop("Internal server error.")
                              } else if (response$status_code == 503) {
                                stop("Service unavailable.")
                              } else if (response$status_code == 504) {
                                stop("Gateway timeout.")
                              }

                              # navigate to url
                              eval_action = tryCatch(self$client$navigate(self$url_),
                                                     error = function(e) e[["message"]])

                              # check for error
                              if (is(eval_action, "error")) {
                                # Try opening a new session from client
                                eval_open = tryCatch(self$client$open(),
                                                     error = function(e) e[["message"]])

                                # check for client error again
                                if (is(eval_open, "error")) {
                                  # restart RSelenium
                                  self$initialize_driver()
                                }

                                # navigate to url
                                eval_action = tryCatch(self$client$navigate(self$url_),
                                                       error = function(e) e[["message"]])

                                if(is(eval_action, "error")) {
                                  stop("Error navigating to url.")
                                }
                              }
                            },

                            #' @description
                            #' Navigate to a Category on DR.dk
                            #'
                            #' Navigates to a specified news category on DR.dk.
                            #' @param category The category to navigate to.
                            #' @param handle_cookie Logical; if TRUE, handles the cookie consent prompt.
                            #' @return None, but navigates to the specified category on DR.dk.
                            go_to_dr = function(category = NULL, handle_cookie = TRUE) {
                              if (!category %in% names(self$dr)) {
                                stop("Invalid category. To see categories use [web-driver]$dr or .$tv2 to explore list./n Available categories are: ", paste(names(self$dr), collapse = ", "))
                              }
                              self$outlet = "dr"
                              self$go_to_url(self$dr[[category]])

                              if (isTRUE(handle_cookie)) {
                                message("Handling cookie consent...")
                                self$handle_cookie_consent()
                              }
                            },

                            #' @description
                            #' Navigate to a Category on TV2.dk
                            #'
                            #' Navigates to a specified news category on TV2.dk.
                            #' @param category The category to navigate to.
                            #' @param handle_cookie Logical; if TRUE, handles the cookie consent prompt.
                            #' @return None, but navigates to the specified category on TV2.dk.
                            go_to_tv2 = function(category = NULL, handle_cookie = TRUE) {
                              if (!category %in% names(self$tv2)) {
                                stop("Invalid category. To see categories use [web-driver]$dr or .$tv2 to explore list./n Available categories are: ", paste(names(self$tv2), collapse = ", "))
                              }
                              self$outlet = "tv2"
                              self$go_to_url(self$tv2[[category]])

                              if (isTRUE(handle_cookie)) {
                                message("Handling cookie consent...")
                                self$handle_cookie_consent()
                              }

                            },

                            #' @description
                            #' Get HTML Content
                            #'
                            #' Retrieves the HTML content of the current page.
                            #' @param do_return Logical, default is FALSE. If TRUE, returns the page source as a character vector.
                            #' @return Character vector containing the page source if do_return is TRUE.
                            get_html = function(do_return = FALSE) {

                              # get page source
                              self$page_source = self$client$getPageSource()[[1]]

                              if (isTRUE(do_return)) {
                                return(self$page_source)
                              }
                            },

                            #' @description
                            #' Handle Cookie Consent
                            #'
                            #' Handles the cookie consent prompt on a webpage by either accepting or rejecting it.
                            #' @param using The method used to find the consent element (e.g., "class name").
                            #' @param value The value to search for in the consent element.
                            #' @param reject Logical; if TRUE, rejects the cookie consent. If FALSE, accepts it.
                            #' @param user_prompt Logical; if TRUE, prompts the user to accept or reject the cookie consent.
                            #' @return None, but prints a message indicating the action taken.
                            handle_cookie_consent = function(using = NULL, value = NULL, reject = FALSE, user_prompt = FALSE) {

                              # Check if using and value are provided
                              if (!is.null(using) && !is.null(value)) {
                                # Message user
                                message("Working on cookie consent prompt with user defined values...")
                              } else {
                                # message user
                                message("Working on cookie consent prompt...")

                                # Check which outlet is being used and set using and value accordingly
                                if (self$outlet == "dr") {
                                  search_method = "class name"
                                  search_value = "drCookieDialog"
                                } else if (self$outlet == "tv2") {
                                  search_method = "class name"
                                  search_value = "banner-actions-container"
                                  # search_method = "id"
                                  # search_value = "onetrust-group-container"# onetrust-button-group
                                }
                              }

                              # Set implicit and page load timeouts
                              self$client$setTimeout(type = "page load", milliseconds = 3000)
                              self$client$setTimeout(type = "implicit", milliseconds = 3000)

                              # Engage cookie consent prompt with click
                              div_elements = self$client$findElements(using = "css selector", value = "div[id *= 'one']")
                              Sys.sleep(.5)
                              div_elements[[1]]$clickElement()

                              # Get button elements
                              button_elements = self$client$findElements(using = "tag name", value = "button")

                              # if self$headless is TRUE
                              if (isFALSE(user_prompt)) {
                                # Set pattern to search for button based on reject argument
                                pattern <- if (isTRUE(reject)) c("reject", "decline", "chosen") else c("accept", "all")

                                for (element in button_elements) {
                                  # convert id and class to lower case
                                  id_lower <- tolower(element$getElementAttribute('id'))
                                  class_lower <- tolower(element$getElementAttribute('class'))

                                  # check if any of the pattern match the id or class
                                  if (any(sapply(pattern, grepl, x = id_lower)) || any(sapply(pattern, grepl, x = class_lower))) {

                                    # if match, click button and return TRUE
                                    if (handle_button_click(element)) {
                                      message("Cookie consent prompt handled.")
                                      return()
                                    }
                                  }
                                }
                                message("No matching button found. Visit the website and inspect the cookie consent prompt to find the correct id or class.")
                              # or else the user is prompted to select the correct button to handle cookie consent prompt
                              } else {

                                # Get button names
                                button_values <- lapply(button_elements, function(button) {
                                  list(
                                    id = button$getElementAttribute('id'),
                                    class_ = button$getElementAttribute('class')
                                  )
                                })

                                # Iterate over buttons and prompt user to select the correct button
                                for (i in 1:length(button_elements)) {

                                  # Prompt user to select the correct button
                                  message(paste0("Is this the correct button? (id: ", button_values[[i]]$id, ", class: ", button_values[[i]]$class_, ")"))

                                  # Get user input
                                  user_input <- readline(prompt = "Enter y/n: ")

                                  # If user input is y, click button and break loop
                                  if (tolower(user_input) == "y") {

                                    # Click button
                                    if (handle_button_click(button_elements[[i]])) {
                                      # Message user and Break loop
                                      message("Cookie consent prompt handled.")
                                      break
                                    } else {
                                      # Message user
                                      message("Cookie consent prompt not handled.\n Try the next button or inspect website")
                                    }
                                  }
                                }
                              }
                            },

                            #' @description
                            #' Locate 'Show More' Button
                            #'
                            #' Attempts to locate the 'Show More' button on a webpage using its class or ID.
                            #' @return None, but sets internal fields related to the 'Show More' button if found.
                            locate_show_more_button = function() {

                              # Set implicit and page load timeouts
                              self$client$setTimeout(type = "page load", milliseconds = 3000)
                              self$client$setTimeout(type = "implicit", milliseconds = 3000)

                              # Engage cookie consent prompt with click
                              div_elements = self$client$findElements(using = "css selector", value = "div[id *= 'one']")
                              Sys.sleep(.5)
                              div_elements[[1]]$clickElement()

                              # query page for button
                              view_more_button = query_page_for_button(remDr = self$client, query_pattern = c("more", "load"), query_attr = "class")

                              # Validate buttons existential state of being
                              if (isTRUE(view_more_button$click)) {
                                # set attribute_type
                                attr_type = view_more_button$button$attr

                                # if attribute_type is class set as class name
                                if (attr_type == "class") {
                                  attr_type = "class name"
                                }

                                # set attribute_value
                                attr_value = view_more_button$button$value


                                if (length(attr_value) > 0) {

                                  # locate button
                                  button_element = tryCatch(self$client$findElement(using = attr_type, value = attr_value), error = function(e) e)

                                  # Click button with handle_button_click() use it instead of query_page_for_button()
                                  # for added functionality that engages website
                                  outcome = tryCatch(handle_button_click(button_element), error = function(e) e)

                                  if (inherits(outcome, "try-error")) {
                                    button_element = tryCatch(self$client$findElement(using = attr_type, value = attr_value), error = function(e) e)

                                    if (inherits(button_element, "try-error")) {
                                      # prompt user to manually inspect page for button
                                      stop("Error: Please inspect page for button")
                                    }

                                    # if button is found, move button in view
                                    button_element$getElementLocationInView()

                                    # Engage page with click
                                    div_element = self$client$findElement(using = "tag name", value = "div")
                                    Sys.sleep(.1)
                                    div_element$clickElement()

                                    # click button with handle_button_click()
                                    outcome = tryCatch(handle_button_click(button_element), error = function(e) e)

                                    if (inherits(button_element, "try-error")) {
                                      # prompt user to manually inspect page for button
                                      stop("Error: Please inspect page for button")
                                    }

                                  }

                                  if (isTRUE(outcome)) {
                                    # Store button values and existance in public list
                                    self$show_more_lgl <- TRUE

                                    if (attr_type == "class name") {
                                      self$show_more_class <- view_more_button$button$value
                                    } else {
                                      self$show_more_id <- view_more_button$button$value
                                    }

                                    # message user
                                    message("Show more button found and clicked.")
                                  } else {
                                    message("Show more button found but not clicked.")
                                  }
                                }
                              } else {
                                # Give error and prompt user to inspect website
                                stop("Error: No show more button found. Please inspect website.")
                              }
                            },

                            #' @description
                            #' Set 'Show More' Button
                            #'
                            #' Manually sets the 'Show More' button's class or ID if it cannot be automatically located.
                            #' @param class_ The class of the 'Show More' button.
                            #' @param id The ID of the 'Show More' button.
                            #' @return None, but updates the internal fields for the 'Show More' button.
                            set_show_more_button = function(class_ = NULL, id = NULL) {

                              # Check if class_ and id are provided
                              if (is.null(class_) & is.null(id)) {
                                stop("Must provide either class or id/n If not then use locate_show_more_button")
                              }

                              # Check if class_ and id are provided
                              if (!is.null(class_)) {
                                # click button with suggested class
                                eval_click = click_button(remDr = self$client, button_class = class_)

                                # Check if eval_click is TRUE
                                if (isTRUE(eval_click)) {
                                  # set self$show_more_button to TRUE and store class
                                  self$show_more_class = class_
                                  self$show_more_lgl = TRUE
                                } else {
                                  self$show_more_lgl = FALSE
                                  print("Could not find show more button with suggested class")
                                  return()
                                }
                              }

                              if (is.null(id)) {
                                # click button with suggested class
                                eval_click = click_button(remDr = self$client, button_id = id)

                                # Check if eval_click is TRUE
                                if (isTRUE(eval_click)) {
                                  # set self$show_more_button to TRUE and store id
                                  self$show_more_id = id
                                  self$show_more_lgl = TRUE
                                } else {
                                  self$show_more_lgl = FALSE
                                  print("Could not find show more button with suggested id")
                                  return()
                                }
                              }
                            },

                            #' @description
                            #' Get News Articles
                            #'
                            #' Scrapes news articles from a specified news outlet and filters them based on a cutoff date.
                            #' @param last_update Date (in 'YYYY-MM-DD' format) indicating when the news outlet was last crawled; used as a cutoff for new articles.
                            #' @return A data frame with metadata and URLs of news articles that meet the criteria.
                            get_news_articles = function(last_update = NULL) {

                              # Check if last_update is NULL or a date
                              # if it is NULL cut_off_date is set to 14 days ago
                              if (is.null(last_update)) {
                                cut_off_date = Sys.Date() - 14
                              } else {
                                # Check if last_update is a date
                                if (!lubridate::is.Date(as.Date(last_update))) {
                                  stop("last_update must be a date in format 'YYYY-MM-DD'")
                                } else {
                                  cut_off_date = as.Date(last_update)
                                }
                              }

                              # If self$outlet is NULL give error message to run set_outlet
                              if (is.null(self$outlet)) {
                                stop("Must set outlet using select_news_outlet before crawling")
                              }

                              # if outlet is dr prompt user for a category
                              if (self$outlet == "dr") {

                                # get page source
                                self$get_html()

                                dr_source = self$page_source

                                # get news elements
                                dr_news_elements = get_news_elements(outlet = 'dr', page_source = dr_source)

                                # retrieve and format date from news elements to filter out previouse crawled articles
                                full_date_list = unlist(lapply(dr_news_elements, function(x) {
                                  date_list = x %>%
                                    rvest::html_elements(css = "div>span>span>span[class *= 'dre-teaser-meta__part']") %>%
                                    rvest::html_text()

                                  if (length(date_list) == 2) {
                                    dr_date = date_list[2]
                                  } else {
                                    if (grepl("\\d", date_list[1])) {
                                      dr_date = date_list[1]
                                    } else {
                                      dr_date = ""
                                    }
                                  }

                                  dr_date = format_date_from_time(dr_date)

                                }))

                                # filter out articles that are older than cut_off_date
                                dr_news_elements = dr_news_elements[full_date_list >= cut_off_date & !is.na(full_date_list)]

                                # retrieve news items from elements
                                dr_news_list = lapply(dr_news_elements, function(x) {
                                  dr_url = x %>%
                                    rvest::html_elements("a") %>%
                                    rvest::html_attr("href")

                                  dr_url = x %>%
                                    rvest::html_elements(css = "div>a[class *= 'dre-teaser-title']") %>%
                                    rvest::html_attr("href")

                                  # dr - headline
                                  dr_headline = x %>%
                                    rvest::html_elements(css = "a>span>span[class *= 'dre-title-text']") %>%
                                    rvest::html_text()

                                  # dr - topic
                                  dr_topic = x %>%
                                    rvest::html_elements(css = "div>span>span>span[class *= 'dre-teaser-meta__part--primary']") %>%
                                    rvest::html_text()

                                  # dr - date
                                  dr_date_list = x %>%
                                    rvest::html_elements(css = "div>span>span>span[class *= 'dre-teaser-meta__part']") %>%
                                    rvest::html_text()


                                  if (length(dr_date_list) == 2) {
                                    dr_date = dr_date_list[2]
                                  } else {
                                    if (grepl("\\d", dr_date_list[1])) {
                                      dr_date = dr_date_list[1]
                                    } else {
                                      dr_date = ""
                                    }
                                  }

                                  dr_date = format_date_from_time(dr_date)

                                  dr_news_list = list('url' = if (length(dr_url) == 0) NA else dr_url,
                                                      'headline' = if (length(dr_headline) == 0) NA else dr_headline,
                                                      'topic' = if (length(dr_topic) == 0) NA else dr_topic,
                                                      'date' = dr_date)
                                })

                                # convert list to data frame
                                dr_news_df = dplyr::bind_rows(dr_news_list)

                                # Check if all urls have "https://" in front of them or else insert "https://www.dr.dk"
                                dr_news_df$url = dplyr::if_else(!grepl("https://", dr_news_df$url), paste0("https://www.dr.dk", dr_news_df$url), dr_news_df$url )

                                # return data frame
                                return(dr_news_df)

                              }

                              # if outlet is tv2 prompt user for a category
                              if (self$outlet == 'tv2') {

                                # set tv2_source to page_source
                                tv2_source = self$get_html(do_return = TRUE)

                                # get news elements
                                tv2_news_elements = get_news_elements(outlet = 'tv2', page_source = tv2_source )

                                # retrieve and format date from news elements to filter out previous crawled articles
                                full_date_list = unlist(lapply(tv2_news_elements, function(x) {
                                  # tv2 - url
                                  tv2_date = x %>%
                                    rvest::html_elements(css = "a[class *= 'tc_teaser__link']") %>%
                                    rvest::html_attr("href") %>%
                                    grab_date_from_url(.)

                                  # if NA then set date to current date YYYY-MM-DD
                                  # This handles cases from tv2 play, where the date is not present in the html or URL
                                  if (is.na(tv2_date)) {
                                    tv2_date = as.character(Sys.Date()) # Messy way to enforce a consistent format for dates
                                  }

                                  return(tv2_date)

                                }))

                                # print full_date_list as vector - debugging purposes
                                # print(full_date_list)
                                # print(min(full_date_list))

                                # if cut-off date is before the earliest article, then click show more button and re-evaluate
                                if (min(full_date_list) > cut_off_date) {

                                  # Set date condition for while loop
                                  date_condition = min(full_date_list) > cut_off_date

                                  # while-loop counter
                                  counter = 0

                                  # while earliest article is before cut-off date keep clicking show more button
                                  while (date_condition) {

                                    # increment counter
                                    counter = counter + 1

                                    # if show more button is not set, then locate it
                                    if (is.null(self$show_more_lgl)) {

                                      # locate show more button
                                      self$locate_show_more_button()

                                    # if show more button is set, then click it
                                    } else if (isTRUE(self$show_more_lgl)) {

                                      for (i in 1:2) {
                                        # Scroll down to 300 pixels
                                        self$client$executeScript("window.scrollTo(0,300)")

                                        if (counter == 4) {
                                          # refresh page
                                          self$client$refresh()
                                        }

                                        # if counter is greater than 3 then sleep for 2 seconds and engage the page
                                        if (counter == 3) {
                                          div_load = self$client$findElement(using = 'css selector', value = "div[class *= 'tc_load']")
                                          if (!is.null(div_load)) {
                                            Sys.sleep(.2)
                                            div_load$clickElement()
                                          }

                                          Sys.sleep(2)

                                          # Scroll down to 600 pixels
                                          self$client$executeScript("window.scrollTo(0,600)")
                                        }

                                        # if show more button is set by class then locate it
                                        if (!is.null(self$show_more_class)) {

                                          # locate show more button and click it
                                          view_more_button = suppressMessages(query_page_for_button(remDr = self$client, query_pattern = self$show_more_class, query_attr = "class"))
                                          # if counter is greater than 3 then sleep for 2 seconds and engage the page
                                          if (counter > 4) {
                                            Sys.sleep(1)
                                            element <- self$client$findElement(using = 'css', value = paste0("button[class *= '",self$show_more_class,"']" ))
                                            Sys.sleep(runif(1, .1, .2))
                                            element$getElementLocationInView()
                                            Sys.sleep(runif(1, .1, .2))
                                            self$client$mouseMoveToLocation(webElement = element)
                                            # Sys.sleep(runif(1, .1, .2))
                                            # self$client$executeScript("arguments[0].click();", list(element))
                                          }

                                        # if show more button is set by class then locate it
                                        } else if (!is.null(self$show_more_id)) {

                                          # locate show more button and click it
                                          view_more_button = suppressMessages(query_page_for_button(remDr = self$client, query_pattern = self$show_more_id, query_attr = "id"))

                                          # if counter is greater than 3 then sleep for 2 seconds and engage the page
                                          if (counter > 4) {
                                            Sys.sleep(1)
                                            element <- self$client$findElement(using = 'css', value = paste0("button[class *= '",self$show_more_class,"']" ))
                                            Sys.sleep(runif(1, .1, .2))
                                            element$getElementLocationInView()
                                            Sys.sleep(runif(1, .1, .2))
                                            self$client$mouseMoveToLocation(webElement = element)
                                            # Sys.sleep(runif(1, .1, .2))
                                            # self$client$executeScript("arguments[0].click();", list(element))
                                          }
                                        } else {
                                          stop("No show more button found/n Please specify show more button class or id using set_show_more_button()")
                                        }
                                      }
                                    }

                                    ## Scroll to current bottom
                                    page_element = self$client$findElement(using = 'css selector', value = "body")
                                    page_element$sendKeysToElement(list(key = "end"))
                                    page_element$sendKeysToElement(list(key = "down_arrow"))

                                    Sys.sleep(runif(1, .2, .4))

                                    # get html
                                    self$get_html()

                                    # get news elements
                                    tv2_news_elements = get_news_elements(outlet = 'tv2', page_source = self$page_source)

                                    # update dates
                                    full_date_list = unlist(lapply(tv2_news_elements, function(x) {

                                      # tv2 - url
                                      tv2_date = x %>%
                                        rvest::html_elements(css = "a[class *= 'tc_teaser__link']") %>%
                                        rvest::html_attr("href")

                                      tv2_date = tv2_date %>%
                                        grab_date_from_url(.)

                                      # if NA then set date to current date YYYY-MM-DD
                                      if (is.na(tv2_date)) {
                                        tv2_date = as.character(Sys.Date()) # Messy way to enforce a consistent format for dates
                                      }

                                      return(tv2_date)

                                    }))

                                    # print full_date_list as vector
                                    #print(min(full_date_list))

                                    # Message user about dates if counter is 4
                                    if (counter %in% c(3,5,7,9)) {
                                      message("Still working on getting news items according to cut-off date\n",paste0("Current earliest article date: ", min(full_date_list), "\nCut-off date: ", cut_off_date))
                                    }


                                    # check if cut-off date is before the earliest article
                                    date_condition = min(full_date_list) > cut_off_date
                                  }
                                }

                                # # filter out articles that are older than cut_off_date
                                # tv2_news_elements = tv2_news_elements[full_date_list >= cut_off_date]

                                # get relevant news items
                                tv2_news_list = lapply(tv2_news_elements, function(x) {
                                  # tv2 - url
                                  tv2_url = x %>%
                                    rvest::html_elements(css = "a[class *= 'tc_teaser__link']") %>%
                                    rvest::html_attr("href")

                                  # tv2 - headline
                                  tv2_headline = x %>%
                                    rvest::html_elements(css = "[class *= 'tc_heading']") %>%
                                    rvest::html_text()

                                  # tv2 - topic
                                  tv2_topic = x %>%
                                    rvest::html_elements(css = "span[class *= 'tc_label--color-nyheder']") %>%
                                    rvest::html_text()

                                  # tv2 - date
                                  tv2_date = grab_date_from_url(tv2_url)

                                  tv2_news_list = list(url = if (length(tv2_url) == 0) NA else tv2_url,
                                                       headline = if (length(tv2_headline) == 0) NA else tv2_headline,
                                                       topic = if (length(tv2_topic) == 0) NA else tv2_topic,
                                                       date = tv2_date)
                                })

                                # convert to data frame
                                tv2_news_df = dplyr::bind_rows(tv2_news_list)

                                # return data frame
                                return(tv2_news_df)
                              }
                            }
                          )
)
