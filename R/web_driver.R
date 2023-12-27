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
                            #' @field topic Current topic being scraped.
                            topic = NULL,
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
                              self$topic = category
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
                            #' @param attr_type Str value of either 'class' or 'id'.
                            #' @param attr_value char string used to query html for button.
                            #' @return None, but updates the internal fields for the 'Show More' button.
                            set_show_more_button = function(attr_type = NULL, attr_value = NULL) {

                              # Check if class_ and id are provided
                              if (is.null(attr_type) || is.null(attr_value)) {
                                stop("Must provide both attri_type ('class' or 'id') and attr_value (char string used to query html for button).\n\nIf not then use locate_show_more_button")
                              }

                              # Validate attr_type
                              if (attr_type != "class" & attr_type != "id") {
                                stop("attr_type must be either 'class' or 'id'")
                              }

                              if (grepl("class", attr_type)) {
                                attr_type = "class name"
                              }

                              if (grepl("id", attr_type)) {
                                attr_type = "id"
                              }

                              evaluate_button = query_show_more_button(remDr = self$client, query_pattern = attr_value, query_attr = attr_type)

                              # Check if evaluate_button is TRUE
                              if (isTRUE(evaluate_button$click)) {
                                # set self$show_more_button to TRUE and store class
                                self$show_more_class = class_
                                self$show_more_lgl = TRUE
                              } else {
                                self$show_more_lgl = FALSE
                                print("Could not find show more button from user input")
                                return()
                              }

                            },

                            #' @description
                            #' Get News Articles
                            #'
                            #' Scrapes news articles from a specified news outlet and filters them based on a cutoff date.
                            #' @param last_update Date (in 'YYYY-MM-DD' format) indicating when the news outlet was last crawled; used as a cutoff for new articles.
                            #' @param verbose_ Logical indicating whether to print messages to the console.
                            #' @param debug_ Logical indicating whether to return additional object for debuging purposes
                            #' @return A data frame with metadata and URLs of news articles that meet the criteria.
                            get_news_articles = function(last_update = NULL, verbose_ = FALSE, debug_ = FALSE) {

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

                                # Get news elements
                                dr_news_elements <- get_news_elements_DR(page_source = self$page_source)

                                # Extract and format dates for each news element
                                news_article_dates <- extract_and_format_dates_DR(list_of_elements = dr_news_elements)

                                # Filter out articles older than cut_off_date
                                filtered_elements <- dr_news_elements[news_article_dates >= cut_off_date & !is.na(news_article_dates)]

                                # Process filtered news elements into a structured list
                                dr_news_list <- process_news_elements_DR(html_elements = filtered_elements)
                                # convert list to data frame
                                dr_news_df = dplyr::bind_rows(dr_news_list)

                                # Check if all urls have "https://" in front of them or else insert "https://www.dr.dk"
                                dr_news_df$url = dplyr::if_else(!grepl("https://", dr_news_df$url), paste0("https://www.dr.dk", dr_news_df$url), dr_news_df$url )

                                # return data frame
                                return(dr_news_df)

                              }

                              # if outlet is tv2 prompt user for a category
                              if (self$outlet == 'tv2') {

                                # Get page html
                                self$get_html()

                                # get news elements
                                tv2_news_elements = get_news_elements_TV2(page_source = self$page_source)

                                # Extract and format dates for each news element
                                news_article_dates <- extract_and_format_dates_TV2(list_of_elements = tv2_news_elements)

                                # Create table of dates
                                count_table = table(news_article_dates)

                                if (isTRUE(verbose_)) {
                                  message("Number of articles per date:\n")
                                  print(count_table)
                                  print(news_article_dates)
                                }

                                # Evaluate condition for while loop
                                while_loop_condition <- simple_count_comparison(count_table = count_table, target_date = cut_off_date, count_tolerance = 1, time_window = 3)

                                # if verbose_ is TRUE then print while loop condition to user
                                if (isTRUE(verbose_)) {
                                  message("While loop condition: ", while_loop_condition)
                                  print(count_table)
                                }

                                # Start list for count tables
                                historical_counts = list(count_table)

                                # while-loop counter
                                counter = 0

                                # Set seq for when to do stuff and message user
                                every_third_seq = seq(0, 40, by = 3)

                                # Set random seq for other actions
                                random_seq = sample(1:30, 25, replace = FALSE)
                                random_seq_b = sample(1:30, 20, replace = FALSE)

                                message("Now working on getting news items according to last update")

                                # while earliest article is before cut-off date keep clicking show more button
                                while (while_loop_condition) {

                                  # if show more button is not set, then locate it
                                  if (is.null(self$show_more_lgl)) {

                                    # locate show more button
                                    self$locate_show_more_button()

                                    # if show more button is set, then click it
                                  } else if (isTRUE(self$show_more_lgl)) {

                                    # Scroll down to 300 pixels
                                    self$client$executeScript("window.scrollTo(0,300)")

                                    # if counter is  3 then sleep for .2 seconds, engage the page and increase RSelenium implicit waits
                                    if (counter %in% c(5, 10, 15, 20, 30)) {

                                      # if counter is 5 then increase implicit wait to 4 seconds
                                      if (counter == 5) {
                                        # Set increased implicit wait
                                        implicit_wait = 4000
                                      } else {
                                        # increase implicit wait by 1-2 seconds
                                        implicit_wait = implicit_wait + runif(1, 1200, 2020)
                                      }

                                      # set implicit timeouts to 5 seconds respectively
                                      self$client$setTimeout(type = "implicit", milliseconds = implicit_wait)
                                      div_load = self$client$findElement(using = 'css selector', value = "div[class *= 'tc_load']")
                                      if (!is.null(div_load)) {
                                        Sys.sleep(.2)
                                        div_load$clickElement()
                                      }

                                      Sys.sleep(2)

                                      # Scroll down to 600 pixels
                                      self$client$executeScript("window.scrollTo(0,600)")
                                    }

                                    # if counter is 4, 7 or 10 then pause for 6-10 seconds
                                    if (counter %in% random_seq) {
                                      pause = runif(1, 8, 14)
                                      if (isTRUE(verbose_)) {
                                        message(paste0("Pausing for ", pause, " seconds.\nHoping that the page will load."))
                                      }
                                      Sys.sleep(pause)
                                    }

                                    # if show more button is set by class then locate it
                                    if (!is.null(self$show_more_class)) {

                                      # if verbose is TRUE then print message
                                      if (isTRUE(verbose_)) {
                                        # locate show more button and click it
                                        view_more_button = query_page_for_button(remDr = self$client, query_pattern = self$show_more_class, query_attr = "class")
                                      } else {
                                        # locate show more button and click it
                                        view_more_button = suppressMessages(query_page_for_button(remDr = self$client, query_pattern = self$show_more_class, query_attr = "class"))
                                      }

                                      # if show more button is set by class then locate it
                                    } else if (!is.null(self$show_more_id)) {

                                      # if verbose is TRUE then print message
                                      if (isTRUE(verbose_)) {
                                        # locate show more button and click it
                                        view_more_button = query_page_for_button(remDr = self$client, query_pattern = self$show_more_id, query_attr = "id")
                                      } else {
                                        # locate show more button and click it
                                        view_more_button = suppressMessages(query_page_for_button(remDr = self$client, query_pattern = self$show_more_id, query_attr = "id"))
                                      }
                                    }

                                    # if counter is greater than 3 then sleep for 2 seconds and engage the page
                                    if (counter %in% random_seq_b) {
                                      if(isTRUE(verbose_)) {
                                        message("Doing some stuff")
                                      }
                                      Sys.sleep(runif(1, .1, .2))
                                      element <- tryCatch(self$client$findElement(using = 'css', value = paste0("button[class *= '",self$show_more_class,"']" )), error = function(e) e )

                                      if (!is.null(element) && inherits(element, "try-error")) {
                                        Sys.sleep(runif(1, .1, .2))
                                        element$getElementLocationInView()
                                        Sys.sleep(runif(1, .1, .2))
                                        self$client$mouseMoveToLocation(webElement = element)
                                      }
                                    }

                                  } else {
                                    stop("No show more button found/n Please specify show more button class or id using set_show_more_button()")
                                  }

                                  if (isTRUE(verbose_)) {
                                    message("Evaluating dates")
                                  }

                                  # if counter is 2, 4, 5, 7 or 9 then hit page down 4-10 times
                                  if (counter %in% every_third_seq) {

                                    ## Scroll to current bottom
                                    page_element = self$client$findElement(using = 'css selector', value = "body")

                                    # Set random number of page down hits between 4 and 10
                                    number_of_down_arrows = round(runif(1, 8, runif(1, 9, 12)))

                                    message(paste0("Hitting page_down ", number_of_down_arrows, " times\nWish me luck! Let's hope we have better luck in next iteration..."))

                                    # hit page down
                                    for (i in 1:number_of_down_arrows) {
                                      page_element$sendKeysToElement(list(key = "page_down"))
                                      Sys.sleep(runif(1, .1, .26))
                                    }
                                  }

                                  # Sleep for 1-3 seconds
                                  Sys.sleep(runif(1, 2, 3.5))

                                  # get html
                                  self$get_html()

                                  # get news elements
                                  tv2_news_elements = get_news_elements_TV2(page_source = self$page_source)

                                  # update list of dates for all available news elements
                                  news_article_dates <- extract_and_format_dates_TV2(list_of_elements = tv2_news_elements)

                                  # Create table of dates
                                  count_table = table(news_article_dates)

                                  # if not first iteration, then add count table to historical count table list
                                  if (counter > 1) {
                                    historical_counts = c(historical_counts, list(count_table))
                                  }

                                  # Get current date
                                  today = Sys.Date()

                                  # Calculate days since cut_off_date
                                  days_since_cut = today - as.Date(cut_off_date)

                                  # When the iteration reaches five iterations plus the number of days from today until cut off date,
                                  # the cut off date is adjusted to the nearest date in the count table
                                  if ((days_since_cut + 5) == counter ) {

                                    # adjust cut off date to the nearest preceding date in the count table
                                    cut_off_date = names(count_table)[which.min(abs(as.Date(names(count_table)) - as.Date(cut_off_date)))]
                                    # if verbose is set to TRUE then print message
                                    if (isTRUE(verbose_)) {
                                      message("Cut off date adjusted to: ", cut_off_date)
                                    }
                                  }

                                  # Evaluate condition for while loop (negate for while loop)
                                  while_loop_condition <- !historical_count_comparison(historical_count_tables = historical_counts,
                                                                                      current_count_table = count_table,
                                                                                      target_date = cut_off_date,
                                                                                      count_tolerance = 1,
                                                                                      iteration_tolerance = 1,
                                                                                      time_window = 3
                                                                                      )

                                  # Message user about dates if counter is 4
                                  if (counter %in% every_third_seq && isTRUE(verbose_)) {
                                    message("While loop condition: ", while_loop_condition)
                                    message("Still working on getting news items according to cut-off date\nCurrent article count per date:\n")
                                    print(count_table)
                                    print(counter)
                                  }

                                  # increment counter
                                  counter = counter + 1

                                }

                                # # filter out articles that are older than cut_off_date
                                tv2_news_elements = tv2_news_elements[as.Date(news_article_dates) >= as.Date(cut_off_date)]

                                # Process news elements
                                tv2_news_list = process_news_elements_TV2(html_elements = tv2_news_elements)

                                # convert to data frame
                                tv2_news_df = dplyr::bind_rows(tv2_news_list)

                                if(isTRUE(debug_)) {
                                  return(list(historical_counts,
                                              count_table,
                                              tv2_news_elements,
                                              tv2_news_df))
                                } else {
                                  # return data frame
                                  return(tv2_news_df)
                                }
                              }
                            }
                          )
)


# Script for debugging purposes - removed from while-loop in tv2
# if (is.na(while_loop_condition) || length(while_loop_condition) == 0 || counter > 10) {
#   # Function to calculate the average count within a date range
#   calculate_average <- function(count_table, range, today) {
#     relevant_dates <- names(count_table)[which(abs(as.Date(names(count_table)) - today) <= range)]
#     output = round(mean(count_table[relevant_dates], na.rm = TRUE))
#     return(output)
#   }
#
#   # Check if focus_date is present in current_counts
#   date_in_current <- as.character(cut_off_date) %in% names(count_table)
#
#   # Today's date
#   today <- Sys.Date()
#
#   # Evaluate conditions
#   if (date_in_current) {
#     # Calculate average count for the specified date range
#     average_count <- calculate_average(count_table, 4, today)
#
#     # Retrieve the count for the target date
#     target_date_count <- count_table[as.character(cut_off_date)]
#
#     # Check if the deviation from average is within tolerance
#     deviation_within_tolerance <- abs(target_date_count - average_count) <= 1
#   } else {
#     deviation_within_tolerance <- FALSE
#   }
#   message("Deviation within tolerance: ", deviation_within_tolerance)
#
#   # Check historical presence and stability of focus date
#   historical_presence <- sapply(historical_counts, function(table) as.character(cut_off_date) %in% names(table), USE.NAMES = FALSE)
#   historical_instances <- which(historical_presence)
#
#   print(historical_presence)
#   print(historical_instances)
#
#   # Check if focus date was stable over more than four historical counts
#   stable_for_four_counts <- FALSE
#   if (length(historical_instances) > 4) {
#     date_counts <- sapply(historical_counts[historical_instances], function(table) table[as.character(cut_off_date)], USE.NAMES = FALSE)
#     stable_for_four_counts <- all(abs(diff(date_counts)) <= 1)
#   }
#
#   message("Stable for four counts: ", stable_for_four_counts)
#
# }
