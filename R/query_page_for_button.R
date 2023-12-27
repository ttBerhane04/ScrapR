#' Query page for button
#'
#' Query page for button and returns the first button that is succesfully clicked
#'
#' @param remDr A remote driver object see remoteDr
#' @param query_pattern A string or vector of strings specifying words used to query class(es) of elements with button tags
#' @param query_attr A string specifying the attribute to search for. Options are 'class' or 'id'.


query_page_for_button = function(remDr, query_pattern = NULL, query_attr = NULL) {

  # check if search_method is valid
  if (!query_attr %in% c("class", "id")) {
    message("Error: search_method must be one of 'class' or 'id' \n Defaulting to 'class'")
    query_attr = "class"
  }

  # Find all buttons on page
  button_elements = remDr$findElements(using = "tag name", value = "button")

  # check that query pattern is valid
  if (is.null(query_pattern)) {
    message("Error: Must provide query pattern")
    return(list(click = FALSE, button = list(value = NA, attr = query_attr)))
  }

  # iterate through children and check if id or class matches any of the patterns
  for (i in 1:length(button_elements)) {
    # convert id and class to lower case
    value <- unlist(button_elements[[i]]$getElementAttribute(query_attr))
    # check if any of the patterns match the id or class
    if (any(any(sapply(query_pattern, grepl, x = tolower(value))))) {
      # if match, click button
      eval_click = tryCatch(handle_button_click(element = button_elements[[i]]), error = function(e) e)
      # check if error
      error_click = inherits(eval_click, "error")

      #counter
      counter = 0

      # while error_click true, try four times to find buttons and click again
      if (error_click) {

        # counter
        counter = counter + 1
        Sys.sleep(2)

        # Find buttons
        button_elements = remDr$findElements(using = "tag name", value = "button")

        # try to click button again
        eval_click = tryCatch(handle_button_click(element = button_elements[[i]]), error = function(e) e)

        # check if error
        error_click = inherits(eval_click, "error")

        if (counter == 4) {
          message("Error: Could not click button")
          return(list(click = FALSE, button = list(value = NA, attr = query_attr)))
        }

      }

      # if match, click button and return TRUE
      if (isTRUE(eval_click)) {
        # if value has " " split and select first element wich matches query_pattern
        pattern = ifelse(length(query_pattern) > 1, paste0(query_pattern, collapse = "|"), query_pattern)

        # if value has " " split and select first element wich matches query_pattern
        if (grepl(" ", value)) {

          # split and select first element wich matches query_pattern
          attr_value = strsplit(value, " ")[[1]][grepl(pattern, strsplit(value, " ")[[1]])][1]
        } else {

          attr_value = value
        }

        return(list(click = TRUE, button = list(value = attr_value, attr = query_attr)))
      }
    }
  }
  # if no match, return FALSE and NA
  return(list(click = FALSE, button = list(value = NA, attr = query_attr)))
}


#' Handle Button Click
#'
#' Highlights and clicks a given web element, typically a button, and prints a message to the console.
#'
#' @param element An object representing a web element of a button to be clicked. (Will return an error if element is not a button)
#' @return Logical value TRUE indicating the function execution was successful.
#'
#' @examples
#' \dontrun{
#' # Assuming 'child' is a valid web element
#' handle_button_click(child)
#' }
#'
handle_button_click <- function(element) {
  # put element in view
  element$getElementLocationInView()
  # highlight
  element$highlightElement()
  # click button
  element$clickElement()
  # message to user
  message("Button clicked")
  return(TRUE)
}
