#' @title Query page for button
#' @description Query page for button and returns the first button that is succesfully clicked
#' @param remDr A remote driver object see remoteDr
#' @param query_pattern A string or vector of strings specifying words used to query class(es) of elements with button tags
#' @param return_type A string specifying the type of object to return. Setting it to "values" will return for a list(click = TRUE, button = list(value = [attr_value], attr = attribute))
#'                    while setting it to "none" will return TRUE/FALSE. Default is "values".
#' @param search_method A string specifying the attribute to search for. Options are 'class' or 'id'.


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
  for (button in button_elements) {
    # convert id and class to lower case
    value <- unlist(button$getElementAttribute(query_attr))
    # check if any of the patterns match the id or class
    if (any(any(sapply(query_pattern, grepl, x = tolower(value))))) {
      # if match, click button and return TRUE
      if (handle_button_click(button)) {
        # if value has " " split and select first element wich matches query_pattern
        pattern = ifelse(length(query_pattern) > 1, paste0(query_pattern, collapse = "|"), query_pattern)

        # if value has " " split and select first element wich matches query_pattern
        if (grepl(" ", value)) {

          # split and select first element wich matches query_pattern
          attr_value = strsplit(value, " ")[[1]][grepl(pattern, strsplit(value, " ")[[1]])][1]
        } else {

          attr_value = view_more_button$button$value
        }

        return(list(click = TRUE, button = list(value = attr_value, attr = query_attr)))
      }
    }
  }
  # if no match, return FALSE and NA
  return(list(click = FALSE, button = list(value = NA, attr = query_attr)))
}
