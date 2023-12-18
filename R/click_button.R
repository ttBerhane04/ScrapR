#' Click a Button on a Web Page using RSelenium
#'
#' This function attempts to click a button on a web page identified either by its ID or class name using RSelenium.
#' It checks if the page length changes after the button click, indicating a successful interaction.
#'
#' @param remDr An object of RSelenium remote driver.
#' @param button_id A string specifying the ID of the button to be clicked. If NULL, button_class is used.
#' @param button_class A string or vector of strings specifying the class(es) of the button to be clicked.
#' If NULL, button_id is used.
#'
#' @return Returns TRUE if the button click leads to a change in page length, indicating successful interaction.
#' Returns FALSE otherwise, or if both button_id and button_class are NULL, or if an error occurs.
#'
#' @examples
#' \dontrun{
#' # Assuming 'remDr' is a valid RSelenium remote driver object
#' click_button(remDr, button_id = "showMoreBtn")
#' click_button(remDr, button_class = "load-more-button")
#'}
#'
#' @importFrom magrittr %>%
#' @importFrom methods is
#' @export
#'
#'



click_button = function(remDr, button_id = NULL, button_class = NULL) {
  # if button id and button class are NULL, return error
  if (is.null(button_id) & is.null(button_class)) {
    print("Error: Must provide button id or class/n If you are laze try using locate_show_more_button ;-)")
    return(FALSE)
  }

  # if button id is not NULL, click on button
  if (!is.null(button_id)) {

    # find button
    show_more_button = tryCatch(remDr$findElement(using = "id", value = button_id), error = function(e) e)

    if (!methods::is(show_more_button, "error")) {
      # if button is found, click on button and evaluate outcome
      if (!methods::is(show_more_button, "error") & length(show_more_button) > 0) {
        # get page length before clicking button
        pre_length <- unlist(remDr$executeScript("return document.body.scrollHeight"))

        ## Scroll to current bottom
        remDr$executeScript("window.scrollTo(0,document.body.scrollHeight)")

        # click button
        show_more_button$clickElement()
        Sys.sleep(.2)

        ## Scroll to current bottom
        remDr$executeScript("window.scrollTo(0,document.body.scrollHeight)")

        # get page length after clicking button
        post_length <- unlist(remDr$executeScript("return document.body.scrollHeight"))

        if (post_length > pre_length) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      }

    } else {
      print("Error with button id")
      return(FALSE)
    }
  }

  # if button class is not NULL, click on button
  if (!is.null(button_class)) {

    if (grepl(" ", button_class, fixed = TRUE)) {
      # if button class contains a space, split on space
      button_class = strsplit(button_class, " ")[[1]]
    }

    # iterate over button classes
    for (i in 1:length(button_class)) {
      # find button
      show_more_button = tryCatch(remDr$findElement(using = "class name", value = button_class[i]), error = function(e) e)

      # if button is found, click on button and evaluate outcome
      if (!is(show_more_button, "error") & length(show_more_button) > 0) {
        # get page length before clicking button
        pre_length <- unlist(remDr$executeScript("return document.body.scrollHeight"))

        ## Scroll to current bottom
        remDr$executeScript("window.scrollTo(0,document.body.scrollHeight)")

        # click button
        show_more_button$clickElement()
        Sys.sleep(.2)

        ## Scroll to current bottom
        remDr$executeScript("window.scrollTo(0,document.body.scrollHeight)")

        # get page length after clicking button
        post_length <- unlist(remDr$executeScript("return document.body.scrollHeight"))

        if (post_length > pre_length) {
          return(TRUE)
        } else {
          next
        }
      }
    }

    if (is(show_more_button, "error")) {
      print("Error with button class")
      return(FALSE)
    }
  }

  # if button id and class are NULL, return FALSE
  return(FALSE)
}
