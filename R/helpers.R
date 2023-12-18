#' Extract Date from URL
#'
#' This function parses a given URL to extract a date in the format YYYY-MM-DD.
#' It is designed to work with URLs that contain dates embedded within their paths.
#'
#' @param url A character string representing the URL to be parsed.
#'
#' @return A character string of the date in 'YYYY-MM-DD' format if a valid date is found in the URL.
#' Returns `NA` if no valid date format is detected.
#'
#' @examples
#' \dontrun{
#' grab_date_from_url("https://example.com/2023-01-01/article-title")
#' # returns "2023-01-01"
#'
#' grab_date_from_url("https://example.com/article-title")
#' # returns NA
#'}
#'
#' @importFrom utils tail
#' @export
#'
#'



grab_date_from_url <- function(url) {
  # Split URL into components
  parsed_url <- strsplit(url, "/")[[1]]

  # Get the last path component
  path_url <- utils::tail(parsed_url, n = 1)

  # Regex pattern for a date in the format YYYY-MM-DD
  pattern <- "(\\d{4})-(\\d{2})-(\\d{2})"

  # Use regex to extract the date
  matches <- regmatches(path_url, regexpr(pattern, path_url))

  # Check if a match was found and return the formatted date
  if (length(matches) > 0 && nchar(matches) > 0) {
    formatted_date <- matches
  } else {
    formatted_date <- NA
  }

  return(formatted_date)
}

#' Format Date from Raw Date String
#'
#' This function converts a raw date string into a standardized date format (YYYY-MM-DD).
#' It handles various formats, including relative dates like "I dag" (today) or "I går" (yesterday) in Danish,
#' as well as specific dates with Danish month abbreviations.
#'
#' @param raw_date A character string representing the raw date to be formatted.
#'
#' @return A character string of the date in 'YYYY-MM-DD' format.
#' If the input is not a recognized date format or is empty, returns `NA`.
#'
#' @examples
#' \dontrun{
#' format_date_from_time("I dag") # returns current date in 'YYYY-MM-DD'
#' format_date_from_time("I går") # returns yesterday's date in 'YYYY-MM-DD'
#' format_date_from_time("5. jun") # returns 'current year-06-05'
#' format_date_from_time("Invalid Date") # returns NA
#'}
#'
#' @export
#'


format_date_from_time <- function(raw_date) {
  if (nzchar(raw_date)) {
    if (grepl("I dag|minut|sekund|min\\. siden", raw_date)) {
      formatted_date <- format(Sys.Date(), "%Y-%m-%d")
    } else if (grepl("I går", raw_date)) {
      formatted_date <- format(Sys.Date() - 1, "%Y-%m-%d")
    } else {
      # List of months in Danish
      months <- c("jan", "feb", "mar", "apr", "maj", "jun",
                  "jul", "aug", "sep", "okt", "nov", "dec")

      if (any(sapply(months, function(m) grepl(m, raw_date)))) {
        parts <- unlist(strsplit(raw_date, " "))
        day <- sub("\\.", "", parts[1])
        month_name <- parts[2]
        month_number <- which(months == month_name)
        current_year <- format(Sys.Date(), "%Y")

        formatted_date <- sprintf("%s-%02d-%s", current_year, month_number, day)
      } else {
        formatted_date <- NA
      }
    }
  } else {
    formatted_date <- NA
  }

  return(formatted_date)
}
