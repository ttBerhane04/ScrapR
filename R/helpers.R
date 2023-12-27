#' historical_count_comparison (with count Stability Evaluation)
#'
#' This function evaluates the stability of a specific date's count in a series of historical data.
#' It checks if the count on the target date significantly deviates from a rounded mean of recent counts
#' or remains stable in previous observations.
#'
#' @param historical_count_tables A list of previous count tables by date.
#' @param current_count_table The current date-count table.
#' @param target_date The specific date to evaluate.
#' @param count_tolerance Threshold for allowable count deviation.
#' @param iteration_tolerance Threshold for allowable count deviation between iterations.
#' @param time_window Number of days for calculating the average count.
#' @return A boolean value indicating if the count for the focus date is stable or within the tolerance range.
#' @examples
#' \dontrun{
#' focus_date <- as.Date("2023-12-24")
#' historical_count_comparison(historical_count_list, current_count_table, focus_date, 2, 7)
#' }
historical_count_comparison <- function(historical_count_tables = NULL,
                                        current_count_table = NULL,
                                        target_date = NULL,
                                        count_tolerance = 1,
                                        iteration_tolerance = 3,
                                        time_window = 3) {

  # Function to calculate the average count within a date range
  calculate_average <- function(count_table, range, today) {
    relevant_dates <- names(count_table)[which(abs(as.Date(names(count_table)) - today) <= range)]
    output = round(mean(count_table[relevant_dates], na.rm = TRUE))
    return(output)
  }

  # Check if focus_date is present in current_counts
  date_in_current <- as.character(target_date) %in% names(current_count_table)

  # Today's date
  today <- Sys.Date()

  # Evaluate conditions
  if (date_in_current) {
    # # Calculate average count for the specified date range
    # average_count <- calculate_average(current_count_table, time_window, today)
    #
    # # Retrieve the count for the target date
    # target_date_count <- current_count_table[as.character(target_date)]
    #
    # # Check if the deviation from average is within tolerance
    # deviation_within_tolerance <- abs(target_date_count - average_count) <= 1
  } else {
    return(date_in_current)
  }

  # Check historical presence and stability of focus date
  historical_presence <- sapply(historical_count_tables, function(table) any(as.character(target_date) %in% names(table)), USE.NAMES = FALSE)
  historical_instances <- which(historical_presence)

  # Check if focus date was stable over more than four historical counts
  stable_for_four_counts <- FALSE
  if (length(historical_instances) > 4) {
    # Get the last four with target date
    historical_instances <- historical_instances[(length(historical_instances) - 3):length(historical_instances)]
    # Get the counts for the target date
    date_counts <- sapply(historical_count_tables[historical_instances], function(table) table[as.character(target_date)], USE.NAMES = FALSE)
    # Check if the counts are within iteration tolerance
    stable_for_four_counts <- all(abs(diff(date_counts)) <= iteration_tolerance)
  }

  # Return TRUE if either condition is met
  return(date_in_current && stable_for_four_counts)

}

#' Simple Count Comparison
#'
#' Compares the count of events on a specific historical date with counts on recent dates.
#' The function examines if the event frequency on a past date aligns closely with the event frequency on dates nearer to the present.
#'
#' @param count_table The current date-count table.
#' @param target_date The specific historical date of interest (as a Date object).
#' @param count_tolerance Acceptable difference in counts for considering them similar.
#' @param time_window Days around the current date to include in the comparison.
#' @return TRUE if the count on the target date is similar to counts on recent dates, FALSE otherwise.
#' @examples
#' \dontrun{
#' date_counts <- c("2017-12-24" = 2, "2022-12-24" = 2, "2023-10-25" = 1, "2023-10-31" = 1,
#'                  "2023-11-10" = 1, "2023-11-19" = 1, "2023-12-20" = 6, "2023-12-21" = 8,
#'                  "2023-12-22" = 10, "2023-12-23" = 9, "2023-12-24" = 14)
#' target_date <- as.Date("2023-11-10")
#' simple_count_comparison(date_counts, target_date)
#' }
simple_count_comparison <- function(count_table = NULL,
                                    target_date = NULL,
                                    count_tolerance = 1,
                                    time_window = 3) {

  # Get raw counts from the count table
  event_counts <- count_table

  # Ensure the target date is part of the date vector
  if (!as.character(target_date) %in% names(count_table)) {
    return(TRUE)
  }

  # Retrieve the count for the target date
  target_date_count <- event_counts[as.character(target_date)]
  # Today's date
  today <- Sys.Date()

  # Identify recent dates within the specified range
  relevant_dates <- which(abs(as.Date(names(count_table)) - today) <= time_window)

  # Calculate count mean for these recent dates
  recent_count_mean = round(mean(event_counts[relevant_dates], na.rm = TRUE))

  # Determine if any recent date count is similar to the target date's count
  is_similar_count <- !any(abs(recent_count_mean - target_date_count) <= count_tolerance)

  return(is_similar_count)
}

#' Extract News Elements from a Web Page Source (TV2)
#'
#' This function extracts news-related HTML elements from the provided web page source,
#' specifically targeting Danish news outlets 'dr' and 'tv2'.
#' It uses XPath to identify relevant elements within the HTML structure.
#'
#' @param page_source A character string or xml2 object representing the HTML source of a web page.
#'
#' @return An object of class `xml_nodeset` containing the extracted news elements.
#' If the page source is NULL or the outlet is not recognized, the function stops with an error message.
#'
#' @examples
#' \dontrun{
#' # Assuming valid HTML content in 'html_content' variable
#' get_news_elements_TV2(page_source = html_content)
#'}
#'
#' @importFrom magrittr %>%
#' @importFrom rvest html_elements
#' @importFrom xml2 read_html
get_news_elements_TV2 = function(page_source = NULL) {
  # check if page_source is NULL
  if (is.null(page_source)) {
    stop("Error: page_source must be provided")
  }

  # Convert page_source to xml
  if (is.character(page_source)) {
    page_source <- xml2::read_html(page_source)
  }

  # get news elements from tv2.dk
  news_elements = page_source %>%
    rvest::html_elements(css = "article[class *= 'tc_teaser']")

}

#' Extract News Elements from a Web Page Source (DR)
#'
#' This function extracts news-related HTML elements from the provided web page source,
#' specifically targeting Danish news outlets 'dr' and 'tv2'.
#' It uses XPath to identify relevant elements within the HTML structure.
#'
#' @param page_source A character string or xml2 object representing the HTML source of a web page.
#'
#' @return An object of class `xml_nodeset` containing the extracted news elements.
#' If the page source is NULL or the outlet is not recognized, the function stops with an error message.
#'
#' @examples
#' \dontrun{
#' # Assuming valid HTML content in 'html_content' variable
#' get_news_elements_DR(page_source = html_content)
#'}
#'
#' @importFrom magrittr %>%
#' @importFrom rvest html_elements
#' @importFrom xml2 read_html
get_news_elements_DR = function(page_source = NULL) {
  # check if page_source is NULL
  if (is.null(page_source)) {
    stop("Error: page_source must be provided")
  }

  # Convert page_source to xml
  if (is.character(page_source)) {
    page_source <- xml2::read_html(page_source)
  }

  news_elements = page_source %>%
    rvest::html_elements(css = "div[class *= 'dre-teaser-content']")

}



#' Process News Elements from DR into Structured List
#'
#' Processes a list of news elements, extracting key details such as the URL,
#' headline, topic, and publication date. It utilizes the 'extract_and_format_date'
#' function to format the date.
#'
#' @param html_elements A list of news article elements to be processed.
#' @return A list of lists, where each inner list contains the URL, headline,
#'         topic, and date of a news article.
#' @importFrom rvest html_elements html_attr html_text
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @examples
#' \dontrun{
#'   # Assuming 'html_elements' is a list of HTML nodes from a news website
#'   process_news_elements_DR <- process_news_elements(html_elements)
#' }
process_news_elements_DR <- function(html_elements) {
  dr_list = purrr::map(html_elements, ~ {
    list(
      url = .x %>% rvest::html_elements("a") %>% rvest::html_attr("href"),
      headline = .x %>% rvest::html_elements(css = "span[class *= 'dre-title-text']") %>% rvest::html_text(),
      topic = .x %>% rvest::html_elements(css = "span[class *= 'dre-teaser-meta__part--primary']") %>% rvest::html_text()
      )
  })

  dr_dates = extract_and_format_dates_DR(html_elements)

  for (i in 1:length(dr_list)) {
    dr_list[[i]]$date = dr_dates[i]
  }

  return(dr_list)
}

#' Process News Elements from TV" into Structured List
#'
#' Processes a list of news elements, extracting key details such as the URL,
#' headline, topic, and publication date. It utilizes the 'extract_and_format_date'
#' function to format the date.
#'
#' @param html_elements A list of news article elements to be processed.
#' @return A list of lists, where each inner list contains the URL, headline,
#'         topic, and date of a news article.
#' @importFrom rvest html_elements html_attr html_text
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @examples
#' \dontrun{
#'   # Assuming 'list_of_elements' is a list of HTML nodes from a news website
#'   process_news_elements_TV2 <- process_news_elements(list_of_elements)
#' }
process_news_elements_TV2 <- function(html_elements) {

  list_of_urls = html_elements %>% rvest::html_elements(css = "a[class *= 'tc_teaser__link']") %>% rvest::html_attr("href")

  output = purrr::map2(html_elements, list_of_urls, function(element, url_) {
    list(
      url = url_,
      headline = element %>% rvest::html_elements(css = "[class *= 'tc_heading']") %>% rvest::html_text(),
      topic = element %>% rvest::html_elements(css = "span[class *= 'tc_label--color-nyheder']") %>% rvest::html_text(),
      date = grab_date_from_url(url_)
    )
  })

  return(output)
}

#' Extract and Format Date from News Element (DR)
#'
#' This function extracts the publication date from a news article element
#' and formats it into a standardized format. It handles different structures
#' of the date element within the news article.
#'
#' @param list_of_elements The news article element from which the date is to be extracted.
#' @return A character string representing the formatted date, or NA if the date cannot be determined.
#' @importFrom rvest html_elements html_text
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @examples
#' \dontrun{
#'   # Assuming 'list_of_elements' is a list HTML node from a news website
#'   extract_and_format_dates_DR <- extract_and_format_date(list_of_elements)
#' }
#'
extract_and_format_dates_DR <- function(list_of_elements) {

  list_of_dates <- purrr::map(list_of_elements, ~ {
    .x %>%
      rvest::html_elements(css = "span[class *= 'dre-teaser-meta__part']") %>% # div>span>span>
      rvest::html_text()
    })

  dr_dates = purrr::map(list_of_dates, ~ {
    # Determine the date based on the length of date_list
    if (length(.x) == 2) {
      return(format_date_from_time(.x[2]))
    } else if (length(.x) == 1 && grepl("\\d", .x[1])) {
      return(format_date_from_time(.x[1]))
    }
    return(NA)
  })

  return(unlist(dr_dates))
}

# > test_dr_politik = ScrapR::retrieve_news_items("dr", "politik")
# Handling cookie consent...
# Working on cookie consent prompt...
# Button clicked
# Cookie consent prompt handled.
# Error in `purrr::map()`:
#   ℹ In index: 1.
# Caused by error in `purrr::map()`:
#   ℹ In index: 1.
# ℹ With name: node.
# Caused by error in `UseMethod()`:
#   ! no applicable method for 'xml_find_all' applied to an object of class "externalptr"
# Run `rlang::last_trace()` to see where the error occurred.


#' Extract and Format Date from News Element (TV2)
#'
#' This function extracts the publication date from a news article element
#' and formats it into a standardized format. It handles different structures
#' of the date element within the news article.
#'
#' @param list_of_elements The news article element from which the date is to be extracted.
#' @return A character string representing the formatted date, or NA if the date cannot be determined.
#' @importFrom rvest html_elements html_text
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @examples
#' \dontrun{
#'   # Assuming 'list_of_elements' is a list HTML node from a news website
#'   extract_and_format_dates_TV2 <- extract_and_format_date(list_of_elements)
#' }
#'
extract_and_format_dates_TV2 <- function(list_of_elements) {
  # tv2 - url
  list_of_urls = list_of_elements %>%
    rvest::html_elements(css = "a[class *= 'tc_teaser__link']") %>%
    rvest::html_attr("href")

  tv2_dates = purrr::map(list_of_urls, ~ {
    tv2_date = grab_date_from_url(.x)
    # if NA then set date to current date YYYY-MM-DD
    # This handles cases from tv2 play, where the date is not present in the html or URL
    # Revisit this!
    return(tv2_date)
  })

  return(unlist(tv2_dates))
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
        day <- as.double(sub("\\.", "", parts[1]))
        month_name <- parts[2]
        month_number <- which(months == month_name)
        current_year <- format(Sys.Date(), "%Y")

        formatted_date <- sprintf("%s-%02d-%02d", current_year, month_number, day)
      } else {
        formatted_date <- NA
      }
    }
  } else {
    formatted_date <- NA
  }

  return(formatted_date)
}

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
    return(matches)
  } else {
    return(as.character(Sys.Date())) # Messy way to enforce a consistent format for dates
  }
}
