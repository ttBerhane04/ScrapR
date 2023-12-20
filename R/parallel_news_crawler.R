#' @title Parallel news crawler
#'
#'
#'
#' @param news_outlet A character vector specifying the news outlet to be scraped.
#' @param topic A character vector specifying the topic to be scraped.
#' @param last_update A character vector specifying the date of the last update.
#' @param cores An integer specifying the number of cores to be used. If cores == NULL it defaults to cores - 2.
#'
#' @importFrom parallel makeCluster stopCluster clusterApply
#'

# Check if news outlet is valid
if (!news_outlet %in% c("dr", "tv2")) {
  stop("Invalid news outlet. Please choose between 'dr' and 'tv2'.")
}

# Check that  topic is valid
if (news_outlet == "dr") {
  if (!topic %in% c('indland','udland', 'penge','politik')) {
    stop("Invalid topic. Please choose between 'indland','udland', 'penge','politik'.")
  }
} else if (news_outlet == "tv2") {
  if (!topic %in% c('krimi', 'politik', 'samfund','udland', 'business', 'penge', 'tech', 'klima')) {
    stop("Invalid topic. Please choose between 'krimi', 'politik', 'samfund','udland', 'business', 'penge', 'tech', 'klima'.")
  }
}

# Set number of cores
if(is.null(cores)){
  cores <- parallel::detectCores() - 2
} else if(!is.numeric(cores)){
  stop("cores is not numeric")
}


library(magrittr)

retrieve_news_items = function(news_outlet, topic, last_crawl = NULL) {

  # load an instance of the web driver
  remote_browser <- web_driver$new()

  # Initialize Rselenuim
  remote_browser$initialize_driver(headless = TRUE)

  if (news_outlet == "dr") {

    if (!topic %in% c('indland','udland', 'penge','politik')) {
      stop("Invalid topic. Please choose between 'indland','udland', 'penge','politik'.")
    }
    # Select news outlet and topic
    remote_browser$go_to_dr(topic)

  } else if (news_outlet == "tv2") {

    if (!topic %in% c('krimi', 'politik', 'samfund','udland', 'business', 'penge', 'tech', 'klima')) {
      stop("Invalid topic. Please choose between 'krimi', 'politik', 'samfund','udland', 'business', 'penge', 'tech', 'klima'.")
    }
    # Select news outlet and topic
    remote_browser$go_to_tv2(topic)
  }

  # Get news articles
  news_items = remote_browser$get_news_articles(last_update = last_crawl)

  # Quit RSelenium
  remote_browser$quit_RSelenium()

  return(news_items)

}

library(future.apply)
library(furrr)
plan(multisession, workers = 6)

par_test = furrr::future_map(c('indland','udland','penge','politik'),
                              ~ retrieve_news_items(topic = .x,
                                                    news_outlet = "dr",
                                                    last_crawl = "2023-12-10"),
                              .options = furrr_options(seed = TRUE),
                              .progress = TRUE)

test = retrieve_news_items(news_outlet = "dr", topic = "indland", last_crawl = "2023-12-10")


crawl_news_outlet = function()


envir <- as.environment(pos)



clust <- parallel::makeCluster(cores)


parallel::clusterApply(clust, ports, function(x) {
  lapply(packages, require, character.only = TRUE)

  assign("rD", RSelenium::rsDriver(browser = browser, port = x,
                                   extraCapabilities = extraCapabilities),
         envir = envir)

  assign("remDr", rD[["client"]],
         envir = envir)
})



chunks <- split(c(1:length(scrape_input)), ceiling(seq_along(c(1:length(scrape_input))) / chunk_size))

scrape_out <- try(parallel::parLapply(clust, input_i, scrape_fun), silent = TRUE)

if (!is(scrape_out, "try-error")) {
  break
}

result_list[[i]] <- scrape_out

close_rselenium(clust = clust)
parallel::stopCluster(clust)
