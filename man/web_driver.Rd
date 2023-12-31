% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/web_driver.R
\name{web_driver}
\alias{web_driver}
\title{web_driver: An R6Class for Web Scraping and Navigation}
\description{
web_driver: An R6Class for Web Scraping and Navigation

web_driver: An R6Class for Web Scraping and Navigation
}
\details{
This class provides methods for web scraping and navigation using RSelenium.
It includes functionalities like initializing a web driver, navigating to URLs, handling cookie consents, and scraping news articles.
}
\examples{
\dontrun{
# load an instance of the web driver
remote_browser <- web_driver$new()

# Initialize Rselenuim
remote_browser$initialize_driver(headless = FALSE)

# Print to see what topics are available from tv2.dk
# print(remote_browser$tv2)

# Select news outlet and topic
remote_browser$go_to_tv2('klima')

# Get news articles
tv2_news_klima = remote_browser$get_news_articles(last_update = "2023-11-20")

# Quit RSelenium
remote_browser$quit_RSelenium()
}


}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{rD}}{Remote driver object.}

\item{\code{server}}{RSelenium server object.}

\item{\code{client}}{RSelenium client object.}

\item{\code{headless}}{Logical indicating if the browser is run in headless mode.}

\item{\code{outlet}}{Current news outlet being scraped.}

\item{\code{topic}}{Current topic being scraped.}

\item{\code{url_}}{Current URL navigated to.}

\item{\code{page_source}}{HTML source of the current page.}

\item{\code{show_more_lgl}}{Logical indicating if a 'show more' button was found.}

\item{\code{show_more_id}}{ID of the 'show more' button.}

\item{\code{show_more_class}}{Class of the 'show more' button.}

\item{\code{tv2}}{List of TV2 news categories and their URLs.}

\item{\code{dr}}{List of DR news categories and their URLs.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-web_driver-initialize_driver}{\code{web_driver$initialize_driver()}}
\item \href{#method-web_driver-quit_RSelenium}{\code{web_driver$quit_RSelenium()}}
\item \href{#method-web_driver-restart_driver}{\code{web_driver$restart_driver()}}
\item \href{#method-web_driver-go_to_url}{\code{web_driver$go_to_url()}}
\item \href{#method-web_driver-go_to_dr}{\code{web_driver$go_to_dr()}}
\item \href{#method-web_driver-go_to_tv2}{\code{web_driver$go_to_tv2()}}
\item \href{#method-web_driver-get_html}{\code{web_driver$get_html()}}
\item \href{#method-web_driver-handle_cookie_consent}{\code{web_driver$handle_cookie_consent()}}
\item \href{#method-web_driver-locate_show_more_button}{\code{web_driver$locate_show_more_button()}}
\item \href{#method-web_driver-set_show_more_button}{\code{web_driver$set_show_more_button()}}
\item \href{#method-web_driver-get_news_articles}{\code{web_driver$get_news_articles()}}
\item \href{#method-web_driver-clone}{\code{web_driver$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-web_driver-initialize_driver"></a>}}
\if{latex}{\out{\hypertarget{method-web_driver-initialize_driver}{}}}
\subsection{Method \code{initialize_driver()}}{
Initialize WebDriver

This method initializes the RSelenium web driver in either headless or normal mode.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{web_driver$initialize_driver(headless = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{headless}}{Logical, default is TRUE. Indicates whether to run the webdriver in headless mode.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
None.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-web_driver-quit_RSelenium"></a>}}
\if{latex}{\out{\hypertarget{method-web_driver-quit_RSelenium}{}}}
\subsection{Method \code{quit_RSelenium()}}{
Quit RSelenium

Shuts down the RSeleium server and closes the client.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{web_driver$quit_RSelenium()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-web_driver-restart_driver"></a>}}
\if{latex}{\out{\hypertarget{method-web_driver-restart_driver}{}}}
\subsection{Method \code{restart_driver()}}{
Restart WebDriver

Restarts the RSelenium web driver if it is not alive, maintaining the current URL.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{web_driver$restart_driver()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-web_driver-go_to_url"></a>}}
\if{latex}{\out{\hypertarget{method-web_driver-go_to_url}{}}}
\subsection{Method \code{go_to_url()}}{
Go to URL

Navigates to a specified URL and checks for common HTTP error responses.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{web_driver$go_to_url(url)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{url}}{The URL to navigate to.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
None, but stops with a message if an error or invalid URL is encountered.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-web_driver-go_to_dr"></a>}}
\if{latex}{\out{\hypertarget{method-web_driver-go_to_dr}{}}}
\subsection{Method \code{go_to_dr()}}{
Navigate to a Category on DR.dk

Navigates to a specified news category on DR.dk.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{web_driver$go_to_dr(category = NULL, handle_cookie = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{category}}{The category to navigate to.}

\item{\code{handle_cookie}}{Logical; if TRUE, handles the cookie consent prompt.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
None, but navigates to the specified category on DR.dk.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-web_driver-go_to_tv2"></a>}}
\if{latex}{\out{\hypertarget{method-web_driver-go_to_tv2}{}}}
\subsection{Method \code{go_to_tv2()}}{
Navigate to a Category on TV2.dk

Navigates to a specified news category on TV2.dk.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{web_driver$go_to_tv2(category = NULL, handle_cookie = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{category}}{The category to navigate to.}

\item{\code{handle_cookie}}{Logical; if TRUE, handles the cookie consent prompt.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
None, but navigates to the specified category on TV2.dk.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-web_driver-get_html"></a>}}
\if{latex}{\out{\hypertarget{method-web_driver-get_html}{}}}
\subsection{Method \code{get_html()}}{
Get HTML Content

Retrieves the HTML content of the current page.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{web_driver$get_html(do_return = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{do_return}}{Logical, default is FALSE. If TRUE, returns the page source as a character vector.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Character vector containing the page source if do_return is TRUE.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-web_driver-handle_cookie_consent"></a>}}
\if{latex}{\out{\hypertarget{method-web_driver-handle_cookie_consent}{}}}
\subsection{Method \code{handle_cookie_consent()}}{
Handle Cookie Consent

Handles the cookie consent prompt on a webpage by either accepting or rejecting it.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{web_driver$handle_cookie_consent(
  using = NULL,
  value = NULL,
  reject = FALSE,
  user_prompt = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{using}}{The method used to find the consent element (e.g., "class name").}

\item{\code{value}}{The value to search for in the consent element.}

\item{\code{reject}}{Logical; if TRUE, rejects the cookie consent. If FALSE, accepts it.}

\item{\code{user_prompt}}{Logical; if TRUE, prompts the user to accept or reject the cookie consent.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
None, but prints a message indicating the action taken.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-web_driver-locate_show_more_button"></a>}}
\if{latex}{\out{\hypertarget{method-web_driver-locate_show_more_button}{}}}
\subsection{Method \code{locate_show_more_button()}}{
Locate 'Show More' Button

Attempts to locate the 'Show More' button on a webpage using its class or ID.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{web_driver$locate_show_more_button()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
None, but sets internal fields related to the 'Show More' button if found.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-web_driver-set_show_more_button"></a>}}
\if{latex}{\out{\hypertarget{method-web_driver-set_show_more_button}{}}}
\subsection{Method \code{set_show_more_button()}}{
Set 'Show More' Button

Manually sets the 'Show More' button's class or ID if it cannot be automatically located.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{web_driver$set_show_more_button(attr_type = NULL, attr_value = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attr_type}}{Str value of either 'class' or 'id'.}

\item{\code{attr_value}}{char string used to query html for button.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
None, but updates the internal fields for the 'Show More' button.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-web_driver-get_news_articles"></a>}}
\if{latex}{\out{\hypertarget{method-web_driver-get_news_articles}{}}}
\subsection{Method \code{get_news_articles()}}{
Get News Articles

Scrapes news articles from a specified news outlet and filters them based on a cutoff date.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{web_driver$get_news_articles(
  last_update = NULL,
  verbose_ = FALSE,
  debug_ = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{last_update}}{Date (in 'YYYY-MM-DD' format) indicating when the news outlet was last crawled; used as a cutoff for new articles.}

\item{\code{verbose_}}{Logical indicating whether to print messages to the console.}

\item{\code{debug_}}{Logical indicating whether to return additional object for debuging purposes}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A data frame with metadata and URLs of news articles that meet the criteria.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-web_driver-clone"></a>}}
\if{latex}{\out{\hypertarget{method-web_driver-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{web_driver$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
