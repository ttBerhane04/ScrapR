% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_page_for_button.R
\name{handle_button_click}
\alias{handle_button_click}
\title{Handle Button Click}
\usage{
handle_button_click(element)
}
\arguments{
\item{element}{An object representing a web element of a button to be clicked. (Will return an error if element is not a button)}
}
\value{
Logical value TRUE indicating the function execution was successful.
}
\description{
Highlights and clicks a given web element, typically a button, and prints a message to the console.
}
\examples{
\dontrun{
# Assuming 'child' is a valid web element
handle_button_click(child)
}

}
