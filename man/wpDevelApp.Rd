% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wp_app.R
\name{wpDevelApp}
\alias{wpDevelApp}
\title{Create a simple app for testing and developing a gtree web play}
\usage{
wpDevelApp(wp, title = paste0("Playing ", wp$play$game$gameId))
}
\arguments{
\item{wp}{a web play object generated with \code{\link{new_wp}}}

\item{title}{A title string}
}
\description{
Returns a shinyEvents app. You can view the app in RStudio using \code{\link{[shinyEvents]viewApp}}
}
\seealso{
Other Web Play: \code{\link{get_wp}}, \code{\link{new_wp}},
  \code{\link{set_wp_for_app}}, \code{\link{wp_copy}},
  \code{\link{wp_developer_ui}}, \code{\link{wp_reset}},
  \code{\link{wp_set_to_play}}
}
