% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bots.R
\name{make_bots}
\alias{make_bots}
\title{Convenience function to create a list of bots for all players}
\usage{
make_bots(game, bot_fun, ..., players = game$players)
}
\arguments{
\item{game}{the game object}

\item{bot_fun}{a bot function like e.g. \code{\link{bot_eq}}}

\item{...}{additional arguments passed to \code{bot_fun}}
}
\description{
Every player gets the same bot type
}
\seealso{
Other Bots: \code{\link{bot_eq}},
  \code{\link{bot_mixture}}, \code{\link{bot_pop}},
  \code{\link{bot_random}}, \code{\link{bot_tables}},
  \code{\link{play_bot_vs_bot}}
}
