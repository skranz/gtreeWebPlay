% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bots.R
\name{bot_random}
\alias{bot_random}
\title{Bot that chooses all actions randomly}
\usage{
bot_random(game, player, ...)
}
\arguments{
\item{game}{the game object}

\item{player}{the player number of this bot}
}
\description{
Always picks each possible move with equal probability
}
\seealso{
Other Bots: \code{\link{bot_eq}},
  \code{\link{bot_mixture}}, \code{\link{bot_pop}},
  \code{\link{bot_tables}}, \code{\link{make_bots}},
  \code{\link{play_bot_vs_bot}}
}
