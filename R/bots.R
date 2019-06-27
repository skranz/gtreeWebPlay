# Note: This file can be moved to gtree


#' Convenience function to create a list of bots for all players
#'
#' Every player gets the same bot type
#'
#' @param game the game object
#' @param bot_fun a bot function like e.g. \code{\link{bot_eq}}
#' @param ... additional arguments passed to \code{bot_fun}
#'
#' @family Bots
make_bots = function(game,bot_fun, ..., players=game$players) {
  lapply(players, function(player) {
    do.call(bot_fun, list(player=player, game=game,...))
  })
}

#' Bot that chooses all actions randomly
#'
#' Always picks each possible move with equal probability
#'
#' @param game the game object
#' @param player the player number of this bot
#' @family Bots
bot_random = function(game,player,...) {
  bot = list(
    name = "random_bot",
    player = player,
    choose_action = function(set,...) {
      sample(set,1)
    }
  )
  bot
}

#' Bot that plays according to a specified equilibrium
#'
#' @param game the game object
#' @param player the player number of this bot
#' @param eq an equilibrium, typically an element of game$eq.li
#' @family Bots
bot_eq = function(game, player, eq=game$eq.li[[1]], eq.tables=gtree::eq_tables(game=game, eq.li=list(eq)), name="eq_bot",...) {
  restore.point("bot_eq")
  bot_tables(game, player, tables = eq.tables, name=name)
}

#' Bot whose actions are determined by key-action tables
#'
#' @param game the game object
#' @param player the player number of this bot
#' @param tables a list of tables for each action. The result of \code{eq_tables} is
#' @family Bots
bot_tables = function(game, player, tables, name="table_bot",...) {
  restore.point("bot_tables")
  bot = list(
    name = name,
    player = player,
    choose_action = choose_action_bot_tables,
    tables = tables
  )
  bot
}
choose_action_bot_tables = function(bot, play, stage, action,set,tables=bot$tables,...) {
  restore.point("choose_action_bot_tables")
  var = action$name
  table = tables[[var]]
  hist.df = as_tibble(play$hist)
  keys = setdiff(colnames(table), c(var, ".prob", "eq.inds", "eq.ind"))
  keys = setdiff(keys, names(play$hist)[is.na(play$hist)])
  if (has.col(table, ".prob")) {
    # Mixed strategy table
    if (length(keys)>0)
      table = semi_join(table, hist.df, by=keys)
    val = sample(table[[var]],1, prob = table$.prob)
  } else {
    # Pure strategy table
    if (length(keys)>0) {
      val = semi_join(table, hist.df, by=keys)[[var]]
    } else {
      val = table[[var]]
    }
  }
  cat("\nend action bot tables")
  return(val)
}


#' Bot that mixes between different bots
#'
#' The first time the bot is called
#' for a particular player in a play
#' He picks a child bot randomly.
#' Then it continues with that child bot
#' for this player the whole play.
#'
#' If you use \code{\link{make_bots}} or call
#' repeatedly \code{bot_mixture} to generate mixture bots
#' for each player, the child bots will be independently
#' drawn for each player.
#'
#' Instead, if bot1 is a mixture bot for player 1
#' and you create a mixture bot for player 2 by
#' \code{bot2 = bot1\nbot2$player = 2}
#' then bot2 will select in every play the same
#' child bot than bot1.
#'
#' @param game the game object
#' @param player the player number of this bot
#' @param child_bots A list of child pots
#' @param prob A vector of weights for each child bot. If \code{NULL} (default) all are equally likely.
#' @family Bots
bot_mixture = function(game,player,child_bots, prob=NULL,...) {
  bot = list(
    env = as.environment(list(play_nonce=0, current_child=1)),
    name = "mixture_bot",
    player = player,
    choose_action = choose_action_bot_mixture,
    child_bots = child_bots,
    prob = prob
  )
  bot
}

choose_action_bot_mixture = function(bot, play,...) {
  restore.point("choose_action_bot_mixture")

  # Draw new child if and only if we have a new play
  if (bot$env$play_nonce != play$play_nonce) {
    bot$env$play_nonce = play$play_nonce
    bot$env$current_child = sample.int(length(bot$child_bots),1,prob = bot$prob)
  }
  child_bot = bot$child_bots[[bot$env$current_child]]
  child_bot$choose_action(bot=child_bot, play=play,...)
}

