# A bot to play against the population
#
# Helper functions to create population play summaries pps
#
# A pps is an environment
#
# It contains of several population play objects ppo



#' Bot who mimics the average player population
#'
#' Draws actions from previous actions stored
#' in population play summary (pps) object
#'
#' @param game the game object
#' @param player the player number of this bot
#' @param pps a pps object, can be extended during play.
#' @param alt.bot a bot who will be aksed if there are too few observations in the popoulation
#' @param alt.bot.count we assume that we already have this many observations for alt.bot. This determines the probability to draw from the alt.bot instead of the population
#' @family Bots
#' @family population play functions
bot_pop = function(game, player, pps, alt.bot = NULL, alt.bot.count = 5, name="pop_bot", alt.bot.fun = bot_random,...) {
  if (is.null(alt.bot)) {
    alt.bot = alt.bot.fun(game=game, player=player,...)
  }
  restore.point("bot_pop")
  bot = list(
    name = name,
    player = player,
    choose_action = choose_action_bot_pop,
    pps = pps,
    alt.bot = alt.bot,
    alt.bot.count=alt.bot.count
  )
  bot
}
choose_action_bot_pop = function(bot, play,player, stage, action,set,pps=bot$pps, alt.bot=bit$alt.bot,alt.bot.count=bot$alt.bot.count,...) {
  restore.point("choose_action_bot_pop")
  df = pps[[stage$name]]

  known.vars = play$known.vars[[player]]
  keys = setdiff(known.vars, names(stage$actions))
  if (length(keys)>0 & NROW(df)>0) {
    df = semi_join(df, as_tibble(play$hist), by=keys)
  }

  # No observations use alt.bot
  if (NROW(df)==0) {
    val = do.call(alt.bot$choose_action, c(list(bot=alt.bot,play=play, action=action, stage=stage, set=set, player=player)))
    return(val)
  }
  # No observations for the current stage yet
  restore.point("choose_action_bot_pop_2")

  total.count = sum(df$.count)
  from.alt.bot = sample(c(TRUE,FALSE),1, prob=c(alt.bot.count, total.count))
  if (from.alt.bot) {
    val = do.call(alt.bot$choose_action, c(list(bot=alt.bot,play=play, action=action, stage=stage, set=set, player=player)))
  } else {
    val = sample(df[[action$name]], 1, prob = df$.count)
  }
  return(val)
}


#' Create a new empty population play summary
#'
#' @family population play functions
new_pps = function(...) {
  pps = as.environment(list())
  class(pps) = c("gtree_pps","environment")
  pps
}

#' Order pps columns naturally
#'
#' If the pps is dynamically created during plays
#' the column order may change
#'
#' @family population play functions
pps_rearrange = function(pps) {
  # Create empty tables for every stage
  vars = NULL
  stages = game$vg$stages
  for (stage in stages) {
    vars = unique(c(vars, names(stage$nature), names(stage$compute), names(stage$actions)))
    if (length(stage$actions)>0) {
      df = pps[[stage$name]]
      if (!is.null(df)) {
        cols = c(vars, ".count")
        df = df[,cols]
        df = arrange_at(df, cols)
        pps[[stage$name]] = df
      }
    }
  }
  pps
}

print.gtree_pps = function(pps,...) {
  print(as.list(pps))
}

#' Call this function in the post.page.handler
#' to update the population play summary
#'
#' @family population play functions
pps_add_play_actions = function(pps,play, stage.num = play$human.stage.finished) {
  restore.point("pps_add_play_actions")

  stage = play$game$vg$stages[[stage.num]]
  player = play$.last.player
  #known.vars = play$known.vars[[player]]
  action.vars = names(stage$actions)
  if (length(action.vars)==0)
    return(pps)
  key.vars = setdiff(names(play$hist), c(names(play$game$vg$params),action.vars))

  df = pps[[stage$name]]
  if (is.null(df)) {
    pps[[stage$name]] = as_tibble(c(
      play$hist[c(key.vars, action.vars)],
      list(.count = 1L)
    ))
  } else {
    val.li = play$hist[c(key.vars, action.vars)]
    if (NROW(df)==0) {
      df = bind_rows(df, as_tibble(c(val.li, list(.count=1L))))
      pps[[stage$name]] = df
    } else {
      row = multi.key.match(val.li, df, keys=key.vars)
      if (!is.na(row)) {
        pps[[stage$name]][[".count"]][row] = pps[[stage$name]][[".count"]][row]+1L
      } else {
        df = bind_rows(df, as_tibble(c(val.li, list(.count=1L))))
        pps[[stage$name]] = df
      }
    }
  }
  pps
}




