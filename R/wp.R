# wp stands for web play
#
# It is a global object that manages the state of play
# possible for several human players
#
# A specific app object has a reference to an wp
# and a player field

wp.game = function(wp=get_wp()) {
  wp$play$game
}

wp.stage = function(wp=get_wp()) {
  stage.num = wp$stage.num
	if (stage.num < 1 | stage.num > length(wp$vg$stages)) return(NULL)
	wp$vg$stages[[stage.num]]
}


wp.pages.dir = function(wp=get_wp()) {
  first.non.null(wp$pages.dir,get.pages.dir(wp.game(wp)))
}

wp.page.name = function(wp=get_wp()) {
  stage.num = wp$stage.num
  if (stage.num==0) {
    return("start-page")
  } else if (stage.num > length(wp$vg$stages)) {
    return("end-page")
  } else {
    return(wp.stage(wp)$name)
  }
}

# Get rmd file of currently shown page
wp.page.file = function(wp=get_wp(), copy.auto=FALSE, make.auto=TRUE) {
  page.name = wp.page.name(wp)
  page.dir = wp.pages.dir(wp)
  file = file.path(page.dir, paste0(page.name,".Rmd"))
  if (file.exists(file)) return(file)
  auto.file = file.path(page.dir, paste0(page.name,".auto.Rmd"))
  if (!file.exists(auto.file) & make.auto) {
    make.page.rmd(wp.game(wp),page.name = page.name, stage=wp.stage(wp))
  }
  if (file.exists(auto.file)) {
    if (copy.auto) {
      file.copy(auto.file, file)
      return(file)
    }
    return(auto.file)
  }
  return(NULL)
}


#' Create a new web play object
#'
#' @param game A gtree game generated with \code{\link[gtree]{new_game}}.
#' @param bots A list with one bots for every player. Also add a bot for the human player. You can call \code{\link{make_bots}} to conveniently create the bots.
#' @param human index of the player that is played by a human in the web app.
#' @param mainUI
#' @family Bots
#' @family Web Play
new_wp = function(game,bots,human=1, wpUI="wpUI", verbose=FALSE, pages.dir = file.path(getwd(),"pages"),custom=list(), pre.page.handler = NULL,post.page.handler = NULL, finish.handler = wp.default.finish.handler,...) {
	restore.point("new.wp")

  play = new_play(game,bots, human)
	wp = as.environment(list(play=play, vg=game$vg, stage.num=0, human=human, wpUI=wpUI, num.stages = length(game$vg$stages), verbose=verbose, pages.dir = pages.dir,custom=custom, pre.page.handler = pre.page.handler, post.page.handler=post.page.handler,  finish.handler=finish.handler,...))
	wp
}

#' Reset a web play to the start of a new play
#'
#' If you immediately want to start the new play. Call
#' \code{\link{wp_continue}} afterwards.
#'
#' @param wp A web play object
#' @param bots You can provide new bots. By default the current bots are used again.
#' @param human You can define a new index of the human player. By default the current human is used.
#' @family Web Play
wp_reset = function(wp=get_wp(), bots=wp$play$bots, human=wp$human) {
  restore.point("wp_reset")
  wp$play = new_play(wp$play$game,bots, human)
  wp$stage.num = 0
  wp$human = human
  wp$bots = bots
  invisible(wp)
}

#' Copy a web play object
#' @familiy Web Play
wp_copy = function(wp) {
  as.environment(as.list(wp))
}

# Is called by wp_continue_play
# Play all moves of nature and bots of a web play
# up until the human player sees again a stage.
#
# @param wp the web play object
# @family Web Play
wp_play_until_human = function(wp) {
  restore.point("wp.play.until.human")
  play = wp$play
  game = wp.game(wp)

  while(
    ((play$is.human.stage==FALSE) |
     (play$human.stage.finished >= play$auto.stage.finished)) &
    (play$auto.stage.finished < wp$num.stages)
  ) {
    if (wp$verbose) {
      num = play$auto.stage.finished+1
      cat("\nPlay auto stage ",num, " ", wp$vg$stages[[num]]$name,"\n")
    }
    play = play_stage_auto(play)
    if (wp$verbose) {
      li = c(list(.cond=play$.last.condition,.player=paste0(play$.last.player, collapse=", "),is.human = play$is.human.stage), play$hist)
      print(as.data.frame(li, row.names=FALSE))
      cat("\n")
    }

  }
  wp$play = play
  if (play$is.human.stage & (play$auto.stage.finished > play$human.stage.finished)) {
    wp$stage.num = play$auto.stage.finished
    if (wp$verbose) {
      cat("\nWait for human input in stage", wp$stage.num, wp$vg$stages[[wp$stage.num]]$name)
    }

  } else {
    wp$stage.num = wp$num.stages +1
    if (wp$verbose) {
      cat("\nAll stages finished.")
    }

  }
  return(invisible(wp))
}

#' Sets the state of a web play to a play object
#'
#' Can for example be used to continue with a human
#' after bots played some earlier rounds
#' @family Web Play
wp_set_to_play = function(wp, play, human=play$human) {
  if (human != play$human) {
    play$human = human
    play$bot.player = setdiff(play$game$players,human)
  }
  wp$play = play
  wp$human = play$human
  if (play$auto.stage.finished > play$human.stage.finished) {
    wp$stage.num = play$auto.stage.finished
  } else {
    wp$stage.num = play$human.stage.finished +1
  }
  invisible(wp)
}

