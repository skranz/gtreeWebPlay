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
#' @param human index of the player that is played by a human in the first play of the web app.
#' @param human_draw_method Method how the index of the human player is determined by default if a new play is started. The default \code{"cycle"} lets the human cycle through all players. \code{"random"} picks a random player, and \code{"fixed"} keeps the current player.
#' @param wpUI the id of the \code{uiOutput} element in the app ui where the web play will be shown.
#' @param verbose shall information about state of play be printed to the standard output?
#' @param pages.dir the directory in which the Rmd files for the stage pages can be found. By default \code{"./pages"}.
#' @param custom A list of custom parameters that will be passed to handlers.
#' @param pre.page.handler a function that is called before a page is shown to a human. It should return a list of values that can be accessed in the whiskers {{ }} of the page Rmd file.
#' @param post.page.handler a function that is called after a human made input in a stage. Can for example be used to update a population play summary. (See the KuhnPoker example)
#' @param finish.handler is called when the final results page of a play is left. The default handler simply starts a new play.
#' @param page.ui.fun optionally a function that returns for each page a shiny tag that will be shown. If NULL (default) we specify the page ui via Rmd files in the pages subfolder.
#' @family Web Play
new_wp = function(game,bots,human=draw_human_pos(human_draw_method=human_draw_method,numPlayers=game$vg$params$numPlayers, human=0),human_draw_method = c("cycle","random","fixed")[1], wpUI="wpUI", verbose=FALSE, pages.dir = file.path(getwd(),"pages"),custom=list(), pre.page.handler = NULL,post.page.handler = NULL, finish.handler = wp.default.finish.handler, comp.pages = as.environment(list()), page.ui.fun=NULL,  ...) {
	restore.point("new.wp")

  play = new_play(game,bots, human)
  stage_secs = rep(NA_real_, length(game$vg$stages))
  names(stage_secs) = names(game$vg$stages)

	wp = as.environment(list(play=play, vg=game$vg, stage.num=0, human=human,human_draw_method=human_draw_method, wpUI=wpUI, num.stages = length(game$vg$stages), verbose=verbose, pages.dir = pages.dir,custom=custom, pre.page.handler = pre.page.handler, post.page.handler=post.page.handler,  finish.handler=finish.handler, stage_secs=stage_secs,comp.pages=comp.pages, page.ui.fun=page.ui.fun,...))
	wp
}

draw_human_pos = function(wp=NULL, human_draw_method=wp$human_draw_method, numPlayers=wp$vg$params$numPlayers, human = wp$human) {
  restore.point("draw_human_pos")
  if (human_draw_method == "cycle") {
    human = human+1
    if (human > numPlayers) human = 1
  } else if (human_draw_method == "random") {
    human = sample.int(numPlayers, 1)
  } else if (human_draw_method=="fixed") {
    if (human < 1) human = 1
  }
  human
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
wp_reset = function(wp=get_wp(), bots=wp$play$bots, human=draw_human_pos(wp)) {
  restore.point("wp_reset")
  wp$play = new_play(wp$play$game,bots, human)
  wp$stage.num = 0
  wp$human = human
  wp$bots = bots
  wp$stage_secs[] = NA
  invisible(wp)
}

#' Copy a web play object
#' @family Web Play
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


