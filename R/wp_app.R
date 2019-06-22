examples.wpDevelApp = function() {
  setwd(system.file("examples/ThreeCardPoker", package="gtreeWebExp"))
  game = readRDS("game.Rds")
  tables = eq_tables(game)
  bots = make_bots(game, bot_eq)

  pre.page.handler = function(values, wp,is.end,human=wp$human,...) {
    # Update by reference wp$custom values
    if (is.end) {
      wp$custom$rounds = wp$custom$rounds+1
      wp$custom$total.win = wp$custom$total.win+values$payoffs[human]
    }
    # Return card vector and card names
    cards = c(values$card1,values$card2)
    list(
      cards = cards,
      card_lab = c(
        "a <b>Jack</b>",
        "a <b>Queen</b>",
        "an <b>Ace</b>"
      )[cards],
      card_sym = c(
        "<span style='font-size=200%'>&#x1F0AB;</span>",
        "<span style='font-size=200%'>&#x1F0AD;</span>",
        "<span style='font-size=200%'>&#x1F0A1;</span>"
      )[cards]
    )
  }
  post.page.handler = function(wp,play,...) {
    app = getApp()
  }

  wp = new_wp(game, bots, human = 1, verbose=TRUE,pre.page.handler = pre.page.handler, post.page.handler = post.page.handler, custom=list(rounds = 0, total.win=0))


  app = eventsApp()
  app$ui = fluidPage(
    h4("Playing 3-Card Poker"),
    wp_developer_ui(),
    uiOutput(wp$wpUI)
  )
  appInitHandler(function(..., app=getApp()) {
    set_wp_for_app(wp, app)
    wp_continue()
  })
  viewApp(app)
}

#' This function should be called in the appInitHandler
#' of your shinyEvents app
#'
#' Assigns a copy of a global web play object to the
#' current instance of the shiny app. This means every
#' user has her own instance. Note that it is not possible
#' to have two or more web plays active in the same app.
#'
#' The function get_wp() returns the web play object of the current app instance.
#' @family Web Play
set_wp_for_app = function(wp, app=getApp(), copy=TRUE) {
  if (copy) wp = wp_copy(wp)
  app$wp = wp
}

#' Gets the web play object of the current app instance
#'
#' @seealso set_wp_for_app
#' @family Web Play
get_wp = function(app=getApp()) {
	app[["wp"]]
}


#' Create a simple app for testing and developing a gtree web play
#'
#' Returns a shinyEvents app. You can view the app in RStudio using \code{\link{[shinyEvents]viewApp}}
#'
#' @param wp a web play object generated with \code{\link{new_wp}}
#' @param title A title string
#' @family Web Play
wpDevelApp = function(wp, title = paste0("Playing ", wp$play$game$gameId)) {
  restore.point("webExpTestApp")

  app = eventsApp()
  app$ui = fluidPage(
    h4(title),
    wp_developer_ui(),
    uiOutput(wp$wpUI)
  )
  app$glob$wp = wp
  appInitHandler(function(..., app=getApp()) {
    set_wp_for_app(wp, app)
    wp_continue()
  })
  app
}

# Play all moves of nature and bots of a web play
# up until the human player sees again a stage.
# Then show that stage in shiny app
#
# @family Web Play
wp_continue = function(wp=get_wp()) {
  restore.point("wp_continue")
  if (wp$stage.num >0) {
    wp$stage_secs[wp$stage.num] = as.numeric(Sys.time())-as.numeric(wp$stage.start.time)
  } else {
    wp$start.time = Sys.time()
  }
  wp_play_until_human(wp)
  ui = make.wp.page.ui(wp)
  wp$stage.start.time = Sys.time()
  dsetUI(wp$wpUI, ui)
  setUI(wp$wpUI, ui)
  invisible(wp)
}


wp.submit.btn.click = function(formValues, stage.name,action.ids,sm.ids, ..., wp=get_wp()) {
	restore.point("wp.submit.btn.click")
	cat("\nsubmit.btn.clicked!\n")

	stage = wp.stage(wp)
  page.name = wp.page.name(wp)

	ids = c(action.ids, sm.ids)
	for (id in ids) {
		if (isTRUE(length(formValues[[id]])==0) |  isTRUE(formValues[[id]]=="")) {
			errorMessage(page.ns(page.name)("msg"),"Please make all required choices before you continue.")
			return()
		}
	}

	if (length(formValues)>0 & length(ids)>0) {
		avals = lapply(formValues[ids], convert.atom)
		wp$play$hist[names(ids)] = avals

		#wp.assign.delayed.strat.meth.realizations(wp=wp)
		#wp.assign.strat.meth.realizations(wp=wp, actions=stage$actions)
	}
  wp$play$human.stage.finished = wp$stage.num
  if (!is.null(wp$post.page.handler)) {
    wp$post.page.handler(wp=wp, play=wp$play)
  }

  wp_continue(wp)
}

wp.default.finish.handler = function(wp,...) {
  wp_reset(wp)
  wp_continue(wp)
}


#' A developer toolbar to your web play app
#'
#' Returns a shiny tag list that you can add to your app$ui definition. It contains buttons to restart the experiment, edit the page rmd file in RStudio and to refresh an edited page.
#' Button handlers are automatically added.
#' @family Web Play
wp_developer_ui = function() {
  ui = tagList(
    simpleButton("develEditPageBtn","Edit Page", class.add="btn-xs"),
    simpleButton("develRefreshPageBtn","Refresh Page", , class.add="btn-xs"),
    simpleButton("develStartPlayBtn","Restart", class.add="btn-xs"),
    simpleButton("develChangePlayerBtn","Change Player", class.add="btn-xs")
  )
  buttonHandler("develStartPlayBtn", function(..., app=getApp()) {
    wp = get_wp()
    wp_reset(wp=wp, human=wp$human)
    wp_continue()
  })
  buttonHandler("develChangePlayerBtn", function(..., app=getApp()) {
    wp = get_wp()
    human = wp$human +1
    if (human > wp$vg$params$numPlayers) {
      human = 1
    }
    wp_reset(wp,human=human)
    wp_continue()
  })

  buttonHandler("develEditPageBtn",function(...) {
    wp = get_wp()
    if (is.null(wp)) return()
    page.file = wp.page.file(wp,make.auto = TRUE, copy.auto=TRUE)
    rstudioapi::navigateToFile(page.file)
  })
  buttonHandler("develRefreshPageBtn",function(...) {
    wp = get_wp()
    if (is.null(wp)) return()
    ui = make.wp.page.ui(wp)
    dsetUI(wp$wpUI, ui)
    setUI(wp$wpUI, ui)
  })

  ui
}


wp.edit.page.btn.click = function(wp=get_wp(),...) {
  if (is.null(wp)) return()
  page.file = wp.page.file(wp,make.auto = TRUE, copy.auto=TRUE)
  rstudioapi::navigateToFile(page.file)
}
