library(gtreeWebPlay)

# Set to directory of this app
setwd(system.file("examples/ThreeCardPoker", package="gtreeWebPlay"))

# The file game_create.R creates and solves the game
# For speed purposes, we just load the solved game.
game = readRDS("game.Rds")

# pps (population play summary) describes how the previous
# web app users have played so far
if (!file.exists("pps.Rds")) {
  pps = new_pps(game)
} else {
  pps = readRDS("pps.Rds")
}

# bot_pop samples from the play of previous app users
bots = make_bots(game, bot_pop,pps=pps, alt.bot.fun=bot_eq)

# pre.page.handler is called before a page is shown to the human player
# Returns a list with specified variables that will be available in the
# page.
pre.page.handler = function(values, wp,is.end,human=wp$human,...) {
  # If the game is finished
  # update by reference wp$custom values
  if (is.end) {
    wp$custom$rounds = wp$custom$rounds+1
    wp$custom$total.win = wp$custom$total.win+values$payoffs[human]
  }

  # Return card vector, card names and card symbols (as large UTF-8 font)
  # These variable are used in the pages specified in the pages subdirectory
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

# The post.page.handler is called after each stage with human input
post.page.handler = function(wp,play,...) {
  # Here we update the pps (population play summary) object
  # by the choices the human player just had made
  pps_add_play_actions(wp$pps, play, stage.num=play$human.stage.finished)

  # Set a flag to globally (for all app instances)
  # mark that pps can be saved in the next cycle
  app = getApp()
  app$glob$pps.changed = TRUE
}

# The main web play argument
wp = new_wp(game, bots, human = 1, verbose=TRUE,pre.page.handler = pre.page.handler, post.page.handler = post.page.handler, custom=list(rounds = 0, total.win=0), pps=pps)

# Define a shinyEvents app
app = eventsApp()

# The ui of the shiny app. We have a play and help panel.
app$ui = fluidPage(
  tabsetPanel(
    tabPanel("Play",
      # comment out next line in production version
      wp_developer_ui(), # developer toolbar
      # output in which web play will be shown
      uiOutput(wp$wpUI)
    ),
    tabPanel("Help",
      HTML(readLines("help.html"))
    )
  )
)

# The function in appInitHandler will be called when a new app
# instance starts.
appInitHandler(function(..., app=getApp()) {
  # Assign a deep copy of the web play object
  # to this app instance
  set_wp_for_app(wp, app)
  # Start playing
  wp_continue()
})

# global observe that check all
# 5 seconds whether the pps
# file is updated and saves it if that
# is the case.
observe(priority = -10000L,{
  # Call again in 5 seconds
  invalidateLater(4000)

  if (isTRUE(app$glob$pps.changed)) {
    cat("\npps saved!")
    app$glob$pps.changed = FALSE
    saveRDS(get_wp()$pps,"pps.Rds")
  }
})

# This line must be called at the end of the
# global.R file of a shinyEvents app
appReadyToRun()

# View app manually in RStudio Viewer Pane
# viewApp(app)
