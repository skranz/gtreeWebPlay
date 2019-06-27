library(gtreeWebPlay)
disable.restore.points(TRUE) # To speed up everything


# The file game_create.R creates and solves the game
# For speed purposes, we just load the solved game.
game = readRDS("game.Rds")

# pps (population play summary) describes how the previous
# web app users have played so far
if (!file.exists("data/pps.Rds")) {
  pps = new_pps(game)
} else {
  pps = readRDS("data/pps.Rds")
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
    restore.point("pre.page.last.page")

    wp$custom$rounds = wp$custom$rounds+1
    wp$custom$total_win = wp$custom$total_win+values$payoffs[human]

    # Add data of current play to plays.csv

    # Duration of each human stage in seconds
    secs = as.list(wp$stage_secs)
    names(secs) = paste0("secs_",names(secs))

    dat = c(list(sessionId=getApp()$sessionId, start_time=wp$start.time, end_time=Sys.time(), rounds=wp$custom$round, total_win=wp$custom$total_win), wp$play$hist, secs) %>% as.data.frame
    file = "data/plays.csv"
    new = !file.exists(file)
    write.table(dat,file = file, append=!new,col.names=new, row.names=FALSE, sep=",")

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
wp = new_wp(game, bots, human = 1, verbose=TRUE,pre.page.handler = pre.page.handler, post.page.handler = post.page.handler, custom=list(rounds = 0, total_win=0), pps=pps)

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

# Check every minute whether pps has been changed
# and save it if that is the case
timerHandler("save_pps_handler",1000*60*1, function(app=getApp(),...) {
  # Call all 2 minutes seconds
  cat("\nCalled save_pps_handler")
  restore.point("jdoidfjoidjf")
  if (isTRUE(app$glob$pps.changed)) {
    #restore.point("dlfjdlfj")
    cat("\npps saved!")
    app$glob$pps.changed = FALSE
    pps = get_wp()$pps
    if (!is.null(pps))
      saveRDS(pps,"data/pps.Rds")
  }
  cat("\nEnd save_pps_handler")
})

# The function in appInitHandler will be called when a new app
# instance starts.
appInitHandler(function(..., app=getApp()) {
  # Random sessionId
  app$sessionId = random.string(1, 20)
  # Assign a deep copy of the web play object
  # to this app instance
  set_wp_for_app(wp, app)
  # Start playing
  wp_continue()
})



# This line must be called at the end of the
# global.R file of a shinyEvents app
appReadyToRun()

# View app manually in RStudio Viewer Pane
# viewApp(app)
