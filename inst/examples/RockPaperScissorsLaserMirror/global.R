# In RStudio you can directly press "Run App" best run it in the viewer pane.

library(gtreeWebPlay)
library(kableExtra) # To show nice tables

disable.restore.points(TRUE) # set to TRUE for performance

# The file game_create.R creates and solves the game
# For speed purposes, we just load the solved game.
game = new_game(
  gameId = "RockPaperScissorsLaserMirror",
  options =make_game_options(verbose=FALSE),
  params = list(numPlayers=2),
  stages = list(
    stage("player1",
      player=1,
      actions = list(
        action("a1",set=c("r","p","s","l","m"))
      )
    ),
    stage("player2",
      player=2,
      actions = list(
        action("a2",set=c("r","p","s","l","m"))
      )
    ),
    stage("PayoffStage",
      compute=list(
        payoff_1 ~ -1+2*(
          (a1=="r" & a2 %in% c("s","m")) |
          (a1=="p" & a2 %in% c("r","m")) |
          (a1=="s" & a2 %in% c("p","m")) |
          (a1=="l" & a2 %in% c("r","p","s")) |
          (a1=="m" & a2=="l")
        ) + (a1==a2),
        payoff_2 ~ -payoff_1
      )
    )
  )
)

# pps (population play summary) describes how the previous
# web app users have played so far
if (!file.exists("data/pps.Rds")) {
  pps = new_pps(game)
} else {
  pps = readRDS("data/pps.Rds")
}

# bot_pop samples from the play of previous app users
bots = make_bots(game, bot_pop,pps=pps, alt.bot.fun=bot_random, alt.bot.count=1)


set = c("r","p","s","l","m")
labels = c(r="rock",p="paper",s="scissors",l="laser",m="mirror")
index = 1:5 %>% set.names(set)
freq.df = data_frame(action=labels,you=0, opponent=0)

# pre.page.handler is called before a page is shown to the human player
# Returns a list with specified variables that will be available in the
# page.
pre.page.handler = function(values, wp,is.end,human=wp$human,...) {
  restore.point("pre.page.handler")
  # If the game is finished
  # update by reference wp$custom values
  a = c(values$a1,values$a2)
  if (is.end) {
    restore.point("pre.page.last.page")

    # Update frequences of play
    ind.you = index[ a[human]]
    ind.bot = index[ a[3-human]]
    wp$freq.df[ ind.you,2] = wp$freq.df[ ind.you,2]+1
    wp$freq.df[ ind.bot,3] = wp$freq.df[ ind.you,3]+1

    # Update total payoffs and rounds played
    wp$rounds = wp$rounds+1
    wp$total_payoff = wp$total_payoff+values$payoffs[human]

    # Add data of current play to plays.csv

    # Duration of each human stage in seconds
    secs = as.list(wp$stage_secs)
    names(secs) = paste0("secs_",names(secs))

    dat = c(list(sessionId=getApp()$sessionId, start_time=wp$start.time, end_time=Sys.time(), show_hist=wp$show_hist, human=human, rounds=wp$rounds, total_payoff=wp$total_payoff), wp$play$hist, secs) %>% as.data.frame
    file = "data/plays.csv"
    new = !file.exists(file)
    write.table(dat,file = file, append=!new,col.names=new, row.names=FALSE, sep=",")

  }

  # Return a label vector
  list(
    a = a,
    labels = c(r="rock",p="paper",s="scissors",l="laser",m="mirror")
  )
}

# If you don't like to work with the Rmd files that specify the pages. It is also possibe to specify the game UI via function page.ui.fun that returns a shiny tag.
# Here is an example.
page.ui.fun = function(wp, page.name, values,...) {
  # Only use this function for player stage otherwise
  # use the Rmd pages approach, by calling default.page.ui
  if (page.name != "player2" & page.name != "player1") {
    return(default.page.ui.fun(wp, page.name, values))
  }

  restore.point("my.page.ui.fun")
  action.name = ifelse(page.name=="player2","a2","a1")

  # If we are in the show_hist treatment
  # show the frequency table. Otherwise not
  if (wp$show_hist) {
    freq.tab = wp$freq.df %>%
      rename("Actions count"=action) %>%
      kable("html") %>%
      kable_styling() %>%
      HTML

    inner.ui = tags$table(tags$tr(
      tags$td(style="padding-left: 0em;", actionField(name=action.name, label="Choose your action:", choiceLabels = values$labels, as.tag=TRUE, width="12em")),
      tags$td(style="padding-left: 2em;",freq.tab)
    ))
  } else {
    inner.ui = actionField(name=action.name, label="Choose your action:", choiceLabels = values$labels, as.tag=TRUE, width="12em")
  }

  tagList(
    inner.ui,
    submitPageBtn("Press to proceed", as.tag=TRUE)
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
wp = new_wp(game, bots, human = 1,human_draw_method = "cycle", verbose=TRUE,pre.page.handler = pre.page.handler, post.page.handler = post.page.handler, page.ui.fun=page.ui.fun, rounds = 0, total_payoff=0, freq.df = freq.df, pps=pps)

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

# Check all 10 seconds whether pps has been changed
# and save it if that is the case
timerHandler("save_pps_handler",1000*1, function(app=getApp(),...) {
  # Call all 2 minutes seconds
  cat("\nCalled save_pps_handler")
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
  wp = set_wp_for_app(wp, app)
  wp$show_hist = sample(c(TRUE, FALSE),1,prob = c(0.7,0.3))
  # Start playing
  wp_continue()
})




# This line must be called at the end of the
# global.R file of a shinyEvents app
appReadyToRun()

# View app manually in RStudio Viewer Pane
# viewApp(app)
