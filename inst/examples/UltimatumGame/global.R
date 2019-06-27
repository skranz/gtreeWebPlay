# In RStudio you can directly press "Run App" best run it in the viewer pane.
#
# Possible manually change the working directory to your local directory
# setwd()

library(gtreeWebPlay)
disable.restore.points(TRUE) # To speed up everything

# Specify UltimatumGame
game = new_game(
  gameId = "UltimatumGame",
  options =make_game_options(verbose=FALSE),
  params = list(numPlayers=2,cake=10),
  stages = list(
    stage("proposerStage",
      player=1,
      actions = list(
        action("offer",~0:cake)
      )
    ),
    stage("responderStage",
      player=2,
      observe = "offer",
      actions = list(
        action("accept",c(FALSE,TRUE))
      )
    ),
    stage("PayoffStage",
      compute=list(
        payoff_1 ~ ifelse(accept, cake-offer,0),
        payoff_2 ~ ifelse(accept, offer,0)
      )
    )
  )
)

# Solve for equilibria to specify equilibrium bots

# Alternative 1: A equilibrium bot assuming inequality averse players
game %>%
  game_set_preferences(pref_ineqAv(alpha=1, beta=0.3)) %>%
  game_solve() %>%
  eq_tables()

# Alternative 2: A QRE equilibrium with lambda = 0.8.
# Assuming inequality averse preferences
# You need to have Gambit installed
# and set gambit dir to the locaction of its command line
# tools (or specify Gambit on the system PATH).
game %>%
  game_set_preferences(pref_ineqAv(alpha=1, beta=0.3)) %>%
  game_gambit_solve(qre.lambda=0.8, gambit.dir="", verbose=TRUE) %>%
  eq_tables()

# Bots that follow equilibrium play
# You can also use other bots like bot_random, bot_pop etc.
bots = make_bots(game, bot_eq)

# The main web play argument
wp = new_wp(game, bots)

# Define a shinyEvents app
app = eventsApp()

# The ui of the shiny app. We have a play and help panel.
app$ui = fluidPage(
  # The developer UI is helpful to design your
  # stage pages. Just press on "Edit Page" to
  # open and change the Rmd file
  wp_developer_ui(), # comment out in development version
  # output in which web play will be shown
  uiOutput(wp$wpUI)
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

# This line must be called at the end of the
# global.R file of a shinyEvents app
appReadyToRun()
