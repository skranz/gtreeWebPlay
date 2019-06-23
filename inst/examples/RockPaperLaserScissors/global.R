# In RStudio you can directly press "Run App" best run it in the viewer pane.
#

library(gtreeWebPlay)

# Specify game
# Laser beats rock and paper but is bet my shiny scissors
# Otherwise as in rock paper scissors
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
        pi1 ~ -1+2*(
          (a1=="r" & a2 %in% c("s","m")) |
          (a1=="p" & a2 %in% c("r","m")) |
          (a1=="s" & a2 %in% c("p","m")) |
          (a1=="l" & a2 %in% c("r","p","s")) |
          (a1=="m" & a2=="l")
        ) + (a1==a2),
        payoff_1 ~ pi1,
        payoff_2 ~ -payoff_1
      )
    )
  )
)

oco = get_outcomes(game)
View(oco)
# Solve for equilibrium
game %>%
  game_gambit_solve(mixed=TRUE) %>%
  eq_expected_outcomes()

game %>%  eq_tables()

# Try to find all equilibria
game %>%
  game_gambit_solve("gambit-enummixed -q -d")

eq_expected_outcomes()


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
