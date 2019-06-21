# gtreeWebExp: Create web apps to play gtree games in the web

Author: Sebastian Kranz, Ulm University

```r
library(gtreeWebPlay)
game = new_game(
  gameId = "UltimatumGame",
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


```
