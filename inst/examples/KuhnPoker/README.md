This is a more advanced shiny app to play Kuhn Poker.

Look at `global.R` for the main code with detailed comments.

Some of the feature of this app:

- Count number of rounds and total wins.

- The pages that specify stages and final results are heavily customized (see pages subfolder).

- Results of all plays are saved in a csv file.

- Users play against the population of earlier players using a population bot created with `bot_pop`. If not enough data is available for some history, they play against equilibrium strategies.
