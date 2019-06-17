This is a more advanced shiny app to play Three-Card Poker games.

Look at `global.R` for the main code with detailed comments.

Some of the feature of this app:

- The pages that specify stages and final results are heavily customized (see pages subfolder).

- Users play against the population of earlier players using `bot_pop`. If not enough data is available for some history, they play against equilibrium strategies.

- All past plays are stored in a SQLite database for possible later analysis. Also the underlying data used by the `bot_pop` is regularly updated.
