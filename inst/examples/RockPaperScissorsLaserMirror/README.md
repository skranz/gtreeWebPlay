This is an example app for Rock-Paper-Scissors-Laser-Mirror.

For an introduction first take a look at "UltimatumGame" and "KuhnPoker"

Look at `global.R` for the code with detailed comments.

Like KuhnPoker this example allows to play against the total population.

Special features compared to KuhnPoker are:

- There are two treatments. In one players see a table of the historical play frequencies in the other not. We let players play against a common population from both treatments. Treatments are randomly selected when the app starts.

- The example illustrates how you could provide a page.ui.fun if you want to create  page UIs directly in R instead of using the Rmd pages approach.
