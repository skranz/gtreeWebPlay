# gtreeWebExp: Create web apps to play gtree games in the web

Author: Sebastian Kranz, Ulm University

This packages is an companion package to [gtree](skranz.github.io/gtree). `gtree` allows to specify and solve game theoretic games in a way akin to the specification of economic experiments via stages in [ztree](https://www.ztree.uzh.ch/en.html). `gtreeWebExp` allows to create simple shiny web apps from such games in which a single player can play against *bots*. A bot could e.g. follow an equilibrium strategy (see  [bot_eq](file:///D:/libraries/gtree/gtreeWebPlay/docs/reference/bot_eq.html)) or allow the current player to play against randomly drawn strategies of earlier users of the app (see [bot_pop]()). The appearance of the shown stages can be customized by adapting RMarkdown files that are initially automatically generated.

Below is an embedded shiny app that allows you to play [Kuhn Poker](https://en.wikipedia.org/wiki/Kuhn_poker) against randomly drawn earlier players:

<div align="center" style="width: 80%; padding-left: 1em; height: 30em;">
<iframe align="center" style="width: 100%; height: 100%; border-width: 0; border-style: solid;" src="http://econ.mathematik.uni-ulm.de:3111/KuhnPoker"/>
</iframe>
</div>

## Installation

To install the package will all dependencies simply run
```r
install.packages("gtreeWebPlay",repos = c("https://skranz-repo.github.io/drat/",getOption("repos")))
```
(I have my own [drat](https://cran.r-project.org/web/packages/drat/index.html) powered R repositorium for my own packages. While CRAN ist great, I find it too time consuming to maintain all my packages there and `devtools::install_github` does not handle custom dependencies as easily.)

## Deploy an Example App

The best way to get started is to look at an example and modify it for your own purposes. The following code copies all files for a simple Ultimatum Game app to the directory specified in `dest.dir`.
```r
deploy_webplay_example(example="UltimatumGame",dest.dir = "D:/gtreeExamples/UltimatumGame")
```
You can open the `global.R` file in the destination directory and then press in RStudio `Run App` to run the example. You can adapt the example to create an app for your own game. A more complex example using a population bot can be deployed with the argument `example="KuhnPoker"`.
