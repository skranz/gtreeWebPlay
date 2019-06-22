examples.make.pages = function() {
  setwd("D:/libraries/gtree/webexp/KuhnPoker")
  game = readRDS("game.Rds")
  make.game.pages(game)
}



get.pages.dir = function(game) {
  first.non.null(game$options$pages.dir,getOption("gtree.pages.dir"), file.path(getwd(),"pages"))
}

load.page.rmd = function(game, page.name, stage=NULL, pages.dir = get.pages.dir(game=game), file=NULL, make.if.missing = TRUE, remake.auto = !TRUE) {
	restore.point("load.page.rmd")
	if (is.null(file)) {
		file = paste0(page.name,".Rmd")
		if (!file.exists(file.path(pages.dir,file)) & !remake.auto)
			file = paste0(page.name,".auto.Rmd")
	}

	if (!file.exists(file.path(pages.dir,file))) {
		if (make.if.missing) {
		  page = make.page.rmd(game, page.name=page.name, pages.dir=pages.dir)
		} else {
			stop(paste0("Page ", page,".Rmd for game ", game$gameId, " does not exist in folder ", pages.dir))
		}
	} else {
		page = readLines(file.path(pages.dir,file))
	}
	return(merge.lines(page))
}

make.page.rmd = function(game, page.name, stage=NULL, pages.dir = get.pages.dir(game=game)) {
	restore.point("make.page.rmd")
  if (page.name=="start-page") {
    page = make.start.page.rmd(game,pages.dir = pages.dir)
  } else if (page.name=="end-page") {
    page = make.end.page.rmd(game,pages.dir = pages.dir)
  } else if (page.name=="wait-page") {
    page = make.wait.page.rmd(game,pages.dir = pages.dir)
  } else {
    page = make.stage.page.rmd(game=game,stage=stage, pages.dir = pages.dir)
  }
	return(invisible(merge.lines(page)))
}


make.game.pages.rmd = function(game,pages.dir = get.pages.dir(game)) {
  make.start.page.rmd(game, pages.dir=pages.dir)
	for (stage in game$vg$stages) {
		make.stage.page.rmd(game,stage, pages.dir=pages.dir)
	}
  make.end.page.rmd(game, pages.dir=pages.dir)
  invisible(NULL)
}

make.start.page.rmd = function(game, pages.dir = get.pages.dir(game), file = NULL, lang="en") {
	if (is.null(file)) {
		file = paste0("start-page.auto.Rmd")
	}
	btn.txt = paste0('\n<br>\n{{submitStartPageBtn("Press to start")}}')
	txt = paste0("<h3>Start ", game$gameId, "</h3>\n", btn.txt)
	writeLines(txt, file.path(pages.dir, file))
	merge.lines(txt)
}


make.end.page.rmd = function(game, pages.dir = get.pages.dir(game), file = NULL, lang="en") {
	if (is.null(file)) {
		file = paste0("end-page.auto.Rmd")
	}
	head.txt = paste0("<h3>You finished a round of ", game$gameId, "</h3>")

	btn.txt = paste0('\n<br>\n{{submitEndPageBtn("Press to restart")}}')

	obs.vars = unlist(lapply(game$vg$stages, function(stage) {
    c(names(stage$nature), names(stage$comp), names(stage$actions))
	}))
	if (length(obs.vars)>0) {
		obs.txt = paste0(obs.vars, ": {{", obs.vars,"}}", collapse = "<br>\n" )
		obs.txt = paste0("\n\n<h3>Realized outcome</h3>\n<p>\n", obs.txt,"\n</p>")
	}

	txt = c(head.txt, obs.txt, btn.txt)

	writeLines(txt, file.path(pages.dir, file))
	merge.lines(txt)
}


make.wait.page.rmd = function(game, pages.dir = get.pages.dir(game), file = NULL, lang="en") {
	if (is.null(file)) {
		file = paste0("wait-page.auto.Rmd")
	}
	txt = paste0("<h3>Please wait...</h3>")
	writeLines(txt, file.path(pages.dir, file))
	merge.lines(txt)
}

make.stage.page.rmd = function(game, stage, pages.dir = get.pages.dir(game), file = NULL, lang="en") {
	restore.point("make.stage.page")

	if (is.numeric(stage) | is.character(stage))
	  stage = game$vg$stages[[stage]]

	# Skip since no player exists
	if (is.empty(stage$player))
	  return()



	if (is.null(file)) {
		file = paste0(stage$name,".auto.Rmd")
	}

	head.txt = paste0(
'<h3>', stage$name,'</h3>
<h4>Player: {{.player}}</h4>'
	)

	if (is.call(stage$observe)) {
		obs.txt = paste0("Cannot automatically generate observations for R formula <br>\n",deparse1(stage$observe))
	} else {
		obs.vars = setdiff(stage$observe, c("",stage$domain.vars))
		if (length(obs.vars)>0) {
			obs.txt = paste0(stage$observe, ": {{", stage$observe,"}}", collapse = "<br>\n" )
			obs.txt = paste0("\n\n<h3>Observations</h3>\n<p>\n", obs.txt,"\n</p>")
		} else {
			obs.txt = ""
		}
	}

	action.txt = ""
	if (length(stage$actions)>0) {
		action.txt = lapply(stage$actions, make.page.action.txt, game=game, stage=stage)
		action.txt = paste0("\n",paste0(action.txt, collapse="\n<br>\n"))
	}

	btn.txt = paste0('\n{{submitPageBtn("Press to proceed")}}')
	txt = c(head.txt, obs.txt, action.txt, btn.txt)

	if (!dir.exists(pages.dir))
		try(dir.create(pages.dir, recursive = TRUE))

	writeLines(txt, file.path(pages.dir,file))

	invisible(sep.lines(txt))

}

make.page.action.txt = function(game, action, stage) {
	restore.point("make.page.action.txt")
	label = paste0(action$name,":")
	choiceLabels = action$labels
	if (identical(choiceLabels,"")) choiceLabels=NULL

	if (is.null(choiceLabels)) {
		clc = "NULL"
	} else {
		clc = paste0("c(", paste0('"', choiceLabels,'"', collapse=", "),")")
	}

	if (!is.null(action$domain.var)) {
		restore.point("make.page.stratmeth.txt")
		domain.var = action$domain.var
		domain.var.td = paste0('<td>', domain.var,'</td>', collapse=" ")
		domain.val.td = paste0('<td>{{domain.val[["', domain.var,'"]]}}</td>', collapse=" ")


		table.class = paste0("table-",stage$name,"-",action$name)
		res = paste0('
Choose your action "',action$name,'" conditional on the value of "',paste0(domain.var, collapse=", "),'"
<!--
You can adapt the style of the strategy method table cells here. -->
<style>
	table.',table.class,' > tbody > tr > td {
		border-bottom: solid;
		border-bottom-width: 1px;
		padding-left: 5px;
	}
	table.',table.class,' table.rowRadioTable td {
		padding-left: 5px;
		padding-right: 3px;
		padding-top: 3px;
		padding-bottom: 3px;
	}
</style>
<table class="',table.class,'">
<tr>', domain.var.td,'<td>Your choice</td></tr>

#< stratMethRows action= "',action$name,'"
<tr>
',domain.val.td,'
<!-- possible input types: "rowRadio", "select", "radio" -->
<td>{{stratMethInput(inputType="rowRadio", choiceLabels= ', clc,')}}</td>
</tr>
#> end stratMethRows

</table>
')
		return(res)
	}
	paste0(
'{{actionField(name="',action$name,'", label="',label,'", choiceLabels = ', clc,")}}")
}

save.stage.page.rmd = function(game, txt, stage.name, pages.dir = get.pages.dir(game), file = NULL, auto=FALSE) {
	restore.point("save.stage.page.rmd")
	if (is.null(file)) {
		file = paste0(stage.name, ifelse(auto,".auto",""), ".Rmd")
	}
	if (!dir.exists(pages.dir))
		try(dir.create(pages.dir, recursive = TRUE))

	writeLines(txt, file.path(pages.dir,file))
}

