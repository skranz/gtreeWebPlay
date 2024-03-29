
page.ns = function(page.name) {
	NS(paste0("page-",page.name))
}


make.wp.page.ui = function(wp=get_wp(), run.pre.page.handler=TRUE) {
  restore.point("make.wp.page.ui")
  page.name = wp.page.name(wp)
  compute.wp.page.values(wp,page.name,run.pre.page.handler=run.pre.page.handler)
  if (is.null(wp$page.ui.fun)) {
    default.page.ui.fun(wp, page.name=page.name)
  } else {
    wp$page.ui.fun(wp, page.name=page.name, values=wp$page.values)
  }
}

default.page.ui.fun = function(wp, page.name = wp.page.name(wp),...) {

  page.comp = wp$page.comp[[page.name]]

  if (is.null(page.comp)) {
    page.rmd = load.page.rmd(game = wp.game(wp),page.name = page.name, stage=wp.stage(wp), pages.dir = wp.pages.dir(wp)) %>% sep.lines()

    page.rmd = adapt.rmd.include(page.rmd,dir = wp.pages.dir(wp), nested=TRUE)
    page.rmd = merge.lines(page.rmd)

	  page.comp = wp$page.comp[[page.name]] = compile.rmd(text=page.rmd, out.type = "shiny",envir = wp$page.values)

  }

  ui = render.compiled.rmd(page.comp,envir=wp$page.values,use.commonmark=FALSE,fragment.only = TRUE,out.type = "shiny")
	ui

}

old.make.wp.page.ui = function(wp=get_wp(), run.pre.page.handler=TRUE) {
  restore.point("make.wp.page.ui")
  play = wp$play
  page.name = wp.page.name(wp)

  page.comp = wp$page.comp[[page.name]]

  page.rmd = load.page.rmd(game = wp.game(wp),page.name = page.name, stage=wp.stage(wp), pages.dir = wp.pages.dir(wp)) %>% sep.lines()

  page.rmd = adapt.rmd.include(page.rmd,dir = wp.pages.dir(wp), nested=TRUE)
  page.rmd = merge.lines(page.rmd)

  is.end = wp$stage.num >length(wp$vg$stages)
  values = c(play$hist, list(.wp=wp, .player = wp$human))
  if (is.end)
    values$payoffs =unlist(values[startsWith(names(play$hist), "payoff_")])


  if (!is.null(wp$pre.page.handler) & run.pre.page.handler) {
    extra.compute = wp$pre.page.handler(wp=wp, values=values, is.end = is.end , is.start = wp$stage.num == 0)
    values[names(extra.compute)] = extra.compute
  }
	wp$page.values = values


	# will only be temporary assigned
	ns = NS(paste0("page-",page.name))
	cr = compile.rmd(text=page.rmd, out.type = "shiny",envir = wp$page.values,blocks = "render")


  ui = render.compiled.rmd(cr,envir=wp$page.values,use.commonmark=FALSE)
	ui
}


submitStartPageBtn = function(label="Press to start",wp=get_wp(),...) {

	page.name = wp.page.name(wp)
	ns = NS(paste0("page-",page.name))
	id = paste0(ns("submitPageBtn"))

	buttonHandler(id, wp.start.btn.click)
	as.character(
		tagList(
			simpleButton(id,label,...)
		)
	)
}

submitEndPageBtn = function(label="Press to start",wp=get_wp(),...) {
	page.name = wp.page.name(wp)
	ns = NS(paste0("page-",page.name))
	id = paste0(ns("submitPageBtn"))

	buttonHandler(id, wp.end.btn.click)
	as.character(
		tagList(
			simpleButton(id,label,...)
		)
	)
}

wp.start.btn.click = function(wp=get_wp(),...) {

}

wp.end.btn.click = function(wp=get_wp(),...) {
  wp$finish.handler(wp,...)
}


submitPageBtn = function(label="Press to continue",wp=get_wp(),as.tag=FALSE,...) {
	restore.point("submitPageBtn")
	stage = wp.stage(wp=wp)

	page.name = wp.page.name(wp)
	ns = NS(paste0("page-",page.name))
	id = paste0(ns("submitPageBtn"))

	actions = stage$actions

	# which actions use strategy method
	use.sm = unlist(sapply(actions, function(action) !is.null(action$domain.var)))
	if (is.null(use.sm)) use.sm = logical(0)

	action.ids = unlist(sapply(names(actions[!use.sm]),get.action.input.id, USE.NAMES = FALSE))

	# get ids of all strategy method fields
	li = lapply(actions[use.sm], function(action) {
		postfix = paste.matrix.cols(action$domain.vals,sep="_")
		get.action.input.id(name=paste0(action$name,"_",postfix))
	})
	names(li) = NULL
	sm.ids = unlist(li)

	app = getApp()

	buttonHandler(id, wp.submit.btn.click, stage.name = stage$name, action.ids=action.ids,sm.ids=sm.ids, app = app)

	try(dsetUI(ns("msg"),"", app=app),silent = TRUE)

	ui =	tagList(
			uiOutput(ns("msg")),
			simpleButton(id,label, form.ids = c(wp$submit.ids, action.ids,sm.ids),...)
	)

	if (as.tag) return(ui)
  as.character(ui)
}


actionField = function(name,label=NULL,choiceLabels=NULL, inputType=c("auto","radio","rowRadio", "selectize", "slider")[1],wp=get_wp(),player=wp$human,action.name = name, as.tag=FALSE, width=NULL, ...) {
	vg = wp$vg
	stage = wp.stage(wp)
	action = stage$actions[[action.name]]
	if (identical(choiceLabels,""))
		choiceLabels = NULL
	restore.point("actionField")

	if (!is.null(label)) {
		label = replace.whiskers(label, wp$page.values,whisker.start = "<<", whisker.end = ">>")
	}

	id = get.action.input.id(name=name)
  choices = eval.or.return(action$set, wp$page.values)

	if (inputType=="auto") {
    if (length(choices)<=12){
      inputType="radio"
    } else {
      inputType="selectize"
    }
	}
  #inputType = "selectize"

  if (!is.null(choiceLabels)) {
    choices = as.list(choices)
    names(choices) = choiceLabels
  }
  if (inputType=="radio") {
    ui = radioButtons(inputId = id,label = label,choices = choices, selected=NA, width=width)
  } else if (inputType=="rowRadio") {
    ui = rowRadioButtons(inputId = id,label = "",choices = choices, selected=NA)
  } else if (inputType=="slider") {
    as.tag = TRUE
    vals = unlist(choices)
    if (!is.numeric(vals))
      stop("Slider needs numeric action space.")
    step = unique(diff(vals))
    if (length(step)>1)
      stop("Slider requires that numeric choices increase in constant steps.")
    ui = sliderInput(inputId=id,label = label,min=min(vals), max=max(vals),step = step, round=FALSE, width=width, ...)

  } else {
  	choices = c(list(""),as.list(choices))
    ui = selectizeInput(inputId = id,label = label,choices = choices, selected=NA, width=width)
  }
  if (as.tag) return(ui)

  html = as.character(ui)
	html
}


get.action.input.id = function(name) {
	id = paste0("action-",name)
	names(id) = name
	id
}


