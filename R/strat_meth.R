
get.sm.value = function(action.name, values, domain.var) {
	restore.point("get.sm.value")
	postfix = paste0(values[domain.var], collapse="_")
	var = paste0(action.name,"_",postfix)
	values[[var]]

}

# try to assign the actual action value
# from the values of a strategy method
# E.g. in an ultimatum game if offer=4
# and accept_4 = TRUE, we set accept= TRUE
# If offer is not yet computed
# (stages can be shown in parallel to players)
# store the action accept in
# wp$delayed.strat.meth and try to assign
# the value of accept later with
# wp.assign.delayed.strat.meth.realizations
wp.assign.strat.meth.realizations = function(wp,actions) {
	restore.point("wp.assign.strat.meth.realizations")
	# which actions use strategy method
	use.sm = sapply(actions, function(action) !is.null(action$domain.var))
	actions = actions[use.sm]

	for (action.name in names(actions)) {
		action = actions[[action.name]]

		has.domain = unlist(lapply(action$domain.var, function (dv) dv %in% names(wp$values)))

		if (!all(has.domain)) {

			wp$delayed.strat.meth[[action.name]] = action
		} else {
			wp$values[[action.name]] = get.sm.value(action.name = action.name,values = wp$values,domain.var = actions[[action.name]]$domain.var)
		}
	}
}

wp.assign.delayed.strat.meth.realizations = function(wp) {
	restore.point("wp.assign.delayed.strat.meth.realizations")
	# which actions use strategy method
	actions = wp$delayed.strat.meth

	for (action.name in names(actions)) {
		action = actions[[action.name]]

		has.domain = unlist(lapply(action$domain.var, function (dv) dv %in% names(wp$values)))

		if (all(has.domain)) {
			wp$values[[action.name]] = get.sm.value(action.name = action.name,values = wp$values,domain.var = actions[[action.name]]$domain.var)

			wp$delayed.strat.meth = wp$delayed.strat.meth[setdiff(names(wp$delayed.strat.meth), action.name)]
		}
	}
}


eval.stratMethRows.block = function(txt,envir=parent.frame(), out.type=first.none.null(cr$out.type,"html"),info=NULL, cr=NULL,...) {
	args = list(...)
	restore.point("eval.stratMethRows.block")

	html = merge.lines(info$inner.txt)
	# need to reverse placeholders to original whiskers
	html = reverse.whisker.placeholders(html, cr=cr)


	args = parse.block.args(info$header)
	action.name = args$action
	wp = envir$.wp

	stage = wp.stage(wp=wp)
	action = stage$actions[[action.name]]

	out = stratMethRows(action=action.name, domain.vals =action$domain.vals, html=html, wp=wp)
	out
}


stratMethRows = function(action.name,domain.vals, html,wp=get_wp(),player=wp$player,as.tr = FALSE, ...) {
	restore.point("stratMethTable")
	vg = wp$vg
	stage = wp.stage(wp)
	domain.var = names(domain.vals)

	domain.vals = as_data_frame(domain.vals)

	stratMethInput = function(inputType="select",choiceLabels=NULL,...) {
		actionField(name = paste0(action.name,"_",domain.val),label = "",inputType = inputType,choiceLabels = choiceLabels, wp=wp, action.name=action.name)
	}

	values = c(nlist(action=action.name, domain.var, stratMethInput), wp$page.values)

	domain.val = 0
	res.html = unlist(lapply(seq_len(NROW(domain.vals)), function(row) {
		# assign to global
		# to make domain.val
		# accessible in stratMethodInput
		domain.val <<- as.list(domain.vals[row,])
		values$domain.val = domain.val
		replace.whiskers(merge.lines(html), values, eval=TRUE)
	}))

	if (as.tr) {
		res.html = paste0("<tr>", res.html,"</tr>", collapse="\n")
	} else {
		res.html = paste0(res.html, collapse="\n")
	}
	res.html
}


