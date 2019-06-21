errorMessage = function(id, txt, ui=HTML(colored.html(txt, color)), color="#cc0000", millis = 5000) {
  restore.point("errorMessage")
  timedMessage(id,ui=ui,millis = millis)
  #shinyEventsUI::colored.html() colored.html
  #showNotification(HTML(txt), type="error")
}

colored.html = function(txt, color="#cc0000") {
  if (is.null(color)) return(txt)
  paste0("<font color='",color,"'>",txt,"</font>")
}
timedMessage = function(id,msg="",html=msg,ui=HTML(html), millis=5000, empty.msg = "", empty.ui=HTML(empty.msg), app=getApp()) {
  restore.point("timedMessage")
  try({
    setUI(id, ui)
    dsetUI(id, ui)
  })

  obs.id = paste0("..timedMessage..",id)
  flag.id = paste0("flag", obs.id)
  app[[flag.id]] = FALSE

  # destroy old observer
  if (!is.null(app[[obs.id]])) try(app[[obs.id]]$destroy())

  if (!is.finite(millis)) return()

  app[[obs.id]] = observe({
    if (!isTRUE(app[[flag.id]])) {
      app[[flag.id]] = TRUE
      invalidateLater(millis)
      return()
    }
    try(app[[obs.id]]$destroy())
    try({
      setUI(id, empty.ui)
      dsetUI(id, empty.ui)
    })
  })
}

rowRadioButtons = function(inputId,label=NULL, choices, selected = NA) {
	restore.point("rowRadioButtons")
	choices =  shiny:::choicesWithNames(choices)

	checked = rep("", length(choices))
	if (!is.na(selected)) {
		names(checked) = as.character(choices)
		checked[selected] = ' checked="checked"'
	}


	inner = paste0('
<td><label>
		<input type="radio" name="', inputId,'" value="',choices,'"',checked,'/>
		<span>',names(choices),'</span>
</label></td>', collapse="\n")

	html = paste0('<div id="',inputId,'" class="shiny-input-radiogroup shiny-input-container"><table class="rowRadioTable"><tr>',inner,'</tr></table></div>')

	HTML(html)

}
