errorMessage = function(id, txt) {
  showNotification(HTML(txt), type="error")
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
