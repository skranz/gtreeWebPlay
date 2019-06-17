# Adapts for ignore and include blocks in pages
#
# Simplified version of the handling in armd
adapt.rmd.ignore = function(txt) {
  restore.point("adapt.ignore")
  # all rows that will be deleted
  # in this precompilation state
  del.lines = NULL
  if (!any(startsWith(txt, "#< ignore")))
    return(txt)

  df = find.rmd.blocks(txt)

  # remove content in ignore blocks
  ig.rows = which(df$type=="ignore")
  if (length(ig.rows)>0) {
    del.lines = c(del.lines,unlist(lapply(ig.rows, function(ig.row) df$start[ig.row]:df$end[ig.row])))
  }

  if (length(del.lines)==0) return(txt)
  del.lines =unique(del.lines)
  txt = txt[-del.lines]
  txt
}

adapt.rmd.include = function(txt, dir=getwd(), nested=TRUE, adapt.ignore=FALSE) {
  restore.point("adapt.rmd.include")
  lines = which(str.starts.with(txt,"#. include "))
  if (length(lines)==0) return(txt)
  files = str.trim(str.right.of(txt[lines],"#. include "))
  i = 1
  for (i in seq_along(lines)) {
    file = files[i]
    line = lines[i]
    ntxt = readLines(file.path(dir,file),warn=FALSE,encoding = "UTF8")
    ntxt = mark_utf8(ntxt)
    if (adapt.ignore)
      ntxt = adapt.rmd.ignore(ntxt)

    if (nested)
      ntxt = adapt.rmd.include(ntxt, dir=dir)

    txt = insert.into.vec(txt,merge.lines(ntxt),pos=line, replace=TRUE)
  }
  return(txt)
}
