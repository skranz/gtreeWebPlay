empty.df = function(cols) {
  li = lapply(cols, function(col) character(0))
  names(li) = cols
  as.data.frame(li, stringsAsFactors = FALSE)
}

multi.key.match = function(x.df, table.df, keys=intersect(colnames(x.df), colnames(table.df)), sep="°") {
  restore.point("multi.key.match")

  if (is.list(x.df)) {
    x.id = paste0(x.df[keys], collapse=sep)
  } else {
    x.id = paste.matrix.cols(x.df, keys,sep = sep)
  }
  table.id = paste.matrix.cols(table.df, keys, sep=sep)
  match(x.id, table.id)
}

eval.or.return = function(call,...) {
  if (!is(call,"name") & !is(call,"call") & !is(call,"expression")) return(call)
  eval(call,...)
}

insert.into.vec = function(vec, new, pos, replace=FALSE) {
  restore.point("insert.into.vec")

  keep.left = seq_len(min(pos)-1)

  if (replace) {
    keep.right = if (max(pos)<length(vec)) (max(pos)+1):length(vec) else integer(0)
  } else {
    keep.right= if (min(pos)<=length(vec)) (min(pos)):length(vec) else integer(0)
  }

  c(vec[keep.left],new,vec[keep.right])

}
