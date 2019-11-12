#' Deploys an example app to local directory
#'
#' @param example Name of the example. Current options are \n\t1. \code{"UltimatumGame"} a simple introductionary app and \n\t2.\code{"KuhnPoker"} a more complex app that implements a \code{\link{bot_pop}} to play against the population of earlier players.
#' @param dest.dir The destination directory. Creates the directory if at least its parent directory exists.
deploy_webplay_example = function(example=c("UltimatumGame","KuhnPoker")[1], dest.dir = file.path(getwd(), example)) {
  if (!dir.exists(dest.dir)) {
    dir.create(dest.dir,recursive = FALSE)
    if (!dir.exists(dest.dir))
      stop("Coud not create directory. See warnings.")
  }
  source.dir = system.file(file.path("examples", example), package="gtreeWebPlay")

  file.copy(source.dir, dest.dir, recursive=TRUE)
  cat(paste0("\nCopied example ", example," to directory: ", dest.dir,"\n\nThe main code is in global.R. You can open it and then run it in RStudio via the 'Run App' button."))
}
