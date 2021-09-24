#' Inverse logit transform
#' @param eta numeric to inverse logit transform
expit <- function(eta){
  exp(eta)/(1 + exp(eta))
}

#' Unlist by a single level, used for survey factors results
#' @param x list (with a factor inside of it) to delist.
#' @export
delist_factor = function(x){
  stopifnot(inherits(x, 'list'))
  unlist(x, recursive = FALSE)
}
