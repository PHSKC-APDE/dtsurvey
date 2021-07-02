#' Inverse logit transform
#' @param eta numeric to inverse logit transform
expit <- function(eta){
  exp(eta)/(1 + exp(eta))
}

#' Unlist by a single level, used for survey factors results
#' @export
delist_factor = function(x){
  stopifnot(inherits(x, 'list'))
  unlist(x, recursive = FALSE)
}


