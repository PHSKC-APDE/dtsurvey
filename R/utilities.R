#' Inverse logit transform
#' @param eta numeric to inverse logit transform
expit <- function(eta){
  exp(eta)/(1 + exp(eta))
}
