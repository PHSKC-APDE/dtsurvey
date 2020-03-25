#' Calculate the survey mean
#'
#' @param x vector. Vector of values to compute a weighted mean over
#' @param na.rm logical. Passed to `weighted.mean`
#' @param ids numeric. list of indices which are being computed
#' @export

smean <- function(x, ...){
  UseMethod('smean')
}

#' @rdname smean
#' @export
smean.numeric = function(x, na.rm = T, ids){

  if(missing(ids)) stop('needs an id column to help why by type operations. The default column should be `_id`')
  stop_assignment()

  sv = get_survey_vars()

  if(na.rm){
    ids <- ids[!is.na(x)]
    x <- x[!is.na(x)]
  }
  r = x * sv$weight[ids]/sum(sv$weight[ids])
  # pweights<-1/design$prob
  # psum<-sum(pweights)
  # average<-colSums(x*pweights/psum)
  #x<-sweep(x,2,sum(r))
  # v<-svyrecvar(x*pweights/psum,design$cluster,design$strata, design$fpc,
  #postStrata=design$postStrata)
  v<-survey::svyrecvar((x - sum(r))*sv$weight[ids]/sum(sv$weight[ids]),
                       data.frame(psu = sv$psu[ids]),
                       data.frame(strata = sv$strata[ids]),
                       list(popsize = NULL, sampsize = as.matrix(sv$sampsize[ids], ncol = 1)),
                       postStrata=NULL)

  if (is.matrix(v))
    se <- sqrt(diag(v))
  else se <- sqrt(v)

  ret <- t(list(mean = sum(r, na.rm = na.rm), mean_se = se))
}

#' @rdname smean
#' @export
smean.default <- function(x, na.rm = T, ids){
  stop_assignment()
  xcl = class(x)
  stop(paste("Don't know how to deal with objects of class(es):", paste0(xcl, collapse = ', ')))
}

#' @rdname smean
#' @export
smean.factor <- function(x, na.rm = T, ids){

}


