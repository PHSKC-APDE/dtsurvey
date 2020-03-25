#' Calculate the survey mean
#'
#' @param x vector. Vector of values to compute a weighted mean over
#' @param na.rm logical. Passed to `weighted.mean`
#' @param ids numeric. list of indices which are being computed
#' @param var_type character. Report variability as one or more of: standard error ("se", default) and confidence interval ("ci")
#' @param ci numeric. A value in the range of (0, 1) denoting what level of confidence the CI should be
#' @param ci_method character. Determines how the ci (if requested via \code{var_type}) should be calculated
#'                  Options beside "mean" are only relevant for proportion (e.g. logical or values )
#' @export

smean <- function(x, ...){
  UseMethod('smean')
}

#' @rdname smean
#' @export
smean.default = function(x, na.rm = T, ids, var_type = 'se', ci = .95, ci_method = 'mean' ){
  var_type = match.arg(var_type, c(NULL, 'se', 'ci'))

  if(missing(ids)) stop('needs an id column to help why by type operations. The default column should be `_id`')
  stop_assignment()

  sv = get_survey_vars()

  if(na.rm){
    ids <- ids[!is.na(x)]
    x <- x[!is.na(x)]
  }

  #construct a model matrix if needed
  if(is.factor(x)){
    x = model.matrix(~0+x, data = data.table::data.table(x = x))
  }else{
    x = matrix(x, ncol = 1)
  }

  r = x * sv$weight[ids]/sum(sv$weight[ids])


  psum<-sum(sv$weight[ids])
  average<-colSums(x*sum(sv$weight[ids])/psum)
  x<-sweep(x,2,average)
  # v<-svyrecvar(x*pweights/psum,design$cluster,design$strata, design$fpc,
  #              postStrata=design$postStrata)


  ret = list(mean = colSums(r))

  if(!all(is.null(var_type))){
    v<-survey::svyrecvar((x - sum(r))*sv$weight[ids]/sum(sv$weight[ids]),
                         data.frame(psu = sv$psu[ids]),
                         data.frame(strata = sv$strata[ids]),
                         list(popsize = NULL, sampsize = as.matrix(sv$sampsize[ids], ncol = 1)),
                         postStrata=NULL)

    if (is.matrix(v))
      se <- sqrt(diag(v))
    else se <- sqrt(v)

    if(any('se' %in% var_type)){
      ret$se = se
    }

  }

  return(t(ret))
}

#' smean.default <- function(x, na.rm = T, ids){
#'   stop_assignment()
#'   xcl = class(x)
#'   stop(paste("Don't know how to deal with objects of class(es):", paste0(xcl, collapse = ', ')))
#' }

#' @rdname smean
#' @export
smean.character <- function(x, ...){
  #similar to smean.numeric, but to use a routine that mimics svyciprop for better confidence intervals
  stop(paste("Don't know how to deal with objects of class: 'character'. Consider converting into a factor before running smean"))

}
#' #' And internal routine to calculate
#' smean_num

