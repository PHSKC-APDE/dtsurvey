#' Calculate the survey mean
#'
#' @param x vector. Vector of values to compute a weighted mean over
#' @param na.rm logical. Determines whether NAs are excluded from the analysis
#' @param var_type character. Report variability as one or more of: standard error ("se", default) and confidence interval ("ci")
#' @param level numeric. A value in the range of (0, 1) denoting what level of confidence the CI should be
#' @param ci_method character. Determines how the ci (if requested via \code{var_type}) should be calculated
#'                  Options beside "mean" are only relevant for proportion (e.g. logical or values).
#'                  When the method is 'mean', the results should match \code{survey::svymean} while other options match \code{survey::svyciprop}
#' @param use_df logical. Should the estimated degrees of freedom by used to calculate CIs? Default is TRUE. FALSE implies df = Inf.
#'               \code{confint(survey::svymean())} uses Inf as a default while \code{confint(survey::svyciprop)} uses \code{degf(design)}
#' @param ids numeric. indices which are being computed. Can be generally omitted and will be added to the call via `[.dtsurvey`
#' @param sv data.table. Data.table of psu, strata, weight and the like to properly do survey statistics.
#' @param st character. type of survey dataset being analyzed. Can be generally omitted and will be added to the call via `[.dtsurvey`
#' @param ... other arguments. Currently unused.
#' @return a vector or a list (the latter likely converted into a data.table) containing the results
#' @details If var_type is "none", a vector is returned. For factors, the names of the vector correspond to the levels. When var_type is not "none" a named list is returned (and then likely converted into a data.table).
#'          The list (which should be named) is ordered in the following manner (by slot): result, se, lower, upper, levels.
#' @export
#'
#' @importFrom stats model.matrix qt confint coef vcov qbeta
#'
#' @importFrom survey svycontrast
#'
#'
smean <- function(x, ...){
  UseMethod('smean')
}

#' @rdname smean
#' @export
smean.default = function(x, na.rm = T, var_type = 'none', ci_method = 'mean',level = .95, use_df = T, ids, sv, st, ...){

  #global bindings
  psu <- strata <- NULL
  var_type = match.arg(var_type, c('none', 'se', 'ci'), TRUE)
  ci_method = match.arg(ci_method, c('mean', 'beta', 'xlogit', 'unweighted_binary'))

  if(is.factor(x)) level_x = levels(x)
  wasfactor = is.factor(x)
  #if(is.factor(x) && !ci_method %in% c('unweighted_binary', 'mean')) stop(paste0('Invalid `ci_method` for factors: ', ci_method,'. Please use the "mean", "beta", "xlogit" or "unweighted_binary"'))

  #get attributes for replicate surveys
  if(st == 'svyrepdt'){
    svyrep_attributes = get_svyrep_attributes()
  }else{
    svyrep_attributes = NULL
  }
  #list for storing the result
  ret = list()

  #compute the mean
  ret$result = sur_mean(x = x, na.rm = na.rm, as_list = FALSE, sv = sv, ids = ids, st = st)

  #compute the variance/uncertainty if required
  if(!all(var_type %in% 'none')){
    ret$v = sur_var(x, na.rm = na.rm, type = 'mean', as_list = FALSE,
                    svyrep_attributes = svyrep_attributes, sv = sv, ids = ids, st = st)

    if('se' %in% var_type){
      ret$se = sur_se(ret$v, input_type = 'var', svyrep_attributes = svyrep_attributes,
                      sv = sv, ids = ids, st = st)

      if(length(ret$se)==0) ret$se = NA_real_
    }

    if('ci' %in% var_type){

      if(wasfactor && ci_method %in% c('beta', 'xlogit')){
        xp = vapply(levels(x), function(y) as.numeric(x == y), rep(1, length(x)))

        if(!is.matrix(xp)) xp <- matrix(xp, ncol = length(levels(x)))

        retci = vapply(seq_len(ncol(xp)), function(xp_c){
          sur_ci(a = xp[, xp_c] , b = 'sur_mean', ab_type = 'raw',
                 ci_part = 'both', ci_method = ci_method, level = level, use_df = use_df,
                 denom = length(prep_ids(x, ids, na.rm = na.rm)),
                 na.rm = na.rm,
                 sv = sv,
                 ids = ids, #this might be overkill
                 st = st)


        }, matrix(c(1.1, 1.2)))

        retci = matrix(aperm(retci, c(3,2,1)), ncol = 2)

      }else{
        retci = sur_ci(a = ret$result, b = ret$v, ab_type = 'agg', ci_part = 'both',
                       ci_method = ci_method, level = level, use_df = use_df, denom = length(prep_ids(x, ids, na.rm = na.rm)),
                       na.rm = na.rm,
                       sv = sv, ids = ids, st = st)
      }

      ret$lower = retci[,1]
      ret$upper = retci[,2]
    }

    ret$v <- NULL #don't return the vcov matrix back

    if(wasfactor) ret$levels = names(ret$result)

  }else{
    ret = ret$result
  }

  return(ret)
}

#' @rdname smean
#' @export
smean.character <- function(x, ...){
  stop(paste("Don't know how to deal with objects of class: 'character'. Consider converting into a factor before running smean"))

}

#' Calculate the survey total
#'
#' @param x vector. Vector of values to compute a weighted total over
#' @param na.rm logical. Determines whether NAs are removed from calculations
#' @param var_type character. Report variability as one or more of: standard error ("se", default) and confidence interval ("ci")
#' @param level numeric. A value in the range of (0, 1) denoting what level of confidence the CI should be
#' @param use_df logical. Should the estimated degrees of freedom by used to calculate CIs? Default is FALSE (different from \code{smean}. FALSE implies df = Inf.
#' @param ids numeric. indices which are being computed. Can be generally omitted and will be added to the call via `[.dtsurvey`
#' @param sv data.table. Data.table of psu, strata, weight and the like to properly do survey statistics.
#' @param st character. type of survey dataset being analyzed. Can be generally omitted and will be added to the call via `[.dtsurvey`
#' @param ... other arguments. Currently unused.
#' @return a vector or a list (the latter likely converted into a data.table) containing the results
#' @details If var_type is "none", a vector is returned. For factors, the names of the vector correspond to the levels. It should also be ordered in that way
#'          In that case a list/data.table is returned with a column for the result and a column specifying the levels. When var_type is not "none" a named list is returned (and then likely converted into a data.table).
#'          The list (which should be named) is ordered in the following manner (by slot): result, se, lower, upper, levels -- when requested.
#'
#'
#' @export
#'
#' @importFrom stats model.matrix qt confint coef vcov qbeta
#'
#' @importFrom survey svycontrast
#'
#'
stotal <- function(x, ...){
  UseMethod('stotal')
}

#' @rdname stotal
#' @export
stotal.default = function(x, na.rm = T, var_type = 'none', level = .95, use_df = T, ids, sv, st, ...){


  #global bindings
  psu <- strata <- NULL
  var_type = match.arg(var_type, c('none', 'se', 'ci'), TRUE)

  if(is.factor(x)) level_x = levels(x)
  wasfactor = is.factor(x)

  #get attributes for replicate surveys
  if(st == 'svyrepdt'){
    svyrep_attributes = get_svyrep_attributes()
  }else{
    svyrep_attributes = NULL
  }
  #list for storing the result
  ret = list()

  #compute the mean
  ret$result = sur_total(x = x, na.rm = na.rm, as_list = FALSE, sv = sv, ids = ids, st = st)

  #compute the variance/uncertainty if required
  if(!all(var_type %in% 'none')){
    ret$v = sur_var(x, na.rm = na.rm, type = 'total', as_list = FALSE,
                    svyrep_attributes = svyrep_attributes, sv = sv, ids = ids, st = st)

    if('se' %in% var_type){
      ret$se = sur_se(ret$v, input_type = 'var', svyrep_attributes = svyrep_attributes,
                      sv = sv, ids = ids, st = st)
    }

    if('ci' %in% var_type){
      retci = sur_ci(a = ret$result, b = ret$v, ab_type = 'agg', ci_part = 'both',
                     ci_method = 'total', level = level, use_df = use_df, na.rm = na.rm,
                     sv = sv, ids = ids, st = st)
      ret$lower = retci[,1]
      ret$upper = retci[,2]
    }

    ret$v <- NULL #don't return the vcov matrix back

    if(wasfactor) ret$levels = names(ret$result)

  }else{
    ret = ret$result
  }

  return(ret)

}

#' @rdname smean
#' @export
stotal.character <- function(x, ...){
  stop(paste("Don't know how to deal with objects of class: 'character'. Consider converting into a factor before running smean"))

}
