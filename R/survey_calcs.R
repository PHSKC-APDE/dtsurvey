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
  ci_method = match.arg(ci_method, c('mean', 'beta', 'xlogit'))

  if(is.factor(x)) level_x = levels(x)
  wasfactor = is.factor(x)
  if(is.factor(x) && !ci_method %in% 'mean') stop(paste0('Invalid `ci_method` for factors: ', ci_method,'. Please use the "mean" option or "nonsurvey_binary"'))


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
    }

    if('ci' %in% var_type){
      retci = sur_ci(a = ret$result, b = ret$v, ab_type = 'agg', ci_part = 'both',
                      ci_method = ci_method, level = level, use_df = use_df,
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

  if(is.factor(x)) level_x = levels(x)
  wasfactor = is.factor(x)

  #global bindings
  psu <- strata <- NULL

  #alternate methods CI making don't apply to totals
  ci_method = 'mean'

  ppp = prep_survey_data(var_type, ci_method, ids, use_df, na.rm, x, sv, st)
  x <- ppp$x
  var_type = ppp$var_type
  ids = ppp$ids
  sv = ppp$sv
  df = ppp$df
  ci_method = ppp$ci_method
  st = ppp$st
  lids = ppp$lids

  if(st %in% 'svydt') ret = calc_total_dtsurvey(x = x, ids = ids, sv = sv, var = !all(var_type %in% 'none'))
  if(st %in% 'svyrepdt'){
    repdat = get_svyrep_attributes()
    ret = calc_total_dtrepsurvey(x = x, sv = sv, ids = ids,
                                scaledata = repdat$scaledata,
                                cw = repdat$combined.weights,
                                mse = repdat$mse,
                                selfrep = repdat$selfrep,
                                var = !all(var_type %in% 'none'))
  }
  if(!exists('ret')) stop('No results generated-- check that attribute `type` is available in the parent data.table')

  if(!all(var_type %in% 'none')){
    ret <- calculate_error(ret, ci_method, level, df, var_type, lids, st)
    if(wasfactor) ret$levels <- level_x
  }else{
    ret = ret[[1]]
    if(wasfactor) names(ret) <- level_x
  }


  return(ret)
}

#' @rdname smean
#' @export
stotal.character <- function(x, ...){
  stop(paste("Don't know how to deal with objects of class: 'character'. Consider converting into a factor before running smean"))

}

#' An internal function to calculate the total (and optionally, SE and CI) from a dtrepsurvey object
#' @param x matrix. Results from the total call
#' @param ids logical. vector determining which rows are being calculated on.
#' @param sv data.table survey variables data.table
#' @param scaledata list the different scale bits from svrepdesigns
#' @param cw logical. denotes combined weights
#' @param mse logical. passed to survey::svrVar
#' @param selfrep logical. Used for some logic taken from survey:::svymean.svrepdesign
#' @param var logical. Should the vcov matrix be returned?
#' @importFrom survey svrVar
#' @noRd
calc_total_dtrepsurvey <- function(x, sv, ids, scaledata, cw, mse, selfrep= NULL, var = T){
  # ttps://github.com/cran/survey/blob/a0f53f8931f4e304af3c758b2ff9a56b0a2a49bd/R/surveyrep.R
  #Not really sure if this will ever actually be useful, but carry over from survey
  if(!cw)
    pw<-sv[ids ,pweights]
  else
    pw<-1
  ret = list()


  ret$result <- colSums(sv[ids, pweights] * x)

  #borrowed from https://github.com/cran/survey/blob/a0f53f8931f4e304af3c758b2ff9a56b0a2a49bd/R/surveyrep.R#L1079
  if(var){

    if (getOption("survey.drop.replicates") && !is.null(selfrep) && all(selfrep)){
      ret$v<-matrix(0,length(ret$result),length(ret$result))
    }else{
      reptotals = sv[ids, lapply(.SD, function(y) colSums(x * pw * y)), .SDcols = grep('rep', names(sv))]
      reptotals = drop(as.matrix(reptotals))
      if(NCOL(reptotals)>1) reptotals <- t(reptotals)
      #calculate the variance
      ret$v <- survey::svrVar(reptotals, scaledata$scale, scaledata$rscales,mse=mse, coef=ret$result)

    }
  }

  return(ret)

}

#' An internal function to calculate the total (and optionally, SE and CI) from a normal dtsurvey object
#' @param x matrix. For calculating column means
#' @param ids logical. vector determining which rows are being calculated on.
#' @param sv data.table. DT of the survey variables
#' @param var logical. Should the variance be calculated and returned?
#' @importFrom survey svyrecvar
#' @noRd
calc_total_dtsurvey <- function(x, ids, sv, var = T){

  N<-sum(sv$weight[ids])
  total <- colSums(x/sv$weight[ids])
  # v<-svyrecvar(x*pweights/psum,design$cluster,design$strata, design$fpc,
  #              postStrata=design$postStrata)

  ret = list(result = total)
  if(var){
    v<-survey::svyrecvar(x /sv$weight[ids],
                         data.frame(psu = sv$psu[ids]), #cluster/psu
                         data.frame(strata = sv$strata[ids]), #strata
                         list(popsize = NULL, sampsize = as.matrix(sv$sampsize[ids], ncol = 1)), #fpc
                         postStrata=NULL)

    ret$v = v
  }


  return(ret)
}
