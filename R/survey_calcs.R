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
#' @return a list. Entry 1 is the result of the calculation (e.g. the mean value). Other item entries are (optionally) the se, lower, and upper.
#' @details When x is a factor, results are returned in order of \code{levels(x)}.
#'          When \code{smean} is called without assignment (e.g. \code{:=}), the result value will translate from a list to a data.table, even if only the mean is returned.
#'
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
smean.default = function(x, na.rm = T, var_type = 'none', ci_method = 'mean',level = .95, use_df = T, ids, sv, ...){

  #global bindings
  psu <- strata <- NULL

  ppp = prep_survey_data(var_type, ci_method, ids, use_df, na.rm, x, sv)
  x <- ppp$x
  var_type = ppp$var_type
  df = ppp$df
  ci_method = ppp$ci_method
  st = ppp$st
  lids = ppp$lids


  if(st %in% 'svydt') ret = calc_mean_dtsurvey(x = x, ids = ids, sv = sv, var = !all(var_type %in% 'none'))
  if(st %in% 'svyrepdt'){
    repdat = get_svyrep_attributes()
    ret = calc_mean_dtrepsurvey(x = x, sv = sv, ids = ids,
                                scaledata = repdat$scaledata,
                                cw = repdat$combined.weights,
                                mse = repdat$mse,
                                selfrep = repdat$selfrep,
                                var = !all(var_type %in% 'none'))
  }
  if(!exists('ret')) stop('No results generated-- check that attribute `type` is available in the parent data.table')

  if(!all(var_type %in% 'none')){
    ret <- calculate_error(ret, ci_method, level, df, var_type, lids, st)
  }else{
    ret = ret[[1]]
  }
  names(ret) = NULL
  #r = t(ret)
  #if(is.factor(x))
  return(ret)
}

#' @rdname smean
#' @export
smean.character <- function(x, ...){
  stop(paste("Don't know how to deal with objects of class: 'character'. Consider converting into a factor before running smean"))

}

#' An internal function to calculate the mean (and optionally, SE and CI) from a dtrepsurvey object
#' @param x matrix. Results from the mean call
#' @param ids logical. vector determining which rows are being calculated on.
#' @param sv data.table survey variables data.table
#' @param scaledata list the different scale bits from svrepdesigns
#' @param cw logical. denotes combined weights
#' @param mse logical. passed to survey::svrVar
#' @param selfrep logical. Used for some logic taken from survey:::svymean.svrepdesign
#' @param var logical. Should the vcov matrix be returned?
#' @importFrom survey svrVar
#' @noRd
calc_mean_dtrepsurvey <- function(x, sv, ids, scaledata, cw, mse, selfrep= NULL, var = T){
  # ttps://github.com/cran/survey/blob/a0f53f8931f4e304af3c758b2ff9a56b0a2a49bd/R/surveyrep.R
  #Not really sure if this will ever actually be useful, but carry over from survey
  if(!cw)
    pw<-sv[ids ,pweights]
  else
    pw<-1
  ret = list()


  ret$result <- colSums(sv[ids, pweights] * x)/sum(sv[ids, pweights])

  #borrowed from https://github.com/cran/survey/blob/a0f53f8931f4e304af3c758b2ff9a56b0a2a49bd/R/surveyrep.R#L1079
  if(var){

    if (getOption("survey.drop.replicates") && !is.null(selfrep) && all(selfrep)){
      ret$v<-matrix(0,length(ret$result),length(ret$result))
    }else{
      repmeans = sv[ids, lapply(.SD, function(y) colSums(x * pw * y)/sum(pw *y)), .SDcols = grep('rep', names(sv))]
      repmeans = drop(as.matrix(repmeans))
      if(NCOL(repmeans)>1) repmeans <- t(repmeans)
      #calculate the variance
      ret$v <- survey::svrVar(repmeans, scaledata$scale, scaledata$rscales,mse=mse, coef=ret$result)

    }
  }

  return(ret)

}

#' An internal function to calculate the mean (and optionally, SE and CI) from a normal dtsurvey object
#' @param x matrix. For calculating column means
#' @param ids logical. vector determining which rows are being calculated on.
#' @param sv data.table. DT of the survey variables
#' @param var logical. Should the variance be calculated and returned?
#' @importFrom survey svyrecvar
#' @noRd
calc_mean_dtsurvey <- function(x, ids, sv, var = T){

  psum<-sum(sv$weight[ids])
  average<-colSums(x*sv$weight[ids]/psum)
  x<-sweep(x,2,average)
  # v<-svyrecvar(x*pweights/psum,design$cluster,design$strata, design$fpc,
  #              postStrata=design$postStrata)

  ret = list(result = average)
 if(var){
   v<-survey::svyrecvar(x *sv$weight[ids]/psum,
     data.frame(psu = sv$psu[ids]),
     data.frame(strata = sv$strata[ids]),
     list(popsize = NULL, sampsize = as.matrix(sv$sampsize[ids], ncol = 1)),
     postStrata=NULL)

   ret$v = v
 }


  return(ret)
}

#' Prepare data for survey calculations
#' @param var_type character.
#' @param ci_method character.
#' @param ids vector.
#' @param use_df logical.
#' @param na.rm logical.
#' @param x vector
#' @param sv data.table
#'
#' @details This function is merely a helper for \code{smean} and \code{stotal}.
#' @noRd
prep_survey_data <- function(var_type, ci_method, ids, use_df, na.rm, x, sv){
  psu <- strata <- NULL #to make the no visible binding problem go away
  var_type = match.arg(var_type, c('none', 'se', 'ci'), TRUE)
  ci_method = match.arg(ci_method, c('mean', 'beta', 'xlogit'))

  if(missing(ids)) stop('Please explicitly pass an id vector')

  ids = which(sv[,`_id` %in% ids])

  #get the df for later
  #taken from survey:::degf.survey.design2
  lids = length(ids)
  if(st %in% 'svydt'){
    df = length(unique(sv[ids, psu])) -length(unique(sv[ids, strata]))
  }else if(st %in% 'svyrepdt') {
    df = qr(as.matrix(sv[ids, .SD, .SDcols=  grep('rep', names(sv))]), tol = 1e-05)$rank - 1
  }

  if(!use_df) df = Inf

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

  return(list(var_type = var_type, x = x, ids = ids, ci_method =  ci_method, sv = sv, df = df, st = st, lids = lids))

}


#' Calculate error statistics for survey means
#' @param ret list. contains the results from calc_mean/total_...
#' @param ci_method character. Type of ci (if any, to calculate)
#' @param level numeric. 0 - 1, confidence level to return for ci
#' @param df numeric. degrees of freedom
#' @param var_type character. type of variability/variance to return
#' @param lids numeric. Length of ids (e.g. number of rows of data to consider)
#' @param st character. survey type
#' @noRd
calculate_error <- function(ret, ci_method, level, df, var_type, lids, st){
  v = ret$v
  ret$v = NULL

  if (is.matrix(v)){
    se <- sqrt(diag(v))
  }else{
    se <- sqrt(v)
  }

  if(any('se' %in% var_type)){
    attributes(se) <- NULL
    ret$se = se
  }

  if(any('ci' %in% var_type)){
    #nicked from survey:::tconfint
    if(ci_method == 'mean') ci = ret$result + se %o% qt(c((1-level)/2, 1-(1-level)/2), df = df)
    #nicked from survey::svyciprop
    if(ci_method %in% c('beta', 'xlogit')){
      m <- ret$result
      attr(m, 'var') <- v

      if(st %in% 'svyrepdt'){
        class(m) <- "svrepstat"
      } else{
        class(m) <- 'svystat'
      }
      names(m) = 1



      if(ci_method %in% 'xlogit'){
        xform <- survey::svycontrast(m, quote(log(`1`/(1 - `1`))))
        ci <- expit(as.vector(confint(xform, 1, level = level,
                                      df = df)))
      }

      if(ci_method %in% 'beta'){
        n.eff <- coef(m) * (1 - coef(m))/vcov(m)
        rval <- coef(m)[1]
        attr(rval, "var") <- vcov(m)
        alpha <- 1 - level
        # n.eff <- n.eff * (qt(alpha/2, nrow(design) - 1)/qt(alpha/2,
        #                                                    degf(design)))^2
        n.eff <- n.eff * (qt(alpha/2, lids - 1)/qt(alpha/2,
                                                   df))^2
        ci <- c(qbeta(alpha/2, n.eff * rval, n.eff * (1 - rval) +
                        1), qbeta(1 - alpha/2, n.eff * rval + 1, n.eff *
                                    (1 - rval)))
      }
    }

    if(is.matrix(ci)){
      ret$lower = ci[,1]
      ret$upper = ci[,2]
    }else{
      ret$lower = ci[1]
      ret$upper = ci[2]
    }
  }
  return(ret)
}

#' Calculate the survey total
#'
#' @param x vector. Vector of values to compute a weighted total over
#' @param na.rm logical. Determines whether NAs are removed from calculations
#' @param ids numeric. list of indices which are being computed.
#' @param var_type character. Report variability as one or more of: standard error ("se", default) and confidence interval ("ci")
#' @param level numeric. A value in the range of (0, 1) denoting what level of confidence the CI should be
#' @param use_df logical. Should the estimated degrees of freedom by used to calculate CIs? Default is FALSE (different from \code{smean}. FALSE implies df = Inf.
#' @param ... other arguments. Currently unused.
#' @return a list. Entry 1 is the result of the calculation (e.g. the mean value). Other item entries are (optionally) the se, lower, and upper.
#' @details When x is a factor, results are returned in order of \code{levels(x)}.
#'          When \code{stotal} is called without assignment (e.g. \code{:=}), the result value will translate from a list to a data.table, even if only the mean is returned.
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
stotal.default = function(x, ids, na.rm = T, var_type = 'none', level = .95, use_df = FALSE, ...){

  #global bindings
  psu <- strata <- NULL

  #alternate methods CI making don't apply to totals
  ci_method = 'mean'

  ppp = prep_survey_data(var_type, ci_method, ids, use_df, na.rm, x)
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
  }else{
    ret = ret[[1]]
  }
  names(ret) = NULL
  #r = t(ret)
  #if(is.factor(x))
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
