#' Calculate the variance/covariance matrix for a survey statistic
#' @param x vector of data
#' @param na.rm logical. If TRUE, omit NAs
#' @param type character. One of 'mean' or 'total' depending on the scale of statistic
#' @param as_list logical. Return values as list
#' @param svyrep_attributes list. List of attributes relevant to replicate design surveys
#' @param sv data.table. Survey vars data.table
#' @param ids numeric. vector of indices to operate on
#' @param st character. survey type
#' @importFrom stats var
#' @export
sur_var <- function(x, na.rm = T, type = 'mean', as_list = TRUE, svyrep_attributes, sv, ids, st){

  #Make sure the various inputs are accounted for
  check_survey_bits(ids, sv, st)
  type <- match.arg(type, c('mean', 'total'))

  if(st == 'svyrepdt'){
    if(missing(svyrep_attributes) && st == 'svyrepdt'){
      svyrep_attributes = get_svyrep_attributes()
    }
  }else{
    svyrep_attributes = NULL
  }

  #prep x and ids (mostly removing NAs)
  ids = prep_ids(x, ids, na.rm = na.rm)
  x = prep_x(x, na.rm = na.rm)

  if(st %in% 'svydt'){
    r = (surdes_var(x, type, ids, sv))
  }

  if(st %in% 'svyrepdt'){
    r = (repdes_var(x, type, ids, sv, svyrep_attributes))
  }

  if(st %in% 'admin'){
    r = var(x)/length(ids) #I think the survey variance calcs do this scaling as part of their specific commands
    #for example: https://github.com/cran/survey/blob/9addea4be893748c60974732b01ef7a393019217/R/multistage.R#L736
  }

  if(as_list) r = list(list(var = r))

  return(r)
}

#' Calculate the variance/covariance matrix for a survey statistic (normal survey)
#' @param x matrix of data
#' @param type character. One of 'mean' or 'total' depending on the scale of statistic
#' @param sv data.table. Survey vars data.table
#' @param ids numeric. vector of indices to operate on
surdes_var <- function(x, type = 'mean', ids, sv){

  type <- match.arg(type, c('mean', 'total'))
  if(type == 'mean'){

    pweights<-sv$weight[ids]
    psum<-sum(pweights)
    average<-colSums(x*pweights/psum)
    x<-sweep(x,2,average)
    v<-survey::svyrecvar(x * sv$weight[ids]/psum,
                         data.frame(psu = sv$psu[ids]),
                         data.frame(strata = sv$strata[ids]),
                         list(popsize = NULL, sampsize = as.matrix(sv$sampsize[ids], ncol = 1)),
                         postStrata=NULL)


  }else if(type == 'total'){
    v<-survey::svyrecvar(x * sv$weight[ids],
                         data.frame(psu = sv$psu[ids]),
                         data.frame(strata = sv$strata[ids]),
                         list(popsize = NULL, sampsize = as.matrix(sv$sampsize[ids], ncol = 1)),
                         postStrata=NULL)
  }

  return(v)
}

#' Calculate the variance/covariance matrix for a survey statistic (replicate design)
#' @param x vector of data
#' @param type character. One of 'mean' or 'total' depending on the scale of statistic
#' @param svyrep_attributes list. List of attributes relevant to replicate design surveys
#' @param sv data.table. Survey vars data.table
#' @param ids numeric. vector of indices to operate on
repdes_var <- function(x, type = 'mean', ids, sv, svyrep_attributes){

  type <- match.arg(type, c('mean', 'total'))
  cw = svyrep_attributes$combined.weights
  selfrep = svyrep_attributes$selfrep

  if(!cw)
    pw<-sv[ids ,pweights]
  else
    pw<-1

  if(type == 'mean'){
    result = repdes_mean(x, ids, sv)
  }else{
    result = repdes_total(x, ids, sv)
  }

  if (getOption("survey.drop.replicates") && !is.null(selfrep) && all(selfrep)){
    v<-matrix(0,length(result),length(result))
  }else{
    if(type == 'mean'){
      replicate_results = sv[ids, lapply(.SD, function(y) colSums(x * pw * y)/sum(pw *y)), .SDcols = grep('rep', names(sv))]
    }else{
      replicate_results = sv[ids, lapply(.SD, function(y) colSums(x * pw * y)), .SDcols = grep('rep', names(sv))]

    }
    replicate_results = drop(as.matrix(replicate_results))
    if(NCOL(replicate_results)>1) replicate_results <- t(replicate_results)

    #calculate the variance
    #If all the data is NA, no need to warn about that
    if(all(is.na(replicate_results)) || all(is.nan(replicate_results))){

      v = matrix(NA, nrow = NROW(replicate_results), ncol = NCOL(replicate_results))

    } else if(NCOL(replicate_results) == 0){
      v <- suppressWarnings(survey::svrVar(replicate_results,
                          svyrep_attributes$scaledata$scale,
                          svyrep_attributes$scaledata$rscales,
                          mse = svyrep_attributes$mse,
                          coef=result))
    }else{
      v <- survey::svrVar(replicate_results,
                          svyrep_attributes$scaledata$scale,
                          svyrep_attributes$scaledata$rscales,
                          mse = svyrep_attributes$mse,
                          coef=result)
    }


  }

  if(!is.matrix(v)) v<- as.matrix(v)
  attr(v, 'means') <- NULL

  v

}

#' Calculate the standard error
#' @param v either a vector of data or a vcov matrix
#' @param input_type Character. One of 'var' or 'x'. Flags what v represents (x for vector of data) or var for an already computed vcov matrix (usually from sur_var)
#' @param svyrep_attributes list. Containing arguments passed to \code{sur_var} when computing the variance from a replicate weight design
#' @param sv data.table. Survey vars data.table
#' @param ids numeric. vector of indices to operate on
#' @param st character. survey type
#' @export
sur_se = function(v, input_type = 'var', svyrep_attributes, sv, ids, st){

    #Make sure the various inputs are accounted for
  check_survey_bits(ids, sv, st)
  input_type <- match.arg(input_type, c('var', 'x'))

  if(st == 'svyrepdt'){
    if(missing(svyrep_attributes)){
      svyrep_attributes = get_svyrep_attributes()
    }
  }else{
    svyrep_attributes = NULL
  }
  #if the variance needs to be computed, do that now
  if(input_type == 'x'){
     v = sur_var(v, sv = sv, ids = ids, st = st, svyrep_attributes) #does this work as I expect?
  }

  if (is.matrix(v)){
    se <- sqrt(diag(v))
  }else{
    se <- sqrt(v)
  }

  se

}

#' Calculate confidence interval
#' @param a Either the result of sur_mean or sur_total OR a vector of data to compute a function over
#' @param b Either a vcov matrix from sur_var OR one of 'sur_total' or 'sur_mean'
#' @param ab_type character. One of "agg" or "raw". If agg, `a` is the result and `b` is the corresponding vcov matrix. If 'raw', `a` is the vector to compute `b` on (e.g. `b`(`a`))
#' @param ci_part character. One of 'lower', 'upper', 'both'. Determines what part of the ci should be returned
#' @param ci_method character. Name of the method by which the ci should be calculated. See details for more information.
#' @param level numeric. 0 - 1, confidence level to return for ci
#' @param use_df logical
#' @param denom numeric. Only useful/used when ci_method == 'unweighted_binary'. Denominator (or N) for prop.test
#' @param na.rm logical. Passed to `b` when `ab_type` = 'raw'
#' @param sv data.table. Survey vars data.table
#' @param ids numeric. vector of indices to operate on
#' @param st character. survey type
#' @export
sur_ci <- function(a, b = 'sur_mean', ab_type = 'raw', ci_part = 'both', ci_method = 'mean', level = .95, use_df = T, denom, na.rm = T, sv, ids, st){

  ab_type <- match.arg(ab_type, c('agg', 'raw'))
  check_survey_bits(ids, sv, st)
  lids = length(ids) #This should always be the number of observations

  if(ab_type == 'agg'){
    stopifnot('Expecting that b is a matrix' = inherits(b, 'matrix'))
    res <- a
    vcov <- b
  }

  if(ab_type == 'raw' & ci_method != 'unweighted_binary'){
    if(b == 'sur_total') ttt = 'total'
    else ttt = 'mean'

    b <- match.fun(match.arg(b, c('sur_total', 'sur_mean')))
    vcov <- sur_var(a, type = ttt, na.rm = na.rm, sv = sv, ids = ids, st = st, as_list = F)
    res <- b(a, na.rm = na.rm, sv = sv, ids = ids, st = st)
  }

  if(all(is.na(res))){
    ci = matrix(c(NA_real_, NA_real_), nrow = 1)
  }else{


    #Degrees of freedom
    if(!use_df){
      df = Inf
    }else{
      if(st %in% 'svydt'){
        idx = ids[sv[ids, weight != 0]]
        df = length(unique(sv[idx, psu])) -length(unique(sv[idx, strata]))
      }else if(st %in% 'svyrepdt') {
        df = qr(as.matrix(sv[ids, .SD, .SDcols=  grep('rep', names(sv))]), tol = 1e-05)$rank - 1
      }else{
        df = length(ids) - 1
      }
    }

    #calculate the se
    se = sur_se(vcov, sv = sv, ids = ids, st = st)


    if(ci_method %in% c('total', 'mean')){
      ci = ci_standard(res, se, level, df, survey = st %in% c('svydt', 'svyrepdt'))
    }else if(ci_method == 'beta'){
      ci = ci_beta(res, vcov, level, df, st, lids)
    }else if(ci_method == 'xlogit'){
      ci = ci_xlogit(res, vcov, level, df, st)
    }else if(ci_method == 'unweighted_binary'){
      ci = ci_score(success = res * denom, denom, level = level)
    }
  }

  if(!is.matrix(ci)){
    ci = matrix(ci, nrow = 1)
  }

  stopifnot(ncol(ci) == 2)

  if(ci_part == 'lower'){
    return(ci[,1])
  }else if(ci_part == 'upper'){
    return(ci[,2])
  }else{
    return(ci)
  }


}

#' What you'd get from survey mean
#' @param x numeric. THe mean/total result
#' @param se numeric. The standard error
#' @param level numeric. 0 - 1 reflecting the confidence level
#' @param df numeric. degrees of freedom
#' @param survey logical. Specifies whether the data comes from a survey.
#' @export
ci_standard = function(x, se, level, df, survey = T){

  if(survey || df < 30){
    ci = x + se %o% stats::qt(c((1-level)/2, 1-(1-level)/2), df = df)
  } else{
    ci = x + se %o% stats::qnorm(c((1-level)/2, 1-(1-level)/2))
  }

  ci
}


#' Refer to beta option of svyciprop
#' @param x numeric. THe mean/total result
#' @param vcov matrix The variance/covariance matrix
#' @param level numeric. 0 - 1 reflecting the confidence level
#' @param df numeric. degrees of freedom
#' @param st character. Survey type, one of svyrepdt or svydt
#' @param lids numeric. Number of rows the subset has
#' @export
ci_beta = function(x, vcov, level, df, st, lids){
  m <- x
  attr(m, 'var') <- vcov
  st = match.arg(st, c('svydt', 'svyrepdt'))

  if(st %in% 'svyrepdt'){
    class(m) <- "svrepstat"
  } else{
    class(m) <- 'svystat'
  }
  names(m) = 1

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

#' Refer to xlogit option of svyciprop
#' @param x numeric. THe mean/total result
#' @param vcov matrix The variance/covariance matrix
#' @param level numeric. 0 - 1 reflecting the confidence level
#' @param df numeric. degrees of freedom
#' @param st character. Survey type, one of svyrepdt or svydt
#' @export
ci_xlogit = function(x, vcov, level, df, st){
  m <- x
  attr(m, 'var') <- vcov
  st = match.arg(st, c('svydt', 'svyrepdt'))

  if(st %in% 'svyrepdt'){
    class(m) <- "svrepstat"
  } else{
    class(m) <- 'svystat'
  }
  names(m) = 1

  xform <- survey::svycontrast(m, quote(log(`1`/(1 - `1`))))
  ci <- expit(as.vector(confint(xform, 1, level = level,
                                df = df)))

}

#' CI of a proportion via score method
#' @param success numeric. Vector of counts
#' @param N numeric. Single number of vector of N such that success/N = proportions
#' @param level numeric [0-1]. Confidence level.
#' @importFrom stats prop.test
ci_score = function(success, N = sum(success), level = .95){
  stopifnot(length(N) == length(success) || length(N) == 1)

  if(length(N) == 1) N = rep(N, length(success))



  r = vapply(seq_along(success), function(x){

    if(is.na(success[x]) || N[x] == 0 || is.na(N[x])) return(matrix(c(NA_real_, NA_real_)))

      prop.test(success[x], N[x], conf.level = level, correct = FALSE)$conf.int
    }, c(1,2))

  return(t(r))

}
