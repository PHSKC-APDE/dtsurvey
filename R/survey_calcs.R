#' Calculate the survey mean
#'
#' @param x vector. Vector of values to compute a weighted mean over
#' @param na.rm logical. Passed to `weighted.mean`
#' @param ids numeric. list of indices which are being computed
#' @param var_type character. Report variability as one or more of: standard error ("se", default) and confidence interval ("ci")
#' @param level numeric. A value in the range of (0, 1) denoting what level of confidence the CI should be
#' @param ci_method character. Determines how the ci (if requested via \code{var_type}) should be calculated
#'                  Options beside "mean" are only relevant for proportion (e.g. logical or values).
#'                  When the method is 'mean', the results should match \code{survey::svymean} while other options match \code{survey::svyciprop}
#' @param use_df logical. Should the estimated degrees of freedom by used to calculate CIs? Default is TRUE. FALSE implies df = Inf.
#'               \code{confint(survey::svymean())} uses Inf as a default while \code{confint(survey::svyciprop)} uses \code{degf(design)}
#'
#' @return a list. Entry 1 is the result of the calculation (e.g. the mean value). Other item entries are (optionally) the se, lower, and upper.
#' @details When x is a factor, results are returned in order of \code{levels(x)}.
#'          When \code{smean} is called without assignment (e.g. \code{:=}), the result value will translate from a list to a data.table, even if only the mean is returned.
#'
#' @export
#'
smean <- function(x, ...){
  UseMethod('smean')
}

#' @rdname smean
#' @export
#'
smean.default = function(x, ids, na.rm = T, var_type = 'none', ci_method = 'mean',level = .95, use_df = T){

  var_type = match.arg(var_type, c('none', 'se', 'ci'), TRUE)
  ci_method = match.arg(ci_method, c('mean', 'beta', 'xlogit'))

  if(missing(ids)) stop('Please explicitly pass an id vector')

  sv = get_survey_vars()
  st = get_survey_type()

  #get the df for later
  #taken from survey:::degf.survey.design2
  if(st %in% 'svydt'){
    lids = length(ids)
    df = length(unique(sv[`_id` %in% ids, psu])) -length(unique(sv[`_id` %in% ids, strata]))
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

  psum<-sum(sv$weight[ids])
  average<-colSums(x*sv$weight[ids]/psum)
  x<-sweep(x,2,average)
  # v<-svyrecvar(x*pweights/psum,design$cluster,design$strata, design$fpc,
  #              postStrata=design$postStrata)


  ret = list(result = average)

  if(!all(var_type %in% 'none')){
    v<-survey::svyrecvar(x *sv$weight[ids]/psum,
                         data.frame(psu = sv$psu[ids]),
                         data.frame(strata = sv$strata[ids]),
                         list(popsize = NULL, sampsize = as.matrix(sv$sampsize[ids], ncol = 1)),
                         postStrata=NULL)

    if (is.matrix(v)){
      se <- sqrt(diag(v))
    }else{
      se <- sqrt(v)
    }

    if(any('se' %in% var_type)){
      ret$se = se
    }


    if(any('ci' %in% var_type)){
      #nicked from survey:::tconfint
      if(ci_method == 'mean') ci = average + se %o% qt(c((1-level)/2, 1-(1-level)/2), df = df)
      #nicked from survey::svyciprop
      if(ci_method %in% c('beta', 'xlogit')){
        m <- average
        attr(m, 'var') <- v
        names(m) = 1
        class(m) <- 'svystat'


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

  }

  names(ret) = NULL
  #r = t(ret)
  #if(is.factor(x))
  return(ret)
}

#' @rdname smean
#' @export
smean.character <- function(x, ...){
  #similar to smean.numeric, but to use a routine that mimics svyciprop for better confidence intervals
  stop(paste("Don't know how to deal with objects of class: 'character'. Consider converting into a factor before running smean"))

}
#' #' And internal routine to calculate
#' smean_num

