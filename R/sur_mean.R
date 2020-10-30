#' Calculate the survey weighted mean of an object
#'
#' @param x vector. Variable to compute a mean
#' @param na.rm logical. Determines whether NA values will be omitted from the analysis
#' @param add_levels logical. Determines whether the result should return a two item list (the results, level names)
#' @param sv data.table. Survey vars data.table
#' @param ids numeric. vector of indices to operate on
#' @param st character. survey type
#'
#' @rdname sur_mean
#' @export
#'
sur_mean = function(x, ...){
  UseMethod('sur_mean')
}

#' @rdname sur_mean
sur_mean.default = function(x, na.rm = T, sv, ids, st){
  #Make sure the various inputs are accounted for
  check_survey_bits(ids, sv, st)

  #prep x and ids (mostly removing NAs)
  ids = prep_ids(ids, na.rm)
  x = prep_x(x, na.rm)

  if(st %in% 'svydt'){
    return(surdes_mean(x, ids, sv))
  }

  if(st %in% 'svyrepdt'){
    return(repdes_mean(x, ids, sv))
  }

  stop("Invalid survey type passed")



}

#' @rdname sur_mean
sur_mean.factor = function(x, na.rm = T, add.levels = FALSE, sv, ids, st){
  level_x = levels(x)
  r = sur_mean.default(x, na.rm, sv, ids, st)
  if(add.levels){
    list(r, level = level_x)
  }else{
    names(r) <- level_x
  }

  r

}


#' @rdname sur_mean
sur_mean.character <- function(x, ...){
  stop('Mean of a character is not implemented. Please cast as a factor before hand')
}

#' Calculate the mean from a normal survey
surdes_mean <- function(x,ids, sv){
  psum<-sum(sv$weight[ids])
  average<-colSums(x*sv$weight[ids]/psum)
  return(average)
}

#' calcualte the mean for a replicate survey
repdes_mean <- function(x, ids, sv){
  colSums(sv[ids, pweights] * x)/sum(sv[ids, pweights])
}
