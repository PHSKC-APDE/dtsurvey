#' Calculate the survey weighted mean of an object
#'
#' @param x vector. Variable to compute a mean
#' @param na.rm logical. Determines whether NA values will be omitted from the analysis
#' @param as_list logical. Determines whether the result should returned as a list-- mostly for factors
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
#' @export
sur_mean.default = function(x, na.rm = T, as_list = FALSE, sv, ids, st){
  #Make sure the various inputs are accounted for
  check_survey_bits(ids, sv, st)

  #prep x and ids (mostly removing NAs)
  ids = prep_ids(x, ids, na.rm)
  x = prep_x(x, na.rm)

  if(st %in% 'svydt'){
    r = (surdes_mean(x, ids, sv))

  }

  if(st %in% 'svyrepdt'){
    r = (repdes_mean(x, ids, sv))
  }
  if(as_list) r = list(list(r)) #data.table unwraps a level
  return(r)




}

#' @rdname sur_mean
#' @export
sur_mean.factor = function(x, na.rm = T, as_list = FALSE , sv, ids, st){
  level_x = levels(x)
  r = sur_mean.default(x = x, na.rm = na.rm, as_list = as_list, sv = sv, ids = ids, st = st)


  if(as_list){
    names(r[[1]][[1]]) = level_x
  }else{
    names(r) <- level_x
  }

  r

}


#' @rdname sur_mean
#' @export
sur_mean.character <- function(x, ...){
  stop('Mean of a character is not implemented. Please cast as a factor before hand')
}

#' Calculate the mean from a normal survey
surdes_mean <- function(x,ids, sv){
  psum<-sum(sv$weight[ids])
  average<-colSums(x*sv$weight[ids]/psum)
  return(average)
}

#' calculate the mean for a replicate survey
repdes_mean <- function(x, ids, sv){
  colSums(sv[ids, pweights] * x)/sum(sv[ids, pweights])
}
