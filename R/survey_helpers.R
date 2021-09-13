
calc_surdes_var <- function(x, ids, sv){
  x <- sweep(x, 2, surdes_mean(x = x, ids = ids, sv = sv)) #default function is "-"

  v<-survey::svyrecvar(x *sv$weight[ids]/sum(sv$weight[ids]),
                       data.frame(psu = sv$psu[ids]),
                       data.frame(strata = sv$strata[ids]),
                       list(popsize = NULL, sampsize = as.matrix(sv$sampsize[ids], ncol = 1)),
                       postStrata=NULL)

}

prep_x <- function(x, na.rm = T){
  if(na.rm){
    x <- x[!is.na(x)]
  }

  #construct a model matrix if needed
  if(is.factor(x)){
    x = model.matrix(~0+x, data = data.table::data.table(x = x))
  }else{
    x = matrix(x, ncol = 1)
  }

  return(x)
}

prep_ids <- function(x, ids, na.rm = T){
  if(na.rm) ids <- ids[!is.na(x)]

  return(ids)
}

check_survey_bits <- function(ids, sv, st){
  if(missing(ids)) stop('Please explicitly pass an id vector')
  if(missing(sv)) stop('Please explicitly pass an survey variables data.table')
  if(missing(st)) stop('Please explicitly pass a survey type')
}

#' Get the survey variables from a dtsurvey
#'
#' @param sur a dtsurvey or dtrepsurey object
#' @export
#'
sv <- function(sur){
  stopifnot(inherits(sur, 'dtsurvey'))

  attr(sur, 'sdes')

}


#' Get the survey type from a dtsurvey
#'
#' @param sur a dtsurvey or dtrepsurey object
#' @export
#'
st <- function(sur){
  stopifnot(inherits(sur, 'dtsurvey'))

  attr(sur, 'stype')

}
