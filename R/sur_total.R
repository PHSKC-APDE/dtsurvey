#' Calculate the survey weighted total of an object
#'
#' @param x vector. Variable to compute a total
#' @param na.rm logical. Determines whether NA values will be omitted from the analysis
#' @param as_list logical. Determines whether the result should returned as a list-- mostly for factors
#' @param sv data.table. Survey vars data.table
#' @param ids numeric. vector of indices to operate on
#' @param st character. survey type
#' @param ... unused
#' @rdname sur_total
#' @export
#'
sur_total = function(x, ...){
  UseMethod('sur_total')
}

#' @rdname sur_total
#' @export
sur_total.default = function(x, na.rm = T, as_list = FALSE, sv, ids, st, ...){
  #Make sure the various inputs are accounted for
  check_survey_bits(ids, sv, st)

  #prep x and ids (mostly removing NAs)
  ids = prep_ids(x, ids, na.rm)
  x = prep_x(x, na.rm)

  if(st %in% 'svydt'){
    r = (surdes_total(x, ids, sv))

  }

  if(st %in% 'svyrepdt'){
    r = (repdes_total(x, ids, sv))
  }

  if(st %in% 'admin'){
    r = admin_total(x)
  }

  if(as_list) r = list(list(r)) #data.table unwraps a level
  return(r)




}

#' @rdname sur_total
#' @export
sur_total.factor = function(x, na.rm = T, as_list = FALSE , sv, ids, st, ...){
  level_x = levels(x)
  r = sur_total.default(x = x, na.rm = na.rm, as_list = as_list, sv = sv, ids = ids, st = st)


  if(as_list){
    names(r[[1]][[1]]) = level_x
  }else{
    names(r) <- level_x
  }

  r

}


#' @rdname sur_total
#' @export
sur_total.character <- function(x, ...){
  stop('total of a character is not implemented. Please convert to a factor and rerun.')
}

#' Calculate the total from a normal survey
#' @rdname sur_total
surdes_total <- function(x,ids, sv){
  #if(identical(x, matrix(integer(0), nrow = 0, ncol = 1))) return(NaN)
  colSums(x*sv$weight[ids])

}

#' calculate the total for a replicate survey
#' @rdname sur_total
repdes_total <- function(x, ids, sv){
  #if(identical(x, matrix(integer(0), nrow = 0, ncol = 1))) return(NaN)
  colSums(sv[ids, pweights] * x)
}

#' calculate admin total
#' @rdname sur_total
admin_total <- function(x){
  colSums(x)
}
