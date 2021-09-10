#' Construct a dtsurvey object
#'
#' @param DT data.table. A data.table containing the survey results
#' @param psu character. Name of the variable(s) containing the psu information. NULL indicates no PSUs (e.g. each observation gets its own)
#' @param strata character. Name of the variable(s) containing the strata information. Null indicates no strata (e.g. all obs belong to the same one)
#' @param weight character. Name of the variables containing the pweight information. If NULL, then weights default to 1 (and what even is the point then)
#' @param nest logical. If true, will re-nest PSUs within strata.
#'             If false, PSUs will be accepted as is and will throw an error when PSUs belong to multiple strata
#'
#' @importFrom data.table 'data.table' ":=" ".SD" ".GRP" ".I" "copy" ".N"
#'
#' @export
dtsurvey = function(DT, psu = NULL, strata = NULL, weight = NULL, nest = TRUE){

  #global bindings
  . <- sampsize <- NULL

  DT = data.table::data.table(DT)
  stopifnot('Dtsurvey creates a column called `_id` in DT to help keep track of things.
            DT already has a column named `_id`. Please rename that column and rerun.' = !"_id" %in% names(DT))

  #confirm that all the specified columns are in the dataset
  cols = c(psu, strata, weight)[!c(psu, strata, weight) %in% names(DT)]
  if(length(cols)>0){
    stop(paste('These columns were not found in DT:'), paste0(cols, collapse = ', '))
  }

  #confirm that there is no missing design variables
  if(!is.null(c(psu, strata, weight))){
    miss_chk = unlist(DT[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = c(psu, strata, weight)])
    miss = which(miss_chk >0)
    if(length(miss)>0){
      stop(paste('Missing values in:', paste0(names(miss), collapse = ', ')))
    }
    sdes = data.table::copy(DT[, .SD, .SDcols = c(psu, strata, weight)])
  }else{
    sdes = data.table::copy(DT[, .(hold = .I)])
  }



  if(is.null(psu)){
    sdes[, psu := .I]
    psu = 'psu'
  }else if(length(psu>1)){
    sdes[, psu := .GRP, by = psu]
    psu = 'psu'
  }

  if(is.null(weight)){
    sdes[, weight := rep(1, nrow(DT))]
    weight = 'weight'
  }else if(length(weight)>1){
    sdes[, (weight) := lapply(.SD, log), .SDcols = weight]
    sdes[, weight := rowSums(.SD), .SDcols = c(weight)]
    weight = 'weight'
  }

  if(is.null(strata)){
    sdes[, strata := rep(1, nrow(DT))]
    strata = 'strata'
  }else if(length(strata) > 1){
    sdes[, strata := .GRP, by = strata]
    strata = 'strata'
  }

  setnames(sdes, c(psu, weight, strata), c('psu', 'weight', 'strata'))
  sdes = sdes[, .(psu, weight, strata)]

  #create a row id to help keep track of which rows are active

  DT[, `_id` := .I]
  sdes[, `_id` := .I]

  if(nest){
    sdes[, psu := .GRP, by = .(strata, psu)]
  }

  #confirm PSUs are nested within strata
  nests =unique(sdes[, .(strata, psu)])
  if(any(nests[, .N, psu]$N>1)){
    stop('Some PSUs are in multiple strata. Did you forget to set nest = TRUE ?')
  }

  sdes[, sampsize := length(unique(psu)), strata]

  data.table::setattr(DT, 'sdes', sdes)
  data.table::setattr(DT, 'stype', 'svydt')

  setattr(DT, 'class', c('dtsurvey', class(DT)))

  return(DT)

}

#' Convert a survey::svydesign object into a dtsurvey
#' @param sur svydesign object
#' This function converts a survey.design2 object into a dtsurvey object.
#' Finite population corrections (fpcs) are not implemented in dtsurvey yet and therefore will also through an error here.
#' @export
as.dtsurvey = function(sur){

  stopifnot('Must by a survey.design2 object' = inherits(sur, "survey.design2"))

  #parse the call
  call <- as.list(match.call(survey::svydesign, sur$call))

  nest = as.character(ifelse(is.null(call$nest), formals(survey::svydesign)$nest, call$nest))
  nest = as.logical(nest)
  psu = as.character(call$ids)[-1]
  strata = as.character(call$strata)[-1]
  weight = as.character(call$weights)[-1]

  if(!is.null(call$probs)) stop('dtsurvey does not use probs. Use weights instead')
  if(!is.null(call$fpc)) stop('dtsurvey does not use fpc')
  if(psu %in% c('0','1')) psu = NULL

  dtsurvey(DT = sur$variables,
           psu = psu,
           strata = strata,
           weight = weight,
           nest = nest)

}


