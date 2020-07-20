#' Construct a dtsurvey object
#'
#' @param DT data.table. A data.table containing the survey results
#' @param psu character. Name of the variable(s) containing the psu information. NULL indicates no PSUs (e.g. each observation gets its own)
#' @param strata character. Name of the variable(s) containing the strata information. Null indicates no strata (e.g. all obs belong to the same one)
#' @param weight character. Name of the variables containing the pweight information. If NULL, then weights default to 1 (and what even is the point then)
#' @param nest logical. If true, will renest PSUs within strata.
#'             If false, PSUs will be accepted as is and will throw an error when PSUs belong to multiple strata
#'
#' @importFrom data.table data.table ":=" ".SD" ".GRP" ".I" copy
#' @export
#'
dtsurvey = function(DT, psu = NULL, strata = NULL, weight = NULL, nest = TRUE){

  DT = data.table::data.table(DT)

  #confirm that all the specified columns are in the dataset
  cols = setdiff(c(psu, strata, weight), names(DT))
  if(length(cols)>0){
    stop(paste('These columns were not found in DT:'), paste0(cols, collapse = ', '))
  }

  sdes = data.table::copy(DT[, .SD, .SDcols = c(psu, strata, weight)])

  if(is.null(psu)){
    sdes[, psu := .I]
  }

  if(is.null(weight)){
    sdes[, weight := rep(1, nrow(DT))]
  }

  if(is.null(strata)){
    sdes[, strata := rep(1, nrow(DT))]
  }

  #create a row id to help keep track of which rows are active
  DT[, `_id` := .I]
  sdes[, `_id` := .I]
  #confirm that there is no missing design variables
  miss = unlist(sdes[, lapply(.SD, function(x) sum(is.na(x)))])[1:3]

  if(any(miss>0)){
    stop(paste0('NA values found in the inputs for: ', paste0(c('psu', 'weight','strata')[miss>0], collapse = ', ')))
  }

  if(nest){
    sdes[, psu := .GRP, by = c(strata, psu)]
  }

  #confirm PSUs are nested within strata
  nests =unique(sdes[, .(strata, psu)])
  if(any(nests[, .N, psu]$N>1)){
    stop('Some PSUs are in multiple strata. Did you forget to set nest = TRUE ?')
  }

  sdes[, sampsize := length(unique(psu)), strata]

  data.table::setattr(DT, 'sdes', sdes)
  data.table::setattr(DT, 'stype', 'svydt')

  #class(DT) <- c('svydt', class(DT))

  return(DT)

}
