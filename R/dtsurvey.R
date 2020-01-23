#' Construct a dtsurvey object
#'
#' @param DT data.table. A data.table containing the survey results
#' @param psu character. Name of the variable(s) containing the psu information. NULL indicates no PSUs (e.g. each observation gets its own)
#' @param strata character. Name of the variable(s) containing the strata information. Null indicates no strata (e.g. all obs belong to the same one)
#' @param weight character. Name of the variables containing the pweight information. If NULL, then weights default to 1 (and what even is the point then)
#' @param nest logical. If true, will renest PSUs within strata.
#'             If false, PSUs will be accepted as is and will throw an error when PSUs belong to multiple strata
#'
#'
dtsurvey = function(DT, psu = NULL, strata = NULL, weight = NULL, nest = TRUE){

  DT = data.table(DT)

  #confirm that all the specified columns are in the dataset

  #confirm that there is no missing

  #add the various attributes

  #process the PSUs
    # make sure they exist
    # make sure there are no NAs
    # survey package converts to character from factor (not sure why)
    # store the PSUs in an attribute

  #process the weights
    #if NULL, set to 1
    #if !NULL, make sure there are no NAs
    #store the weights in an attribute

  #process strata
    #if null, set to 1
    #if !NULL check for NAs
    #store the strata in an attribute

  #construct psu/strata relations (ln ~145)
    # survey package checks if there is only one PSU (after check whether there is a valid strata and nesting is on) (line 145)
    # Nest subclusters in cluster
    # force clusters into strata
    # force substrata to nest in clusters



  if(!is.null(psu)){
    setattr(DT, 'psu', DT[, .SD, .SDcols = psu][[1]])
  }else{
    setattr(DT, 'psu', rep(1, nrow(DT)))
  }

  if(!is.null(weight)){
    setattr(DT, 'weight', DT[, .SD, .SDcols = weight][[1]])
  }else{
    setattr(DT, 'weight', rep(1, nrow(DT)))
  }

  if(!is.null(strata)){
    setattr(DT, 'strata', DT[, .SD, .SDcols = strata][[1]])
  }else{
    setattr(DT, 'strata', rep(1, nrow(DT)))
  }


  #create a row id to help keep track of which rows are active
  DT[, `_id` := .I]

  return(DT)

}
