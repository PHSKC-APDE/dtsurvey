#' Tidyverse methods for dtsurvey objects. `_id` and attributes are sticky, use \link{as.data.frame} to let \code{dplyr}'s own methods drop them. Use these methods without the .dtsurvey suffix and after loading the tidyverse package with the generic (or after loading package tidyverse).
#' @param .data data.frame object
#' @param ... other arguments
#' @name dplyr
NULL

#' @name dplyr
#' @details \code{dplyr_reconstruct} Add the sdes and stype attributes back on so dtsurvey aggregation function still work
dplyr_reconstruct.dtsurvey = function(data, template) {

  attr(data, 'stype') = attr(template, 'stype')
  attr(data, 'sdes') = attr(template, 'sdes')
  class(data) <- class(template) #preserve dtsurvey class

  return(data)

}


#' @name dplyr
#' @details \code{select} keeps the id column regardless whether it is selected or not; to deselect it, first pipe through \code{as.data.frame} to let dplyr's own \code{select} drop it.
select.dtsurvey <- function(.data, ...) {
  #code largely borrowed from sf package

  a = NextMethod()
  a$`_id` <- .data$`_id` #make id be sticky

  return(a)

}

#' @name dplyr
dplyr_col_modify.dtsurvey <- function(data, cols){

  #check to see if `_id` is being used
  cols_used = attr(cols,'used')
  if(!'_id' %in% names(cols_used) || cols_used['_id']){
    stop('This operation is trying to modify the `_id` column in this dtsurvey. The `_id` column should not be modified because it will screw things up when doing survey calculations.')
  }

  NextMethod()
}

#' @name dplyr
group_by.dtsurvey = function(...){
  stop('group_by is not supported by dtsurvey objects. Consider converting the dtsurvey into a tbl_svy (from srvyr package) doing data mungunging under that framework and then convert back to a dtsurvey object once you are done (e.g. as.dtsurvey or as.dtrepsurvey).')
}

