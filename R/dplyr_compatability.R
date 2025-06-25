#' Tidyverse methods for dtsurvey objects. `_id` and attributes are sticky, use [as.data.frame()] to let `dplyr`'s own methods drop them. Use these methods without the .dtsurvey suffix and after loading the tidyverse package with the generic (or after loading package tidyverse).
#' @param .data data.frame object
#' @param template template object to reconstruct
#' @param cols cols to keep
#' @param data data.frame object
#' @param ... other arguments
#' @name dplyr
NULL

#' @name dplyr
#' @details `dplyr_reconstruct` Add the sdes and stype attributes back on so dtsurvey aggregation function still work
#' @importFrom dplyr dplyr_reconstruct
#' @keywords internal
#' @export
#' @method dplyr_reconstruct dtsurvey
dplyr_reconstruct.dtsurvey = function(data, template) {

  attr(data, 'stype') = attr(template, 'stype')
  attr(data, 'sdes') = attr(template, 'sdes')
  class(data) <- class(template) #preserve dtsurvey class

  return(data)

}


#' @name dplyr
#' @details `select` keeps the id column regardless whether it is selected or not; to deselect it, first pipe through `as.data.frame` to let dplyr's own `select` drop it.
#' @importFrom dplyr select
#' @keywords internal
#' @export
#' @method select dtsurvey
select.dtsurvey <- function(.data, ...) {
  #code largely borrowed from sf package

  a = NextMethod()
  a$`_id` <- .data$`_id` #make id be sticky

  return(a)

}

#' @name dplyr
#' @importFrom dplyr dplyr_col_modify
#' @keywords internal
dplyr_col_modify.dtsurvey <- function(data, cols){

  #check to see if `_id` is being used
  cols_used = attr(cols,'used')

  if(is.na(cols_used['_id'])){
    stop('somehow the `_id` columns has gone missing. Either you are setting it to NULL or something else has gone wrong (e.g. a merge resulting in _id.x and _id.y')
  } else{
    if(cols_used['_id']){
      stop('This operation is trying to modify the `_id` column in this dtsurvey. The `_id` column should not be modified because it will screw things up when doing survey calculations.')
    }
  }
  NextMethod()
}

#' @name dplyr
#' @importFrom dplyr group_by
#' @keywords internal
#' @export
#' @method group_by dtsurvey
group_by.dtsurvey = function(...){
  stop('group_by is not supported by dtsurvey objects. Consider converting the dtsurvey into a tbl_svy (from srvyr package) doing data mungunging under that framework and then convert back to a dtsurvey object once you are done (e.g. as.dtsurvey or as.dtrepsurvey).')
}

