#' Tidyverse methods for dtsurvey objects. `_id` and attributes are sticky, use \link{as.data.frame} to let \code{dplyr}'s own methods drop them. Use these methods without the .dtsurvey suffix and after loading the tidyverse package with the generic (or after loading package tidyverse).
#' @param .data data object of class \link{sf}
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

#borrowed from sf (and I guess hms)
# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R
# Thu Apr 19 10:53:24 CEST 2018
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

.onLoad = function(libname, pkgname) {
  if ( requireNamespace("dplyr", quietly=TRUE) ){
    register_s3_method("dplyr", "select", "dtsurvey")
    register_s3_method("dplyr", "dplyr_reconstruct", "dtsurvey")
    register_s3_method("dplyr", "dplyr_col_modify", "dtsurvey")
    register_s3_method("dplyr", "group_by", "dtsurvey")
  }
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

