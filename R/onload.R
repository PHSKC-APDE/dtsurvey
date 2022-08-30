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
  register_s3_method('base', 'merge', 'dtsurvey')
}
