#' A function to create a dtsvyrep from a data.frame or svyrep.design type object
#' @param svyrep svyrep.design or an object that inherits data.frame.
#' @param ... options passed to survey::svrepdesign when svyrep is a data.frame object.
#'            Ignored for svyrep which already contains the necessary information for transformation
#' @export
#' @importFrom data.table data.table as.data.table setattr ".I" setnames
#'
#'
dtrepsurvey <- function(svyrep, ...){
  UseMethod('dtrepsurvey', svyrep)
}

#' @rdname dtrepsurvey
#' @export
dtrepsurvey.data.frame <- function(svyrep, ...){
  dtrepsurvey(survey::svrepdesign(svyrep, ...))
}

#' @rdname dtrepsurvey
#' @export
dtrepsurvey.svyrep.design <- function(svyrep, ...){

  #bindings to get past rcheck
  sv <- ids <- scaledata <- cw <- mse <- selfrep <- NULL


  stopifnot(inherits(svyrep, 'svyrep.design'))
  #extract the variables and convert to data.frame
  DT = data.table::as.data.table(svyrep$variables)
  DT[, `_id` := .I]


  sdes = data.table::data.table(pweights = svyrep$pweights)
  sdes[, `_id`:= .I]

  if(inherits(svyrep$repweights, 'repweights_compressed')){
    #this will decompress the weights but whatever
    idx = svyrep$repweights$index
    wts = data.table(svyrep$repweights$weights)[, `_id` := .I]
    sdes = data.table(`_id` = idx)
    sdes = merge(sdes, wts, by = '_id', all.x = T)
    rm(idx); rm(wts);
    data.table::setorder(sdes, `_id`)
    sdes[, `_id` := .I]

  }else{
    sdes = data.table::data.table(svyrep$repweights)
    setnames(sdes, names(sdes), paste0('V', seq_len(ncol(sdes))))
    sdes[, `_id` := .I]
  }
  data.table::setnames(sdes, paste0('V', seq_len(ncol(sdes)-1)), paste0('rep', seq_len(ncol(sdes)-1)))

  sdes[, pweights := svyrep$pweights]

  data.table::setattr(DT, 'sdes', sdes)
  data.table::setattr(DT, 'stype', 'svyrepdt')
  data.table::setattr(DT, 'scaledata', list(scale = svyrep$scale, rscales = svyrep$rscales))
  data.table::setattr(DT, 'combined.weights', svyrep$combined.weights)
  data.table::setattr(DT, 'mse', svyrep$mse)
  data.table::setattr(DT, 'selfrep', svyrep$selfrep)
  setattr(DT, 'class', c('dtrepsurvey', 'dtsurvey', class(DT)))

  return(DT)
}
