#' Custom extract function (or subset or whatever) for dtsurveys. A loose wrapper over `[.data.table`
#' @param x a dtsurvey object
#' @param i i in the data.table format
#' @param j j in the data.table format
#' @param by by in the data.table format
#' @param ... extra options passed to data table
#' @export
#' @name extract
"[.dtsurvey" <- function(x, i, j, by, ...){


  if(!missing(j)){
    mc <- match.call()
    #check to see if survey functions should be swapped in or out
    st = attr(x, 'stype')
    is_svy = st %in% c('svydt', 'svyrepdt')

    jsub = substitute(j)
    #iterate all of the calls in j
    jsub = dtsurvey_j_calls(jsub, is_svy, xname = mc[['x']], st)
    mc[["j"]] <- jsub
    mc[[1]] <- quote(data.table:::`[.data.table`)
    res = eval.parent(mc)
  }else{
    NextMethod()
  }

  #browser()


}

#logic largely borrowed from replace_dot_alias in data.table
dtsurvey_j_calls = function(e, is_svy = TRUE, xname, st) {
  if (is.call(e) && !is.function(e[[1L]])) {
    #if calling a survey function
    if (e[[1L]] == 'smean' || e[[1L]] == 'stotal'){
      if(e[[1L]] == 'smean') e = match.call(call = e, definition = smean.default)
      if(e[[1L]] == 'stotal') e = match.call(call = e, definition = stotal.default)
      #and its a survey
      if(is_svy){
        #add ids if its not there
        if(!'ids' %in% names(e)) e[['ids']] = quote(`_id`)

        #get the name of the data.table making the call
        sv_replacement = quote(attr(x, 'sdes'))
        sv_replacement[[2]] <- xname

        if(!'sv' %in% names(e)) e[['sv']] = sv_replacement
        if(!'st' %in% names(e)) e[['st']] = st
      }
    }
    for (i in seq_along(e)[-1L]) if (!is.null(e[[i]])) e[[i]] = dtsurvey_j_calls(e[[i]], is_svy, xname, st)
  }

  e
}

