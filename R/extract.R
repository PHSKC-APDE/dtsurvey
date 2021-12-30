#' Custom extract function (or subset or whatever) for dtsurveys. A loose wrapper over `[.data.table`
#' @param x a dtsurvey object
#' @param i i in the data.table format
#' @param j j in the data.table format
#' @param by by in the data.table format
#' @param ... extra options passed to data table
#' @param drop used to make dplyr things work nice. should generally be ignored
#' @export
#' @name extract
"[.dtsurvey" <- function(x, i, j, by, ..., drop = NULL){


  #if being called by a function that is not data.table aware, borrow the extract logic from data.frame (code used from data.table)
  if (!data.table:::cedta()) {
        # Fix for #500 (to do)
    Nargs = nargs() - (!missing(drop))
    if (Nargs<3L) {
      ans = `[.data.frame`(x,i) # drop ignored anyway by DF[i]
      # if(!names(ans) %in% '_id') ans$`_id` <- x$`_id` -- won't work for select. Will probably have to override select
    } else if (missing(drop)){
      ans = `[.data.frame`(x,i,j)
      #if(!names(ans) %in% '_id') ans$`_id` = `[.data.frame`(x,i,'_id')
    } else{
      ans = `[.data.frame`(x,i,j,drop)
    }
    # added is.data.table(ans) check to fix bug #81
    if (!missing(i) && is.data.table(ans)) setkey(ans, NULL)  # drops index too; tested by plyr::arrange test in other.Rraw

    if(inherits(ans, 'data.frame')){
      attr(ans, 'stype') = attr(x, 'stype')
      attr(ans, 'sdes') = attr(x, 'sdes')
    }
    return(ans)
  }

  mc <- match.call()

  if(!missing(j)){
    #check to see if survey functions should be swapped in or out
    st = attr(x, 'stype')

    #if(is.null(st)) stop('dtsurvey `stype` attribute is NULL.')

    #This should be uneeded since the `[` method should cover it
    #is_svy = st %in% c('svydt', 'svyrepdt')

    jsub = substitute(j)
    #iterate all of the calls in j
    jsub = dtsurvey_j_calls(jsub, T, xname = mc[['x']], st)
    mc[["j"]] <- jsub

  }

  mc[[1]] <- quote(data.table:::`[.data.table`)
  res = eval.parent(mc)

  return(res)


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

#a function to match

