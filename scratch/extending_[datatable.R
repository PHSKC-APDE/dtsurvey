library('data.table')

a = data.table(v1 = 1:100, v2 = 1:2)
b = copy(a)

"[.dtsurvey" <- function(x, i, j, by, ...){

  mc <- match.call()

  #check to see if survey functions should be swapped in or out
  st = get_survey_type()
  swap = st %in% c('svydt')


  #extract J




  #

  mc[["j"]] <- do.call(substitute, list(substitute(j), list(mean = quote(median))))
  mc[[1]] <- quote(`[`)
  mc[[2]] <- substitute(setDT(x))
  eval.parent(mc)

  #reclass when using `:=`

}
class(a) <- c('dtsurvey', class(a))
a[, mean(v1), by = v2]

#
#
# "[.dtsurvey" <- function(x, i, j, by, ...){
#   mc <- match.call()
#   j <- substitute(j)
#   j <- do.call(substitute, list(j, list(mean = quote(median))))
#   mc[["j"]] <- j
#   mc[[1]] <- quote(data.table:::`[.data.table`)
#   eval.parent(mc)
# }
#
# class(a) <- c('dtsurvey', class(a))
# a[, mean(v1)]


