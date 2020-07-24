#' Stop a call in a dtsurvey if its part of an assignment
#' @export
stop_assignment <- function(){

  callme = sys.calls()
  callme = lapply(callme, as.character)

  #check to see if `:=` is in the call history
  callme = vapply(callme, function(x) any(grepl('`:=`', x, fixed = T)), TRUE)

  if(any(callme)){
    stop('This function does not work with `:=`')
  }

  return(NULL)

}
