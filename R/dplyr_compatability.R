#' Make dplyr work by adding on attributes
dplyr_reconstruct.dtsurvey = function(data, template) {
  attr(data, 'stype') = attr(template, 'stype')
  sdes(data, 'sdes') = attr(template, 'sdes')

  return(data)

}
