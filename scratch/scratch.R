library('data.table')
library('survey')
library('srvyr')

data(api)
a0 = svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
a0_sub = subset(a0, api00 >720)
a<-as_survey_design(a0)

sur_mean = a %>% summarize(mean = survey_mean(api00))


dt_survey = function(x, psu = NULL, strata = NULL, weight = NULL){

  x = data.table(x)

  #add the various attributes

  if(!is.null(psu)){
    setattr(x, 'psu', x[, .SD, .SDcols = psu][[1]])
  }else{
    setattr(x, 'psu', rep(1, nrow(x)))
  }

  if(!is.null(weight)){
    setattr(x, 'weight', x[, .SD, .SDcols = weight][[1]])
  }else{
    setattr(x, 'weight', rep(1, nrow(x)))
  }

  if(!is.null(strata)){
    setattr(x, 'strata', x[, .SD, .SDcols = strata][[1]])
  }else{
    setattr(x, 'strata', rep(1, nrow(x)))
  }


  #create a row id to help keep track of which rows are active
  x[, `_id` := .I]

  return(x)

}

b <- dt_survey(apiclus1, 'dnum', weight = 'pw')


dt_mean = function(x, col, by = NULL, na.rm = T){
  x[, list(mean = sum(get(col) * attr(x, 'weight')[`_id`]/sum(attr(x, 'weight')[`_id`])),
           se = svyCprod()), by = by]
}

dt_mean(b, 'api00', by = 'stype')
a %>% group_by(stype) %>% summarize(mean = survey_mean(api00))
