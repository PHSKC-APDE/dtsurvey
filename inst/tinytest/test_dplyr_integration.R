library('tinytest')
library('survey')
library('data.table')
library('dtsurvey')
if ( requireNamespace("dplyr", quietly=TRUE) ){
  library('dplyr')

  #dumb survey
  set.seed(98112)
  size = 101
  fake = data.table(num = runif(size),
                    fact = as.factor(sample(letters[1:5], size, TRUE)),
                    logi = sample(c(TRUE, FALSE), size, TRUE),
                    weight = 1,
                    strata = sample(1:10, size, TRUE),
                    byvar = sample(1:2, size, TRUE),
                    byna = sample(c(NA, 1, 2), size , TRUE))
  fake[, psu := as.numeric(paste0(strata, sample(1:5, size, TRUE)))]
  fake_sur = dtsurvey(fake, psu = 'psu', strata = 'strata', weight = 'weight', nest = T)

  expect_true('_id' %in% names(select(fake_sur, fact)), '_id is sticky')
  expect_true('_id' %in% names(select(fake_sur, -`_id`)), '_id is sticky')
  expect_true(all(c('sdes', 'stype') %in% names(attributes(select(fake_sur, fact)))), 'sdes and stype are sticky')
  expect_error(mutate(fake_sur, `_id` = NULL), 'somehow the `_id`',info = 'Missing id column')
  expect_error(mutate(fake_sur, `_id` = 1), 'This operation is trying to modify the `_id`',info = 'Touching ID does not work')
  expect_true(all(mutate(fake_sur, canada = 1)$canada ==1))
  expect_error(group_by(fake_sur,byvar), 'group_by is not supported by dtsurvey objects', info = 'group_by is not implemented')
  expect_true(inherits(mutate(fake_sur, var = 1), 'dtsurvey'), info = 'mutate returns dtsurvey')

  #what happens when someone is naughty and removes `_id`
  fake_sur$`_id` = NULL
  expect_error(all(mutate(fake_sur, canada = 2)$canada == 2))

}


