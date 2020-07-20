library('survey')
library('data.table')

#dumb survey
set.seed(98112)
size = 101
fake = data.table(num = runif(size),
                  fact = as.factor(sample(letters[1:5], size, TRUE)),
                  logi = sample(c(TRUE, FALSE), size, TRUE),
                  weight = 1,
                  strata = sample(1:10, size, TRUE),
                  byvar = sample(1:2, size, TRUE))
fake[, psu := as.numeric(paste0(strata, sample(1:5, size, TRUE)))]
fake[, num_na := num]
fake[sample(seq_len(size), 20), num_na := NA]
fake[, logi_num := as.numeric(logi)]
setorder(fake, byvar)

#make sure the survey package will take it
og <- survey::svydesign(~psu, strata = ~strata, data = fake, weights = ~weight)

r1 = confint(svymean(~logi_num,og))
r2 = confint(svyciprop(~logi_num, og, method = 'mean', df = Inf))
r3 = confint(svymean(~logi_num,og), df = degf(og))
r4 = confint(svyciprop(~logi_num, method = 'mean', og))

