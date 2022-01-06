library('tinytest')
library('survey')
library('data.table')
library('dtsurvey')

#dumb survey
set.seed(98112)
size = 101
fake = data.table(num = runif(size),
                  fact = as.factor(sample(letters[1:5], size, TRUE)),
                  logi = sample(c(TRUE, FALSE), size, TRUE),
                  weight = 1,
                  strata = sample(1:10, size, TRUE),
                  byvar = sample(1:2, size, TRUE),
                  byna = sample(c(NA, 1, 2), size , TRUE),
                  weight2 = runif(size, 1, 2))
fake[, psu := as.numeric(paste0(strata, sample(1:5, size, TRUE)))]
fake[, num_na := num]
fake[sample(seq_len(size), 20), num_na := NA]
setorder(fake, byvar)

fake_sur = dtsurvey(fake)
fake_sur2 = dtsurvey(fake, weight = 'weight2')

expect_equal(fake[, mean(num)], fake_sur[, mean(num)])
expect_equal(fake[, mean(num)], fake_sur[, smean(num)])
expect_equal(fake[, weighted.mean(num, weight2)], fake_sur2[, smean(num)])
expect_equal(fake_sur2[logi==FALSE, smean(num, ids = `_id`)], fake_sur2[logi==FALSE, smean(num)])


#is ID sticky
a = 'logi'
fake_sur[, .(psu, get(a))]
fake_sur[strata >5, .(psu, get(a))]
