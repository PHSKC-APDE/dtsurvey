library('tinytest')
library('survey')
library('data.table')
library('dtsurvey')
library(profvis)

#dumb survey
set.seed(98112)
size = 10000000
fake = data.table(num = runif(size),
                  fact = as.factor(sample(letters[1:5], size, TRUE)),
                  logi = sample(c(TRUE, FALSE), size, TRUE),
                  weight = 1,
                  strata = sample(1:10, size, TRUE),
                  byvar = sample(1:100, size, TRUE),
                  byna = sample(c(NA, 1, 2), size , TRUE))
fake[, psu := as.numeric(paste0(strata, sample(1:5, size, TRUE)))]
fake[, num_na := num]
fake[sample(seq_len(size), 20), num_na := NA]
fake = fake[sample(seq_len(nrow(fake)),size = nrow(fake))]

fake_sur1 = dtsurvey(fake, psu = 'psu', strata = 'strata', weight = 'weight', nest = T)

#a <- profvis(fake_sur1[, smean(num, ids = '_id'), by = 'byvar'])

fake_sur2 = dtsurvey(fake, psu = 'psu', strata = 'strata', weight = 'weight', nest = T)
fake_sur3 = dtsurvey(fake, psu = 'psu', strata = 'strata', weight = 'weight', nest = T)

mean2 = function(x) mean(x)

system.time(fake_sur1[, mean(num), by = 'strata', verbose = T])
print("")
system.time(fake_sur2[, smean(num, ids = '_id'), by = 'strata', verbose = T])
print("")
system.time(fake_sur3[, mean2(num), by = 'strata', verbose = T])

a <- profvis(fake_sur2[, smean(num, ids = '_id'), by = 'byvar'])



#which(sv[,`_id` %in% ids])
idz = fake_sur1[byvar %in% c(1,3,5), `_id`]
system.time(fake_sur1[`_id` %in% idz])
system.time(which(fake_sur1[,`_id` %in% idz]))
