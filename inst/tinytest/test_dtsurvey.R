library('tinytest')
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

#make sure the survey package will take it
expect_silent(og <- survey::svydesign(~psu, strata = ~strata, data = fake, weights = ~weight), info = 'Fake data works with survey package')

fake_sur = dtsurvey(fake, psu = 'psu', strata = 'strata', weight = 'weight', nest = T)

#confirm that a mean, no bys or NAs are equal
r1 = svymean(~num, og)
r2 = fake_sur[, smean(num, ids = `_id`)]
expect_equal(as.numeric(r1), r2$V1, info = 'Simple survey means are equal')
expect_equal(as.numeric(SE(r1)), r2$V2, info = 'Simple survey ses are equal')

#this time with bys
r3 = svyby(~num, ~byvar, og, svymean)
r4 = fake_sur[, smean(num, ids = `_id`), by = byvar]
expect_equal(r3$num, r4$V1, info = 'Simple survey means are equal, with one byvar')
expect_equal(as.numeric(SE(r3)), r4$V2, info = 'Simple survey ses are equal, with one byvar')

#simple, with NA
r5 = svymean(~num, og)
r6 = fake_sur[, smean(num_na, na.rm = TRUE, ids = `_id`)]
expect_equal(as.numeric(r5), r4$V1, info = 'Simple survey means are equal, with some NAs')
expect_equal(as.numeric(SE(r3)), r4$V2, info = 'Simple survey ses are equal, with some NAs')

#simple, with NA and
r7 = svyby(~num_na, ~byvar, og, svymean, na.rm = TRUE)
r8 = fake_sur[, smean(num_na, ids = `_id`), by = byvar]
expect_equal(r7$num_na, r8$V1, info = 'Simple survey means are equal, with one byvar and some NAs')
expect_equal(as.numeric(SE(r7)), r8$V2, info = 'Simple survey ses are equal, with one byvar and some NAs')
