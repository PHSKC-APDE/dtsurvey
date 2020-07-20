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
                  byna = sample(c(NA, 1, 2), size , TRUE))
fake[, psu := as.numeric(paste0(strata, sample(1:5, size, TRUE)))]
fake[, num_na := num]
fake[sample(seq_len(size), 20), num_na := NA]
setorder(fake, byvar)

#make sure the survey package will take it
expect_silent(og <- survey::svydesign(~psu, strata = ~strata, data = fake, weights = ~weight), info = 'Fake data works with survey package')

fake_sur = dtsurvey(fake, psu = 'psu', strata = 'strata', weight = 'weight', nest = T)

#confirm that a mean, no bys or NAs are equal
r1.1 = svymean(~num, og)
r1.2 = fake_sur[, smean(num, var_type = 'se', ids = `_id`)]
expect_equal(as.numeric(r1.1), r1.2$V1, info = 'Simple survey means are equal')
expect_equal(as.numeric(SE(r1.1)), r1.2$V2, info = 'Simple survey ses are equal')

#this time with bys
r2.1 = svyby(~num, ~byvar, og, svymean)
r2.2 = fake_sur[, smean(num, var_type = 'se', ids = `_id`), by = byvar]
setorder(r2.2, byvar)
expect_equal(r2.1$num, r2.2$V1, info = 'Simple survey means are equal, with one byvar')
expect_equal(as.numeric(SE(r2.1)), r2.2$V2, info = 'Simple survey ses are equal, with one byvar')

#simple, with NA
r3.1 = svymean(~num_na, og, na.rm = T)
r3.2 = fake_sur[, smean(num_na, var_type = 'se', na.rm = TRUE, ids = `_id`)]
expect_equal(as.numeric(r3.1), r3.2$V1, info = 'Simple survey means are equal, with some NAs')
expect_equal(as.numeric(SE(r3.1)), r3.2$V2, info = 'Simple survey ses are equal, with some NAs')

#simple, with NA and by
r4.1 = svyby(~num_na, ~byvar, og, svymean, na.rm = TRUE)
r4.2 = fake_sur[, smean(num_na, var_type = 'se', ids = `_id`), by = byvar]
setorder(r4.2, byvar)
expect_equal(r4.1$num_na, r4.2$V1, info = 'Simple survey means are equal, with one byvar and some NAs')
expect_equal(as.numeric(SE(r4.1)), r4.2$V2, info = 'Simple survey ses are equal, with one byvar and some NAs')

#try assignments
expect_silent(r5 <- fake_sur[, blah := smean(num, ids = `_id`, var_type = NULL)])
expect_equal(r5[1, blah], as.numeric(svymean(~num, og, na.rm = T)))
expect_silent(r6 <- fake_sur[, blah := smean(num_na, ids = `_id`, na.rm = T, var_type = NULL), by = byvar])
expect_equal(sort(unique(r6[, blah])), svyby(~num_na, ~byvar, og, svymean, na.rm = T)$num_na)

#means, se, degrees of freedom
expect_silent(r7.1 <- fake_sur[, smean(num, ids = `_id`, var_type = c('se', 'ci'), use_df = FALSE)])
r7.2 <- svymean(~num, og)
expect_equal(unname(unlist(r7.1)), unname(c(coef(r7.2), SE(r7.2), confint(r7.2))))

#Test alternative ways of calculating cis, first without by vars
r13.1 <- fake_sur[, smean(num_na, ids = `_id`, var_type = 'ci', ci_method = 'xlogit')]
r13.2 <- fake_sur[, smean(num_na, ids = `_id`, var_type = 'ci', ci_method = 'beta')]
r13.3 <- confint(svyciprop(~num_na, og, method = 'xlogit', na.rm = T))
r13.4 <- confint(svyciprop(~num_na, og, method = 'beta', na.rm = T))
expect_equal(unname(unlist(r13.1)[2:3]), as.vector(unname(r13.3)))

#alternative ci methods, with bys
r14.1 <- fake_sur[, smean(num_na, ids = `_id`, var_type = 'ci', ci_method = 'xlogit'), byvar]
r14.2 <- fake_sur[, smean(num_na, ids = `_id`, var_type = 'ci', ci_method = 'beta'), byvar]
r14.3 <- svyby(~num_na, ~byvar, og, svyciprop, vartype = 'ci', method = 'xlogit', na.rm = T)
r14.4 <- svyby(~num_na, ~byvar, og, svyciprop, vartype = 'ci', method = 'beta', na.rm = T)
expect_equal(unname(as.matrix(r14.1)), unname(as.matrix(r14.3)))
expect_equal(unname(as.matrix(r14.2)), unname(as.matrix(r14.4)))

#alternative ci methods, with filtering
r15.1 <- fake_sur[fact != 'a', smean(num_na, ids = `_id`, var_type = 'ci', ci_method = 'xlogit'), byvar]
r15.2 <- fake_sur[fact != 'a', smean(num_na, ids = `_id`, var_type = 'ci', ci_method = 'beta'), byvar]
r15.3 <- svyby(~num_na, ~byvar, subset(og, fact != 'a'), svyciprop, vartype = 'ci', method = 'xlogit', na.rm = T)
r15.4 <- svyby(~num_na, ~byvar, subset(og,fact != 'a'), svyciprop, vartype = 'ci', method = 'beta', na.rm = T)
expect_equal(unname(as.matrix(r15.1)), unname(as.matrix(r15.3)))
expect_equal(unname(as.matrix(r15.2)), unname(as.matrix(r15.4)))

#factors
r16.1 <- fake_sur[, smean(fact, ids = `_id`, var_type = 'se')]
r16.2 <- svymean(~fact, og, vartype = c('se'))
expect_equal(r16.1[, V1], unname(coef(r16.2)))
expect_equal(r16.1[, V2], unname(SE(r16.2)))

#factors, assign
expect_silent(fake_sur[, blah_fact := smean(fact, ids = `_id`)])
expect_true(all(unique(fake_sur[, blah_fact])[[1]] == fake_sur[, smean(fact, ids = `_id`)]))

#factors, by
r17.1 <- fake_sur[, smean(fact, ids = `_id`, var_type = c('se', 'ci'), use_df = FALSE), byvar]
r17.2 <- as.data.table(svyby(~fact, ~byvar, og, svymean))
r17.2 <- melt(r17.2, id.vars = 'byvar')
setorder(r17.2, byvar, variable)
expect_equal(r17.1[,V1], r17.2[!substr(variable, 1,2) == 'se', value])
expect_equal(r17.1[,V2], r17.2[substr(variable, 1,2) == 'se', value])
r17.3 = confint(svyby(~fact, ~byvar, og, svymean))
r17.1[, fact := rep(levels(fake_sur[, fact]),2)]
setorder(r17.1, fact, byvar)
expect_equal(unique(r17.1[,V3]), unname(r17.3[,1]))
expect_equal(unique(r17.1[,V4]), unname(r17.3[,2]))

#byna
r18.1 = fake_sur[, smean(num_na, ids = `_id`, var_type = 'se', use_df = FALSE), as.factor(byna)]
r18.2 = svyby(~num_na, ~as.factor(byna), og, svymean, na.rm = T)
expect_false(length(r18.1[,V1]) ==  length(r18.2[,2]), )
