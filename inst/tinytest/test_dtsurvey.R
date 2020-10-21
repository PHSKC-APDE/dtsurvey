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
expect_equal(as.numeric(SE(r1.1)), r1.2$se, info = 'Simple survey ses are equal')

#this time with bys
r2.1 = svyby(~num, ~byvar, og, svymean)
r2.2 = fake_sur[, smean(num, var_type = 'se', ids = `_id`), by = byvar]
setorder(r2.2, byvar)
expect_equal(r2.1$num, r2.2$V1, info = 'Simple survey means are equal, with one byvar')
expect_equal(as.numeric(SE(r2.1)), r2.2$se, info = 'Simple survey ses are equal, with one byvar')

#simple, with NA
r3.1 = svymean(~num_na, og, na.rm = T)
r3.2 = fake_sur[, smean(num_na, var_type = 'se', na.rm = TRUE, ids = `_id`)]
expect_equal(as.numeric(r3.1), r3.2$V1, info = 'Simple survey means are equal, with some NAs')
expect_equal(as.numeric(SE(r3.1)), r3.2$se, info = 'Simple survey ses are equal, with some NAs')

#simple, with NA and by
r4.1 = svyby(~num_na, ~byvar, og, svymean, na.rm = TRUE)
r4.2 = fake_sur[, smean(num_na, var_type = 'se', ids = `_id`), by = byvar]
setorder(r4.2, byvar)
expect_equal(r4.1$num_na, r4.2$V1, info = 'Simple survey means are equal, with one byvar and some NAs')
expect_equal(as.numeric(SE(r4.1)), r4.2$se, info = 'Simple survey ses are equal, with one byvar and some NAs')

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
r13.1 <- fake_sur[, smean(logi, ids = `_id`, var_type = 'ci', ci_method = 'xlogit')]
r13.2 <- fake_sur[, smean(logi, ids = `_id`, var_type = 'ci', ci_method = 'beta')]
r13.3 <- confint(svyciprop(~logi, og, method = 'xlogit', na.rm = T))
r13.4 <- confint(svyciprop(~logi, og, method = 'beta', na.rm = T))
expect_equal(unname(unlist(r13.1)[2:3]), as.vector(unname(r13.3)))

#alternative ci methods, with bys
r14.1 <- fake_sur[, smean(logi, ids = `_id`, var_type = 'ci', ci_method = 'xlogit'), byvar]
r14.2 <- fake_sur[, smean(logi, ids = `_id`, var_type = 'ci', ci_method = 'beta'), byvar]
r14.3 <- svyby(~logi, ~byvar, og, svyciprop, vartype = 'ci', method = 'xlogit', na.rm = T)
r14.4 <- svyby(~logi, ~byvar, og, svyciprop, vartype = 'ci', method = 'beta', na.rm = T)
expect_equal(unname(as.matrix(r14.1)), unname(as.matrix(r14.3)))
expect_equal(unname(as.matrix(r14.2)), unname(as.matrix(r14.4)))

#alternative ci methods, with filtering
r15.1 <- fake_sur[fact != 'a', smean(logi, ids = `_id`, var_type = 'ci', ci_method = 'xlogit'), byvar]
r15.2 <- fake_sur[fact != 'a', smean(logi, ids = `_id`, var_type = 'ci', ci_method = 'beta'), byvar]
r15.3 <- svyby(~logi, ~byvar, subset(og, fact != 'a'), svyciprop, vartype = 'ci', method = 'xlogit', na.rm = T)
r15.4 <- svyby(~logi, ~byvar, subset(og,fact != 'a'), svyciprop, vartype = 'ci', method = 'beta', na.rm = T)
expect_equal(unname(as.matrix(r15.1)), unname(as.matrix(r15.3)))
expect_equal(unname(as.matrix(r15.2)), unname(as.matrix(r15.4)))

#factors
r16.1 <- fake_sur[, smean(fact, ids = `_id`, var_type = 'se')]
r16.2 <- svymean(~fact, og, vartype = c('se'))
expect_equal(r16.1[, V1], unname(coef(r16.2)))
expect_equal(r16.1[, se], unname(SE(r16.2)))

#factors, assign
expect_silent(fake_sur[, blah_fact := smean(fact, ids = `_id`)])
expect_true(all(unique(fake_sur[, blah_fact])[[1]] == fake_sur[, smean(fact, ids = `_id`)]))

#factors, by
r17.1 <- fake_sur[, smean(fact, ids = `_id`, var_type = c('se', 'ci'), use_df = FALSE), byvar]
r17.2 <- as.data.table(svyby(~fact, ~byvar, og, svymean))
r17.2 <- melt(r17.2, id.vars = 'byvar')
setorder(r17.2, byvar, variable)
expect_equal(r17.1[,V1], r17.2[!substr(variable, 1,2) == 'se', value])
expect_equal(r17.1[,se], r17.2[substr(variable, 1,2) == 'se', value])
r17.3 = confint(svyby(~fact, ~byvar, og, svymean))
r17.1[, fact := rep(levels(fake_sur[, fact]),2)]
setorder(r17.1, fact, byvar)
expect_equal(unique(r17.1[,lower]), unname(r17.3[,1]))
expect_equal(unique(r17.1[,upper]), unname(r17.3[,2]))

#byna
r18.1 = fake_sur[, smean(num_na, ids = `_id`, var_type = 'se', use_df = FALSE), byna]
r18.2 = svyby(~num_na, ~byna, og, svymean, na.rm = T)
expect_false(length(r18.1[,V1]) ==  length(r18.2[,2]), info = 'dtsurvey returns a row when a by var is NA, survey does not')
expect_equal((r18.1[!is.na(byna),result, keyby = byna][,V1]), r18.2[,2], info = 'dtsurvey returns a row when a by var is NA, survey does not')

#multiby
r19.1 = fake_sur[, smean(num, ids = `_id`), keyby = .(byvar, logi)]
r19.2 = svyby(~num, ~byvar + logi, og, svymean, na.rm = T)
setorder(r19.1, logi, byvar)
expect_equal(r19.1[, V1], r19.2[, 'num'])

#confirm that a total, no bys or NAs are equal
r20.1 = svytotal(~num, og)
r20.2 = fake_sur[, stotal(num, var_type = 'se', ids = `_id`)]
expect_equal(as.numeric(r20.1), r20.2$V1, info = 'Simple rep survey totals are equal')
expect_equal(as.numeric(SE(r20.1)), r20.2$se, info = 'Simple rep totals ses are equal')

#this time with bys
r21.1 = svyby(~num, ~byvar, og, svytotal)
r21.2 = fake_sur[, stotal(num, var_type = 'se', ids = `_id`), by = byvar]
setorder(r21.2, byvar)
expect_equal(r21.1$num, r21.2$V1, info = 'Simple rep survey totals are equal, with one byvar')
expect_equal(as.numeric(SE(r21.1)), r21.2$se, info = 'Simple rep survey ses are equal, with one byvar')

#simple, with NA
r22.1 = svytotal(~num_na, og, na.rm = T)
r22.2 = fake_sur[, stotal(num_na, var_type = 'se', na.rm = TRUE, ids = `_id`)]
expect_equal(as.numeric(r22.1), r22.2$V1, info = 'Simple rep survey totals are equal, with some NAs')
expect_equal(as.numeric(SE(r22.1)), r22.2$se, info = 'Simple rep survey ses are equal, with some NAs')

#simple, with NA and by
r23.1 = svyby(~num_na, ~byvar, og, svytotal, na.rm = TRUE)
r23.2 = fake_sur[, stotal(num_na, var_type = 'se', ids = `_id`), by = byvar]
setorder(r23.2, byvar)
expect_equal(r23.1$num_na, r23.2$V1, info = 'Simple rep survey totals are equal, with one byvar and some NAs')
expect_equal(as.numeric(SE(r23.1)), r23.2$se, info = 'Simple rep survey ses are equal, with one byvar and some NAs')

#factors, by
r24.1 <- fake_sur[, stotal(fact, ids = `_id`, var_type = c('se', 'ci'), use_df = FALSE), keyby = byvar]
r24.2 <- as.data.table(svyby(~fact, ~byvar, og, svytotal))
r24.2 <- melt(r24.2, id.vars = 'byvar')
setorder(r24.2, byvar, variable)
expect_equal(r24.1[,V1], r24.2[!substr(variable, 1,2) == 'se', value])
expect_equal(r24.1[,se], r24.2[substr(variable, 1,2) == 'se', value])
r24.3 = confint(svyby(~fact, ~byvar, og, svytotal))
r24.1[, fact := rep(levels(fake_sur[, fact]),2)]
setorder(r24.1, fact, byvar)
expect_equal(unique(r24.1[,lower]), unname(r24.3[,1]))
expect_equal(unique(r24.1[,upper]), unname(r24.3[,2]))

#converting a survey object into a dtsurvey
r25 = as.dtsurvey(og)
expect_equivalent(r25, dtsurvey(fake, psu = 'psu', strata = 'strata', weight = 'weight', nest = T))

r26 = survey::svydesign(~1, strata = ~strata, data = fake, weights = ~weight)
expect_equivalent(as.dtsurvey(r26), dtsurvey(fake, psu = NULL, strata = 'strata', weight = 'weight', nest = T))

#ci method errors
expect_error(fake_sur[, smean(fact, ci_method = 'xlogit')])

#return naming conventions
fake_sur[, smean(fact)]
fake_sur[, smean(logi)]
fake_sur[, smean(num)]

fake_sur[, smean(fact), by = byvar]
fake_sur[, smean(fact, add_levels = TRUE), by = byvar]
fake_sur[, c('r1') := smean(fact), by = byvar] #assignment with factors don't really work because multiple returns for a single by grouping
fake_sur[, c('r2', 'l') := smean(fact, add_levels = TRUE), by = byvar]

fake_sur[, smean(logi), by = byvar]
fake_sur[, smean(num), by = byvar]

fake_sur[, smean(fact, var_type = c('se', 'ci')), by = byvar]
fake_sur[, smean(logi, var_type = c('se', 'ci')), by = byvar]
fake_sur[, smean(num, var_type = c('se', 'ci')), by = byvar]

fake_sur[, .(smean(logi), smean(num), t(smean(fact)))] #multiple computations without by can work, factors have to be transposed though
fake_sur[, .(smean(logi), smean(num), smean(fact))]

#can I make check for factors to return the transpose if in a ./list?
fake_sur[, .(t(smean(fact)))]
fake_sur[, (t(smean(fact)))] #looks the same as if it was in .() or list, but a matrix is returned instead of the expected vector
fake_sur[, .(t(smean(fact, var_type = 'ci')))]
fake_sur[, .(t(smean(fact, var_type = 'ci')), smean(num, var_type = 'ci'))] #transpose doesn't really work here. Also the smean(num) is being treated as three 3 rows

#The transpose doesn't matter outside the .() or list() context
fake_sur[, (t(smean(fact, var_type = 'ci')))]
fake_sur[, ((smean(fact, var_type = 'ci')))]



fake_sur[, .(smean(logi, var_type = c('se', 'ci')), smean(num, var_type = 'se'))] #where do all the extra columns go? They are long. Multiple computation is a bit tricky
fake_sur[, .((smean(logi, var_type = c('se', 'ci'))))] #the . or list notation makes everything go long
fake_sur[, .(t(smean(logi, var_type = c('se', 'ci'))))] #the transpose works, except with by
fake_sur[, .((smean(logi, var_type = c('se', 'ci')))), by = 'byvar'] # data is long rather than wide, which is desired

fs = fake_sur[, .((smean(logi, var_type = c('se', 'ci'))))] # why is this long?
fs = fake_sur[, .(t(smean(logi, var_type = c('se', 'ci')))), by = 'byvar'] #it doesn't like this

fake_sur[, .(t(smean(logi, var_type = c('se', 'ci'))))]
fake_sur[, smean(logi, var_type = c('se', 'ci'))]



