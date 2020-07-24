library('tinytest')
library('survey')
library('data.table')
library('dtsurvey')

#dumb survey
set.seed(98112)
data(api)
apiclus1$apina = apiclus1$api00
apiclus1$apina[sample(1:nrow(apiclus1), 10)] <- NA
apiclus1$stypena = apiclus1$stype
apiclus1$stypena[sample(1:nrow(apiclus1), 10)] <- NA
apiclus1$bothnum = as.numeric(apiclus1$both) - 1
dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
og = as.svrepdesign(dclus1)
dt = dtrepsurvey(og)

#confirm that a mean, no bys or NAs are equal
r1.1 = svymean(~api00, og)
r1.2 = dt[, smean(api00, var_type = 'se', ids = `_id`)]
expect_equal(as.numeric(r1.1), r1.2$V1, info = 'Simple rep survey means are equal')
expect_equal(as.numeric(SE(r1.1)), r1.2$V2, info = 'Simple rep survey ses are equal')

#this time with bys
r2.1 = svyby(~api00, ~dname, og, svymean)
r2.2 = dt[, smean(api00, var_type = 'se', ids = `_id`), by = dname]
setorder(r2.2, dname)
expect_equal(r2.1$api00, r2.2$V1, info = 'Simple rep survey means are equal, with one byvar')
expect_equal(as.numeric(SE(r2.1)), r2.2$V2, info = 'Simple rep survey ses are equal, with one byvar')

#simple, with NA
r3.1 = svymean(~apina, og, na.rm = T)
r3.2 = dt[, smean(apina, var_type = 'se', na.rm = TRUE, ids = `_id`)]
expect_equal(as.numeric(r3.1), r3.2$V1, info = 'Simple rep survey means are equal, with some NAs')
expect_equal(as.numeric(SE(r3.1)), r3.2$V2, info = 'Simple rep survey ses are equal, with some NAs')

#simple, with NA and by
r4.1 = svyby(~apina, ~stype, og, svymean, na.rm = TRUE)
r4.2 = dt[, smean(apina, var_type = 'se', ids = `_id`), by = stype]
setorder(r4.2, stype)
expect_equal(r4.1$apina, r4.2$V1, info = 'Simple rep survey means are equal, with one byvar and some NAs')
expect_equal(as.numeric(SE(r4.1)), r4.2$V2, info = 'Simple rep survey ses are equal, with one byvar and some NAs')

#try assignments
expect_silent(r5 <- dt[, blah := smean(api00, ids = `_id`, var_type = NULL)])
expect_equal(r5[1, blah], as.numeric(svymean(~api00, og, na.rm = T)))
expect_silent(r6 <- dt[, blah := smean(apina, ids = `_id`, na.rm = T, var_type = NULL), by = stype])
expect_equal(sort(unique(r6[, blah])), sort(svyby(~apina, ~stype, og, svymean, na.rm = T)$apina))

#means, se, degrees of freedom
expect_silent(r7.1 <- dt[, smean(api00, ids = `_id`, var_type = c('se', 'ci'), use_df = FALSE)])
r7.2 <- svymean(~api00, og)
expect_equal(unname(unlist(r7.1)), unname(c(coef(r7.2), SE(r7.2), confint(r7.2))))

#Test alternative ways of calculating cis, first without by vars
r13.1 <- dt[, smean(bothnum, ids = `_id`, var_type = 'ci', ci_method = 'xlogit')]
r13.2 <- dt[, smean(bothnum, ids = `_id`, var_type = 'ci', ci_method = 'beta')]
r13.3 <- confint(svyciprop(~bothnum, og, method = 'xlogit', na.rm = T))
r13.4 <- confint(svyciprop(~bothnum, og, method = 'beta', na.rm = T))
expect_equal(unname(unlist(r13.1)[2:3]), as.vector(unname(r13.3)))

# little filtering
r13.5 <- dt[stype == 'H', smean(bothnum, ids = `_id`, var_type = 'ci', ci_method = 'xlogit')]
r13.6 <- svyciprop(~bothnum, subset(og, stype == 'H'), method = 'xlogit', na.rm = T)
expect_equal(unname(unlist(r13.5)[2:3]),as.vector(unname(confint(r13.6))))

#alternative ci methods, with bys
r14.1 <- dt[, smean(bothnum, ids = `_id`, var_type = 'ci', ci_method = 'xlogit'), keyby = stype]
r14.2 <- dt[, smean(bothnum, ids = `_id`, var_type = 'ci', ci_method = 'beta'), keyby = stype]
r14.3 <- svyby(~bothnum, ~stype, og, svyciprop, vartype = 'ci', method = 'xlogit', na.rm = T)
r14.4 <- svyby(~bothnum, ~stype, og, svyciprop, vartype = 'ci', method = 'beta', na.rm = T)
expect_equal(unname(as.matrix(r14.1[, 2:4])), unname(as.matrix(r14.3[2:4])))
expect_equal(unname(as.matrix(r14.2[, 2:4])), unname(as.matrix(r14.4[, 2:4])))

#alternative ci methods, with filtering
r15.1 <- dt[sch.wide != 'No', smean(bothnum, ids = `_id`, var_type = 'ci', ci_method = 'xlogit'), keyby = stype]
r15.2 <- dt[sch.wide != 'No', smean(bothnum, ids = `_id`, var_type = 'ci', ci_method = 'beta'), keyby = stype]
r15.3 <- svyby(~bothnum, ~stype, subset(og, sch.wide != 'No'), svyciprop, vartype = 'ci', method = 'xlogit', na.rm = T)
r15.4 <- svyby(~bothnum, ~stype, subset(og, sch.wide != 'No'), svyciprop, vartype = 'ci', method = 'beta', na.rm = T)
expect_equal(unname(as.matrix(r15.1)), unname(as.matrix(r15.3)))
expect_equal(unname(as.matrix(r15.2)), unname(as.matrix(r15.4)))

#factors
r16.1 <- dt[, smean(awards, ids = `_id`, var_type = 'se')]
r16.2 <- svymean(~awards, og, vartype = c('se'))
expect_equal(r16.1[, V1], unname(coef(r16.2)))
expect_equal(r16.1[, V2], unname(SE(r16.2)))

#factors, assign
expect_silent(dt[, blah_fact := smean(awards, ids = `_id`)])
expect_true(all(unique(dt[, blah_fact])[[1]] == dt[, smean(awards, ids = `_id`)]))

#factors, by
r17.1 <- dt[, smean(awards, ids = `_id`, var_type = c('se', 'ci'), use_df = FALSE), keyby = stype]
r17.2 <- as.data.table(svyby(~awards, ~stype, og, svymean))
r17.2 <- melt(r17.2, id.vars = 'stype')
setorder(r17.2, stype, variable)
expect_equal(r17.1[,V1], r17.2[!substr(variable, 1,2) == 'se', value])
expect_equal(r17.1[,V2], r17.2[substr(variable, 1,2) == 'se', value])
r17.3 = confint(svyby(~awards, ~stype, og, svymean))
r17.1[, fact := rep(levels(dt[, awards]),3)]
setorder(r17.1, fact, stype)
expect_equal(unique(r17.1[,V3]), unname(r17.3[,1]))
expect_equal(unique(r17.1[,V4]), unname(r17.3[,2]))

#byna
r18.1 = dt[, smean(apina, ids = `_id`, var_type = 'se', use_df = FALSE), stypena]
r18.2 = svyby(~apina, ~stypena, og, svymean, na.rm = T)
expect_false(length(r18.1[,V1]) ==  length(r18.2[,2]), info = 'dtsurvey returns a row when a by var is NA, survey does not')
expect_equal((r18.1[!is.na(stypena),V1, keyby = stypena][,V1]), r18.2[,2], info = 'dtsurvey returns a row when a by var is NA, survey does not')

#multiby
r19.1 = dt[, smean(api00, ids = `_id`, var_type = 'se'), keyby = .(stype, awards)]
r19.2 = svyby(~api00, ~stype + awards, og, svymean, na.rm = T)
setorder(r19.1, awards, stype)
expect_equal(r19.1[, V1], r19.2[, 'api00'])

