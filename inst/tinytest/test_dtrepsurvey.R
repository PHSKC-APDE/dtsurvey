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
apiclus1$fact_na = factor(sample(c(NA,letters[1:5]), nrow(apiclus1), T))
fnas = data.frame(vapply(levels(apiclus1$fact_na), function(x) as.numeric(apiclus1$fact_na == x), rep(1, nrow(apiclus1))))
names(fnas) <- paste0('fct_na_', levels(apiclus1$fact_na))
apiclus1 = cbind(apiclus1, fnas)

dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
og = as.svrepdesign(dclus1)
herp = data.table(og$variables)
dt = dtrepsurvey(og)

#confirm that a mean, no bys or NAs are equal
r1.1 = svymean(~api00, og)
r1.2 = dt[, smean(api00, var_type = 'se', ids = `_id`)]
expect_equal(as.numeric(r1.1), r1.2[, result], info = 'Simple rep survey means are equal')
expect_equal(as.numeric(SE(r1.1)), r1.2$se, info = 'Simple rep survey ses are equal')

#this time with bys
r2.1 = svyby(~api00, ~dname, og, svymean)
r2.2 = dt[, smean(api00, var_type = 'se', ids = `_id`), by = dname]
setorder(r2.2, dname)
expect_equal(r2.1$api00, r2.2$result, info = 'Simple rep survey means are equal, with one byvar')
expect_equal(as.numeric(SE(r2.1)), r2.2$se, info = 'Simple rep survey ses are equal, with one byvar')

#simple, with NA
r3.1 = svymean(~apina, og, na.rm = T)
r3.2 = dt[, smean(apina, var_type = 'se', na.rm = TRUE, ids = `_id`)]
expect_equal(as.numeric(r3.1), r3.2$result, info = 'Simple rep survey means are equal, with some NAs')
expect_equal(as.numeric(SE(r3.1)), r3.2$se, info = 'Simple rep survey ses are equal, with some NAs')

#simple, with NA and by
r4.1 = svyby(~apina, ~stype, og, svymean, na.rm = TRUE)
r4.2 = dt[, smean(apina, var_type = 'se', ids = `_id`), by = stype]
setorder(r4.2, stype)
expect_equal(r4.1$apina, r4.2$result, info = 'Simple rep survey means are equal, with one byvar and some NAs')
expect_equal(as.numeric(SE(r4.1)), r4.2$se, info = 'Simple rep survey ses are equal, with one byvar and some NAs')

#try assignments
expect_silent(r5 <- dt[, blah := smean(api00, ids = `_id`, var_type = NULL)], info = 'Assignment is silent')
expect_equal(r5[1, blah], as.numeric(svymean(~api00, og, na.rm = T)),info = 'Assignment matches svymean')
expect_silent(r6 <- dt[, blah := smean(apina, ids = `_id`, na.rm = T, var_type = NULL), by = stype], info = 'Assignment with by is silent')
expect_equal(sort(unique(r6[, blah])), sort(svyby(~apina, ~stype, og, svymean, na.rm = T)$apina), info = 'Assignment is equal to svymean results')

#means, se, degrees of freedom
expect_silent(r7.1 <- dt[, smean(api00, ids = `_id`, var_type = c('se', 'ci'), use_df = FALSE)], info = 'Changing use_df is silent')
r7.2 <- svymean(~api00, og)
expect_equal(unname(unlist(r7.1)), unname(c(coef(r7.2), SE(r7.2), confint(r7.2))), info = 'use_df toggle recovers svymean results')

#Test alternative ways of calculating cis, first without by vars
r13.1 <- dt[, smean(bothnum, ids = `_id`, var_type = 'ci', ci_method = 'xlogit')]
r13.2 <- dt[, smean(bothnum, ids = `_id`, var_type = 'ci', ci_method = 'beta')]
r13.3 <- confint(svyciprop(~bothnum, og, method = 'xlogit', na.rm = T))
r13.4 <- confint(svyciprop(~bothnum, og, method = 'beta', na.rm = T))
expect_equal(unname(unlist(r13.1)[2:3]), as.vector(unname(r13.3)), info =  'xlogit works')
expect_equal(unname(unlist(r13.2)[2:3]), as.vector(unname(r13.4)), info = 'beta works')

# little filtering
r13.5 <- dt[stype == 'H', smean(bothnum, ids = `_id`, var_type = 'ci', ci_method = 'xlogit')]
r13.6 <- svyciprop(~bothnum, subset(og, stype == 'H'), method = 'xlogit', na.rm = T)
expect_equal(unname(unlist(r13.5)[2:3]),as.vector(unname(confint(r13.6))), info = 'xlogit with filtering')

#alternative ci methods, with bys
r14.1 <- dt[, smean(bothnum, ids = `_id`, var_type = 'ci', ci_method = 'xlogit'), keyby = stype]
r14.2 <- dt[, smean(bothnum, ids = `_id`, var_type = 'ci', ci_method = 'beta'), keyby = stype]
r14.3 <- svyby(~bothnum, ~stype, og, svyciprop, vartype = 'ci', method = 'xlogit', na.rm = T)
r14.4 <- svyby(~bothnum, ~stype, og, svyciprop, vartype = 'ci', method = 'beta', na.rm = T)
expect_equal(unname(as.matrix(r14.1[, 2:4])), unname(as.matrix(r14.3[2:4])), info = 'xlogit with bys')
expect_equal(unname(as.matrix(r14.2[, 2:4])), unname(as.matrix(r14.4[, 2:4])), info = 'beta with bys')

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
expect_equal(r16.1[, result], unname(coef(r16.2)))
expect_equal(r16.1[, se], unname(SE(r16.2)))

# #factors, assign: Note 6/10/21-- cannot remember what was being achieved here
# expect_silent(dt[, blah_fact := smean(awards, ids = `_id`)])
# expect_true(all(unique(dt[, blah])[[1]] == dt[, smean(awards, ids = `_id`)]))

#factors, by
r17.1 <- dt[, smean(awards, ids = `_id`, var_type = c('se', 'ci'), use_df = FALSE), keyby = stype]
r17.2 <- as.data.table(svyby(~awards, ~stype, og, svymean))
r17.2 <- melt(r17.2, id.vars = 'stype')
setorder(r17.2, stype, variable)
expect_equal(r17.1[,result], r17.2[!substr(variable, 1,2) == 'se', value])
expect_equal(r17.1[,se], r17.2[substr(variable, 1,2) == 'se', value])
r17.3 = confint(svyby(~awards, ~stype, og, svymean))
r17.1[, fact := rep(levels(dt[, awards]),3)]
setorder(r17.1, fact, stype)
expect_equal(unique(r17.1[,lower]), unname(r17.3[,1]))
expect_equal(unique(r17.1[,upper]), unname(r17.3[,2]))

#byna
r18.1 = dt[, smean(apina, ids = `_id`, var_type = 'se', use_df = FALSE), stypena]
r18.2 = svyby(~apina, ~stypena, og, svymean, na.rm = T)
expect_false(length(r18.1[,result]) ==  length(r18.2[,2]), info = 'dtsurvey returns a row when a by var is NA, survey does not')
expect_equal((r18.1[!is.na(stypena),result, keyby = stypena][,result]), r18.2[,2], info = 'dtsurvey returns a row when a by var is NA, survey does not')

#multiby
r19.1 = dt[, smean(api00, ids = `_id`, var_type = 'se'), keyby = .(stype, awards)]
r19.2 = svyby(~api00, ~stype + awards, og, svymean, na.rm = T)
setorder(r19.1, awards, stype)
expect_equal(r19.1[, result], r19.2[, 'api00'])

#confirm that a total, no bys or NAs are equal
r20.1 = svytotal(~api00, og)
r20.2 = dt[, stotal(api00, var_type = 'se', ids = `_id`)]
expect_equal(as.numeric(r20.1), r20.2$result, info = 'Simple rep survey totals are equal')
expect_equal(as.numeric(SE(r20.1)), r20.2$se, info = 'Simple rep totals ses are equal')

#this time with bys
r21.1 = svyby(~api00, ~dname, og, svytotal)
r21.2 = dt[, stotal(api00, var_type = 'se', ids = `_id`), by = dname]
setorder(r21.2, dname)
expect_equal(r21.1$api00, r21.2$result, info = 'Simple rep survey totals are equal, with one byvar')
expect_equal(as.numeric(SE(r21.1)), r21.2$se, info = 'Simple rep survey ses are equal, with one byvar')

#simple, with NA
r22.1 = svytotal(~apina, og, na.rm = T)
r22.2 = dt[, stotal(apina, var_type = 'se', na.rm = TRUE, ids = `_id`)]
expect_equal(as.numeric(r22.1), r22.2$result, info = 'Simple rep survey totals are equal, with some NAs')
expect_equal(as.numeric(SE(r22.1)), r22.2$se, info = 'Simple rep survey ses are equal, with some NAs')

#simple, with NA and by
r23.1 = svyby(~apina, ~stype, og, svytotal, na.rm = TRUE)
r23.2 = dt[, stotal(apina, var_type = 'se', ids = `_id`), by = stype]
setorder(r23.2, stype)
expect_equal(r23.1$apina, r23.2$result, info = 'Simple rep survey totals are equal, with one byvar and some NAs')
expect_equal(as.numeric(SE(r23.1)), r23.2$se, info = 'Simple rep survey ses are equal, with one byvar and some NAs')

#factors, by
r24.1 <- dt[, stotal(awards, ids = `_id`, var_type = c('se', 'ci'), use_df = FALSE), keyby = stype]
r24.2 <- as.data.table(svyby(~awards, ~stype, og, svytotal))
r24.2 <- melt(r24.2, id.vars = 'stype')
setorder(r24.2, stype, variable)
expect_equal(r24.1[,result], r24.2[!substr(variable, 1,2) == 'se', value])
expect_equal(r24.1[,se], r24.2[substr(variable, 1,2) == 'se', value])
r24.3 = confint(svyby(~awards, ~stype, og, svytotal))
r24.1[, fact := rep(levels(dt[, awards]),3)]
setorder(r24.1, fact, stype)
expect_equal(unique(r24.1[,lower]), unname(r24.3[,1]))
expect_equal(unique(r24.1[,upper]), unname(r24.3[,2]))

#passing in a data.frame of sorts
data(scd)
# use BRR replicate weights from Levy and Lemeshow
repweights<-2*cbind(c(1,0,1,0,1,0), c(1,0,0,1,0,1), c(0,1,1,0,0,1),
                    c(0,1,0,1,1,0))

r25.1 <- dtrepsurvey(svrepdesign(data=scd, type="BRR", repweights=repweights, combined.weights=FALSE))
r25.2 <- dtrepsurvey(scd, type="BRR", repweights=repweights, combined.weights=FALSE)
r25.3 <- dtrepsurvey(as.data.table(scd), type="BRR", repweights=repweights, combined.weights=FALSE)
expect_equal(r25.1, r25.2)
expect_equal(r25.1,r25.3)

#factors as proportions
#beta
r26.1 = dt[, smean(fact_na, var_type = c('ci'), ci_method = 'beta')]
r26.1 = data.table(r26.1)
levs = dt[, levels(fact_na)]
r26.2 = lapply(levs, function(x){
  r = svyciprop(as.formula(paste0('~','fct_na_',x, collapse = '')),og, 'beta', na.rm = T)
  r= c(r, confint(r))
  r = data.table(t(r))
  setnames(r, c('result', 'lower', 'upper'))
})
r26.2 = rbindlist(r26.2)[, levels := as.character(levs)]
setorder(r26.2, levels)
expect_equivalent(r26.1, r26.2)

#with NAs
r27.1 = dt[, smean(fact_na, var_type = c('ci'), ci_method = 'xlogit')]
r27.1 = data.table(r27.1)
r27.2 = lapply(levs, function(x){
  r = svyciprop(as.formula(paste0('~','fct_na_',x, collapse = '')),og, 'xlogit', na.rm = T)
  r= c(r, confint(r))
  r = data.table(t(r))
  setnames(r, c('result', 'lower', 'upper'))
})
r27.2 = rbindlist(r27.2)[, levels := as.character(levs)]
setorder(r27.2, levels)
expect_equivalent(r27.1, r27.2)

#weights of 0
apicopy = copy(apiclus1)
apicopy$pw2 = apicopy$pw
apicopy$pw2[apicopy$stype == 'H'] = 0
apc<-svydesign(id=~dnum, weights=~pw2, data=apicopy, fpc=~fpc)
apc = as.svrepdesign(apc)
dtapc = dtrepsurvey(apc)

#if a svyrep has only 0s as the weight for a calculation it'll throw an error
r28.1 = expect_error(svymean(~fact_na, subset(apc, stype == 'H')))

#DT survey changes this to return NaNs and NAs
r28.2 = dtapc[, smean(fact_na, var_type = 'ci', ci_method = 'xlogit'), by = stype]
expect_true(r28.2[stype == 'H', all(is.na(result))])
r28.3 = dtapc[stype != 'H', smean(fact_na, var_type = 'ci', ci_method = 'xlogit'), by = stype]
expect_equivalent(r28.2[stype != 'H'], r28.3)

