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


og_complex <- svydesign(id=~dnum, weights=~pw, data=apiclus1)
og_replicate <- as.svrepdesign(og_complex)
new_complex <- dtsurvey(apiclus1, psu = 'dnum', weight = 'pw')
new_replicate <- dtrepsurvey(og_replicate)

#Mean on numerics, no NAs, normal (complex survey)
a_1 = new_complex[, sur_mean(api00, na.rm = TRUE, sv = sv(new_complex), ids = `_id`, st = st(new_complex))]
b_1 = svymean(~api00, og_complex, TRUE)
expect_equal(a_1, unname(coef(b_1)) ,
             info = 'Mean of numerics, no NAs, normal survey')

#Mean on numerics, no NAs, normal (replicate survey)
a_2 = new_replicate[, sur_mean(api00, na.rm = TRUE, sv = sv(new_replicate), ids = `_id`, st = st(new_replicate))]
b_2 = svymean(~api00, og_replicate, TRUE)
expect_equal(a_2, unname(coef(b_2)) ,
             info = 'Mean of numerics, no NAs, replicate survey')

#Mean on numerics, with NAs, normal (complex survey)
a_3 = new_complex[, sur_mean(apina, na.rm = TRUE, sv = sv(new_complex), ids = `_id`, st = st(new_complex))]
b_3 = svymean(~apina, og_complex, TRUE)
expect_equal(a_3, unname(coef(b_3)) ,
             info = 'Mean of numerics, NAs, normal survey')

#Mean on numerics, with NAs, normal (replicate survey)
a_4 = new_replicate[, sur_mean(apina, na.rm = TRUE, sv = sv(new_replicate), ids = `_id`, st = st(new_replicate))]
b_4 = svymean(~apina, og_replicate, TRUE)
expect_equal(a_4, unname(coef(b_4)) ,
             info = 'Mean of numerics, NAs, replicate survey')

#Mean on numerics, with NAs, normal (complex survey), with bys
a_5 = new_complex[, sur_mean(apina, na.rm = TRUE, sv = sv(new_complex), ids = `_id`, st = st(new_complex)), keyby = stype]
b_5 = svyby(~apina, ~stype, og_complex, svymean, na.rm = T)
expect_equal(a_5[, V1], unname(coef(b_5)) ,
             info = 'Mean of numerics, NAs, normal survey, with by')

#Mean on numerics, with NAs, normal (replicate survey)
a_6 = new_replicate[, sur_mean(apina, na.rm = TRUE, sv = sv(new_replicate), ids = `_id`, st = st(new_replicate)), keyby = stype]
b_6 = svyby(~apina, ~stype, og_replicate, svymean, na.rm = T)
expect_equal(a_6[,V1], unname(coef(b_6)) ,
             info = 'Mean of numerics, NAs, replicate survey, with by')

#Mean on factor, normal (complex survey)
a_7.1 = new_complex[, sur_mean(awards, na.rm = TRUE, sv = sv(new_complex), ids = `_id`, st = st(new_complex))]
a_7.2 = new_complex[, sur_mean(awards, na.rm = TRUE, as_list = T, sv = sv(new_complex), ids = `_id`, st = st(new_complex))]
b_7 = svymean(~awards, og_complex, TRUE)
expect_equal(unname(a_7.1), unname(coef(b_7)) ,
             info = 'Mean of factor, NAs, normal survey')
expect_equal(unname(a_7.2[, V1][[1]]), unname(coef(b_7)) ,
             info = 'Mean of factor, NAs, normal survey, with as_list')

#Mean on factor,normal (replicate survey)
a_8.1 = new_replicate[, sur_mean(awards, na.rm = TRUE, sv = sv(new_replicate), ids = `_id`, st = st(new_replicate))]
a_8.2 = new_replicate[, sur_mean(awards, na.rm = TRUE, as_list = T, sv = sv(new_replicate), ids = `_id`, st = st(new_replicate))]
b_8 = svymean(~awards, og_replicate, TRUE)
expect_equal(unname(a_8.1), unname(coef(b_8)) ,
             info = 'Mean of factor, NAs, normal survey')
expect_equal(unname(a_8.2[, V1][[1]]), unname(coef(b_8)) ,
             info = 'Mean of factor, NAs, normal survey, with as_list')


#Mean on factor, normal (complex survey), with by
a_9.1 = new_complex[, sur_mean(awards, na.rm = TRUE, sv = sv(new_complex), ids = `_id`, st = st(new_complex)), keyby = stype]
a_9.2 = new_complex[, sur_mean(awards, na.rm = TRUE, as_list = T, sv = sv(new_complex), ids = `_id`, st = st(new_complex)), keyby = stype]
b_9 = svyby(~awards, ~stype, og_complex, svymean, na.rm = TRUE)

a_9.1r = a_9.1[,V1]
a_9.2r = unname(unlist(a_9.2[,V1]))
b_9r = as.vector(t(as.matrix(b_9[,2:3])))
expect_equal(b_9r, a_9.1r,
             info = 'Mean of factor, NAs, normal survey, with by')
expect_equal(a_9.2r, b_9r ,
             info = 'Mean of factor, NAs, normal survey, with as_list, with by')

#Mean on factor, normal (replicate survey), with by
a_10.1 = new_replicate[, sur_mean(awards, na.rm = TRUE, sv = sv(new_replicate), ids = `_id`, st = st(new_replicate)), keyby = stype]
a_10.2 = new_replicate[, sur_mean(awards, na.rm = TRUE, as_list = T, sv = sv(new_replicate), ids = `_id`, st = st(new_replicate)), keyby = stype]
b_10 = svyby(~awards, ~stype, og_replicate, svymean, na.rm = TRUE)

a_10.1r = a_10.1[,V1]
a_10.2r = unname(unlist(a_10.2[,V1]))
b_10r = as.vector(t(as.matrix(b_10[,2:3])))
expect_equal(b_10r, a_10.1r,
             info = 'Mean of factor, NAs, normal survey, with by')
expect_equal(a_10.2r, b_10r ,
             info = 'Mean of factor, NAs, normal survey, with as_list, with by')

#Mean on factor, normal (complex survey), with by, in a dot
a_11.1 = new_complex[, .(sur_mean(awards, na.rm = TRUE, sv = sv(new_complex), ids = `_id`, st = st(new_complex))), keyby = stype]
a_11.2 = new_complex[, .(sur_mean(awards, na.rm = TRUE, as_list = T, sv = sv(new_complex), ids = `_id`, st = st(new_complex))), keyby = stype]
a_11.2[, V1 := delist_factor(V1)] #Delisting a factor is required because the .() puts a new layer on it. This probably could be avoided in `[` but whatever
b_11 = svyby(~awards, ~stype, og_complex, svymean, na.rm = TRUE)
a_11.1r = a_11.1[,V1]
a_11.2r = unname(unlist(a_11.2[,V1]))
b_11r = as.vector(t(as.matrix(b_11[,2:3])))
expect_equal(b_11r, a_11.1r,
             info = 'Mean of factor, NAs, normal survey, with by')
expect_equal(a_11.2r, b_11r ,
             info = 'Mean of factor, NAs, normal survey, with as_list, with by')

#Uncertainty functions
#variance for complex/normal surveys
#within ./list() and outside
#
a_12.1 = new_complex[, sur_var(api00, na.rm = T, type = 'mean', sv = sv(new_complex), ids = `_id`, st = st(new_complex))]
a_12.2 = new_complex[, .(sur_var(api00, na.rm = T, type = 'mean', sv = sv(new_complex), ids = `_id`, st = st(new_complex)))]
a_12.3 = new_complex[, sur_var(api00, na.rm = T, type = 'mean', as_list = FALSE,  sv = sv(new_complex), ids = `_id`, st = st(new_complex))]
a_12.4 = new_complex[, .(sur_var(api00, na.rm = T, type = 'mean',as_list = FALSE, sv = sv(new_complex), ids = `_id`, st = st(new_complex)))]

expect_true(is.list(a_12.1[,V1]), info = 'sur var, complex design, as_list = TRUE')
expect_true(is.matrix(a_12.1[,V1][[1]]), info = 'sur var, complex design, as_list = TRUE, interior object is a matrix')
expect_true(is.list(a_12.2[,V1]), info = 'sur var, complex design, as_list = TRUE, within a list/.')
expect_true(is.list(a_12.2[,V1][[1]]), info = 'sur var, complex design, as_list = TRUE, interior object is a list')
expect_true(is.matrix(a_12.2[,V1][[1]][[1]]), info = 'sur var, complex design, as_list = TRUE, second interior object is a matrix')
expect_true(is.matrix(a_12.3), info = 'sur var, complex, as_list = FALSE, returns a matrix')
expect_true(!is.matrix(a_12.4[,V1]), info = 'sur var, complex, as_list = FALSE, returns a matrix which since its [1,1] gets turned into a numeric column in the data.table')

#adding by and factor
a_13.1 = new_complex[, sur_var(stype, na.rm = T, type = 'mean', sv = sv(new_complex), ids = `_id`, st = st(new_complex)), by = 'awards']
a_13.2 = new_complex[, .(sur_var(stype, na.rm = T, type = 'mean', sv = sv(new_complex), ids = `_id`, st = st(new_complex))), by = 'awards']
expect_error(new_complex[, .(sur_var(stype, na.rm = T, type = 'mean',as_list = FALSE, sv = sv(new_complex), ids = `_id`, st = st(new_complex))), by = 'awards'])

expect_true(inherits(a_13.1[, V1], 'list'), info = 'as_list with factor and by returns a list of matrices')
expect_true(is.matrix(a_13.1[, V1][[1]]), info = 'as_list with factor and by returns a list of matrices')

expect_true(is.list(a_13.2[, V1]), info = 'as_list with factor within a ./list and by returns a list of list(matrices)')
expect_true(is.list(a_13.2[, V1][[1]]), info = 'as_list with factor within a ./list and by returns a list of list(matrices)')
expect_true(is.matrix(a_13.2[, V1][[1]][[1]]), info = 'ultimate return object is a matrix')

#Warning: Don't run sur_var this way-- It'll work, but its weird since it'll return the matrix straight up
a_13.3 = new_complex[, sur_var(stype, na.rm = T, type = 'mean', as_list = FALSE,  sv = sv(new_complex), ids = `_id`, st = st(new_complex)), by = 'awards']

