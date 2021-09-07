library('tinytest')
library('srvyr')
library('data.table')
library('dtsurvey')
library('rads')
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
                  weight2 = runif(size, 1, 2),
                  ttt = sample(1:3, size, T))
fake[, psu := as.numeric(paste0(strata, sample(1:5, size, TRUE)))]
fake[, num_na := num]
fake[sample(seq_len(size), 20), num_na := NA]
setorder(fake, byvar)

fake_sur = dtsurvey(fake, weight = 'weight')
fake_sur2 = srvyr::as_survey(fake, weights = weight)
fake_ad = dtadmin(fake)
#TODO fix this
calcdt = dtsurvey:::calc.dtsurvey

compare_ab = function(a, b, info_pre = ""){
  #make sure the names all match
  nma = sort(names(a))
  nmb = sort(names(b))
  nm = tinytest::expect_true(all(nma==nmb), info = paste(info_pre,'A and B names are all aligned'))


  r = lapply(names(a), function(nnn){
    setorderv(a, nnn)
    setorderv(b, nnn)
    expect_equal(a[[nnn]], b[[nnn]], info = paste(info_pre, 'a v b:', nnn))
  })

  r = append(r, nm)

  r
}


#compare dtsurvey, calc.survey and calc.data.table with no windowing
a1 = calcdt(fake_sur, 'num',  metrics = rads::metrics(),
         per = 1, win = 1, time_var = 'ttt',
         proportion = F, fancy_time = T,
         ci = .95)
b1 = rads::calc(fake_sur2, 'num', metrics = rads::metrics(),
                per = 1, win = 1, time_var = 'ttt',
                proportion = F, fancy_time = T,
                ci = .95)
c1 = rads::calc(fake, 'num', metrics = rads::metrics(),
                per = 1, win = 1, time_var = 'ttt',
                proportion = F, fancy_time = T,
                ci = .95)
d1 = calcdt(fake_ad, 'num',  metrics = rads::metrics(),
                 per = 1, win = 1, time_var = 'ttt',
                 proportion = F, fancy_time = T,
                 ci = .95)


r1 = compare_ab(a1,b1, info_pre ='Basic dtsurvey vs. calc')
table(unlist(r1))


a2 = calcdt(fake_sur, 'num', byvar == 1, metrics = rads::metrics(),
            per = 1, win = 2, time_var = 'strata',
            proportion = F, fancy_time = T,
            ci = .95)
a3 = calcdt(fake_sur, 'fact', byvar == 1, metrics = rads::metrics(),
            per = 1, win = NULL, time_var = 'strata',
            proportion = F, fancy_time = T,
            ci = .95)
a4 = calcdt(fake_sur, 'fact', byvar == 1, metrics = rads::metrics(),
            per = 1, win = 1, time_var = 'strata',
            proportion = F, fancy_time = T,
            ci = .95)
a5 = calcdt(fake_sur, 'fact', byvar == 1, metrics = rads::metrics(),
                per = 1, win = 2, time_var = 'strata',
                proportion = F, fancy_time = T,
                ci = .95)
