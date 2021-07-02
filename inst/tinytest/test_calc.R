library('tinytest')
library('survey')
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
                  weight2 = runif(size, 1, 2))
fake[, psu := as.numeric(paste0(strata, sample(1:5, size, TRUE)))]
fake[, num_na := num]
fake[sample(seq_len(size), 20), num_na := NA]
setorder(fake, byvar)

fake_sur = dtsurvey(fake)

#TODO fix this
calcdt = dtsurvey:::calc.dtsurvey

t1 = calcdt(fake_sur, 'num', byvar == 1, metrics = rads::metrics(),
         per = 1, win = 2, time_var = 'strata',
         proportion = F, fancy_time = T,
         ci = .95)
t2 = calcdt(fake_sur, 'fact', byvar == 1, metrics = rads::metrics(),
            per = 1, win = NULL, time_var = 'strata',
            proportion = F, fancy_time = T,
            ci = .95)
t3 = calcdt(fake_sur, 'fact', byvar == 1, metrics = rads::metrics(),
            per = 1, win = 1, time_var = 'strata',
            proportion = F, fancy_time = T,
            ci = .95)
t4 = calcdt(fake_sur, 'fact', byvar == 1, metrics = rads::metrics(),
                per = 1, win = 2, time_var = 'strata',
                proportion = F, fancy_time = T,
                ci = .95)
