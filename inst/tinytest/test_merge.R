library('tinytest')
library('survey')
library('data.table')
library('dtsurvey')

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
fake[, id := .I]
fake_sur = dtsurvey(fake, psu = 'psu', strata = 'strata', weight = 'weight', nest = T)

rhs = fake[, .(id, newvar = runif(nrow(fake)))]

new = merge(fake_sur, rhs, all.x = T, by = 'id')

expect_true(inherits(new, 'dtsurvey'))
expect_true(all(names(attributes(fake_sur)) %in% names(attributes(new))))
expect_true(nrow(new) == nrow(fake_sur))
expect_true(all.equal(attr(fake_sur, 'sdes'), attr(new, 'sdes')))

# try without the all.x
new2 = merge(fake_sur, rhs[40:50,], by = 'id')
expect_true(all(names(attributes(fake_sur)) %in% names(attributes(new2))))
expect_true(all.equal(attr(fake_sur, 'sdes'), attr(new2, 'sdes')))
expect_true(nrow(new2) == length(40:50))
