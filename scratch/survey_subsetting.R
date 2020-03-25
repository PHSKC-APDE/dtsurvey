library('survey')
data(api)
a <- svydesign(id=~dnum, weights=~pw, data=apiclus1)
b = dclus1[apiclus1$stype != 'E']


#pweights<-1/design$prob
#psum<-sum(pweights)
# v<-svyrecvar(x*pweights/psum,design$cluster,design$strata, design$fpc,
#              postStrata=design$postStrata)

length(a$prob)
length(b$prob)

dim(a$cluster)
dim(b$cluster)

dim(a$strata)
dim(b$strata)

a$fpc
b$fpc
