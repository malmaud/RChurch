model = function() {
 x = rnorm(1,0,1)
}

m = church.model(model, predicate = function() {x>0})
samples = church.samples(m, n.iter=1000, thin=10, variable.names=c('x'), n.chains=4)
plot(samples)
#autocorr.plot(samples)
#crosscorr.plot(samples)