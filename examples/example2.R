model = function() {
  x = rnorm(1,0,1)
  y = rnorm(1,5,1)
}

m = church.model(model, engine='mit-church')
m.sampled = church.samples(m, n.iter=10, thin=1, variable.names=c('x','y'), n.chains=1, debug=T, parallel=F)

#autocorr.plot(samples)
#crosscorr.plot(samples)