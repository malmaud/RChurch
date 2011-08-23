model = function() {
  x = rnorm(1,0,1)
  y = rnorm(1,5,1)
}

m = church.model(model, engine='mit-church')
m.sampled = church.samples(m, n.iter=100, thin=1, variable.names=c('x','y'), n.chains=4, debug=T, parallel=T, do.parse=T)
s = as.mcmc.list(m.sampled)
#autocorr.plot(samples)
#crosscorr.plot(samples)