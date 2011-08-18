model = function() {
 x = rnorm(1,0,1)
 name = uniform.draw(c('jon','liz'))
}

m = church.model(model, predicate = function() {x>0})
samples = church.samples(m, n.iter=100, thin=1, variable.names=c('x', 'name'), n.chains=4)
plot(samples)
#autocorr.plot(samples)
#crosscorr.plot(samples)