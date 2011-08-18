model = function() {
  x = rnorm(1, 0, 1)
  y = rnorm(1, 0, 1)
  w = if(x>y) {
    z = x-y
    if(z>1) z else y
  }
  else {
    y
  }
}

m = church.model(model, predicate = function() {y>0})
samples = church.samples(m, n.iter=100, thin=10, variable.names=c('w', 'x','y'))
plot(samples)
autocorr.plot(samples)
crosscorr.plot(samples)