model = function() {
  #x.vals = c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
  #obs.y.vals = c(-0.199, -0.208, -0.673, -0.431, -0.360, -0.478, -0.984, 0.516, 1.138, 2.439, 3.501)
  x.vals = c(1,2)
  obs.y.vals = c(3.1, 6.4)
  make.poly = function(c) {
    function(x) {
      term = function(coeff, order) coeff * expt(x, order)
      sum(sapply(c, iota(length(c)), term))
    }
  }
  noisy.equals = function(x, y) log.flip(-1000*expt(x-y,2))
  poly.order = sample.integer(4)
  coefficients = replicate(poly.order+1, function() rnorm(1,0,2))
  y.vals = sapply(x.vals, make.poly(coefficients))
}

predicate = function() all(sapply(y.vals, obs.y.vals, noisy.equals))

curve.fitter = church.model(model, predicate)
samples = church.samples(curve.fitter, variable.names=c('poly.order'), n.iter=10, thin=1)
mean_coeffs = mean(samples)
