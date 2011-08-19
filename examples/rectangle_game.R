model = function() {
  obs.data = list(c(0.4, 0.7), c(0.5, 0.4), c(0.46, 0.63), c(0.43, 0.51))
  noise = .001
  num.examples = length(obs.data)
  
  x1 = runif(1,0,1)
  x2 = runif(1,0,1)
  y1 = runif(1,0,1)
  y2 = runif(1,0,1)
  
  concept = function() list(runif(1,x1,x2), runif(1,y1,y2))
  sim.data = replicate(num.examples, concept)
}

m = church.model(model, function() sim.data==obs.data, engine='mit-church')
m = church.samples(m, variable.names=c('x1','x2','y1','y2'), n.iter=2, thin=1)