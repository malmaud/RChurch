model = function() {
  colors = c('blue', 'green', 'red')
  phi = rdirichlet(1, c(1,1,1))
  alpha = .1
  prototype = sapply(phi, function(w) alpha*w) # Will later be alpha*phi directly
  bag.prototype = mem(function(bag) rdirichlet(1, prototype))
  obs.bag = mem(function(obs.name) sample(c('bag1', 'bag2', 'bag3'), 1))
  draw.marble = mem(function(obs.name) multinomial(colors, bag.prototype(obs.bag(obs.name))))
  same1 = obs.bag('obs1')==obs.bag('obs2')
  same2 = obs.bag('obs1')==obs.bag('obs3')
}

predicate = function() {
    draw.marble('obs1')=='red' &&  
    draw.marble('obs2')=='red' && 
    draw.marble('obs3')=='blue' && 
    draw.marble('obs4')=='blue' && 
    draw.marble('obs5')=='red' && 
    draw.marble('obs6')=='blue'
}

m = church.model(model, predicate, engine='mit-church')
m = church.samples(m, n.iter=200, thin=100, variable.names=c('same1', 'same2'), n.chains=2)