# Occam's Razor
model = function() {
  hypothesis.set = function(hyp) {
    if(hyp==1) c('a', 'b', 'c', 'd', 'e', 'f') else  c('a', 'b', 'c')#1==Big hypothesis, 0==Small hypothesis   
  }
  hypothesis = if(flip()) {1} else {0}
  observe = function(N) {
    replicate(N, function() {uniform.draw(hypothesis.set(hypothesis))})
  }
}

learning.model = church.model(model, predicate = function() {observe(length(data))==data})
obs_data = list(c('a'),  c('a','b'), c('a','b','a'), c('a', 'b', 'a', 'b', 'c'))
N = sapply(obs_data, length)
samples = list()

for(i in 1:length(data)) {
  samples[[i]] = church.samples(learning.model, variable.names=c('hypothesis'), n.iter=100, thin=10, 
                     method='mcmc',inputs=list(data=obs_data[[i]]))
}
means = sapply(samples, mean)

p = qplot(N, means, geom=c('point', 'line'), col=I('blue'), xlim=c(1, max(N)),
          xlab=expression(N[obs]), ylab=expression(P(H[big])), main="Occam's Razor learning curve") +
            geom_smooth(col=I('red')) +theme_bw()
print(p)
