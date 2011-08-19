model = function() {
 tosses = function(K) {
   if(K==0) list() else pair(flip(.5), tosses(K-1))
 }
 t = tosses(10)

}

m = church.model(model, predicate = function() {true}, engine='mit-church')
m_sampled = church.samples(m, n.iter=10, thin=1, variable.names=c('t'), n.chains=1, debug=T, parallel=F)
samples = m_sampled$samples[[1]]$t;

#autocorr.plot(samples)
#crosscorr.plot(samples)