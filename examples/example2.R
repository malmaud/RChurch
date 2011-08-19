model = function() {
 nums = c(5,11)
 num = sample(nums,1)
}

m = church.model(model, predicate = function() {true}, engine='mit-church')
m_sampled = church.samples(m, n.iter=100, thin=1, variable.names=c('num'), n.chains=1, debug=T)
samples = m_sampled$samples;
plot(samples)
#autocorr.plot(samples)
#crosscorr.plot(samples)