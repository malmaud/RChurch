church.diag <- function(church) {
  if(!require('coda')) stop('Requires the coda package')
  as.mcmc.list(church)
}


#Generic functions implemented by the church object

as.mcmc.list.church = function(x, ...) { # Returns an MCMC list for convergence assessment. Currently strips out returned lists (keeps only scalars)
  church = x
  var.types = church$var.types
  n.chains = length(church$samples)
  n.samples = length(church$samples[[1]][[1]])
  into.list = which(sapply(var.types, function(a) !is.list(a)))
  mclist = list()
 
  for(chain in 1:n.chains) {
    mclist[[chain]] = matrix(nrow=n.samples, ncol=length(into.list))
    for(var in 1:length(into.list)) {
      sample = church$samples[[chain]][[into.list[[var]]]]
      if(var.types[[into.list[var]]]=="character") {
        sample = factor(sample)
      }
      mclist[[chain]][, var] = as.numeric(sample)
    }
  }
  mc.obs = lapply(mclist, mcmc)
  mclist = do.call(mcmc.list, mc.obs)
}

print.church = function(church, ...) {
    cat('Church code: \n\n')
    cat(church.program(church))
}

# Fill in these generic functions at some point
summary.church = function(church, ...) {
  
}

plot.church = function(church, ...) {
  
}