print.church = function(church, ...) {
    cat('Church code: \n\n')
    cat(church.program(church))
}

load.church.from.file <- function(filename='~/tmp/test.church') {
  church = new.church()
  lines = readLines(filename);
  church$original = lines
  wrap.church(church)
}

wrap.church <- function(church) {
  #church$prefix = c('(import (church church))', '(church')
  #church$suffix = c(')', '(exit 0)')
  church$prefix= ''
  church$suffix = ''
  church
}
  
new.church = function() {
  church = list()
  class(church) <- 'church'
  church
}

church.model <- function(model=function() {}, predicate=function() {},context=function() {}) {
  query = predicate
  church = new.church()
  model_code = R.to.church(model)
  church$vars = unique(model_code$vars)
  church$query = R.to.church(query)$code
  church$context = R.to.church(context)$code
  church$model = model_code$code
  #var_str = paste(church$vars, collapse=' ')
  #church$original = paste(R.to.church(context)$code, model_code$code, '\n(list ', var_str, ')\n', R.to.church(query)$code, '\n')
  wrap.church(church)
}

church.program <- function(church) {
  paste(c(church$prefix, church$context, church$query.line.prefix, church$inputs, church$model,
          church$obs.vars, church$query, church$query.line.suffix, church$suffix), collapse="\n")
}

  
church.samples <- function (church, variable.names=church$vars,  n.iter=100, 
                            thin=100, method='mcmc', inputs=list()) {
  vars = variable.names
  n.samples= n.iter
  vars.for.church = gsub('\\.', '-', vars)
  list_line = sprintf('(list %s)', paste(vars.for.church,collapse=' '))
  church$obs.vars= list_line
  #tmp_file = '/Users/malmaud/tmp/tmp_church.church'    
  #church_path = '.:/Users/malmaud/tmp/scheme-tools:/Users/malmaud/tmp/bher'
  #bher_path = '/Users/malmaud/tmp/bher'
  tmp_file = file.path(tempdir(), 'tmp_church.church')
  church_path = paste('.', system.file('scheme-tools', package='RChurch'), system.file('bher', package='RChurch'), sep=':')
  bher_path = system.file('bher', package='RChurch')
  church$inputs = R.to.church.inits(inputs)
  if(method=='mcmc') {
    church$query.line.prefix = paste('(mh-query', n.samples, thin, sep=' ')
    church$query.line.suffix = ')'
  }
  else if(method=='rejection'){
    church$query.line.prefix = sprintf('(repeat %d (lambda () (rejection-query ', n.iter)
    church$query.line.suffix=')))'
  }
  #file.remove(tmp_file)
  writeLines(church.program(church), tmp_file)
  cat(church.program(church))
  old_warn = getOption('warn')
  options(warn=-1)
  env_str = c()
  env_str[1] = paste("PATH=", bher_path,':$PATH', sep='')
  env_str[2] = paste("VICARE_LIBRARY_PATH=", church_path,":$VICARE_LIBRARY_PATH", sep='')
  raw_output = system2('bher', tmp_file, env=paste(env_str,collapse="\n"), stdout=T)
  cat(raw_output)
  options(warn=old_warn)
  data_start = which(regexpr('^\\(', raw_output)==1)[[1]]
  data_str = raw_output[data_start:length(raw_output)]
  data_str = paste(data_str, collapse='')
  data_str = gsub('\\(', '', data_str)
  data_str = gsub('\\)', '', data_str)
  data_str = gsub('#f', '0', data_str)
  data_str = gsub('#t', '1', data_str)
  data = strsplit(data_str,' ')[[1]]
  data = as.numeric(data)
  n_data = length(data)
  res = list()
  for(i in 1:length(vars)) {
    res[[i]] = data[seq(i, n_data, length(vars))]
#     if(var_types[i]=='logical') {
#       res[[i]] = as.logical(res[[i]])
#     }
  }
  names(res) = vars
  res.mcmc = mcmc(do.call(cbind, res))
  
  plot(res.mcmc)
  res.mcmc
}
