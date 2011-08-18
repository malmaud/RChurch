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
                            thin=100, n.chains=1, method='mcmc', inputs=list()) {
  vars = variable.names
  n.samples= n.iter
  vars.for.church = gsub('\\.', '-', vars)
  list_line = sprintf('(list %s)', paste(vars.for.church,collapse=' '))
  church$obs.vars= list_line
  #tmp_file = '/Users/malmaud/tmp/tmp_church.church'    
  #church_path = '.:/Users/malmaud/tmp/scheme-tools:/Users/malmaud/tmp/bher'
  #bher_path = '/Users/malmaud/tmp/bher'
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
  church$church.program = church.program(church)
  env_str = c()
  env_str[1] = paste("PATH=", bher_path,':$PATH', sep='')
  env_str[2] = paste("VICARE_LIBRARY_PATH=", church_path,":$VICARE_LIBRARY_PATH", sep='')
  env_str = paste(env_str, collapse="\n")
  res.mcmc = draw_parallel_chain(church, n.chains, env_str, vars)
  #plot(res.mcmc)
  church$samples = res.mcmc
  church$level.names = attr(res.mcmc[[1]], 'level_names')
  for(i in length(church$samples)) {
    attr(church$samples[[i]], 'level_names')= NULL
  }
  church
}

find.lists <- function(output) {
  output = strsplit(output, '')[[1]]
  pos = 2
  pars = 0
  repeat {
    char = output[pos]
    if(char=="(") {
      pars = pars+1
    }
    if(char==")") {
      pars = pars-1
      if(pars==0) {
        break
      }
    }
    pos = pos + 1
  }
  block = output[3:(pos-1)]
}

parse.church.output <- function(raw_output, vars) {
  data_start = which(regexpr('^\\(', raw_output)==1)[[1]]
  data_str = raw_output[data_start:length(raw_output)]
  data_str = paste(data_str, collapse='')
  data_str = gsub('\\(', '', data_str)
  data_str = gsub('\\)', '', data_str)
  data_str = gsub('#f', '0', data_str)
  data_str = gsub('#t', '1', data_str)
  data = strsplit(data_str,' ')[[1]]
  n_data = length(data)
  res = list()
  level_names = list() 
  factor_outputs = c()
  for(i in 1:length(vars)) {
    r = data[seq(i, n_data, length(vars))]
    if(!is.na(as.numeric(r[1]))) { #r is numeric
      r = as.numeric(r);
    }
    else {
      r = factor(r)   
      factor_outputs = c(factor_outputs, i)
      level_names[[length(factor_outputs)]] = levels(r)
    }
    res[[i]] = r
  }
  names(level_names) = vars[factor_outputs]
  names(res) = vars
  res.mcmc = mcmc(do.call(cbind, res))  
  attr(res.mcmc, 'level_names') = level_names
  res.mcmc
}

draw_parallel_chain <- function(church, n.chains, env_str, vars) {
  if(require(doMC)) {
    registerDoMC(n.chains)
  }
  res = foreach(i=icount(n.chains)) %dopar% {
    draw_single_chain(church, env_str, vars)
  }
  do.call(mcmc.list, res)
}

draw_single_chain <- function(church, env_str, vars) {
  tmp_file = tempfile('tmp_church', fileext='church')
  writeLines(church$church.program, tmp_file)
  old_warn = getOption('warn')
  options(warn=-1)
  raw_output = system2('bher', tmp_file, env=env_str, stdout=T)
  options(warn=old_warn)
  parse.church.output(raw_output, vars)
}
