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
  if(church$engine=="mit-church") {
    church$prefix = c('(import (church church))', '(church')
    church$suffix = c(')', '(exit 0)')
  }
  else {
    church$prefix= ''
    church$suffix = ''
  }
  church
}
  
new.church = function() {
  church = list()
  class(church) <- 'church'
  church
}

church.model <- function(model=function() {}, predicate=function() {},context=function() {}, engine='bher') {
  query = predicate
  church = new.church()
  model_code = R.to.church(model)
  church$engine = engine
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
                            thin=100, n.chains=1, method='mcmc', inputs=list(), debug=F) {
  vars = variable.names
  n.samples= n.iter
  vars.for.church = gsub('\\.', '-', vars)
  list_line = sprintf('(list %s)', paste(vars.for.church,collapse=' '))
  church$obs.vars= list_line
  if(church$engine=="bher")
    church_path = paste('.', system.file('scheme-tools', package='RChurch'), system.file('bher', package='RChurch'), sep=':')
  else if (church$engine=="mit-church")
    #church_path = paste('.', system.file('scheme-tools', package='RChurch'), system.file('mit-church', package='RChurch'), sep=':')
    church_path = system.file('mit-church', package='RChurch')
  engine_path = system.file(church$engine, package='RChurch')
  church$inputs = R.to.church.inits(inputs)
  if(method=='mcmc') {
    church$query.line.prefix = paste('(mh-query', n.samples, thin, sep=' ')
    church$query.line.suffix = ')'
  }
  else if(method=='rejection'){
    church$query.line.prefix = sprintf('(repeat %d (lambda () (rejection-query ', n.iter)
    church$query.line.suffix=')))'
  }
  church$church.program = church.program(church)
  env_str = c()
  env_str[1] = paste("PATH=", engine_path,':$PATH', sep='')
  if(church$engine=="bher")
    env_str[2] = paste("VICARE_LIBRARY_PATH=", church_path,":$VICARE_LIBRARY_PATH", sep='')
  else
    env_str[2] = paste("IKARUS_LIBRARY_PATH=", church_path, ":$IKARUS_LIBRARY_PATH", sep='')
  env_str = paste(env_str, collapse="\n")
  
  if(debug) cat(church$church.program)
  
  res.mcmc = draw_parallel_chain(church, n.chains, env_str, vars)
  church$samples = res.mcmc
  church$level.names = attr(res.mcmc[[1]], 'level_names')
  for(i in length(church$samples)) {
    attr(church$samples[[i]], 'level_names')= NULL
  }
  church
}

find.lists <- function(output) { #Will be used for parsing list outputs, not currently used
  output1 = gsub('\\(', 'c(', output)
  output2 = gsub(' ', ',', output1)
  e = parse(text=output2)[[1]]
  group1 = e[2:length(e)]
  n.iter = length(group1)
  n.vars = length(e[[2]])-1
  res = list()
  for(i in 1:length(group1)) {
    g = group1[[i]]
    group2 = g[2:length(g)]
    res[[i]] = list()
    for(j in 1:length(group2)) {
      h = group2[[j]]
      if(length(h)>1) { 
        h = as.list(h[2:length(h)])
      }
      res[[i]][[j]]=h
    }
  }
  res
}

parse.church.output <- function(raw_output, vars) {
  data_start = which(regexpr('^\\(', raw_output)==1)[[1]]
  data_str = raw_output[data_start:length(raw_output)]
  data_str = paste(data_str, collapse='')
  data_str = gsub('#f', '0', data_str)
  data_str = gsub('#t', '1', data_str)
  parse.res = find.lists(data_str)
  res = list()
  level_names = list() 
  factor_outputs = c()
  for(i in 1:length(vars)) {
    r = parse.res[, i]
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
  if(church$engine=="bher") {
    raw_output = system2('bher', tmp_file, env=env_str, stdout=T)
  }
  else if (church$engine=="mit-church") {
    raw_output = system2('ikarus', tmp_file, env=env_str, stdout=T)
  }
  options(warn=old_warn)
  parse.church.output(raw_output, vars)
}
