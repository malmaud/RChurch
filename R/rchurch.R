# Entry points for user interaction with RChurch



wrap.church <- function(church) {
  if(church$engine=="mit-church") {
    church$prefix = paste('(import (church church))', '(church')
    church$suffix = paste(')', '(exit 0)')
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

update.church <- function(church, ...) {
  if(church$from.file) {
    church.from.file(church, church$file.name)
  }
}

church.from.file <- function(church, filename) {
    church$from.file = T
    church$file.name = filename
    if(!file.exists(filename)) stop(paste('File', filename,'not found'))
    church$file.content = paste(readLines(filename, warn=F), collapse="\n")
    church
}

church.model <- function(model=function() {}, predicate=function() {true},context=function() {}, engine='bher', filename=NA) {
  #if(engine!='bher' && engine!='mit-church') stop('Engine not supported')
  if(!is.element(engine, c('bher', 'mit-church'))) stop('Engine not supported')
  church = new.church()
  church$engine = engine
  if(!is.na(filename)) {

    church = church.from.file(church, filename)
  }
  else {
    query = predicate
    model_code = R.to.church(model)
    church$vars = unique(model_code$vars)
    church$query = R.to.church(query)$code
    church$context = R.to.church(context)$code
    church$model = model_code$code
    church$from.file = F
  }
  #var_str = paste(church$vars, collapse=' ')
  #church$original = paste(R.to.church(context)$code, model_code$code, '\n(list ', var_str, ')\n', R.to.church(query)$code, '\n')
  wrap.church(church)
}

church.program <- function(church) {
  base= if(church$from.file) church$file.content else
    paste(c(church$context, church$query.line.prefix, church$inputs, church$model,
          church$obs.vars, church$query, church$query.line.suffix), collapse="\n")
  paste(church$prefix, base, church$suffix, collapse="\n")
}

  
church.samples <- function(church, variable.names=church$vars,  n.iter=100, 
                            thin=100, n.chains=1, method='mcmc', inputs=list(), 
                            parallel=T, debug=F, do.parse=T) {
  vars = variable.names
  n.samples= n.iter
  vars.for.church = convert.strings(vars)
  list_line = sprintf('(list %s)', paste(vars.for.church,collapse=' '))
  church$obs.vars= list_line
  church$do.parse = do.parse
  if(church$engine=="bher")
    church_path = paste('.', system.file('scheme-tools', package='RChurch'), system.file('bher', package='RChurch'), sep=':')
  else if (church$engine=="mit-church")
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
  church$parallel = parallel
  res = draw_parallel_chain(church, n.chains, env_str, vars, debug)
  church$samples = list()
  church$raw.output = list()
  for(chain in 1:length(res)) {
    church$samples[[chain]] = res[[chain]][[1]]
    church$raw.output[[chain]] = res[[chain]][[3]]
  }
  church$var.types = res[[1]][[2]]
  church
}

draw_parallel_chain <- function(church, n.chains, env_str, vars, debug=F) {
  if(require(doMC)) {
    if(church$parallel) registerDoMC(n.chains) else registerDoSEQ()
  }
  if(church$parallel) {
    res = foreach(i=icount(n.chains)) %dopar% {
      draw_single_chain(church, env_str, vars, debug)
    }
  }
  else { #This form is easier for debugging
    res = list()
    for(i in 1:n.chains) {
      res[[i]] = draw_single_chain(church, env_str, vars, debug)
    }
  }
  #do.call(mcmc.list, res)
}

draw_single_chain <- function(church, env_str, vars, debug=F) {
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
  if(debug) cat(raw_output)
  options(warn=old_warn)
  if(church$do.parse)
    l = parse.church.output(raw_output, vars)
  else
    l = list()
  l[[3]] = raw_output
  l
}
