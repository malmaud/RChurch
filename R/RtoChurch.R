R.to.church <- function(R_exprs) {
  if(length(R_exprs)==0) {
    return(list(code='',vars=c()))
  }
  if(class(R_exprs)=="character") {
    R_exprs = parse(text=R_exprs)
  }
  else if(class(R_exprs)=="function") {
    R_exprs = body(R_exprs)[2:length(body(R_exprs))]
    if(is.null(R_exprs[[1]])) {
      return(list(code='',vars=c()))
    }     
  }
  lines = c()
  vars = c()
  for(i in 1:length(R_exprs)) {
    p = R.to.church.rec(R_exprs[[i]])
    lines[i] = p$code
    vars = c(vars, p$vars)
  }
  list(code=paste(lines, collapse="\n"),vars=vars)
}

R.to.church.lambda.wrap <- function(R_exprs) {
  if(length(R_exprs)==0) {
    return('')
  }
    lines = c()

  if(class(R_exprs)!="{") {
    R_exprs = list(R_exprs)
  }
  else {
    if(length(R_exprs) > 2) {
      for(i in 2:(length(R_exprs)-1)) {
        e = R_exprs[[i]];
        left = R.to.church.rec(e[[2]])
        right = R.to.church.rec(e[[3]])
        lines[i-1] = sprintf('(%s %s)', left$code, right$code)
      }
    }
  }
  val = R.to.church.rec(R_exprs[[length(R_exprs)]])
  let_statement = sprintf('(let* (%s) %s)', paste(lines, collapse=' '), val$code)
}

R.if.to.church <- function(R_expr) {
  p1 = R.to.church.rec(R_expr[[2]])$code
  p2 = R.to.church.lambda.wrap(R_expr[[3]])
  p3 = R.to.church.lambda.wrap(R_expr[[4]])
  sprintf('(if %s %s %s)', p1, p2, p3)
}
  
R.func.to.church <- function(R_expr) { #Translates an R function into a lambda expression
  if(is.null(R_expr[[2]])) {
    formal_arg_names = c()
  }
  else {
    formal_args = as.list(R_expr[[2]])
    formal_arg_names = ls(formal_args)
  }
#   body = R_expr[[3]][2:length(R_expr[[3]])]
#   parsed_body = c()
#   for(i in 1:length(body)) {
#     parsed_body[i] = R.to.church.rec(body[[i]])
#   }
  parsed_body = R.to.church.lambda.wrap(R_expr[[3]])
  sprintf('(lambda (%s) %s)', paste(formal_arg_names, collapse=' '), parsed_body)
}
  
R.list.to.church <- function(R_expr) {
  sprintf("(list %s)", paste(R_expr[2:length(R_expr)], collapse=' '))
}

R.vectorized.to.church <- function(base.fun, part) {
  n = part[2]
  if(n=="1") {
    res= sprintf("(%s %s %s)", base.fun, part[3], part[4])
  }
  else {
    res = sprintf("(repeat %s (lambda () (%s %s %s)))", part[2], base.fun, part[3], part[4])
  }  
  res
}

R.to.church.idioms <- function(part, R_expr) { #We translate some common R idioms into Church notation, for convenience
  res = list(code='', vars=c())
  n_parts = length(part)
  if(part[1]=="<-" || part[1]=="=") { 
    res$code = sprintf("(define %s %s)", part[2], part[3])
    res$vars = c(res$vars, part[2])
  }
  else if(part[1]=='rnorm') {
    res$code = R.vectorized.to.church('gaussian', part)
  }
  else if(part[1]=="runif") { #check if this is right
    res$code = R.vectorized.to.church('uniform-draw', part)
  }
  else if(part[1]=='rgamma') { #rate has to be converted to scale
    res$code = sprintf("(gamma %s %s)", part[3], as.character(1/as.numeric(part[4])))
  }
  else if(part[1]=='rdirichlet') {
    res$code = sprintf("(rdirichlet %s %s)", part[3], part[4])
  }
  else if(part[1]=='uniform.draw') {
    res$code = sprintf("(uniform-draw %s)", part[2])
  }
  else if(part[1]=="==") {
    res$code = sprintf("(equal? %s %s)", part[2], part[3])
  }
  else if(part[1]=="sapply") { #sapply and map take their arguments in the opposite order
    res$code = sprintf("(map %s %s)", part[n_parts], paste(part[2:(n_parts-1)],collapse=' '))
  }
  else if(part[1]=="replicate") {
    res$code = sprintf("(repeat %s %s)", part[2], part[3])
  }
  else if(part[1]=="&&") {
    res$code = sprintf("(and %s %s)", part[2], part[3])
  }
  else if(part[1]=="ifelse") {
    res$code = sprintf("(if %s %s %s)", part[2], part[3], part[4])
  }
  else if(part[1]=="if") {
    res$code = R.if.to.church(R_expr)
  }
  else if(part[1]=="function") {
    res$code = R.func.to.church(R_expr)
  }
  else if(part[1]=="[") {
    res$code = R.to.church.index(R_expr)
  }
  else if(part[1]=="c") {
    res$code = R.list.to.church(part)
  }
  else {
    res$code = sprintf("(%s %s)", part[1], paste(part[2:length(part)], collapse=' '))
  }
  res
}

R.to.church.inits <- function(inputs) {
  if(length(inputs)==0) {
    return('')
  }
  input_names = ls(inputs)
  input_str = c()
  for(i in 1:length(input_names)) {
    if(length(inputs[[i]])==1) { #Because R doesn't distinguish between scalars and list of length 1, assume everything should be a list
      input_str[i] = R.to.church(sprintf('%s=c(%s)', input_names[i], deparse(inputs[[i]])))$code
    }
    else {
      input_str[i] = R.to.church(sprintf('%s=%s', input_names[i], deparse(inputs[[i]])))$code
    }
  }
  paste(input_str, collapse="\n")
}

R.to.church.index <- function(R_expr) { # An expression like x[3]
  varname = R.to.church.rec(R_expr[[2]])$code
  indexval = R.to.church.rec(R_expr[[3]])$code
  sprintf("(list-ref %s %s)", varname, indexval)
}
  
R.to.church.rec <- function(R_expr, vars=c()) { #Recursive function for parsing R code into Church code
  as.c = as.character;
  if(length(R_expr)==0) {
    return(list(code='',vars=vars))
  }
  if(length(R_expr)==1 ) { #A leaf in the R parse tree
    base_str = as.c(R_expr)
    base_str = sub('^TRUE$', 'true', base_str)
    base_str = sub('^T$', 'true', base_str)
    base_str = sub('^F$', 'false',  base_str)
    base_str = sub('^FALSE$', 'false', base_str)
    if(class(R_expr)!="character" && class(R_expr)!="numeric") {
      base_str = gsub('\\.', '-', base_str)
    }
    if(class(R_expr)=="call") { #A function call with no arguments need special treatment
      base_str = paste('(',base_str,')',sep='')
    }
    else if(class(R_expr)=="character") { #String literals are converted to Scheme symbols
      base_str = paste("'", base_str, "", sep='')
    }
    return(list(code=base_str, vars=c()))
  }
  part = c();
  for(i in 1:length(R_expr)) {
    p = R.to.church.rec(R_expr[[i]])
    part[i] = p$code
    vars = c(vars, p$vars)
  }
  res = R.to.church.idioms(part, R_expr);
  res$vars = c(res$vars, vars)
  res
}