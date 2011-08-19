# Translates Church output into R structures
parse.church.output <- function(raw_output, vars) {
  data_start = which(regexpr('^\\(', raw_output)==1)[[1]]
  data_str = raw_output[data_start:length(raw_output)]
  data_str = paste(data_str, collapse='')
  data_str = gsub('#f', '0', data_str)
  data_str = gsub('#t', '1', data_str)
  parse.res = find.lists(data_str)
  res = list()
  for(i in 1:length(vars)) {
    res[[i]] = list()
    for(j in 1:length(parse.res)) {
      res[[i]][[j]] = parse.res[[j]][[i]]
    }
  }
  names(res) = vars
  res
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