# Translates Church output into R structures
parse.church.output <- function(raw_output, vars) {
  data_start = which(regexpr('^\\(', raw_output)==1)[[1]]
  data_str = raw_output[data_start:length(raw_output)]
  data_str = paste(data_str, collapse='')
  data_str = gsub('#f', '0', data_str)
  data_str = gsub('#t', '1', data_str)
  parse.res = find.lists(data_str)
  var.types = parse.res[[2]]
  parse.res = parse.res[[1]]
  res = list()
  for(i in 1:length(vars)) {
    res[[i]] = list()
    for(j in 1:length(parse.res)) {
      res[[i]][[j]] = parse.res[[j]][[i]]
    }
    if(var.types[i]!='list') {
      res[[i]] = unlist(res[[i]])
    }
  }
  names(res) = vars
  
  list(res, var.types)
}

find.lists <- function(output) { 
  output1 = gsub('\\(', 'c(', output)
  output2 = gsub(' ', ',', output1)
  e = parse(text=output2)[[1]]
  group1 = e[2:length(e)]
  n.iter = length(group1)
  n.vars = length(e[[2]])-1
  res = list()
  var.types = list()
  for(i in 1:length(group1)) { #For each iteration
    g = group1[[i]]
    group2 = g[2:length(g)]
    res[[i]] = list()
    for(j in 1:length(group2)) { #For each returned variable in that iteration
        find.lists.rec = function(h) {
          if(class(h)=='name') {
            h = as.character(h)
            return(list(h, 'character'))
          }
          else if(class(h)=='call'){
            if(h[[1]]=='-') {
              h = -as.numeric(h[[2]])
              return(list(h,'numeric'))
            }
            else if(h[[1]]=='c') {
              #h = as.list(h[2:length(h)])
              h.out = list()
              h.subset = h[2:length(h)]
              h.subset = as.list(h.subset)
              h.subset = h.subset[h.subset!="."]
              var.types = list()
              for(term in 1:length(h.subset)) {
                output = find.lists.rec(h.subset[[term]])
                h.out[[term]] = output[[1]]
                var.types[[term]] = output[[2]]
                if(is.list(h.out[[term]]) && length(h.out[[term]]==1)) {
                  h.out[[term]] = h.out[[term]][[1]]
                  var.types[[term]] = var.types[[term]][[1]]
                }
              }
              return(list(h.out, var.types))
            }
          }
          else if(class(h)=='numeric') {
            h = as.numeric(h)
            return(list(h, 'numeric'))
          }    
      }
      h = group2[[j]]
      h.out = find.lists.rec(h)
      var.types[[j]] = h.out[[2]]
      res[[i]][[j]]=h.out[[1]]
    }
  }
  list(res, var.types)
}