\name{church.samples}
\alias{church.samples}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw samples from a Church program
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
church.samples(church, variable.names = church$vars, n.iter = 100, thin = 100, method = "mcmc", inputs = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{church}{
%%     ~~Describe \code{church} here~~
The Church program, created by \code{\link{church.model}}
}
  \item{variable.names}{
%%     ~~Describe \code{variable.names} here~~
A list of variable names defined in the model whose sampled values will be returned
}
  \item{n.iter}{
%%     ~~Describe \code{n.iter} here~~
The number of iterations to run the sampler for.
}
  \item{thin}{
%%     ~~Describe \code{thin} here~~
The thinning interval. An integer giving the frequency of samples to keep.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
The sampler method. Either "mcmc" for the Metropolis Hastings sampler or "rejection" for rejection sampling.
}
  \item{inputs}{
%%     ~~Describe \code{inputs} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
Note that the total number of iterations the sampler will be run for is n.iter * thin.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (church, variable.names=church$vars,  n.iter=100, 
                            thin=100, method='mcmc', inputs=list()) {
  vars = variable.names
  n.samples= n.iter
  vars.for.church = gsub('\\.', vars, '-')
  list_line = sprintf('(list \%s)', paste(vars.for.church,collapse=' '))
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
    church$query.line.prefix = sprintf('(repeat \%d (lambda () (rejection-query ', n.iter)
    church$query.line.suffix=')))'
  }
  #file.remove(tmp_file)
  writeLines(church.program(church), tmp_file)
  old_warn = getOption('warn')
  options(warn=-1)
  env_str = c()
  env_str[1] = paste("PATH=", bher_path,':$PATH', sep='')
  env_str[2] = paste("VICARE_LIBRARY_PATH=", church_path,":$VICARE_LIBRARY_PATH", sep='')
  raw_output = system2('bher', tmp_file, env=paste(env_str,collapse="\n"), stdout=T)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

