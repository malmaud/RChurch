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
A list of inptus to the Church model.
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
Returns a list of samples of monitored variables. The list is of type 'mcmc', which allows the Coda package to run MCMC diagnostics.
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
#Assume my.program already exists
samples = church.samples(my.program, variable.names=c('x','y'), n.iter=100, thin=10, method='mcmc', inputs=list(name='jon', age=24))
plot(samples)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

