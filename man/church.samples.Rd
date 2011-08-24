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
The names of the variables being observed in the Church program.
}
  \item{n.iter}{
%%     ~~Describe \code{n.iter} here~~
The number of iterations to run the sampler for.
}
  \item{thin}{
%%     ~~Describe \code{thin} here~~
The thinning interval. An integer giving the frequency of samples to keep. Defaults to 1 (meaning no samples are dropped).
}
  \item{n.chains}{
  The number of chains to run.  Defaults to 1.
  }
  
  \item{method}{
%%     ~~Describe \code{method} here~~
The sampler method. Either "mcmc" for the Metropolis Hastings sampler or "rejection" for rejection sampling. Defaults to "mcmc".
}
  \item{inputs}{
%%     ~~Describe \code{inputs} here~~
A list of inptus to the Church model.
}
\item{parallel} {
Logical variable for whether to run the chains in parallel or run them serially. Running them in parallel requires the \code{doMC} and \code{parfor} package be intalled.
}
\item{debug} {
Logical variable for whether to print various debugging information.
}
\item {do.parse}{
Logical variable for whether to parse the results from the Scheme interpreter into R structures or just record the returned string.
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
Returns a new Church object containing the drawn samples.
\item{samples}{A list of the returned samples. Each component corresponds to one chain. }
}

\author{
%%  ~~who you are~~
Jon Malmaud
}
\note{
%%  ~~further notes~~
Note that the total number of iterations the sampler will be run for is n.iter * thin, as n.iter is the number of returned iterations.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{church.model}}
}
\examples{
#Assume my.model already exists
samples = church.samples(my.model, variable.names=c('x','y'), n.iter=100, thin=10, method='mcmc', inputs=list(name='jon', age=24))
plot(samples)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
