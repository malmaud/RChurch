\name{church.model}
\alias{church.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a Church program object
}
\description{
Creates an object which encapsules a single Church program.\cr
A church program consists of a model and a predicate.
}
\usage{
church.model(model = function() {
}, predicate = function() {
}, context = function() {
})
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
An R function containing the model.
}
  \item{predicate}{
%%     ~~Describe \code{predicate} here~~
An R function containing the preciate. Must return a logical value.
}
  \item{context}{
  An optional R function providing definitions of objects that can be referenced in the model.
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
A Church object. It is an S3 object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Jon Malmaud
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{church.samples}} for drawing samples from the model.
}
\examples{
  model = function() {
    x = rnorm(1,0,1)
    y = rnorm(1,0,1)
    z = x+y
  }
  
  my.program = church.model(model, predicate=function() {z>0})
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
