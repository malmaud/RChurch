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
An R function containing the predicate. The predicate is what is the model is being conditioned on - only samples satisfying the predicate are generated. Must return a logical value. Defaults to always returning true, so that you are effectively sampling from the prior.
}
  \item{context}{
  An optional R function providing definitions of objects that can be referenced in the model. This function is convereted to Scheme and inserted before the query line. Useful if you have many models that share some function definitions and data.
}

  \item{engine}{
  Which Church engine to use. Allowed values are "bher" and "mit-church". Defaults to "bher". 
  }
  \item{filename}{
  If you already have a model written in Scheme and  want to bypass the R translator, set this parameter to the filename where the model is stored. You should not set both this and the \code{model} parameter.
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
A Church object. A Church object is a list containing various information about the model. Run \code{ls} on this object to see all its components.
}

\author{
%%  ~~who you are~~
Jon Malmaud
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{church.samples}} for drawing samples from the model. 
}
\examples{
  model = function() {
    x <- rnorm(1,0,1)
    y <- rnorm(1,0,1)
    z <- x+y
  }
  
  my.model = church.model(model, predicate=function() {z>0})
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
