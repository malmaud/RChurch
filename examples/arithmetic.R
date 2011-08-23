context = function() {
  random.arithmetic.expression = function() {
    if(flip(.6)) {
      if(flip()) 'x' else sample.integer(10)
    }
    else {
      c(uniform.draw(c('+','-')), random.arithmetic.expression(), random.arithmetic.expression())
    }
  }
  procedure.from.expression = function(expr) {
    eval(c('lambda', c('x'), expr), get.current.environment())
  }
}

model = function() {
  my.expr = random.arithmetic.expression()
  my.proc = procedure.from.expression(my.expr)
}

m = church.model(model, function() (my.proc(1)==3), context, engine='mit-church')
m = church.samples(m, variable.names=c('my.expr'), n.iter=100, thin=100, debug=F, do.parse=F)