model = function() {
  strength = mem(function(person) if(flip()) 10 else 5)
  lazy = function(person) flip(1/3)
  total.pulling = function(team) {
    realized.strength = function(person) if(lazy(person)) strength(person)/2 else strength(person)
    sum(sapply(team, realized.strength))
  }
  winner = function(team1, team2) if(total.pulling(team1) < total.pulling(team2)) 'team2' else 'team1'
  team1.wins = 'team1' == winner(c('bob', 'mary'), c('jim', 'sue'))
}

predicate = function() strength('mary')>=strength('sue') && 'team1' == winner(c('bob', 'francis'), c('tom', 'jim'))

tug.of.war.model = church.model(model, predicate)
samples = church.samples(tug.of.war.model, variable.names=c('team1.wins'), n.iter=100, thin=10, method='mcmc')
hist(samples, main="Do bob and mary win against jim and sue")