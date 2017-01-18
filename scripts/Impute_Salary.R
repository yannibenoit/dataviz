library(readr)
R <- read.csv(file="~/Documents/Toucan Toco/HR.csv", stringsAsFactors =FALSE, sep=";", header=TRUE)

#Génération d'une loi normale pour générer le salaire. Salaire annuel moyen aux Etats Unis = 50 120$.
salary_distrib <-trunc(rnorm(n = 100000,mean=50120,sd=10000))
hist(salary_distrib,freq = FALSE)
lines(density(salary_distrib),type = "l",col="red")

#Tirage aléatoire pour chaque individu en fonction de sa tranche (low, medium & high)
#low = <40k
#medium = >=40k & <=70k
#high = >70k

compute_income <- function(income_level){
  if(income_level="low"){
    
  }
  else if(income_level="medium"){
    
  }
  else{
    
  }
}