library(readr)
RH <- read_excel("~/Desktop/HR_with_income.xlsx")
table(RH$salary)


###########
#  DEPTS  #
###########
depts <- c("Val-d'Oise","Seine-Saint-Denis","Essonne","Seine-et-Marne","Paris","Hauts-de-Seine","Yvelines","Val-de-Marne")
probs <- c(0.05,0.18,0.02,0.03,0.3,0.25,0.05,0.12)
imputed_depts<- sample(depts,size = nrow(RH), replace = TRUE,prob = (probs))

#Concatenation avec le dataset
RH$Employee_Location = imputed_depts

#########
#  AGE  #
#########
bus = sort(unique(RH$Department))

impute_age <- function(BU){
  age <- 0
  switch(BU, 
         "hr"={
           age <- trunc(rnorm(n=1,mean = 1988,sd = 3))
         },
         "IT"={
           age <-trunc(rnorm(n=1,mean = 1984,sd = 3))
         },
         "management"={
           age <-trunc(rnorm(n=1,mean = 1969,sd = 2))
         },
         "marketing"={
           age <-trunc(rnorm(n=1,mean = 1991,sd = 3))
         },
         "product_mng"={
           age <-trunc(rnorm(n=1,mean = 1981,sd = 3))
         },
         "RandD"={
           age <-trunc(rnorm(n=1,mean = 1987,sd = 3))
         },
         "sales"={
           age <-trunc(rnorm(n=1,mean = 1993,sd = 2))
         },
         "support"={
           age <-trunc(rnorm(n=1,mean = 1990,sd = 3))
         },
         "technical"={
           age <-trunc(rnorm(n=1,mean = 1985,sd = 3))
         },
         "accounting"={
           age <-trunc(rnorm(n=1,mean = 1989,sd = 3))
         },{
           age <- -1
         }
  )
  if(age>1999){
    age <- 1999
  }
  return(age)
}

yearsOfBirth <- sapply(RH$Department,FUN =impute_age )
#checks :
min(yearsOfBirth)
max(yearsOfBirth)

#Concatenation avec le dataset
RH$Date_Of_Birth = yearsOfBirth

#Export en csv
write.csv(RH, file = "~/Documents/HR_with_age_depts.csv")
