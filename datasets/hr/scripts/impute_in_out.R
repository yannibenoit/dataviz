library(readxl)
library(stringr)

RH <- read_excel("~/Documents/Datasets/RH/hr_dataset.xlsx")
table(RH$Date_Of_Birth)

#############
# CHECK AGE #
#############

check_age <- function(date_birth, time_spent){
  
  if(((2016 - date_birth) - time_spent)<19){
    result <- (date_birth - (18 - ((2016 - date_birth) - time_spent)+ 1) )
  }
  else{
    result <- date_birth
  }
  return(result)
}



RH$Date_Of_Birth <- mapply(FUN=check_age,RH$Date_Of_Birth,RH$time_spend_company)

############
# CAN WORK #
############


can_work <- function(birth_years){
  return(birth_years + 19)
}

year_can_work <- sapply(RH$Date_Of_Birth,FUN=can_work)

#######
# AGE #
#######

RH$Age <- (2017-RH$Date_Of_Birth)
table(RH$Age)

##################
# MOIS NAISSANCE #
##################

months <- c("Janvier","Février","Mars","Avril","Mai","Juin","Juillet","Août","Septembre","Octobre","Décembre")
probs <- c(0.078,0.084,0.089,0.075,0.076,0.086,0.081,0.076,0.113,0.07,0.089)

imputed_months<- sample(months,size = nrow(RH), replace = TRUE,prob = (probs))


##################
# DATE NAISSANCE #
##################

days_months = substr(sample(seq(as.Date(paste0(min(RH$Date_Of_Birth),'/01/01')), as.Date(paste0(max(RH$Date_Of_Birth),'/12/31')), by="day"), nrow(RH),replace = TRUE),start = 6,stop = 10)
           

birth_inc <-str_split_fixed(days_months, "-", 2)

birth_com = paste(birth_inc[,2],birth_inc[,1],RH$Date_Of_Birth, sep ="/")


#Concatenation avec le dataset
RH$Birth = birth_com


######
# IN #
######

impute_in <- function(time_spent,left,can){
  
  if(left == 0){
    return(2016-time_spent)
  }
  else{
    if(can == (2016-time_spent)){
      return(can)
    }else{
      duree <- sample(can:(2016-time_spent), 1)
      return(duree)
    }
  }
}


yearsOfIn <- mapply(FUN=impute_in,RH$time_spend_company, RH$left,year_can_work)


days_months = substr(sample(seq(as.Date(paste0(min(RH$Date_Of_Birth),'/01/01')), as.Date(paste0(max(RH$Date_Of_Birth),'/12/31')), by="day"), nrow(RH),replace = TRUE),start = 6,stop = 10)

day_inc <-str_split_fixed(days_months, "-", 2)



day_in_com = paste(day_inc[,2],day_inc[,1],yearsOfIn, sep ="/")


#Concatenation avec le dataset
RH$In = day_in_com


#######
# OUT #
#######

impute_out <- function(time_spent,left,date_in) {
  if(left == 1){
    out <- (date_in + time_spent)
    return(out)
  }
  else{
    
  }
}

yearsOfOut <- mapply(FUN=impute_out,RH$time_spend_company, RH$left,yearsOfIn)

days_months = substr(sample(seq(as.Date(paste0(min(RH$Date_Of_Birth),'/01/01')), as.Date(paste0(max(RH$Date_Of_Birth),'/12/31')), by="day"), nrow(RH),replace = TRUE),start = 6,stop = 10)

day_out <-str_split_fixed(days_months, "-", 2)

day_out_com = paste(day_out[,2],day_out[,1],yearsOfOut, sep ="/")


#Concatenation avec le dataset
RH$Out = day_out_com

day_out_com


clear <- function(left,out_date){
  
  if(left == 0){
    out_date <- ' '
  }
  else{
    return (out_date)
  }
}

years <- mapply(FUN=clear, RH$left, RH$Out)


##########
# EXPORT #
##########

#Export en csv
write.csv(RH, file = "~/Documents/HR_with_month.csv")

