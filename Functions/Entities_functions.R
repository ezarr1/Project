# ENTITIES



# colonne da aggiungere: -type.subject (individual,corporate) length(cf.piva)=16 --> ind; =10 --> corp
#                        -sex     da cf.piva
#                        -age     da cf.piva
#                        - range.age   ""
#                        - type.pg    da name (srl....)
#                        - area using the GEO sheet




#type.subject:

create_type_subject <- function(table, key) {
  for (j in 1:length(key)) {                 
    if (is.na(key[j])) {
      table$type.subject[j] <- NA  
    } else if (nchar(key[j]) == 16) {
      table$type.subject[j] <- "individual"
    } else if (nchar(key[j]) == 10) {
      table$type.subject[j] <- "corporate"
    } else {
      table$type.subject[j] <- NA
    }
  }
  return(table)
}



# sex
create_sex <- function(table, key) {
  for (j in 1:length(key)) {                 
    if (is.na(key[j])) {
      table$sex[j] <- NA  
    } else if (nchar(key[j]) == 16) {
      if(as.numeric(substr(key[j],10,11))<= 31){
        table$sex[j] <- 'male'
      } else{
        table$sex[j] <- 'female'
      }
    }  else {
      table$sex[j] <- NA
    }
  }
  return(table)
}



# age:


create_date_of_birth <- function(table,key){
  for (j in 1:length(key)) {                 
    if (is.na(key[j])) {
      table$date.of.birth[j] <- NA  
    } else if (nchar(key[j]) == 16) {
      year <- substr(key[j], 7, 8)
      month_letter <- substr(key[j], 9,9)
      day <- substr(key[j], 10, 11)
      month_mapping <- c(
        "A" = "01", "B" = "02", "C" = "03", "D" = "04",
        "E" = "05", "H" = "06", "L" = "07", "M" = "08",
        "P" = "09", "R" = "10", "S" = "11", "T" = "12"
      )
      month <- month_mapping[month_letter]
      
      if(table$sex[j]=='male'){
        table$date.of.birth[j] <- paste(paste("19",year,sep=''), month, day, sep = "-")
      } else {
        day <- as.numeric(day) - 40
        table$date.of.birth[j] <- paste(paste("19",year,sep=''), month, day, sep = "-")
      }
      
    }  else {
      table$date.of.birth[j] <- NA
    }
  }
  return(table)
}


create_age <- function(table, key) {
  current_date <- Sys.Date()
  for (j in 1:length(key)) {                 
    if(is.na(key[j])) {
      table$age[j] <- NA  
    } else{
      table$age[j] <- as.integer(difftime(current_date, key[j], units = "days") / 365.25)  
      if(table$age[j] > 118){
        table$age[j] <- table$age[j] -100
        table$key[j] <- table$key[j] +  lubridate::years(100)
      }
    }
  }
  return(table)
}


#range.age:
# cut()

create_range_age <- function(table,key){
  for (j in 1:length(key)) {                 
    if (is.na(key[j])) {
      table$range.age[j] <- NA  
    } else if(key[j] <= 25){
      table$range.age[j] <- '0-25'
    }  else if(key[j] > 25 & key[j] <= 50){
      table$range.age[j] <- '25-50'
    }  else if(key[j] > 50 & key[j] <= 65){
      table$range.age[j] <- '50-65'
    }  else if(key[j] > 65 & key[j] <= 75){
      table$range.age[j] <- '65-75'
    }  else if(key[j] > 75){
      table$range.age[j] <- '75+'
    }
  }
  return(table)
}


#type.pg: 
find_corporate_type <- function(names) {
  result <- character()
  corporate_types <- c("DI", "SS", "SAS", "SNC", "SPA", "SRL", "SC")
  pattern <- paste(corporate_types, collapse = "|")
  matches <- str_extract(names, pattern)
  matches <- str_trim(matches)
  result <- matches
  return(result)
}


create_type_pg <- function(table, key, name) {
  for (j in 1:length(key)) {                 
    if (is.na(key[j])) {
      table$type.pg[j] <- NA  
    } else if (key[j] == "corporate") {
      table$type.pg[j] <- find_corporate_type(name[j])
    }  else {
      table$type.pg[j] <- NA
    }
  }
  return(table)
}



create_n_entities <- function(table){
  for(j in 1:nrow(table)){
    table$n.entities[j] <- sum(Link_counterparties_entities$id.counterparty== table$id.counterparty[j])
  }
  return(table)
}