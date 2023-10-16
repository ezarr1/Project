
###------------------------------------------###
#---          write feather files        -----
###------------------------------------------###

write_feather(Loans, "Feather_Files/Loans.feather")
write_feather(Link_loans_counterparties, "Feather_Files/Link_loans_counterparties.feather")
write_feather(Counterparties, "Feather_Files/Counterparties.feather")
write_feather(Link_counterparties_entities, "Feather_Files/Link_counterparties_entities.feather")
write_feather(Entities, "Feather_Files/Entities.feather")

###------------------------------------------###
#---         read feather files        -----
###------------------------------------------###

Loans_feather <- read_feather("Feather_Files/Loans.feather")
Link_loans_counterparties_feather <- read_feather("Feather_Files/Link_loans_counterparties.feather")
Counterparties_feather <- read_feather("Feather_Files/Counterparties.feather")
Link_counterparties_entities_feather <- read_feather("Feather_Files/Link_counterparties_entities.feather")
Entities_feather <- read_feather("Feather_Files/Entities.feather")




###------------------------------------------###
#---           check primary key    -----
###------------------------------------------###
check_primary_key <- function(table, column) 
  if(column %in% colnames(table)) {
    if (length(unique(table[[column]])) == nrow(table) && !any(is.na(table[[column]]))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return('Error: column not found')
}

check_primary_key(Loans_feather,"id.loan")
check_primary_key(Entities_feather,"id.entity")
check_primary_key(Counterparties_feather,"id.counterparty")


###------------------------------------------###
#---            check sum GBV    -----
###------------------------------------------###

# check sum gbv ----> residual = principal + interest + penalties + expenses
#our residual and penalties = 0  ---> check original = interest + principal

check_sum <- function(table,col1,col2,col3){
  for(i in 1:nrow(table)){
    x <- round((col1[i] - col2[i] - col3[i]),2)
    if(x == 0){
      table$check.sum[i] <- TRUE
    } else {
      table$check.sum[i] <- FALSE
    }
  }
  return(table)
}
Loans_feather$check.sum <- NA
Loans_feather <- check_sum(Loans_feather,Loans_feather$gbv.original,Loans_feather$principal,Loans_feather$interest)





###------------------------------------------###
#---          check on Loans        -----
###------------------------------------------###

check_residual <- function(table,column){
  if(column %in% colnames(table)) {
    if (!any(is.na(table[[column]]))) {
      return('The column doesnt contain any NAs')
    } else {
      return('The column contains NAs')
    }
  } else {
    return('Error: column not found')
  }
}

check_residual(Loans_feather,'gbv.residual')



###------------------------------------------###
#---          check on Links      -----
###------------------------------------------###

# entity in counter in loan ecc...

check_col1_in_col2 <- function(col1, col2){
  lista <- c()
  for(i in 1:length(col1)){
    if(col1[i] %in% col2){
      check <- TRUE
    } else {
      check <- FALSE
      lista <- c(lista,col1[i])
    }
  }
  result <- c(check,lista)
  return(result)
}

result_counterparty_in_entities <- check_col1_in_col2(Counterparties_feather$id.counterparty,Link_counterparties_entities_feather$id.counterparty)
result_entities_in_counterparty <- check_col1_in_col2(Entities_feather$id.entity,Link_counterparties_entities_feather$id.entity)
result_counterparty_in_loan <- check_col1_in_col2(Counterparties_feather$id.counterparty,Link_loans_counterparties_feather$id.counterparty)
result_loans_in_counterparty <- check_col1_in_col2(Loans_feather$id.loan,Link_loans_counterparties_feather$id.loan)



###------------------------------------------###
#---      check on n.entities      -----
###------------------------------------------###


L <- Link_counterparties_entities_feather %>% group_by(id.counterparty) %>% summarise(N =  n_distinct(id.entity))
m <- merge(L,Counterparties_feather, by = "id.counterparty")
are_columns_same <- identical(m$N,m$n.entities)
are_columns_same



###------------------------------------------###
#---   check on duplicates in cf.piva     -----
###------------------------------------------###

are_duplicates <- function(colonna){
  x <- colonna %>% as.data.frame() %>% filter(!is.na(.))
  if(n_distinct(x) == nrow(x))
    return('non ci sono duplicati')
}

are_duplicates(Entities_feather$cf.piva)



###------------------------------------------###
#---        check on  cf.piva     -----
###------------------------------------------###


generate_fiscal_code <- function(name) {
  input_string <- trimws(name)
  words <- strsplit(input_string, " ")[[1]]
  name <- toupper(gsub(" ", "", words[2]))
  last_name <- toupper(gsub(" ", "", words[1]))
  if (nchar(name) < 2 || nchar(last_name) < 2) {
    return("Invalid input: Name and last name must be at least 2 characters long.")
  }
  last_name_consonants <- gsub("[AEIOU]", "", last_name)
  if (nchar(last_name_consonants) >= 3) {
    last_name_consonants <- substr(last_name_consonants, 1, 3)
  } else {
    # If the last name has fewer than 3 consonants, fill with the first available vowels
    last_name_vowels <- gsub("[BCDFGHJKLMNPQRSTVWXYZ]", "", last_name)
    last_name_consonants <- paste0(last_name_consonants, substr(last_name_vowels, 1, 3 - nchar(last_name_consonants)))
  }
  name_consonants <- gsub("[AEIOU]", "", name)
  if (nchar(name_consonants) >= 3) {
    name_consonants <- substr(name_consonants, 1, 3)
  } else {
    name_vowels <- gsub("[BCDFGHJKLMNPQRSTVWXYZ]", "", name)
    name_consonants <- paste0(name_consonants, substr(name_vowels, 1, 3 - nchar(name_consonants)))
  }
  fiscal_code <- paste0(last_name_consonants, name_consonants)
  return(fiscal_code)
}

# Function to check if a given fiscal code is correct
check_fiscal_code <- function(table, name, given_fiscal_code) {
  for(j in 1:nrow(table)){
    if(is.na(given_fiscal_code[j])){
      table$check[j] <- 'non disponibile'
    } else if(nchar(given_fiscal_code[j])==16){
      generated_fiscal_code <- toupper(generate_fiscal_code(table$name[j]))
      if (generated_fiscal_code == toupper(substr(given_fiscal_code[j],1,6))) {
        table$check[j] <- TRUE
      } else {
        table$check[j] <- FALSE
      }
    } else {
      table$check[j] <- 'non valido'
    }
  }
  return(table)
}

prova_cf <- Entities %>% select(name,cf.piva, type.subject)
prova_cf <- prova_cf %>% filter(type.subject == 'individual') %>% select(-type.subject)
prova_cf$check <- NA
prova_cf <- check_fiscal_code(prova_cf,prova_cf$name,prova_cf$cf.piva)
x <- nrow(prova_cf) - sum(prova_cf$check==TRUE)
cat('ci sono ', x, 'errori in ',nrow(prova_cf),' righe')


