
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
result_entities_in_counterparty <- check_col1_in_col2(Entities_feather$id.entity,Link_counterparties_entities_feather$id.entity)
result_counterparty_in_loan <- check_col1_in_col2(Counterparties_feather$id.counterparty,Link_loans_counterparties_feather$id.counterparty)
result_loans_in_counterparty <- check_col1_in_col2(Loans_feather$id.loan,Link_loans_counterparties_feather$id.loan)








# check primary key (da migliorare)

check_presence_primary_key <- function(data, primary_key_name) {
  if (primary_key_name %in% colnames(data)) {
    print("------------ THE DATAFRAME HAS THE PRIMARY KEY ------------")
    return(TRUE)
  } else {
    warning(paste("Column", primary_key_name, "does not exist in the dataframe"))
    return(FALSE)
  }
}


data <- Entities
primary_key_name <- "id.entity"
check_result <- check_presence_primary_key(data, primary_key_name)


check_factors_for_dependent_column <- function(data, factors, column_to_check, dependency_column, dependency_factor) {
  for (i in 1:nrow(data)) {
    if (dependency_column[[i]] == dependency_factor) {
      if (column_to_check ==) {
        warning(paste())
        return(FALSE)
      } 
    }
  }
}


