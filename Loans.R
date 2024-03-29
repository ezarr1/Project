###------------------------------------------###
#---               loading        -----
###------------------------------------------###


#load the libraries and the functions
source("Functions/Library.R")
source("Functions/initial_functions.R")
###------------------------------------------###
#---               LOANS                 -----
###------------------------------------------###


###------------------------------------------###
#---               load the data        -----
###------------------------------------------###
paths_content <- readLines("paths.txt")
LoansMetadata_line <- grep("^LoansMetadata", paths_content)
LoansMetadata_value <- sub("^LoansMetadata=\\s*", "", paths_content[LoansMetadata_line])

Loans_Raw <- read_excel(LoansMetadata_value, sheet = "LOANS")
Loans_table <- Loans_Raw[-1, ] %>% row_to_names(1)

###------------------------------------------###
#---               data preparation       -----
###------------------------------------------###
Loans <- Loans_table %>% select(c(1,3,6,8,10,12:15))

names(Loans) <- colname_function(names(Loans))

Loans <- Loans %>% rename("id.bor" = 'ndg', "id.loan" = 'id.loans', "type" = "type.of.credit", 
                          "gbv.original" = "total.gbv", "principal" = "gbv.capital", 
                          "interest" = "gbv.interest", "expenses" = "gbv.expenses",
                          "gbv.residual" = "residual.position")
#create the status column (UTP, Bad)
status <- Loans$default.date %>% as.character()
Loans<- cbind(status,Loans)
Loans$status <- if_else(Loans$status =='UTP','UTP','Bad')

Loans$default.date <- excel_numeric_to_date(as.numeric(Loans$default.date)) # NAs introduced by coercion

Loans$gbv.residual <- Loans$gbv.residual %>% gsub('-','0',.) %>% as.numeric()

#turn as.numeric and round numeric values
vector_column <- Loans %>% select(principal, interest, gbv.original, expenses) %>% names()
Loans <- Loans %>% mutate_at(vars(all_of(vector_column)), funs(round(as.numeric(.), 2)))

#create the empty columns
#vector_empty <- c("id.group","originator","ptf","cluster.ptf", "penalties",  "date.origination","date.last.act","flag.imputed")
#Loans <- create_empty_columns(Loans, vector_empty)

Loans <- Loans %>% mutate(id.group=NA,
                           originator=NA,
                           ptf=NA,
                           cluster.ptf=NA,
                           penalties=NA,
                           date.origination=NA,
                           date.last.act=NA,
                           flag.imputed=NA)

#change column format
Loans$date.origination <- Loans$date.origination %>% as.Date()
Loans$date.last.act <- Loans$date.last.act %>% as.Date()
Loans$flag.imputed <- Loans$flag.imputed %>% as.integer()

#rename date column and rearrange
Loans <- Loans %>% rename("date.status" = "default.date")
Loans <- Loans[,c(3,2,11:14,5,1,10,6:8,15,9,16,4,17,18)]




source('Functions/Excel_Type.R')

#categorize type of Loans:
#Loans$type <- sapply(Loans$type, categorize_type)
Loans <- map_types_Loans(Loans,types_table)

###------------------------------------------###
#---                    NDG              -----
###------------------------------------------###


#Load NDG table:
NDG_table <- read_excel(LoansMetadata_value, sheet = "NDG")

#Basic modifications for column names and NAs removal:
NDG_table <- NDG_table %>% rename(Region=`Borrower's Region`, Tax_ID = 'Tax ID', Name = 'BorrowerName')
NDG_table$Group <- NDG_table$Group %>% gsub('-',NA,.)


Borrowers <- NDG_table %>% select(-c(1:3,6))

separated_table <- Borrowers %>% 
  separate(Name,c('BorrowerName1','BorrowerName2','BorrowerName3','BorrowerName4'),sep = '[-,,]') %>% 
  separate(`Tax_ID`,c('TaxID1','TaxID2','TaxID3'),sep = '[-,,]')
Borrowers_table <-  separated_table %>% 
  pivot_longer(cols = matches('BorrowerN|Tax'),
               names_to = c(".value", "taxID"),
               names_pattern = "(BorrowerName|TaxID)(.*)",values_drop_na =  TRUE)

Borrowers_table$TaxID <- Borrowers_table$TaxID %>% str_trim(.,'left')
Borrowers_table$BorrowerName <- Borrowers_table$BorrowerName %>% str_trim(.,'left')
Borrowers_table <- Borrowers_table %>% 
  select(- taxID)  %>%
  rename(Name = BorrowerName, Tax_ID = TaxID)

Borrowers_table <- Borrowers_table[,c(4,5,1:3)]



Guarantors <- Loans_table %>% select(`Guarantors Name`,`TAX CODE for Guarantors`) %>% distinct() %>% na.omit(.)

separated_table_g <- Guarantors %>% 
  separate(`Guarantors Name`,c('guarantors.name1','guarantors.name2','guarantors.name3','guarantors.name4','guarantors.name5'),sep = ',') %>% 
  separate(`TAX CODE for Guarantors`,c('tax.code.for.guarantors1','tax.code.for.guarantors2','tax.code.for.guarantors3','tax.code.for.guarantors4','tax.code.for.guarantors5'),sep = ',')
Guarantors_groups <-  separated_table_g %>% 
  pivot_longer(cols = matches('guarantors|tax'),
               names_to = c(".value", "taxID"),
               names_pattern = "(guarantors.name|tax.code.for.guarantors)(.*)",values_drop_na =  TRUE)

Guarantors_groups <- Guarantors_groups %>% select(-taxID)
Guarantors_groups$guarantors.name <- Guarantors_groups$guarantors.name %>% str_trim(.,'left')
Guarantors_groups$tax.code.for.guarantors <- Guarantors_groups$tax.code.for.guarantors %>% str_trim(.,'left')


Loans <- Loans %>% mutate_if(is.character, tolower)

Loans$type <- as.factor(Loans$type)
Loans$status <- as.factor(Loans$status)
