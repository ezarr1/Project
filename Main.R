#TO RUN THE CODE:
#1) Loans: we normalize the Loan and Ndg tables
source('Loans.R')

#2) Count_Ent_Loan: we create the Loan, Counterparties, Entities tables
source('Count_Ent_Loan.R')

#3) Loans_level: create the reports tables on loan level
source('Loans_level.R')

#4) Borrower_level: create the reports tables on borrower level
source('Borrower_level.R')

#5) Report_in_excel:
source('Report_in_excel.R')

#6) Feather: save the tables in feather files
source('Feather.R')

#7) Checks: some functions to check the consistencies in the tables
source('checks.R')