
###------------------------------------------###
#---            create workbook      -----
###------------------------------------------###

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Report_Loans", gridLines = FALSE)
addWorksheet(wb, sheetName = "Report_Borrowers", gridLines = FALSE)

###------------------------------------------###
#---            create styles      -----
###------------------------------------------###


TitleStyle <- createStyle(fontSize = 20,textDecoration = c('bold','underline'))
titleStyle <- createStyle(fontSize = 14,textDecoration ='bold', border = 'Bottom', borderColour = 'black')
NamesStyle <- createStyle(fontColour = "white", fgFill = "#6495ED", border = 'TopBottom',
                          textDecoration = 'bold', fontSize = 12)
cellStyle <- createStyle(fontColour = "black", fgFill = "#E0FFFF", wrapText = TRUE, fontSize = 12,halign = "right")
totalStyle <- createStyle(fontColour = "black", fgFill = "#FAFAFA", wrapText = TRUE, fontSize = 12,border = 'TopBottom')
custom_format <- createStyle(numFmt = "0.0,\"k\"")
bordosinistro <- createStyle(border = 'Left')
bordodestro <- createStyle(border = 'Right')
bordosotto <- createStyle(border = 'Bottom')
bordosopra <- createStyle(border = 'Top')
colStyle <- createStyle(halign = "left")

###------------------------------------------###
#---               functions         -----
###------------------------------------------###

crea_mini_tabella <- function(workbook,sheetname,table,titolo,col_iniziale,row_iniziale){
  nrows <- nrow(table)
  ncols <- ncol(table)
  
  writeData(workbook,sheet = sheetname,titolo,startCol = col_iniziale,startRow =row_iniziale-1 )
  addStyle(workbook, sheet = sheetname, style = titleStyle, rows = row_iniziale-1, cols = col_iniziale)
  for(col in 1:ncols){
    writeData(workbook, sheet = sheetname, x = names(table)[col], startCol = col+col_iniziale-1, startRow = row_iniziale)
    addStyle(workbook, sheet = sheetname, rows = row_iniziale, cols = col+col_iniziale-1, style = NamesStyle)
    for (row in 2:(nrows + 1)) {
      writeData(workbook, sheet = sheetname, x = table[row - 1, col], startCol = col+col_iniziale-1, startRow = row+row_iniziale-1)
      addStyle(workbook, sheet = sheetname, rows = row+row_iniziale-1, cols = col+col_iniziale-1, style = cellStyle)
    }
    setColWidths(workbook, sheet = sheetname, cols = col+col_iniziale-1, widths = 'auto')
  }
  custom_format <- createStyle(numFmt = "0.0,\"k\"")
  addStyle(workbook, sheet = sheetname, style = custom_format, rows = row_iniziale+1, cols = col_iniziale+2, stack = TRUE)
}

crea_report_tabella <- function(workbook,sheetname,table,titolo,col_iniziale,row_iniziale,n_spaces){
  nrows <- nrow(table)
  ncols <- ncol(table)
  
  writeData(workbook,sheet = sheetname,titolo,startCol = col_iniziale,startRow =row_iniziale-1 )
  addStyle(workbook, sheet = sheetname, style = titleStyle, rows = row_iniziale-1, cols = col_iniziale)
  
  for(col in 1:ncols){
    writeData(workbook, sheet = sheetname, x = names(table)[col], startCol = col+col_iniziale-1, startRow = row_iniziale)
    addStyle(workbook, sheet = sheetname, rows = row_iniziale, cols = col+col_iniziale-1, style = NamesStyle)
    for (row in 2:(nrows + 1)) {
      writeData(workbook, sheet = sheetname, x = table[row - 1, col], startCol = col+col_iniziale-1, startRow = row+row_iniziale)
      addStyle(workbook, sheet = sheetname, rows = row+row_iniziale, cols = col+col_iniziale-1, style = cellStyle)
    }
    setColWidths(workbook, sheet = sheetname, cols = col+col_iniziale-1, widths = 'auto')
  }
  
  for(row in 1:(nrow(table)+2)){
    for(col in (ncol(table)-2):(ncol(table)-1)){  
      addStyle(workbook, sheet = sheetname, style = custom_format, rows = row+row_iniziale, cols = col+col_iniziale, stack = TRUE)
    }
  }
  
  
  totali <- c('Totals',sum(table$`N Borrowers`),' ',sum(table$`N Loans`),sum(table$`GBV Sum`),' ')
  if(n_spaces > 0){
    for(i in 1:n_spaces){
      totali <- c(totali[1],' ',totali[(i+1):length(totali)])
    }
  }
  
  totali <- as.data.frame(t(totali)) 
  totali[,2:ncol(totali)] <- as.numeric(totali[,2:ncol(totali)])
  
  for(i in 1:ncol(totali)){
    writeData(workbook,sheet = sheetname,totali[,i],startCol = col_iniziale+i-1,startRow = row_iniziale+1)
    addStyle(workbook, sheet = sheetname, style = totalStyle, rows =row_iniziale+1, cols = i+col_iniziale-1, stack = TRUE)
    if(i == 4){
      addStyle(workbook, sheet = sheetname, style = custom_format, rows =row_iniziale+1, cols = i+col_iniziale+n_spaces, stack = TRUE)
    }
  }
  
}

crea_report_pivot <- function(workbook,sheetname,table,titolo,col_iniziale,row_iniziale){
  nrows <- nrow(table)
  ncols <- ncol(table)
  
  writeData(workbook,sheet = sheetname,titolo,startCol = col_iniziale,startRow =row_iniziale-1 )
  addStyle(workbook, sheet = sheetname, style = titleStyle, rows = row_iniziale-1, cols = col_iniziale)
  
  for(col in 1:ncols){
    writeData(workbook, sheet = sheetname, x = names(table)[col], startCol = col+col_iniziale-1, startRow = row_iniziale)
    addStyle(workbook, sheet = sheetname, rows = row_iniziale, cols = col+col_iniziale-1, style = NamesStyle)
    for (row in 2:(nrows + 1)) {
      writeData(workbook, sheet = sheetname, x = table[row - 1, col], startCol = col+col_iniziale-1, startRow = row+row_iniziale)
      addStyle(workbook, sheet = sheetname, rows = row+row_iniziale, cols = col+col_iniziale-1, style = cellStyle)
    }
    setColWidths(workbook, sheet = sheetname, cols = col+col_iniziale-1, widths = 'auto')
  }
  
  totali <- c('Totals',sum(table$`0-60k`),sum(table$`60k-300k`),sum(table$`300k +`),sum(table$Total))
  totali <- as.data.frame(t(totali)) 
  totali[,2:ncol(totali)] <- as.numeric(totali[,2:ncol(totali)])
  
  for(i in 1:length(totali)){
    writeData(workbook,sheet = sheetname,totali[,i],startCol = col_iniziale+i-1,startRow = row_iniziale+1)
    addStyle(workbook, sheet = sheetname, style = totalStyle, rows =row_iniziale+1, cols = i+col_iniziale-1, stack = TRUE)
  }
  
  for(row in 1:(nrow(table)+2)){
    for(col in (ncol(table)-4):(ncol(table)-1)){  
      addStyle(workbook, sheet = sheetname, style = custom_format, rows = row+row_iniziale, cols = col+col_iniziale, stack = TRUE)
    }
  }
  
}

crea_bordi <- function(workbook, sheetname, tabella,col_iniziale,row_iniziale){
  n_row <- nrow(tabella)
  n_col <- ncol(tabella)
  for(col in 1:(n_col)){
    addStyle(workbook, sheet = sheetname, style = bordosopra, rows = row_iniziale+1, cols = col_iniziale+col-1,stack = TRUE)
    addStyle(workbook, sheet = sheetname, style = bordosotto, rows = row_iniziale+n_row+1, cols = col_iniziale+col-1,stack = TRUE)
  }
  for(row in 1:(n_row)){
    addStyle(workbook, sheet = sheetname, style = bordosinistro, rows = row_iniziale+row+1, cols = col_iniziale,stack = TRUE)
    addStyle(workbook, sheet = sheetname, style = bordodestro, rows = row_iniziale+row+1, cols = col_iniziale+n_col-1,stack = TRUE)
  }
}

sistema_colonna <- function(workbook, sheetname, colonna, riga_iniziale, riga_finale){
  for(riga in riga_iniziale:riga_finale){
    addStyle(workbook,sheetname,style = colStyle, cols = colonna,rows = riga, stack = TRUE)
  }
}

###------------------------------------------###
#---              create tables      -----
###------------------------------------------###

###------------------------------------------###
#---                 Loans        -----
###------------------------------------------###
writeData(wb,"Report_Loans",'Report about the Loans Data',1,1)
addStyle(wb,sheet = "Report_Loans",style = TitleStyle,1,1)

summary <- 'SUMMARY: \n This portfolio involves 11 borrowers for a total of about $5,5 milion. \n In the first sheet of this report we analyze it on a Loan level.\n  A borrower can be counted more the once, if it has a loan with guarantors and another without. \n In the second one we do it on a borrower level.'
lines <- unlist(strsplit(summary, "\n"))
df <- data.frame(Text = lines)
for(i in 1:nrow(df)){
  writeData(wb,"Report_Loans",df$Text[i],1,i+1)
}



crea_mini_tabella(wb,"Report_Loans",totals,'Totals',2,9)

crea_report_tabella(wb,"Report_Loans",Loans_with_without_g,'Loans with/without Guarantors',5,13,0)
crea_bordi(wb,"Report_Loans",Loans_with_without_g,5,13)


crea_report_tabella(wb,"Report_Loans",Loan_by_type,'Table for type of Loan',5,19,0)
crea_bordi(wb,"Report_Loans",Loan_by_type,5,19)

crea_report_tabella(wb,"Report_Loans",Loan_by_status,'Table for status of Loan',5,27,0)
crea_bordi(wb,"Report_Loans",Loan_by_status,5,27)

crea_report_tabella(wb,"Report_Loans",Loan_by_gbv,'Table for gbv clusters',5,33,0)
crea_bordi(wb,"Report_Loans",Loan_by_gbv,5,33)

crea_report_tabella(wb,"Report_Loans",Loan_by_status_gbv,'by status and gbv clusters',5,40,2)
crea_bordi(wb,"Report_Loans",Loan_by_status_gbv,5,40)

sistema_colonna(wb, "Report_Loans", 5, 12,48)
sistema_colonna(wb, "Report_Loans", 6, 42,48)

###------------------------------------------###
#---                 Borrowers        -----
###------------------------------------------###

writeData(wb,"Report_Borrowers",'Report about the Borrower Data',1,1)
addStyle(wb,sheet = "Report_Borrowers",style = TitleStyle,1,1)

Borrower_with_without_g <- as.data.frame(Borrower_with_without_g)
crea_report_tabella(wb,"Report_Borrowers",Borrower_with_without_g,'Borrowers with/without guarantors',5,8,0)
crea_bordi(wb,"Report_Borrowers",Borrower_with_without_g,5,8)

borrower_for_type <- as.data.frame(borrower_for_type)
crea_report_tabella(wb,"Report_Borrowers",borrower_for_type,'Table of borrowers by type',5,14,0) 
crea_bordi(wb,"Report_Borrowers",borrower_for_type,5,14)

borrower_for_status <- as.data.frame(borrower_for_status)
crea_report_tabella(wb,"Report_Borrowers",borrower_for_status,'Table of borrowers by status',5,21,0) 
crea_bordi(wb,"Report_Borrowers",borrower_for_status,5,21)

crea_report_tabella(wb,"Report_Borrowers",Borrower_by_area,'Table for area',5,27,0) 
crea_bordi(wb,"Report_Borrowers",Borrower_by_area,5,27)

crea_report_pivot(wb,"Report_Borrowers",gbv_by_cluster_g,'Sum of GBV by gbv CLuster/Guarantors',5,33)
crea_bordi(wb,"Report_Borrowers",gbv_by_cluster_g,5,33)

sistema_colonna(wb, "Report_Borrowers", 5, 7,37)


###------------------------------------------###
#---             add the graph      -----
###------------------------------------------###
insertImage(wb,sheet = "Report_Loans","File/grafico.png",startCol = 12, startRow = 13, width = 4.5, height = 4.5, dpi = 300)

insertImage(wb,sheet = "Report_Borrowers","File/grafico_borr.png",startCol = 12, startRow = 13, width = 4.5, height = 4.5, dpi = 300)

###------------------------------------------###
#---              save the file      -----
###------------------------------------------###

saveWorkbook(wb, file = "File/Report_Tables.xlsx",overwrite =TRUE )

