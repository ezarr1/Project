# Tables:
#number of borrowers overall
#sum gbv overall
#number and ratio of borrowers, number of loans, sum and mean gbv by loans with/without guarantors 
#number and ratio of borrowers, number of loans, sum and mean gbv by type of loan
#number and ratio of borrowers, number of loans, sum and mean gbv by status of loan
#number and ratio of borrowers, number of loans, by gbv clusters (buckets) (create 3 clusters that make sense)
#number and ratio of borrowers, number of loans, by area
#pivot(cross table or contingency table of sum gvb by gbv clusters that you create and loans with/without guarantors



###------------------------------------------###
#---  number of borrowers overall        -----
###------------------------------------------###

number.of.borrowers <- Counterparties %>% filter(role == 'borrower') %>% summarise(total_entities = n_distinct(id.counterparty))
total.borrowers <- number.of.borrowers %>% select(total_entities) %>% rename(Borrowers = total_entities)

###------------------------------------------###
#---            sum gbv overall       -----
###------------------------------------------###

total.gbv.original <- Loans %>% select(gbv.original) %>% summarise('Total GBV' = sum(.))


###------------------------------------------###
#---            number of loans       -----
###------------------------------------------###

number.of.loans <- Loans %>% select(id.loan) %>% summarise('N Loans' = n_distinct(.))


totals <- cbind(total.borrowers,number.of.loans,total.gbv.original)

###-----------------------------------------------------------------------------------------------------###
#---number and ratio of borrowers, number of loans, sum and mean gbv by loans with/without guarantors -----
###-----------------------------------------------------------------------------------------------------###
###------------------------------------------------------###
#---        loans with/without guarantors -----
###------------------------------------------------------###

merged_data <- Loans %>%
  left_join(Link_loans_counterparties, by = "id.loan",relationship = "one-to-many") %>%
  left_join(Counterparties, by = "id.counterparty",relationship = "many-to-one")

numb_loans <- merged_data %>%                                         
  summarise(TotalLoans_with_guarantors = sum(duplicated(id.loan)), TotalLoans_without_guarantors = n_distinct(id.loan)-sum(duplicated(id.loan)))

numb_loans <- as.data.frame(t(numb_loans)) %>% rename(NofLoans = V1)

numb_borrowers <- merged_data %>%
  summarise(
    TotalBorrowers_with_guarantors = merged_data %>% arrange(role == "borrower") %>%
      filter(duplicated(id.loan)) %>%filter(role=='borrower') %>% summarise(n_distinct(id.counterparty)),
    TotalBorrowers_without_guarantors = merged_data %>% arrange(role == "borrower") %>%
      filter(!duplicated(id.loan))%>% filter(role == 'borrower') %>% summarise(n_distinct(id.counterparty)))

numb_borrowers <- as.data.frame(t(numb_borrowers)) %>% rename(NofBorrowers = V1)

ratio <- round(numb_borrowers / (numb_borrowers$NofBorrowers[1]+numb_borrowers$NofBorrowers[2]) *100,1)
ratio$NofBorrowers <- paste(ratio$NofBorrowers, "%", sep = "")
ratio <- ratio %>% rename(ratioBorrowers = NofBorrowers)

gbv_sum <- merged_data  %>%
  summarize(
    sum_gbv_with = merged_data %>% arrange(role == "borrower") %>%
      filter(duplicated(id.loan)) %>% summarise(sum(gbv.original)),
    sum_gbv_without = merged_data %>% arrange(role == "borrower") %>%
      filter(!duplicated(id.loan))%>% filter(role == 'borrower') %>% summarise(sum(gbv.original)))

gbv_sum <- as.data.frame(t(gbv_sum)) %>% rename(sumGbv = V1)

gbv_mean = merged_data  %>%
  summarize(
    mean_gbv_with = merged_data %>% arrange(role == "borrower") %>%
      filter(duplicated(id.loan)) %>% summarise(mean(gbv.original)), 
    mean_gbv_without = merged_data %>% arrange(role == "borrower") %>%
      filter(!duplicated(id.loan))%>% filter(role == 'borrower') %>% summarise(mean(gbv.original)))

gbv_mean <- as.data.frame(t(gbv_mean)) %>% rename(meanGbv = V1) 


nomi_col <- c('with guarantors','without guarantors')
nomi_col <- as.data.frame(nomi_col)
Loans_with_without_g <- cbind(nomi_col,numb_borrowers,ratio,numb_loans,gbv_sum,gbv_mean)

names(Loans_with_without_g) <- c('   ','N Borrowers','Borrowers Ratio','N Loans','GBV Sum','GBV Mean')


###-----------------------------------------------------------------------------------------------------###
#---     number and ratio of borrowers, number of loans, sum and mean gbv by type of loan        -----
###-----------------------------------------------------------------------------------------------------###


total_loans <- merged_data %>%
  group_by(type) %>%  filter(role=='borrower') %>%                                        
  summarise(TotalLoans = n_distinct(id.loan))


num_borr <- merged_data %>%
  group_by(type) %>%  filter(role=='borrower')  %>%                                     
  summarise(NumBorrowers = n_distinct(id.counterparty))


Ratio<- round(num_borr$NumBorrowers / sum(num_borr$NumBorrowers) *100,1)
Ratio <- paste(Ratio, "%", sep = "")

num_borr <- cbind(num_borr,Ratio)


gbv_summary_type <- Loans %>%
  group_by(type) %>%
  summarize(
    sum_gbv = sum(gbv.original),
    mean_gbv = mean(gbv.original)
  )


Loan_by_type <- merge(num_borr, merge(total_loans, gbv_summary_type, by = "type"),by = "type")
names(Loan_by_type) <- c('Type of Loan','N Borrowers','Borrowers Ratio','N Loans','GBV Sum','GBV Mean')

###-----------------------------------------------------------------------------------------------------###
#---     number and ratio of borrowers, number of loans, sum and mean gbv by status of loan        -----
###-----------------------------------------------------------------------------------------------------###


total_loans_2 <- merged_data %>%
  group_by(status) %>%   filter(role=='borrower') %>%                                        
  summarise(TotalLoans = n_distinct(id.loan))


num_borr_2 <- merged_data %>%
  group_by(status) %>% filter(role=='borrower')  %>%                                         
  summarise(NumBorrowers =n_distinct(id.counterparty))



Ratio_2<- round(num_borr_2$NumBorrowers / sum(num_borr_2$NumBorrowers) *100,1)
Ratio_2 <- paste(Ratio_2, "%", sep = "")

num_borr_2 <- cbind(num_borr_2,Ratio_2)


gbv_summary_type_2 <- Loans %>%
  group_by(status) %>%
  summarize(
    sum_gbv = sum(gbv.original),
    mean_gbv = mean(gbv.original)
  )


Loan_by_status <- merge(num_borr_2, merge(total_loans_2, gbv_summary_type_2, by = "status"),by = "status")
names(Loan_by_status) <- c('Status of Loan','N Borrowers','Borrowers Ratio','N Loans','GBV Sum','GBV Mean')



###-----------------------------------------------------------------------------###
#---     number and ratio of borrowers, number of loans, by gbv clusters   -----
###-----------------------------------------------------------------------------###
q <- quantile(Loans$gbv.original,probs = c(0.33, 0.66))

#range_gbv <- c(0,1200,120000,Inf)
#range_gbv_labels <- c('0-1.2k','1.2k-120k','120k +')

Range_gbv <- c(0,60000,300000,Inf)
Range_gbv_labels <- c('0-60k','60k-300k','300k +')
merged_data$range.gbv <- cut(merged_data$gbv.original, breaks = Range_gbv, labels = Range_gbv_labels, include.lowest = TRUE)


total_loans_3 <- merged_data %>%
  group_by(range.gbv) %>%  filter(role=='borrower') %>%                                       
  summarise(TotalLoans = n_distinct(id.loan))

num_borr_3 <- merged_data %>%
  group_by(range.gbv) %>%  filter(role=='borrower')  %>%                                       
  summarise(NumBorrowers = n_distinct(id.counterparty))


Ratio_3<- round(num_borr_3$NumBorrowers / sum(num_borr_3$NumBorrowers) *100,1)
Ratio_3 <- paste(Ratio_3, "%", sep = "")

num_borr_3 <- cbind(num_borr_3,Ratio_3)

gbv_summary_type_3 <- merged_data %>% 
  group_by(range.gbv) %>% filter(role=='borrower') %>%
  summarize(
    sum_gbv = sum(gbv.original),
    mean_gbv = mean(gbv.original)
  )


Loan_by_gbv <- left_join(num_borr_3, left_join(total_loans_3, gbv_summary_type_3, by = "range.gbv"),by = "range.gbv")
Loan_by_gbv <- as.data.frame(Loan_by_gbv)
names(Loan_by_gbv) <- c('GBV Clusters','N Borrowers','Borrowers Ratio','N Loans','GBV Sum','GBV Mean')


###-----------------------------------------------------------------------------------------------------###
#---     number and ratio of borrowers, number of loans, sum and mean gbv by status of loan and gbv cluster        -----
###-----------------------------------------------------------------------------------------------------###

q <- quantile(Loans$gbv.original,probs = c(0.33, 0.66))

#range_gbv <- c(0,1200,120000,Inf)
#range_gbv_labels <- c('0-1.2k','1.2k-120k','120k +')
Range_gbv <- c(0,60000,300000,Inf)
Range_gbv_labels <- c('0-60k','60k-300k','300k +')
merged_data$range.gbv <- cut(merged_data$gbv.original, breaks = Range_gbv, labels = Range_gbv_labels, include.lowest = TRUE)



Total_loans_2 <- merged_data %>%
  group_by(status,range.gbv) %>%   filter(role=='borrower') %>%                                        
  summarise(TotalLoans = n_distinct(id.loan))

Num_borr_2 <- merged_data %>%
  group_by(status,range.gbv) %>% filter(role=='borrower')  %>%                                         
  summarise(NumBorrowers =n_distinct(id.counterparty))



Ratio__2<- round(Num_borr_2$NumBorrowers / sum(Num_borr_2$NumBorrowers) *100,1)


Num_borr_2 <- cbind(Num_borr_2,Ratio__2)


Gbv_summary_type_2 <- merged_data %>%
  group_by(status,range.gbv) %>% filter(role=='borrower')  %>% 
  summarise(
    sum_gbv = sum(gbv.original),
    mean_gbv = mean(gbv.original)
  )


Loan_by_status_gbv <- merge(Num_borr_2, merge(Total_loans_2, Gbv_summary_type_2, by = c("status","range.gbv")),by = c("status","range.gbv"))
names(Loan_by_status_gbv) <- c('Status','Range GBV','N Borrowers','Borrowers Ratio','N Loans','GBV Sum','GBV Mean')


status_gbv <- data.frame(status = character(0), range = character(0))
status_possible <- Loans %>% select(status) %>% distinct() %>% as.data.frame() %>% rename(Status = status)
gbv_possible <- c('0-60k','60k-300k','300k +') %>% as.data.frame() %>% rename(`Range GBV` = '.')
for(i in 1:nrow(status_possible)){
  for(j in 1:nrow(gbv_possible)){
    combination <- data.frame(Status = status_possible$Status[i], `Range GBV` = gbv_possible$`Range GBV`[j])
    status_gbv <- rbind(status_gbv, combination)
  }
}
status_gbv <- status_gbv %>% rename(`Range GBV` = Range.GBV)
Loan_by_status_gbv <- left_join(status_gbv,Loan_by_status_gbv, by = c('Status','Range GBV'))

Loan_by_status_gbv[is.na(Loan_by_status_gbv)] <- 0
Loan_by_status_gbv$`Borrowers Ratio` <- paste(Loan_by_status_gbv$`Borrowers Ratio`, "%", sep = "")





###------------------------------------------------###
#---    Loan  status and type  GRAPH    -----
###------------------------------------------------###

total_loans_stat_type <- merged_data %>%
  group_by(type,status) %>%   filter(role=='borrower') %>%                                        
  summarise(TotalLoans = n_distinct(id.loan), GBV = sum(gbv.original))
total_loans_stat_type$GBV <- paste(round((total_loans_stat_type$GBV/1000),1),'k')

grafico <- ggplot(total_loans_stat_type, aes(x = type, y = TotalLoans, fill = status)) +
  geom_bar(stat = 'identity', color = 'black' ) +
  geom_text(aes(label = TotalLoans), vjust = 2.5, size = 5) +
  geom_text(aes(label = GBV ), vjust = 1.2, size = 5) +
  labs(x = "Type", y = "N Loans", color = "Status") +
  scale_fill_manual(values = c("utp" = "#56B0FF", "bad" = "#CD6E94"))+ 
  theme_minimal()+ theme(axis.text.x = element_text(angle=30,size = 12),axis.text.y = element_text(size = 12))+
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  ggtitle('N Loans/GBV by type and status') +
  theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=15))

ggsave("File/grafico.png",plot = grafico)




file.remove("File/grafico.png")



