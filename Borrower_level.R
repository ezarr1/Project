# BORROWER LEVEL



###------------------------------------------###
#---            table       -----
###------------------------------------------###

merge_c_l_L <- merge(Loans,merge(Counterparties,Link_loans_counterparties, by = 'id.counterparty'),by = 'id.loan')

for(i in 1:nrow(merge_c_l_L)){
  if(sum(merge_c_l_L$id.loan == merge_c_l_L$id.loan[i]) > 1){
    merge_c_l_L$flag_guarantors[i] <- TRUE
  } else{
    merge_c_l_L$flag_guarantors[i] <- FALSE
  }
}

merge_c_l_L <- merge_c_l_L %>% select(id.loan,gbv.original,flag_guarantors,id.counterparty,role) 
merge_c_l_L <- merge_c_l_L %>% filter(role == 'borrower') %>% group_by(id.counterparty) %>% 
  summarise(NLoans = n_distinct(id.loan),GBV = sum(gbv.original),flag_g = sum(flag_guarantors))
merge_c_l_L$flag_g[merge_c_l_L$flag_g >=1] <- 'with guarantors'
merge_c_l_L$flag_g[merge_c_l_L$flag_g == 0] <- 'without guarantors'


###------------------------------------------###
#---         Borrower table       -----
###------------------------------------------###
merged_data2 <-  merged_data %>%
  left_join(Link_counterparties_entities, by = "id.counterparty",relationship = "many-to-many") %>%
  left_join(Entities, by = "id.entity",relationship = "many-to-one")


borrower_mat <- merged_data2 %>% select(id.counterparty,id.loan,type,status,gbv.original,role,area)%>%
  distinct(id.counterparty,id.loan, .keep_all = TRUE)


for(i in 1:nrow(borrower_mat)){
  if(sum(borrower_mat$id.loan == borrower_mat$id.loan[i]) > 1){
    borrower_mat$flag_guarantors[i] <- TRUE
  } else{
    borrower_mat$flag_guarantors[i] <- FALSE
  }
}


borrower_mat <- borrower_mat %>% filter(role == 'borrower') %>% group_by(id.counterparty) 

borrower_mat <- borrower_mat %>%
  group_by(id.counterparty) %>%
  mutate(
    type = ifelse(gbv.original == max(gbv.original), as.character(type), " "))

borrower_mat <- borrower_mat %>% summarise(NLoans = n_distinct(id.loan), gbv.original = sum(gbv.original),
                                           flag_g = sum(flag_guarantors), area = area, status = status,
                                           type = type)
borrower_mat <- borrower_mat %>% filter(type != " ")

borrower_mat$flag_g[borrower_mat$flag_g >=1] <- 'with guarantors'
borrower_mat$flag_g[borrower_mat$flag_g == 0] <- 'without guarantors'



###------------------------------------------###
#---   Borrower table with/without g      -----
###------------------------------------------###
###------------------------------------------###
Borrower_with_without_g <- borrower_mat %>% group_by(flag_g) %>% 
  summarise(N_borrower = n_distinct(id.counterparty), N_Loans = sum(NLoans), SommaGBV = sum(gbv.original),MeanGBV = mean(gbv.original))
Borrower_with_without_g$Ratio <- round(Borrower_with_without_g$N_borrower / sum(Borrower_with_without_g$N_borrower) *100,1)
Borrower_with_without_g$Ratio <- paste(Borrower_with_without_g$Ratio, '%',sep='')
Borrower_with_without_g <- Borrower_with_without_g[,c(1,2,6,3,4,5)]

names(Borrower_with_without_g) <- c('Flag','N Borrowers','Borrowers Ratio','N Loans','GBV Sum','GBV Mean')



###---------------------------------------###
#---    borrower for type of loan      -----
###---------------------------------------###

borrower_for_type <- borrower_mat %>% group_by(type) %>%
  summarise(`N Borrowers` = n_distinct(id.counterparty),
            'N Loans' = sum(NLoans),
            'GBV Sum' = sum(gbv.original),
            'GBV Mean' = mean(gbv.original))

borrower_for_type$Ratio <- paste(round(borrower_for_type$`N Borrowers` / sum(borrower_for_type$`N Borrowers`) *100,1), "%", sep = "")
borrower_for_type <- borrower_for_type[,c(1,2,6,3,4,5)]



###---------------------------------------###
#---    borrower by status     -----
###---------------------------------------###

borrower_for_status <- borrower_mat %>% group_by(status) %>%
  summarise(`N Borrowers` = n_distinct(id.counterparty),
            'N Loans' = sum(NLoans),
            'GBV Sum' = sum(gbv.original),
            'GBV Mean' = mean(gbv.original))

borrower_for_status$Ratio <- paste(round(borrower_for_status$`N Borrowers` / sum(borrower_for_status$`N Borrowers`) *100,1), "%", sep = "")
borrower_for_status <- borrower_for_status[,c(1,2,6,3,4,5)]


###---------------------------------------------------------------------------###
#---     number and ratio of borrowers, number of loans, by area        -----
###---------------------------------------------------------------------------###



total_loans_4 <- merged_data2 %>%
  group_by(area) %>%  filter(!duplicated(id.loan)) %>% filter(role=='borrower') %>%                                      
  summarise(TotalLoans = n_distinct(id.loan))


num_borr_4 <- merged_data2 %>%
  group_by(area) %>% filter(!duplicated(id.loan)) %>% filter(role=='borrower') %>%                                         
  summarise(NumBorrowers = n_distinct(id.counterparty))

Ratio_4<- round(num_borr_4$NumBorrowers / sum(num_borr_4$NumBorrowers) *100,1)
Ratio_4 <- paste(Ratio_4, "%", sep = "")

num_borr_4 <- cbind(num_borr_4,Ratio_4)


gbv_summary_type_4 <- merged_data2 %>% filter(!duplicated(id.loan)) %>% filter(role=='borrower') %>%
  group_by(area) %>%
  summarize(
    sum_gbv = sum(gbv.original),
    mean_gbv = mean(gbv.original)
  )


Borrower_by_area <- merge(num_borr_4, merge(total_loans_4, gbv_summary_type_4, by = "area"),by = "area")
names(Borrower_by_area) <- c('Area','N Borrowers','Borrowers Ratio','N Loans','GBV Sum','GBV Mean')

Borrower_by_area[is.na(Borrower_by_area)] <- "N/a"



###-----------------------------------------------------------------------------------------------------------------------------------###
#---     pivot(cross table or contingency table of sum gvb by gbv clusters that you create and loans with/without guarantors        -----
###-----------------------------------------------------------------------------------------------------------------------------------###

q_pivot <- quantile(merge_c_l_L$GBV,probs = c(0.33, 0.66))

Range_gbv <- c(0,60000,300000,Inf)
Range_gbv_labels <- c('0-60k','60k-300k','300k +')

merge_c_l_L$Range.gbv <- cut(merge_c_l_L$GBV, breaks = Range_gbv, labels = Range_gbv_labels, include.lowest = TRUE)

pivot_table <- merge_c_l_L %>% group_by(Range.gbv,flag_g) %>% summarise(values = sum(GBV))
pivot_table <- as.data.frame(pivot_table)

gbv_by_cluster_g <- pivot_table %>%
  pivot_wider(names_from = Range.gbv, values_from = values)

gbv_by_cluster_g[is.na(gbv_by_cluster_g)] <- 0
gbv_by_cluster_g <- as.data.frame(gbv_by_cluster_g)
gbv_by_cluster_g$Total <- rowSums(gbv_by_cluster_g[2:4])
gbv_by_cluster_g <- gbv_by_cluster_g %>% rename('Guarantors /GBV Clusters' = flag_g)