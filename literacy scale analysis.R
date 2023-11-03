#call necessary packages
x<- c('readxl', 'writexl', 'dplyr','corrplot','psych','lavaan','psy')
lapply(x, require, character.only = TRUE)

#read in excel file of expert panel member responses contained in sheet 1 (item ratings)
item_ratings <- read_excel("C:/Users/megha/OneDrive/Documents/response_climate.infectiousdisease.literacy.xlsx", sheet = "ITEM RATINGS")

#count number of matching ratings of scale items (essential, useful, not needed) from expert panel members from sheet 1 
item_ratings$essential <- rowSums(item_ratings == 'E', na.rm = TRUE)
item_ratings$useful <- rowSums(item_ratings == 'U', na.rm = TRUE)
item_ratings$notneeded <- rowSums(item_ratings == 'NN', na.rm = TRUE)
View(item_ratings)

#categorizes items as either to keep for final scale: 'keep' or take out out final scale: 'throw' in new column 'rating' for item ratings
item_ratings$rating<- ifelse(item_ratings[10] >= 5, "keep","throw")

#subsets item_ratings sheet by items rated as essential by 5 or more expert reviewers (80% agreement)
item_ratings$rating<-as.vector(item_ratings$rating)
essential_for_scale <- item_ratings %>% filter(rating == 'keep')

#write excel file with essential rated items with over 80% agreement
write_xlsx(essential_for_scale, "C:/Users/megha/Documents/essential_scale_items.xlsx")

#imports participant file by sheet and renames to 'PB_participants' for Paramaribo and 'NK_participants' for Nickerie
PB_participants<-read_excel("C:/Users/megha/Documents/Matlack_participantIDlist.xlsx", sheet = 'Paramaribo')
NK_participants<-read_excel("C:/Users/megha/Documents/Matlack_participantIDlist.xlsx", sheet = 'Nickerie')

#390 participants are to be sampled with 30% oversampling to account for participant non-response
#70% from Paramaribo and 30% from Nickerie -- gives n = 273 for PB, n = 117 for NK
#sets seed and generates random sample of participants 
set.seed(123)
#generate random sample of 273 rows for PB participants and 117 rows for NK participants
PB_participants_random<-PB_participants[sample(nrow(PB_participants), 273) , ]
NK_participants_random<-NK_participants[sample(nrow(NK_participants), 117) , ]

#write excel files for completed random sample
write_xlsx(PB_participants_random, "C:/Users/megha/Documents/paramaribo_random_sample.xlsx")
write_xlsx(NK_participants_random, "C:/Users/megha/Documents/nickerie_random_sample.xlsx")

#########################################################################################################
#import csv file with completed survey data for 301 participants 
surveydata_raw<- read.csv("C:/Users/megha/Downloads/DevelopmentOfAClimat_DATA_2023-07-31_0958.csv")

#drop columns with participant id and recruiter id; not needed for analysis
surveydata_raw<-surveydata_raw[-c(2:3)]

#separate demographic data from scale data (2 datasets)
demo_data<-surveydata_raw[c(1:60)]
scale_data<-surveydata_raw[c(1,61:113)]

#set seed and randomly sample scale_data with 60:40 split for EFA and CFA 
#60% of participants will be selected for EFA, rest will be used for CFA 
sample_size<- floor(0.6*nrow(scale_data))
set.seed(123)
random_sample = sample(seq_len(nrow(scale_data)),size = sample_size)
EFA_random_sample<-scale_data[random_sample,]
CFA_random_sample<-scale_data[-random_sample,]

#divide EFA and CFA samples by subscales (separation of general climate change items from infectious disease items)
EFA_CC<- EFA_random_sample[,c(2:7, 11, 14:23, 25:38)]
EFA_ID<- EFA_random_sample[,c(8:10,12,13,24,39:53)]

CFA_CC<-CFA_random_sample[,c(2:7, 11, 14:23, 25:38)]
CFA_ID<-CFA_random_sample[,c(8:10,12,13,24,39:53)]
###############################################################################################
#exploratory factor analysis on both the CC and infectious disease specific samples

#create correlation matrix of CC EFA sample
CC_matrix <- cor(EFA_CC[,c(-32)])
corrplot(CC_matrix, method="number")
#generate eigenvalues from CC correlation matrix
CC_eigen<-eigen(CC_matrix)
#generate scree plot of eigenvalues to determine number of factors
CC_eigen_plot<-plot(CC_eigen$values, type = 'b', ylab = 'Eigenvalues', xlab = 'Factor')
#run initial factor analysis with determined number of factors
CC_EFA<-factanal(x=EFA_CC, factors=7, rotation='promax')
CC_EFA
#
EFA_CC2<-EFA_CC[,-c(2:3,8,15,16,17,20,21,22,23,28,29)]
CC_EFA2<-factanal(x=EFA_CC2, factors=7, rotation='promax')
CC_EFA2

EFA_CC3<-EFA_CC2[,-c(13,17,18)]
CC_EFA3<-factanal(x=EFA_CC3, factors=4, rotation = 'promax')
CC_EFA3

#create correlation matrix of ID EFA sample  
ID_matrix <- cor(EFA_ID[,c(-22)])
corrplot(ID_matrix, method="number")
#generate eigenvalues of ID correlation matrix
ID_eigen<-eigen(ID_matrix)
#plot scree plot of eigenvalues to determine number of factors
ID_eigen_plot<-plot(ID_eigen$values, type = 'b', ylab = 'Eigenvalues', xlab = 'Factor')
#run factor analysis with determined number of factors
ID_EFA<-factanal(x=EFA_ID, factors=6, rotation='promax') 
ID_EFA

EFA_ID2<-EFA_ID[-c(1,2,3,7,8,14,15,18,19,20)]
ID_EFA2<-factanal(x=EFA_ID, factors=4, rotation='promax') 
ID_EFA2
#################################################################################################
#confirmatory factor analysis on both the CC and infectious disease samples

#construct CC CFA model based on factors previously retained in EFA
CC_CFA_model<-'
 CC_effects =~ cc_item1 + cc_item5 + cc_item6
 ocean =~ cc_item30 + cc_item31
 staying_cool =~ cc_item16 + cc_item17
 heat =~ cc_item 14 + cc_item15 + cc_item18 + cc_item19
'

#estimate CC CFA model using 'cfa' function
CFA_CC_estimate<-cfa(CC_CFA_model, data = CFA_CC)
#show summary of CFA CC results
summary(CFA_CC_estimate, fit = TRUE)

#get additional goodness of fit indices (NFI, NNFI, IFI, RFI)
fitMeasures(CFA_CC_estimate, 'nfi')
fitMeasures(CFA_CC_estimate, 'nnfi')
fitMeasures(CFA_CC_estimate, 'ifi')
fitMeasures(CFA_CC_estimate, 'rfi')

#construct ID CFA model based on factors previously retained in EFA
ID_CFA_model<-'
  viruses =~ cc_item42 + cc_item43
  water_holding =~ cc_item47 + cc_item48
  transmission =~ cc_item7 + cc_item8 + cc_item23
  temperature =~ cc_item11 + cc_item12
 '

#estimate ID CFA model using 'cfa' function
CFA_ID_estimate<-cfa(ID_CFA_model, data = CFA_ID)
#show summary of CFA ID results (chi-sq, RMSEA, Bentler CFI)
summary(CFA_ID_estimate, fit = TRUE)
#get additional goodness of fit indices (NFI, NNFI, IFI, RFI)
fitMeasures(CFA_ID_estimate, 'nfi')
fitMeasures(CFA_ID_estimate, 'nnfi')
fitMeasures(CFA_ID_estimate, 'ifi')
fitMeasures(CFA_ID_estimate, 'rfi')

#assess psychometric properties of both datasets
#separate overall scale data into CC and ID scales
CC_data<-CFA_CC[,c(1,5,6,9:14,24:25)]
ID_data<-CFA_ID[,c(1,2,4,5,6,11,12,16,17)]

#calculate cronbach alpha for both CC and ID scales
cronbach(CC_data)
cronbach(ID_data)

#calculate cronbach alpha for CC scale by factor
CC_data_f1<-CC_data[,c(1:3)]
cronbach(CC_data_f1)

CC_data_f2<-CC_data[,c(4,5,8,9)]
cronbach(CC_data_f2)

CC_data_f3<-CC_data[,c(6,7)]
cronbach(CC_data_f3)

CC_data_f4<-CC_data[,c(10,11)]
cronbach(CC_data_f4)

#calculate cronbach alpha for ID scale by factor
ID_data_f1<-ID_data[,c(1,2,5)]
cronbach(ID_data_f1)

ID_data_f2<-ID_data[,c(3,4)]
cronbach(ID_data_f2)

ID_data_f3<-ID_data[,c(6,7)]
cronbach(ID_data_f3)

ID_data_f4<-ID_data[,c(8,9)]
cronbach(ID_data_f4)