##########***************###################**********************##############
############///                10-04-2024                   \\\\\\\\############
##########***************###################**********************##############
Sys.setenv(http_proxy = "172.16.12.70:80",
           https_proxy = "172.16.12.70:80")
setwd("D:/0_PhD Work/6_DataAnalysis")
library(tidyverse)
library(mice)
library(broom)
?mice
##########***************###################**********************##############
##########*Imputation of 2015-19 data
##########***************###################**********************##############
#to prepare sub-domain wise data 2015-19
data<-read.csv("D:/0_PhD Work/6_DataAnalysis/Objective 0/1_Data_15_19/data_V1_O_15_19.csv")

# we need to separate the data based on the sub-domain.
# Note: I have excluded those subdomains without any missing value because we dont need to impute them
# Sub-domains without missing values:
#                                   HS_3_1: HS_3_2
#                                   SC_7_1
#                                   HS_3_1: HS_3_2
# I have numbered the sub-domain by excluding the above sub-domain
# I will also exclude the health governance sub-domain, because they are categorical in nature
# i will read about how to impute categorical variables in R using mice package
mean(is.na(data$HSy_6_1))
SD1<-select(data, 2,5:11, HS_1_1:HS_1_7)
SD2<-select(data, 2,5:11, HS_2_1:HS_2_12)
SD3<-select(data, 2,5:11, HS_4_1:HS_4_12)
SD4<-select(data, 2,5:11, HR_1_4:HR_1_8)
SD5<-select(data, 2,5:11, HR_3_1:HR_3_4)
SD6<-select(data, 2,5:11, HR_4_1:HR_4_4)
SD7<-select(data, 2,5:11, SC_1_1:SC_1_9)
SD8<-select(data, 2,5:11, SC_2_1A:SC_2_1D)
SD9<-select(data, 2,5:11, SC_3_1:SC_3_3)
SD10<-select(data, 2,5:11, SC_4_2:SC_4_3)
SD11<-select(data, 2,5:11, SC_5_1:SC_5_2)
SD12<-select(data, 2,5:11, SC_11_1)
SD13<-select(data, 2,5:11, HSy_1_6)
SD14<-select(data, 2,5:11, HSy_2_3)
SD15<-select(data, 2,5:11, HSy_3_1A:HSy_3_1D)
SD16<-select(data, 2,5:11, HSy_5_1:HSy_5_2)
SD17<-select(data, 2,5:11, HSy_6_1)
SD18<-select(data, 2,5:11, HSy_7_1A: HSy_7_1L)

i<-7
# i am going to save the sub-domain wise 2015-19 data to a folder, so that i can use that later to impute
for (i in 1:18) {
  write.csv(get(sprintf("SD%d",i)),sprintf("D:/0_PhD Work/6_DataAnalysis/Objective 0/1_Data_15_19/SD_15_19/SD%d.csv",i),row.names = F)
}
       
##########***************###################**********************##############
# here we are trying to impute continuous variables and preferred imputation method 
# is "pmm" (predictive mean matching)
##########***************###################**********************##############

data_V2<-list()
for (i in 1:17) {
  data<-read.csv(file.path(sprintf("D:/0_PhD Work/6_DataAnalysis/Objective 0/1_Data_15_19/SD_15_19/SD%d.csv",i)))   
  #to find the missing values
  MP<-mean(is.na(data[,-(1:8)]))
  # i decided to generate 5 datasets if the missing proportion <=0.1, 
  #                      10 datasets if the missing proportion <=0.2,
  #                      15 datasets if the missing proportion <=0.3,
  #                      20 datasets if the missing proportion >0.3,
  nmd <-ifelse (MP<=0.1,5,
                       ifelse(MP<=0.2,10,
                                     ifelse(MP<=0.3,15,20)))
  MD <- mice(data, m=nmd, maxit=50, method="pmm", pred= quickpred(data), 
             seed = 23189, printFlag = FALSE)
  MD_data<-list()
  for (j in 1:nmd){
    imp <-complete(MD, j)
    MD_data [[j]]<-imp
  }
  # Combine all data frames in the list into one data frame
  combined_data <- do.call(rbind, MD_data)
  aggregated_result <- aggregate(. ~ CC + YC, data = combined_data, FUN = mean)
  data_V2[[i]]<-aggregated_result [,-(1:8)]
}
# Combine all data frames in the list into one data frame
df <- bind_cols(data_V2)
write.csv(aggregated_result, "data.csv")
##########***************###################**********************##############
# here we are trying to impute categorical variables (Binary variables) and 
# preferred imputation method is "logreg" (predictive mean matching)
# in our study sub-domain health governance is constituted by a number binary variables
##########***************###################**********************##############
i<-18
data<-read.csv(file.path(sprintf("D:/0_PhD Work/6_DataAnalysis/Objective 0/1_Data_15_19/SD_15_19/SD%d.csv",i)))
data[, paste0("HSy_7_1", LETTERS[1:12])] <- lapply(data[, paste0("HSy_7_1", LETTERS[1:12])], as.factor)

# since it is a binary variable we can not take mean values of multiple datasets
# generated using mice, therefore we will generate only one data set, m=1
MD <- mice(data, m=1, maxit=50, method="logreg", pred= quickpred(data), 
           seed = 23189, printFlag = FALSE)
df<-complete(MD,1)
write.csv(df,'demo.csv')

##########***************###################**********************##############
##########*Imputation of 2020-21 data
##########***************###################**********************##############

#to prepare sub-domain wise data 2020-21
data<-read.csv("D:/0_PhD Work/6_DataAnalysis/Objective 0/2_Data_20_21/data_V1_O_20_21.csv")
# we need to separate the data based on the sub-domain.
# Note: I have excluded those subdomains without any missing value and with 
#       complete missing values because we dont need to impute them
#
#         Sub-domains without missing values:
#                                   
#                                   SC_7
#                                   HS_3
#         Sub-domains with variables which have complete missing values: 
#                                   
#                                   HR_1
#                                   HR_3
#                                   HS_2
#                                   HR_4
#                                   SC_1
#                                   SC_2
#                                   HSy_2
#
#
# I have numbered the sub-domain by excluding the above sub-domains
# I will also exclude the health governance sub-domain, because they are 
# categorical in nature, and will impute separately
# i will read about how to impute categorical variables in R using mice package
lapply(data[, 12:87], function(x) mean(is.na(x)))

SD1<-select(data, 2,5:11, HS_1_1:HS_1_7)
SD2<-select(data, 2,5:11, HS_4_1:HS_4_12)
SD3<-select(data, 2,5:11, SC_3_1:SC_3_3)
SD4<-select(data, 2,5:11, SC_4_2:SC_4_3)
SD5<-select(data, 2,5:11, SC_5_1:SC_5_2)
SD6<-select(data, 2,5:11, SC_11_1)
SD7<-select(data, 2,5:11, HSy_1_6)
SD8<-select(data, 2,5:11, HSy_3_1A:HSy_3_1D)
SD9<-select(data, 2,5:11, HSy_5_1:HSy_5_2)
SD10<-select(data, 2,5:11, HSy_6_1)
SD11<-select(data, 2,5:11, HSy_7_1A: HSy_7_1L)

# i am going to save the sub-domain wise 2020-21 data to a folder, so that i 
# can use that later to impute
for (i in 1:11) {
  write.csv(get(sprintf("SD%d",i)),sprintf("D:/0_PhD Work/6_DataAnalysis/Objective 0/2_Data_20_21/SD_20_21/SD%d.csv",i),row.names = F)
}

##########***************###################**********************##############
# here we are trying to impute continuous variables (SD1:SD11) and preferred 
# imputation method is "pmm" (predictive mean matching)
##########***************###################**********************##############
data_V2<-list()
i<-3
for (i in 1:10) {
  data<-read.csv(file.path(sprintf("D:/0_PhD Work/6_DataAnalysis/Objective 0/2_Data_20_21/SD_20_21/SD%d.csv",i)))   
  #to find the missing values
  MP<-mean(is.na(data[,-(1:8)]))
  # i decided to generate 5 datasets if the missing proportion <=0.1, 
  #                      10 datasets if the missing proportion <=0.2,
  #                      15 datasets if the missing proportion <=0.3,
  #                      20 datasets if the missing proportion >0.3,
  nmd <-ifelse (MP<=0.1,5,
                ifelse(MP<=0.2,10,
                       ifelse(MP<=0.3,15,20)))
  MD <- mice(data, m=nmd, maxit=50, method="pmm", pred= quickpred(data), 
             seed = 23189, printFlag = FALSE)
  MD_data<-list()
  for (j in 1:nmd){
    imp <-complete(MD, j)
    MD_data [[j]]<-imp
  }
  # Combine all data frames in the list into one data frame
  combined_data <- do.call(rbind, MD_data)
  aggregated_result <- aggregate(. ~ CC + YC, data = combined_data, FUN = mean)
  data_V2[[i]]<-aggregated_result
}
# Combine all data frames in the list into one data frame
df <- bind_cols(data_V2)
write.csv(df,'demo.csv')
##########***************###################**********************##############
# here we are trying to impute categorical variables (Binary variables) and 
# preferred imputation method is "logreg" (predictive mean matching)
# in our study sub-domain health governance is constituted by a number binary variables
##########***************###################**********************##############
#i<-11
#data<-read.csv(file.path(sprintf("D:/0_PhD Work/6_DataAnalysis/Objective 0/2_Data_20_21/SD_20_21/SD%d.csv",i)))
#data[, paste0("HSy_7_1", LETTERS[1:12])] <- lapply(data[, paste0("HSy_7_1", LETTERS[1:12])], as.factor)
# since it is a binary variable we can not take mean values of multiple datasets
# generated using mice, therefore we will generate only one data set, m=1
#MD <- mice(data, m=1, maxit=50, method="logreg", pred= quickpred(data), 
#           seed = 23189, printFlag = FALSE)
#df<-complete(MD,1)
#write.csv(df,'demo.csv')

################################################################################
# NOw I want collate 2015-19 original and imputed indicators data with 2020-2021
# data. please note that even in the imputed data, we can see some of the indicators 
# 2020-2021 values were missing as the data were not available for these indicators
################################################################################
df1<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V2.csv'))
colnames(df1)
df1<-select(df1,c(1:79,91:92))
df1<-select(df1,-c("HSy_6_1_old", "HSy_7_1A", "HSy_7_1_old"))
df1<-pivot_longer(df1,9:78,names_to = 'index', values_to = 'value')

df2<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv'))
df2<-select(df2,c(1:61))
df2<-pivot_longer(df2,7:61,names_to = 'index', values_to = 'value')
colnames(df2)

df1<-select(df1, c(3:10))

df<-rbind(df1,df2)
df<-pivot_wider(df, names_from = 'index', values_from = 'value')
write.csv(df, '15-21 data.csv', row.names = F)
################################################################################
#
#                                  OBJECTIVE 1 
#
# To compute a composite index of Health System, Health Risk factors, Health 
# Service Coverage and Health Status (reflecting cumulative indices for 
# Fertility,Morbidity and Mortality) for selected HDI countries.
#                                   
################################################################################
#
#                   Index construction 2015-2019 data 
#                               
################################################################################
# I will use the same code to normalize both 2015-19 and 2020-21 data
# Because the benchmarks are same. However, i don't need run the entire code as 
# we do not have all the indicator values are available in the 2020-21 data
################################################################################
#             
#                                 15-04-2024
#
################################################################################
#
#                     STEP 1: Normalization of data
#
################################################################################
# load data for 2015-19 data
data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 0/1_Data_15_19/data_V2_IMP_15_19.csv"))

# load data for 2015-19 data
data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 0/2_Data_20_21/data_V2_IMP_20_21.csv"))

############///           Normalization: HS_1_1             \\\\\\\\############
############///        UL: 84.56 (Japan 2020)               \\\\\\\\############
############///        LL: 11.995  (Combodia 1975)          \\\\\\\\############

data$HS_1_1_N<-(data$HS_1_1-11.995)/(84.56-11.995)
summary(data$HS_1_1_N)

############///           Normalization: HS_1_2             \\\\\\\\############
############///        UL: 586.4427803 (Rwanda 1994)         \\\\\\\\###########
############///        LL: 1.172687085 (Luxembourg 2021)     \\\\\\\\###########

data$HS_1_2_N<-(586.4427803-data$HS_1_2)/(586.4427803-1.172687085)
summary(data$HS_1_2_N)

############///           Normalization: HS_1_3A (male)     \\\\\\\\############
############///            UL: 999.942 (Rwanda 1994)        \\\\\\\\\###########
############///        LL: 54.626 (Maldives 2021)           \\\\\\\\############

data$HS_1_3A_N<-(999.942-data$HS_1_3A)/(999.942-54.626)
summary(data$HS_1_3A_N)

############///           Normalization: HS_1_3B (male)     \\\\\\\\############
############///            UL: 973.382 (Combodia 1976)      \\\\\\\\############
############///    LL: 22.07 (Republic of Korea 2019)       \\\\\\\\############

data$HS_1_3B_N<-(973.382-data$HS_1_3B)/(973.382-22.07)
summary(data$HS_1_3B_N)

################################################################################
# NOTE: take the average of HS_1_3A_N and HS_1_3B_N to have a single indicator HS_1_3_N
data$HS_1_3_N<-(data$HS_1_3A_N+data$HS_1_3B_N)/2
summary(data$HS_1_3_N)

################################################################################

############///           Normalization: HS_1_4             \\\\\\\\############
############///       UL: 378.3097518 (Mali 1970)           \\\\\\\\\###########
############///    LL: 2.08832672 (Singapore 2021)          \\\\\\\\############

data$HS_1_4_N<-(378.3097518-data$HS_1_4)/(378.3097518-2.08832672)
summary(data$HS_1_4_N)

############///           Normalization: HS_1_5             \\\\\\\\############
############///            UL: 222.1178556 (Yemen 1970)     \\\\\\\\\###########
############///           LL: 1.728960224 (Singapore 2021)  \\\\\\\\############

data$HS_1_5_N<-(222.1178556-data$HS_1_5)/(222.1178556-1.728960224)
summary(data$HS_1_5_N)

############///           Normalization: HS_1_6             \\\\\\\\############
############///            UL: 97.45755802 (Mali 1970)      \\\\\\\\\###########
############///           LL: 0.747102018 (Singapore 2021)  \\\\\\\\############

data$HS_1_6_N<-(97.45755802-data$HS_1_6)/(97.45755802-0.747102018)
summary(data$HS_1_6_N)

############///           Normalization: HS_1_7             \\\\\\\\############
############///   UL: 45.94612087 (Guinea Bissau 2000)      \\\\\\\\############
############///           LL: 1.544647181 (Japan 2020)      \\\\\\\\############

data$HS_1_7_N<-(45.94612087-data$HS_1_7)/(45.94612087-1.544647181)
summary(data$HS_1_7_N)

############///           Normalization: HS_2_1             \\\\\\\\############
############///   UL: 6775 (South Sudan 1987)               \\\\\\\\############
############///   LL: 1.663741403 (Norway 2020)             \\\\\\\\############

data$HS_2_1_N<-(6775-data$HS_2_1)/(6775-1.663741403)
summary(data$HS_2_1_N)

############///           Normalization: HS_2_2             \\\\\\\\############
############///   UL: 126.2240681 (Nepal 2000)              \\\\\\\\############
############///   LL: 0.082351814 (Jordan 2020)             \\\\\\\\############

data$HS_2_2_N<-(126.2240681-data$HS_2_2)/(126.2240681-0.082351814)
summary(data$HS_2_2_N)

############///           Normalization: HS_2_3             \\\\\\\\############
############///   UL: 1002.712895 (Botswana 2002)           \\\\\\\\############
############///   LL: 0.073258353 (Bangladesh 2003)         \\\\\\\\############

data$HS_2_3_N<-(1002.712895-data$HS_2_3)/(1002.712895-0.073258353)
summary(data$HS_2_3_N)

############///           Normalization: HS_2_4             \\\\\\\\############
############///   UL: 355.6 (Burkina Fasco 2000)            \\\\\\\\############
############///   LL: 0 (Algeria 2020)                      \\\\\\\\############

data$HS_2_4_N<-(355.6-data$HS_2_4)/(355.6-0)
summary(data$HS_2_4_N)

############///           Normalization: HS_2_5 (%)         \\\\\\\\############
############///   UL: 100 (ideal low)                       \\\\\\\\############
############///   LL: 0 (desired goalpost)                  \\\\\\\\############

data$HS_2_5_N<-(100-data$HS_2_5)/(100-0)
summary(data$HS_2_5_N)

############///           Normalization: HS_2_8             \\\\\\\\############
############///   UL: 9.4 (Republic of Moldova 2003         \\\\\\\\############
############///   LL: 0 (Singapore 2019)                    \\\\\\\\############

data$HS_2_8_N<-(9.4-data$HS_2_8)/(9.4-0)
summary(data$HS_2_8_N)

############///           Normalization: HS_2_9             \\\\\\\\############
############///   UL: 116.2 (Lesotho 2014)                  \\\\\\\\############
############///   LL: 0 (Saint Vincent 2017)                \\\\\\\\############

data$HS_2_9_N<-(116.2-data$HS_2_9)/(116.2-0)
summary(data$HS_2_9_N)

############///           Normalization: HS_2_10            \\\\\\\\############
############///   UL: 64.6 (Dominican Republic 2019)        \\\\\\\\############
############///   LL: 0 (Micronesia 2005)                   \\\\\\\\############

data$HS_2_10_N<-(64.6-data$HS_2_10)/(64.6-0)
summary(data$HS_2_10_N)

############///           Normalization: HS_2_12            \\\\\\\\############
############///   UL: 98.3 (Colombia 2002)                  \\\\\\\\############
############///   LL: 0.2 (Singapore 2014)                  \\\\\\\\############

data$HS_2_12_N<-(98.3-data$HS_2_12)/(98.3-0.2)
summary(data$HS_2_12_N)

############///           Normalization: HS_3_1             \\\\\\\\############
############///   UL: 179.765 (Niger  2016)           \\\\\\\\############
############///   LL: 1.893 (Denmark 2020)        \\\\\\\\############

data$HS_3_1_N<-(179.765-data$HS_3_1)/(179.765-1.893)
summary(data$HS_3_1_N)

############///           Normalization: HS_3_2             \\\\\\\\############
############///   UL: 5.195 (Niger 2010)                    \\\\\\\\############
############///   LL: 0.003 (Nepal 2014)                    \\\\\\\\############

data$HS_3_2_N<-(5.195-data$HS_3_2)/(5.195-0.003)
summary(data$HS_3_2_N)

############///           Normalization: HS_4_1             \\\\\\\\############
############///   UL: 2217.459563 (Maldives 2005)           \\\\\\\\############
############///   LL: 0.007739905 (Algeria 2014)            \\\\\\\\############

data$HS_4_1_N<-(2217.459563-data$HS_4_1)/(2217.459563-0.007739905)
summary(data$HS_4_1_N)

############///           Normalization: HS_4_3             \\\\\\\\############
############///   UL: 29.8 (Eswatini 2016)                  \\\\\\\\############
############///   LL: 0 (desired goal post)                 \\\\\\\\############

data$HS_4_3_N<-(29.8-data$HS_4_3)/(29.8-0)
summary(data$HS_4_3_N)

############///           Normalization: HS_4_4             \\\\\\\\############
############///   UL: 26.2 (Botswana 1994)                  \\\\\\\\############
############///   LL: 0 (Afghanistan 1990)                  \\\\\\\\############

data$HS_4_4_N<-(26.2-data$HS_4_4)/(26.2-0)
summary(data$HS_4_4_N)

############///           Normalization: HS_4_7C            \\\\\\\\############
############///   UL: 14.63 (Vanuatu 2015)                  \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############


data$HS_4_7_N<-(14.63-data$HS_4_7)/(14.63-0)
summary(data$HS_4_7_N)

############///           Normalization: HS_4_9             \\\\\\\\############
############///   UL: 1590 (Eswatini 2010)                  \\\\\\\\############
############///   LL: 0 (Antigua and Barbuda 2019)          \\\\\\\\############

data$HS_4_9_N<-(1590-data$HS_4_9)/(1590-0)
summary(data$HS_4_9_N)

############///           Normalization: HS_4_12            \\\\\\\\############
############///   UL: 599.74821 (Burkina Faso 2001)         \\\\\\\\############
############///   LL: 0 (Iraq 2021)                         \\\\\\\\############

data$HS_4_12_N<-(599.74821-data$HS_4_12)/(599.74821-0)
summary(data$HS_4_12_N)

############///           Normalization: HR_1_1             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$HR_1_4_N<-(100-data$HR_1_4)/(100-0)
summary(data$HR_1_4_N)

############///           Normalization: HR_1_5             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$HR_1_5_N<-(100-data$HR_1_5)/(100-0)
summary(data$HR_1_5_N)

############///           Normalization: HR_1_6             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$HR_1_6_N<-(100-data$HR_1_6)/(100-0)
summary(data$HR_1_6_N)

############///           Normalization: HR_1_7             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$HR_1_7_N<-(100-data$HR_1_7)/(100-0)
summary(data$HR_1_7_N)

############///           Normalization: HR_1_8             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$HR_1_8_N<-(100-data$HR_1_8)/(100-0)
summary(data$HR_1_8_N)

############///           Normalization: HR_3_1             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

#****   it is a positive indicator

data$HR_3_1_N<-(data$HR_3_1-0)/(100-0)
summary(data$HR_3_1_N)

############///           Normalization: HR_3_2             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

#****   it is a positive indicator

data$HR_3_2_N<-(data$HR_3_2-0)/(100-0)
summary(data$HR_3_2_N) 

############///           Normalization: HR_3_3             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

#****   it is a positive indicator
data$HR_3_3_N<-(data$HR_3_3-0)/(100-0)
summary(data$HR_3_3_N)

############///           Normalization: HR_3_4             \\\\\\\\############
############///   UL: 72.18289 (Afghanistan 2013)           \\\\\\\\############
############///   LL: 5.37331 (Finland 2017)                 \\\\\\\\###########

data$HR_3_4_N<-(72.18289-data$HR_3_4)/(72.18289-5.37331)
summary(data$HR_3_4_N)

############///           Normalization: HR_4_1             \\\\\\\\############
############///   UL: 15.9 (Moldova 2006)                   \\\\\\\\############
############///   LL: 0 (Yemen 2015)                         \\\\\\\\###########

data$HR_4_1_N<-(15.9-data$HR_4_1)/(15.9-0)
summary(data$HR_4_1_N)

############///           Normalization: HR_4_2             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$HR_4_2_N<-(100-data$HR_4_2)/(100-0)
summary(data$HR_4_2_N)

############///           Normalization: HR_4_4             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$HR_4_4_N<-(100-data$HR_4_4)/(100-0)
summary(data$HR_4_4_N)

############///           Normalization: SC_1_1             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_1_1_N<-(data$SC_1_1-0)/(100-0)
summary(data$SC_1_1_N)

############///           Normalization: SC_1_2             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_1_2_N<-(data$SC_1_2-0)/(100-0)
summary(data$SC_1_2_N)

############///           Normalization: SC_1_3             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_1_3_N<-(data$SC_1_3-0)/(100-0)
summary(data$SC_1_3_N)

############///           Normalization: SC_1_4             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_1_4_N<-(data$SC_1_4-0)/(100-0)
summary(data$SC_1_4_N)

############///           Normalization: SC_1_9             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_1_9_N<-(data$SC_1_9-0)/(100-0)
summary(data$SC_1_9_N)

############///           Normalization: SC_2_1A            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_2_1A_N<-(data$SC_2_1A-0)/(100-0)
summary(data$SC_2_1A_N)

############///           Normalization: SC_2_1B            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_2_1B_N<-(data$SC_2_1B-0)/(100-0)
summary(data$SC_2_1B_N)

############///           Normalization: SC_2_1C            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_2_1C_N<-(data$SC_2_1C-0)/(100-0)
summary(data$SC_2_1C_N)

############///           Normalization: SC_2_1D            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_2_1D_N<-(data$SC_2_1D-0)/(100-0)
summary(data$SC_2_1D_N)

################################################################################
# NOTE: take the average of SC_2_1A_N: SC_2_1D to have a single indicator SC_2_1_N
data$SC_2_1_N<-(data$SC_2_1A_N+data$SC_2_1B_N+data$SC_2_1C_N+data$SC_2_1D_N)/4
summary(data$SC_2_1_N)

################################################################################

############///           Normalization: SC_3_1             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_3_1_N<-(data$SC_3_1-0)/(100-0)
summary(data$SC_3_1_N)

############///           Normalization: SC_3_2             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_3_2_N<-(data$SC_3_2-0)/(100-0)
summary(data$SC_3_2_N)

############///           Normalization: SC_3_3             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_3_3_N<-(data$SC_3_3-0)/(100-0)
summary(data$SC_3_3_N)

############///           Normalization: SC_4_2             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_4_2_N<-(data$SC_4_2-0)/(100-0)
summary(data$SC_4_2_N)

############///           Normalization: SC_4_3             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_4_3_N<-(data$SC_4_3-0)/(100-0)
summary(data$SC_4_3_N)

############///           Normalization: SC_5_1             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_5_1_N<-(data$SC_5_1-0)/(100-0)
summary(data$SC_5_1_N)

############///           Normalization: SC_5_2             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_5_2_N<-(data$SC_5_2-0)/(100-0)
summary(data$SC_5_2_N)

############///           Normalization: SC_7_1 (min-max)   \\\\\\\\############
#####///       UL: 117.0877164 (central african republic 2021)  \\\\############
############///   LL: 0 (Canada 2015)                       \\\\\\\\############

data$SC_7_1_N<-(117.0877164-data$SC_7_1)/(117.0877164-0)
summary(data$SC_7_1_N)

############///           Normalization: SC_11_1            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

# positive indicator
data$SC_11_1_N<-(data$SC_11_1-0)/(100-0)
summary(data$SC_11_1_N)

############///           Normalization: HSy_1_6            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

# positive indicator
data$HSy_1_6_N<-(data$HSy_1_6-0)/(100-0)
summary(data$HSy_1_6_N)

############///           Normalization: HSy_2_3            \\\\\\\\############
############///   UL: 146.9 (Japan 2000)                    \\\\\\\\############
############///   LL: 1 (Mali 2010)                         \\\\\\\\############

# positive indicator
data$HSy_2_3_N<-(data$HSy_2_3-1)/(146.9-1)
summary(data$HSy_2_3_N)

############///           Normalization: HSy_3_1A           \\\\\\\\############
############///   UL: 70.616 (Sweden 2020)                  \\\\\\\\############
############///   LL: 0.125 (Malawi 2013)                   \\\\\\\\############

# positive indicator
data$HSy_3_1A_N<-(data$HSy_3_1A-0.125)/(70.616-0.125)
summary(data$HSy_3_1A_N)

############///           Normalization: HSy_3_1B           \\\\\\\\############
############///   UL: 230.711 (Australia 1995)              \\\\\\\\############
############///   LL: 0.482 (Chad 1997)                     \\\\\\\\############

# positive indicator
data$HSy_3_1B_N<-(data$HSy_3_1B-0.482 )/(230.711-0.482)
summary(data$HSy_3_1B_N)

############///           Normalization: HSy_3_1C           \\\\\\\\############
############///   UL: 17.733 (Sweden 2020)                  \\\\\\\\############
############///   LL: 0.002 (Chad 2018)                     \\\\\\\\############

# positive indicator
data$HSy_3_1C_N<-(data$HSy_3_1C-0.002 )/(17.733-0.002)
summary(data$HSy_3_1C_N)

############///           Normalization: HSy_3_1D           \\\\\\\\############
############///   UL: 20.269 (Belgium 2021)                 \\\\\\\\############
############///   LL: 0.004 (Burundi 2012)                  \\\\\\\\############

# positive indicator
data$HSy_3_1D_N<-(data$HSy_3_1D-0.004 )/(20.269-0.004)
summary(data$HSy_3_1D_N)

################################################################################
# NOTE: take the average of HSy_3_1A_N: HSy_3_1D to have a single indicator HSy_3_1_N
data$HSy_3_1_N<-(data$HSy_3_1A_N+data$HSy_3_1B_N+data$HSy_3_1C_N+data$HSy_3_1D_N)/4
summary(data$HSy_3_1_N)

################################################################################

############///           Normalization: HSy_5_1            \\\\\\\\############
############///   UL: 21.83 (Afghanistan 2021)              \\\\\\\\############
############///   LL: 1.11 (Equatorial Guinea 2000)        \\\\\\\\############

# positive indicator
data$HSy_5_1_N<-(data$HSy_5_1-1.11 )/(21.83-1.11)
summary(data$HSy_5_1_N)

############///           Normalization: HSy_5_2            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

# positive indicator
data$HSy_5_2_N<-(data$HSy_5_2-0)/(100-0)
summary(data$HSy_5_2_N)


############///           Normalization: HSy_6_1           \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$HSy_6_1_N<-(data$HSy_6_1-0)/(100-0)
summary(data$HSy_6_1_N)

############///           Normalization: HSy_7_1           \\\\\\\\############
############///   UL: 2.5 (Desired goal post)               \\\\\\\\############
############///   LL: -2.5 (Desired goal post)              \\\\\\\\############
data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv"))
data$HSy_7_1_N<-(data$HSy_7_1+2.5)/5
summary(data$HSy_7_1_N)
?prcomp
################################################################################
# to export the normalized 2015-19 data 
write.csv(data,"data_V3_IC_15_19_V2.csv", row.names = F)

# to export the normalized 2020-19 data 
write.csv(data,"data_V3_IC_20_21.csv", row.names = F)

################################################################################
#
#                 STEP 2: Construction of sub-domain indices
#
################################################################################
# NOTE:
#
#-> We will be constructing sub-domain indices by determining the weights for
#    each indicator using the Principal component analysis (PCA).
#
#-> Although PCA is not appropriate for panel data, for the current study, we 
#    consider the panel data as a cross-sectional data with 500 observations.
#
#-> we will not be able to estimate all sub-domain indices of 2020-21 data, as 
#    the data for some of the sub-domains are not available.
#
#-> For those sub-domains with one or two indicators, we don't need to estimate 
#    weight. 
#           for single indicator sub-domain, weight = 1
#           for two indicator sub-domains, weight = 0.707106781 each indicators
################################################################################
# load data for 2015-19 data
data<-read.csv("data_V3_IC_15_19.csv")

# load data for 2015-19 data
data<-read.csv("data_V3_IC_20_21.csv")
# NOTE: we wont conduct PCA analysis with 2020-21 data, instead we will use the 
#       same weight determined with 2015-19 data
################################################################################
#              SI1: Health Status (Mortality by age)                           #
################################################################################
colnames(data)
#Selection of SI1 data
SI1<-data[,c("HS_1_1_N", "HS_1_2_N","HS_1_3_N", "HS_1_4_N", "HS_1_5_N", "HS_1_6_N",
             "HS_1_7_N")]
# to conduct PCA
pca_SI1<-prcomp(SI1, scale = T)

# to summarize the importance of PC1
summary(pca_SI1)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     2.5333 
# -> Proportion of Variance 0.9168
#************************************#

# to determine the weights of each indicators
pca_SI1

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HS_1_PCA <- 1- sqrt((0.3854671-0.3854671*data$HS_1_1_N)^2+
                              (0.3734399-0.3734399*data$HS_1_2_N)^2+
                              (0.3600743-0.3600743*data$HS_1_3_N)^2+
                              (0.3821838-0.3821838*data$HS_1_4_N)^2+
                              (0.3873406-0.3873406*data$HS_1_5_N)^2+
                              (0.3837701-0.3837701*data$HS_1_6_N)^2+
                              (0.3727300-0.3727300*data$HS_1_7_N)^2)
summary(data$SI_HS_1_PCA)

################################################################################
#              SI2: Health Status (Mortality by cause)                         #
################################################################################
# Selection of SI2 data
SI2<-data[,c("HS_2_1_N", "HS_2_2_N", "HS_2_3_N", "HS_2_4_N", "HS_2_5_N", "HS_2_8_N",
             "HS_2_9_N", "HS_2_10_N", "HS_2_12_N")]
# to conduct PCA
pca_SI2<-prcomp(SI2, scale = T)

# to summarize the importance of PC1
summary(pca_SI2)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     2.0745 
# -> Proportion of Variance 0.4782
#************************************#

# to determine the weights of each indicators
pca_SI2

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HS_2_PCA <- 1- sqrt((0.00798784-0.00798784*data$HS_2_1_N)^2+
                              (0.01477017-0.01477017*data$HS_2_2_N)^2+
                              (0.53460142-0.53460142*data$HS_2_3_N)^2+
                              (0.13950988-0.13950988*data$HS_2_4_N)^2+
                              (0.79679666-0.79679666*data$HS_2_5_N)^2+
                              (0.06760810-0.06760810*data$HS_2_8_N)^2+
                              (0.22359109-0.22359109*data$HS_2_9_N)^2+
                              (0.02159363-0.02159363*data$HS_2_10_N)^2+
                              (0.06738938-0.06738938*data$HS_2_12_N)^2)
summary(data$SI_HS_2_PCA)

################################################################################
#              SI3: Health Status (Fertility)                                  #
################################################################################
# Since Fertility sub-domain has only two indicator, the weight for each indicators
# will be equal and the value will be 0.707106781.
colnames(data)
# Selection of SI2 data
SI3<-data[,c("HS_3_1_N", "HS_3_2_N")]
# to conduct PCA
pca_SI3<-prcomp(SI3, scale = T)

# to summarize the importance of PC1
summary(pca_SI3)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.3103 
# -> Proportion of Variance 0.8585
#************************************#

# to determine the weights of each indicators
pca_SI3

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HS_3_PCA <- 1- sqrt((0.707106781-0.707106781*data$HS_3_1_N)^2+
                              (0.707106781-0.707106781*data$HS_3_2_N)^2)
summary(data$SI_HS_3_PCA)

################################################################################
#              SI4: Health Status (Morbidity)                                  #
################################################################################

colnames(data)
# Selection of SI2 data
SI4<-data[,c("HS_4_1_N", "HS_4_3_N", "HS_4_4_N", "HS_4_7_N", "HS_4_9_N", "HS_4_12_N")]
# to conduct PCA
pca_SI4<-prcomp(SI4, scale = T)

# to summarize the importance of PC1
summary(pca_SI4)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.686 
# -> Proportion of Variance 0.474
#************************************#

# to determine the weights of each indicators
pca_SI4

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HS_4_PCA <- 1- sqrt((0.1199021-0.1199021*data$HS_4_1_N)^2+
                              (0.5065450-0.5065450*data$HS_4_3_N)^2+
                              (0.5359376-0.5359376*data$HS_4_4_N)^2+
                              (0.3490016-0.3490016*data$HS_4_7_N)^2+
                              (0.4915818-0.4915818*data$HS_4_9_N)^2+
                              (0.2799138-0.2799138*data$HS_4_12_N)^2)
summary(data$SI_HS_4_PCA)
################################################################################
#              SI5: Health risk factors (Nutrition)                            #
################################################################################

colnames(data)
# Selection of SI2 data
SI5<-data[,c("HR_1_4_N", "HR_1_5_N", "HR_1_6_N", "HR_1_7_N", "HR_1_8_N")]
# to conduct PCA
pca_SI5<-prcomp(SI5, scale = T)

# to summarize the importance of PC1
summary(pca_SI5)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.9181 
# -> Proportion of Variance 0.7359
#************************************#

# to determine the weights of each indicators
pca_SI5

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HR_1_PCA <- 1- sqrt((0.4357465-0.4357465*data$HR_1_4_N)^2+
                              (0.4438000-0.4438000*data$HR_1_5_N)^2+
                              (0.3912305-0.3912305*data$HR_1_6_N)^2+
                              (0.4827125-0.4827125*data$HR_1_7_N)^2+
                              (0.4765436-0.4765436*data$HR_1_8_N)^2)
summary(data$SI_HR_1_PCA)  
################################################################################
#              SI6: Health risk factors (Environmental risk factors)           #
################################################################################

colnames(data)
# Selection of SI2 data
SI6<-data[,c("HR_3_1_N","HR_3_2_N","HR_3_3_N","HR_3_4_N")]
# to conduct PCA
pca_SI6<-prcomp(SI6, scale = T)

# to summarize the importance of PC1
summary(pca_SI6)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.7448 
# -> Proportion of Variance 0.7611
#************************************#

# to determine the weights of each indicators
pca_SI6

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HR_3_PCA <- 1- sqrt((0.5470390-0.5470390*data$HR_3_1_N)^2+
                              (0.5287983-0.5287983*data$HR_3_2_N)^2+
                              (0.5206372-0.5206372*data$HR_3_3_N)^2+
                              (0.3873726-0.3873726*data$HR_3_4_N)^2)
summary(data$SI_HR_3_PCA)

################################################################################
#              SI7: Health risk factors (NCDs)                                 #
################################################################################

colnames(data)
# Selection of SI2 data
SI7<-data[,c("HR_4_1_N","HR_4_2_N","HR_4_4_N")]
# to conduct PCA
pca_SI7<-prcomp(SI7, scale = T)

# to summarize the importance of PC1
summary(pca_SI7)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.2564 
# -> Proportion of Variance 0.5262
#************************************#

# to determine the weights of each indicators
pca_SI7

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HR_4_PCA <- 1- sqrt((0.5723900-0.5723900*data$HR_4_1_N)^2+
                              (0.5176766-0.5176766*data$HR_4_2_N)^2+
                              (0.6359093-0.6359093*data$HR_4_4_N)^2)
summary(data$SI_HR_4_PCA)
################################################################################
#              SI8: Service coverage (RMNCH)                                   #
################################################################################

colnames(data)
# Selection of SI2 data
SI8<-data[,c("SC_1_1_N","SC_1_2_N","SC_1_3_N","SC_1_4_N","SC_1_9_N")]
# to conduct PCA
pca_SI8<-prcomp(SI8, scale = T)

# to summarize the importance of PC1
summary(pca_SI8)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.7813 
# -> Proportion of Variance 0.6346
#************************************#

# to determine the weights of each indicators
pca_SI8

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_1_PCA <- 1- sqrt((0.512693704-0.512693704*data$SC_1_1_N)^2+
                              (0.516385379-0.516385379*data$SC_1_2_N)^2+
                              (0.486367404-0.486367404*data$SC_1_3_N)^2+
                              (0.483655421-0.483655421*data$SC_1_4_N)^2+
                              (0.003935419-0.003935419*data$SC_1_9_N)^2)
summary(data$SI_SC_1_PCA)

################################################################################
#              SI9: Service coverage (Immunization)                            #
################################################################################
# Since there is only  one indicator (SC_2_1_N), this consider as  the sub-domain
# index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_2_PCA<- data$SC_2_1_N
summary(data$SI_SC_2_PCA)

################################################################################
#              SI10: Service coverage (HIV)                                    #
################################################################################

colnames(data)
# Selection of SI2 data
SI10<-data[,c("SC_3_1_N","SC_3_2_N","SC_3_3_N")]
# to conduct PCA
pca_SI10<-prcomp(SI10, scale = T)

# to summarize the importance of PC1
summary(pca_SI10)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.6223 
# -> Proportion of Variance 0.8773
#************************************#

# to determine the weights of each indicators
pca_SI10

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_3_PCA <- 1- sqrt((0.5960719-0.5960719*data$SC_3_1_N)^2+
                              (0.5494444-0.5494444*data$SC_3_2_N)^2+
                              (0.5854990-0.5854990*data$SC_3_3_N)^2)
summary(data$SI_SC_3_PCA)

################################################################################
#              SI11: Service coverage (HIV/TB)                                 #
################################################################################

colnames(data)
# Selection of SI2 data
SI11<-data[,c("SC_4_2_N","SC_4_3_N")]
# to conduct PCA
pca_SI11<-prcomp(SI11, scale = T)

# to summarize the importance of PC1
summary(pca_SI11)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.1137 
# -> Proportion of Variance 0.6202
#************************************#

# Since HIV/TB sub-domain has only two indicator, the weight for each indicators
# will be equal and the value will be 0.707106781.

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_4_PCA <- 1- sqrt((0.707106781-0.707106781*data$SC_4_2_N)^2+
                              (0.707106781-0.707106781*data$SC_4_3_N)^2)
summary(data$SI_SC_4_PCA)

################################################################################
#              SI12: Service coverage (TB)                                     #
################################################################################

colnames(data)
# Selection of SI2 data
SI12<-data[,c("SC_5_1_N","SC_5_2_N")]
# to conduct PCA
pca_SI12<-prcomp(SI12, scale = T)

# to summarize the importance of PC1
summary(pca_SI12)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.2044 
# -> Proportion of Variance 0.7253
#************************************#

# Since TB sub-domain has only two indicator, the weight for each indicators
# will be equal and the value will be 0.707106781.

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_5_PCA <- 1- sqrt((0.707106781-0.707106781*data$SC_5_1_N)^2+
                              (0.707106781-0.707106781*data$SC_5_2_N)^2)
summary(data$SI_SC_5_PCA)

################################################################################
#              SI13: Service coverage (NTD)                                     #
################################################################################
# Since there is only  one indicator (SC_7_1_N), this consider as  the sub-domain
# index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_7_PCA<- data$SC_7_1_N

################################################################################
#              SI14: Service coverage (Essential health services)              #
################################################################################
# Since there is only  one indicator (SC_11_1_N), this consider as  the sub-domain
# index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_11_PCA<- data$SC_11_1_N

################################################################################
#              SI15: Health system (Quality and safety of care)                #
################################################################################
# Since there is only  one indicator (HSy_1_6_N), this consider as  the sub-domain
# index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HSy_1_PCA<- data$HSy_1_6_N

################################################################################
#              SI16: Health system (utilization and access)                    #
################################################################################
# Since there is only  one indicator (HSy_2_3_N), this consider as  the sub-domain
# index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HSy_2_PCA <- data$HSy_2_3_N
summary(data$SI_HSy_2_PCA)

################################################################################
#              SI17: Health system (Health work force)                         #
################################################################################
# Since there is only  one indicator (HSy_3_1_N), this consider as  the sub-domain
# index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HSy_3_PCA <- data$HSy_3_1_N
summary(data$SI_HSy_3_PCA)

################################################################################
#              SI18: Health System (Health financing)                       #
################################################################################

colnames(data)
# Selection of SI2 data
SI18<-data[,c("HSy_5_1_N","HSy_5_2_N")]
# to conduct PCA
pca_SI18<-prcomp(SI18, scale = T)

# to summarize the importance of PC1
summary(pca_SI18)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.0042 
# -> Proportion of Variance 0.5042
#************************************#

# Since Health financing sub-domain has only two indicator, the weight for each 
# indicators will be equal and the value will be 0.707106781.

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HSy_5_PCA <- 1- sqrt((0.7071068-0.7071068*data$HSy_5_1_N)^2+
                               (0.7071068-0.7071068*data$HSy_5_2_N)^2)
summary(data$SI_HSy_5_PCA)
################################################################################
#              SI19: Health system (Health security)                           #
################################################################################
# Since there is only  one indicator (HSy_6_1_N), this consider as  the sub-domain
# index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HSy_6_PCA <- data$HSy_6_1_N
summary(data$SI_HSy_6_PCA)

################################################################################
#              SI20: Health system (Health governance)                         #
################################################################################
data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19.csv'))
# Since there is only  one indicator (HSy_7_1_N), this consider as  the sub-domain
# index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HSy_7_PCA <- data$HSy_7_1_N
summary(data$SI_HSy_7_PCA)

################################################################################
#
#                 STEP 3: Construction of composite indices
#
################################################################################
# NOTE:
#
#-> We will be constructing composite indices by determining the weights for
#    each sub-domain indices using the Principal component analysis (PCA).
#
#-> Although PCA is not appropriate for panel data, for the current study, we 
#    consider the panel data as a cross-sectional data with 500 observations.
#
#-> we will not be able to estimate any composite indices of 2020-21 data 
#   (as of 16-04-2024),as the data for constructing composite indices are not 
#   available completely.
#
################################################################################
#              D1: Health status (mortality by age)                            #
################################################################################

colnames(data)
# Selection of D1 data
D1<-data[,c("SI_HS_1_PCA","SI_HS_3_PCA","SI_HS_4_PCA")]
# to conduct PCA
pca_D1<-prcomp(D1, scale = T)

# to summarize the importance of PC1
summary(pca_D1)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.5452 
# -> Proportion of Variance 0.7958
#************************************#

# to determine the weights of each indicators
pca_D1

# Calculation of Sub-domain indices using inverse euclidean formula
data$D_HS_MA_PCA <- 1- sqrt((0.6117514-0.6117514*data$SI_HS_1_PCA)^2+
                              (0.5770716-0.5770716*data$SI_HS_3_PCA)^2+
                              (0.5410625-0.5410625*data$SI_HS_4_PCA)^2)

summary(data$D_HS_MA_PCA)

################################################################################
#              D2: Health status (mortality by cause)                          #
################################################################################
colnames(data)
# Selection of D2 data
D2<-data[,c("SI_HS_2_PCA","SI_HS_3_PCA","SI_HS_4_PCA")]
# to conduct PCA
pca_D2<-prcomp(D2, scale = T)

# to summarize the importance of PC1
summary(pca_D2)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.4474 
# -> Proportion of Variance 0.6984
#************************************#

# to determine the weights of each indicators
pca_D2

# Calculation of Sub-domain indices using inverse euclidean formula
data$D_HS_MC_PCA <- 1- sqrt((0.5751774-0.5751774*data$SI_HS_2_PCA)^2+
                              (0.5376787-0.5376787*data$SI_HS_3_PCA)^2+
                              (0.6165003-0.6165003*data$SI_HS_4_PCA)^2)

summary(data$D_HS_MC_PCA)

################################################################################
#              D3: Health risk factors                                         #
################################################################################
colnames(data)
# Selection of D3 data
D3<-data[,c("SI_HR_1_PCA","SI_HR_3_PCA","SI_HR_4_PCA")]
# to conduct PCA
pca_D3<-prcomp(D3, scale = T)

# to summarize the importance of PC1
summary(pca_D3)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.6301 
# -> Proportion of Variance 0.8857
#************************************#

# to determine the weights of each indicators
pca_D3

# Calculation of Sub-domain indices using inverse euclidean formula
data$D_HR_PCA <- 1- sqrt((0.5814444-0.5814444*data$SI_HR_1_PCA)^2+
                           (0.5860110-0.5860110*data$SI_HR_3_PCA)^2+
                           (0.5643700-0.5643700*data$SI_HR_4_PCA)^2)
summary(data$D_HR_PCA)

################################################################################
#              D4: Health service coverage                                     #
################################################################################
colnames(data)
# Selection of D4 data
D4<-data[,c("SI_SC_1_PCA", "SI_SC_2_PCA","SI_SC_3_PCA","SI_SC_4_PCA","SI_SC_5_PCA", 
            "SI_SC_7_PCA", "SI_SC_11_PCA")]
# to conduct PCA
pca_D4<-prcomp(D4, scale = T)

# to summarize the importance of PC1
summary(pca_D4)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.959 
# -> Proportion of Variance 0.548
#************************************#

# to determine the weights of each indicators
pca_D4

# Calculation of Sub-domain indices using inverse euclidean formula
data$D_SC_PCA <- 1- sqrt((0.44749765-0.44749765*data$SI_SC_1_PCA)^2+
                           (0.35583625-0.35583625*data$SI_SC_2_PCA)^2+
                           (0.35842270-0.35842270*data$SI_SC_3_PCA)^2+
                           (0.01005207-0.01005207*data$SI_SC_4_PCA)^2+
                           (0.39200108-0.39200108*data$SI_SC_5_PCA)^2+
                           (0.40485790-0.40485790*data$SI_SC_7_PCA)^2+
                           (0.47642814-0.47642814*data$SI_SC_11_PCA)^2)
summary(data$D_SC_PCA)

################################################################################
#              D5: Health System - version 1 
# Health System Index created using the new health governance and security indicators
################################################################################

#data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V2.csv'))
#colnames(data)
# Selection of D4 data
#D5<-data[,c("SI_HSy_1_PCA","SI_HSy_2_PCA","SI_HSy_3_PCA","SI_HSy_5_PCA",
#            "SI_HSy_6_PCA","SI_HSy_7_PCA")]


# to conduct PCA
#pca_D5<-prcomp(D5, scale = T)

# to summarize the importance of PC1
#summary(pca_D5)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.8841 
# -> Proportion of Variance 0.5917
#************************************#

# to determine the weights of each indicators
#pca_D5

# Calculation of Sub-domain indices using inverse euclidean formula

#data$D_HSy_PCA <- 1- sqrt((0.2326198-0.2326198*data$SI_HSy_1_PCA)^2+
#                            (0.3804321-0.3804321*data$SI_HSy_2_PCA)^2+
#                            (0.4913108-0.4913108*data$SI_HSy_3_PCA)^2+
#                            (0.3941873-0.3941873*data$SI_HSy_5_PCA)^2+
#                            (0.4200929-0.4200929*data$SI_HSy_6_PCA)^2+
#                            (0.4774007-0.4774007*data$SI_HSy_7_PCA)^2)
#summary(data$D_HSy_PCA)

#write.csv(data, file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V2.csv'),row.names = F)
################################################################################
#              D5: Health System - version 2                                   #
################################################################################
#data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19.csv'))
#colnames(data)
# Selection of D5 data
#D5<-data[,c("SI_HSy_1_PCA","SI_HSy_2_PCA","SI_HSy_3_PCA","SI_HSy_5_PCA",
#            "SI_HSy_6_PCA")]
# to conduct PCA
#pca_D5<-prcomp(D5, scale = T)

# to summarize the importance of PC1
#summary(pca_D5)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.6697 
# -> Proportion of Variance 0.5576
#************************************#

# to determine the weights of each indicators
#pca_D5

# Calculation of Sub-domain indices using inverse euclidean formula

#data$D_HSy_PCA_V2 <- 1- sqrt((0.2756664-0.2756664*data$SI_HSy_1_PCA)^2+
#                            (0.4454970-0.4454970*data$SI_HSy_2_PCA)^2+
#                            (0.5466870-0.5466870*data$SI_HSy_3_PCA)^2+
#                            (0.4573801-0.4573801*data$SI_HSy_5_PCA)^2+
#                            (0.4663446-0.4663446*data$SI_HSy_6_PCA)^2)
#summary(data$D_HSy_PCA_V2)

#write.csv(data, file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19.csv'), row.names = F)

################################################################################
#              D5: Health System - version 3- excluding health security components from the health system index #
################################################################################
library(tidyverse)
data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv'))
colnames(data)
# Selection of D5 data
D5<-data[,c("SI_HSy_1_PCA","SI_HSy_2_PCA","SI_HSy_3_PCA","SI_HSy_5_PCA",
            "SI_HSy_7_PCA")]
# to conduct PCA
pca_D5<-prcomp(D5, scale = T)

# to summarize the importance of PC1
summary(pca_D5)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.734
# -> Proportion of Variance 0.601
#************************************#

# to determine the weights of each indicators
pca_D5

# Calculation of Sub-domain indices using inverse euclidean formula

data$D_HSy_PCA <- 1- sqrt((0.2775661-0.2775661*data$SI_HSy_1_PCA)^2+
                               (0.4260045-0.4260045*data$SI_HSy_2_PCA)^2+
                               (0.5354086-0.5354086*data$SI_HSy_3_PCA)^2+
                               (0.4384296-0.4384296*data$SI_HSy_5_PCA)^2+
                               (0.5124397-0.5124397*data$SI_HSy_7_PCA)^2)
summary(data$D_HSy_PCA)

write.csv(data, file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv'), row.names = F)


################################################################################
#              D5: Health System - version 4- removing worker density and seccurity                                   #
################################################################################
#data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V2.csv'))
#colnames(data)
#Selection of D5 data
#D5<-data[,c("SI_HSy_1_PCA","SI_HSy_2_PCA","SI_HSy_5_PCA",
#            "SI_HSy_7_PCA")]
# to conduct PCA
#pca_D5<-prcomp(D5, scale = T)

# to summarize the importance of PC1
#summary(pca_D5)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.4838
# -> Proportion of Variance 0.5504
#************************************#

# to determine the weights of each indicators
#pca_D5

# Calculation of Sub-domain indices using inverse euclidean formula

#data$D_HSy_PCA_V4 <- 1- sqrt((0.3567286-0.3567286*data$SI_HSy_1_PCA)^2+
#                               (0.5077500-0.5077500*data$SI_HSy_2_PCA)^2+
#                               (0.5234103-0.5234103*data$SI_HSy_5_PCA)^2+
#                               (0.5839318-0.5839318*data$SI_HSy_7_PCA)^2)
#summary(data$D_HSy_PCA_V4)
#
#write.csv(data, file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv'), row.names = F)


################################################################################
####                                                                       #####
####  Descriptive for OBJECTIVE 1                                          #####
####  --> here i have written the code for health system index description #####
####  --> So that I can always modify the code to find the descriptives of #####
####      other domain indices and for the period 2020-2021                #####
################################################################################
# Table 1: Description of the performance of HDI groups in underlying indicators 
# of Health Service Coverage (HSC)
################################################################################
# load data: Every time, data should load from Ph.D related folders. 
#            Data file name is data_V3_IC_15_19_V2, and data_V3_IC_15_19_V2 in which we 
#            we will get, imputed data, normalized indicator data, sub-domain 
#            and composite indices for all the variables in my phd. for the 
#            present analysis, we will extract related data for 'Health Service 
#            Coverage' domain
################################################################################
library(tidyverse)
data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv'))
colnames(data)
df<-select(data, c("HC",which(colnames(data) == "SC_1_1_N"):which(colnames(data) == "SC_11_1_N")))
df<-aggregate(df, by = list(data$HC), FUN = "mean")
colnames(df)
df<-select(df, -HC)
df<-pivot_longer(df,cols = 2:ncol(df),names_to = "indicator", values_to = "value")
df<-pivot_wider(df, names_from = "Group.1", values_from = "value")
write.csv(df,'table1.csv')
################################################################################
# Figure 2: Distribution of countries across different HDI levels and HST levels
# for the period 2015-2019
################################################################################
library(tidyverse)
data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv'))
colnames(data)
df<-select(data, c("C","YN","HC","D_HSy_PCA"))

df$Index<-ifelse(df$D_HSy_PCA>=0.8,"VH",
               ifelse(df$D_HSy_PCA>=0.7, "H",
                      ifelse(df$D_HSy_PCA>=0.55,"M","L")))

# Cross-tabulation for each year for HSYI
for (yr in unique(df$YN)) {
  cat("\n Cross-tabulation for year:", yr, "\n")
  print(table(df[df$YN == yr, c("HC","Index")]))
}

################################################################################
# Table 2: Description of Composite and sub-indices of health status across HDI 
# groups between the period 2015-19
################################################################################
data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv'))
colnames(data)
df<-select(data, c("YN","HC",
                   which(colnames(data)=="SI_SC_1_PCA"): which(colnames(data)=="SI_SC_11_PCA"),
                   "D_SC_PCA"))
df$HC<-as.factor(df$HC)
df<-aggregate(x = df, by = list(df$YN, df$HC), FUN = "mean")
df<-pivot_longer(df, cols = 5:ncol(df),names_to = "index", values_to = "value")
df<-select(df, c("YN","Group.2","index","value"))
df<-pivot_wider(df,names_from = "YN", values_from = "value")
write.csv(df,'table2.csv')

################################################################################
# Table 3: sub-domain wise and overall HST-MA and HST-MC by countries 
# (top 10 and bottom 10 countries in the year 2015 and 2019)
################################################################################
colnames(data)
# for HST-MA
data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv'))
df<-select(data, c("C","YN","HC",
                   which(colnames(data)=="SI_SC_1_PCA"): which(colnames(data)=="SI_SC_11_PCA"),
                   "D_SC_PCA"))

df1<-df[df$YN==2015,]
df1 <- df1[order(df1$D_SC_PCA, decreasing = TRUE), ]
df1<-df1[c(1:10, 91:100),]
df1$group <- ifelse(1:nrow(df1) <= 10, 1, ifelse(1:nrow(df1) <= 20, 2, NA))

df2<-df[df$YN==2019,]
df2 <- df2[order(df2$D_SC_PCA, decreasing = TRUE), ]
df2<-df2[c(1:10, 91:100),]
df2$group <- ifelse(1:nrow(df2) <= 10, 1, ifelse(1:nrow(df2) <= 20, 2, NA))

data<-rbind(df1,df2) %>% arrange(group)

write.csv(data,'table3.csv')


################################################################################
################################################################################
##########***************###################**********************##############
################################################################################
###########
##########**
##########*          Imputation of 2020-21 data
##########*
##########*
##########*
##########*
##########*
##########*
##########*
##########*#####################################################################
################################################################################
##########***************###################**********************##############
##########*
##########*
##########*
##########*
##########*
##########*
##########*
##########*
##########*
##########*
##########*
##########*##########*#####################################################################
################################################################################
##########***************###################**********************##############
library(tidyverse)
library(mice)
#to prepare sub-domain wise data 2015-19
data<-read.csv("D:/0_PhD Work/6_DataAnalysis/Objective 0/2_Data_20_21/data_V1_O_20_21_V2.csv")
summary(data)
# we need to separate the data based on the sub-domain.
# Note: I have excluded those subdomains without any missing value because we dont need to impute them
# Sub-domains without missing values:
#                                   HS_3_1: HS_3_2
#                                   SC_7_1
#                                   HS_3_1: HS_3_2
# I have numbered the sub-domain by excluding the above sub-domain
# i will read about how to impute categorical variables in R using mice package
mean(is.na(data$HSy_6_1))
SD1<-select(data, 2,5:11, HS_1_1:HS_1_7)
SD2<-select(data, 2,5:11, HS_2_1:HS_2_4)
SD3<-select(data, 2,5:11, HS_4_1:HS_4_12)
SD4<-select(data, 2,5:11, HR_1_4:HR_1_6)
SD5<-select(data, 2,5:11, HR_3_1:HR_3_3)
SD6<-select(data, 2,5:11, HR_4_2)
SD7<-select(data, 2,5:11, SC_1_2:SC_1_9)
SD8<-select(data, 2,5:11, SC_2_1A:SC_2_1D)
SD9<-select(data, 2,5:11, SC_3_1:SC_3_3)
SD10<-select(data, 2,5:11, SC_4_2:SC_4_3)
SD11<-select(data, 2,5:11, SC_5_1:SC_5_2)
SD12<-select(data, 2,5:11, SC_11_1)
SD13<-select(data, 2,5:11, HSy_1_6)
SD14<-select(data, 2,5:11, HSy_3_1A:HSy_3_1D)
SD15<-select(data, 2,5:11, HSy_5_1:HSy_5_2)
SD16<-select(data, 2,5:11, HSy_6_1)
SD17<-select(data, 2,5:11, HSy_7_1)
summary(SD16)
# i am going to save the sub-domain wise 2020-21 data to a folder named SD_20_21, so that i can use that later to impute
for (i in 1:17) {
  write.csv(get(sprintf("SD%d",i)),sprintf("D:/0_PhD Work/6_DataAnalysis/Objective 0/2_Data_20_21/SD_20_21/SD%d.csv",i),row.names = F)
}

##########***************###################**********************##############
# here we are trying to impute continuous variables and preferred imputation method 
# is "pmm" (predictive mean matching)
##########***************###################**********************##############
data_V2<-list()
i<-1
for (i in 2:17) {
  data<-read.csv(file.path(sprintf("D:/0_PhD Work/6_DataAnalysis/Objective 0/2_Data_20_21/SD_20_21/SD%d.csv",i)))   
  #to find the missing values
  MP<-mean(is.na(data[,-(1:8)]))
  # i decided to generate 5 datasets if the missing proportion <=0.1, 
  #                      10 datasets if the missing proportion <=0.2,
  #                      15 datasets if the missing proportion <=0.3,
  #                      20 datasets if the missing proportion >0.3,
  nmd <-ifelse (MP<=0.1,5,
                ifelse(MP<=0.2,10,
                       ifelse(MP<=0.3,15,20)))
  MD <- mice(data, m=nmd, maxit=50, method="pmm", pred= quickpred(data), 
             seed = 23189, printFlag = FALSE)
  MD_data<-list()
  for (j in 1:nmd){
    imp <-complete(MD, j)
    MD_data [[j]]<-imp
  }
  # Combine all data frames in the list into one data frame
  combined_data <- do.call(rbind, MD_data)
  aggregated_result <- aggregate(. ~ CC + YC, data = combined_data, FUN = mean)
  data_V2[[i]]<-aggregated_result[,-(1:8)]
}
# Combine all data frames in the list into one data frame
df <- bind_cols(data_V2)
data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 0/2_Data_20_21/data_V2_IMP_20_21_V1.csv"))
df1<-left_join(data, df, by = c("CC","YC"))
write.csv(df, "data.csv")


################################################################################
#
#                                  OBJECTIVE 1 
#
# To compute a composite index of Health System, Health Risk factors, Health 
# Service Coverage and Health Status (reflecting cumulative indices for 
# Fertility,Morbidity and Mortality) for selected HDI countries.
#                                   
################################################################################
#
#                   Index construction 2015-2019 data 
#                               
################################################################################
# I will use the same code to normalize both 2015-19 and 2020-21 data
# Because the benchmarks are same. However, i don't need run the entire code as 
# we do not have all the indicator values are available in the 2020-21 data
################################################################################
#             
#                                 15-04-2024
#
################################################################################
#
#                     STEP 1: Normalization of data
#
################################################################################
# load data for 2015-19 data
data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 0/1_Data_15_19/data_V2_IMP_15_19.csv"))

# load data for 2015-19 data
data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 0/2_Data_20_21/data_V2_IMP_20_21_V1.csv"))
min(data$HSy_3_1B)
############///           Normalization: HS_1_1             \\\\\\\\############
############///        UL: 84.56 (Japan 2020)               \\\\\\\\############
############///        LL: 11.995  (Combodia 1975)          \\\\\\\\############

data$HS_1_1_N<-(data$HS_1_1-11.995)/(84.56-11.995)
summary(data$HS_1_1_N)

############///           Normalization: HS_1_2             \\\\\\\\############
############///        UL: 586.4427803 (Rwanda 1994)         \\\\\\\\###########
############///        LL: 1.172687085 (Luxembourg 2021)     \\\\\\\\###########

data$HS_1_2_N<-(586.4427803-data$HS_1_2)/(586.4427803-1.172687085)
summary(data$HS_1_2_N)

############///           Normalization: HS_1_3A (male)     \\\\\\\\############
############///            UL: 999.942 (Rwanda 1994)        \\\\\\\\\###########
############///        LL: 54.626 (Maldives 2021)           \\\\\\\\############

data$HS_1_3A_N<-(999.942-data$HS_1_3A)/(999.942-54.626)
summary(data$HS_1_3A_N)

############///           Normalization: HS_1_3B (male)     \\\\\\\\############
############///            UL: 973.382 (Combodia 1976)      \\\\\\\\############
############///    LL: 22.07 (Republic of Korea 2019)       \\\\\\\\############

data$HS_1_3B_N<-(973.382-data$HS_1_3B)/(973.382-22.07)
summary(data$HS_1_3B_N)

################################################################################
# NOTE: take the average of HS_1_3A_N and HS_1_3B_N to have a single indicator HS_1_3_N
data$HS_1_3_N<-(data$HS_1_3A_N+data$HS_1_3B_N)/2
summary(data$HS_1_3_N)

################################################################################

############///           Normalization: HS_1_4             \\\\\\\\############
############///       UL: 378.3097518 (Mali 1970)           \\\\\\\\\###########
############///    LL: 2.08832672 (Singapore 2021)          \\\\\\\\############

data$HS_1_4_N<-(378.3097518-data$HS_1_4)/(378.3097518-2.08832672)
summary(data$HS_1_4_N)

############///           Normalization: HS_1_5             \\\\\\\\############
############///            UL: 222.1178556 (Yemen 1970)     \\\\\\\\\###########
############///           LL: 1.728960224 (Singapore 2021)  \\\\\\\\############

data$HS_1_5_N<-(222.1178556-data$HS_1_5)/(222.1178556-1.728960224)
summary(data$HS_1_5_N)

############///           Normalization: HS_1_6             \\\\\\\\############
############///            UL: 97.45755802 (Mali 1970)      \\\\\\\\\###########
############///           LL: 0.747102018 (Singapore 2021)  \\\\\\\\############

data$HS_1_6_N<-(97.45755802-data$HS_1_6)/(97.45755802-0.747102018)
summary(data$HS_1_6_N)

############///           Normalization: HS_1_7             \\\\\\\\############
############///   UL: 45.94612087 (Guinea Bissau 2000)      \\\\\\\\############
############///           LL: 1.544647181 (Japan 2020)      \\\\\\\\############

data$HS_1_7_N<-(45.94612087-data$HS_1_7)/(45.94612087-1.544647181)
summary(data$HS_1_7_N)

############///           Normalization: HS_2_1             \\\\\\\\############
############///   UL: 6775 (South Sudan 1987)               \\\\\\\\############
############///   LL: 1.663741403 (Norway 2020)             \\\\\\\\############
colnames(data)
data$HS_2_1_N<-(6775-data$HS_2_1)/(6775-1.663741403)
summary(data$HS_2_1_N)

############///           Normalization: HS_2_2             \\\\\\\\############
############///   UL: 126.2240681 (Nepal 2000)              \\\\\\\\############
############///   LL: 0.082351814 (Jordan 2020)             \\\\\\\\############

data$HS_2_2_N<-(126.2240681-data$HS_2_2)/(126.2240681-0.082351814)
summary(data$HS_2_2_N)

############///           Normalization: HS_2_3             \\\\\\\\############
############///   UL: 1002.712895 (Botswana 2002)           \\\\\\\\############
############///   LL: 0.073258353 (Bangladesh 2003)         \\\\\\\\############

data$HS_2_3_N<-(1002.712895-data$HS_2_3)/(1002.712895-0.073258353)
summary(data$HS_2_3_N)

############///           Normalization: HS_2_4             \\\\\\\\############
############///   UL: 355.6 (Burkina Fasco 2000)            \\\\\\\\############
############///   LL: 0 (Algeria 2020)                      \\\\\\\\############

data$HS_2_4_N<-(355.6-data$HS_2_4)/(355.6-0)
summary(data$HS_2_4_N)
write.csv(data, file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv'))
############///           Normalization: HS_3_1             \\\\\\\\############
############///   UL: 179.765 (Niger  2016)           \\\\\\\\############
############///   LL: 1.893 (Denmark 2020)        \\\\\\\\############

data$HS_3_1_N<-(179.765-data$HS_3_1)/(179.765-1.893)
summary(data$HS_3_1_N)

############///           Normalization: HS_3_2             \\\\\\\\############
############///   UL: 5.195 (Niger 2010)                    \\\\\\\\############
############///   LL: 0.003 (Nepal 2014)                    \\\\\\\\############

data$HS_3_2_N<-(5.195-data$HS_3_2)/(5.195-0.003)
summary(data$HS_3_2_N)

############///           Normalization: HS_4_1             \\\\\\\\############
############///   UL: 2217.459563 (Maldives 2005)           \\\\\\\\############
############///   LL: 0.007739905 (Algeria 2014)            \\\\\\\\############

data$HS_4_1_N<-(2217.459563-data$HS_4_1)/(2217.459563-0.007739905)
summary(data$HS_4_1_N)

############///           Normalization: HS_4_3             \\\\\\\\############
############///   UL: 29.8 (Eswatini 2016)                  \\\\\\\\############
############///   LL: 0 (desired goal post)                 \\\\\\\\############

data$HS_4_3_N<-(29.8-data$HS_4_3)/(29.8-0)
summary(data$HS_4_3_N)

############///           Normalization: HS_4_4             \\\\\\\\############
############///   UL: 26.2 (Botswana 1994)                  \\\\\\\\############
############///   LL: 0 (Afghanistan 1990)                  \\\\\\\\############

data$HS_4_4_N<-(26.2-data$HS_4_4)/(26.2-0)
summary(data$HS_4_4_N)

############///           Normalization: HS_4_7C            \\\\\\\\############
############///   UL: 14.63 (Vanuatu 2015)                  \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############


data$HS_4_7_N<-(14.63-data$HS_4_7)/(14.63-0)
summary(data$HS_4_7_N)

############///           Normalization: HS_4_9             \\\\\\\\############
############///   UL: 1590 (Eswatini 2010)                  \\\\\\\\############
############///   LL: 0 (Antigua and Barbuda 2019)          \\\\\\\\############

data$HS_4_9_N<-(1590-data$HS_4_9)/(1590-0)
summary(data$HS_4_9_N)

############///           Normalization: HS_4_12            \\\\\\\\############
############///   UL: 599.74821 (Burkina Faso 2001)         \\\\\\\\############
############///   LL: 0 (Iraq 2021)                         \\\\\\\\############

data$HS_4_12_N<-(599.74821-data$HS_4_12)/(599.74821-0)
summary(data$HS_4_12_N)

############///           Normalization: HR_1_4             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############
colnames(data)
data$HR_1_4_N<-(100-data$HR_1_4)/(100-0)
summary(data$HR_1_4_N)

############///           Normalization: HR_1_6             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$HR_1_6_N<-(100-data$HR_1_6)/(100-0)
summary(data$HR_1_6_N)


############///           Normalization: HR_3_1             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

#****   it is a positive indicator

data$HR_3_1_N<-(data$HR_3_1-0)/(100-0)
summary(data$HR_3_1_N)

############///           Normalization: HR_3_2             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

#****   it is a positive indicator

data$HR_3_2_N<-(data$HR_3_2-0)/(100-0)
summary(data$HR_3_2_N) 

############///           Normalization: HR_3_3             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

#****   it is a positive indicator
data$HR_3_3_N<-(data$HR_3_3-0)/(100-0)
summary(data$HR_3_3_N)


############///           Normalization: HR_4_2             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$HR_4_2_N<-(100-data$HR_4_2)/(100-0)
summary(data$HR_4_2_N)


############///           Normalization: SC_1_2             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_1_2_N<-(data$SC_1_2-0)/(100-0)
summary(data$SC_1_2_N)


############///           Normalization: SC_1_4             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_1_4_N<-(data$SC_1_4-0)/(100-0)
summary(data$SC_1_4_N)

############///           Normalization: SC_1_9             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_1_9_N<-(data$SC_1_9-0)/(100-0)
summary(data$SC_1_9_N)

############///           Normalization: SC_2_1A            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_2_1A_N<-(data$SC_2_1A-0)/(100-0)
summary(data$SC_2_1A_N)


############///           Normalization: SC_2_1C            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_2_1C_N<-(data$SC_2_1C-0)/(100-0)
summary(data$SC_2_1C_N)

############///           Normalization: SC_2_1D            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_2_1D_N<-(data$SC_2_1D-0)/(100-0)
summary(data$SC_2_1D_N)

################################################################################
# NOTE: take the average of SC_2_1A_N: SC_2_1D to have a single indicator SC_2_1_N
data$SC_2_1_N<-(data$SC_2_1A_N+data$SC_2_1C_N+data$SC_2_1D_N)/3
summary(data$SC_2_1_N)

################################################################################

############///           Normalization: SC_3_1             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_3_1_N<-(data$SC_3_1-0)/(100-0)
summary(data$SC_3_1_N)

############///           Normalization: SC_3_2             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_3_2_N<-(data$SC_3_2-0)/(100-0)
summary(data$SC_3_2_N)

############///           Normalization: SC_3_3             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_3_3_N<-(data$SC_3_3-0)/(100-0)
summary(data$SC_3_3_N)

############///           Normalization: SC_4_2             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_4_2_N<-(data$SC_4_2-0)/(100-0)
summary(data$SC_4_2_N)

############///           Normalization: SC_4_3             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_4_3_N<-(data$SC_4_3-0)/(100-0)
summary(data$SC_4_3_N)

############///           Normalization: SC_5_1             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_5_1_N<-(data$SC_5_1-0)/(100-0)
summary(data$SC_5_1_N)

############///           Normalization: SC_5_2             \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$SC_5_2_N<-(data$SC_5_2-0)/(100-0)
summary(data$SC_5_2_N)

############///           Normalization: SC_7_1 (min-max)   \\\\\\\\############
#####///       UL: 117.0877164 (central african republic 2021)  \\\\############
############///   LL: 0 (Canada 2015)                       \\\\\\\\############

data$SC_7_1_N<-(117.0877164-data$SC_7_1)/(117.0877164-0)
summary(data$SC_7_1_N)

############///           Normalization: SC_11_1            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

# positive indicator
data$SC_11_1_N<-(data$SC_11_1-0)/(100-0)
summary(data$SC_11_1_N)

############///           Normalization: HSy_1_6            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

# positive indicator
data$HSy_1_6_N<-(data$HSy_1_6-0)/(100-0)
summary(data$HSy_1_6_N)


############///           Normalization: HSy_3_1A           \\\\\\\\############
############///   UL: 70.616 (Sweden 2020)                  \\\\\\\\############
############///   LL: 0.125 (Malawi 2013)                   \\\\\\\\############

# positive indicator
data$HSy_3_1A_N<-(data$HSy_3_1A-0.125)/(70.616-0.125)
summary(data$HSy_3_1A_N)

############///           Normalization: HSy_3_1B           \\\\\\\\############
############///   UL: 230.711 (Australia 1995)              \\\\\\\\############
############///   LL: 0.482 (Chad 1997)                     \\\\\\\\############

# positive indicator
data$HSy_3_1B_N<-(data$HSy_3_1B-0.482 )/(230.711-0.482)
summary(data$HSy_3_1B_N)

############///           Normalization: HSy_3_1C           \\\\\\\\############
############///   UL: 17.733 (Sweden 2020)                  \\\\\\\\############
############///   LL: 0.002 (Chad 2018)                     \\\\\\\\############

# positive indicator
data$HSy_3_1C_N<-(data$HSy_3_1C-0.002 )/(17.733-0.002)
summary(data$HSy_3_1C_N)

############///           Normalization: HSy_3_1D           \\\\\\\\############
############///   UL: 20.269 (Belgium 2021)                 \\\\\\\\############
############///   LL: 0.004 (Burundi 2012)                  \\\\\\\\############

# positive indicator
data$HSy_3_1D_N<-(data$HSy_3_1D-0.004 )/(20.269-0.004)
summary(data$HSy_3_1D_N)

################################################################################
# NOTE: take the average of HSy_3_1A_N: HSy_3_1D to have a single indicator HSy_3_1_N
data$HSy_3_1_N<-(data$HSy_3_1A_N+data$HSy_3_1B_N+data$HSy_3_1C_N+data$HSy_3_1D_N)/4
summary(data$HSy_3_1_N)
?write.csv()

write.csv(data, file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv'), row.names = F)
################################################################################

############///           Normalization: HSy_5_1            \\\\\\\\############
############///   UL: 21.83 (Afghanistan 2021)              \\\\\\\\############
############///   LL: 1.11 (Equatorial Guinea 2000)        \\\\\\\\############

# positive indicator
data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv'))
data$HSy_5_1_N<-(data$HSy_5_1-1.11 )/(21.83-1.11)
summary(data$HSy_5_1_N)

############///           Normalization: HSy_5_2            \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

# positive indicator
data$HSy_5_2_N<-(data$HSy_5_2-0)/(100-0)
summary(data$HSy_5_2_N)


############///           Normalization: HSy_6_1           \\\\\\\\############
############///   UL: 100 (Desired goal post)               \\\\\\\\############
############///   LL: 0 (Desired goal post)                 \\\\\\\\############

data$HSy_6_1_N<-(data$HSy_6_1-0)/(100-0)
summary(data$HSy_6_1_N)

############///           Normalization: HSy_7_1           \\\\\\\\############
############///   UL: 2.5 (Desired goal post)               \\\\\\\\############
############///   LL: -2.5 (Desired goal post)              \\\\\\\\############

data$HSy_7_1_N<-(data$HSy_7_1+2.5)/5
summary(data$HSy_7_1_N)
write.csv(data, file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv'), row.names = F)

################################################################################
#
#                 STEP 2: Construction of sub-domain indices
#
################################################################################
# NOTE:
#
#-> We will be constructing sub-domain indices by determining the weights for
#    each indicator using the Principal component analysis (PCA).
#
#-> Although PCA is not appropriate for panel data, for the current study, we 
#    consider the panel data as a cross-sectional data with 500 observations.
#
#-> we will not be able to estimate all sub-domain indices of 2020-21 data, as 
#    the data for some of the sub-domains are not available.
#
#-> For those sub-domains with one or two indicators, we don't need to estimate 
#    weight. 
#           for single indicator sub-domain, weight = 1
#           for two indicator sub-domains, weight = 0.707106781 each indicators
################################################################################
# Libraries
library(tidyverse)
# load data for 2020-21 data

data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv'))

################################################################################
#              SI1: Health Status (Mortality by age)                           #
################################################################################
colnames(data)
#Selection of SI1 data
SI1<-data[,c("HS_1_1_N", "HS_1_2_N","HS_1_3_N", "HS_1_4_N", "HS_1_5_N", "HS_1_6_N",
             "HS_1_7_N")]
# to conduct PCA
pca_SI1<-prcomp(SI1, scale = T)

# to summarize the importance of PC1
summary(pca_SI1)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     2.5282 
# -> Proportion of Variance 0.9131
#************************************#

# to determine the weights of each indicators
pca_SI1

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HS_1_PCA <- 1- sqrt((0.3822436-0.3822436*data$HS_1_1_N)^2+
                              (0.3718571-0.3718571*data$HS_1_2_N)^2+
                              (0.3602145-0.3602145*data$HS_1_3_N)^2+
                              (0.3821818-0.3821818*data$HS_1_4_N)^2+
                              (0.3874352-0.3874352*data$HS_1_5_N)^2+
                              (0.3839646-0.3839646*data$HS_1_6_N)^2+
                              (0.3771734-0.3771734*data$HS_1_7_N)^2)
summary(data$SI_HS_1_PCA)

################################################################################
#              SI2: Health Status (Mortality by cause)                         #
################################################################################
colnames(data)
# Selection of SI2 data
SI2<-data[,c("HS_2_1_N","HS_2_2_N","HS_2_3_N","HS_2_4_N")]
# to conduct PCA
pca_SI2<-prcomp(SI2, scale = T)

# to summarize the importance of PC1
summary(pca_SI2)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.5687 
# -> Proportion of Variance 0.6152
#************************************#

# to determine the weights of each indicators
pca_SI2

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HS_2_PCA <- 1- sqrt((0.5728775-0.5728775*data$HS_2_1_N)^2+
                              (0.5200089-0.5200089*data$HS_2_2_N)^2+
                              (0.3708069-0.3708069*data$HS_2_3_N)^2+
                              (0.5137163-0.5137163*data$HS_2_4_N)^2)
summary(data$SI_HS_2_PCA)

################################################################################
#              SI3: Health Status (Fertility)                                  #
################################################################################
# Since Fertility sub-domain has only two indicator, the weight for each indicators
# will be equal and the value will be 0.707106781.
colnames(data)
# Selection of SI2 data
SI3<-data[,c("HS_3_1_N", "HS_3_2_N")]
# to conduct PCA
pca_SI3<-prcomp(SI3, scale = T)

# to summarize the importance of PC1
summary(pca_SI3)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.2999 
# -> Proportion of Variance 0.8449
#************************************#

# to determine the weights of each indicators
pca_SI3

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HS_3_PCA <- 1- sqrt((0.707106781-0.707106781*data$HS_3_1_N)^2+
                              (0.707106781-0.707106781*data$HS_3_2_N)^2)
summary(data$SI_HS_3_PCA)

################################################################################
#              SI4: Health Status (Morbidity)                                  #
################################################################################

colnames(data)
# Selection of SI2 data
SI4<-data[,c("HS_4_1_N", "HS_4_3_N", "HS_4_4_N", "HS_4_7_N", "HS_4_9_N", "HS_4_12_N")]
# to conduct PCA
pca_SI4<-prcomp(SI4, scale = T)

# to summarize the importance of PC1
summary(pca_SI4)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.7512 
# -> Proportion of Variance 0.5111
#************************************#

# to determine the weights of each indicators
pca_SI4

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HS_4_PCA <- 1- sqrt((0.1544218-0.1544218*data$HS_4_1_N)^2+
                              (0.4367891-0.4367891*data$HS_4_3_N)^2+
                              (0.4895525-0.4895525*data$HS_4_4_N)^2+
                              (0.4814611-0.4814611*data$HS_4_7_N)^2+
                              (0.4668370-0.4668370*data$HS_4_9_N)^2+
                              (0.3097837-0.3097837*data$HS_4_12_N)^2)
summary(data$SI_HS_4_PCA)
################################################################################
#              SI5: Health risk factors (Nutrition)                            #
################################################################################

colnames(data)
# Selection of SI2 data
SI5<-data[,c("HR_1_4_N", "HR_1_6_N")]
# to conduct PCA
pca_SI5<-prcomp(SI5, scale = T)

# to summarize the importance of PC1
summary(pca_SI5)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.2397 
# -> Proportion of Variance 0.7684
#************************************#

# to determine the weights of each indicators
pca_SI5

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HR_1_PCA <- 1- sqrt((0.7071068-0.7071068*data$HR_1_4_N)^2+
                              (0.7071068-0.7071068*data$HR_1_6_N)^2)
summary(data$SI_HR_1_PCA)  
################################################################################
#              SI6: Health risk factors (Environmental risk factors)           #
################################################################################

colnames(data)
# Selection of SI2 data
SI6<-data[,c("HR_3_1_N","HR_3_2_N","HR_3_3_N")]
# to conduct PCA
pca_SI6<-prcomp(SI6, scale = T)

# to summarize the importance of PC1
summary(pca_SI6)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.6371 
# -> Proportion of Variance 0.8934
#************************************#

# to determine the weights of each indicators
pca_SI6

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HR_3_PCA <- 1- sqrt((0.5968824-0.5968824*data$HR_3_1_N)^2+
                              (0.5624882-0.5624882*data$HR_3_2_N)^2+
                              (0.5721350-0.5721350*data$HR_3_3_N)^2)
summary(data$SI_HR_3_PCA)

################################################################################
#              SI7: Health risk factors (NCDs)                                 #
################################################################################

colnames(data)
#Note: Since there is only one indicator in this articular subdomain, we dont 
#      need to conduct PCA 

data$SI_HR_4_PCA <- data$HR_4_2_N
summary(data$SI_HR_4_PCA)
################################################################################
#              SI8: Service coverage (RMNCH)                                   #
################################################################################

colnames(data)
# Selection of SI2 data
SI8<-data[,c("SC_1_2_N","SC_1_4_N","SC_1_9_N")]
# to conduct PCA
pca_SI8<-prcomp(SI8, scale = T)

# to summarize the importance of PC1
summary(pca_SI8)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.4386 
# -> Proportion of Variance 0.6898
#************************************#

# to determine the weights of each indicators
pca_SI8

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_1_PCA <- 1- sqrt((0.6016441-0.6016441*data$SC_1_2_N)^2+
                              (0.6139039-0.6139039*data$SC_1_4_N)^2+
                              (0.5110249-0.5110249*data$SC_1_9_N)^2)
summary(data$SI_SC_1_PCA)

################################################################################
#              SI9: Service coverage (Immunization)                            #
################################################################################
# Note: Since there is only  one indicator (SC_2_1_N), this consider as  the sub-domain
#       index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_2_PCA<- data$SC_2_1_N
summary(data$SI_SC_2_PCA)

################################################################################
#              SI10: Service coverage (HIV)                                    #
################################################################################

colnames(data)
# Selection of SI2 data
SI10<-data[,c("SC_3_1_N","SC_3_2_N","SC_3_3_N")]
# to conduct PCA
pca_SI10<-prcomp(SI10, scale = T)

# to summarize the importance of PC1
summary(pca_SI10)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.5984 
# -> Proportion of Variance 0.6087
#************************************#

# to determine the weights of each indicators
pca_SI10

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_3_PCA <- 1- sqrt((0.5953802-0.5953802*data$SC_3_1_N)^2+
                              (0.5368898-0.5368898*data$SC_3_2_N)^2+
                              (0.5977222-0.5977222*data$SC_3_3_N)^2)
summary(data$SI_SC_3_PCA)


################################################################################
#              SI11: Service coverage (HIV/TB)                                 #
################################################################################

colnames(data)
# Selection of SI2 data
SI11<-data[,c("SC_4_2_N","SC_4_3_N")]
# to conduct PCA
pca_SI11<-prcomp(SI11, scale = T)

# to summarize the importance of PC1
summary(pca_SI11)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.1052 
# -> Proportion of Variance 0.6108
#************************************#

# Since HIV/TB sub-domain has only two indicator, the weight for each indicators
# will be equal and the value will be 0.707106781.

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_4_PCA <- 1- sqrt((0.707106781-0.707106781*data$SC_4_2_N)^2+
                              (0.707106781-0.707106781*data$SC_4_3_N)^2)
summary(data$SI_SC_4_PCA)

################################################################################
#              SI12: Service coverage (TB)                                     #
################################################################################

colnames(data)
# Selection of SI2 data
SI12<-data[,c("SC_5_1_N","SC_5_2_N")]
# to conduct PCA
pca_SI12<-prcomp(SI12, scale = T)

# to summarize the importance of PC1
summary(pca_SI12)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.0944 
# -> Proportion of Variance 0.5988
#************************************#

# Since TB sub-domain has only two indicator, the weight for each indicators
# will be equal and the value will be 0.707106781.

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_5_PCA <- 1- sqrt((0.707106781-0.707106781*data$SC_5_1_N)^2+
                              (0.707106781-0.707106781*data$SC_5_2_N)^2)
summary(data$SI_SC_5_PCA)

################################################################################
#              SI13: Service coverage (NTD)                                     #
################################################################################
# Note: Since there is only  one indicator (SC_7_1_N), this consider as  the sub-domain
#       index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_7_PCA<- data$SC_7_1_N

################################################################################
#              SI14: Service coverage (Essential health services)              #
################################################################################
# Since there is only  one indicator (SC_11_1_N), this consider as  the sub-domain
# index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_SC_11_PCA<- data$SC_11_1_N

################################################################################
#              SI15: Health system (Quality and safety of care)                #
################################################################################
# Since there is only  one indicator (HSy_1_6_N), this consider as  the sub-domain
# index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HSy_1_PCA<- data$HSy_1_6_N


################################################################################
#              SI17: Health system (Health work force)                         #
################################################################################
# Since there is only  one indicator (HSy_3_1_N), this consider as  the sub-domain
# index score
colnames(data)
# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HSy_3_PCA <- data$HSy_3_1_N
summary(data$SI_HSy_3_PCA)

################################################################################
#              SI18: Health System (Health financing)                       #
################################################################################

colnames(data)
# Selection of SI2 data
SI18<-data[,c("HSy_5_1_N","HSy_5_2_N")]
# to conduct PCA
pca_SI18<-prcomp(SI18, scale = T)

# to summarize the importance of PC1
summary(pca_SI18)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.0426 
# -> Proportion of Variance 0.5435
#************************************#

# Since Health financing sub-domain has only two indicator, the weight for each 
# indicators will be equal and the value will be 0.707106781.

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HSy_5_PCA <- 1- sqrt((0.7071068-0.7071068*data$HSy_5_1_N)^2+
                               (0.7071068-0.7071068*data$HSy_5_2_N)^2)
summary(data$SI_HSy_5_PCA)
################################################################################
#              SI19: Health system (Health security)                           #
################################################################################
# Since there is only  one indicator (HSy_6_1_N), this consider as  the sub-domain
# index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HSy_6_PCA <- data$HSy_6_1_N
summary(data$SI_HSy_6_PCA)

################################################################################
#              SI20: Health system (Health governance)                         #
################################################################################
# Since there is only  one indicator (HSy_7_1_N), this consider as  the sub-domain
# index score

# Calculation of Sub-domain indices using inverse euclidean formula
data$SI_HSy_7_PCA <- data$HSy_7_1_N
summary(data$SI_HSy_7_PCA)

write.csv(data, file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv'), row.names = F)
################################################################################
#
#                 STEP 3: Construction of composite indices
#
################################################################################
# NOTE:
#
#-> We will be constructing composite indices by determining the weights for
#    each sub-domain indices using the Principal component analysis (PCA).
#
#-> Although PCA is not appropriate for panel data, for the current study, we 
#    consider the panel data as a cross-sectional data with 500 observations.
#
#-> we will not be able to estimate any composite indices of 2020-21 data 
#   (as of 16-04-2024),as the data for constructing composite indices are not 
#   available completely.
#
################################################################################
#              D1: Health status (mortality by age)                            #
################################################################################
colnames(data)
# Selection of D1 data
D1<-data[,c("SI_HS_1_PCA","SI_HS_3_PCA","SI_HS_4_PCA")]
# to conduct PCA
pca_D1<-prcomp(D1, scale = T)

# to summarize the importance of PC1
summary(pca_D1)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.5528 
# -> Proportion of Variance 0.8037
#************************************#

# to determine the weights of each indicators
pca_D1

# Calculation of Sub-domain indices using inverse euclidean formula
data$D_HS_MA_PCA <- 1- sqrt((0.5996169-0.5996169*data$SI_HS_1_PCA)^2+
                              (0.5809066-0.5809066*data$SI_HS_3_PCA)^2+
                              (0.5504608-0.5504608*data$SI_HS_4_PCA)^2)

summary(data$D_HS_MA_PCA)

################################################################################
#              D2: Health status (mortality by cause)                          #
################################################################################
colnames(data)
# Selection of D2 data
D2<-data[,c("SI_HS_2_PCA","SI_HS_3_PCA","SI_HS_4_PCA")]
# to conduct PCA
pca_D2<-prcomp(D2, scale = T)

# to summarize the importance of PC1
summary(pca_D2)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.539 
# -> Proportion of Variance 0.79
#************************************#

# to determine the weights of each indicators
pca_D2

# Calculation of Sub-domain indices using inverse euclidean formula
data$D_HS_MC_PCA <- 1- sqrt((0.5950827-0.5950827*data$SI_HS_2_PCA)^2+
                              (0.5517191-0.5517191*data$SI_HS_3_PCA)^2+
                              (0.5843651-0.5843651*data$SI_HS_4_PCA)^2)

summary(data$D_HS_MC_PCA)

################################################################################
#              D3: Health risk factors                                         #
################################################################################
colnames(data)
# Selection of D3 data
D3<-data[,c("SI_HR_1_PCA","SI_HR_3_PCA","SI_HR_4_PCA")]
# to conduct PCA
pca_D3<-prcomp(D3, scale = T)

# to summarize the importance of PC1
summary(pca_D3)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.4206 
# -> Proportion of Variance 0.6727
#************************************#

# to determine the weights of each indicators
pca_D3

# Calculation of Sub-domain indices using inverse euclidean formula
data$D_HR_PCA <- 1- sqrt((0.6072507-0.6072507*data$SI_HR_1_PCA)^2+
                           (0.6362936-0.6362936*data$SI_HR_3_PCA)^2+
                           (0.4757909-0.4757909*data$SI_HR_4_PCA)^2)
summary(data$D_HR_PCA)

################################################################################
#              D4: Health service coverage                                     #
################################################################################
colnames(data)
# Selection of D4 data
D4<-data[,c("SI_SC_1_PCA", "SI_SC_2_PCA","SI_SC_3_PCA","SI_SC_4_PCA","SI_SC_5_PCA", 
            "SI_SC_7_PCA", "SI_SC_11_PCA")]
# to conduct PCA
pca_D4<-prcomp(D4, scale = T)

# to summarize the importance of PC1
summary(pca_D4)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.8376 
# -> Proportion of Variance 0.4824
#************************************#

# to determine the weights of each indicators
pca_D4

# Calculation of Sub-domain indices using inverse euclidean formula
data$D_SC_PCA <- 1- sqrt((0.43615031-0.43615031*data$SI_SC_1_PCA)^2+
                           (0.40379174-0.40379174*data$SI_SC_2_PCA)^2+
                           (0.29746289-0.29746289*data$SI_SC_3_PCA)^2+
                           (0.07830632-0.07830632*data$SI_SC_4_PCA)^2+
                           (0.40656907-0.40656907*data$SI_SC_5_PCA)^2+
                           (0.39115041-0.39115041*data$SI_SC_7_PCA)^2+
                           (0.48354114-0.48354114*data$SI_SC_11_PCA)^2)
summary(data$D_SC_PCA)

################################################################################
#              D5: Health System - version 1 
# Health System Index created using the new health governance and security indicators
################################################################################

#colnames(data)
# Selection of D4 data
#D5<-data[,c("SI_HSy_1_PCA","SI_HSy_3_PCA","SI_HSy_5_PCA",
#            "SI_HSy_6_PCA","SI_HSy_7_PCA")]
# to conduct PCA
#pca_D5<-prcomp(D5, scale = T)

# to summarize the importance of PC1
#summary(pca_D5)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.7656 
# -> Proportion of Variance 0.6235
#************************************#

# to determine the weights of each indicators
#pca_D5

# Calculation of Sub-domain indices using inverse euclidean formula

#data$D_HSy_PCA <- 1- sqrt((0.3320051-0.3320051*data$SI_HSy_1_PCA)^2+
#                            (0.5187005-0.5187005*data$SI_HSy_3_PCA)^2+
#                            (0.3977662-0.3977662*data$SI_HSy_5_PCA)^2+
#                            (0.4524087-0.4524087*data$SI_HSy_6_PCA)^2+
#                            (0.5077705-0.5077705*data$SI_HSy_7_PCA)^2)
#summary(data$D_HSy_PCA)

#write.csv(data, file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv'), row.names = F)
################################################################################
#              D5: Health System - version 2 
# Health System Index created using by excluding the health security sub-index 
# scores as undelrying indicators
################################################################################

colnames(data)
# Selection of D4 data
D5<-data[,c("SI_HSy_1_PCA","SI_HSy_3_PCA","SI_HSy_5_PCA","SI_HSy_7_PCA")]
# to conduct PCA
pca_D5<-prcomp(D5, scale = T)

# to summarize the importance of PC1
summary(pca_D5)

#************************************#
# Importance of first component (PC1):
# -> Standard deviation     1.6052 
# -> Proportion of Variance 0.6442
#************************************#

# to determine the weights of each indicators
pca_D5

# Calculation of Sub-domain indices using inverse euclidean formula

data$D_HSy_PCA <- 1- sqrt((0.4129984-0.4129984*data$SI_HSy_1_PCA)^2+
                            (0.5763586-0.5763586*data$SI_HSy_3_PCA)^2+
                            (0.4441976-0.4441976*data$SI_HSy_5_PCA)^2+
                            (0.5476600-0.5476600*data$SI_HSy_7_PCA)^2)
summary(data$D_HSy_PCA)

write.csv(data, file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv'), row.names = F)

################################################################################
# Table 1: Description of the performance of HDI groups in underlying indicators 
#          Health domains
################################################################################
library(tidyverse)
data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv'))
colnames(data)
df<-select(data, c("HC",which(colnames(data) == "SC_1_2_N"):which(colnames(data) == "SC_11_1_N")))
df<-aggregate(df, by = list(data$HC), FUN = "mean")
colnames(df)
df<-select(df, -HC)
df<-pivot_longer(df,cols = 2:ncol(df),names_to = "indicator", values_to = "value")
df<-pivot_wider(df, names_from = "Group.1", values_from = "value")
write.csv(df,'table1.csv')
################################################################################
# Figure 2: Distribution of countries across different HDI levels and HST levels
# for the period 2020-2021
################################################################################
colnames(data)
df<-select(data, c("C","YN","HC","D_HSy_PCA"))

df$Index<-ifelse(df$D_HSy_PCA>=0.8,"VH",
                ifelse(df$D_HSy_PCA>=0.7, "H",
                       ifelse(df$D_HSy_PCA>=0.55,"M","L")))

# Cross-tabulation for each year for HSYI
for (yr in unique(df$YN)) {
  cat("\n Cross-tabulation for year:", yr, "\n")
  print(table(df[df$YN == yr, c("HC","Index")]))
}

################################################################################
# Table 2: Description of Composite and sub-indices of health status across HDI 
# groups between the period 2020-21
################################################################################
colnames(data)
df<-select(data, c("YN","HC",
                   which(colnames(data)=="SI_HR_1_PCA"): which(colnames(data)=="SI_HR_4_PCA"),
                   "D_HR_PCA"))
df$HC<-as.factor(df$HC)
df<-aggregate(x = df, by = list(df$YN, df$HC), FUN = "mean")
df<-pivot_longer(df, cols = 5:ncol(df),names_to = "index", values_to = "value")
df<-select(df, c("YN","Group.2","index","value"))
df<-pivot_wider(df,names_from = "YN", values_from = "value")
write.csv(df,'table2.csv')

################################################################################
# Table 3: sub-domain wise and Composite index by countries 
# (top 10 and bottom 10 countries in the year 2020 and 2021)
################################################################################
colnames(data)
df<-select(data, c("C","YN","HC",
                   which(colnames(data)=="SI_HR_1_PCA"): which(colnames(data)=="SI_HR_4_PCA"),
                   "D_HR_PCA"))

df1<-df[df$YN==2020,]
df1 <- df1[order(df1$D_HR_PCA, decreasing = TRUE), ]
df1<-df1[c(1:10, 91:100),]
df1$group <- ifelse(1:nrow(df1) <= 10, 1, ifelse(1:nrow(df1) <= 20, 2, NA))

df2<-df[df$YN==2021,]
df2 <- df2[order(df2$D_HR_PCA, decreasing = TRUE), ]
df2<-df2[c(1:10, 91:100),]
df2$group <- ifelse(1:nrow(df2) <= 10, 1, ifelse(1:nrow(df2) <= 20, 2, NA))

df3<-rbind(df1,df2) %>% arrange(group)
df3$HC_group<-ifelse(df3$HC==1,"VH",
                     ifelse(df3$HC==2,"H",
                            ifelse(df3$HC==3,"M","L")))
write.csv(df3,'table3.csv')


################################################################################
#
#                                  OBJECTIVE 2 
#
# To estimate trend growth of the Health System index, Health Risk Factors index, 
# Health Service Coverage index and Health Status index for each selected HDI 
# countries from 2015 to 2021 and further forecast their values up to the year 
# 2025.
# 
################################################################################
####                                                                       #####
####  Descriptive (Mean ± SD, Coefficient of variation) for each indicator #####
####                  in sub-domain wise for each country                  #####
####                                                                       #####
################################################################################
library(tidyverse)
data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 0/3_Data_15_21/data_V2_IMP_15_21_V1.csv'))
colnames(data)

#------@@@@@@@-------
# COV_Indicator_15_21
#--------------------
df<-select(data, where(~ all(!is.na(.)))) # Select columns with no missing values
colnames(df)
df1<-summarize(df, across(.cols = 12:61, .fns = list(mean = mean, sd = sd)))
indicator_cols<-colnames(df[,12:61])
# Initialize a vector to store the names of the CV columns
cv_cols <- character(length(indicator_cols))
for (i in seq_along(indicator_cols)) {
  mean_col <- paste0(indicator_cols[i], "_mean")
  sd_col <- paste0(indicator_cols[i], "_sd")
  cv_col <- paste0(indicator_cols[i], "_CV")
  df1 [[cv_col]] <- df1 [[sd_col]] / df1 [[mean_col]]
  cv_cols[i] <- cv_col
}
df1<-t(df1)

write.csv(df1,'COV_Indicator_15_21.csv', row.names = F)

#------@@@@@@@--------------------
# COV_Indicator_country-wise_15_21
#---------------------------------
df<-select(data, where(~ all(!is.na(.)))) # Select columns with no missing values
result <- df %>%
  group_by(C) %>%
  summarize(across(.cols = everything(), .fns = list(mean = mean, sd = sd)))

indicator_cols<-colnames(df[,7:61])
# Initialize a vector to store the names of the CV columns
cv_cols <- character(length(indicator_cols))
# Loop through each indicator column and calculate the CV
for (i in seq_along(indicator_cols)) {
  mean_col <- paste0(indicator_cols[i], "_mean")
  sd_col <- paste0(indicator_cols[i], "_sd")
  cv_col <- paste0(indicator_cols[i], "_CV")
  
  result [[cv_col]] <- result [[sd_col]] / result[[mean_col]]
  
  cv_cols[i] <- cv_col
}

# Arrange the columns in the desired order
ordered_cols <- c(rbind(paste0(indicator_cols, "_mean"), 
                        paste0(indicator_cols, "_sd"), 
                        paste0(indicator_cols, "_CV")))

colnames(result)[1]<-"C"
# Select the relevant columns (country and ordered CV columns)
result <- result %>%
  select(C, all_of(ordered_cols))

colnames(result)

write.csv(result, 'COV_Indicator_country-wise_15_21.csv', row.names = F)
#------@@@@@@@-------
# COV_Indicator_15_19
#--------------------
library(tidyverse)
data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 0/3_Data_15_21/data_V2_IMP_15_21_V1.csv'))
colnames(data)

# I want to select only those columns in data which has complete values only for the period 2015-2019
df0<-select(data, where(~ all(!is.na(.)))) # Select columns with missing values
colnames(df0)
df<-select(data, -colnames(df0))
df<-cbind.data.frame(data[,1],df)
df<-df[is.na(df[2])== F,]
df1<-summarize(df, across(.cols = 2:16, .fns = list(mean = mean, sd = sd)))
indicator_cols<-colnames(df[,2:16])
# Initialize a vector to store the names of the CV columns
cv_cols <- character(length(indicator_cols))
# Loop through each indicator column and calculate the CV
for (i in seq_along(indicator_cols)) {
  mean_col <- paste0(indicator_cols[i], "_mean")
  sd_col <- paste0(indicator_cols[i], "_sd")
  cv_col <- paste0(indicator_cols[i], "_CV")
  
  df1 [[cv_col]] <- df1 [[sd_col]] / df1 [[mean_col]]
  
  cv_cols[i] <- cv_col
}

df1<-pivot_longer(data = df1,cols = 1:45,names_to = 'index', values_to = 'value')

write.csv(df1, 'COV_Indicator_15_19.csv', row.names = F)

#------@@@@@@@--------------------
# COV_Indicator_country-wise_15_21
#---------------------------------
# I want to select only those columns in data which has complete values only for the period 2015-2019
df0<-select(data, where(~ all(!is.na(.)))) # Select columns with missing values
colnames(df)
df<-select(data, -colnames(df0))
df<-cbind.data.frame(data[,1],df)
colnames(df)[1]<-"C"
df<-df[is.na(df[2])== F,]

result <- df %>%
  group_by(C) %>%
  summarize(across(.cols = everything(), .fns = list(mean = mean, sd = sd)))

indicator_cols<-colnames(df[,2:16])
# Initialize a vector to store the names of the CV columns
cv_cols <- character(length(indicator_cols))
# Loop through each indicator column and calculate the CV
for (i in seq_along(indicator_cols)) {
  mean_col <- paste0(indicator_cols[i], "_mean")
  sd_col <- paste0(indicator_cols[i], "_sd")
  cv_col <- paste0(indicator_cols[i], "_CV")
  
  result [[cv_col]] <- result [[sd_col]] / result[[mean_col]]
  
  cv_cols[i] <- cv_col
}

# Arrange the columns in the desired order
ordered_cols <- c(rbind(paste0(indicator_cols, "_mean"), 
                        paste0(indicator_cols, "_sd"), 
                        paste0(indicator_cols, "_CV")))

# Select the relevant columns (country and ordered CV columns)
result <- result %>%
  select(C, all_of(ordered_cols))


write.csv(result, 'COV_Indicator_country-wise_15_19.csv', row.names = F)


################################################################################
#                                                                          
#     CAGR calculation for sub indices and domain indices which has been   
#                 estimated using Non equal method (PCA)                  
#                                                                    
################################################################################
library(broom)
library(tidyverse)
#-----------@@@@@----------------------------
# CAGR for index calculation during 2015-2019
#--------------------------------------------
data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv"))
colnames(data)
df<-select(data, c("C","YC",
                   which(colnames(data)=="SI_HS_1_PCA"):which(colnames(data)=="D_HSy_PCA")))
df<- pivot_longer(df,cols = 3:29, names_to = "Indicator",values_to = "value")
dfl <- df %>% mutate_at (vars(4), ~ log(.))
dfl<-arrange(dfl,interaction(dfl$C, dfl$Indicator))
dfl <- dfl[!is.infinite(dfl$value) & dfl$value != -Inf, ]


C_list <- split(dfl, interaction (dfl$C, dfl$Indicator))

# Check if any dataframe within C_list has 0 rows
empty_df_indices <- sapply(seq_along(C_list), function(i) {
  if (nrow(C_list[[i]]) == 0) {
    print(paste("Dataframe", i, "has 0 rows."))
    return(i)
  }
})

# Remove dataframes with 0 rows from C_Indicator_list
empty_df_indices <- unlist(empty_df_indices)
if (any(empty_df_indices)) {
  C_list <- C_list[-empty_df_indices]
}



# Regression to only those variables which does not have any missing values after taking logarithm
R <- lapply(C_list, function(x) lm(cbind(value)~ YC, data = x))

R_df <- lapply(R, tidy) %>% bind_rows(.id = "C")
#to use pivot wider function
R_df <- pivot_wider(R_df, names_from = term, values_from = estimate:p.value)
colnames(R_df)
write.csv(R_df,"CAGR_Index_15_19_Regression_output.csv")

R_df<-select(R_df, c("C", "estimate_(Intercept)", "estimate_YC","p.value_YC"))
R_df <- R_df %>% separate(C, into = c("C", "indicator"), sep = "\\.")


####  Rearranging data for forecasting purpose- CAGR Indicators            #####
df<-select(data, c("C","YC",
                   which(colnames(data)=="SI_HS_1_PCA"):which(colnames(data)=="D_HSy_PCA")))
colnames(R_df)<- c("C","indicator","Alpha", "Beta", "Sig_value")
df<-pivot_longer(df,3:29, names_to = "indicator", values_to = "value")
df<-pivot_wider(df,names_from = YC, values_from = value)
dfl<-left_join(R_df,df,by = c("C","indicator"))
write.csv(dfl,"CAGR_Index_15_19_calculation_complete.csv", row.names = F)



#-----------@@@@@----------------------------
# CAGR for index calculation during 2020-2021
#--------------------------------------------
data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv"))
colnames(data)
df<-select(data, c("C","YC",
                   which(colnames(data)=="SI_HS_1_PCA"):which(colnames(data)=="D_HSy_PCA")))
df<- pivot_longer(df,cols = 3:26, names_to = "Indicator",values_to = "value")
dfl <- df %>% mutate_at (vars(4), ~ log(.))
dfl<-arrange(dfl,interaction(dfl$C, dfl$Indicator))
dfl <- dfl[!is.infinite(dfl$value) & dfl$value != -Inf, ]


C_list <- split(dfl, interaction (dfl$C, dfl$Indicator))

# Check if any dataframe within C_list has 0 rows
empty_df_indices <- sapply(seq_along(C_list), function(i) {
  if (nrow(C_list[[i]]) == 0) {
    print(paste("Dataframe", i, "has 0 rows."))
    return(i)
  }
})

# Remove dataframes with 0 rows from C_Indicator_list
empty_df_indices <- unlist(empty_df_indices)
if (any(empty_df_indices)) {
  C_list <- C_list[-empty_df_indices]
}



# Regression to only those variables which does not have any missing values after taking logarithm
R <- lapply(C_list, function(x) lm(cbind(value)~ YC, data = x))

R_df <- lapply(R, tidy) %>% bind_rows(.id = "C")
#to use pivot wider function
R_df <- pivot_wider(R_df, names_from = term, values_from = estimate:p.value)
colnames(R_df)
write.csv(R_df,"CAGR_Index_20_21_Regression_output.csv")

R_df<-select(R_df, c("C", "estimate_(Intercept)", "estimate_YC","p.value_YC"))
R_df <- R_df %>% separate(C, into = c("C", "indicator"), sep = "\\.")


####  Rearranging data for forecasting purpose- CAGR Indicators            #####
df<-select(data, c("C","YC",
                   which(colnames(data)=="SI_HS_1_PCA"):which(colnames(data)=="D_HSy_PCA")))
colnames(R_df)<- c("C","indicator","Alpha", "Beta", "Sig_value")
df<-pivot_longer(df,3:26, names_to = "indicator", values_to = "value")
df<-pivot_wider(df,names_from = YC, values_from = value)
dfl<-left_join(R_df,df,by = c("C","indicator"))
write.csv(dfl,"CAGR_Index_20_21_calculation_complete.csv", row.names = F)

################################################################################
####                                                                       #####
####          CAGR calculation for each health indicators                  #####
####                                                                       #####
################################################################################

#-----------@@@@@------------------------------------
# CAGR for those indicators available from 2015-2021
#----------------------------------------------------

data<-read.csv(file.path('D:/0_PhD Work/6_DataAnalysis/Objective 2/Forecasting/data_obj2_forecast_V1.csv'))
colnames(data)
summary(data)
library(tidyverse)
library(broom)
df<-select(data, c("C","YC",
                   which(colnames(data)=="HS_1_1"):which(colnames(data)=="HSy_7_1")))
# We have three problems in this data which is a barrier to conduct semi-log linear analysis
#           1. Lot of values are zero (log 0 is undefined)
#           2. HSy_7_1 values are ranges from -2.44 to 2.2846 (log of negative values are undefined)
#           3. Since this is 2015-2021 data, for many of the indicator 2020-2021 values may be missing
# What we need to address this issue 
#           1. we have to omit these observations in the regression process
#           2. we have to transform this variable in the way that none of the values are negative
#           3. we have to omit these values 

df$HSy_7_1<-df$HSy_7_1+2.5 # Solution 2
summary(df$HSy_7_1)

df<- pivot_longer(df,cols = 3:66, names_to = "indicator",values_to = "value")
df<-na.omit(df) # Solution 3
sum (is.na(df$value))
df<-df[df$value != 0, ] # Solution 1
summary(df$value)
min(df$value)


dfl <- df %>% mutate_at (vars(4), ~ log(.))
summary (dfl)
#dfl[dfl$value==-Inf,]
#dfl<-arrange(dfl,interaction(dfl$C, dfl$indicator))
#dfl <- dfl[!is.infinite(dfl$value) & dfl$value != -Inf, ]

C_list <- split(dfl, interaction (dfl$C, dfl$indicator))

# Check if any dataframe within C_list has 0 rows
empty_df_indices <- sapply(seq_along(C_list), function(i) {
  if (nrow(C_list[[i]]) == 0) {
    print(paste("Dataframe", i, "has 0 rows."))
    return(i)
  }
})

# Remove dataframes with 0 rows from C_Indicator_list
empty_df_indices <- unlist(empty_df_indices)
if (any(empty_df_indices)) {
  C_list <- C_list[-empty_df_indices]
}



# Regression to only those variables which does not have any missing values after taking logarithm
R <- lapply(C_list, function(x) lm(cbind(value)~ YC, data = x))

R_df <- lapply(R, tidy) %>% bind_rows(.id = "C")
#to use pivot wider function
R_df <- pivot_wider(R_df, names_from = term, values_from = estimate:p.value)
colnames(R_df)
write.csv(R_df,"CAGR_Indicator_15_21_Regression_output.csv")

R_df<-select(R_df, c("C", "estimate_(Intercept)", "estimate_YC","p.value_YC"))
R_df <- R_df %>% separate(C, into = c("C", "indicator"), sep = "\\.")
unique(R_df$indicator)
unique(R_df$C)
################################################################################
####                                                                       #####
####  Rearranging data for forecasting purpose- CAGR Indicators            #####
####                                                                       #####
################################################################################
df<-select(data, c("C","YC",
                   which(colnames(data)=="HS_1_1"):which(colnames(data)=="HSy_7_1")))
colnames(R_df)<- c("C","indicator","Alpha", "Beta", "Sig_value")
df<-pivot_longer(df,3:66, names_to = "indicator", values_to = "value")
df<-pivot_wider(df,names_from = YC, values_from = value)
dfl<-left_join(R_df,df,by = c("C","indicator"))
write.csv(dfl,"CAGR_Indicator_15_21_calculation_complete.csv", row.names = F)

################################################################################
#                                                                      
#  To prepare CAGR top 5 and bottom 5 countries list for each           
#             HDI level
#                                                                     
################################################################################
data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 2/CAGR_Country_Index_20_21_Table_Prep.csv"))
colnames(data)
data<-select(data, c("HC","C","indicator","EXP_Beta_1"))
unique (data$indicator)
# Composite index I
for (i in 1:4) {
  assign(paste0('df',i), data %>%
           filter(HC == i & indicator == "D_HS_MA_PCA") %>%
           arrange(desc(EXP_Beta_1)) %>%
           slice(c(1:5, 21:25)) %>%
           select(c("C", "EXP_Beta_1")))
}
d1<-cbind(df1,df2,df3,df4)
# Composite index II
for (i in 1:4) {
  assign(paste0('df',i), data %>%
           filter(HC == i & indicator == "D_HS_MC_PCA") %>%
           arrange(desc(EXP_Beta_1)) %>%
           slice(c(1:5, 21:25)) %>%
           select(c("C", "EXP_Beta_1")))
}
d2<-cbind(df1,df2,df3,df4)
# Composite index III
for (i in 1:4) {
  assign(paste0('df',i), data %>%
           filter(HC == i & indicator == "D_HR_PCA") %>%
           arrange(desc(EXP_Beta_1)) %>%
           slice(c(1:5, 21:25)) %>%
           select(c("C", "EXP_Beta_1")))
}
d3<-cbind(df1,df2,df3,df4)
# Composite index IV
for (i in 1:4) {
  assign(paste0('df',i), data %>%
           filter(HC == i & indicator == "D_SC_PCA") %>%
           arrange(desc(EXP_Beta_1)) %>%
           slice(c(1:5, 21:25)) %>%
           select(c("C", "EXP_Beta_1")))
}
d4<-cbind(df1,df2,df3,df4)
# Composite index IV
for (i in 1:4) {
  assign(paste0('df',i), data %>%
           filter(HC == i & indicator == "D_HSy_PCA") %>%
           arrange(desc(EXP_Beta_1)) %>%
           slice(c(1:5, 21:25)) %>%
           select(c("C", "EXP_Beta_1")))
}
d5<-cbind(df1,df2,df3,df4)

df<-rbind(d1,d2,d3,d4,d5)

write.csv(df, "CAGR_TBTable.csv")
################################################################################
#                                                                      
#  To prepare CAGR of HDI groups table for each composite index
#                                                                     
################################################################################
library(tidyverse)
library(broom)

# CAGR for the period 2015-2019
data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv"))
# CAGR for the period 2020-2021
data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv"))

colnames(data)

data<-select(data, c("HC","YC","D_HS_MA_PCA","D_HS_MC_PCA","D_HR_PCA","D_SC_PCA","D_HSy_PCA"))
df<-aggregate(.~HC+YC,data = data,FUN = "mean")
df<- pivot_longer(df,cols = 3:7, names_to = "Indicator",values_to = "value")
dfl <- df %>% mutate_at (vars(4), ~ log(.))
C_list <- split(dfl, interaction (dfl$HC, dfl$Indicator))
# Regression to only those variables which does not have any missing values after taking logarithm
R <- lapply(C_list, function(x) lm(cbind(value)~ YC, data = x))

R_df <- lapply(R, tidy) %>% bind_rows(.id = "HC")
#to use pivot wider function
R_df <- pivot_wider(R_df, names_from = term, values_from = estimate:p.value)
colnames(R_df)

R_df<-select(R_df, c("HC", "estimate_(Intercept)", "estimate_YC","p.value_YC"))
R_df <- R_df %>% separate(HC, into = c("C", "indicator"), sep = "\\.")
colnames(R_df)<- c("HC","indicator","Alpha", "Beta", "Sig_value")
write.csv(R_df,"CAGR_HDI_Index_15_19_calculation_complete.csv", row.names = F)
#  Rearranging data for forecasting purpose- CAGR Indicators
R_df<-read.csv("CAGR_HDI_Index_15_19_calculation_complete.csv")
colnames(R_df)<- c("HC","indicator","Alpha", "Beta", "Sig_value")

df<-aggregate(.~HC+YC,data = data,FUN = "mean")#for the period 2015-2019
df<-pivot_longer(df,3:7, names_to = "indicator", values_to = "value")
df<-pivot_wider(df,names_from = YC, values_from = value)
dfl<-left_join(R_df,df,by = c("HC","indicator"))
write.csv(dfl,"CAGR_HDI_Index_20_21_calculation_complete.csv", row.names = F) #for the period 2015-2019

write.csv(df, 'observation.csv') #for the period 2020-2021
write.csv(R_df, 'CAGRHDI.csv') #for the period 2020-2021
# rearrange data for table in result section
df<-read.csv("CAGR_HDI_Index_15_19_calculation_complete.csv")#for the period 2015-2019
df<-read.csv("CAGR_HDI_Index_20_21_calculation_complete.csv")#for the period 2020-2021

colnames(df)
df<-select(df,c("HC","indicator","EXP_Beta_1"))
df<-pivot_wider(df,names_from = HC, values_from = EXP_Beta_1)
write.csv(df, "demo.csv")


################################################################################
#                                                                      
#  To prepare Forecasted indicator organized (long format) data
#                                                                     
################################################################################
data<- read.csv('D:/0_PhD Work/6_DataAnalysis/Objective 2/Forecasting/data_V4_15_25_FORECAST_V1.csv')
data<-pivot_longer(data,4:14,names_to = 'years',values_to = 'value')
colnames(data)
data<-pivot_wider(data,names_from = "indicator",values_from = 'value')
write.csv(data, "data_V4_15_25_FORECAST_V2.csv")

################################################################################
#                                                                      
#  To prepare Forecasted indicator organized (wide format) data
#                                                                     
################################################################################
library(tidyverse)
data<- read.csv('D:/0_PhD Work/6_DataAnalysis/Objective 2/Forecasting/Forecasted_Analysis_Data_V1.csv')
df<-pivot_longer (data,4:67, names_to = "indicator", values_to = 'value')
df<- pivot_wider (df,names_from = YC, values_from = value)
df<-select(df, 2:14)
df1 <- aggregate (. ~ HC+indicator,data = df, FUN = mean)
df2 <- aggregate (. ~ indicator,data = df, FUN = mean)

write.csv(df2, 'Forecasted_Analysis_Data_V4.csv')
################################################################################
#
#                                  OBJECTIVE 3 
#
# To study whether there is any significant difference in Health System, Health 
# Risk Factors, Health Service Coverage and Health Status (Fertility, morbidity 
# and mortality) index values across different groups of countries based on HDI 
# level.
# 
################################################################################
# STEP 1: One-way ANOVA
# STEP 2: Levenes test of homogeneity
# STEP 3: Tukey's Post-hoc test on fulfillment of homogeneity, or Games-Howell 
#         post-hoc test otherwise.
################################################################################
# Earlier in SPSS we conducted ANOVA for each year. But here in this script we 
# take the average of values across the study period for each country and then 
# apply ANOVA for one time. later after discussion we will do for each year if
# it is needed.
################################################################################

setwd('D:/0_PhD Work/11_DC-V_Presentation_April_2024')
library(tidyverse)
library(ggplot2)

data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv"))
colnames(data)
df<-select(data, c( "C","CC","HC","AUX1","D_HS_MA_PCA","D_HR_PCA","D_SC_PCA","D_HSy_PCA"))
df<-pivot_longer(df,cols = 5:8,names_to = "Index",values_to = "value")
df$HG<-if_else (df$HC == 1,"Very high HDI", 
               if_else(df$HC == 2,"High HDI",
                       if_else(df$HC == 3,"Medium HDI", "Low HDI")))
df$index_cat<-if_else (df$value < 0.55,"Low",
                      if_else(df$value <0.7,"Medium",
                              if_else(df$value <0.8,"High", "Very high")))

df$HG<-as.factor(df$HG)
write.csv(df, "demo.csv", row.names = F)


df<-select(data, c("HC",115:138))
df1<-df %>% aggregate(.~HC, FUN = "mean")
df2<-df %>% aggregate(.~HC, FUN = "sd")
df3<-cbind(df1,df2)
write.csv(df3, "demo1.csv")
# Visualization I
# 1. I need to assign colors and shapes to each category
# 2. Avoid background make it white
# Create a vector of shapes corresponding to the HG levels
shapes <- c("Very high" = 8, "High" = 17, "Medium" = 16, "Low" = 15)

# Create a vector of colors corresponding to the D1 categories
colors <- c("D_HS_MA_PCA" = "#F08080", "D_HR_PCA" = "#008B8B", "D_SC_PCA" = "#F4A460", "D_HSy_PCA" = "#8B1C62")


# HST+HRF+HSC+HSy vs HDI Category
ggplot(df) +
  aes(x = factor(HG, levels = c("Very high HDI", "High HDI", "Medium HDI", "Low HDI")), 
      y = value, 
      color = factor (Index),
      shape = factor (index_cat),
      group = factor (Index))+  
  geom_jitter(size = 5, alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1)+
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  labs(x = "HDI groups", y = "Health Status Index (HSI)") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none")




# HST+HRF+HSC+HSy vs HDI score 
ggplot(df) +
  aes(x = AUX1, 
      y = value, 
      color = factor (Index),
      shape = factor (index_cat),
      group = factor (Index))+
  geom_rect(
    xmin = 0, xmax = 0.55, ymin = -Inf, ymax = Inf, 
    fill = "#F6FFEA", alpha = 0.3, inherit.aes = FALSE
  ) +
  geom_rect(
    xmin = 0.55, xmax = 0.7, ymin = -Inf, ymax = Inf, 
    fill = "#E1FFC9", alpha = 0.3, inherit.aes = FALSE
  ) +
  geom_rect(
    xmin = 0.7, xmax = 0.8, ymin = -Inf, ymax = Inf, 
    fill = "#D6FFA5", alpha = 0.3, inherit.aes = FALSE
  ) +
  geom_rect(
    xmin = 0.8, xmax = 1, ymin = -Inf, ymax = Inf, 
    fill = "#C2FF89", alpha = 0.3, inherit.aes = FALSE
  )+
  geom_jitter(size = 5, alpha = 0.8) + 
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 1)+
  scale_color_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  labs(x = "HDI groups", y = "Health Status Index (HSI)") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "none")


# Perform ANOVA
model <- aov(D_HS_MA_PCA ~ HG, data = df)

shapiro.test(model$residuals)
# Summary of ANOVA results
summary(model)


# Visualization of ANOVA and pairwise comparison of composite index
library(ggstatsplot)
ggbetweenstats(
  data = df,
  x = HG,
  y = D_HS_MA_PCA,
  type = "parametric", # ANOVA or Kruskal-Wallis
  var.equal = TRUE, # ANOVA or Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)

# data preparation for objective 3- pairwise comparison of sub-indices figure
data<-read.csv("demo1.csv")
data<- select(data, 1:23)
df<-pivot_longer(data,cols = 4:23, names_to = "Indicator", values_to = "value")
write.csv(df, "demo1.csv")

# Visualization of pairwise comparison of sub-indices
data<-read.csv("demo1.csv")
data$HG<-if_else(data$HC == 1,"Very high HDI", 
                 if_else(data$HC == 2,"High HDI",
                         if_else(data$HC == 3,"Medium HDI", "Low HDI")))
i<-6
data$index_level<-if_else(data[[i]] >= 0.8,"Very high", 
                          if_else(data[[i]] >=0.7,"High",
                                  if_else(data[[i]] >= 0.55,"Medium", "Low")))
df<-data[data$Domain=="D4",]
ggplot(df) +
  aes(x = factor(HG, levels = c("Very high HDI", "High HDI", "Medium HDI", "Low HDI")), 
      y = value, 
      color = Indicator,
      shape = index_level) +  
  geom_jitter(size = 5, alpha = 0.6) +  
  labs(x = "HDI groups", y = "Index values") +
  theme(legend.position = "none")




#DC 6 meeting content preparation
library(tidyverse)
library(ggplot2)
data1<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv"))
data2<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv"))
colnames(data1)
mean(data2$D_HSy_PCA)
data2 %>%
  group_by(HC) %>%
  summarise(mean_ = mean(D_HSy_PCA))

################################################################################
#
#                  Discussion Writing : Objective 1 
#
# 
################################################################################
library(tidyverse)
data1<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv"))
data2<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv"))
colnames(data)
data<-data1 [c(116:120,221:230), c(3:8, 14:38, 93:118,163:186)]

df<-select(data1, c(3:5,8,93:116,163:166,185:186))

write.csv(df, 'DisO1.csv')

################################################################################
#
#                  Discussion Writing : Objective 1 
#
# 
################################################################################

library(tidyverse)
library(ggplot2)

data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 0/1_Data_15_19/data_V1_O_15_19_V2.csv"))
df<-data[,-(1:11)]
# to know the structure of the data
str(df)
#Missing Proportion
MP<-mean(is.na(data[,-(1:11)]))
#Total Number of Missing data
sum(is.na(data[,-(1:11)]))
nrow(df) * ncol(df)



data<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 0/2_Data_20_21/data_V1_O_20_21_V2.csv"))
df<-data[,-(1:11)]
# to know the structure of the data
str(df)
#Missing Proportion
MP<-mean(is.na(df))
#Total Number of Missing data
sum(is.na(df))
nrow(df) * ncol(df)

colnames(data)

################################################################################
#
#                  Appendix Writing 
#
# 
################################################################################
#Appendix VIII provides the average values of Health Status Index – I and II, 
#Health Risk Factors Index, Health Service Coverage Index and Health System Index 
#for each country in the study over the period 2015 – 2019.
library(tidyverse)
data1<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_15_19_V3.csv"))
colnames(data1)
data<-select(data1, c(3,8,185:189))
data<-aggregate(.~C,data = data,FUN = mean)
data$HDGroup<-ifelse(data$HC == 4,"Low HDI", 
                     ifelse(data$HC==3,"Medium HDI",
                            ifelse(data$HC==2,"High HDI","Very high HDI")))
write.csv(data,'appendi.csv')
#APPENDIX IX: The average values of Health Status Index – I and II, Health Risk 
#Factors Index, Health Service Coverage Index and Health System Index for each 
#country in the study over the period 2020 – 2021
data2<-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv"))
colnames(data2)
data<-select(data2, c(2,7,135:139))
data<-aggregate(.~C,data = data,FUN = mean)
data$HDGroup<-ifelse(data$HC == 4,"Low HDI", 
                     ifelse(data$HC==3,"Medium HDI",
                            ifelse(data$HC==2,"High HDI","Very high HDI")))
write.csv(data,'appendi.csv')

################################################################################
#
#                  Supplementary File 3 
#
# 
################################################################################
library(tidyverse)
data <-read.csv(file.path("D:/0_PhD Work/6_DataAnalysis/Objective 1/data_V3_IC_20_21_V3.csv"))
colnames(data)
data <- select (data, c("HC", which (colnames(data)== "HS_1_1"):which (colnames(data)== "HSy_7_1") ))
data <- aggregate(. ~ HC, data = data, FUN = sd)
write.csv (data,'data_SD.csv')
colnames(data)
df<-data.frame(colMeans (data))
write.csv(df,'data.csv')
df<- sapply (data, FUN = function(x) c(Mean = mean(x, na.rm = TRUE), 
                                       SD = sd(x, na.rm = TRUE),
                                       MIN = min(x, na.rm = T),
                                       MAX = max(x, na.rm = T)))
df<- data.frame(df)
df<- t(df)
