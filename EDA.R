library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gtsummary)
####################################################################### 
# Data Setup
#######################################################################

EDA <- read_excel("DeID2.xlsx", sheet = "2TKA_w.syn", range = cell_rows(1:34))
EDA <- EDA %>% mutate(MUA_type = ifelse(MUA==1&C_MUA!=1, 
  "MUA",ifelse(MUA!=1&C_MUA==1, "C_MUA","Both")))
EDA$MUA_type <- factor(EDA$MUA_type, levels = c("MUA","C_MUA","Both"))

EDA$race <- ifelse(EDA$race == "American Indian", "Other",EDA$race)

names(EDA)[8] <- c("BMI")
names(EDA)[11] <- c("Insurance")
EDA$Insurance <- ifelse(EDA$Insurance == c("Blue Cross Commercial","Commercial LUHS","Insurance","Worker's Comp"), "Private",
                 ifelse(EDA$Insurance == c("Medicare","Medicare LUHS"),"Medicare","Medicaid"))

names(EDA)[15] <- c("StayDays")
names(EDA)[17] <- c("ASA")
EDA$ASA <- as.factor(EDA$ASA)

names(EDA)[18] <- c("OperationT")
names(EDA)[51] <- c("VarusValgus1")
names(EDA)[64] <- c("VarusValgus2")
EDA$VarusValgus1 <- as.factor(EDA$VarusValgus1)
EDA$VarusValgus2 <- as.factor(EDA$VarusValgus2)

EDA<- EDA %>% mutate(Days2TKA= as.integer(difftime(`Date of contralateral TKA`,surgery_date, units = "days")))
names(EDA)[56] <- c("DateMUA")
EDA$DateMUA <- ifelse(EDA$DateMUA == "other knee", NA,EDA$DateMUA)
EDA$DateMUA<- as.Date(as.integer(EDA$DateMUA),origin= "1899-12-30")
EDA<- EDA %>% mutate(DaysTKAMUA= difftime(DateMUA,surgery_date, units = "days"))




#############################################################################################
##### sex, age, race, ethnicity, insurance summary
EDA_1 <- EDA %>%  select(sex, age, race, ethnicity,Insurance, MUA_type)
EDA_1 %>% tbl_summary(by = MUA_type, 
  statistic = c(age)~"{mean}({min},{max})")  %>% 
  add_n() %>% modify_header(label ~ "**Variable**") %>%
  bold_labels()

# sex 
EDA %>% group_by(MUA_type) %>% count(sex)
ggplot(data = EDA) +
    geom_bar(mapping = aes(x = MUA_type, fill=sex), position = position_dodge(preserve = "single"))+
    labs(x = NULL)

# age
EDA %>% group_by(MUA_type) %>% summarise(range(age))
ggplot(data = EDA, aes(x = age, y = MUA_type)) +
  geom_boxplot(aes(fill=MUA_type))+
  geom_point(aes(group=MUA_type), position = position_dodge(preserve = "single")) +
  coord_flip()+
  labs(y = NULL, fill=NULL)

# race
EDA %>% group_by(MUA_type) %>% tbl_summary()
ggplot(data = EDA) +
  geom_bar(mapping = aes(fill = race, x=MUA_type),position = position_dodge(preserve = "single"))+
  labs(x = NULL)

# ethnicity
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = MUA_type, fill=ethnicity),position = position_dodge(preserve = "single"))+
  labs(x = NULL) 

# financial_class (insurance type) 
#EDA$Insurance <- ifelse(EDA$Insurance == c("Blue Cross Commercial","Commercial LUHS","Insurance","Worker's Comp"), "Private",
#                 ifelse(EDA$Insurance == c("Medicare","Medicare LUHS"),"Medicare","Medicaid"))

ggplot(data = EDA) +
  geom_bar(mapping = aes(x = MUA_type, fill=Insurance),position = position_dodge(preserve = "single"))+
  labs(x = NULL) 


############### health info EDA ##################
##### BMI, tobacco, ASA, ethnicity, insurance summary
EDA_2 <- EDA %>%  select(BMI, tobacco, ASA,  MUA_type)
EDA_2 %>% tbl_summary(by = MUA_type, 
  statistic = c(BMI)~"{mean}({min},{max})")  %>% 
  add_n() %>% modify_header(label ~ "**Variable**") %>%
  bold_labels()

# BMI (https://www.cdc.gov/healthyweight/assessing/index.html#:~:text=If%20your%20BMI%20is%20less,falls%20within%20the%20obese%20range.)
# if your BMI is higher than 30, obese range. If your BMI is 25.0 to 29.9, it falls within the overweight range.
ggplot(data = EDA, aes(x = BMI, y = MUA_type)) +
  geom_boxplot(aes(fill=MUA_type))+
  geom_point(aes(group=MUA_type), position = position_dodge(preserve = "single")) +
  coord_flip()+
  labs(y = NULL, fill=NULL)

# tobacco 
ggplot(data = EDA) +
  geom_bar(mapping = aes(fill = tobacco, x=MUA_type), position = position_dodge(preserve = "single"))+
  labs(x = NULL, fill=NULL)

# ASA
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = MUA_type, fill=ASA), position = "dodge")+
  labs(x = NULL, fill=NULL)

############
# categorical PCA? 
apply(EDA[,c(20,24:27,29:38)],2,sum) # using BMI more make sense.. 
comorbities<-as.data.frame(cbind(as.character(EDA$MUA_type),apply(EDA[,c(20:41)],1,sum))) # only 12 out of 33 patients have diagnosed more than 1 
colnames(comorbities) <- c("MUA_type","# of diagnosed Disease")
comorbities %>% tbl_summary(by = MUA_type) %>% bold_labels()

# Jason code 
library(ggcorrplot)
names(EDA)
comorb_2 = EDA[,c(20,24:27, 29:38,71:72)] # Diabetes_cc is included in Diabetes_no_cc
ggcorrplot(cor(comorb_2), title = "Correlation matrix of comorbities of patients who underwent MUA",
  colors = c("#6D9EC1", "white", "#E46726"),
  type = "lower"
  )
# I really don't think we can find any meaningful relationship in these information.

########################### about procedure EDA ###############################


# " Length of stay (days)" 
ggplot(data = EDA) +
  geom_point(mapping = aes(x = StayDays, y = MUA_type, color=MUA_type))+
  coord_flip()

ggplot(data = EDA,mapping = aes(x = StayDays, y = MUA_type, color=MUA_type)) +
  geom_point() +   coord_flip()


# operation time
ggplot(data = EDA,mapping = aes(x =OperationT, y = MUA_type, color=MUA_type)) +
geom_point() +   coord_flip() + 
  labs(x = "Operation time", y = NULL, color = NULL)

# The # days between 2 TKAs 
#EDA<- EDA %>% mutate(Days2TKA= as.integer(difftime(`Date of contralateral TKA`,surgery_date, units = "days")))

ggplot(data = EDA,mapping = aes(x =Days2TKA, y = MUA_type, color=MUA_type)) +
  geom_point() +   coord_flip() + 
  labs(x = "the # days between 2 TKAs", y = NULL, color = NULL)

ggplot(data = EDA,mapping = aes(x =Days2TKA, y = MUA_type, color=MUA_type)) +
  geom_boxplot() +   coord_flip() + 
  labs(x = "Days2TKA", y = NULL, color = NULL)

# varus/valgus ((normal=0, varus=1, valgus=2))

ggplot(data = EDA) +
  geom_bar(mapping = aes(x = MUA_type, fill=VarusValgus1), position = "dodge") + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Varus/Valgus (normal=0, varus=1, valgus=2)" )

# The # days between TKA & MUA  

#EDA$DateMUA <- ifelse(EDA$DateMUA == "other knee", NA,EDA$DateMUA)
#EDA$DateMUA<- as.Date(as.integer(EDA$DateMUA),origin= "1899-12-30")
#EDA<- EDA %>% mutate(DaysTKAMUA= difftime(DateMUA,surgery_date, units = "days"))

ggplot(data = EDA,mapping = aes(x =DaysTKAMUA, y = MUA_type, color=MUA_type)) +
  geom_point() +   coord_flip() + 
  labs(x = "the # days between TKA and MUA", y = NULL, color = NULL)


# The difference between pre-op ROM and post-op ROM (1)
names(EDA)
names(EDA)[c(49,52,54)] <- c("PreROM1", "PostROM11","PostROM12")
EDA <- EDA %>% mutate(D_PrePostROM= PostROM11-PreROM1)
EDA$MUA <- as.factor(EDA$MUA)
EDA$C_MUA <- as.factor(EDA$C_MUA)

# Q: is there any relation between the difference between preROM1-PostROM11 and MUA?

ggplot(data = EDA,mapping = aes(x =D_PrePostROM, y = MUA, color=MUA)) +
  geom_point() + coord_flip() + 
  labs(x = "Post ROM - Pre ROM", y = "0: No MUA, 1: MUA", color = NULL)


#### The difference between 135 - pre-op ROM
EDA<-EDA %>% mutate(D_135preROM= 135-PreROM1)

# Q: is there any relation between the difference between 135-preROM1 and MUA?
ggplot(data = EDA,mapping = aes(x =D_135preROM, y = MUA, color=MUA)) +
  geom_point() + coord_flip() + 
  labs(x = "135 - Pre ROM", y = "0: No MUA, 1: MUA", color = NULL)

# Q: is there any relation between the difference between 135-preROM1 and C_MUA?
ggplot(data = EDA,mapping = aes(x =D_135preROM, y = C_MUA, color=C_MUA)) +
  geom_point() + coord_flip() + 
  labs(x = "135 - Pre ROM", y = "0: No C_MUA, 1: C_MUA", color = NULL)

