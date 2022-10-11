library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gtsummary)
library(ggcorrplot)
####################################################################### 
# Data Setup
#######################################################################

EDA <- read_excel("DeID2.xlsx", sheet = "2TKA_w.syn", range = cell_rows(1:34))
EDA <- EDA %>% mutate(MUA_type = ifelse(MUA==1&C_MUA!=1, 
  "MUA",ifelse(MUA!=1&C_MUA==1, "C_MUA","Both")))
EDA$MUA_type <- factor(EDA$MUA_type, levels = c("MUA","C_MUA","Both"))
EDA$C_MUA<-factor(EDA$C_MUA)
EDA$MUA<- factor(EDA$MUA)


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

names(EDA)[c(49,52,54)] <- c("PreROM1", "PostROM11","PostROM12") #PreROM1 : PreROM before 1st TKA, PostROM11 is 1st post ROM after 1st TKA, PostROM12 is 2nd post ROM after 1st TKA
EDA <- EDA %>% mutate(D_PrePostROM= PostROM11-PreROM1) 

EDA$ID <- c(1:33)
level_order <- c("PreROM1","PostROM11") 

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
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")


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
##### BMI, tobacco, ASA summary
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
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")


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
#library(ggcorrplot)
comorb_2 = EDA[,c(20,24:27, 29:38,71:72)] # Diabetes_cc is included in Diabetes_no_cc
ggcorrplot(cor(comorb_2), title = "Correlation matrix of comorbities of patients who underwent MUA",
  colors = c("#6D9EC1", "white", "#E46726"),
  type = "lower"
  )
# I really don't think we can find any meaningful relationship in these information.

##################################################################################
#### about procedure EDA

##### "length pf stay", operation time, vagus/valgus,2TKA, 2ROM summary
names(EDA)
EDA_3 <- EDA %>%  select(OperationT,VarusValgus1,Days2TKA,PreROM1,PostROM11,MUA_type)
EDA_3 %>% tbl_summary(by = MUA_type, 
  statistic = c(OperationT,Days2TKA,PreROM1,PostROM11)~"{mean}({min},{max})")  %>% 
  add_n() %>% modify_header(label ~ "**Variable**") %>%
  bold_labels()


# " Length of stay (days)" 
ggplot(data = EDA) +
  geom_point(mapping = aes(x = StayDays, y = MUA_type, color=MUA_type))+
  coord_flip()

ggplot(data = EDA,mapping = aes(x = StayDays, y = MUA_type, color=MUA_type)) +
  geom_boxplot() +   coord_flip()  ##??????

ggplot(data = EDA, aes(x = StayDays, y=MUA_type)) +
  geom_boxplot(aes(fill=MUA_type))+
  geom_point(aes(group=MUA_type), position = position_dodge(width = 0.75)) +
  coord_flip()+
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")

# operation time
ggplot(data = EDA, aes(x = OperationT, y=MUA)) +
  geom_boxplot(aes(fill=MUA))+
  geom_point(aes(group=MUA), position = position_dodge(width = 0.75)) +
  coord_flip()+
  labs(x = "1st TKA Operation Time",y = NULL, fill=NULL)+ theme(legend.position="none")

ggplot(data = EDA, aes(x = OperationT, y=MUA_type)) +
  geom_boxplot(aes(fill=MUA_type))+
  geom_point(aes(group=MUA_type), position = position_dodge(width = 0.75)) +
  coord_flip()+
  labs(x = "1st TKA Operation Time",y = "0: NO MUA,          1: MUA", fill=NULL)+ theme(legend.position="none")




# varus/valgus ((normal=0, varus=1, valgus=2))
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = MUA_type, fill=VarusValgus1), position = "dodge") + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Varus/Valgus (normal=0, varus=1, valgus=2)" )


# The # days between 2 TKAs 
#EDA<- EDA %>% mutate(Days2TKA= as.integer(difftime(`Date of contralateral TKA`,surgery_date, units = "days")))

ggplot(data = EDA, aes(x = Days2TKA, y = MUA_type)) +
  geom_boxplot(aes(color=MUA_type))+
  geom_point(aes(group=MUA_type), position = position_dodge(preserve = "single")) +
  coord_flip()+
  labs(y = NULL, color=NULL)+ theme(legend.position="none")
# 17 out of 20 MUA only patient got MUA before the 2nd TKA 
# 2 out of 20 MUA only patient got MUA on the 2nd TKA surgery day
# 1 out of 20 MUA only patient got MUA after the 2nd TKA
# I think this plot is meaningless. 


# Q The number of days between two TKAs is a risk factor of contralateral TKA? 
EDA$C_MUA<- as.factor(EDA$C_MUA)
ggplot(data = EDA, aes(x = Days2TKA, y = C_MUA)) +
  geom_boxplot(aes(color=C_MUA))+
  geom_point(aes(group=C_MUA), position = position_dodge(preserve = "single")) +
  coord_flip()+
  labs(y = c("0:No C_MUA            1:C_MUA"), color=NULL)+ theme(legend.position="none")


###### The # days between TKA & MUA  <- what's the point? .. forget about it. 
#EDA$DateMUA <- ifelse(EDA$DateMUA == "other knee", NA,EDA$DateMUA)
#EDA$DateMUA<- as.Date(as.integer(EDA$DateMUA),origin= "1899-12-30")
#EDA<- EDA %>% mutate(DaysTKAMUA= difftime(DateMUA,surgery_date, units = "days"))
#
#ggplot(data = EDA,mapping = aes(x =DaysTKAMUA, y = MUA_type, color=MUA_type)) +
#  geom_point() +   coord_flip() + 
#  labs(x = "the # days between TKA and MUA", y = NULL, color = NULL)

# The difference between pre-op ROM and post-op ROM (1)
# data setup 
#EDA <- EDA %>% mutate(D_PrePostROM= PostROM11-PreROM1) PreROM1 : PreROM before 1st TKA, PostROM11 is 1st post ROM after 1st TKA

# Q: is there any relation between the difference between preROM1-PostROM11 and MUA?
ggplot(data = EDA,mapping = aes(PreROM1,PostROM11))+ #, y = MUA, color=MUA)) +
  geom_line() + coord_flip()  
  #labs(x = "Post ROM - Pre ROM", y = "0: No MUA, 1: MUA", color = NULL)

ggplot(data = EDA,mapping = aes(D_PrePostROM, y = MUA, color=MUA)) +
  geom_point() + coord_flip() + 
  labs(x = "Post ROM - Pre ROM", y = "0: No MUA, 1: MUA", color = NULL)+
  facet_grid(cols=MUA)

names(EDA)
EDA %>% 
pivot_longer(c(PreROM1,PostROM11), names_to = "time", values_to = "ROM") %>%
ggplot(aes(x=time, y= ROM)) +
  geom_point() #+ coord_flip() 

#EDA$ID <- c(1:33)
level_order <- c("PreROM1","PostROM11") 
EDA %>% 
  mutate(sign= ifelse(PreROM1>PostROM11,"Down","Up" )) %>% 
  select(ID,sign,PreROM1,PostROM11,MUA_type) %>% 
  pivot_longer(c(PreROM1,PostROM11), names_to = "time", values_to = "ROM") %>% 
  ggplot(aes(x=ROM, y=time, group = ID, color = sign), position = "dodge") +
  geom_line(size = 0.5) + 
  scale_y_discrete(limits = level_order)+
  geom_vline(aes(xintercept=135), col = "blue")+
  coord_flip() +
  facet_wrap(facets =  vars(MUA_type))

# I think this plot is better. 
EDA %>% 
  mutate(sign= ifelse(PreROM1>PostROM11,"Down","Up" )) %>% 
  select(ID,sign,PreROM1,PostROM11,MUA_type,MUA) %>% 
  pivot_longer(c(PreROM1,PostROM11), names_to = "time", values_to = "ROM") %>% 
  ggplot(aes(x=ROM, y=time, group = ID, color = sign), position = "dodge") +
  geom_line(size = 0.5) + 
  scale_y_discrete(limits = level_order)+
  geom_vline(aes(xintercept=135), col = "blue")+
  coord_flip() +
  facet_wrap(facets =  vars(MUA))+
  labs(y = NULL, color=NULL, title = "MUA vs the difference between preROM and postROM ")+
  theme(legend.position="none")

# Q: is there any relation between the difference between 135-preROM1 and MUA?
# Q: is there any relation between the difference between 135-preROM1 and C_MUA?
