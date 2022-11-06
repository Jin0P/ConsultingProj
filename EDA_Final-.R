library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gtsummary)
library(ggcorrplot)

####################################################################### 
# Data Setup
#######################################################################

EDA <- read_excel("TKAMUA5_IDSET_Nov02.xlsx", sheet = "Final", range = cell_rows(3:668), col_names=TRUE)
EDA$MUA_type<-as.factor(EDA$MUA_type)
EDA$Group2<-as.factor(EDA$Group2)
EDA$ASA<- as.factor(EDA$ASA)
EDA$ASA_C_TKA<- as.factor(EDA$ASA_C_TKA)

names(EDA)[11] <- c("Insurance")
EDA$Insurance <- 
  ifelse(EDA$Insurance %in% c("Blue Cross Commercial","Commercial LUHS","Insurance","Worker's Comp","Worker's Comp LUHS"), "Private",
    ifelse(EDA$Insurance %in% c("Medicare","Medicare LUHS","Managed Medicare"),"Medicare",
      ifelse(EDA$Insurance %in% c("Managed Medicaid","Medicaid","MMAI"),"Medicaid","Uninsured")))


names(EDA)[75] <- c("Insurance_C_TKA")
EDA$Insurance_C_TKA <- 
  ifelse(EDA$Insurance_C_TKA %in% c("Blue Cross Commercial","Commercial LUHS","Insurance","Worker's Comp","Worker's Comp LUHS"), "Private",
    ifelse(EDA$Insurance_C_TKA %in% c("Medicare","Medicare LUHS","Managed Medicare"),"Medicare",
      ifelse(EDA$Insurance_C_TKA %in% c("Managed Medicaid","Medicaid","MMAI"),"Medicaid","Uninsured")))




##################################################

### Sex 

EDA %>% group_by(MUA_type) %>% count(sex)
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = MUA_type, fill=sex), position = position_dodge(preserve = "single"))+
  labs(x = NULL)

  ggplot(data = EDA) +
    geom_bar(mapping = aes(x = Group2, fill=sex), position = position_dodge(preserve = "single"))+
    labs(x = NULL) 

#ggplot(data = EDA,aes(x = Group2, 
#  y = prop.table(stat(count)), 
#  fill = Group2, 
#  label = scales::percent(prop.table(stat(count))))) +
#  geom_bar( position = "dodge")+
#  facet_wrap(~sex) +
#  geom_text(stat = 'count')+
#  scale_y_continuous(labels = scales::percent)+
#  labs(x = NULL)

# proportion of MUA in each gender 
EDA %>% group_by(sex) %>%  count(Group2) %>% 
  mutate(pct = prop.table(n)) %>% 
ggplot(aes(x = Group2, y = pct, fill = Group2, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  facet_wrap(~sex) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
  #  vjust = -0.5,    # nudge above top of bar
    size = 3)+ 
  labs(x = NULL, y=NULL)+
  theme(legend.position="none")


### age
ggplot(data = EDA, aes(x = age_C_TKA, y = MUA_type)) +
  geom_boxplot(aes(fill=MUA_type))+
  geom_point(aes(group=MUA_type), position = position_dodge(preserve = "single")) +
  coord_flip()+
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")


### Insurance
# financial_class (insurance type) 
#EDA$Insurance <- ifelse(EDA$Insurance == c("Blue Cross Commercial","Commercial LUHS","Insurance","Worker's Comp"), "Private",
#                 ifelse(EDA$Insurance == c("Medicare","Medicare LUHS"),"Medicare","Medicaid"))

ggplot(data = EDA) +
  geom_bar(mapping = aes(x = MUA_type, fill=Insurance),position = position_dodge(preserve = "single"))+
  labs(x = NULL) 

# proportion of MUA in each insurance type
EDA %>% group_by(Insurance) %>%  count(Group2) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Group2, y = pct, fill = Group2, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  facet_wrap(~Insurance) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 3)+ 
  labs(x = NULL, y=NULL)+
  theme(legend.position="none")


############### health info EDA ###############################################
names(EDA)

# BMI (https://www.cdc.gov/healthyweight/assessing/index.html#:~:text=If%20your%20BMI%20is%20less,falls%20within%20the%20obese%20range.)
# if your BMI is higher than 30, obese range. If your BMI is 25.0 to 29.9, it falls within the overweight range.
# First TKA
ggplot(data = EDA, aes(x = BMI, y = MUA_type)) +
  geom_boxplot(aes(fill=MUA_type))+
  geom_point(aes(group=MUA_type), position = position_dodge(preserve = "single")) +
  coord_flip()+
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")

# Second TKA
ggplot(data = EDA, aes(x = bmi_C_TKA, y = MUA_type)) +
  geom_boxplot(aes(fill=MUA_type))+
  geom_point(aes(group=MUA_type), position = position_dodge(preserve = "single")) +
  coord_flip()+
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")


#### tobacco 
ggplot(data = EDA) +
  geom_bar(mapping = aes(fill = tobacco, x=MUA_type), position = position_dodge(preserve = "single"))+
  labs(x = NULL, fill=NULL)

ggplot(data = EDA) +
  geom_bar(mapping = aes(fill = tobacco_C_TKA, x=MUA_type), position = position_dodge(preserve = "single"))+
  labs(x = NULL, fill=NULL)

#### ASA
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = MUA, fill=ASA), position = "dodge")+
  labs(x = NULL, fill=NULL)

ggplot(data = EDA) +
  geom_bar(mapping = aes(x = C_MUA, fill=ASA_C_TKA), position = "dodge")+
  labs(x = NULL, fill=NULL)


### comorbities
EDA$MUA<- as.numeric(ifelse(EDA$MUA=="Yes",1,0))
comorb_1 = EDA[,c(3,21:28,31:42)] # Diabetes_cc is included in Diabetes_no_cc
ggcorrplot(cor(comorb_1), title = "Correlation matrix of comorbities of patients who underwent MUA",
  colors = c("#6D9EC1", "white", "#E46726"),
  type = "lower"
)

EDA$C_MUA<- as.numeric(ifelse(EDA$C_MUA=="Yes",1,0))
comorb_2 = EDA[,c(4,80:87,90:101)] # Diabetes_cc is included in Diabetes_no_cc
ggcorrplot(cor(comorb_2), title = "Correlation matrix of comorbities of patients who underwent MUA",
  colors = c("#6D9EC1", "white", "#E46726")
)


##################################################################################
#### about procedure EDA

### Stay Days

ggplot(data = EDA, aes(x = los_C_TKA, y=C_MUA)) +
  geom_boxplot(aes(fill=C_MUA))+
  geom_point(aes(group=C_MUA), position = position_dodge(width = 0.75)) +
  coord_flip()+
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")

ggplot(data = EDA, aes(x = los, y=MUA)) +
  geom_boxplot(aes(fill=MUA))+
  geom_point(aes(group=MUA), position = position_dodge(width = 0.75)) +
  coord_flip()+
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")


### operation hours
ggplot(data = EDA, aes(x = op_time, y=MUA)) +
  geom_boxplot(aes(y=MUA))+
  geom_point(aes(group=MUA), position = position_dodge(width = 0.75)) +
  coord_flip()+
  labs(x = "1st TKA Operation Time (mins)",y = NULL, fill=NULL)+ theme(legend.position="none")


ggplot(data = EDA, aes(x = OperationT, y=MUA)) +
  geom_boxplot(aes(fill=MUA))+
  geom_point(aes(group=MUA), position = position_dodge(width = 0.75)) +
  coord_flip()+
  labs(x = "1st TKA Operation Time (mins)",y = "0: NO MUA,          1: MUA", fill=NULL)+ theme(legend.position="none")






