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
EDA$MUA<- as.factor(EDA$MUA)
EDA$C_MUA<- as.factor(EDA$C_MUA)

names(EDA)[13] <- c("Insurance")
EDA$Insurance <- 
  ifelse(EDA$Insurance %in% c("Blue Cross Commercial","Commercial LUHS","Insurance","Worker's Comp","Worker's Comp LUHS"), "Private",
    ifelse(EDA$Insurance %in% c("Medicare","Medicare LUHS","Managed Medicare"),"Medicare",
      ifelse(EDA$Insurance %in% c("Managed Medicaid","Medicaid","MMAI"),"Medicaid","Uninsured")))


names(EDA)[77] <- c("Insurance_C_TKA")
EDA$Insurance_C_TKA <- 
  ifelse(EDA$Insurance_C_TKA %in% c("Blue Cross Commercial","Commercial LUHS","Insurance","Worker's Comp","Worker's Comp LUHS"), "Private",
    ifelse(EDA$Insurance_C_TKA %in% c("Medicare","Medicare LUHS","Managed Medicare"),"Medicare",
      ifelse(EDA$Insurance_C_TKA %in% c("Managed Medicaid","Medicaid","MMAI"),"Medicaid","Uninsured")))




##################################################

### Sex 

#EDA %>% group_by(MUA_type) %>% count(sex)
#ggplot(data = EDA) +
#  geom_bar(mapping = aes(x = MUA_type, fill=sex), position = position_dodge(preserve = "single"))+
#  labs(x = NULL)

  ggplot(data = EDA) +
    geom_bar(mapping = aes(x = Group2, fill=sex), position = position_dodge(preserve = "single"))+
    labs(x = NULL) 

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
ggplot(data = EDA, aes(x = age_C_TKA, y = C_MUA)) +
  geom_boxplot(aes(fill=C_MUA))+
  geom_point(aes(group=C_MUA), position = position_dodge(preserve = "single")) +
  coord_flip()+
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")

### race (I need idea how to organize it better)
ggplot(data = EDA) +
  geom_bar(mapping = aes(fill = race, x=MUA_type),position = position_dodge(preserve = "single"))+
  labs(x = NULL, title = "Race")


### ethnicity (prefers not to answer..?)
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = Group2, fill=ethnicity),position = position_dodge(preserve = "single"))+
  labs(x = NULL, title = "Ethnicity" ) 

# proportion of MUA in each gender
EDA %>% group_by(ethnicity) %>%  count(Group2) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = Group2, y = pct, fill = Group2, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  facet_wrap(~ethnicity) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 3)+ 
  labs(x = NULL, y=NULL)+
  theme(legend.position="none")

### Insurance
# financial_class (insurance type) 
#EDA$Insurance <- ifelse(EDA$Insurance == c("Blue Cross Commercial","Commercial LUHS","Insurance","Worker's Comp"), "Private",
#                 ifelse(EDA$Insurance == c("Medicare","Medicare LUHS"),"Medicare","Medicaid"))

# when the 1st TKA 
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = MUA, fill=Insurance),position = position_dodge(preserve = "single"))+
  labs(x = NULL) 

# proportion of MUA in each insurance type
EDA %>% group_by(Insurance) %>%  count(MUA) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = MUA, y = pct, fill = MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  facet_wrap(~Insurance) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 3)+ 
  labs(x = NULL, y=NULL)+
  theme(legend.position="none")


# when the 2nd TKA 
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = C_MUA, fill=Insurance_C_TKA),position = position_dodge(preserve = "single"))+
  labs(x = NULL) 

# proportion of MUA in each insurance type
EDA %>% group_by(Insurance_C_TKA) %>%  count(C_MUA) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = C_MUA, y = pct, fill = C_MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  facet_wrap(~Insurance_C_TKA) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 3)+ 
  labs(x = NULL, y=NULL)+
  theme(legend.position="none")


############### health info EDA ###############################################

# BMI (https://www.cdc.gov/healthyweight/assessing/index.html#:~:text=If%20your%20BMI%20is%20less,falls%20within%20the%20obese%20range.)
# if your BMI is higher than 30, obese range. If your BMI is 25.0 to 29.9, it falls within the overweight range.
# First TKA
ggplot(data = EDA, aes(x = BMI, y = MUA)) +
  geom_boxplot(aes(fill=MUA))+
  #geom_point(aes(group=MUA), position = position_dodge(preserve = "single")) +
  coord_flip()+
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")

# Second TKA
ggplot(data = EDA, aes(x = bmi_C_TKA, y = C_MUA)) +
  geom_boxplot(aes(fill=C_MUA))+
  #geom_point(aes(group=C_MUA), position = position_dodge(preserve = "single")) +
  coord_flip()+
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")


#### tobacco 
ggplot(data = EDA) +
  geom_bar(mapping = aes(fill = tobacco, x=MUA), position = position_dodge(preserve = "single"))+
  labs(x = NULL, fill=NULL)

ggplot(data = EDA) +
  geom_bar(mapping = aes(fill = tobacco_C_TKA, x=C_MUA), position = position_dodge(preserve = "single"))+
  labs(x = NULL, fill=NULL)

#### ASA

# when 1st TKA (some NA...)
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = MUA, fill=ASA), position = "dodge")+
  labs(x = NULL, fill=NULL)

# proportion of MUA in each ASA rate
EDA %>% group_by(ASA) %>%  count(MUA) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = MUA, y = pct, fill = MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  facet_wrap(~ASA) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 3)+ 
  labs(x = NULL, y=NULL)+
  theme(legend.position="none")


# when 2nd TKA 
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = C_MUA, fill=ASA_C_TKA), position = "dodge")+
  labs(x = NULL, fill=NULL)

# proportion of MUA in each ASA rate
EDA %>% group_by(ASA_C_TKA) %>%  count(C_MUA) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = C_MUA, y = pct, fill = C_MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  facet_wrap(~ASA_C_TKA) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 3)+ 
  labs(x = NULL, y=NULL)+
  theme(legend.position="none")


### comorbities
names(EDA)
EDA$MUA<- as.numeric(ifelse(EDA$MUA=="Yes",1,0))
comorb_1 = EDA[,c(3,23:30,33:44)] # Diabetes_cc is included in Diabetes_no_cc
ggcorrplot(cor(comorb_1), title = "Correlation matrix of comorbities of patients who underwent MUA",
  colors = c("#6D9EC1", "white", "#E46726"),
  type = "lower"
)

EDA$C_MUA<- as.numeric(ifelse(EDA$C_MUA=="Yes",1,0))

comorb_2 = EDA[,c(4,82:89,91:103)] # Diabetes_cc is included in Diabetes_no_cc
ggcorrplot(cor(comorb_2), title = "Correlation matrix of comorbities of patients who underwent MUA",
  colors = c("#6D9EC1", "white", "#E46726"),
  type = "lower"
)


##################################################################################
#### about procedure EDA

### Stay Days

EDA$C_MUA<- as.factor(ifelse(EDA$C_MUA==1,"Yes","No"))
EDA$MUA<- as.factor(ifelse(EDA$MUA==1,"Yes","No"))


ggplot(data = EDA, aes(x = los, y=MUA)) +
  geom_boxplot(aes(fill=MUA))+
  #geom_point(aes(group=MUA), position = position_dodge(width = 0.75)) +
  coord_flip()+
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")

ggplot(data = EDA, aes(x = los_C_TKA, y=C_MUA)) +
  geom_boxplot(aes(fill=C_MUA))+
  #geom_point(aes(group=C_MUA), position = position_dodge(width = 0.75)) +
  coord_flip()+
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")


### operation hours

# 1st TKA some missing value 
ggplot(data = EDA, aes(x = op_time, y=MUA)) +
  geom_boxplot(aes(fill=MUA))+
  #geom_point(aes(group=MUA), position = position_dodge(width = 0.75)) +
  coord_flip()+
  labs(x = "1st TKA Operation Time (mins)",y = NULL, fill=NULL)+ theme(legend.position="none")

# 2nd TKA  
ggplot(data = EDA, aes(x = op_time_C_TKA, y=C_MUA)) +
  geom_boxplot(aes(fill=C_MUA))+
  #geom_point(aes(group=C_MUA), position = position_dodge(width = 0.75)) +
  coord_flip()+
  labs(x = "2nd TKA Operation Time (mins)",y = NULL, fill=NULL)+ theme(legend.position="none")


