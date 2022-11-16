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

EDA$Group2<-ifelse(EDA$Group2=="MUA",paste0("Yes_",EDA$Group2),paste(EDA$Group2))


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

# date_diff 
EDA$date_diff =as.Date(EDA$`Date of contralateral TKA`) - as.Date(EDA$surgery_date)
EDA %>% select(surgery_date,`Date of contralateral TKA`,date_diff)
EDA$date_diff <- ifelse(EDA$date_diff <0, -EDA$date_diff,EDA$date_diff)

#race
EDA$redu_race <- ifelse(EDA$race %in% c("Hispanic","Multiracial","Other","Preference not indicated","American Indian"), "Ohter",EDA$race)




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




######################### age

ggplot(data = EDA, aes(x = age_C_TKA, fill=C_MUA)) +
  geom_histogram(binwidth = 4,color="#e9ecef")+
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")

ggplot(data = EDA, aes(x = age_C_TKA, y = C_MUA)) +
  geom_boxplot(aes(fill=C_MUA))+
 # geom_point(aes(group=C_MUA), position = position_dodge(preserve = "single")) +
  coord_flip()+
  labs(y = NULL, fill=NULL)+ theme(legend.position="none")

# proportion of C_MUA when the patients are in 50s or 60s 8/391 : 0.02046036
EDA %>% filter(age_C_TKA >50 & age_C_TKA <70) %>%  count(C_MUA) %>% # No:391/Yes: 8
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = C_MUA, y = pct, fill = C_MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  #facet_wrap(~ MUA) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 5)+ 
  labs(x = "Contraletral MUA", y=NULL)+
  theme(legend.position="none")

# proportion of C_MUA when the patients are in 40s or over 70s  2/241 : 0.008298755
EDA %>% filter(age_C_TKA <50 | age_C_TKA >70) %>%  count(C_MUA) %>%  # No:241/Yes: 2
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = C_MUA, y = pct, fill = C_MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  #facet_wrap(~ MUA) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 5)+ 
  labs(x = "Contraletral MUA", y=NULL)+
  theme(legend.position="none")



######################### race
EDA$redu_race <- ifelse(EDA$race %in% c("Hispanic","Multiracial","Other","Preference not indicated","American Indian"), "Ohter",EDA$race)

ggplot(data = EDA) +
  geom_bar(mapping = aes(fill = redu_race, x=MUA_type),position = position_dodge(preserve = "single"))+
  labs(x = NULL, title = "Race")


# proportion of C_MUA by race 
EDA %>% group_by(redu_race) %>%  count(C_MUA) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = C_MUA, y = pct, fill = C_MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  facet_wrap(~redu_race) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 3)+ 
  labs(x = NULL, y=NULL)+
  theme(legend.position="none")


# proportion of MUA by race 
EDA %>% group_by(redu_race) %>%  count(MUA) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = MUA, y = pct, fill = MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  facet_wrap(~redu_race) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 3)+ 
  labs(x = NULL, y=NULL)+
  theme(legend.position="none")


######################### ethnicity 
EDA$ethnicity <- ifelse(EDA$ethnicity %in% c("Prefers not to answer"), "Non-Hispanic Origin",EDA$ethnicity)

ggplot(data = EDA) +
  geom_bar(mapping = aes(x = Group2, fill=ethnicity),position = position_dodge(preserve = "single"))+
  labs(x = NULL, title = "Ethnicity" ) 

# proportion of any MUA in each ethnicity
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

# proportion of C_MUA in each ethnicity
EDA %>% group_by(ethnicity) %>%  count(C_MUA) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = C_MUA, y = pct, fill = C_MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  facet_wrap(~ethnicity) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 3)+ 
  labs(x = NULL, y=NULL)+
  theme(legend.position="none")

# proportion of MUA in each ethnicity
EDA %>% group_by(ethnicity) %>%  count(MUA) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = MUA, y = pct, fill = MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  facet_wrap(~ethnicity) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 3)+ 
  labs(x = NULL, y=NULL)+
  theme(legend.position="none")

## two results(plots) of MUA vs ethnicity and  C_MUA vs ethnicity seems so different.. 




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

EDA %>% select(BMI,MUA ) %>% 
  tbl_summary(by = MUA) %>% #, statistic = c(age)~"{mean}({min},{max})")  %>% 
   modify_header(label ~ "**Variable**") %>%
  bold_labels()

EDA %>% select(bmi_C_TKA,C_MUA ) %>% 
  tbl_summary(by = C_MUA) %>% #, statistic = c(age)~"{mean}({min},{max})")  %>% 
  modify_header(label ~ "**Variable**") %>%
  bold_labels()


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
#### about procedure EDA : los, operation time, # days between 2 TKAs

### Stay Days ###

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


################################  operation hours ###

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


################################ The # of dates between TKAs ###
ggplot(data = EDA, aes(x = date_diff, y=C_MUA)) +
  geom_boxplot(aes(fill=C_MUA))+
  #geom_point(aes(group=C_MUA), position = position_dodge(width = 0.75)) +
  coord_flip()
  #labs(x = "2nd TKA Operation Time (mins)",y = NULL, fill=NULL)+ theme(legend.position="none")


ggplot(data = EDA, aes(x = date_diff, y=Group2)) +
  geom_boxplot(aes(fill=Group2))+
  #geom_point(aes(group=C_MUA), position = position_dodge(width = 0.75)) +
  coord_flip()
#labs(x = "2nd TKA Operation Time (mins)",y = NULL, fill=NULL)+ theme(legend.position="none")

ggplot(data = EDA, aes(x = date_diff, y=MUA)) +
  geom_boxplot(aes(fill=MUA))+
  #geom_point(aes(group=C_MUA), position = position_dodge(width = 0.75)) +
  coord_flip()
#labs(x = "2nd TKA Operation Time (mins)",y = NULL, fill=NULL)+ theme(legend.position="none")





EDA %>%  group_by(C_MUA) %>% summarise(mean(date_diff), median(date_diff))

EDA %>% filter(date_diff < 365) %>% 
  ggplot(aes(x = date_diff, y=C_MUA)) +
  geom_boxplot(aes(fill=C_MUA))+
  #geom_point(aes(group=C_MUA), position = position_dodge(width = 0.75)) +
  coord_flip()


# proportion of getting C_MAU when the # of dates between 2TKA < 1yr 
EDA %>% filter(date_diff < 365) %>%  count(C_MUA) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = C_MUA, y = pct, fill = C_MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 5)+ 
  labs(x = NULL, y=NULL)+
  theme(legend.position="none")


# proportion of getting C_MAU when the patients got bilateral TKA depending on MUA 
EDA %>% filter(date_diff == 0) %>%  group_by(MUA) %>% count(C_MUA) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = C_MUA, y = pct, fill = C_MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  facet_wrap(~ MUA) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 5)+ 
  labs(x = "Contraletral MUA", y=NULL)+
  theme(legend.position="none")

# proportion of getting C_MAU when the patients got bilateral TKA (97%:3%)
EDA %>% filter(date_diff == 0)  %>% count(C_MUA) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = C_MUA, y = pct, fill = C_MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  #facet_wrap(~ MUA) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 5)+ 
  labs(x = "Contraletral MUA", y=NULL)+
  theme(legend.position="none")

# proportion of getting MAU when the patients got bilateral TKA (98%:2%)
EDA %>% filter(date_diff == 0)  %>% count(MUA) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = MUA, y = pct, fill = MUA, label = scales::percent(pct))) +
  geom_col( position = "dodge")+
  #facet_wrap(~ MUA) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
    #  vjust = -0.5,    # nudge above top of bar
    size = 5)+ 
  labs(x = "MUA", y=NULL)+
  theme(legend.position="none")

