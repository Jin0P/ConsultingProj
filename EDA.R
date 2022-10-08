library(readxl)
library(ggplot2)
library(tidyverse)
EDA <- read_excel("DeID2.xlsx", sheet = "2TKA_w.syn", range = cell_rows(1:34))

EDA <- EDA %>% mutate(MUA_type = ifelse(MUA==1&C_MUA!=1, 
  "MUA",ifelse(MUA!=1&C_MUA==1, "C_MUA","Both")))

EDA$MUA_type <- factor(EDA$MUA_type, levels = c("MUA","C_MUA","Both"))


# overall summary

EDA %>% select(sex, MUA_type, age,BMI) %>% summary()
EDA %>% count(sex)



# sex 
ggplot(data = EDA) +
    geom_bar(mapping = aes(x = sex, fill=sex))+
    facet_wrap(MUA_type~., nrow =2)
  
ggplot(data = EDA) +
    geom_bar(mapping = aes(x = sex, fill=sex))

# age
ggplot(data = EDA) +
  geom_boxplot(mapping = aes(x = age, y = MUA_type, color=MUA_type))+
  coord_flip()

# race
EDA$race <- ifelse(EDA$race == "American Indian", "Other",EDA$race)
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = race, fill=race))+
  facet_wrap(MUA_type~., nrow =2)

# ethnicity
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = ethnicity, fill=ethnicity))+
  facet_wrap(MUA_type~., nrow =1) 

# BMI (https://www.cdc.gov/healthyweight/assessing/index.html#:~:text=If%20your%20BMI%20is%20less,falls%20within%20the%20obese%20range.)
# if your BMI is higher, obese range. If your BMI is 25.0 to 29.9, it falls within the overweight range.
names(EDA)[8] <- c("BMI")
ggplot(data = EDA) +
  geom_boxplot(mapping = aes(x = BMI, y = MUA_type, color=MUA_type))+
  coord_flip()

# tobacco 
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = tobacco, fill=tobacco))+
  facet_wrap(MUA_type~., nrow =2)

# financial_class (insurance type) ??? 
names(EDA)[11] <- c("Insurance")
ggplot(data = EDA) +
  geom_bar(mapping = aes(x = Insurance, fill=Insurance))+
  facet_wrap(MUA_type~., nrow =2)

# " Length of stay (days)" 
names(EDA)[15] <- c("StayDays")
ggplot(data = EDA) +
  geom_point(mapping = aes(x = StayDays, y = MUA_type, color=MUA_type))+
  coord_flip()

ggplot(data = EDA,mapping = aes(x = StayDays, y = MUA_type, color=MUA_type)) +
  geom_point() +   coord_flip()

# ASA
names(EDA)[17] <- c("ASA")
EDA$ASA <- as.factor(EDA$ASA)

ggplot(data = EDA) +
  geom_bar(mapping = aes(x = MUA_type, fill=ASA), position = "dodge")

#ggplot(data = EDA, aes(x = "", y =ASA, fill=ASA)) +
#  geom_col() +
#  coord_polar("y", start = 0) +
#  facet_grid(.~MUA_type)

# operation time
names(EDA)[18] <- c("OperationT")
ggplot(data = EDA,mapping = aes(x =OperationT, y = MUA_type, color=MUA_type)) +
geom_point() +   coord_flip() + 
  labs(x = "Operation time", y = NULL, color = NULL)

# categorical PCA? 
apply(EDA[,c(20:41)],2,sum) # using BMI more make sense.. 


# Jason code 
library(ggcorrplot)
comorb_2 = EDA[,c(20, 24:38)] 
ggcorrplot(cor(comorb_2), title = "Correlation matrix of comorbities of patients who underwent MUA")


# The # days between 2 TKAs 
EDA<- EDA %>% mutate(Days2TKA= as.integer(difftime(`Date of contralateral TKA`,surgery_date, units = "days")))

ggplot(data = EDA,mapping = aes(x =Days2TKA, y = MUA_type, color=MUA_type)) +
  geom_point() +   coord_flip() + 
  labs(x = "the # days between 2 TKAs", y = NULL, color = NULL)

ggplot(data = EDA,mapping = aes(x =Days2TKA, y = MUA_type, color=MUA_type)) +
  geom_boxplot() +   coord_flip() + 
  labs(x = "Days2TKA", y = NULL, color = NULL)

# varus/valgus ((normal=0, varus=1, valgus=2))
names(EDA)[51] <- c("VarusValgus1")
names(EDA)[64] <- c("VarusValgus2")

EDA$VarusValgus1 <- as.factor(EDA$VarusValgus1)
EDA$VarusValgus2 <- as.factor(EDA$VarusValgus2)

ggplot(data = EDA) +
  geom_bar(mapping = aes(x = MUA_type, fill=VarusValgus1), position = "dodge") + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Varus/Valgus (normal=0, varus=1, valgus=2)" )

# The # days between TKA & MUA  
EDA<- EDA %>% mutate(DaysTKAMUA= as.integer(difftime(`Date of MUA`,surgery_date, units = "days")))


