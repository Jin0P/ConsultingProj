############## (started with Jason's code)
#Model building

library(readxl)
library(tidyverse)
library(tree)
library(gam)
library(ggplot2)
library(glmnet)
library(fastDummies)
library(ggcorrplot)

#### data setup
MUA_data = read_excel("TKAMUA5_IDSET_Nov02.xlsx", sheet = "Final", range = cell_rows(3:668), col_names=TRUE)

#Changing MUA/C_MUA in to binary 1 or 0
MUA_data1 = mutate(MUA_data,
  MUA_bi= ifelse(MUA == "Yes", 1, 0),
  C_MUA_bi = ifelse(C_MUA == "Yes", 1, 0),
  MUA_count_T = ifelse(MUA_type == "Both", mua_count +1, mua_count),
  age_diff = age_C_TKA - age,
  los_total = los+los_C_TKA,
  op_time_total = op_time+ op_time_C_TKA,
  date_diff =as.Date(`Date of contralateral TKA`) - as.Date(surgery_date) )

MUA_data1$date_diff <- ifelse(MUA_data1$date_diff <0, -MUA_data1$date_diff,MUA_data1$date_diff)


MUA_data1 = MUA_data1[1:665,]


MUA_data1$C_MUA<- as.factor(MUA_data1$C_MUA)
MUA_data1$MUA<- as.factor(MUA_data1$MUA)

MUA_data1$redu_race <- ifelse(MUA_data1$race %in% c("Hispanic","Multiracial","Other","Preference not indicated","American Indian","Asian"), "Other",MUA_data1$race)



###################################################################################
### Q1) MUA is a risk factor of C_MUA?  - Yes
###################################################################################

#Model with  MUA predicting C_mua
mua_glm = glm(C_MUA_bi ~ MUA_bi, data= MUA_data1, family = "binomial")
summary(mua_glm)
# logit(odds) = -4.5061 + 2.5602*MUA_bi (0/1)
# if the patient got MUA, the odds of getting C_MUA is exp(-4.5061 + 2.5602) = 0.1428586
# if the patient didn't get MUA, the odds of getting C_MUA is exp(-4.5061) = 0.01104144

#Odds ratio
exp(mua_glm$coefficients[-1])  # 0.1428586/0.01104144
#Odds of receiving MUA on contralateral knee is almost 13x times larger than not

cor(MUA_data1$C_MUA_bi,MUA_data1$MUA_bi)

###################################################################################
### + Q2) is the # between 2 TKAs a risk factor? - No 
###################################################################################
#Model with  MUA predicting C_mua with time

cor(MUA_data1[MUA_data1$date_diff<645, ]$C_MUA_bi,MUA_data1[MUA_data1$date_diff<645, ]$date_diff)
cor(MUA_data1$C_MUA_bi,MUA_data1$date_diff)

mua_glm1 = glm(C_MUA ~ MUA +date_diff, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(C_MUA ~ MUA +date_diff + MUA*date_diff, data= MUA_data1, family = "binomial")
summary(mua_glm1)

# let's see patients who got the 2nd TKA within 645 days
summary(as.vector(MUA_data1$date_diff))   #3rd Q : 645
mua_glm1 = glm(C_MUA ~ date_diff, data= MUA_data1[MUA_data1$date_diff<645, ], family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(C_MUA ~ MUA+date_diff+ MUA*date_diff, data= MUA_data1[MUA_data1$date_diff<645, ], family = "binomial")
summary(mua_glm1)
# getting obvious that date_diff is insignificant 

# If the patient got Bilateral TKA, is the risk of getting MUA significantly higher? No. 
MUA_data1$BTKA <- ifelse(MUA_data1$date_diff==0, "Yes","No")   
mua_glm1 = glm(as.factor(Group2) ~ as.factor(BTKA), data= MUA_data1, family = "binomial")
summary(mua_glm1)


# 1yr/2yr/more than 2yr 
MUA_data1$yeardiff <- ifelse(MUA_data1$date_diff<366, "1yr",
                             ifelse(MUA_data1$date_diff<365*2,"2yr","more than 2yrs"))  
#MUA_data1$yeardiff<- ordered(as.factor(MUA_data1$yeardiff), levels = c("1yr", "2yr", "more than 2yrs"))
mua_glm1 = glm(C_MUA ~ yeardiff, data= MUA_data1, family = "binomial")
summary(mua_glm1)


# decision tree 
MUA_tree <- tree(C_MUA ~ MUA +date_diff,data= MUA_data1)
summary(MUA_tree)  # too many nodes and there is no way to prune. 

MUA_tree1 <- tree(C_MUA ~ MUA +date_diff,data= MUA_data1[MUA_data1$date_diff<645, ])
summary(MUA_tree1)

MUA_tree2 <- tree(C_MUA ~ MUA +BTKA,data= MUA_data1)
summary(MUA_tree2)  # BTKA is not used in the decision tree. 



##############################################################
#### Q3 sex: No, age(factor): Maybe, race:Maybe, ethnicity: No (str.error >>>>0)
# Insurance: C_MUA: No, MUA - Yes,  BMI: No, ... los : Maybe 
#
#############################################################

##### add sex variable  - insignificant #######
mua_glm1 = glm(C_MUA ~ sex, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(MUA ~ sex, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 <- glm(as.factor(Group2) ~ sex, data= MUA_data1, family = "binomial")
summary(mua_glm1)

mua_glm1 = glm(C_MUA ~ MUA +sex, data= MUA_data1, family = "binomial")
summary(mua_glm1)


############################# age variable
## reference : Is it advisable to drop certain levels of a categorical variable?
## https://stats.stackexchange.com/questions/141063/is-it-advisable-to-drop-certain-levels-of-a-categorical-variable

mua_glm1 = glm(C_MUA ~ MUA +age_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)  #exp(-0.04859) = 0.9525716 : as age increase 1, the odds of getting C_MUA decrease 5%..? Not true
# numeric age variable seems insignificant and not to make sense. EDA shows that when the patients are in 50s, 60s, the odds of getting MUA seems higher 

#MUA_data1$Fac_age_C_TKA <- ifelse((MUA_data1$age_C_TKA<50|MUA_data1$age_C_TKA>70),"less50 or over 70s","50s,60s")

MUA_data1$Fac_age_C_TKA <- ifelse(MUA_data1$age_C_TKA<50,"less50",
                                   ifelse(MUA_data1$age_C_TKA<60, "50s",
                                     ifelse(MUA_data1$age_C_TKA<70, "60s","over70s")))
MUA_data1$Fac_age_TKA <- ifelse(MUA_data1$age<50,"less50",
                                   ifelse(MUA_data1$age<60, "50s",
                                       ifelse(MUA_data1$age<70, "60s","over70s")))

mua_glm1 = glm(C_MUA ~ Fac_age_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(MUA ~ Fac_age_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)

mua_glm1 = glm(C_MUA ~ MUA +Fac_age_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(C_MUA ~ (MUA +Fac_age_C_TKA)^2, data= MUA_data1, family = "binomial")
summary(mua_glm1) # interaction term is insignificant 

MUA_data1 %>% group_by(Fac_age_C_TKA) %>%  count(Group2)

# when the patients are in 50s or 60s, age is significant. interaction is insignificant 
# keep age in the model. 

#############################  race variable
# the stand.error is way tooooooooo big..
# race black is significant..? 

MUA_data1$redu_race <- ifelse(MUA_data1$race %in% c("Hispanic","Multiracial","Other","Preference not indicated","American Indian","Asian"), "Other",MUA_data1$race)

mua_glm1 <- glm(C_MUA ~ as.factor(redu_race), data= MUA_data1, family = "binomial")
summary(mua_glm1)

mua_glm1 = glm(C_MUA ~ MUA+ as.factor(redu_race)+Fac_age_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)



############################  ethnicity variable
MUA_data1$ethnicity <- ifelse(MUA_data1$ethnicity %in% c("Prefers not to answer"), "Non-Hispanic Origin",MUA_data1$ethnicity)


mua_glm1 = glm(C_MUA ~  ethnicity, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(MUA ~  ethnicity, data= MUA_data1, family = "binomial")
summary(mua_glm1)
# the stand.error is tooooooooo big..


############################ Insurance

names(MUA_data1)[13] <- c("Insurance")
MUA_data1$Insurance <- 
  ifelse(MUA_data1$Insurance %in% c("Blue Cross Commercial","Commercial LUHS","Insurance","Worker's Comp","Worker's Comp LUHS"), "Private",
    ifelse(MUA_data1$Insurance %in% c("Medicare","Medicare LUHS","Managed Medicare"),"Medicare",
      ifelse(MUA_data1$Insurance %in% c("Managed Medicaid","Medicaid","MMAI"),"Medicaid","Uninsured")))

names(MUA_data1)[77] <- c("Insurance_C_TKA")
MUA_data1$Insurance_C_TKA <- 
  ifelse(MUA_data1$Insurance_C_TKA %in% c("Blue Cross Commercial","Commercial LUHS","Insurance","Worker's Comp","Worker's Comp LUHS"), "Private",
    ifelse(MUA_data1$Insurance_C_TKA %in% c("Medicare","Medicare LUHS","Managed Medicare"),"Medicare",
      ifelse(MUA_data1$Insurance_C_TKA %in% c("Managed Medicaid","Medicaid","MMAI"),"Medicaid","Uninsured")))

mua_glm1 = glm(C_MUA ~  Insurance_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(MUA ~  Insurance, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(as.factor(Group2) ~  Insurance, data= MUA_data1, family = "binomial")
summary(mua_glm1)

MUA_data1 %>% group_by(Insurance) %>% count(Group2)

# decision tree 
MUA_tree <- tree(C_MUA ~ MUA +redu_race+Insurance_C_TKA+Fac_age_C_TKA,data= MUA_data1)
summary(MUA_tree) # MUA is the only variable used ..


############################ BMI
### from EDA.. BMI is so insignificant. 

mua_glm1 = glm(C_MUA ~  bmi_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(MUA ~  BMI, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(as.factor(Group2) ~  BMI, data= MUA_data1, family = "binomial")
summary(mua_glm1)


############################ tobacco 
### from EDA.. tobacco is insignificant. 
MUA_data1$tobacco_C_TKA<- ifelse(is.na(MUA_data1$tobacco_C_TKA)==TRUE, MUA_data1$tobacco,MUA_data1$tobacco_C_TKA) 
MUA_data1$redu_tobacco <- ifelse(MUA_data1$tobacco %in% c("Passive","Yes"), "Yes","No")
MUA_data1$redu_tobacco_C_TKA <- ifelse(MUA_data1$tobacco_C_TKA %in% c("Passive","Yes"), "Yes","No")

mua_glm1 = glm(C_MUA ~  redu_tobacco_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(MUA ~  redu_tobacco, data= MUA_data1, family = "binomial")
summary(mua_glm1)


############################ ASA

MUA_data1$redu_ASA_C_TKA <- ifelse(MUA_data1$ASA_C_TKA %in% c(2,3),"2/3","else")
MUA_data1$redu_ASA <- ifelse(MUA_data1$ASA %in% c(2,3),"2/3","else")

# missing value 
MUA_data1$ASA<- ifelse(is.na(MUA_data1$ASA)==TRUE, MUA_data1$ASA_C_TKA,MUA_data1$ASA) 

mua_glm1 = glm(C_MUA ~  as.factor(ASA_C_TKA), data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(C_MUA ~  as.factor(redu_ASA_C_TKA), data= MUA_data1, family = "binomial")
summary(mua_glm1)

mua_glm1 = glm(MUA ~  as.factor(ASA), data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(MUA ~  as.factor(redu_ASA), data= MUA_data1, family = "binomial")
summary(mua_glm1)


############################ comorbidity
### List dont use
blood_transfusion+
  platelet_transfusion+
  AIDS +
  Malignancy +
  Cerebrovascular +
  COPD +
  CHF +
  Dementia +
  Diabetes_cc +
  Diabetes_no_cc +
  Hemiplegia +
  Metastatic +
  Mild_Liver +
  Moderate_Liver +
  MI +
  Peptic_Ulcer +
  PVD +
  CKD +
  Rheumatic +
  hematoma +
  wound_infection +
  knee_infection +
  `Readmission within 90 days (1=yes)`
race
los+
  library(pscl)
#######
############################ los

mua_glm1 = glm(C_MUA ~  los_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)

mua_glm1 = glm(C_MUA ~  MUA + los_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)



mua_glm1 = glm(MUA ~  los, data= MUA_data1, family = "binomial")
summary(mua_glm1)

# hmm


############################ operation time 

mua_glm1 = glm(C_MUA ~  op_time_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)

mua_glm1 = glm(MUA ~  op_time, data= MUA_data1, family = "binomial")
summary(mua_glm1)



###########################################################################

#Lasso model  - comorbidities 
library(glmnet)
library(fastDummies)
#Creating dummy matrix for lasso
#X matrix
x1matrix = data.matrix(MUA_data1[,c(80:104)]) # ASA, disease, readmission when C_TKA
#x2matrix = dummy_cols(MUA_data1[,c(6:9,11)]) # sex, age, race, ethnicity, tobaco
#x2matrix = data.matrix(MUA_data1[,c(6,118,9,74,76,81,78,77,75)]) # sex, redu_race,ethnicity, age_C_TKA,tobacco_C_TKA, op_time_C_TKA, los_C_TKA,Insurance_C_TKA,bmi_C_TKA
#x22matrix = x2matrix[,-c(1:5)] 
#xmatrix= cbind(x2matrix,x1matrix)
#xmatrix= data.matrix(xmatrix)

#Y matrix
#y_muacount_t = as.matrix((MUA_data1$MUA_count_T))
y_vector = as.vector(MUA_data1$C_MUA_bi)

#Finding lambda
k = 5
set.seed(987)
cv.lasso<- cv.glmnet(x1matrix, y_vector, alpha=1, family = "binomial")
lasso.mod = glmnet(x1matrix, y_vector, alpha=1,family = "binomial",lambda=cv.lasso$lambda.min)
lasso.coef <- predict(lasso.mod, type = "coefficients", s=cv.lasso$lambda.min)
paste0(round(lasso.coef@x,4),"X",lasso.coef@i, collapse=" + ")
#-4.5446 + 2.2272 "blood transfusion"

library(ggcorrplot)
ggcorrplot(cor(cbind(x1matrix, y_vector)),colors = c("#6D9EC1", "white", "#E46726"))
# the predictor that lasso picked matches with correlation between C_MUA and diseases

################################### sum up 

names(MUA_data1)
mua_glm <- glm(as.factor(C_MUA_bi) ~ as.factor(MUA_bi)+Fac_age_C_TKA+MUA_data1$redu_race+los_C_TKA+blood_transfusion_C_TKA, data= MUA_data1, family = "binomial")
mua_glm <- glm(C_MUA_bi ~ MUA_bi+Fac_age_C_TKA+MUA_data1$redu_race+los_C_TKA+blood_transfusion_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm)

mua_glm <- glm(C_MUA_bi ~ MUA_bi+blood_transfusion_C_TKA+los_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm)
mua_glm <- glm(C_MUA_bi ~ MUA_bi+blood_transfusion_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm)








### Total MUA count is response

#Zero inflated model
mua_t_glm = zeroinfl(MUA_count_T ~ sex + age+
    
    #tobacco+
    op_time_total+
    los_total
  #BMI 
  
  #'Readmission within 90 days (1=yes)'
  
  , data = MUA_data1, dist= "negbin")
summary(mua_t_glm)



#Model with MUA count
library(pls)
MUA_count_data = MUA_data1[,22:44]
mua_count = pcr(mua_count ~ ., data= MUA_count_data)
summary(mua_count)

mua_count1 = glm(MUA_count ~., data = mua_count_reduced)
summary(mua_count1)
validationplot(mua_count)


MUA_data1[,24:44]

MUA_data1[,44]





#### Binomial models with just probability of MUA (no MUA/C_MUA distinction)
#Imputing MUA/C_MUA data as vectors
y= c(MUA_data1$MUA_bi, MUA_data1$C_MUA_bi)
los = c(MUA_data1$los, MUA_data1$los_C_TKA)
sex = rep(MUA_data1$sex, 2)
op_time = c(MUA_data1$op_time, MUA_data1$op_time_C_TKA)
tobacco = c(MUA_data1$tobacco, MUA_data1$tobacco_C_TKA)
ethnicity = rep(MUA_data1$ethnicity, 2)
BMI = c(MUA_data1$BMI, MUA_data1$bmi_C_TKA)
age = c(MUA_data1$age, MUA_data1$age_C_TKA)
ASA = c(MUA_data1$ASA, MUA_data1$ASA_C_TKA)
ID = rep(MUA_data1$ID,2)



#Standard Glm binomial
M_glm = glm(y~ los +
    sex+
    op_time+
    tobacco+
    ethnicity+
    BMI+
    age, family = "binomial")


summary(M_glm)



#Generalized Estimating Equation
library(gee)
M_glm2 = gee(y~ los +
    sex+
    op_time+
    ethnicity+
    BMI+
    age, id = ID, family = "binomial", corstr ="independence")


summary(M_glm2)
library(geepack)
mf=formula(y~ los +
    sex+
    op_time+
    ethnicity+
    BMI+
    age)
M_glm4 = geeglm(y~ los +
    sex+
    op_time+
    ethnicity+
    BMI+
    age, id = ID, family = "binomial", corstr ="ind")
summary(M_glm4)




#Random effects
library(lme4)
M_glm3 = glmer(y~ los +
    sex+
    op_time+
    #ethnicity+
    as.factor(ASA)+
    
    BMI+
    age + (1|ID), family = "binomial")


summary(M_glm3)





