############## (Jason's code)
#Model building

library(readxl)
library(tidyverse)
library(tree)
library(gam)

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

###################################################################################
### Q1) MUA is a risk factor of C_MUA? 
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
### + Q2) the # between 2 TKAs 
###################################################################################
#Model with  MUA predictiing C_mua with time

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

# If the patient got Bilateral TKA, is the risk of getting MUA significantly higher?
MUA_data1$BTKA <- ifelse(MUA_data1$date_diff==0, "Yes","No")   
mua_glm1 = glm(as.factor(Group2) ~ as.factor(BTKA), data= MUA_data1, family = "binomial")
summary(mua_glm1)

# 1yr/2yr/more than 2yr 
MUA_data1$yeardiff <- ifelse(MUA_data1$date_diff<366, "1yr",
                             ifelse(MUA_data1$date_diff<365*2,"2yr","more than 2yrs"))  
MUA_data1$yeardiff<- ordered(as.factor(MUA_data1$yeardiff), levels = c("1yr", "2yr", "more than 2yrs"))
mua_glm1 = glm(C_MUA ~ yeardiff, data= MUA_data1, family = "binomial")
summary(mua_glm1)


# decision tree 
MUA_tree <- tree(C_MUA ~ MUA +date_diff,data= MUA_data1)
summary(MUA_tree)  # too many nodes and there is no way to prune. 
plot(MUA_tree)

MUA_tree1 <- tree(C_MUA ~ MUA +date_diff,data= MUA_data1[MUA_data1$date_diff<645, ])
summary(MUA_tree1)




##############################################################
#### Q3 sex, age, ....
#############################################################

# add sex variable 
mua_glm1 = glm(C_MUA ~ MUA +as.factor(sex), data= MUA_data1, family = "binomial")
summary(mua_glm1)


# add age variable

mua_glm1 = glm(C_MUA ~ MUA +age_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)

MUA_data1$Fac_age_C_TKA <- ifelse((MUA_data1$age_C_TKA<50|MUA_data1$age_C_TKA>70),"less50 or over 70s","50s,60s")
mua_glm1 = glm(C_MUA ~ Fac_age_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(C_MUA ~ MUA +Fac_age_C_TKA, data= MUA_data1, family = "binomial")
summary(mua_glm1)
mua_glm1 = glm(C_MUA ~ (MUA +Fac_age_C_TKA)^2, data= MUA_data1, family = "binomial")
summary(mua_glm1)

# when the patients are in 50s or 60s, age is significant. interaction is insignificant 




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


#Lasso model
library(glmnet)
library(fastDummies)
#Creating dummy matrix for lasso
#X matrix
x1matrix = data.matrix(MUA_data1[,c(20,23:45)])
x2matrix = dummy_cols(MUA_data1[,c(6:9,11)])
x22matrix = x2matrix[,-c(1:5)]
xmatrix= cbind(x22matrix,x1matrix)
xmatrix= data.matrix(xmatrix)
#Y matrix
y_muacount_t = as.matrix((MUA_data1$MUA_count_T))


#Finding lambda
cv.out= cv.glmnet(xmatrix, y_muacount_t, alpha=1, nfolds=600)
bestlam = cv.out$lambda.min
grid <- 10^ seq (10, -2, length = 100)

lasso.mod = glmnet(xmatrix, y_muacount_t, alpha=1, lambda=bestlam)
lasso.mod$beta


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





