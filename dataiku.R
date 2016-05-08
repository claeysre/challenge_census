# Packages needed to run this code. If you do not posess one of this library, please run the following command
# install.packages('e1071') for example
library(randomForest)
library(ggplot2)
library(rmarkdown)
library(e1071)
library(ROCR)
library(caret)
library(lubridate)


# Please change your path according to your need
train_location = '/Volumes/RemiSDCard/Dataiku/us_census_full/census_income_learn.csv'
test_location = '/Volumes/RemiSDCard/Dataiku/us_census_full/census_income_test.csv'

# Provide contextual information
context = c('age','class_of_worker','industry_code','occupation_code','education','wage_per_hour','enrolled_edu', 
            'marital_status', 'maj_ind_code', 'maj_occ_code','race','hispanic_origin','sex', 
            'member_labor_union','reason_unemployment','employment_status', 'capital_gains',
            'capital_losses','dividends', 'tax_filer_status', 'region_prev_res', 'reg_prev_state',
            'household_stats', 'household_summary', 'instance_weight', 'migration_msa', 'migration_reg', 
            'mig_within_region', 'same_house', 'migration_sunbelt', 'num_persons_worked_for_employer', 
            'relatives_under_18' , 'country_father', 'country_mother', 'country_self', 'citizenship', 
            'own_business', 'veterans_filled','veterans_benefits', 'weeks_worked_year', 'year', 'income')

# Ensure that columns have the correct type
type_context = c('numeric',rep('factor',4),'numeric',rep('factor',10),rep('numeric',3),
                 rep('factor',11),'numeric',rep('factor',8),'numeric',rep('factor',2))

# Load train and test dataset
train_df <- read.csv(train_location, header = F, na.strings = '?', col.names = context, 
            strip.white	= T, colClasses = type_context)

test_df  <- read.csv(test_location, header = F, na.strings = '?', col.names = context, 
            strip.white	= T, colClasses = type_context)

# Drop weight column (cf Metadata)
train_df <- subset(train_df, select = -c(instance_weight))
test_df <- subset(test_df, select = -c(instance_weight))

train_df$income <- ifelse(train_df$income == "- 50000.", "0",
                   ifelse(train_df$income == "50000+.", "1", "other"))

test_df$income <- ifelse(test_df$income == "- 50000.", "0",
                          ifelse(test_df$income == "50000+.", "1", "other"))

train_df$income <- as.factor(train_df$income)
test_df$income <- as.factor(test_df$income)

# Let's see if there are any missing values 
incomplete_columns <- sapply(train_df, function(x) (sum(is.na(x)) / nrow(train_df)))*100; incomplete_columns[ incomplete_columns > 0]

# --------------------------------------------------------------------------
# Let's explore the influence of the categorical variables first

#First the class of worker 
qplot (income, data = train_df, fill = class_of_worker) + facet_grid (. ~ class_of_worker) 


#Then the industry code, too many categories, furthermore we don't know what the categories mean, same for occupation code 
qplot (income, data = train_df, fill = industry_code) + facet_grid (. ~ industry_code) 
# => same info with maj industry code et major occupation code

#  Education
qplot (income, data = train_df, fill = education) + facet_grid (. ~ education) 
#=> a very good feature that will  be helpful for classification
# Maybe  gather the children. 

# Enrolled in edu last week
qplot (income, data = train_df, fill = enrolled_edu) + facet_grid (. ~ enrolled_edu) 
#=>  Can also be discriminativ, students don't make 50K during schools. maybe merge college and high school

# Marital Status
qplot (income, data = train_df, fill = marital_status) + facet_grid (. ~ marital_status) 

# Major Industry Code
qplot (income, data = train_df, fill = maj_ind_code) + facet_grid (. ~ maj_ind_code)
# =>Trade, Manufacturing, Finance categories have a bigger proportion to earn +50K 

# Major Occupation Code
qplot (income, data = train_df, fill = maj_occ_code) + facet_grid (. ~ maj_occ_code)
# => Managerial, Professional Special, Protective Services have better proportion to earn +50K

# Race
qplot (income, data = train_df, fill = race) + facet_grid (. ~ race)
# White people seem advantaged , maybe transform the other races into a cat "minorities"would help the RF

#hispanic_origin
qplot (income, data = train_df, fill = hispanic_origin) + facet_grid (. ~ hispanic_origin)
#=>  Not Relevant All other reprensent white people and the others minorities 

#sex
qplot (income, data = train_df, fill = sex) + facet_grid (. ~ sex)
# =>Relevant males advantanged 

#member labor union
qplot (income, data = train_df, fill = member_labor_union) + facet_grid (. ~ member_labor_union)
# => Relevant

#reason_unemployment
qplot (income, data = train_df, fill = reason_unemployement) + facet_grid (. ~ reason_unemployement)
# Useless since people unemployed will not help to classify the problematic categorie+50K 
# No +50K for

#employment_status
qplot (income, data = train_df, fill = employment_status) + facet_grid (. ~ employment_status)
#=> Full time schedule more likely to earn +50K

# tax_filer_status
qplot (income, data = train_df, fill = tax_filer_status) + facet_grid (. ~ tax_filer_status)
#=> Relevant, Joint both under 65

# region_prev_res
qplot (income, data = train_df, fill = region_prev_res) + facet_grid (. ~ region_prev_res)
# => Not relevant 

# reg_prev_state
qplot (income, data = train_df, fill = reg_prev_state) + facet_grid (. ~ reg_prev_state)
# => Not  Relevant 

# household_stats
qplot (income, data = train_df, fill = household_stats) + facet_grid (. ~ household_stats)
# Redundant with household_summary

# household_summary
qplot (income, data = train_df, fill = household_summary) + facet_grid (. ~ household_summary)
# => Relevant

# migration_msa
qplot (income, data = train_df, fill = migration_msa) + facet_grid (. ~ migration_msa)
# => could be relevant but too much missing information

# migration_reg
qplot (income, data = train_df, fill = migration_reg) + facet_grid (. ~ migration_reg)
# => could be relevant but too much missing information

# migration within the same region
qplot (income, data = train_df, fill = mig_within_region) + facet_grid (. ~ migration_within_reg)

# same house
qplot (income, data = train_df, fill = same_house) + facet_grid (. ~ same_house)
# => Not relevant

# Migration sunbelt
qplot (income, data = train_df, fill = migration_sunbelt) + facet_grid (. ~ migration_sunbelt)

#relatives_under_18
qplot (income, data = train_df, fill = relatives_under_18) + facet_grid (. ~ relatives_under_18)
# => Not relevant, sems to apply only to childs

#country_father
qplot (income, data = train_df, fill = country_father) + facet_grid (. ~ country_father)
# 

#country_mother
qplot (income, data = train_df, fill = country_mother) + facet_grid (. ~ country_mother)
# 

#country_self
qplot (income, data = train_df, fill = country_self) + facet_grid (. ~ country_self)
# May be relavant redundant with citizenship

# citizenship
qplot (income, data = train_df, fill = citizenship) + facet_grid (. ~ citizenship)
# let's see later


# own_business
qplot (income, data = train_df, fill = own_business) + facet_grid (. ~ own_business)
# => Creator of business advantaged 

# veterans_filled 
qplot (income, data = train_df, fill = veterans_filled) + facet_grid (. ~ veterans_filled)
# => Not relevant, all the data is in the not in univers class and there is not enough data for veterans

# veterans_benefits
qplot (income, data = train_df, fill = veterans_benefits) + facet_grid (. ~ veterans_benefits )
# => Maybe relevant

# year 
qplot (income, data = train_df, fill = year) + facet_grid (. ~ year )
# As predicted, useless 


# ----------------------------------------------------------------------------------
# Let's explore the influence of numerical variables 

#AGE
boxplot (age ~ income, data = train_df, main = "Age distribution depending on classes", 
         xlab = "class", ylab = "Age", col = c("green") )
# => Relevant

ggplot(train_df, aes(x=age, fill=income)) +
  geom_histogram(binwidth=2, alpha=0.5, position="identity")
# => Different distribution
# 


#WAGE
boxplot (wage_per_hour ~ income, data = train_df, main = "wage distribution depending on classes", 
         xlab = "class", ylab = "wage", col = c("green") )

#Too much zeros in the dataset, doesn't seem to be useful, a simple overview of the first lines shows
# a individual having 1200 of hourly wage and still under 50 K 

# => Relevant

# Capitals Gains + Capital Losses + Dividends

train_df$sum_losses_gains = train_df$capital_gains - train_df$capital_losses + train_df$dividends 
test_df$sum_losses_gains = test_df$capital_gains - test_df$capital_losses + test_df$dividends 

boxplot (sum_losses_gains ~ income, data = train_df, main = "gains - losses  distribution depending on classes", 
         xlab = "class", ylab = "Age", col = c("green") )

# Even if that's not very insightful we can clearly see that people who lose money don't make +50K
#m <- ggplot(train_df, aes(x = sum_losses_gains))
#m + geom_density()

# Let's create some category, this can be good for a decision tree  or a Random Forest 
train_df$sum_losses_gains_cat<-ifelse(train_df$sum_losses_gains< -1000,"big-loser",
             ifelse(train_df$sum_losses_gains >= -1000 & train_df$sum_losses_gains < 0, "small-loser",
             ifelse(train_df$sum_losses_gains == 0 , "balanced",
             ifelse(train_df$sum_losses_gains> 0 & train_df$sum_losses_gains <= 5000 , "small-winner",
             ifelse(train_df$sum_losses_gains > 5000 , "big-winner","other"
             )))))

test_df$sum_losses_gains_cat<-ifelse(test_df$sum_losses_gains< -1000,"big-loser",
                               ifelse(test_df$sum_losses_gains >= -1000 & test_df$sum_losses_gains < 0, "small-loser",
                               ifelse(test_df$sum_losses_gains == 0 , "balanced",
                               ifelse(test_df$sum_losses_gains> 0 & test_df$sum_losses_gains <= 5000 , "small-winner",
                               ifelse(test_df$sum_losses_gains > 5000 , "big-winner","other"
                               )))))


qplot (income, data = train_df, fill = sum_losses_gains_cat) + facet_grid (. ~ sum_losses_gains_cat)

#Num persons worked for employer
boxplot (num_persons_worked_for_employer ~ income, data = train_df, main = "Num persons worked for employer distribution depending on classes", 
         xlab = "class", ylab = "nums_persons", col = c("green") )
# => Relevant

ggplot(train_df, aes(x = num_persons_worked_for_employer, fill=income)) +
  geom_histogram(binwidth = 2, alpha = 0.5, position="identity")

# Bigger the corporate is, you have more chance to earn money
# Relevant to put that variable into factor
train_df$num_persons_worked_for_employer <- as.factor(train_df$num_persons_worked_for_employer)
test_df$num_persons_worked_for_employer <- as.factor(test_df$num_persons_worked_for_employer)

qplot (income, data = train_df, fill = num_persons_worked_for_employer) + facet_grid (. ~ num_persons_worked_for_employer)


# WEEKS WORKED IN A YEAR
boxplot (weeks_worked_year ~ income, data = train_df, main = "Weeks worked in a year distribution depending on classes", 
         xlab = "class", ylab = "nums_persons", col = c("green") )
# => Relevant

ggplot(train_df, aes(x = weeks_worked_year, fill=income)) +
  geom_histogram(binwidth = 2, alpha = 0.5, position="identity")


# Age Categories
train_df$age_cat<-ifelse(train_df$age < 18, "youth",
          ifelse(train_df$age >= 18 & train_df$age < 27, "y_workers",
          ifelse(train_df$age >= 27 & train_df$age < 67, "wokers",
          ifelse(train_df$age >= 67 , "retired","other"
          ))))

# Reproduce the same for the test set
test_df$age_cat<-ifelse(test_df$age < 18, "youth",
                  ifelse(test_df$age >= 18 & test_df$age < 27, "y_workers",
                  ifelse(test_df$age >= 27 & test_df$age < 67, "wokers",
                  ifelse(test_df$age >= 67 , "retired","other"
                  ))))


# Education Category reduction
train_df$education_cat<-ifelse(train_df$education == "10th grade", "youth",
                    ifelse(train_df$education == "11th grade", "youth",
                    ifelse(train_df$education == "12th grade no diploma", "youth" ,
                    ifelse(train_df$education == "1st 2nd 3rd or 4th grade", "youth", 
                    ifelse(train_df$education == "5th or 6th grade", "youth", 
                    ifelse(train_df$education == "7th and 8th grade", "youth",  
                    ifelse(train_df$education == "9th grade", "youth", 
                    ifelse(train_df$education == "Less than 1st grade", "youth", 
                    ifelse(train_df$education == "Children", "youth", 
                    ifelse(train_df$education == "Associates degree-academic program", "basicdegree", 
                    ifelse(train_df$education == "Associates degree-occup /vocational", "basicdegree", 
                    ifelse(train_df$education == "Some college but no degree", "basicdegree", 
                    ifelse(train_df$education == "High school graduate", "high school graduate", 
                    ifelse(train_df$education == "Bachelors degree(BA AB BS)", "bachelor", 
                    ifelse(train_df$education == "Masters degree(MA MS MEng MEd MSW MBA)", "master", 
                    ifelse(train_df$education == "Doctorate degree(PhD EdD)", "prof_doct", 
                    ifelse(train_df$education == "Prof school degree (MD DDS DVM LLB JD)", "prof_doct", "other"
                    )))))))))))))))))


test_df$education_cat<- ifelse(test_df$education == "10th grade", "youth",
                        ifelse(test_df$education == "11th grade", "youth",
                        ifelse(test_df$education == "12th grade no diploma", "youth" ,
                        ifelse(test_df$education == "1st 2nd 3rd or 4th grade", "youth", 
                        ifelse(test_df$education == "5th or 6th grade", "youth", 
                        ifelse(test_df$education == "7th and 8th grade", "youth",  
                        ifelse(test_df$education == "9th grade", "youth", 
                        ifelse(test_df$education == "Less than 1st grade", "youth", 
                        ifelse(test_df$education == "Children", "youth", 
                        ifelse(test_df$education == "Associates degree-academic program", "basicdegree", 
                        ifelse(test_df$education == "Associates degree-occup /vocational", "basicdegree", 
                        ifelse(test_df$education == "Some college but no degree", "basicdegree", 
                        ifelse(test_df$education == "High school graduate", "high school graduate", 
                        ifelse(test_df$education == "Bachelors degree(BA AB BS)", "bachelor", 
                        ifelse(test_df$education == "Masters degree(MA MS MEng MEd MSW MBA)", "master", 
                        ifelse(test_df$education == "Doctorate degree(PhD EdD)", "prof_doct", 
                        ifelse(test_df$education == "Prof school degree (MD DDS DVM LLB JD)", "prof_doct", "other"
                        )))))))))))))))))

# Remove column, also  income to append it at the end, after
train_clean_df <- subset(train_df, select = -c(age,industry_code,occupation_code,education,hispanic_origin, reason_unemployment,
                                               capital_gains,capital_losses,dividends,region_prev_res, reg_prev_state, household_stats,
                                               migration_msa, migration_reg, mig_within_region, migration_sunbelt, country_father, country_mother,
                                               country_self, veterans_filled, year, income, sum_losses_gains))
# Re-Append it at the end 
train_clean_df$income <- train_df$income

# Produce the same  for the test dataset
test_clean_df <- subset(test_df, select = -c(age,industry_code,occupation_code,education,hispanic_origin, reason_unemployment,
                                               capital_gains,capital_losses,dividends,region_prev_res, reg_prev_state, household_stats,
                                               migration_msa, migration_reg, mig_within_region, migration_sunbelt, country_father, country_mother,
                                               country_self, veterans_filled, year, income, sum_losses_gains))
# Re-Append it at the end 
test_clean_df$income <- test_df$income



# Set new variables as Factor

train_clean_df$education_cat <- as.factor(train_clean_df$education_cat)
train_clean_df$age_cat <- as.factor(train_clean_df$age_cat)
train_clean_df$sum_losses_gains_cat <- as.factor(train_clean_df$sum_losses_gains_cat)

test_clean_df$education_cat <- as.factor(test_clean_df$education_cat)
test_clean_df$age_cat <- as.factor(test_clean_df$age_cat)
test_clean_df$sum_losses_gains_cat <- as.factor(test_clean_df$sum_losses_gains_cat)

# Set the seed
set.seed(3004)

# Train a rf
rf <-randomForest(income ~ class_of_worker + wage_per_hour + enrolled_edu + marital_status + maj_ind_code + maj_occ_code
                  + race + sex + member_labor_union + employment_status + tax_filer_status + household_summary + same_house
                  + num_persons_worked_for_employer + relatives_under_18 + citizenship + own_business + veterans_benefits +
                    weeks_worked_year + sum_losses_gains_cat + age_cat + education_cat, 
                    data=train_clean_df, 
                    mtry= 5, 
                    sampsize=c(5000,1000),
                    ntree=300, 
                    na.action=na.omit, 
                    do.trace=100, 
                    importance=TRUE)


foo <- predict (rf,train_clean_df[,1:22])

ber <- function(confusion_mat)
{
  foo <- confusion_mat[1,2] / (confusion_mat[1,1]+confusion_mat[1,2])
  bar <- confusion_mat[2,1] / (confusion_mat[2,1]+confusion_mat[2,2])
  
  ber <- (foo + bar)*0.5*100
}


imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

 

# Train final rf
rf8 <-randomForest(income ~ class_of_worker + wage_per_hour + enrolled_edu + marital_status + maj_ind_code + maj_occ_code
                  + race + sex + tax_filer_status + household_summary 
                  + num_persons_worked_for_employer +  citizenship + own_business +
                    weeks_worked_year + sum_losses_gains_cat + age_cat  + education_cat, 
                  data=train_clean_df, 
                  mtry= 5, 
                  sampsize=c(1900,1500),
                  ntree=500, 
                  na.action=na.omit, 
                  do.trace=100, 
                  importance=TRUE)

# PREDICTIONS ON TEST SET
featuresToKeep <- c('class_of_worker', 'wage_per_hour', 'enrolled_edu', 'marital_status', 'maj_ind_code', 'maj_occ_code',
                    'race', 'sex', 'tax_filer_status', 'household_summary', 'num_persons_worked_for_employer','citizenship',
                    'own_business','weeks_worked_year', 'sum_losses_gains_cat', 'age_cat', 'education_cat') 
test_clean_df2 <- test_clean_df[featuresToKeep]

test_clean_df2$income_predicted <- predict(rf8,	test_clean_df2)

xtab <- table(test_clean_df2$income_predicted, test_clean_df$income)
conf_object <- confusionMatrix(xtab)

Ber_error <- ber(t(conf_object$table ))




