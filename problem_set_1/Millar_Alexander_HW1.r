KG_Samples = read.table('igsr_samples.tsv',
                        sep = '\t',
                        header = T)
head(KG_Samples)

library(tidyverse)

head(KG_Samples$Data.collections,2)

KG_Samples = KG_Samples %>%
  separate(Data.collections,c('a','b','c','d','e'),',')

nrow(KG_Samples)

table(KG_Samples$Sex)

KG_Samples %>% group_by(Sex) %>% tally()

table(KG_Samples$Population.code,
      KG_Samples$Sex)

KG_Samples %>% filter(Population.code=='GBR') %>% group_by(Sex) %>% tally()

sample(c(0:100),1)

sample(c(0:100),size=nrow(KG_Samples),replace = TRUE)

KG_Samples$Age = sample(c(0:100),size=nrow(KG_Samples),replace = TRUE)
head(KG_Samples,2)

head(KG_Samples[,c(1,2,14,3:8)])

KG_Samples = KG_Samples[,c(1,2,14,3:8)]
head(KG_Samples,2)

colnames(KG_Samples)

KG_Samples = subset(KG_Samples, select = -c(Biosample.ID))

colnames(KG_Samples)

summary_Age <- KG_Samples %>%
  summarize(mean = mean(Age), std_dev = sd(Age))
summary_Age

summary_sex_Age <- KG_Samples %>%
  group_by(Sex) %>%
  summarize(mean = mean(Age, na.rm=TRUE),
            std_dev = sd(Age, na.rm = TRUE))
summary_sex_Age

KG_Samples %>% 
  group_by(Population.name) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))

KG_Samples <- KG_Samples %>% 
  mutate(Age_Months = Age * 12)

ggplot(data=KG_Samples, mapping = aes(x=Age)) +
  geom_histogram(color='white',bins = 25)+
  theme_classic()

KG_Samples = KG_Samples %>% 
  mutate(Age_EUR = if_else(condition = Age<50 & Superpopulation.code=='EUR',
                           true=1,
                           false=0))
head(KG_Samples,2)





# 2.1
list.files()
diabetes_data = read.csv('diabetes.csv',header = TRUE)
head(diabetes_data,5)
# Pregnancies Glucose BloodPressure SkinThickness Insulin  BMI
# 1           6     148            72            35       0 33.6
# 2           1      85            66            29       0 26.6
# 3           8     183            64             0       0 23.3
# 4           1      89            66            23      94 28.1
# 5           0     137            40            35     168 43.1
# DiabetesPedigreeFunction Age Outcome Outcome_BP
# 1                    0.627  50       1          2
# 2                    0.351  31       0          3
# 3                    0.672  32       1          2
# 4                    0.167  21       0          3
# 5                    2.288  33       1          2


# 2.2
#diabetes_data %>% group_by(Outcome) %>% tally()
# diabetes_data %>% 
#   filter(Outcome==1) %>% 
#   summarize(count=n())
sum(diabetes_data$Outcome)
# [1] 268


# 2.3
# diabetes_data %>% 
#   filter(Outcome==1 & Age>45) %>% 
#   summarize(count=n())
sum((diabetes_data %>% filter(Outcome==1 & Age>45))$Outcome)
# [1] 58


# 2.4
diabetes_data %>% 
  filter(Outcome==0) %>% 
  summarize(mean=mean(Glucose),
            var=var(Glucose))
# mean      var
# 1 109.98 683.3623


# 2.5
diabetes_data = diabetes_data %>% 
  mutate(Outcome_BP = case_when(
    !Outcome ~ 3,
    Outcome & BloodPressure>100 ~ 1,
    Outcome & BloodPressure<=100 ~ 2))
colnames(diabetes_data)
# [1] "Pregnancies"              "Glucose"                 
# [3] "BloodPressure"            "SkinThickness"           
# [5] "Insulin"                  "BMI"                     
# [7] "DiabetesPedigreeFunction" "Age"                     
# [9] "Outcome"                  "Outcome_BP"  
table(diabetes_data$Outcome_BP)
# 1   2   3 
# 8 260 500 


# 2.6
ggplot(diabetes_data %>% filter(Outcome==1), mapping = aes(x=BMI))+
  geom_histogram(color='white',bins = 25)+
  theme_classic()+
  ggtitle('Diabetes')
ggplot(diabetes_data %>% filter(Outcome==0), mapping = aes(x=BMI))+
  geom_histogram(color='white',bins = 25)+
  theme_classic()+
  ggtitle('No Diabetes')


# 3.1
occurrence <- nrow(KG_Samples 
                   %>% filter(Population.name=='Colombian' |
                              Sex=='male'))
population <- nrow(KG_Samples)
print(occurrence/population)
#[1] 0.5614803


# 3.2
#p(H|D) = p(D|H)*p(H)/p(D)
## p(D) = (p(D|H)*p(H)) + (p(D|H')*p(H'))
diabetes_data = diabetes_data %>% 
  mutate(hbmi = BMI>30)

diabetes_data.h_bmi.outcome.df <-
  diabetes_data %>% 
  group_by(Outcome, hbmi) %>% 
  summarize(n = n())
diabetes_data.h_bmi.outcome.df

diabetes_data.h_bmi.outcome.prop.df <-
  diabetes_data.h_bmi.outcome.df %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n))
diabetes_data.h_bmi.outcome.prop.df

h_bmi.marginal.df <-
  diabetes_data.h_bmi.outcome.prop.df %>% 
  group_by(hbmi) %>% 
  summarize(marginal = sum(prop))
h_bmi.marginal.df

outcome.marginal.df <-
  diabetes_data.h_bmi.outcome.prop.df %>% 
  group_by(Outcome) %>% 
  summarize(marginal = sum(prop))
outcome.marginal.df

outcome.marginal.df
h_bmi.marginal.df

joint.prob <-
  diabetes_data.h_bmi.outcome.prop.df %>% 
  filter(hbmi, Outcome==1) %>% 
  .$prop

marg.prob <- 
  outcome.marginal.df %>% 
  filter(Outcome==1) %>%
  .$marginal
marg.prob

cond.prob <- joint.prob / marg.prob
cond.prob

total = nrow(diabetes_data)
p_bhp = nrow(diabetes_data %>% filter(BMI>30)) / total
p_dbt = nrow(diabetes_data %>% filter(Outcome)) / total

nrow(diabetes_data %>% filter(BMI>30 & Outcome)) / total
nrow(diabetes_data %>% filter(BMI>30 & !Outcome)) / total
nrow(diabetes_data %>% filter(BMI<=30 & Outcome)) / total
nrow(diabetes_data %>% filter(BMI<=30 & !Outcome)) / total

diabetes_data %>% 
  group_by(Outcome, BMI>30) %>% 
  summarize(count=sum(prop))

diabetes_data %>% 
  group_by(Outcome, BMI>30) %>% 
  summarize(count=n()) %>% 
  ungroup()
  










### TODO: wants answer assigned to a var, printed out, and verbal interpretation (not just value)
