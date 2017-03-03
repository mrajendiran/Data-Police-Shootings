setwd("~/Documents/MSDS/1_Machine Learning/Projects/data-police-shootings")

suppressMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(ggthemes)
  library(purrr)
  library(randomForest)
  library(bnlearn)
  library(arules)
  library(RWeka)
  library(caret)
  library(e1071)
  library(mlbench)
})

set.seed(12345)


##     Loading & Processing Data     #############################################

# read in the CSV file
shootings <- read_delim("fatal-police-shootings-data.csv", delim=',', progress=F)
shootings %>% glimpse

# factor variables except for date and age
shootings$date  <- ymd(shootings$date)
shootings %>% glimpse

# check for NAs
sum(is.na(shootings)) # 207 NAs
shootings %>%
  summarise_each(funs(sum(is.na(.))))
#    id  name  date manner_of_death armed   age gender  race  city state signs_of_mental_illness threat_level  flee body_camera
# <int> <int> <int>           <int> <int> <int>  <int> <int> <int> <int>                   <int>        <int> <int>       <int>
#     0     0     0               0     7    46      1   116     0     0                       0            0    37           0

# inspect DF with complete cases
shootings_complete <- shootings[complete.cases(shootings), ]
shootings_complete %>% glimpse # 1,959 cases from 2,126

# separate cases between 2015 and 2016 (1820 total cases)
shootings_complete %>%
  filter(year(date) == 2015) -> shootings2015 # 947 obs
shootings2015 %>% glimpse

shootings_complete %>%
  filter(year(date) == 2016) -> shootings2016 # 873 obs
shootings2016 %>% glimpse

# Cases for 2017
shootings_complete %>%
  filter(year(date) == 2017) -> shootings2017 # 139 obs


##     Descriptive Insights     #############################################

# Gender
shootings_complete %>%
  group_by(gender) %>%
  summarise(count = n_distinct(id)) -> shootings_gender
shootings_gender %>% glimpse
# gender <chr> "F", "M"
# count  <int> 81, 1878

# Race
shootings2 %>%
  group_by(race) %>%
  summarise(count = n_distinct(id)) -> shootings_race
shootings_race %>% glimpse
# race  <chr> "A", "B", "H", "N", "O", "W"
# count <int> 31, 517, 349, 26, 28, 1008

# Age
shootings_complete %>%
  group_by(age) %>%
  summarise(count = n_distinct(id)) -> shootings_age
shootings_age %>% glimpse
ggplot(shootings_age, aes(x=age, y=count)) +
  geom_bar(stat="identity") +
  labs(list(x="Age", y="Count", title="Shootings by Age"))

# Manner of Death
shootings_complete %>%
  group_by(manner_of_death) %>%
  summarise(count = n_distinct(id)) -> shootings_death
shootings_death %>% glimpse
# manner_of_death <chr> "shot", "shot and Tasered"
# count           <int> 1823, 136

# Threat Level
shootings_complete %>%
  group_by(threat_level) %>%
  summarise(count = n_distinct(id)) -> shootings_threat
shootings_threat %>% glimpse
# threat_level <chr> "attack", "other", "undetermined"
# count        <int> 1284, 568, 107

# Flee
shootings_complete %>%
  group_by(flee) %>%
  summarise(count = n_distinct(id)) -> shootings_flee
shootings_flee %>% glimpse
# flee  <chr> "Car", "Foot", "Not fleeing", "Other"
# count <int> 299, 243, 1341, 76

# City
shootings_complete %>%
  group_by(city) %>%
  summarise(count = n_distinct(id)) -> shootings_city
ggplot(shootings_city %>% filter(count > 10), aes(x=city, y=count, fill=city)) +
  geom_bar(stat="identity") +
  labs(list(x="Count", title="Shootings by City")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)

# State
shootings_complete %>%
  group_by(state) %>%
  summarise(count = n_distinct(id)) -> shootings_state
shootings_state %>% glimpse
ggplot(shootings_state %>% filter(count > 20), aes(x=state, y=count, fill=state)) +
  geom_bar(stat="identity") +
  labs(list(x="Count", title="Shootings by State")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)

# Signs of Mental Illness
shootings_complete %>%
  group_by(signs_of_mental_illness) %>%
  summarise(count = n_distinct(id)) -> shootings_illness
shootings_illness %>% glimpse
# signs_of_mental_illness <chr> "False", "True"
# count                   <int> 1473, 486

# Armed
shootings_complete %>%
  group_by(armed) %>%
  summarise(count = n_distinct(id)) -> shootings_armed
shootings_armed %>% glimpse
ggplot(shootings_armed %>% filter(count > 5), aes(x=armed, y=count)) +
  geom_bar(stat="identity") +
  labs(list(x="Count", title="Shootings by Armed Status")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##     Functions for Reducing Dimensions    #############################################

# places age variables into 5 bins
bin_age <- function(age_vector) {
  age_vector %>%
    map_chr(function(age) {
      if (age < 18) {
        return("<18")
      }
      if (age < 26) {
        return("<26")
      }
      if (age < 35) {
        return("<35")
      }
      if (age < 50) {
        return("<50")
      }
      "50+"
    }) %>%
    factor
}

# places armed variables into 8 bins
bin_armed <- function(armed_vector) {
  armed_vector %>%
    as.character() %>%
    map_chr(function(armed) {
      if (armed %in% c("gun","guns and explosives","gun and knife","hatchet and gun","machete and gun")) {
        return("gun")
      }
      if (armed %in% c("nail gun","shovel","hammer","hatchet","box cutter","cordless drill","screwdriver","metal hand tool","meat cleaver","carjack","contractor's level","stapler","ax","hand torch","chain saw","garden tool","scissors","pick-axe","flashlight","pitchfork","metal rake","crowbar")) {
        return("tools")
      }
      if (armed %in% c("knife","sword","machete","lawn mower blade","sharp object","straight edge razor","bayonet","glass shard")) {
        return("blades")
      }
      if (armed %in% c("metal object","flagpole","metal pole","metal pipe","blunt object","metal stick","brick","baseball bat","pole","rock","piece of wood","baton","baseball bat and fireplace poker","oar")) {
        return("blunt")
      }
      if (armed %in% c("bean-bag gun","Taser","chain","crossbow","spear")) {
        return("other")
      }
      if (armed %in% c("vehicle","motorcycle")) {
        return("vehicle")
      }
      if (armed == "unknown weapon") {
        return("undetermined")
      }
      armed
    }) %>%
    factor
}

# creates separate attribute for seasons based on dates
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231))
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

# bins race by either being 'White' or 'Other'
# originally we attempted to classify across all 6 races but because race=W is disproportionaly represented (accounting for over
# 50% of the data while only being 1 of 6 race categories) we reduced the dimensions to race=W and race=0 (accounting for Black, Asian, Hispanic,
# Native American, and Other). This binning creates two almost proportionally equivalent categories.  
bin_race <- function(race_vector) {
  race_vector %>%
    map_chr(function(race) {
      if (race %in% c("A", "B", "H", "N")) {
        return("O")
      }
      race
    }) %>%
    factor
}


##     Process Data     #############################################

# data not including race as binned into 2 categories (dataframe used for association rule mining)
# apply above functions, mutate columns to factors, and filter to only 2015 and 2016 years
shootings_complete %>%
  mutate(name = factor(name),
         manner_of_death = factor(manner_of_death),
         armed = bin_armed(armed),
         gender = factor(gender),
         race = factor(race),
         city = factor(city),
         state = factor(state),
         signs_of_mental_illness = factor(signs_of_mental_illness == "True"),
         threat_level = factor(threat_level),
         flee = factor(flee),
         body_camera = factor(body_camera == "True"),
         age = bin_age(age),
         season = getSeason(date)) %>%
         filter(year(date) == 2015 | year(date) == 2016) -> shootings2

# data including race binned
# apply above functions, mutate columns to factors, and filter to only 2015 and 2016 years
shootings_complete %>%
  mutate(name = factor(name),
         manner_of_death = factor(manner_of_death),
         armed = bin_armed(armed),
         gender = factor(gender),
         race = bin_race(race),
         city = factor(city),
         state = factor(state),
         signs_of_mental_illness = factor(signs_of_mental_illness == "True"),
         threat_level = factor(threat_level),
         flee = factor(flee),
         body_camera = factor(body_camera == "True"),
         age = bin_age(age),
         season = getSeason(date)) %>%
  filter(year(date) == 2015 | year(date) == 2016) -> shootings_binned


# Pull all factor columns
shootings2 %>%
  Filter(is.factor, .) %>%
  names ->
  colnames_factors

# Compare all factor types against each other with stacked bar charts
colnames_factors %>%
  walk(function(base_col){

    # Compare all but base_col against base_col
    colnames_factors %>%
      discard(~ . == base_col) %>%
      walk(function(other_col) {

        # collect totals to allow sum of each group to 1
        shootings2 %>%
          group_by_(other_col) %>%
          summarise(total_count = n()) ->
          totals

        # plot stacked bars
        shootings2 %>%
          inner_join(totals) %>%
          mutate(value_count = 1 / total_count) %>%
          select_(other_col, "value_count", base_col) %>%
          ggplot(
            aes(
              x = eval(as.name(paste(other_col))),
              y = value_count,
              fill = eval(as.name(paste(base_col)))
            )
          ) +
          geom_bar(stat="identity", width = 0.7) +
          labs(
            list(
              x=other_col,
              y="",
              title=paste0(base_col, " vs ", other_col),
              fill = base_col
            )
          ) +
          theme_minimal(base_size = 14) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) ->
          p

        # write plot to plots output
        ggsave(file.path('plots',paste0(base_col, ".",other_col,".png")), plot = p)
      })
  })



##     Association Rule Mining     #############################################

# Top 5 rules for different parameters based on lift
# Using non-binned race attribute

# Race=B
shootings2 %>%
  dplyr::select(-id, -date) %>%
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 2),
          appearance = list(lhs = c("race=B"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#         lhs         rhs                             support    confidence lift
# [1] {race=B} => {age=<26}                       0.08901099 0.3410526  1.646461
# [2] {race=B} => {signs_of_mental_illness=FALSE} 0.22197802 0.8505263  1.142404
# [3] {race=B} => {season=Spring}                 0.07142857 0.2736842  1.102003
# [4] {race=B} => {age=<35}                       0.08131868 0.3115789  1.088433
# [5] {race=B} => {armed=gun}                     0.15274725 0.5852632  1.055678


# Race=W
shootings2 %>%
  dplyr::select(-id, -date) %>%
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 2),
          appearance = list(lhs = c("race=W"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#         lhs         rhs                            support   confidence lift
# [1] {race=W} => {age=50+}                      0.1324176 0.2547569  1.495669
# [2] {race=W} => {signs_of_mental_illness=TRUE} 0.1670330 0.3213531  1.257769
# [3] {race=W} => {age=<50}                      0.1813187 0.3488372  1.098415
# [4] {race=W} => {flee=Not fleeing}             0.3741758 0.7198732  1.043959
# [5] {race=W} => {season=Summer}                0.1461538 0.2811839  1.042270


# Gender=M
shootings2 %>%
  dplyr::select(-id, -date) %>%
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 2),
          appearance = list(lhs = c("gender=M"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#         lhs           rhs                             support   confidence lift
# [1] {gender=M} => {season=Summer}                 0.2642857 0.2761194  1.023498
# [2] {gender=M} => {signs_of_mental_illness=FALSE} 0.7192308 0.7514351  1.009308
# [3] {gender=M} => {age=<35}                       0.2763736 0.2887486  1.008680
# [4] {gender=M} => {armed=gun}                     0.5335165 0.5574053  1.005429
# [5] {gender=M} => {threat_level=attack}           0.6307692 0.6590126  1.002845


# Gender=F
shootings2 %>%
  dplyr::select(-id, -date) %>%
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 2),
          appearance = list(lhs = c("gender=F"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#         lhs           rhs                            support    confidence lift
# [1] {gender=F} => {signs_of_mental_illness=TRUE} 0.01758242 0.4102564  1.605735
# [2] {gender=F} => {season=Winter}                0.01538462 0.3589744  1.390071
# [3] {gender=F} => {threat_level=other}           0.01538462 0.3589744  1.232704
# [4] {gender=F} => {age=<50}                      0.01648352 0.3846154  1.211073
# [5] {gender=F} => {race=W}                       0.02582418 0.6025641  1.159267


# Race=B and Gender=M
shootings2 %>%
  dplyr::select(-id, -date) %>%
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 3),
          appearance = list(lhs = c("race=B", "gender=M"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#         lhs                  rhs                             support    confidence lift
# [1] {gender=M,race=B} => {age=<26}                       0.08626374 0.3450549  1.665782
# [2] {gender=M,race=B} => {signs_of_mental_illness=FALSE} 0.21428571 0.8571429  1.151292
# [3] {gender=M,race=B} => {season=Spring}                 0.06758242 0.2703297  1.088496
# [4] {gender=M,race=B} => {armed=gun}                     0.14835165 0.5934066  1.070367
# [5] {gender=M,race=B} => {age=<35}                       0.07637363 0.3054945  1.067179


# Race=B and Gender=F
shootings2 %>%
  dplyr::select(-id, -date) %>%
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 3),
          appearance = list(lhs = c("race=B", "gender=F"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
# lhs                  rhs                    support    confidence lift
# [1] {gender=F,race=B} => {manner_of_death=shot} 0.01043956 0.95       1.015864


# Race=W and Gender=M
shootings2 %>%
  dplyr::select(-id, -date) %>%
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 3),
          appearance = list(lhs = c("race=W", "gender=M"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#         lhs                  rhs                            support   confidence lift
# [1] {gender=M,race=W} => {age=50+}                      0.1269231 0.2569522  1.508558
# [2] {gender=M,race=W} => {signs_of_mental_illness=TRUE} 0.1549451 0.3136819  1.227744
# [3] {gender=M,race=W} => {age=<50}                      0.1692308 0.3426029  1.078784
# [4] {gender=M,race=W} => {season=Summer}                0.1428571 0.2892102  1.072022
# [5] {gender=M,race=W} => {flee=Not fleeing}             0.3554945 0.7196885  1.043692


# Race=W and Gender=F
shootings2 %>%
  dplyr::select(-id, -date) %>%
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 3),
          appearance = list(lhs = c("race=W", "gender=F"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#         lhs                  rhs                            support    confidence lift
# [1] {gender=F,race=W} => {signs_of_mental_illness=TRUE} 0.01208791 0.4680851  1.832075
# [2] {gender=F,race=W} => {age=<50}                      0.01208791 0.4680851  1.473901
# [3] {gender=F,race=W} => {threat_level=attack}          0.01978022 0.7659574  1.165587
# [4] {gender=F,race=W} => {body_camera=FALSE}            0.02417582 0.9361702  1.054350
# [5] {gender=F,race=W} => {flee=Not fleeing}             0.01868132 0.7234043  1.049080


# From the above association rule mining, we can see that comparatively, race=B shootings are associated with
# younger age groups while race=W shootings are associated with older age groups.
# Female related shootings are associated with mental illnesses and winter, while males related shootings are associated
# with not having mental illnesses and summer


##     Classification Question       #############################################

# Over the past two years, the use of deadly force by police against civilians has caused fiery national debate as citizens
# and government agencies try to understand the motives and circumstances around such killings. Fatalities such as that of an unarmed
# black teenager by a white police officer in Ferguson, Missouri have prompted the public to question if police shootings are racially
# motivated. We intend to use the Washington Post police shooting database in order to determine if it is possible to classify a shooting event,
# (given attributes recorded such as mental health, gender, etc.) based on race. 

#     Descriptive Analysis - Logistic Regression                           #########################

# From our association rule mining we believe interesting trends pertaining to race may exist in our data. To statistically 
# determine if there is a correlation between our response (race) and our other variables, we use logistic regression.


s1.glm <- glm(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
                 body_camera + season, data = shootings2, family = binomial)
summary(s1.glm)
# armedgun                         1.663e+00  5.366e-01   3.099  0.00194 **
  

# The deviance of our model is lower than that of the null model so there is something in our data that might be able to 
# help us classify race. The only attribute that is significant based on a p value of < 0.05 is the attribute where a person
# is armed with a gun. 

# We also want to try our logistic regression models on two other scenarios that are based on race but take into account
# the high proportion of Race=W in the data: 
# 1. Race=W vs. Race=B and 2. Race=W vs. Race=O (with O taking the combined place of the previous 5 races, B, O, N, H, A)

# Race='W' v. Race='B' logistic regression
shootings2 %>%
  filter(race=='W' | race =='B') -> shootingBW
s2.glm <- glm(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
                body_camera + season, data = shootingBW, family = binomial)
summary(s2.glm)
# armedunarmed                    -6.294e-01  2.960e-01  -2.126   0.0335 *  
# age50+                           1.442e+00  4.938e-01   2.920   0.0035 ** 
# signs_of_mental_illnessTRUE      9.682e-01  1.763e-01   5.492 3.97e-08 ***
# fleeFoot                        -6.706e-01  2.647e-01  -2.533   0.0113 *  
  
# The above attributes are indicated as significant (based on p value < 0.05) so we will examine them in our classifiers later on
  

# Race='W' v. Race='O' logistic regression
s3.glm <- glm(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
                body_camera + season, data = shootings_binned, family = binomial)
summary(s3.glm)
# armedtoy weapon                  7.241e-01  3.028e-01   2.391 0.016793 *  
# age50+                           1.378e+00  4.055e-01   3.398 0.000678 ***
# stateOR                          2.313e+00  9.975e-01   2.319 0.020419 *  
# stateUT                          2.868e+00  1.227e+00   2.337 0.019444 *  
# signs_of_mental_illnessTRUE      7.504e-01  1.324e-01   5.668 1.45e-08 ***

# The above attributes are indicated as significant (based on p value < 0.05) so we will examine them in our classifiers later on


# Now that we have an idea of what attributes may be significant to our question of if we can classify based on race or not, we can
# start using random forest and naive bayes classifiers. We attempt to classify race based on race structured in three different ways:
# 1. Race=W vs. Race=B, H, A, N, O 
# 2. Race=W vs. Race=O (Race=W accounts for over 50% of the data so by binning the other groups into one, we make the data a little more proportional)
# 3. Race=W vs. Race=B (These are the two most populous categories of race. The media also tends to examine the dichotemy of these two races in particular 
# so we believe that this pairing is worth investigating separately as well)

####################################################################################################

#     Race=W vs. Race=B, H, A, N, O                    #############################################

####################################################################################################


# select only necessary variables
shootings2 %>%
  select(-id, -date, -name) -> shootings2_sub

# randomize order of shootings
shootings_rfdata <- shootings2_sub[sample(nrow(shootings2_sub)),]

# first random forest classifier based on race
rf_all <- J48(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
             body_camera + season, data = shootings_rfdata)
evaluate_Weka_classifier(rf_all, numfolds=10)
# === Summary ===
#
# Correctly Classified Instances        1252               68.7912 %
# Incorrectly Classified Instances       568               31.2088 %
# Kappa statistic                          0.4755
# Mean absolute error                      0.1438
# Root mean squared error                  0.2682
# Relative absolute error                 68.4181 %
# Root relative squared error             82.7589 %
# Total Number of Instances             1820
#
# === Confusion Matrix ===
#
# a   b   c   d   e   f   <-- classified as
# 2   3   8   0   1  14 |   a = A
# 0 260  39   0   1 175 |   b = B
# 0  16 183   0   0 122 |   c = H
# 0   1   1   6   0  16 |   d = N
# 0   4   4   0   7  11 |   e = O
# 1  80  67   3   1 794 |   f = W

# Accuracy is 68.7912% overall
# Accuracy for A: 2/28     = 0.07142857
# Accuracy for B: 260/475  = 0.5473684
# Accuracy for H: 183/321  = 0.5700935
# Accuracy for N: 6/24     = 0.25
# Accuracy for O: 7/26     = 0.2692308
# Accuracy for W: 794/946  = 0.8393235

# Our random forests poor classify by race due to a disproportionate amount of instances belong to race=W


# Random classifier built with attributes identified as significant with our logistic regression model (armed)
shootings2 %>%
  select(-id, -date, -name) -> shootings2_sub

# randomize order of shootings
shootings_rfdata <- shootings2_sub[sample(nrow(shootings2_sub)),]

# first random forest classifier based on race
rf_allsig <- J48(race ~ armed, data = shootings_rfdata)
evaluate_Weka_classifier(rf_allsig, numfolds=10)

# This classifier performance is very poor as it classifies every race as "W'

# We examine classifying race based on whether the victim was armed with a gun (armedgun was significant in our logistic regression)
shootings_rfdata %>%
  filter(armed == "gun") -> s_gun
s_gun %>% glimpse

rf_allGun <- J48(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
             body_camera + season, data = s_gun)
evaluate_Weka_classifier(rf_allGun, numfolds=10)
# === Summary ===
#   
# Correctly Classified Instances         601               59.5639 %
# Incorrectly Classified Instances       408               40.4361 %
# Kappa statistic                          0.2055
# Mean absolute error                      0.1813
# Root mean squared error                  0.3011
# Relative absolute error                 89.0666 %
# Root relative squared error             94.4769 %
# Total Number of Instances             1009     
# 
# === Confusion Matrix ===
#   
# a   b   c   d   e   f   <-- classified as
# 0   0   0   0   0   8 |   a = A
# 0 101   0   0   0 177 |   b = B
# 0  35   0   0   0 120 |   c = H
# 0   4   0   0   0  10 |   d = N
# 0   0   0   0   0  10 |   e = O
# 0  44   0   0   0 500 |   f = W

# Overall Accuracy: 59.5639%
# Accuracy A: 0/8    = 0
# Accuracy B: 101/178  = 0.5674157
# Accuracy H: 0/155 = 0
# Accuracy N: 4/14    = 0.2857143
# Accuracy O: 0/16    = 0
# Accuracy W: 500/544 = 0.9191176

# Our classifier poorly performs on race when examining the population of victims who were armed with a gun


#     Naive Bayes                          #########################

# select only necessary variables
shootings2 %>%
  dplyr::select(-id, -date, -name) -> shootings2_sub

# randomize order of shootings
shootings_rand <- shootings2_sub[sample(nrow(shootings2_sub)),]
indices <- sample(nrow(shootings2_sub), size=nrow(shootings2_sub)*.75)

# split train/test by 75/25
# training set
shootings_train <- shootings_rand[indices,]
shootings_train %>% glimpse

# testing set
shootings_test <- shootings_rand[-indices,]
shootings_test %>% glimpse

## Break down the training data for each issue into posterior probabilities:
## Invoke naiveBayes method
shooting_nb <- naiveBayes(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
                            body_camera + season, data = shootings_rand)

shooting_nb
summary(shooting_nb)

## Let's see how well our model predicts race:

nb_test_predict <- predict(shooting_nb,shootings_test$race)
#confusion matrix
table(pred=nb_test_predict,true=shootings_test$race)
#     true
# pred   A   B   H   N   O   W
#    A   0   0   0   0   0   0
#    B   0   0   0   0   0   0
#    H   0   0   0   0   0   0
#    N   0   0   0   0   0   0
#    O   0   0   0   0   0   0
#    W   7 116  78   5   7 242


## Fraction of correct predictions
mean(nb_test_predict==shootings_test$race)
# 0.5230769

## Create function to create, run and record model results
nb_multiple_runs <- function(train_fraction,n){
  fraction_correct <- rep(NA,n)
  for (i in 1:n){
    shootings2_sub[,'train'] <- ifelse(runif(nrow(shootings2_sub))<train_fraction,1,0)
    trainColNum <- grep('train',names(shootings2_sub))
    trainshootings2_sub <- shootings2_sub[shootings2_sub$train==1,-trainColNum]
    testshootings2_sub <- shootings2_sub[shootings2_sub$train==0,-trainColNum]
    nb_model <- naiveBayes(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
                             body_camera + season ,data = trainshootings2_sub)
    nb_test_predict <- predict(nb_model,testshootings2_sub[,-1])
    fraction_correct[i] <- mean(nb_test_predict==testshootings2_sub$race)
  }
  return(fraction_correct)
}

# 20 runs, 80% of data randomly selected for training set in each run
fraction_correct_predictions <- nb_multiple_runs(0.8,20)
fraction_correct_predictions
# 0.6373626 0.5722222 0.5625000 0.5845272 0.6314363 0.5669291 0.5888325 0.5741758 0.5864865 0.5642458 0.5566502 0.5771429
# 0.6039326 0.5913978 0.6106443 0.5733696 0.6062323 0.5263158 0.5726027 0.5646067

# summary of results
summary(fraction_correct_predictions)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.5263  0.5663  0.5757  0.5826  0.5945  0.6374 

# standard deviation
sd(fraction_correct_predictions)
# 0.02602038

# Our Naive Bayes classifier performs poorly as it classifies every instance as Race="W"

####################################################################################################

#     Race=W vs. Race=O                                #############################################

####################################################################################################


rf_WO <- J48(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
             body_camera + season, data = shootings_binned)
evaluate_Weka_classifier(rf_WO, numfolds=10)
# === Confusion Matrix ===
#
#   a   b   <-- classified as
# 513 361 |   a = O
# 206 740 |   b = W

# Overall Accuracy: 68.8462 %
# Accuracy O: 0.5869565
# Accuracy W: 0.782241

# Even after binning races into two categories to handle the high disproportionality of race=W, our random forest classifier isn't able
# to classify between genders very well. We continue our analysis into trying to classify by gender by further analyzing attributes


# We examine classifying race based on armed status, age, mental illness status, and state of shooting
# (attributes with at least one factor determined significant (p < 0.05) in our logistic regression model)

shootings_binned %>%
  filter(armed == "gun") -> s_sig

rf_WOg <- J48(race ~ armed + age + state + signs_of_mental_illness, data = s_sig)
evaluate_Weka_classifier(rf_WOg, numfolds=10)

# === Confusion Matrix ===
#   
#   a   b   <-- classified as
# 290 175 |   a = O
# 154 390 |   b = W
# Overall Accuracy: 67.3935 (down from 68.8462% from whole population)
# Accuracy O: 0.6236559 (up from 0.5869565)
# Accuracy W: 0.3482587 (up from 0.2830882)

# When this subset of the population, our overall accuracy slightly goes down by 1.45% but the accuracy
# of classifying our individual classes goes up. Our accuracy for classifying Race='O' (Other) increases by 0.0366994 (3.6%) 
# and our accuracy for classifying race='W' (White) increases by 0.0651705 (6.5%)


#     Naive Bayes                          #########################

# B, N, H, races consolidated into 'O' to account for high disproportionality of W

# select only necessary variables
shootings_binned %>%
  dplyr::select(-id, -date, -name) -> shootings2_sub2

# randomize order of shootings
shootings_rand <- shootings2_sub2[sample(nrow(shootings2_sub2)),]
indices <- sample(nrow(shootings2_sub2), size=nrow(shootings2_sub2)*.75)

# split train/test by 75/25
# training set
shootings_train <- shootings_rand[indices,]
shootings_train %>% glimpse

# testing set
shootings_test <- shootings_rand[-indices,]
shootings_test %>% glimpse

## Break down the training data for each issue into posterior probabilities:
## Invoke naiveBayes method
shooting_nb <- naiveBayes(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
                            body_camera + season, data = shootings_rand)

shooting_nb
summary(shooting_nb)

## Let's see how well our model predicts race:

nb_test_predict <- predict(shooting_nb,shootings_test$race)
#confusion matrix
table(pred=nb_test_predict,true=shootings_test$race)
#     true
# pred   O   W
# O      0   0
# W    217 238
sum(shootings_test$race == 'W')
238/248
## Fraction of correct predictions
mean(nb_test_predict==shootings_test$race)
# 0.5230769

#20 runs, 80% of data randomly selected for training set in each run
fraction_correct_predictions <- nb_multiple_runs(0.8,20)
fraction_correct_predictions

#summary of results
summary(fraction_correct_predictions)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5417  0.5802  0.5944  0.5902  0.6003  0.6167 

#standard deviation
sd(fraction_correct_predictions)
# 0.01809703

# Our Naive Bayes classifier performs poorly as it classifies every instance as Race="W"


####################################################################################################

#     Race=W vs. Race=B                                #############################################

####################################################################################################


# Full shooting data pared down to race='B' and race='W' since those are the most populous races
shootings2 %>%
  filter(race=='W' | race =='B') -> shootingBW

rf_BW <- J48(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
               body_camera + season, data = shootingBW)
evaluate_Weka_classifier(rf_BW, numfolds=10)

# === Summary ===
#   
# Correctly Classified Instances         946               66.5728 %
# Incorrectly Classified Instances       475               33.4272 %
# Kappa statistic                          0     
# Mean absolute error                      0.4698
# Root mean squared error                  0.6092
# Relative absolute error                105.5312 %
# Root relative squared error            129.1329 %
# Total Number of Instances             1421     
# 
# === Confusion Matrix ===
#   
# a   b   <-- classified as
# 0 475 |   a = B
# 0 946 |   b = W

# Subsetting our data to either race='W' or race='B' and then performing random forest classification does not produce a classifier
# that performs well. Our overall accuracy rate of 66.57% is misleading since every instance is classified as race='W' 


#     Random Forest Classification considering Race='W' v. Race='B' with Downsampling    #########################

# Manual downsampling
shootings2 %>%
  filter(race == 'W') -> shootingsW # 946
shootings2 %>%
  filter(race == 'B') -> shootingsB # 475

sampleW <- shootingsW[sample(1:nrow(shootingsW), 475,
                             replace=FALSE),]

sampleBW <- rbind(sampleW, shootingsB)

rf_BWDownSample <- J48(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
                         body_camera + season, data = sampleBW)
evaluate_Weka_classifier(rf_BWDownSample, numfolds=10)

# === Summary ===
#   
# Correctly Classified Instances         475               50      %
# Incorrectly Classified Instances       475               50      %
# Kappa statistic                          0     
# Mean absolute error                      0.5297
# Root mean squared error                  0.6478
# Relative absolute error                105.9493 %
# Root relative squared error            129.5561 %
# Total Number of Instances              950     
# 
# === Confusion Matrix ===
#   
#   a   b   <-- classified as
# 0 475 |   a = B
# 0 475 |   b = W

# Subsetting our data to either race='W' or race='B' and then performing random forest classification does not produce a classifier
# that performs well. Our overall accuracy rate of 50% is misleading since every instance is classified as race='W' 


# We examine classifying race based on armed status, age, mental illness status, and fleeing status 
# (attributes with at least one factor determined significant (p < 0.05) in our logistic regression model)

sampleBW %>%
  filter(armed == "gun") -> s_sig

rf_WBg <- J48(race ~ armed + age + signs_of_mental_illness + flee, data = s_sig)
evaluate_Weka_classifier(rf_WBg, numfolds=10)

# === Confusion Matrix ===
#   
# a   b   <-- classified as
# 0 278 |   a = B
# 0 269 |   b = W
# Overall Accuracy: 49.1773
# Accuracy O: 0.6236559 (up from 0.5869565)
# Accuracy W: 0.3482587 (up from 0.2830882)

# When considering this population, our overall accuracy slightly goes down by 1.45% but the accuracy
# of classifying our individual classes goes up. Our accuracy for classifying Race='O' (Other) increases by 0.0366994 (3.6%) 
# and our accuracy for classifying race='W' (White) increases by 0.0651705 (6.5%)


#     Naive Bayes                          #########################

# Naive Bayes Classification for race=W or race=B

# select only necessary variables
sampleBW %>%
  dplyr::select(-id, -date, -name) -> bayesdf_BW
bayesdf_BW$race <- factor(bayesdf_BW$race) # to reduce levels to B and W

# randomize order of shootings
shootings_rand <- bayesdf_BW[sample(nrow(bayesdf_BW)),]

indices <- sample(nrow(bayesdf_BW), size=nrow(bayesdf_BW)*.75)

# split train/test by 75/25
# training set
shootings_train <- shootings_rand[indices,]
shootings_train %>% glimpse

# testing set
shootings_test <- shootings_rand[-indices,]
shootings_test %>% glimpse

## Break down the training data for each issue into posterior probabilities:
## Invoke naiveBayes method
shooting_nb <- naiveBayes(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
                            body_camera + season, data = shootings_rand)

summary(shooting_nb)

## Let's see how well our model predicts race:

nb_test_predict <- predict(shooting_nb,shootings_test$race)
#confusion matrix
table(pred=nb_test_predict,true=shootings_test$race)
#     true
# pred   B   W
#    B 127 111
#    W   0   0

## Fraction of correct predictions
mean(nb_test_predict==shootings_test$race)
# 0.5336134

#20 runs, 80% of data randomly selected for training set in each run
fraction_correct_predictions <- nb_multiple_runs(0.8,20)
fraction_correct_predictions

#summary of results
summary(fraction_correct_predictions)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5606  0.5758  0.5916  0.5885  0.5989  0.6122 

#standard deviation
sd(fraction_correct_predictions)
# 0.01475072

# Our Naive Bayes classifier performs poorly as it classifies every instance as Race="B"




######################################################################################

##     Mental Health Analysis            #############################################

######################################################################################

# total mental illness prevalence
m <- shootings2[shootings2$signs_of_mental_illness == "TRUE",]
465/1820
# about a fourth of fatalities show signs of mental illness


# mental illness female
countfemale <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$gender == "F")
count_total_female <- sum(shootings2$gender == "F")
countfemale/count_total_female
# 41%

# mental illness male
countmale <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$gender == "M")
count_total_male <- sum(shootings2$gender == "M")
countmale/count_total_male
# 24.9%


# mental illness white
countwhite <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "W")
count_total_white <- sum(shootings2$race == "W")
countwhite/count_total_white
# 32.1%

# mental illness white female
countwhitef <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "W" &
                     shootings2$gender == "F")
total_white_f <- sum(shootings2$race == "W" & shootings2$gender == "F")
countwhitef/total_white_f
# 46.8%

# mental illness white male
countwhitem <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "W" &
                     shootings2$gender == "M")
total_white_m <- sum(shootings2$race == "W" & shootings2$gender == "M")
countwhitem/total_white_m
# 31.4%
# difference = 15.4%


# mental illness black
countblack <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "B")
count_total_black <-  sum(shootings2$race == "B")
countblack/count_total_black
# 14.9%

# mental illness black female
countblackf <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "B" &
                     shootings2$gender == "F")
total_black_f <- sum(shootings2$race == "B" & shootings2$gender == "F")
countblackf/total_black_f
# 30%

# mental illness black male
countblackm <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "B" &
                     shootings2$gender == "M")
total_black_m <- sum(shootings2$race == "B" & shootings2$gender == "M")
countblackm/total_black_m
# 14.3%
# difference = 15.7%


# mental illness hispanic
counthisp <-  sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "H")
count_total_hisp <- sum(shootings2$race == "H")
counthisp/count_total_hisp
# 20.9%

# mental illness hispanic female
counthispf <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "H" &
                    shootings2$gender == "F")
total_hisp_f <- sum(shootings2$race == "H" & shootings2$gender == "F")
counthispf/total_hisp_f
# 25%

# mental illness hispanic male
counthispm <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "H" &
                    shootings2$gender == "M")
total_hisp_m <- sum(shootings2$race == "H" & shootings2$gender == "M")
counthispm/total_hisp_m
# 20.8%
# difference = 4.2%


# mental illness asian
countasian <-  sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "A")
count_total_asian <- sum(shootings2$race == "A")
countasian/count_total_asian
# 32.1%


# No asian females in database


# mental illness native american
countn <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "N")
count_total_n <- sum(shootings2$race == "N")
countn/count_total_n
# 29.2%

# mental illness native american female
countnf <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "N" &
                 shootings2$gender == "F")
total_n_f <- sum(shootings2$race == "N" & shootings2$gender == "F")
countnf/total_n_f
# 40%

# mental illness native american male
countnm <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "N" &
                 shootings2$gender == "M")
total_n_m <- sum(shootings2$race == "N" & shootings2$gender == "M")
countnm/total_n_m
# 26.3%
# difference = 13.7%


# mental illness other
counto <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "O")
count_total_o <- sum(shootings2$race == "O")
counto/count_total_o
# 26.9%

# mental illness other female
countof <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "O" &
                 shootings2$gender == "F")
total_o_f <- sum(shootings2$race == "O" & shootings2$gender == "F")
countof/total_o_f
# 50%

# mental illness other male
countom <- sum(shootings2$signs_of_mental_illness == "TRUE" & shootings2$race == "O" &
                 shootings2$gender == "M")
total_o_m <- sum(shootings2$race == "O" & shootings2$gender == "M")
countom/total_o_m
# 25%
# difference = 25%


# Threat level investigation
stblack <- sum(shootings2$threat_level == "undetermined" & shootings2$race == "B" )
black <- sum(shootings2$race == "B")
stblack/black
# 5.47%

stwhite <- sum(shootings2$threat_level== "undetermined" & shootings2$race == "W" )
white <- sum(shootings2$race == "W")
stwhite/white
# 7.1%


# total killed black vs. white
black <- sum(shootings2$race == "B")
white <- sum(shootings2$race == "W")
(475/37685848) * 100000
# every 100,000 people, 1.26 blacks killed

(946/196817552) * 100000
# every 100,000 people, 0.481 whites killed


# Classifying mental illness
rf <- randomForest(formula = signs_of_mental_illness ~ age + manner_of_death +      gender + race + threat_level + flee + body_camera + armed,      data = shootings2, ntree = 49) 

#Type of random forest: classification
#Number of trees: 49
#No. of variables tried at each split: 2

#OOB estimate of  error rate: 25.82%
#Confusion matrix:
# FALSE TRUE class.error
#FALSE  1314   41   0.0302583
#TRUE    429   36   0.9225806

# Not very good classification


# downsampling

w<- shootings2[shootings2$race == "W",]
b <- shootings2[shootings2$race == "B",]

w2 <- w[sample(1:nrow(w), 475,replace=FALSE),]

sampleBW <- rbind(w, b)
rf <- randomForest(formula = signs_of_mental_illness ~ age + manner_of_death +      gender + race + threat_level + flee + body_camera + armed,      data = sampleBW, ntree = 49) 
rf

#OOB estimate of  error rate: 26.6%
#Confusion matrix:
#      FALSE TRUE class.error
#FALSE  1013   33  0.03154876
#TRUE    345   30  0.92000000

# Did not improve classification 


##     Appendix Code                     #############################################

# In determining a classification question, we looked at how different attributes in the data were generally able
# to classify without any pre-binning, downsampling, etc. We also looked at gender to see if it might be a good attribute to classify 
# on but due to high disproportionality (most victims in the data are male) are classifier did not perform well so we abandoned
# any further work on answering classifiation questions about gender

##     Random Forest Classifications     #############################################

We
rfg <- J48(gender ~ manner_of_death + armed + age + race + state + signs_of_mental_illness + threat_level + flee +
             body_camera + season, data = shootings_rfdata)
evaluate_Weka_classifier(rfg, numfolds=10)
# === Summary ===
#
# Correctly Classified Instances        1742               95.7143 %
# Incorrectly Classified Instances        78                4.2857 %
# Kappa statistic                          0
# Mean absolute error                      0.082
# Root mean squared error                  0.2025
# Relative absolute error                 99.4439 %
# Root relative squared error             99.9997 %
# Total Number of Instances             1820
#
# === Confusion Matrix ===
#
# a    b   <-- classified as
# 0   78 |    a = F
# 0 1742 |    b = M

# On gender, our random forests is a poor classifier due to there being 1742 male instances as compared to only 78 female instances



randomForest(age ~ manner_of_death + armed + gender + race + threat_level + flee + body_camera + signs_of_mental_illness, data = shootings2, ntree=64)
# OOB estimate of  error rate: 66.87%
# Confusion matrix:
#   <18 <26 <35 <50 50+ class.error
# <18   0   8  12  10   4   1.0000000
# <26   0 101 109 148  19   0.7320955
# <35   0 104 137 237  43   0.7370441
# <50   0  75 136 298  69   0.4844291
# 50+   0  16  40 187  67   0.7838710

randomForest(gender ~ manner_of_death + armed + age + race + threat_level + flee + body_camera + signs_of_mental_illness, data = shootings2, ntree=64)
# OOB estimate of  error rate: 4.29%
# Confusion matrix:
#   F    M class.error
# F 0   78           1
# M 0 1742           0

randomForest(manner_of_death ~ age + armed + gender + race + threat_level + flee + body_camera + signs_of_mental_illness, data = shootings2, ntree=64)
# OOB estimate of  error rate: 6.65%
# Confusion matrix:
#                  shot shot and Tasered class.error
# shot             1699                3 0.001762632
# shot and Tasered  118                0 1.000000000

randomForest(armed ~ age + manner_of_death + gender + race + threat_level + flee + body_camera + signs_of_mental_illness, data = shootings2, ntree=64)
# OOB estimate of  error rate: 37.36%
# Confusion matrix:
#              blades blunt gun other tools toy weapon unarmed undetermined vehicle class.error
# blades          164     0 111     0     1          0       8            3       6   0.4402730
# blunt            13     0  13     0     0          0       2            0       1   1.0000000
# gun              76     0 904     0     0          0       6            9      14   0.1040634
# other             2     0   4     0     0          0       3            0       0   1.0000000
# tools            29     0  15     0     0          0       4            1       0   1.0000000
# toy weapon       17     0  68     0     0          0       0            0       1   1.0000000
# unarmed          46     0  57     0     0          0      11           16      10   0.9214286
# undetermined     19     0  27     0     0          0       6           32       6   0.6444444
# vehicle          19     0  57     0     0          0       5            5      29   0.7478261

randomForest(signs_of_mental_illness ~ age + manner_of_death + gender + race + threat_level + flee + body_camera + armed, data = shootings2, ntree=64)
# OOB estimate of  error rate: 26.04%
# Confusion matrix:
#       FALSE TRUE class.error
# FALSE  1304   51  0.03763838
# TRUE    423   42  0.90967742

randomForest(flee ~ age + manner_of_death + gender + race + threat_level + signs_of_mental_illness + body_camera + armed, data = shootings2, ntree=64)
# OOB estimate of  error rate: 29.12%
# Confusion matrix:
#             Car Foot Not fleeing Other class.error
# Car          71    1         203     0  0.74181818
# Foot          4    0         219     0  1.00000000
# Not fleeing  32    4        1219     0  0.02868526
# Other         5    0          62     0  1.00000000

randomForest(body_camera ~ age + manner_of_death + gender + race + threat_level + signs_of_mental_illness + flee + armed, data = shootings2, ntree=64)
# OOB estimate of  error rate: 11.26%
# Confusion matrix:
#       FALSE TRUE  class.error
# FALSE  1615    1 0.0006188119
# TRUE    204    0 1.0000000000

randomForest(race ~ age + manner_of_death + gender + body_camera + threat_level + signs_of_mental_illness + flee + armed, data = shootings2, ntree=64)
# OOB estimate of  error rate: 45.38%
# Confusion matrix:
#   A   B H N O   W class.error
# A 0   1 0 0 0  27   1.0000000
# B 0 150 5 0 0 320   0.6842105
# H 0  60 4 0 0 257   0.9875389
# N 0   6 0 0 0  18   1.0000000
# O 0   1 0 0 0  25   1.0000000
# W 0 105 4 0 0 837   0.1152220
randomForest(race ~ age + manner_of_death + gender + body_camera + threat_level + signs_of_mental_illness + flee + armed, data = shootings_binned, ntree=64)
# Confusion matrix:
#     O   W class.error
# O 538 336   0.3844394
# W 348 598   0.3678647
