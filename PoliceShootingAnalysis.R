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

##     Classification Question       #############################################

# Over the past two years, the use of deadly force by police against civilians has caused fiery national debate as citizens 
# and government agencies try to understand the motives and circumstances around such killings. Fatalities such as that of an unarmed
# black teenager by a white police officer in Ferguson, Missouri have prompted the public to question if police shootings are racially
# motivated. We intend to use the Washington Post police shooting database in order to determine if it is possible to classify a shooting event,
# (given attributes recorded such as mental health, gender, etc.) based on race. We also extend this classification question to see if
# a shooting event can be classified by what gender the victim is (male or female). 

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
shootings_complete %>%
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
# Native American, and Other)
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

# does not include race binned (for association rule mining)
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

# includes race binned
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

shootings2 %>%
  group_by(race) %>% 
  summarise(count = n_distinct(id))


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


# From the above association rule mining, we can see that in compartively, race=B shootings are associated with 
# younger age groups while race=W shootings are associated with older age groups. 
# Female related shootings are associated with mental illnesses and winter, while males related shootings are associated
# with not having mental illnesses and summer


##     Random Forest Classifications     #############################################

# get general idea of how attributes classify

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


#     Random Forest Classification based on Gender/Race (unbinned race)    #########################

# The following depicts classification evaluation for race unbinned (6 classes for race exist)

# select only necessary variables
shootings2 %>%
  select(-id, -date, -name) -> shootings2_sub

# randomize order of shootings
shootings_rfdata <- shootings2_sub[sample(nrow(shootings2_sub)),]

# first random forest classifier based on race
rf1 <- J48(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
             body_camera + season, data = shootings_rfdata)
evaluate_Weka_classifier(rf1, numfolds=10)
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

# Our random forests poor classify by race due to a disproportionate amount of distances belong to race=W

# second random forest classifier based on gender
rf2 <- J48(gender ~ manner_of_death + armed + age + race + state + signs_of_mental_illness + threat_level + flee +
             body_camera + season, data = shootings_rfdata)
evaluate_Weka_classifier(rf2, numfolds=10)
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

# On gender, our random forests is a poor classifier due to there being 1742 male instances as compared to only 78 instances


#     Random Forest Classification based on Race (binned race)    #########################

# The following depicts classification evaluation for race when binned into two categories: race=W and race=O (including A, B, H, N, O)
# This binning was done to handle the issue of race=W being represented disproportionately due to it accounting for over 50% of the data

rf3 <- J48(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
             body_camera + season, data = shootings_binned)
evaluate_Weka_classifier(rf3, numfolds=10)
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


#     Random Forest Classification by Race not considering guns         #########################

shootings_rfdata %>%
  filter(armed != "gun") -> s_nogun
s_nogun %>% glimpse

rf4 <- J48(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
             body_camera + season, data = s_nogun)
evaluate_Weka_classifier(rf4, numfolds=10)
# === Confusion Matrix ===
#   
# a   b   c   d   e   f   <-- classified as
# 0   2  11   0   1   6 |   a = A
# 0  99  36   0   0  62 |   b = B
# 0  16 108   0   0  42 |   c = H
# 0   0   1   2   0   7 |   d = N
# 0   2   4   0   4   6 |   e = O
# 0  37  66   2   0 297 |   f = W

# Overall Accuracy: 62.8853%
# Accuracy A: 0/20    = 0
# Accuracy B: 99/197  = 0.5025381
# Accuracy H: 108/166 = 0.6506024
# Accuracy N: 2/10    = 0.2
# Accuracy O: 4/16    = 0.25
# Accuracy W: 297/402 = 0.738806


shootings_binned %>%
  filter(armed != "gun") -> s_nogun
s_nogun %>% glimpse

rf5 <- J48(race ~ manner_of_death + armed + age + gender + state + signs_of_mental_illness + threat_level + flee +
             body_camera + season, data = s_nogun)
evaluate_Weka_classifier(rf5, numfolds=10)
# === Confusion Matrix ===
#   
#   a   b   <-- classified as
# 513 361 |   a = O
# 206 740 |   b = W
# Overall Accuracy: 72.7497 (up from 68.8462%)
# Accuracy O: 0.801956
# Accuracy W: 0.3482587


#     Naive Bayes Classification                                        #########################

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

## Fraction of correct predictions
mean(nb_test_predict==testHouseVotes84$Class)

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

#20 runs, 80% of data randomly selected for training set in each run
fraction_correct_predictions <- nb_multiple_runs(0.8,20)
fraction_correct_predictions

#summary of results
summary(fraction_correct_predictions)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5367  0.5808  0.6021  0.5956  0.6089  0.6313 

#standard deviation
sd(fraction_correct_predictions)
# 0.02287208
