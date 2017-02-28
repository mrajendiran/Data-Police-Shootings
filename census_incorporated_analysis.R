suppressMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(ggthemes)
  library(purrr)
  library(arules)
})
#source("PoliceShootingAnalysis.R")
# Requires the functions declared in the main analysis script: getSeason, bin_armed, bin_age
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

# creates separate column for seasons based on dates 
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231))
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}
census_shootings = read_delim("fatal-police-shootings-data-census.csv", delim=',', progress=F)

census_shootings_complete <- census_shootings[complete.cases(census_shootings), ] 

census_shootings_complete %>%
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
  filter(year(date) == 2015 | year(date) == 2016) -> census_shootings_2


census_shootings_2 %>% glimpse
# Generate some ratios for the housing types in the fatalities we could tie to census statistics
census_shootings_2$occupied_housing_units_renter_occupied_ratio = census_shootings_2$occupied_housing_units_renter_occupied/census_shootings_2$occupied_housing_units_total
census_shootings_2$occupied_housing_units_owned_free_and_clear_ratio = census_shootings_2$occupied_housing_units_owned_free_and_clear/census_shootings_2$occupied_housing_units_total
census_shootings_2$occupied_housing_units_owned_with_a_mortgage_or_a_loan_ratio = census_shootings_2$occupied_housing_units_owned_with_a_mortgage_or_a_loan/census_shootings_2$occupied_housing_units_total

# Race ratios, again for shootings that occurred in cities we could tie population statistics to
census_shootings_2$asian_ratio = census_shootings_2$asian_population/census_shootings_2$total_population
census_shootings_2$black_ratio = census_shootings_2$black_population/census_shootings_2$total_population
census_shootings_2$white_ratio = census_shootings_2$white_population/census_shootings_2$total_population
census_shootings_2$hispanic_ratio = census_shootings_2$hispanic_population/census_shootings_2$total_population

census_shootings_2[census_shootings_2$white_ratio <=.5,] -> census_shootings_higher_minority_pops

census_shootings_higher_minority_pops %>%
  group_by(armed) %>% 
  summarise(count = n_distinct(id)) -> census_shootings_hm_armed
census_shootings_hm_armed %>% glimpse
#$ armed <fctr> blades, blunt, gun, other, tools, toy weapon, unarmed, undetermined, vehicle
#$ count <int> 69, 5, 189, 4, 9, 15, 29, 27, 26

census_shootings_2[census_shootings_2$white_ratio >.5,] -> census_shootings_higher_white_pop

census_shootings_higher_white_pop %>%
  group_by(armed) %>% 
  summarise(count = n_distinct(id)) -> census_shootings_hw_armed
census_shootings_hw_armed %>% glimpse
# $ armed <fctr> blades, blunt, gun, other, tools, toy weapon, unarmed, undetermine...
# $ count <int> 176, 18, 690, 4, 31, 60, 95, 52, 82

census_shootings_hm_armed$count = census_shootings_hm_armed$count/sum(census_shootings_hm_armed$count)
census_shootings_hm_armed

census_shootings_hw_armed$count = census_shootings_hw_armed$count/sum(census_shootings_hw_armed$count)
census_shootings_hw_armed
## A tibble: 9 × 2
#armed       count
#<fctr>       <dbl>
#  1       blades 0.145695364
#2        blunt 0.014900662
#3          gun 0.571192053
#4        other 0.003311258
#5        tools 0.025662252
#6   toy weapon 0.049668874
#7      unarmed 0.078642384
#8 undetermined 0.043046358
#9      vehicle 0.067880795


census_shootings_higher_minority_pops %>% 
  group_by(signs_of_mental_illness) %>% 
  summarise(count = n_distinct(id)) -> census_shootings_hm_mental
census_shootings_hm_mental %>%  glimpse
#$ signs_of_mental_illness <fctr> FALSE, TRUE
#$ count                   <int> 289, 84
# 0.2906574

census_shootings_higher_white_pop %>% 
  group_by(signs_of_mental_illness) %>% 
  summarise(count = n_distinct(id)) -> census_shootings_hw_mental
census_shootings_hw_mental %>%  glimpse
#$ signs_of_mental_illness <fctr> FALSE, TRUE
#$ count                   <int> 896, 312
# 0.3482143

# Looking at the housing rates for the cities
census_shootings_2 %>%
  group_by(state) %>% 
  summarise(state_ratio = mean(occupied_housing_units_renter_occupied_ratio)) %>% 
  ggplot( aes(x=state, y=state_ratio, fill=state)) + 
  geom_bar(stat="identity") +
  labs(list(x="Count", title="Renter Occupied Ratio by State")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)
# Cities represented in this data mean renter occupied ratio

census_shootings_2 %>%
  group_by(state) %>% 
  summarise(state_ratio = mean(occupied_housing_units_owned_free_and_clear_ratio)) %>% 
  ggplot( aes(x=state, y=state_ratio, fill=state)) + 
  geom_bar(stat="identity") +
  labs(list(x="Count", title="Free and Clear Ratio by State")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)
# Cities represented in this data mean free and clear ratio

census_shootings_2 %>%
  group_by(state) %>% 
  summarise(state_ratio = mean(occupied_housing_units_owned_with_a_mortgage_or_a_loan_ratio)) %>% 
  ggplot( aes(x=state, y=state_ratio, fill=state)) + 
  geom_bar(stat="identity") +
  labs(list(x="Count", title="Free and Clear Ratio by State")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill=FALSE)
# Cities represented in this data mean free and clear ratio


#Categorizing the cities that the shootings occurred in as Renter, Morgage/Loan, or Free/Clear
#These categorizations say that if the city has that attribute, then more than 1/3 of the homes in the city are of that type
census_shootings_2$free_clear = ifelse(census_shootings_2$occupied_housing_units_owned_free_and_clear_ratio > .33, TRUE, FALSE)
census_shootings_2$morgage_loan = ifelse(census_shootings_2$occupied_housing_units_owned_with_a_mortgage_or_a_loan_ratio > .33, TRUE, FALSE)
census_shootings_2$renter = ifelse(census_shootings_2$occupied_housing_units_renter_occupied_ratio > .33, TRUE, FALSE)

census_shootings_2 %>%
  group_by(renter) %>% 
  summarise(count = n_distinct(id)) -> census_shootings_renter
census_shootings_renter %>% glimpse
#$ renter <lgl> FALSE, TRUE
#$ count  <int> 312, 1269
census_shootings_2 %>%
  group_by(morgage_loan) %>% 
  summarise(count = n_distinct(id)) -> census_shootings_morgage
census_shootings_morgage %>% glimpse
#$ morgage_loan <lgl> FALSE, TRUE
#$ count        <int> 402, 1179
census_shootings_2 %>%
  group_by(free_clear) %>% 
  summarise(count = n_distinct(id)) -> census_shootings_fc
census_shootings_fc %>% glimpse
#$ free_clear <lgl> FALSE, TRUE
#$ count      <int> 1503, 78


##########
#
# Association Rules with renter/morgage/free and clear as additional parameters
#
#
keeps = c("manner_of_death", "race", "signs_of_mental_illness", "flee", "armed", "threat_level", "body_camera", "city", "name", "state", "age", "gender", "season", "free_clear","morgage_loan","renter")

association_census_shootings = census_shootings_2[,which(names(census_shootings_2) %in% keeps)]

#Same queries as original analysis
association_census_shootings %>% 
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 2),
          appearance = list(lhs = c("race=B"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#lhs         rhs                             support    confidence lift    
#[1] {race=B} => {age=<26}                       0.09361164 0.3410138  1.609382
#[2] {race=B} => {signs_of_mental_illness=FALSE} 0.23339658 0.8502304  1.134358
#[3] {race=B} => {season=Spring}                 0.07400380 0.2695853  1.127551
#[4] {race=B} => {renter}                        0.24794434 0.9032258  1.125296
#[5] {race=B} => {age=<35}                       0.08602151 0.3133641  1.067734
#

# Race=W
association_census_shootings %>% 
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 2),
          appearance = list(lhs = c("race=W"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#lhs         rhs                            support   confidence lift    
#[1] {race=W} => {signs_of_mental_illness=TRUE} 0.1574953 0.3167939  1.264776
#[2] {race=W} => {age=<50}                      0.1752056 0.3524173  1.105500
#[3] {race=W} => {morgage_loan}                 0.3908918 0.7862595  1.054348
#[4] {race=W} => {flee=Not fleeing}             0.3548387 0.7137405  1.047747
#[5] {race=W} => {season=Winter}                0.1372549 0.2760814  1.036781


# Gender=M
association_census_shootings %>% 
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 2),
          appearance = list(lhs = c("gender=M"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#lhs           rhs                             support   confidence lift    
#[1] {gender=M} => {season=Summer}                 0.2599620 0.2721854  1.024584
#[2] {gender=M} => {signs_of_mental_illness=FALSE} 0.7223276 0.7562914  1.009027
#[3] {gender=M} => {age=<35}                       0.2820999 0.2953642  1.006403
#[4] {gender=M} => {armed=gun}                     0.5338393 0.5589404  1.005330
#[5] {gender=M} => {race=B}                        0.2631246 0.2754967  1.003595

# Gender=F
association_census_shootings %>% 
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 2),
          appearance = list(lhs = c("gender=F"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#lhs           rhs                            support    confidence lift    
#[1] {gender=F} => {signs_of_mental_illness=TRUE} 0.01771031 0.3943662  1.574477
#[2] {gender=F} => {season=Winter}                0.01707780 0.3802817  1.428089
#[3] {gender=F} => {age=<50}                      0.01834282 0.4084507  1.281271
#[4] {gender=F} => {race=W}                       0.02719798 0.6056338  1.218202
#[5] {gender=F} => {threat_level=other}           0.01518027 0.3380282  1.164319

# Race=B and Gender=M
association_census_shootings %>% 
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 3),
          appearance = list(lhs = c("race=B", "gender=M"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#lhs                  rhs                             support    confidence lift    
#[1] {race=B,gender=M} => {age=<26}                       0.09108159 0.3461538  1.633639
#[2] {race=B,gender=M} => {signs_of_mental_illness=FALSE} 0.22517394 0.8557692  1.141748
#[3] {race=B,gender=M} => {renter}                        0.23719165 0.9014423  1.123074
#[4] {race=B,gender=M} => {season=Spring}                 0.06957622 0.2644231  1.105960
#[5] {race=B,gender=M} => {armed=gun}                     0.15812777 0.6009615  1.080910

# Race=B and Gender=F
association_census_shootings %>% 
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 3),
          appearance = list(lhs = c("race=B", "gender=F"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#lhs                  rhs                    support    confidence lift    
#[1] {race=B,gender=F} => {renter}               0.01075269 0.9444444  1.176648
#[2] {race=B,gender=F} => {manner_of_death=shot} 0.01075269 0.9444444  1.008215
#[3] {race=B,gender=F} => {body_camera=FALSE}    0.01012018 0.8888889  1.008130

# Race=W and Gender=M
association_census_shootings %>% 
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 3),
          appearance = list(lhs = c("race=W", "gender=M"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#lhs                  rhs                            support   confidence lift    
#[1] {race=W,gender=M} => {signs_of_mental_illness=TRUE} 0.1448450 0.3082100  1.230505
#[2] {race=W,gender=M} => {age=<50}                      0.1619228 0.3445491  1.080818
#[3] {race=W,gender=M} => {season=Summer}                0.1321948 0.2812921  1.058864
#[4] {race=W,gender=M} => {morgage_loan}                 0.3693865 0.7860027  1.054004
#[5] {race=W,gender=M} => {flee=Not fleeing}             0.3352309 0.7133244  1.047136


# Race=W and Gender=F
association_census_shootings %>% 
  apriori(parameter = list(support = 0.01, confidence = 0.25, minlen = 3),
          appearance = list(lhs = c("race=W", "gender=F"), default = "rhs")) %>%
  sort(by = "lift") %>%
  head(5) %>%
  inspect
#lhs                  rhs                            support    confidence lift    
#[1] {race=W,gender=F} => {signs_of_mental_illness=TRUE} 0.01265022 0.4651163  1.856942
#[2] {race=W,gender=F} => {age=<50}                      0.01328273 0.4883721  1.531977
#[3] {race=W,gender=F} => {threat_level=attack}          0.02213789 0.8139535  1.242143
#[4] {race=W,gender=F} => {morgage_loan}                 0.02150538 0.7906977  1.060299
#[5] {race=W,gender=F} => {flee=Not fleeing}             0.01960784 0.7209302  1.058301