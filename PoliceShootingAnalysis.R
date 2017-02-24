setwd("~/Documents/MSDS/1_Machine Learning/Projects/data-police-shootings")

suppressMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(ggthemes)
  library(purrr)
})

# Read in the CSV file
shootings <- read_delim("fatal-police-shootings-data.csv", delim=',', progress=F)
shootings %>% glimpse

# Factor variables except for date and age
shootings[] <- lapply(shootings, factor)
shootings$date  <- ymd(shootings$date)
shootings$age <- as.numeric(shootings$age)
shootings %>% glimpse

# Check for NAs
sum(is.na(shootings)) # 207 NAs
shootings %>%
  summarise_each(funs(sum(is.na(.))))
#    id  name  date manner_of_death armed   age gender  race  city state signs_of_mental_illness threat_level  flee body_camera
# <int> <int> <int>           <int> <int> <int>  <int> <int> <int> <int>                   <int>        <int> <int>       <int>
#     0     0     0               0     7    46      1   116     0     0                       0            0    37           0

# Inspect DF with complete cases
shootings_complete <- shootings[complete.cases(shootings), ] 
shootings_complete %>% glimpse # 1,959 cases from 2,126

# Separate cases between 2015 and 2016 (1820 total cases)
shootings_complete %>%
  filter(year(date) == 2015) -> shootings2015 # 947 obs
shootings2015 %>%
  summarise_each(funs(sum(is.na(.))))

shootings_complete %>%
  filter(year(date) == 2016) -> shootings2016 # 873 obs
shootings2016 %>%
  summarise_each(funs(sum(is.na(.))))

# Cases for 2017
shootings_complete %>%
  filter(year(date) == 2017) -> shootings2017 # 139 obs

# Descriptive Insights

# Gender
shootings_complete %>%
  group_by(gender) %>% 
  summarise(count = n_distinct(id)) -> shootings_gender
shootings_gender %>% glimpse
# gender <chr> "F", "M"
# count  <int> 81, 1878

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
        return("other")
      }
      if (armed == "unknown weapon") {
        return("undetermined")
      }
      armed
    }) %>%
    factor
}

getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231))
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}


shootings_complete %>% glimpse


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

shootings2 %>%
  group_by(body_camera) %>% 
  summarise(count = n_distinct(id))

# determine best attribute for classifying
library(randomForest)
randomForest(age ~ manner_of_death + armed + gender + race + threat_level + flee + body_camera + signs_of_mental_illness, data = shootings2, ntree=64)
randomForest(manner_of_death ~ age + armed + gender + race + threat_level + flee + body_camera + signs_of_mental_illness, data = shootings2, ntree=64)
randomForest(armed ~ age + manner_of_death + gender + race + threat_level + flee + body_camera + signs_of_mental_illness, data = shootings2, ntree=64)
randomForest(signs_of_mental_illness ~ age + manner_of_death + gender + race + threat_level + flee + body_camera + armed, data = shootings2, ntree=64)
randomForest(flee ~ age + manner_of_death + gender + race + threat_level + signs_of_mental_illness + body_camera + armed, data = shootings2, ntree=64)
randomForest(body_camera ~ age + manner_of_death + gender + race + threat_level + signs_of_mental_illness + flee + armed, data = shootings2, ntree=64)
randomForest(race ~ age + manner_of_death + gender + body_camera + threat_level + signs_of_mental_illness + flee + armed, data = shootings2, ntree=64)

library(bnlearn)

# use an optimization routine (hill-climbing in this case) to determine network structure.
shootingbbn <- hc(shootings2)

drops = c("id", "name", "state", "city")
shootingbbn$nodes = shootingbbn$nodes[-which(names(shootingbbn$nodes) %in% drops)]

shootingbbn$arcs <- shootingbbn$arcs[-which((shootingbbn$arcs[,'from'] == "season" & shootingbbn$arcs[,'to'] == "date")),]
shootingbbn$arcs <- shootingbbn$arcs[-which((shootingbbn$arcs[,'from'] == "threat_level" & shootingbbn$arcs[,'to'] == "date")),]

drops2 = c("season", "date")
shootingbbn$nodes = shootingbbn$nodes[-which(names(shootingbbn$nodes) %in% drops2)]

plot(shootingbbn)

# conditional table
shootingbbn <- hc(shootings2)
plot(shootingbbn)
fittedbn <- bn.fit(shootingbbn, data = shootings2)
traceback()
shootings2 %>% glimpse
