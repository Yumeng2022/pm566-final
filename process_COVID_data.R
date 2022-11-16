## data extracted from New York Times state-level data from NYT Github repository
# https://github.com/nytimes/covid-19-data

## state-level population information from us_census_data available on GitHub repository:
# https://github.com/COVID19Tracking/associated-data/tree/master/us_census_data

# load COVID state-level data from NYT
### FINISH THE CODE HERE ###
cv_states_readin <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") )

# load state population data
### FINISH THE CODE HERE ###
state_pops_readin <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_states.csv"))

state_pops <- state_pops_readin
state_pops$abb <- state_pops$state
state_pops$state <- state_pops$state_name
state_pops$state_name <- NULL

### FINISH THE CODE HERE ###
cv_states <- merge( cv_states_readin, state_pops, by="state")

# format the date
cv_states$date <- as.Date(cv_states$date, format="%Y-%m-%d")

# format the state variable
state_list <- unique(cv_states$state)
cv_states$state <- factor(cv_states$state, levels = state_list)

# format the state abbreviation (abb) variable
### FINISH THE CODE HERE ###
abb_list = unique(cv_states$abb)
cv_states$abb = factor(cv_states$abb, levels = abb_list)

# order the data first by state, second by date
cv_states = cv_states[order(cv_states$state, cv_states$date),]

# Add variables for new_cases and new_deaths:
for (i in 1:length(state_list)) {
  cv_subset = subset(cv_states, state == state_list[i])
  cv_subset = cv_subset[order(cv_subset$date),]
  
  # add starting level for new cases and deaths
  cv_subset$new_cases = cv_subset$cases[1]
  cv_subset$new_deaths = cv_subset$deaths[1]
  
  #### FINISH THE CODE HERE ###
  for (j in 2:nrow(cv_subset)) {
    cv_subset$new_cases[j] = cv_subset$cases[j] - cv_subset$cases[j-1] 
    cv_subset$new_deaths[j] = cv_subset$deaths[j] - cv_subset$deaths[j-1]
  }
  
  # include in main dataset
  cv_states$new_cases[cv_states$state==state_list[i]] = cv_subset$new_cases
  cv_states$new_deaths[cv_states$state==state_list[i]] = cv_subset$new_deaths
}

# set negative new case or death counts to 0
cv_states$new_cases[cv_states$new_cases<0] = 0
cv_states$new_deaths[cv_states$new_deaths<0] = 0

# Recalculate `cases` and `deaths` as cumulative sum of updates `new_cases` and `new_deaths`
for (i in 1:length(state_list)) {
  cv_subset = subset(cv_states, state == state_list[i])
  
  # add starting level for new cases and deaths
  cv_subset$cases = cv_subset$cases[1]
  cv_subset$deaths = cv_subset$deaths[1]
  
  for (j in 2:nrow(cv_subset)) {
    cv_subset$cases[j] = cv_subset$new_cases[j] + cv_subset$cases[j-1]
    cv_subset$deaths[j] = cv_subset$new_deaths[j] + cv_subset$deaths[j-1]
  }
  # include in main dataset
  cv_states$cases[cv_states$state==state_list[i]] = cv_subset$cases
  cv_states$deaths[cv_states$state==state_list[i]] = cv_subset$deaths
}

# add population normalized (by 100,000) counts for each variable
cv_states$per100k =  as.numeric(format(round(cv_states$cases/(cv_states$population/100000),1),nsmall=1))
cv_states$newper100k =  as.numeric(format(round(cv_states$new_cases/(cv_states$population/100000),1),nsmall=1))
cv_states$deathsper100k =  as.numeric(format(round(cv_states$deaths/(cv_states$population/100000),1),nsmall=1))
cv_states$newdeathsper100k =  as.numeric(format(round(cv_states$new_deaths/(cv_states$population/100000),1),nsmall=1))

# add a naive_CFR variable = deaths / cases
cv_states = cv_states %>% mutate(naive_CFR = round((deaths*100/cases),2))

# create a `cv_states_today` variable
max_date <- max(cv_states$date)
cv_states_today = cv_states %>% filter(date==as.Date(max_date))

