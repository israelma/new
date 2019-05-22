################################### Set up ###################################
## install.packages("dplyr")
library("dplyr")

## Reads the any_drinking data frame
any_drinking <- read.csv('data/any_drinking.csv', stringsAsFactors = FALSE)
View(any_drinking)

## Reads the binge_drinking data frame
binge_drinking <- read.csv('data/binge_drinking.csv', stringsAsFactors = FALSE)
View(binge_drinking)

################################### Any drinking in 2012 #####################

## Creates dataframe that has the 'state' and 'location' columns in 2012.
select_rows <- select(any_drinking, state, location, females_2012, males_2012)

## Adds a column that has the difference in male and female drinking patterns
data_2012 <- mutate(select_rows, difference = abs(females_2012 - males_2012))

## Writes a csv file of the data_2012 dataframe.
write.csv(data_2012, file = 'output/year_2012.csv', row.names = FALSE)

## Creates dataframe where females drink more than males
female_drink <- filter(data_2012, females_2012 > males_2012)
## There are no locations where females drink more than males.

## Creates dataframe that shows which location has the smallest difference 
## in drinking rates.
smallest_difference <- filter(data_2012, difference == min(abs(females_2012 - males_2012)))

## Creates new dataframe the groups the states and combines drinking rates for each.
state_data <- group_by(data_2012, state) %>%
  filter(state != "National") %>% 
  summarize(
    combined = sum(females_2012, males_2012)
  )
View(state_data)

## Dataframe of the state with the lowest level
lowest_level <- filter(state_data, combined == min(combined))

## Writes a csv file of the lowest_level dataframe.
write.csv(lowest_level, file = 'output/lowest_level_state.csv', row.names = FALSE)

## Function that specifies a state then saves a .csv file with only observations from
## that state.
specified_state <- function(state_name) {
  info_state <- filter(any_drinking, state == state_name)
  file_name <- paste("output/", state_name, ".csv")
  new_file <- write.csv(info_state, file = file_name, 
                        row.names = FALSE)
  return(new_file)
}

## Demonstrates that the function works
specified_state("Washington")
specified_state("Oregon")
specified_state("California")



################################### Binge drinking Dataset ###################

## Creates dataframe with only county level observations from the binge_drinking dataset
county_level <- filter(binge_drinking, grepl('County|Borough|Area|Municipality|Parish|City',
                                             location))

## Average level of binge drinking in 2012 for both sexes across the counties
average <- summarize(county_level, average = mean(both_sexes_2012))

## Minimum level of binge drinking in each state in 2012 for both sexes
minimum_data <- filter(binge_drinking, state != "National") %>% 
  group_by(state) %>%
  summarize(
    minumum = min(both_sexes_2012)
  )



################################### Joining Data ###################################




## ------------------------------ Write a function to ask your own question(s) --------------

below_30 <- function(state_name) {
  
  
}

