### Neighborhood Analysis using Arizona Data ###
library(ggplot2)
## Arizona data from census.gov/quickfacts/AZ 2014-2018
left_houses = 28
right_houses = 75
total_houses = left_houses + right_houses

## Arizona Demographics
average_persons_per_household_az = 2.69 
white_az = .544
black_az = .051
amerindian_az = .053
asian_az = .037
latinx_az = .316
total_race_az = white + black + amerindian + asian + latinx

## Finance using AZ data
median_value_owner_occupied_houses_az = 209600
median_household_income_az = 56213
value_of_all_homes_az = median_value_owner_occupied_houses_az * total_houses
annual_income_of_neighborhood_az = median_household_income_az * total_houses

## Will a car coming in the gate turn left or right?
prob_left = left_houses / total_houses
prob_right = right_houses / total_houses

## How many people in the neighborhood according to AZ demographics?
pop_az = average_persons_per_household_az * total_houses

## What is the racial composition of the neighborhood according to AZ demographics?
pop_white_az = pop_az*white_az
pop_black_az = pop_az*black_az
pop_amerindian_az = pop_az*amerindian_az
pop_asian_az = pop_az*asian_az
pop_latinx_az = pop_az*latinx_az
pop_frame_az = data.frame(pop_white_az,pop_black_az,pop_amerindian_az,pop_asian_az,pop_latinx_az)



### Neighborhood Analysis using Ahwatukee, Arizona Data ###
## Ahwatukee data from http://www.usa.com/85048-az.htm

## Ahwatukee 85048 Demographics
total_ahwatukee_pop = 36698
total_occupied_houses = 13827
average_persons_per_household_ah = total_ahwatukee_pop / total_occupied_houses
white_ah = .8331
black_ah = .0462
amerindian_ah = .0022
asian_ah = .0702
latinx_ah = .0947
total_race_ah = white_ah + black_ah + amerindian_ah + asian_ah + latinx_ah

## How many people in the neighborhood according to AZ demographics?
pop_ah = average_persons_per_household_ah * total_houses

## What is the racial composition of the neighborhood according to 85048 demographics?
pop_white_ah = pop_ah*white_ah
pop_black_ah = pop_ah*black_ah
pop_amerindian_ah = pop_ah*amerindian_ah
pop_asian_ah = pop_ah*asian_ah
pop_latinx_ah = pop_ah*latinx_ah
pop_frame_ah = data.frame(pop_white_ah,pop_black_ah,pop_amerindian_ah,pop_asian_ah,pop_latinx_ah)

## Finance using 85048 data
median_value_owner_occupied_houses_ah = 304800
median_household_income_ah = 95628
value_of_all_homes_ah = median_value_owner_occupied_houses_ah * total_houses
annual_income_of_neighborhood_ah = median_household_income_ah * total_houses



### Analysis of Differences ###
##Race
Race = c("White","Black","Amerindian","Asian","Latinx")
Location = c("Ahwatukee","Ahwatukee","Ahwatukee","Ahwatukee","Ahwatukee","Arizona","Arizona","Arizona","Arizona","Arizona")
values_r = c(pop_white_ah,pop_black_ah,pop_amerindian_ah,pop_asian_ah,pop_latinx_ah,
           pop_white_az,pop_black_az,pop_amerindian_az,pop_asian_az,pop_latinx_az)
race_diff_data = data.frame(Race, Location, values_r)

race_visualization = ggplot(race_diff_data, aes(fill=Location,y=values_r, x=Race)) + 
  geom_bar(position="dodge", stat="identity")

##Home Value
options(scipen=10000)
home_location = c("Ahwatukee", "Arizona")
home_value = c(value_of_all_homes_ah, value_of_all_homes_az)
home_data = data.frame(home_location, home_value)
home_visualization = ggplot(home_data, aes(y=home_value, x=home_location)) + 
  geom_bar(stat = "identity", color = "blue", fill = "white", width = .4) + 
  geom_text(aes(label=c("$31,394,400","$21,588,800")), vjust=.8,hjust = 2, color="blue", size=3.5)+
  ggtitle("Difference in Total Value of Homes") + xlab("Home Loaction") + ylab("Home Value in $") + coord_flip()

##Annual Neighborhood Income
inc_location = c("Ahwatukee", "Arizona")
inc_value = c(annual_income_of_neighborhood_ah, annual_income_of_neighborhood_az)
inc_data = data.frame(inc_location, inc_value)
income_visualization = ggplot(inc_data, aes(y=inc_value, x=inc_location)) + 
  geom_bar(stat = "identity", color = "blue", fill = "white", width = .4) + 
  geom_text(aes(label=c("$9,849,684","$5,789,939")), vjust=.8,hjust = 2, color="blue", size=3.5)+
  ggtitle("Difference in Annual Neighborhood Income") + xlab("Income Loaction") + ylab("Income in $") + coord_flip()




