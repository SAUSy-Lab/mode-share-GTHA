library(tidyverse)
library(wesanderson)
library(RColorBrewer)

setwd("~/Dropbox/work/mode/mode-share-GTHA/")

## initial data loadaing

df <- as_tibble(read.csv("trip_cross_tabs/trips_cross_tabs.csv"))

df$mode_cats <- recode_factor(df$mode_cats, 'Auto Driver' = "Auto Driver", 'Auto Passenger' = "Auto Passenger","Transit public" = "Transit (Public)", "Transit go" = "Transit (GO)","Transit public and go" = "Transit (Public & GO)", 'taxi and rideshare' = "Taxi & Rideshare", 'Walk' = "Walk", 'Bicycle' = "Bicycle", 'Other' = "Other" )



# summary of just mode share for all trips

bmode <- df %>% 
  group_by(mode_cats) %>%
  summarise(
    total = sum(N)
  )

bmode$prop <- bmode$total / sum(bmode$total)



# summary of just income for all trips

bincome <- df %>% 
  group_by(income_cat) %>%
  summarise(
    total = sum(N)
  )

bmode$prop <- bmode$total / sum(bmode$total)


# mode v income




b_mode_inc <- df %>% 
  group_by(mode_cats,income_cat) %>%
  summarise(
    total = sum(N)
  )

b_mode_inc <- inner_join(b_mode_inc, bmode, by = "mode_cats")
b_mode_inc$prop <- b_mode_inc$total.x / b_mode_inc$total.y


spectral_scheme <- rev(c("#D53E4F", "#FC8D59", "#ffd766", "#9ec975", "#3eab5c", "#3288BD"))

ggplot() + 
  geom_bar(aes(x=b_mode_inc$mode_cats, y=(100 * b_mode_inc$prop), fill=forcats::fct_rev(b_mode_inc$income_cat)), stat="identity", colour="white", width = 0.5) +
  geom_text(aes(label = round(100 * b_mode_inc$prop, 0),x=b_mode_inc$mode_cats, round(100 * b_mode_inc$prop, 0)), size = 3, position = position_stack(vjust = 0.5, reverse = FALSE), color = "white") +
  xlab("Travel Mode") + ylab("Percent of trips by income group (%)") + labs(fill = "Household Income") + 
  scale_fill_manual(values=spectral_scheme,labels = c("decline / don't know", "$125k + ","100k - 125k", "$60k - 100k", "40k - 60k", "< 40k" ))  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))



# income v mode !

b_inc_mode <- inner_join(b_mode_inc, bincome, by = "income_cat")
b_inc_mode$prop <- b_inc_mode$total.x / b_inc_mode$total

temp <- subset(b_inc_mode,b_inc_mode$mode_cats == "Bicycle" | b_inc_mode$mode_cats == "Walk")
pa <- ggplot() + geom_bar(aes(x=temp$income_cat, y=(100 * temp$prop), fill=forcats::fct_rev(temp$mode_cats)), stat="identity", colour="white", width = 0.5) + 
  xlab("Household Income") + ylab("Percent of Trips by Active Modes (%)") + labs(fill = "Travel Mode") + 
  scale_x_discrete(labels = rev(c("decline / don't know", "$125k + ","100k - 125k", "$60k - 100k", "40k - 60k", "< 40k" ))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


temp <- subset(b_inc_mode, (b_inc_mode$mode_cats == "Transit (Public)" | b_inc_mode$mode_cats == "Transit (GO)" | b_inc_mode$mode_cats == "Transit (Public & GO)"))
pt <- ggplot() + geom_bar(aes(x=temp$income_cat, y=(100 * temp$prop), fill=forcats::fct_rev(temp$mode_cats)), stat="identity", colour="white", width = 0.5) + 
  xlab("Household Income") + ylab("Percent of Trips by Transit (%)") + labs(fill = "Travel Mode") + 
  scale_x_discrete(labels = rev(c("decline / don't know", "$125k + ","100k - 125k", "$60k - 100k", "40k - 60k", "< 40k" ))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


temp <- subset(b_inc_mode, (b_inc_mode$mode_cats == "Auto Driver" | b_inc_mode$mode_cats == "Auto Passenger"))
pd <- ggplot() + geom_bar(aes(x=temp$income_cat, y=(100 * temp$prop), fill=forcats::fct_rev(temp$mode_cats)), stat="identity", colour="white", width = 0.5) + 
  xlab("Household Income") + ylab("Percent of Trips by Driving (%)") + labs(fill = "Travel Mode") + 
  scale_x_discrete(labels = rev(c("decline / don't know", "$125k + ","100k - 125k", "$60k - 100k", "40k - 60k", "< 40k" ))) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


# now let's look at car-ownership

b_car <- df %>% 
  group_by(hhld_veh_per_adult_cat) %>%
  summarise(
    total = sum(N)
)

b_car$prop <- b_car$total / sum(b_car$total)

ggplot() + 
  geom_bar(aes(x=b_car$hhld_veh_per_adult_cat, y=(100 * b_car$prop), fill=forcats::fct_rev(b_car$hhld_veh_per_adult_cat)), stat="identity", colour="white", width = 0.5) +
  geom_text(aes(label = round(100 * b_mode_car$prop, 0),x=b_mode_car$mode_cats, round(100 * b_mode_car$prop, 0)), size = 3, position = position_stack(vjust = 0.5, reverse = FALSE), color = "white") +
  xlab("Travel Mode") + ylab("Percent of trips by income group (%)") + labs(fill = "Vechiles per adult\n in the household (VA)") + 
  scale_fill_manual(values=spectral_scheme, labels = c("VA >= 1", "0.5 < VA < 1", "VA = 0.5", "0 < VA < 0.5", "VA = 0"))  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))





b_mode_car <- df %>% 
  group_by(mode_cats,hhld_veh_per_adult_cat) %>%
  summarise(
    total = sum(N)
  )



b_mode_car <- inner_join(b_mode_car, bmode, by = "mode_cats")
b_mode_car$prop <- b_mode_car$total.x / b_mode_car$total.y


spectral_scheme <- wes_palette("Zissou1")

ggplot() + 
  geom_bar(aes(x=b_mode_car$mode_cats, y=(100 * b_mode_car$prop), fill=forcats::fct_rev(b_mode_car$hhld_veh_per_adult_cat)), stat="identity", colour="white", width = 0.5) +
  geom_text(aes(label = round(100 * b_mode_car$prop, 0),x=b_mode_car$mode_cats, round(100 * b_mode_car$prop, 0)), size = 3, position = position_stack(vjust = 0.5, reverse = FALSE), color = "white") +
  xlab("Travel Mode") + ylab("Percent of trips by income group (%)") + labs(fill = "Vechiles per adult\n in the household (VA)") + 
  scale_fill_manual(values=spectral_scheme, labels = c("VA >= 1", "0.5 < VA < 1", "VA = 0.5", "0 < VA < 0.5", "VA = 0"))  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


# # # # # # # #
# #     # #
# # # # # #
# #     # #
# # # # # # # #

###########