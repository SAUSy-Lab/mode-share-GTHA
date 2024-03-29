library(tidyverse)
library(wesanderson)
library(RColorBrewer)
require(reshape2)
library(Hmisc)

setwd("~/Dropbox/work/mode/mode-share-GTHA/")

## initial data loadaing

df <- as_tibble(read.csv("trip_cross_tabs/trips_cross_tabs.csv"))

df$mode_cats <- recode_factor(df$mode_cats, 'Auto Driver' = "Auto Driver", 'Auto Passenger' = "Auto Passenger","Transit public" = "Transit (Public)", "Transit go" = "Transit (GO)","Transit public and go" = "Transit (Public & GO)", 'taxi and rideshare' = "Taxi & Rideshare", 'Walk' = "Walk", 'Bicycle' = "Bicycle", 'Other' = "Z Other" ) #Z Other



# summary of just mode share for all trips

bmode <- df %>% 
  group_by(mode_cats) %>%
  summarise(
    total = sum(N)
  )

bmode$prop <- bmode$total / sum(bmode$total)

bmode$k <- round(bmode$prop * 1000,0)


k_modes <- rev(c(
  replicate(bmode$k[1], as.character(bmode$mode_cats[1])),
  replicate(bmode$k[2], as.character(bmode$mode_cats[2])),
  replicate(bmode$k[8], as.character(bmode$mode_cats[8])),
  replicate(bmode$k[6], as.character(bmode$mode_cats[6])),
  replicate(bmode$k[3], as.character(bmode$mode_cats[3])),
  replicate(bmode$k[4], as.character(bmode$mode_cats[4])),
  replicate(bmode$k[5], as.character(bmode$mode_cats[5])),
  replicate(bmode$k[7], as.character(bmode$mode_cats[7])),
  replicate(bmode$k[9], as.character(bmode$mode_cats[9]))
  ))

k_x <- rep(seq(1,25,by=1), 40)
k_y <- rep(1:40, each=25)

k <- data.frame(matrix(, nrow=1000, ncol=0))
k$k_x <- k_x
k$k_y <- k_y
k$k_modes <- k_modes
k$k_modes <- as.factor(k$k_modes)

scheme <- c('#f54254','#ffbd42','#30ff87','#2e2e2e','#327a3c','#a319ff','#24a4ff','#ff3df2','#b3b3b3')

legend <- c("Auto Driver","Auto Passenger","Bicycle","Taxi & Rideshare","Transit (GO)" ,"Transit (Public & GO)","Transit (Public)","Walk","Other")    

# plot at 400 x 400 px
ggplot() + 
  geom_point(aes(y = k$k_y, x = k$k_x, color = k$k_modes), size = 1, stroke = 1) +
  scale_colour_manual(values=scheme,labels = legend) +
  labs(color = "Primary travel mode\nper 1000 trips") +
  theme_void()



# summary of just income 

b_inc <- read.csv("trip_cross_tabs/N_trips_by_income.csv")
b_inc$prop <- b_inc$N / sum(b_inc$N)
b_inc$inc_cats <- rev(c("F) decline / don't know", "E) $125k + ","D) $100k - $125k", "C) $60k - $100k", "B) $40k - $60k", "A) < $40k" ))

spectral_scheme <- rev(c("#D53E4F", "#FC8D59", "#ffd766", "#9ec975", "#3eab5c", "#3288BD"))

ggplot() + 
  geom_bar(aes(x="", y=(100 * b_inc$prop), fill=forcats::fct_rev(b_inc$inc_cats)), stat="identity", colour="white", width = 0.5) +
  geom_text(aes(label = round(100 * b_inc$prop, 0),x="", round(100 * b_inc$prop, 0)), size = 3, position = position_stack(vjust = 0.5, reverse = FALSE), color = "white") +
  xlab("") + ylab("Proportion of households by yearly income (%)") + labs(fill = "") + 
  scale_fill_manual(values=spectral_scheme, labels =c("decline / don't know", "$125k + ","$100k - $125k", "$60k - $100k", "$40k - $60k", "< $40k" )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + coord_flip()



# number of trips by income group

ggplot() + 
  geom_segment(aes(y = b_inc$Trips_95l[1], x = b_inc$inc_cats[1], yend =  b_inc$trips_95h[1], xend = b_inc$inc_cats[1]), data = df) +
  geom_segment(aes(y = b_inc$Trips_95l[2], x = b_inc$inc_cats[2], yend =  b_inc$trips_95h[2], xend = b_inc$inc_cats[2]), data = df) +
  geom_segment(aes(y = b_inc$Trips_95l[3], x = b_inc$inc_cats[3], yend =  b_inc$trips_95h[3], xend = b_inc$inc_cats[3]), data = df) +
  geom_segment(aes(y = b_inc$Trips_95l[4], x = b_inc$inc_cats[4], yend =  b_inc$trips_95h[4], xend = b_inc$inc_cats[4]), data = df) +
  geom_segment(aes(y = b_inc$Trips_95l[5], x = b_inc$inc_cats[5], yend =  b_inc$trips_95h[5], xend = b_inc$inc_cats[5]), data = df) +
  geom_segment(aes(y = b_inc$Trips_95l[6], x = b_inc$inc_cats[6], yend =  b_inc$trips_95h[6], xend = b_inc$inc_cats[6]), data = df) +
geom_point(aes(x = b_inc$inc_cats, y = b_inc$trips_mean), size = 2) + 
  scale_y_continuous(limits = c(1.5,3)) + 
  xlab("Yearly Household Income") + ylab("Mean trips per person, per day") + 
  scale_x_discrete(labels =rev(c("decline / don't know", "$125k + ","$100k - $125k", "$60k - $100k", "$40k - $60k", "< $40k" ))) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))



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
  scale_fill_manual(values=spectral_scheme,labels = c("decline / don't know", "$125k + ","$100k - $125k", "$60k - $100k", "$40k - $60k", "< $40k" ))  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))



# income v mode!

b_inc_mode <- inner_join(b_mode_inc, bincome, by = "income_cat")
b_inc_mode$prop <- b_inc_mode$total.x / b_inc_mode$total

temp <- subset(b_inc_mode,b_inc_mode$mode_cats == "Bicycle" | b_inc_mode$mode_cats == "Walk")
pa <- ggplot() + geom_bar(aes(x=temp$income_cat, y=(100 * temp$prop), fill=forcats::fct_rev(temp$mode_cats)), stat="identity", colour="white", width = 0.5) + 
  xlab("Household Income") + ylab("Percent of Trips by Active Modes (%)") + labs(fill = "Travel Mode") + 
  scale_x_discrete(labels = rev(c("decline / don't know", "$125k + ","$100k - $125k", "$60k - $100k", "$40k - $60k", "< $40k" ))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


temp <- subset(b_inc_mode, (b_inc_mode$mode_cats == "Transit (Public)" | b_inc_mode$mode_cats == "Transit (GO)" | b_inc_mode$mode_cats == "Transit (Public & GO)"))
pt <- ggplot() + geom_bar(aes(x=temp$income_cat, y=(100 * temp$prop), fill=forcats::fct_rev(temp$mode_cats)), stat="identity", colour="white", width = 0.5) + 
  xlab("Household Income") + ylab("Percent of Trips by Transit (%)") + labs(fill = "Travel Mode") + 
  scale_x_discrete(labels = rev(c("decline / don't know", "$125k + ","$100k - $125k", "$60k - $100k", "$40k - $60k", "< $40k" ))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


temp <- subset(b_inc_mode, (b_inc_mode$mode_cats == "Auto Driver" | b_inc_mode$mode_cats == "Auto Passenger"))
pd <- ggplot() + geom_bar(aes(x=temp$income_cat, y=(100 * temp$prop), fill=forcats::fct_rev(temp$mode_cats)), stat="identity", colour="white", width = 0.5) + 
  xlab("Household Income") + ylab("Percent of Trips by Driving (%)") + labs(fill = "Travel Mode") + 
  scale_x_discrete(labels = rev(c("decline / don't know", "$125k + ","$100k - $125k", "$60k - $100k", "$40k - $60k", "< $40k" ))) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))





# now let's look at car-ownership

b_car <- c(0.111,0.114,0.232,0.156,0.387)

car_labels <- c("A VA >= 1", "B 0.5 < VA < 1", "C VA = 0.5", "D 0 < VA < 0.5", "E VA = 0")

ggplot() + 
  geom_bar(aes(x="", y=(100 * b_car), fill=forcats::fct_rev(car_labels)), stat="identity", colour="white", width = 0.5) +
  geom_text(aes(label = round(100 * b_car, 1),x="", round(100 * b_car, 1)), size = 3, position = position_stack(vjust = 0.5, reverse = FALSE), color = "white") +
  xlab("") + ylab("Proportions of households by auto ownership (%)") + labs(fill = "Vehicles per adult (VA) \nin the household") + 
  scale_fill_manual(values=spectral_scheme, labels =rev(c("VA = 0", "0 < VA < 0.5","VA = 0.5", "0.5 < VA < 1", "VA >= 1" ))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + coord_flip()



# mode share by car ownership

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
  xlab("Travel Mode") + ylab("Percent of trips by auto ownership (%)") + labs(fill = "Vechiles per adult\n in the household (VA)") + 
  scale_fill_manual(values=spectral_scheme, labels = c("VA >= 1", "0.5 < VA < 1", "VA = 0.5", "0 < VA < 0.5", "VA = 0"))  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))



# car ownership by mode share

bcar <-  df %>% 
  group_by(hhld_veh_per_adult_cat) %>%
  summarise(
    total = sum(N))

b_car_mode <- inner_join(b_mode_car, bcar, by = "hhld_veh_per_adult_cat")
b_car_mode$prop <- b_car_mode$total.x / b_car_mode$total.y

temp <- subset(b_car_mode,b_car_mode$mode_cats == "Bicycle" | b_car_mode$mode_cats == "Walk")
ggplot() + geom_bar(aes(x=temp$hhld_veh_per_adult_cat, y=(100 * temp$prop), fill=forcats::fct_rev(temp$mode_cats)), stat="identity", colour="white", width = 0.5) + 
  xlab("Household vehicles per adult (VA)") + ylab("Percent of Trips by Active Modes (%)") + labs(fill = "Travel Mode") + 
  scale_x_discrete(labels = rev(c("VA >= 1", "0.5 < VA < 1", "VA = 0.5", "0 < VA < 0.5", "VA = 0"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

temp <- subset(b_car_mode, (b_car_mode$mode_cats == "Transit (Public)" | b_car_mode$mode_cats == "Transit (GO)" | b_car_mode$mode_cats == "Transit (Public & GO)"))
ggplot() + geom_bar(aes(x=temp$hhld_veh_per_adult_cat, y=(100 * temp$prop), fill=forcats::fct_rev(temp$mode_cats)), stat="identity", colour="white", width = 0.5) + 
  xlab("Household vehicles per adult (VA)") + ylab("Percent of Trips by Transit (%)") + labs(fill = "Travel Mode") + 
  scale_x_discrete(labels = rev(c("VA >= 1", "0.5 < VA < 1", "VA = 0.5", "0 < VA < 0.5", "VA = 0"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


temp <- subset(b_car_mode, (b_car_mode$mode_cats == "Auto Driver" | b_car_mode$mode_cats == "Auto Passenger"))
ggplot() + geom_bar(aes(x=temp$hhld_veh_per_adult_cat, y=(100 * temp$prop), fill=forcats::fct_rev(temp$mode_cats)), stat="identity", colour="white", width = 0.5) + 
  xlab("Household vehicles per adult (VA)") + ylab("Percent of Trips by Car (%)") + labs(fill = "Travel Mode") + 
  scale_x_discrete(labels = rev(c("VA >= 1", "0.5 < VA < 1", "VA = 0.5", "0 < VA < 0.5", "VA = 0"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))



# trip times

times_all <- df %>% 
  group_by(mode_cats) %>%
  summarise(
    mean = wtd.mean(x = mean_time, weights = N)
)
times_all <- subset(times_all, times_all$mean > 0)
times_all[4,2] <- times_all[4,2] * 0.85
times_all[5,2] <- times_all[5,2] * 0.90

temp <- subset(df,df$income_cat == "A low <40")
times_all_low_inc <- temp %>% 
  group_by(mode_cats) %>%
  summarise(
    mean = wtd.mean(x = mean_time, weights = N)
  )
times_all_low_inc <- subset(times_all_low_inc, times_all_low_inc$mean > 0)
times_all_low_inc[4,2] <- times_all_low_inc[4,2] * 0.85
times_all_low_inc[5,2] <- times_all_low_inc[5,2] * 0.90

temp <- subset(df,df$hhld_veh_per_adult_cat == "A 0")
times_all_nocar <- temp %>% 
  group_by(mode_cats) %>%
  summarise(
    mean = wtd.mean(x = mean_time, weights = N)
  )
times_all_nocar <- subset(times_all_nocar, times_all_nocar$mean > 0)
times_all_nocar[4,2] <- times_all_nocar[4,2] * 0.85
times_all_nocar[5,2] <- times_all_nocar[5,2] * 0.90

temp <- subset(df,df$hhld_veh_per_adult_cat == "A 0" & df$income_cat == "A low <40")
times_all_nocar_lowince <- temp %>% 
  group_by(mode_cats) %>%
  summarise(
    mean = wtd.mean(x = mean_time, weights = N)
  )
times_all_nocar_lowince <- subset(times_all_nocar_lowince, times_all_nocar_lowince$mean > 0)
times_all_nocar_lowince[4,2] <- times_all_nocar_lowince[4,2] * 0.85
times_all_nocar_lowince[5,2] <- times_all_nocar_lowince[5,2] * 0.90

ggplot() + 
  geom_point(aes(x = times_all$mode_cats, y = times_all$mean)) +
  geom_point(aes(x = times_all_low_inc$mode_cats, y = times_all_low_inc$mean), color = "blue") +
  geom_point(aes(x = times_all_nocar$mode_cats, y = times_all_nocar$mean), color = "red") +
  geom_point(aes(x = times_all_nocar_lowince$mode_cats, y = times_all_nocar_lowince$mean), color = "purple") +
  theme_minimal()


# # # # # # # #
# #     # #
# # # # # # # #
# #     # #
# # # # # # # #

###########