## STAT 2094 Final Project

library(tidyverse)  # for cleaning and piping
library(ggplot2)    # for plotting


# 1) Loading and cleaning data
data <- read.csv("C:\\Users\\Katrina Peralta\\Downloads\\Driveline Baseball\\Hitting Data\\poi_metrics.csv")

data_clean <- data %>% select(exit_velo_mph_x, pelvis_angular_velocity_maxhss_x, 
                              torso_angular_velocity_maxhss_x, x_factor_fp_x) %>%
  drop_na()

# 2) Call summary stats
summary(data_clean)

# 3) Visualization
# pelvis and exit velo
ggplot(data_clean, 
       aes(x=pelvis_angular_velocity_maxhss_x, y=exit_velo_mph_x)) +
  geom_point() + 
  geom_smooth(method='lm', se=TRUE) + 
  labs(x = "Pelvis Angular Velocity (deg/s)", y = "Exit Velocity (mph)", 
       title = "Exit Velocity Due to Pelvis Angular Velocity")
 
# torso and exit velo
ggplot(data_clean, 
       aes(x=torso_angular_velocity_maxhss_x, y=exit_velo_mph_x)) +
  geom_point() + 
  geom_smooth(method='lm', se=TRUE) + 
  labs(x = "Torso Angular Velocity (deg/s)", y = "Exit Velocity (mph)", 
       title = "Exit Velocity Due to Torso Angular Velocity")

# X-factor and exit velo
ggplot(data_clean, 
       aes(x=x_factor_fp_x, y=exit_velo_mph_x)) +
  geom_point() + 
  geom_smooth(method='lm', se=TRUE) + 
  labs(x = "X-Factor (deg)", y = "Exit Velocity (mph)", 
       title = "Exit Velocity Due to X-Factor at Foot Plant")

# 4) Multiple Linear Regression Model
model <- lm(exit_velo_mph_x ~ pelvis_angular_velocity_maxhss_x +
              torso_angular_velocity_maxhss_x +
              x_factor_fp_x, data = data_clean)
summary(model)

