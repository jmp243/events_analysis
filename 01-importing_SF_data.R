# reading in dataframes for events analysis
# September 30, 2024

#### load libraries ####
library("tidyverse")
library(readr)
library(dplyr)
library(lubridate)

#### create a Write csv function#### 
write_named_csv <- function(x) 
  write_csv(x, file = paste0(
    "~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data",
    deparse(substitute(x)),".csv"))

#### read in SF files ####
events_users <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/events_users.csv")
all_events <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/all_Events.csv")
all_published_events <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/all_published_events.csv")
# all_published_events <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/all_published_events.csv")
# attendees_report <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/attendees_report.csv")
# events_by_role <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/events_by_role.csv")
# events_users <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/events_users.csv")
# Events_with_venue <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/Events_with_venue.csv")

# join data tables
all_events <- all_events %>% 
  full_join(all_published_events)

events_users_join <- all_events %>% 
  left_join(events_users, by = c("Event: Created By" = "Full Name"))

write_named_csv(events_users_join)

# table of form assembly
table(events_users_join$`Event: Created By`)
FormAssembly_list <- events_users_join %>% 
  filter(`Event: Created By` == "FormAssembly")
