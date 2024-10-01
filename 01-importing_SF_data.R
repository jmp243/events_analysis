# reading in dataframes for events analysis
# September 30, 2024

#### load libraries ####
library("tidyverse")
library(readr)
library(dplyr)
library(lubridate)
library(purrr)
#### create a Write csv function#### 
write_named_csv <- function(x) 
  write_csv(x, file = paste0(
    "~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/",
    deparse(substitute(x)),".csv"))

#### read in SF files ####
events_users <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/events_users.csv")
all_events <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/all_Events.csv")
all_published_events <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/all_published_events.csv")
# all_published_events <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/all_published_events.csv")
# attendees_report <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/attendees_report.csv")
Trellis_users <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/trellis_users.csv")
# Events_with_venue <- read_csv("~/Documents/Trellis/Events_predictive_analytics/events_analysis_from_salesforce/data/events_with_venue.csv")


# join trellis and events users
# Trellis_events_users <- events_users %>% 
#   left_join(Trellis_events_users)

# join data tables
# Trellis_events_users <-Trellis_users |>
#   filter(str_detect(`User: Feature`, 'Event'))
# 
# Trellis_events_users_long <-Trellis_events_users |>
#   separate_longer_delim(cols = `User: Feature`, delim = "; ")

all_events <- all_events %>% 
  full_join(all_published_events)

events_users_join <- all_events %>%
  left_join(events_users, by = c("Event: Owner Name" = "Full Name"))

# events_users_join <- events_users_join %>% 
#   left_join(events_users, by = c("Event: Owner Name" = "Full Name"))
# Trellis_users <- Trellis_users %>% 
#   select(-`First Name`, -`Last Name`)
Trellis_events_users <- events_users_join %>%
  left_join(events_users)
  # left_join(Trellis_users, by = c("NetID" = "NetID"))

# events_users_Venue_join <- events_users_join %>% 
#   left_join(Events_with_venue)

# Trellis_users$Orig_features <- Trellis_users$`User: Feature`


# omit draft
Trellis_events_users_wo_draft <-Trellis_events_users%>% 
  filter(Status != "Draft") %>% 
  filter(Status != "Canceled")

table(Trellis_events_users_wo_draft$Status)
table(Trellis_events_users_wo_draft$`Event: Owner Name`)

Trellis_events_users_wo_draft %>% keep(~all(is.na(.x))) %>% names
names(Trellis_events_users_wo_draft)

# Trellis_events_users_wo_draft <- Trellis_events_users_wo_draft %>% 
#   select(-c("Industry",            "Organizer (Account)", "Organizer (Contact)")) 
            
# test events to remove

test_subset <- subset(Trellis_events_users_wo_draft, grepl("test", `Event: Event Name`, ignore.case = TRUE))
test_demo <- subset(Trellis_events_users_wo_draft, grepl("demo", `Event: Event Name`, ignore.case = TRUE))
test_cancel <- subset(Trellis_events_users_wo_draft, grepl("cancel", `Event: Event Name`, ignore.case = TRUE))
# write_named_csv(test_demo)
# test_subset <- test_subset %>% 
#   select(`Event: Event Name`, Campaign, `Event: ID`)
write_named_csv(test_subset)
write_named_csv(test_cancel)

# remove events that are tests 
# Define the IDs you want to remove
remove_ids <- c("a3U3n0000098JOg", "a3U3n0000089no2", "a3U3n0000098KIF", "a3U3n00000IwLS7", 
                "a3U3n0000098JKZ", "a3U6R000002lEwd", "a3U6R000003BxTK", "a3U6R000002l1WQ", 
                "a3U3n0000098JEb", "a3U3n00000IqS8E", "a3U6R000003By0x", "a3U3n00000IqS1w", 
                "a3U3n0000098JPZ", "a3U3n0000089oOI", "a3U6R000002l15L", "a3U6R000002l15L", 
                "a3U6R000002l15L", "a3U6R000003BzS9", "a3U6R000002lEsC", "a3U6R000002lFPu", 
                "a3U3n00000IqSNe", "a3U3n00000IqSFe", "a3U6R000002l1dG", "a3U6R000003ByBW", 
                "a3U3n0000098J2S", "a3U6R000003BzYs", "a3U6R000002l1aW", "a3U3n0000098J97", 
                "a3U6R000002lCFN", "a3U6R000002lCFN", "a3U6R000002l1ch", "a3U6R000002l1cr", 
                "a3U3n00000IwL4P", "a3U6R000003ByNW", "a3U3n00000IqSSS", "a3U6R000002l1BO", 
                "a3U3n00000IqS3d", "a3U6R000002l0pI", "a3U6R000002l0pI", "a3U3n00000IwLcl", 
                "a3U3n00000IwLcl", "a3U6R000003Bx4d", "a3U6R000003Bx4n", "a3U6R000003Bx4s", 
                "a3U6R000002l0zw", "a3U6R000002lCEy", "a3U3n00000IwLGp", "a3U6R000003BybA", 
                "a3U3n00000IqS8O", "a3U6R000002l0sy", "a3U3n00000IqSG3", "a3U3n00000IwLZX", 
                "a3U3n00000IqSDx", "a3U3n00000IqSRj", "a3U6R000002l1Ps", "a3U6R000002lETb", 
                "a3U6R000002lE5B", "a3U6R000003C04m", "a3U3n0000098J23", "a3U3n0000089myZ", 
                "a3U3n00000IqSRj", "a3U3n0000089n3k", "a3U3n0000089n54", "a3U3n00000IqSUd", 
                "a3U3n00000IwLI2", "a3U3n00000IwLI7", "a3U3n000009KVzj", "a3U3n00000IwLU8", 
                "a3U6R000002lE2a", "a3U3n00000IwLbY", "a3U3n0000098J2D", "a3U6R000003Bxln")

# Remove the IDs
# filtered_events <- setdiff(events, remove_ids)
# Filter the dataframe to remove rows where Event_ID is in remove_ids
Trellis_events_users_wo_draft <- Trellis_events_users_wo_draft[!(Trellis_events_users_wo_draft$`Event: ID` %in% remove_ids), ]
Trellis_events_users_wo_draft$Created_Date <- as.Date(parse_date_time(Trellis_events_users_wo_draft$`Event: Created Date`,"mdy"))

# delete event name sdszdfds, Canceled, 
Trellis_events_users_wo_draft <- Trellis_events_users_wo_draft %>% 
  filter(`Event: Event Name` != "sdszdfds") %>% 
  filter(`Event: Event Name` != "Canceled") %>% 
  filter(`Event: Event Name` != "DO NOT USE") %>% 
  distinct()


names(Trellis_events_users_wo_draft)

Trellis_events_users_trim <- Trellis_events_users_wo_draft %>% 
  select(`Event: Event Name`, `Event: Owner Role`, `Event: Owner Name`, `Event: Created By`, `Event: Created Date`,   
         `Event Start Date`, `Event End Date`, `Event Format`,
          Audiences, `Venue City`, Category, Topic, Created_Date, `Event: Record Type`, `Event: ID`)

Trellis_events_users_trim <- Trellis_events_users_trim %>% 
  rename(`Event Location` = `Venue City`) %>% 
  rename(`Event Modality` = `Event Format`) 

write_named_csv(Trellis_events_users_trim)


# merge in user information
mismatched_cases <- Trellis_events_users_trim %>% 
  filter(`Event: Created By` != `Event: Owner Name`)

Trellis_events_users_trim_merge <- Trellis_events_users_trim %>% 
  left_join(Trellis_users, by = c("Event: Created By" = "Full Name")) %>%
  left_join(Trellis_users, by = c("Event: Owner Name" = "Full Name"))

names(Trellis_events_users_trim_merge)

# No_EventID_DF <- dplyr::filter(Trellis_events_users_wo_draft,is.na(`Event: ID`)) %>% 
#   distinct()
# 
# Trellis_events_users_w_EventID <- Trellis_events_users_wo_draft %>% 
#   filter(!is.na(`Event: ID`))
# 
# # Trellis_events_users_w_EventID$Created_Date <- as.Date(parse_date_time(Trellis_events_users_w_EventID$`Event: Created Date`,"mdy"))
# write_named_csv(Trellis_events_users_w_EventID)

# table of form assembly
table(Trellis_events_users_wo_draft$`Event: Created By`)

FormAssembly_list <- Trellis_events_users_wo_draft %>% 
  filter(`Event: Owner Name` == "FormAssembly") 

sapply(FormAssembly_list, function(FormAssembly_list)all(is.na(FormAssembly_list)))

FormAssembly_list %>% keep(~all(is.na(.x))) %>% names

# delete ones without any value
FormAssembly_list <- FormAssembly_list %>% 
  select(-c("Invited", "Registered", "Attended", 
            "Redirect URL", "RSVP Requested",
            # "First Name.x", 
            # "Last Name.x", 
            # "First Name.y", 
            # "Last Name.y", 
            "No Show",
            "Username", 
            "Alias", "Last Login", "NetID", 
            "Profile", "Role", "Full Name", 
            # "Emplid", 
            # "Primary Department: Account Name", "Title", "User: Profile: Name", 
            # "User: Feature", "Parent Organization", "EDS Primary Affiliation", 
            # "EDS Affiliations"
            )) %>% 
  distinct()

write_named_csv(FormAssembly_list)
