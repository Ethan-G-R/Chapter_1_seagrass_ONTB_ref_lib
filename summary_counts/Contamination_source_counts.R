# 02.11.24
# Ethan Ross

# In this script I will get counts for internal and external contamination 

library(writexl)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)

#################################### COI #######################################

All_ONTB_COI_contamination <- read_excel("summary_counts/Working_data/contamination/COI_contamination_record_v3.xlsx", sheet = 1)

str(All_ONTB_COI_contamination)

# 23 instances of internal contamination 
# 22 because 026_LC had both internal and external so I will count it as external
All_ONTB_COI_contamination %>%
  group_by(SpecimenID_OG) %>%
  dplyr::count(Type) %>% 
  filter(Type == "Internal") %>%
  nrow()

# 27 instances of external contamination
All_ONTB_COI_contamination %>%
  group_by(SpecimenID_OG) %>%
  dplyr::count(Type) %>% 
  filter(Type == "External") %>%
  nrow()

##################################### 18S ######################################

All_ONTB_18S_contamination <- read_excel("summary_counts/Working_data/contamination/18S_contamination_record_v3.xlsx", sheet = 1)

str(All_ONTB_18S_contamination)

# 2 instances of internal contamination 
All_ONTB_18S_contamination %>%
  group_by(SpecimenID_OG) %>%
  dplyr::count(Type) %>% 
  filter(Type == "Internal") %>%
  nrow()

# 9 instances of external contamination
All_ONTB_18S_contamination %>%
  group_by(SpecimenID_OG) %>%
  dplyr::count(Type) %>% 
  filter(Type == "External") %>%
  nrow()

