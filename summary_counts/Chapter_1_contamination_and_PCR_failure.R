# 01.11.24
# Ethan Ross

# In this script I will look at the number of PCR failures
# and the number of contaminated OTUs

library(writexl)
library(readxl)
library(dplyr)
library(worms)
library(ggplot2)
library(stringr)
library(gridExtra)
library(tidyr)

################################################################################

All_barcode_data <- read_excel("summary_counts/Chapter_1_summary_with_PCRs.xlsx", sheet = 1)

str(All_barcode_data)

str(All_barcode_data %>% select(class))

class_percentage <- All_barcode_data %>%
  dplyr::count(class) %>%
  mutate(percentage = (n / sum(n)) * 100)

class_contam_percentage <- All_barcode_data %>%
  filter(Contamination_COI == "Y" | str_ends(stage_COI, "rerun")) %>%
  dplyr::count(class) %>%
  mutate(percentage = (n / sum(n)) * 100)

All_barcode_data %>%
  filter(Contamination_COI == "Y" | str_ends(stage_COI, "rerun")) %>%
  filter(is.na(class)) %>% select(FINAL_ID)


################################################################################
# Which classes contribute most to contamination?

# NEED TO REMOVE INTERNAL CONTAMINATION!!!

All_ONTB_COI_contamination <- read_excel("summary_counts/Working_data/contamination/COI_contamination_record_v3.xlsx", sheet = 1)

names(All_ONTB_COI_contamination)

All_ONTB_COI_contamination <- All_ONTB_COI_contamination %>% 
  select(Run, SpecimenID_OG, SpecimenID, Type) %>%
  dplyr::rename(RUN_COI = Run)

# # Step 1: Define Failure Status
# All_barcode_data_COI_contam <- All_barcode_data %>%
#   filter(!is.na(Contamination_COI)) %>%
#   mutate(Contamination_COI_status = if_else(Contamination_COI == "Y" | str_ends(stage_COI, "rerun"), "Failure", "Success"))

# Now lets add if the contamination is internal or external
All_barcode_data_COI_contam <- left_join(All_barcode_data, 
                                         All_ONTB_COI_contamination, 
                                         by = c("RUN_COI", "SpecimenID_OG", "SpecimenID"))

# Step 1: Define Failure Status
All_barcode_data_COI_contam <- All_barcode_data_COI_contam %>%
  filter(!is.na(Contamination_COI)) %>%
  mutate(Contamination_COI_status = if_else(!is.na(Type), "Failure", "Success"))

# Step 2: Filter for Classes with at Least 10 Unique FINAL_IDs
classes_with_enough_replicates <-  All_barcode_data_COI_contam %>%
  group_by(class) %>%
  filter(n_distinct(FINAL_ID) >= 10) %>%
  ungroup()

# Step 3: Summarize Failure Counts by Class
# Excluding rows which have internal contamination
Contamination_COI_summary <- classes_with_enough_replicates %>%
#  filter(Type == "External" | is.na(Type)) %>%
  group_by(class, Contamination_COI_status) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Contamination_COI_status, values_from = count, values_fill = list(count = 0))


# Step 4: Calculate Failure Proportion by Class
Contamination_COI_summary <- Contamination_COI_summary %>%
  mutate(total = Failure + Success,
         failure_rate = Failure / total)

# Step 5: Statistical Test for Disproportionate Contribution

# Create a contingency table
contingency_table <- table(classes_with_enough_replicates$class, classes_with_enough_replicates$Contamination_COI_status)

# Perform the chi-squared test
chi_test <- chisq.test(contingency_table)

# View results
print(chi_test)

Contamination_COI_summary <- Contamination_COI_summary %>% 
  mutate(Percentage_failure = failure_rate*100)

Contamination_COI_summary$class <- factor(Contamination_COI_summary$class, levels = c("Bivalvia", "Gastropoda", "Polychaeta", "Malacostraca"))

# Step 6: Plot the Contamination Rate by Class
p1 <- ggplot(Contamination_COI_summary, aes(x = class, y = Percentage_failure)) +
  geom_bar(stat = "identity", fill = "#EBCC2A", color = "black", width = 0.45, linewidth  = 1.5) +
  labs(x = "Class", y = "", title = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 45),
    axis.title.y = element_text(size = 45),
    axis.text.x = element_text(size = 45),
    axis.text.y = element_text(size = 45),
    legend.text = element_text(size = 40),
    legend.title = element_text(size = 45),
    text = element_text(size = 40),
    legend.position = "top",
    legend.justification = c("center", "top"),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5)
  ) +
  coord_flip() +
  ylim(0, 45) +
  # scale_x_discrete(limits = c("Bivalvia", "Gastropoda", "Polychaeta", "Malacostraca", "")) +
  # annotate("text", x = 4.5, y = 43, label = "(a)", size = 15, hjust = 0, vjust = 1)  # Adjusted y position
  annotate("text", x = 5, y = 0, label = "(a)", size = 15, hjust = 0, vjust = 1, alpha = 0)  # Adjusted y position

p1

# ggsave("Plots/Contamination_rate_by_class.tiff", plot = p1, width = 30, height = 11, bg = "white")

# Step 1: Filter for the Classes of Interest
target_classes <- c("Polychaeta", "Gastropoda", "Malacostraca", "Bivalvia")

# Calculate the total unique FINAL_IDs
total_final_ids <- All_barcode_data %>%
  summarise(total_ids = n_distinct(FINAL_ID)) %>%
  pull(total_ids)

# Calculate the percentage of FINAL_IDs belonging to each target class
class_percentages <- All_barcode_data %>%
  filter(class %in% target_classes) %>%
  group_by(class) %>%
  summarise(unique_ids = n_distinct(FINAL_ID)) %>%
  mutate(percentage = (unique_ids / total_final_ids) * 100)

# View the results
print(class_percentages)


# Calculate the percentage of unique FINAL_IDs for each class individually
Polychaete_percentage <- All_barcode_data %>%
  filter(class == "Polychaeta") %>%
  summarise(unique_ids = n_distinct(FINAL_ID),
            percentage = (unique_ids / total_final_ids) * 100)

Gastropoda_percentage <- All_barcode_data %>%
  filter(class == "Gastropoda") %>%
  summarise(unique_ids = n_distinct(FINAL_ID),
            percentage = (unique_ids / total_final_ids) * 100)

Malacostraca_percentage <- All_barcode_data %>%
  filter(class == "Malacostraca") %>%
  summarise(unique_ids = n_distinct(FINAL_ID),
            percentage = (unique_ids / total_final_ids) * 100)

Bivalve_percentage <- All_barcode_data %>%
  filter(class == "Bivalvia") %>%
  summarise(unique_ids = n_distinct(FINAL_ID),
            percentage = (unique_ids / total_final_ids) * 100)


# Print the results
Polychaete_percentage
Gastropoda_percentage
Malacostraca_percentage
Bivalve_percentage

################################################################################
# Which classes contribute most to PCR?

names(All_barcode_data)

# Step 1: Define Failure Status
All_barcode_data_COI_PCR_fail <- All_barcode_data %>%
  filter(!is.na(PCR_product_COI)) %>%
  mutate(PCR_failure_COI_status = if_else(PCR_product_COI == "N", "Failure", "Success"))

# Step 2: Filter for Classes with at Least 10 Unique FINAL_IDs
classes_with_enough_replicates <- All_barcode_data_COI_PCR_fail %>%
  group_by(class) %>%
  filter(n_distinct(FINAL_ID) >= 10) %>%
  ungroup()

# Step 3: Summarize Failure Counts by Class
PCR_success_COI_summary <- classes_with_enough_replicates %>%
  group_by(class, PCR_failure_COI_status) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = PCR_failure_COI_status, values_from = count, values_fill = list(count = 0))

# Step 4: Calculate Failure Proportion by Class
PCR_success_COI_summary <- PCR_success_COI_summary %>%
  mutate(total = Failure + Success,
         failure_rate = Failure / total)

# Step 5: Statistical Test for Disproportionate Contribution

# Create a contingency table
contingency_table <- table(classes_with_enough_replicates$class, classes_with_enough_replicates$PCR_failure_COI_status)

# Perform the chi-squared test
chi_test <- chisq.test(contingency_table)

# View results
print(chi_test)

PCR_success_COI_summary <- PCR_success_COI_summary %>% 
  mutate(Percentage_failure = failure_rate*100)

PCR_success_COI_summary$class <- factor(PCR_success_COI_summary$class, levels = c("Bivalvia", "Gastropoda", "Polychaeta", "Malacostraca"))

# Step 6: Plot the Contamination Rate by Class
p2 <- ggplot(PCR_success_COI_summary, aes(x = class, y = Percentage_failure)) +
  geom_bar(stat = "identity", fill = "#EBCC2A", color = "black", width = 0.45, linewidth  = 1.5) +
  labs(x = "Class", y = "", title = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 45),
    axis.title.y = element_text(size = 45),
    axis.text.x = element_text(size = 45),
    axis.text.y = element_text(size = 45),
    legend.text = element_text(size = 40),
    legend.title = element_text(size = 45),
    text = element_text(size = 40),
    legend.position = "top",
    legend.justification = c("center", "top"),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5)
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  # scale_x_discrete(limits = c("Bivalvia", "Gastropoda", "Polychaeta", "Malacostraca", "")) +
  # annotate("text", x = 4.5, y = 9.5, label = "(c)", size = 15, hjust = 0, vjust = 1)  # Adjusted y position
  annotate("text", x = 5, y = 0, label = "(c)", size = 15, hjust = 0, vjust = 1, alpha = 0)  # Adjusted y position
p2

# ggsave("Plots/PCR_failure_rate_by_class.tiff", plot = p2, width = 30, height = 11, bg = "white")

################################################################################

################################### 18S ########################################

# Which classes contribute most to contamination?

# NEED TO REMOVE INTERNAL CONTAMINATION!!!

All_ONTB_18S_contamination <- read_excel("summary_counts/Working_data/contamination/18S_contamination_record_v3.xlsx", sheet = 1)

names(All_ONTB_18S_contamination)
All_ONTB_18S_contamination <- All_ONTB_18S_contamination %>% 
  select(Run, SpecimenID_OG, SpecimenID, Type) %>%
  dplyr::rename(RUN_18S = Run)

# Step 1: Define Failure Status
All_barcode_data_18S_contam <- All_barcode_data %>%
  filter(!is.na(Contamination_18S)) %>%
  mutate(Contamination_18S_status = if_else(Contamination_18S == "Y" | str_ends(stage_18S, "rerun"), "Failure", "Success"))

# Now lets add if the contamination is internal or external
All_barcode_data_18S_contam <- left_join(All_barcode_data_18S_contam, 
               All_ONTB_18S_contamination, 
               by = c("RUN_18S", "SpecimenID_OG", "SpecimenID"))

# Now lets remove the rows where the contamination is internal 
All_barcode_data_18S_contam <- All_barcode_data_18S_contam %>%
  filter(Type == "External")

# Step 2: Filter for Classes with at Least 10 Unique FINAL_IDs
classes_with_enough_replicates <-  All_barcode_data_18S_contam %>%
  group_by(class) %>%
  filter(n_distinct(FINAL_ID) >= 2) %>%
  ungroup()

# Step 3: Summarize Failure Counts by Class
Contamination_18S_summary <- classes_with_enough_replicates %>%
  group_by(class, Contamination_18S_status) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Contamination_18S_status, values_from = count, values_fill = list(count = 0))

# Step 4: Calculate Failure Proportion by Class
Contamination_18S_summary <- Contamination_18S_summary %>%
  mutate(total = Failure + Success,
         failure_rate = Failure / total)

# Step 5: Statistical Test for Disproportionate Contribution

# Create a contingency table
contingency_table <- table(classes_with_enough_replicates$class, classes_with_enough_replicates$Contamination_18S_status)

# Perform the chi-squared test
chi_test <- chisq.test(contingency_table)

# View results
print(chi_test)

Contamination_18S_summary <- Contamination_18S_summary %>% 
  mutate(Percentage_failure = failure_rate*100)

Contamination_18S_summary$class <- factor(Contamination_18S_summary$class, levels = c("Bivalvia", "Gastropoda", "Polychaeta", "Malacostraca"))


# Step 6: Plot the Contamination Rate by Class
p3 <- ggplot(Contamination_18S_summary, aes(x = class, y = Percentage_failure)) +
  geom_bar(stat = "identity", fill = "#3C9AB2", color = "black", width = 0.45, linewidth  = 1.5) +
  labs(x = "Class", y = "Percentage of Contaminated OTUs", title = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 45),
    axis.title.y = element_text(size = 45),
    axis.text.x = element_text(size = 45),
    axis.text.y = element_text(size = 45),
    legend.text = element_text(size = 40),
    legend.title = element_text(size = 45),
    text = element_text(size = 40),
    legend.position = "top",
    legend.justification = c("center", "top"),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5)
  ) +
  coord_flip() +
  ylim(0, 45) +
  # scale_x_discrete(limits = c("Bivalvia", "Gastropoda", "Polychaeta", "Malacostraca", "")) +
  # annotate("text", x = 4.5, y = 43, label = "(b)", size = 15, hjust = 0, vjust = 1)  # Adjusted y position
  annotate("text", x = 5, y = 0, label = "(b)", size = 15, hjust = 0, vjust = 1, alpha = 0)  # Adjusted y position

p3

#ggsave("Plots/Contamination_rate-18S_by_class.tiff", plot = p3, width = 30, height = 11, bg = "white")

# Which classes contribute most to 18S PCR failure?

# Step 1: Define Failure Status
All_barcode_data_18S_PCR_fail <- All_barcode_data %>%
  filter(!is.na(PCR_product_18S)) %>%
  mutate(PCR_failure_18S_status = if_else(PCR_product_18S == "N", "Failure", "Success"))

# Step 2: Filter for Classes with at Least 10 Unique FINAL_IDs
classes_with_enough_replicates <-  All_barcode_data_18S_PCR_fail %>%
  group_by(class) %>%
  filter(n_distinct(FINAL_ID) >= 10) %>%
  ungroup()

# Step 3: Summarize Failure Counts by Class
PCR_success_18S_summary <- classes_with_enough_replicates %>%
  group_by(class, PCR_failure_18S_status) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = PCR_failure_18S_status, values_from = count, values_fill = list(count = 0))

# Step 4: Calculate Failure Proportion by Class
PCR_success_18S_summary <- PCR_success_18S_summary %>%
  mutate(total = Failure + Success,
         failure_rate = Failure / total)

# Step 5: Statistical Test for Disproportionate Contribution

# Create a contingency table
contingency_table <- table(classes_with_enough_replicates$class, classes_with_enough_replicates$PCR_failure_18S_status)

# Perform the chi-squared test
chi_test <- chisq.test(contingency_table)

# View results
print(chi_test)

PCR_success_18S_summary <- PCR_success_18S_summary %>% 
  mutate(Percentage_failure = failure_rate*100)

PCR_success_18S_summary$class <- factor(PCR_success_18S_summary$class, levels = c("Bivalvia", "Gastropoda", "Polychaeta", "Malacostraca"))

# Step 6: Plot the Contamination Rate by Class
p4 <- ggplot(PCR_success_18S_summary, aes(x = class, y = Percentage_failure)) +
  geom_bar(stat = "identity", fill = "#3C9AB2", color = "black", width = 0.45, linewidth  = 1.5) +
  labs(x = "Class", y = "Percentage PCR failures", title = "") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 45),
    axis.title.y = element_text(size = 45),
    axis.text.x = element_text(size = 45),
    axis.text.y = element_text(size = 45),
    legend.text = element_text(size = 40),
    legend.title = element_text(size = 45),
    text = element_text(size = 40),
    legend.position = "top",
    legend.justification = c("center", "top"),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(1.5, "cm"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5)
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  # scale_x_discrete(limits = c("Bivalvia", "Gastropoda", "Polychaeta", "Malacostraca", "")) +
  # annotate("text", x = 4.5, y = 9.5, label = "(d)", size = 15, hjust = 0, vjust = 1)  # Adjusted y position
  annotate("text", x = 5, y = 0, label = "(d)", size = 15, hjust = 0, vjust = 1, alpha = 0)  # Adjusted y position

p4

################################ combine #######################################

combo1 <- grid.arrange(
  p1, p3, 
  nrow = 2, 
  ncol = 1)

ggsave("Plots/Contamination_rates_by_class.tiff", plot = combo1, width = 15, height = 24, bg = "white")

combo2 <- grid.arrange(
  p2, p4, 
  nrow = 2, 
  ncol = 1)

ggsave("Plots/PCR_failure_rates_by_class.tiff", plot = combo2, width = 15, height = 24, bg = "white")

