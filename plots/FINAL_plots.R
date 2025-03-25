# 19/11/24
# Ethan Ross

# In this script I will have all my final plots in one place

library(writexl)
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(wesanderson)
library(viridis) 
library(RColorBrewer)
library(ggthemes)
library(extrafont)
library(gridExtra)
library(cowplot)
library(forcats)
library(ggh4x)

############################# ONT vs Sanger ####################################

########### Sequence Recovery ##################################

seq_recovery <- read_excel("plots/plot_data/Sanger_vs_ONTB_data_for_plotting.xlsx", sheet = 1)

seq_recovery$Sequencing_method <- factor(seq_recovery$Sequencing_method, 
                                         levels = c("ONT", "Sanger"))

seq_recovery$Category <- factor(seq_recovery$Category, 
                                levels = c("LCO only", "Contig"))

# Plotting
p1 <- ggplot(seq_recovery, aes(x = Sequencing_method, y = Count, fill = Category)) +
  geom_bar(stat = "identity", color = "black", width = 0.9, linewidth = 1.5, aes(group = Category)) +
  theme_classic() +
  labs(x = "", y = "", title = "") +
  ggtitle("") +
  scale_fill_manual(values = c("#F2F2F2", "#D9D9D9"), labels = c(" LCO only ", " Contig ")) +
  theme(
    text = element_text(size = 60),
    axis.text.y = element_text(hjust = 0.5),
    legend.position = "top",  
    legend.margin = margin(20, 0, 0, 0), 
    legend.text = element_text(size = 40),  
    legend.title = element_text(size = 1)
  ) + 
  coord_flip() +
  scale_x_discrete(expand = expansion(add = c(1, 0))) + # Add space before the first bar
  scale_y_continuous(limits = c(0, 70)) +  # Use scale_y_continuous instead of ylim
  annotate("text", x = 3, y = 0, label = "(a)", size = 15, hjust = 0, vjust = 1) +  # Adjusted y position
  annotate("text", x = 3.2, y = 0, label = "", size = 15, hjust = 0, vjust = 1) +  # Adjusted y position
  geom_text(data = subset(seq_recovery, Count > 0),  # Filter out zero counts
            aes(y = Count, label = Count), 
            position = position_stack(vjust = 0.5), size = 15, color = "black") 

#####
p1
#####
########### Sequence Quality ###################################

seq_quality <- read_excel("plots/plot_data/Sanger_vs_ONTB_data_for_plotting.xlsx", sheet = 2)

seq_quality$Sequencing_method <- factor(seq_quality$Sequencing_method, 
                                        levels = c("ONT", "Sanger"))

seq_quality$Ambiguous_bases <- factor(seq_quality$Ambiguous_bases, 
                                      levels = c("With", "Without"))
# Plotting
p2 <- ggplot(seq_quality, aes(x = Sequencing_method, y = Count, fill = Ambiguous_bases)) +
  geom_bar(stat = "identity", color = "black", width = 0.9, linewidth = 1.5, aes(group = Ambiguous_bases)) +
  theme_classic() +
  labs(x = "", y = "", title = "") +
  ggtitle("") +
  scale_fill_manual(values = c("#EBCC2A", "#D9D9D9"), labels = c(" With Ambiguous Bases ", " Without Ambiguous Bases ")) +
  theme(text = element_text(size = 60),
        axis.text.y = element_text(hjust = 0.5),
        legend.position = "top",  # Adjust legend position within the plot area
        legend.margin = margin(20, 0, 0, 0), 
        legend.text = element_text(size = 40),  # Adjust text size in legend
        legend.title = element_text(size = 1)) + # Adjust size of legend key
  coord_flip() +
  scale_x_discrete(expand = expansion(add = c(1, 0))) + # Add space before the first bar
  scale_y_continuous(limits = c(0, 70)) +  # Use scale_y_continuous instead of ylim
  annotate("text", x = 3, y = 0, label = "(b)", size = 15, hjust = 0, vjust = 1) +  # Adjusted y position
  annotate("text", x = 3.2, y = 0, label = "", size = 15, hjust = 0, vjust = 1) +  # Adjusted y position
  geom_text(data = subset(seq_quality, Count > 0),  # Filter out zero counts
            aes(y = Count, label = Count), 
            position = position_stack(vjust = 0.5), size = 15, color = "black") 

#####
p2
#####
########### Taxonomic Recovery (contamination) #########################

taxonomic_recovery <- read_excel("plots/plot_data/Sanger_vs_ONTB_data_for_plotting.xlsx", sheet = 3)

taxonomic_recovery$Sequencing_method <- factor(taxonomic_recovery$Sequencing_method, 
                                               levels = c("ONT", "Sanger"))

taxonomic_recovery$Taxonomic_recovery <- factor(taxonomic_recovery$Taxonomic_recovery, 
                                                levels = c("Contamination", "Target manual consensus", "Target"))

# Plotting
p3 <- ggplot(taxonomic_recovery, aes(x = Sequencing_method, y = Count, fill = Taxonomic_recovery)) +
  geom_bar(stat = "identity", color = "black", width = 0.9, linewidth = 1.5, aes(group = Taxonomic_recovery)) +
  theme_classic() +
  labs(x = "", y = "", title = "") +
  ggtitle("") +
  scale_fill_manual(values = c("#FF654B", "#737373", "#D9D9D9"), labels = c(" Contamination ", " Target (mapped) ", " Target ")) +
  #  scale_fill_manual(values = c("#3C9AB2", "#EBCC2A", "#F22300"), labels = c("Target", "Target taxon (manual consensus)", "Contamination")) +
  theme(text = element_text(size = 60),
        axis.text.y = element_text(hjust = 0.5),
        legend.position = "top",  # Adjust legend position within the plot area
        legend.margin = margin(20, 0, 0, 0), 
        legend.text = element_text(size = 40),  # Adjust text size in legend
        legend.title = element_text(size = 1)) + # Adjust size of legend key
  guides(fill = guide_legend(title = NULL, nrow = 1, ncol = 3)) +  # Adjust legend to stack vertically
  coord_flip() +
  scale_x_discrete(expand = expansion(add = c(1, 0)), limits = c("ONT", "Sanger")) + # Add space before the first bar
  scale_y_continuous(limits = c(0, 70)) +  # Use scale_y_continuous instead of ylim
  annotate("text", x = 3, y = 0, label = "(c)", size = 15, hjust = 0, vjust = 1) +  # Adjusted y position
  annotate("text", x = 3.2, y = 0, label = "", size = 15, hjust = 0, vjust = 1) +  # Adjusted y position
  geom_text(data = subset(taxonomic_recovery, Count > 0),  # Filter out zero counts
            aes(y = Count, label = Count), 
            position = position_stack(vjust = 0.5), size = 15, color = "black") 

#####
p3
#####

ggsave("plots/Sanger_vs_ONT/Recovery.pdf", plot = p1, device = "pdf", width = 32, height = 8)
ggsave("plots/Sanger_vs_ONT/Ns.pdf", plot = p2, device = "pdf", width = 32, height = 8)
ggsave("plots/Sanger_vs_ONT/Contam.pdf", plot = p3, device = "pdf", width = 32, height = 8)


############################ Reference Library #################################

########### PCR success ########################################################

PCR_success_long <- read_xlsx("plots/plot_data/PCR_success_data.xlsx")

# Get percentage success

success_percentage <- PCR_success_long %>%
  group_by(Primer) %>%
  dplyr::summarise(
    TotalCount = sum(Count, na.rm = TRUE),
    SuccessCount = sum(Count[PCR_success == "Success"], na.rm = TRUE),
    Percentage = round((SuccessCount / TotalCount) * 100),  # Round to nearest whole number
    .groups = 'drop'  # Ungroup after summarising
  )

# Set the levels of the Primer factor in the desired order
PCR_success_long$Primer <- factor(PCR_success_long$Primer, 
                                  levels = c("polyLCO/polyHCO", "echLCO/HCO", "jgLCO/jgHCO", "LCO/HCO", "Uni18S/Uni18R"))


p4 <- ggplot(PCR_success_long, aes(x = Count, y = Primer, fill = PCR_success)) +
  geom_bar(stat = "identity", color = "black", aes(group = PCR_success), width = 0.45, linewidth = 1.5) +
  theme_classic() +
  labs(x = "Number of OTUs", y = "Primer Pair", title = "") +
  
  # Manual fill colors for PCR success categories
  scale_fill_manual(values = c("#FF654B", "#D9D9D9"),
                    labels = c(" Failed PCRs ", " Successful PCRs ")) +
  
  # Customize y-axis labels to match the primers
  scale_y_discrete(labels = c(
    "polyLCO_polyHCO_band" = "polyLCO/polyHCO",
    "echLCO_HCO_band" = "echLCO/HCO",
    "jgLCO_jgHCO_band" = "jgLCO/jgHCO",
    "LCO_HCO_band" = "LCO/HCO",
    "Uni18S_Uni18R" = "Uni18S/Uni18R")) +
  
  # Adjusting overall theme elements
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
    legend.key.width = unit(1.5, "cm")
  ) +
  
  # Define x-axis limits
  coord_cartesian(xlim = c(0, 155)) +
  
  # Remove legend title
  guides(fill = guide_legend(title = NULL)) +
  
  # Add the percentage labels at the end of the stacked bars
  geom_text(data = success_percentage, aes(x = TotalCount + 1, y = Primer, 
                                           label = paste0(round(Percentage, 1), "%")),
            size = 16, hjust = -0.1, color = "black", inherit.aes = FALSE)  # hjust to slightly nudge the labels to the right of the bar

#####
p4
####
ggsave("plots/PCR_success_by_OTU.pdf", plot = p4, width = 30, height = 15, bg = "white")

########### Flongle recovery success ############################################

Barcode_recovery_by_marker_long <- read_xlsx("plots/plot_data/Flongle_recovery_success.xlsx")

# Step 1: Summarize the data for recovery counts
recovered_data <- Barcode_recovery_by_marker_long %>%
  filter(Recovery_Status == "Specimens_recovered") %>%
  group_by(RUN, Marker) %>%
  dplyr::summarise(Recovered_Count = sum(Specimens)) %>%
  ungroup()

# Step 2: Calculate the total number of specimens per Marker and RUN
total_data <- Barcode_recovery_by_marker_long %>%
  group_by(RUN, Marker) %>%
  dplyr::summarise(TotalCount = sum(Specimens)) %>%
  ungroup()

# Step 3: Merge recovery counts with total counts
recovery_percentage <- recovered_data %>%
  left_join(total_data, by = c("RUN", "Marker")) %>%
  dplyr::mutate(Percentage_Recovered = round((Recovered_Count / TotalCount) * 100))

# Step 4: Prepare data for total specimen count labels (y_values)
y_values <- Barcode_recovery_by_marker_long %>%
  group_by(RUN) %>%
  dplyr::summarise(Total_Specimens = sum(Specimens)) %>%
  ungroup()

# Reorder factor levels
Barcode_recovery_by_marker_long <- Barcode_recovery_by_marker_long %>%
  mutate(Recovery_Status = factor(Recovery_Status, levels = c("Specimens_unrecovered", "Specimens_recovered")))

levels(Barcode_recovery_by_marker_long$Recovery_Status)

# Step 6: Update the plot to include percentage of sequences recovered and total specimen counts
p5 <- ggplot(Barcode_recovery_by_marker_long, aes(x = Specimens, y = Marker, fill = Recovery_Status)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, color = "black", linewidth = 1.5) +
  facet_grid(RUN ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(
    values = c("Specimens_recovered" = "#D9D9D9", "Specimens_unrecovered" = "#FF654B"),
    labels = c("Specimens_recovered" = " Recovered ", "Specimens_unrecovered" = " Unrecovered ")
  ) +
  labs(
    x = "Number of Consensus Sequences",
    y = "Marker",
    fill = "",
    title = ""
  ) +
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
    panel.spacing.y = unit(1, "lines"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.background = element_rect(color = "black", fill = "#737373", linewidth = 1),
    strip.text = element_text(color = "white", size = 35),  # Set facet label text to white
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  
  # Display total specimen counts with a higher y-coordinate (adjust to move it higher)
  geom_text(data = y_values, aes(x = 141, y = Inf, label = paste("n =", Total_Specimens)),
            hjust = 1, vjust = 1.5, size = 14, color = "black", inherit.aes = FALSE) +
  
  # Display percentage of recovered sequences at the end of each bar
  geom_text(data = recovery_percentage, aes(x = TotalCount + 1, y = Marker, label = paste0(Percentage_Recovered, "%")),
            size = 16, hjust = -0.1, color = "black", inherit.aes = FALSE) +
  
  guides(fill = guide_legend(title = NULL)) +
  coord_cartesian(xlim = c(0, 135))

#####
p5
#####
ggsave("plots/Flongle_recovery_per_marker.pdf", plot = p5, width = 30, height = 20, bg = "white")

########### Demultiplexed sequences and Mapping ################################

# 04.12.24 updated for ONTB2!

# ONTB1 All_ONTB_raw <- read_excel("C:/Users/r01er21/OneDrive - University of Aberdeen/Chapter 1 Invert ID results/Chapter_1_rerun_post_16.05.24/Chapter_1_rerun_summary_stats_and_plotting/Data_for_final_plots/N_before_and_after_mapping.xlsx", sheet = 1)
# ONTB2
All_ONTB_raw <- read_excel("plots/plot_data/N_before_and_after_mapping_ONTB2.xlsx", sheet = 1)

# ONTB1 All_ONTB_mapped <- read_excel("C:/Users/r01er21/OneDrive - University of Aberdeen/Chapter 1 Invert ID results/Chapter_1_rerun_post_16.05.24/Chapter_1_rerun_summary_stats_and_plotting/Data_for_final_plots/N_before_and_after_mapping.xlsx", sheet = 2)
# ONTB2
All_ONTB_mapped <- read_excel("plots/plot_data/N_before_and_after_mapping_ONTB2.xlsx", sheet = 2)


# Define y_limits for the histogram
y_limits <- c(0, 60)  # Adjust as necessary based on your data

# Define x-axis limits for both plots
x_limits <- c(3, 30000)  # Consistent x-axis limits for both plots


### Pre Scatter plot #####

p6a <- ggplot(All_ONTB_raw, aes(x = Number_of_sequences_demultiplexed, y = Conut_non_AGCT)) +
  # Points for COI and 18S markers
  geom_point(aes(color = Marker, shape = Marker), size = 10) +
  # Points for contaminated samples
  geom_point(data = subset(All_ONTB_raw, Contamination == "Y"),
             aes(color = "Contaminated", shape = "Contaminated"),  # Label for contamination
             size = 13, stroke = 2) +
  # Labels and theme settings
  labs(title = "",
       x = "",
       y = "Number of Ambiguous Bases") +
  theme_classic() + 
  theme(axis.text = element_text(size = 45),
        axis.title.y = element_text(size = 45),
        axis.title.x = element_text(size = 45),
        plot.title = element_text(size = 50, hjust = 0.5),
        legend.text = element_text(size = 45),
        legend.title = element_text(size = 45),
        legend.position = "top",  # Position legend at the top
        legend.box = "horizontal") +  # Arrange legend items horizontally
  # Smooth line
  #  geom_smooth(aes(group = 1), method = "glm", formula = y ~ x, 
  #              method.args = list(family = quasipoisson), color = "#7C7C7C", linewidth = 1, 
  #              alpha = 0.4, fill = alpha("#D9D9D9", 0.2)) +
  # Horizontal lines
  geom_hline(yintercept = 6.58, color = "#EBCC2A", alpha = 0.5, linetype = "solid", linewidth = 3) +
  geom_hline(yintercept = 4.59, color = "#3C9AB2", alpha = 0.5, linetype = "solid", linewidth = 3) +
  # Manual scales for color and shape
  scale_color_manual(values = c("COI" = "#EBCC2A", "18S" = "#3C9AB2", "Contaminated" = "#FF654B"), 
                     labels = c("COI" = " COI ", "18S" = " 18S ", "Contaminated" = " Contaminated "), 
                     name = NULL) +  # Remove legend title
  scale_shape_manual(values = c("COI" = 16, "18S" = 16, "Contaminated" = 21), 
                     labels = c("COI" = " COI ", "18S" = " 18S ", "Contaminated" = " Contaminated "),
                     name = NULL) +  # Remove legend title
  # Log scale for x and sqrt scale for y
  scale_x_log10(limits = x_limits, breaks = c(10, 100, 1000, 10000), labels = scales::comma) +
  scale_y_sqrt(limits = c(0, 200)) +
  # Adjust legend appearance
  guides(shape = guide_legend(override.aes = list(size = 7)),  # Adjust size of legend shapes
         color = guide_legend(override.aes = list(size = 11))) +  # Adjust size of legend colors
  annotate("text", x = 3, y = 195, label = "(a)", size = 15, hjust = 0, vjust = 1)

# Create the title as a ggdraw object
title_label_p6 <- ggdraw() +
  draw_label("Before Mapping", size = 60, hjust = 0.5) +
  theme(plot.background = element_rect(fill = "#E8E8E8", colour = "#929292", linewidth = 1),  # Grey box with outline
        plot.margin = margin(10, 10, 10, 10))

# Combine title and plot
p6a_full <- plot_grid(title_label_p6, p6a, ncol = 1, rel_heights = c(0.1, 1), align = "v", axis = "lr")

p6a_full

### Pre Boxplot ####
marker_colors <- c("COI" = "#EBCC2A", "18S" = "#3C9AB2")

# Create the boxplot with consistent x-axis limits
p6b <- ggplot(data = All_ONTB_raw, aes(x = Number_of_sequences_demultiplexed, y = Marker, fill = Marker)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +  # Exclude outliers for cleaner appearance
  theme_classic() +
  theme(axis.ticks = element_line(),
        legend.position = "none",  # Hide legend for boxplot
        axis.text = element_text(size = 45), 
        axis.title.y = element_text(size = 45, margin = margin(r = 0)),
        axis.title.x = element_text(size = 45)) +
  scale_x_log10(limits = x_limits, breaks = c(10, 100, 1000, 10000), labels = scales::comma) +  # Log scale for x-axis
  scale_y_discrete(labels = c("COI" = "COI", "18S" = "18S")) +
  coord_cartesian(xlim = x_limits) +  # Ensure x-axis limits are consistent
  labs(title = "", x = "", y = "Marker") +  # Add y-axis label
  scale_fill_manual(values = marker_colors) +
  annotate("text", x = 3, y = 2.4, label = "(b)", size = 15, hjust = 0, vjust = 1)  # Adjusted y position

p6b


### Pre Histogram ####

p6c <- ggplot(data = All_ONTB_raw, aes(x = Number_of_sequences_demultiplexed, fill = Marker)) +
  geom_histogram(aes(y = after_stat(count)), bins = 30, position = "stack", color = "black", show.legend = FALSE) +
  theme_classic() +
  theme(axis.ticks = element_line(),
        legend.position = "top",
        legend.box = "horizontal",
        legend.text = element_text(size = 45),
        legend.title = element_text(size = 45)) +
  scale_x_log10(limits = x_limits, breaks = c(10, 100, 1000, 10000), labels = scales::comma) +
  coord_cartesian(ylim = y_limits) +  # Set y-axis limits
  labs(title = "", x = "Number of Reads used to \nProduce Consensus Sequences", y = "Number of Consensus Sequences") +  # Add y-axis label
  theme(axis.text = element_text(size = 45), 
        axis.title.y = element_text(size = 45, margin = margin(r = 15)),
        axis.title.x = element_text(size = 45)) +
  scale_fill_manual(values = c("COI" = "#EBCC2A", "18S" = "#3C9AB2")) +
  scale_y_continuous(labels = c(" 0", " 20", " 40", " 60")) +
  annotate("text", x = 3, y = 59, label = "(c)", size = 15, hjust = 0, vjust = 1)

p6c

### Post Scatter plot ####
p6d <- ggplot(All_ONTB_mapped, aes(x = Number_of_sequences_demultiplexed, y = Count_non_AGCT)) +
  # Points for COI and 18S markers
  geom_point(aes(color = Marker, shape = Marker), size = 10) +
  # Points for contaminated samples
  geom_point(data = subset(All_ONTB_mapped, Contamination == "Y"),
             aes(color = "Contaminated", shape = "Contaminated"),  # Label for contamination
             size = 13, stroke = 2) +
  # Labels and theme settings
  labs(title = "",
       x = "",
       y = "") +  # Updated y-axis label
  theme_classic() + 
  theme(axis.text = element_text(size = 45),
        axis.title.y = element_text(size = 45),
        axis.title.x = element_text(size = 45),
        plot.title = element_text(size = 50, hjust = 0.5),
        legend.text = element_text(size = 45),
        legend.title = element_text(size = 45),
        legend.position = "top",  # Position legend at the top
        legend.box = "horizontal") +  # Arrange legend items horizontally
  # Smooth line
  #  geom_smooth(aes(group = 1), method = "glm", formula = y ~ x, 
  #              method.args = list(family = quasipoisson), color = "#7C7C7C", linewidth = 1, 
  #              alpha = 0.4, fill = alpha("#D9D9D9", 0.2)) +
  # Horizontal lines
  geom_hline(yintercept = 6.58, color = "#EBCC2A", alpha = 0.5, linetype = "solid", linewidth = 3) +
  geom_hline(yintercept = 4.59, color = "#3C9AB2", alpha = 0.5, linetype = "solid", linewidth = 3) +
  # Manual scales for color and shape
  scale_color_manual(values = c("COI" = "#EBCC2A", "18S" = "#3C9AB2", "Contaminated" = "#FF654B"), 
                     labels = c("COI" = " COI ", "18S" = " 18S ", "Contaminated" = " Contaminated "), 
                     name = NULL) +  # Remove legend title
  scale_shape_manual(values = c("COI" = 16, "18S" = 16, "Contaminated" = 21), 
                     labels = c("COI" = " COI ", "18S" = " 18S ", "Contaminated" = " Contaminated "),
                     name = NULL) +  # Remove legend title
  # Log scale for x and sqrt scale for y
  scale_x_log10(limits = x_limits, breaks = c(10, 100, 1000, 10000), labels = scales::comma) +
  scale_y_sqrt(limits = c(0, 200)) +
  # Adjust legend appearance
  guides(shape = guide_legend(override.aes = list(size = 7)),  # Adjust size of legend shapes
         color = guide_legend(override.aes = list(size = 11))) +  # Adjust size of legend colors
  annotate("text", x = 3, y = 195, label = "(a)", size = 15, hjust = 0, vjust = 1)

# Create the title as a ggdraw object
title_label_p6d <- ggdraw() +
  draw_label("After Mapping", size = 60, hjust = 0.5) +
  theme(plot.background = element_rect(fill = "#E8E8E8", colour = "#929292", size = 1),  # Grey box with outline
        plot.margin = margin(10, 10, 10, 10))

# Combine title and plot
p6d_full <- plot_grid(title_label_p6d, p6d, ncol = 1, rel_heights = c(0.1, 1), align = "v", axis = "lr")

p6d_full


########################### SIDE QUEST #########################################

# How many reads do we need to get zero Ns?
# It's hard to model becasue it's so zero inflated
# Instead lets ask what is the highest value of Number_of_sequences_demultiplexed
# which produces a barcode with more than 1% N

# Filter rows where Perc_non_AGCT > 1
filtered_data <- All_ONTB_mapped %>% 
  filter(Perc_non_AGCT > 1) %>%
  filter(Contamination == "N")
  
#The maximum Number_of_sequences_demultiplexed  needed to produce an
# sequence with greater than 1% Ns was 32
max_sequences <- filtered_data %>% 
  summarise(max_sequences = max(Number_of_sequences_demultiplexed, na.rm = TRUE))

#The minimun Number_of_sequences_demultiplexed  needed to produce an
# sequence with greater than 1% Ns was 6
min_sequences <- filtered_data %>% 
  summarise(min_sequences = min(Number_of_sequences_demultiplexed, na.rm = TRUE))

print(max_sequences)
print(min_sequences)

filtered_data <- All_ONTB_mapped %>% 
  filter(translation_check == "0") %>%
  filter(Contamination == "N")

# The maximum Number_of_sequences_demultiplexed  needed to produce an
# untranslatable sequence was 24 reads
max_sequences <- filtered_data %>% 
  summarise(max_sequences = max(Number_of_sequences_demultiplexed, na.rm = TRUE))

# The minimun Number_of_sequences_demultiplexed  needed to produce an
# untranslatable sequence was 6 reads
min_sequences <- filtered_data %>% 
  summarise(min_sequences = min(Number_of_sequences_demultiplexed, na.rm = TRUE))

print(max_sequences)
print(min_sequences)

# What was the maximum number of 18S reads used to produce a sequence with 
# ambiguous bases?

filtered_data <- All_ONTB_mapped %>% 
  filter(Contamination == "N") %>%
  filter(Marker == "18S") %>%
  filter(Count_non_AGCT != "0")

# The max number of 18S reads which still resulted in Ns is 3214
max_sequences <- filtered_data %>% 
  summarise(max_sequences = max(Number_of_sequences_demultiplexed, na.rm = TRUE))

print(max_sequences)

# Find the row with the maximum Number_of_sequences_demultiplexed 
# B006_11_MB Ampharetidae
max_row <- filtered_data %>%
  filter(Number_of_sequences_demultiplexed == max(Number_of_sequences_demultiplexed, na.rm = TRUE)) %>%
  select(SpecimenID, Run, Number_of_sequences_demultiplexed)

print(max_row)

###### Weird 18S data!
# Filter rows where Number_of_sequences_demultiplexed > 500
filtered_specimens <- filtered_data %>%
  filter(Number_of_sequences_demultiplexed > 500) %>%
  select(SpecimenID, Run, Number_of_sequences_demultiplexed)

print(filtered_specimens)
################################################################################

### Post Boxplot ####
p6e <- ggplot(data = All_ONTB_mapped, aes(x = Number_of_sequences_demultiplexed, y = Marker, fill = Marker)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +  # Exclude outliers for cleaner appearance
  theme_classic() +
  theme(axis.ticks = element_line(),
        legend.position = "none",  # Hide legend for boxplot
        axis.text = element_text(size = 45), 
        axis.title.y = element_text(size = 45, margin = margin(r = 0)),
        axis.title.x = element_text(size = 45)) +
  scale_x_log10(limits = x_limits, breaks = c(10, 100, 1000, 10000), labels = scales::comma) +  # Log scale for x-axis
  scale_y_discrete(labels = c("COI" = "COI", "18S" = "18S")) +
  coord_cartesian(xlim = x_limits) +  # Ensure x-axis limits are consistent
  labs(title = "", x = "", y = "") +  # Add y-axis label
  scale_fill_manual(values = marker_colors) +
  annotate("text", x = 3, y = 2.4, label = "(e)", size = 15, hjust = 0, vjust = 1)  # Adjusted y position

p6e

### Post Histogram ####
p6f <- ggplot(data = All_ONTB_mapped, aes(x = Number_of_sequences_demultiplexed, fill = Marker)) +
  geom_histogram(aes(y = after_stat(count)), bins = 30, position = "stack", color = "black", show.legend = FALSE) +
  theme_classic() +
  theme(axis.ticks = element_line(),
        legend.position = "top",
        legend.box = "horizontal",
        legend.text = element_text(size = 45),
        legend.title = element_text(size = 45)) +
  scale_x_log10(limits = x_limits, breaks = c(10, 100, 1000, 10000), labels = scales::comma) +
  coord_cartesian(ylim = y_limits) +  # Set y-axis limits
  labs(title = "", x = "Number of Reads used to \nProduce Consensus Sequences", y = "") +  # Add y-axis label
  theme(axis.text = element_text(size = 45), 
        axis.title.y = element_text(size = 45, margin = margin(r = 15)),
        axis.title.x = element_text(size = 45)) +
  scale_fill_manual(values = c("COI" = "#EBCC2A", "18S" = "#3C9AB2")) +
  scale_y_continuous(labels = c(" 0", " 20", " 40", " 60")) +
  annotate("text", x = 3, y = 59, label = "(f)", size = 15, hjust = 0, vjust = 1)

p6f


#####
p6a
p6b
p6c
p6d
p6e
p6f

p6_combined <- grid.arrange(
  p6a, p6d, p6b, p6e, p6c, p6f, 
  nrow = 3, 
  ncol = 2,
  widths = c(1, 1),  # Set equal widths for both columns
  heights = c(1, 0.5, 0.75)  # Set equal heights for both rows if needed
)

ggsave("plots/Ns_vs_dem_reads_ONTB2.pdf", plot = p6_combined, width = 30, height = 30, bg = "white")

##### p6.5
########### Input concentration vs Reads #####

All_ONTB_sequences_17.07_sub <- read_xlsx("plots/plot_data/Input_conc_vs_Reads.xlsx")

m8 <- lm(data = All_ONTB_sequences_17.07_sub, log(Number_of_sequences_demultiplexed) ~ log(Input_concentration))
par(mfrow = c(2,2))
plot(m8)
summary(m8)
anova(m8)
print(m6_aic_model <- AIC(m8))


unique_shapes <- c(16, 18, 15, 17, 16, 18, 15, 17)  # Add more shapes as needed

# Extract the R-squared value
r_squared <- summary(m8)$r.squared

# Create predictions using the model
All_ONTB_sequences_17.07_sub <- All_ONTB_sequences_17.07_sub %>%
  mutate(predicted = exp(predict(m8)))  # Use exp to get predictions on the original scale

# Update the plot to include the fitted line for the whole dataset
p6.5 <- ggplot(All_ONTB_sequences_17.07_sub, 
             aes(x = library_input_ng, y = Number_of_sequences_demultiplexed, 
                 color = Species)) +
  geom_point(aes(shape = Species), size = 15, alpha = 0.7) +  # Keep shape for points
  labs(title = "", x = "Input DNA (ng)", y = "Number of Sequences Demultiplexed",
       color = "OTU", shape = "OTU") +
  theme_classic() + 
  theme(,
        legend.text = element_text(size = 40, face="italic"),
        axis.title.x = element_text(size = 45),
        axis.title.y = element_text(size = 45),
        axis.text.x = element_text(size = 45),
        axis.text.y = element_text(size = 45),
        legend.title = element_text(size = 45),
        legend.spacing.y = unit(1, 'cm'),
        legend.key.height = unit(2, 'cm')) +
  scale_y_log10() + 
  scale_x_log10() +
  geom_line(aes(y = predicted), color = "black", linewidth = 1) +  # Single smooth line for the entire dataset
  scale_shape_manual(values = unique_shapes) +
  scale_color_manual(values = c(
    "Steromphala cineraria" = "#A2A2A2",
    "Asterias rubens" = "#30788C",
    "Tubificoides benedii" = "#A2A2A2",
    "Clymenura clypeata" = "#30788C",
    "Macropodia rostrata" = "#FF654B",
    "Facelina bostoniensis" = "#FF654B",
    "Praunus neglectus" = "#EBCC2A",
    "Peringia ulvae" = "#EBCC2A")
  ) +
  annotate("text", x = Inf, y = Inf, 
           label = paste("R² =", round(r_squared, 3)), 
           hjust = 2.1, vjust = 1.5, 
           size = 15, color = "black")

p6.5

ggsave("plots/Input_DNA_vs_Reads.pdf", plot = p6.5, width = 30, height = 15, bg = "white")




#####
########### Marker Recovery by Phylum ###########################################

overlap_counts_by_phylum <- read_xlsx("plots/plot_data/Marker_recovery_by_phylum.xlsx")

# Define factor levels for phylum and overlap
overlap_counts_by_phylum$phylum <- factor(overlap_counts_by_phylum$phylum, 
                                          levels = c("Porifera", "Cnidaria", "Nematoda",
                                                     "Arthropoda", "Bryozoa", "Platyhelminthes",
                                                     "Nemertea", "Annelida", "Mollusca",
                                                     "Chordata", "Hemichordata", "Echinodermata"))

overlap_counts_by_phylum$overlap <- factor(overlap_counts_by_phylum$overlap, 
                                           levels = c("Neither", "18S_only", 
                                                      "COI_only", "Both"))

p7 <- ggplot(overlap_counts_by_phylum, aes(x = Count, y = phylum, fill = overlap)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", width = 0.9, linewidth = 1.5) +
  labs(x = "Number of OTUs", y = "Phylum", fill = "") +
  ggtitle("") +
  scale_fill_manual(values = c("#FF654B", "#3C9AB2", "#EBCC2A", "#D9D9D9"), 
                    labels = c(" Neither ", " 18S only ", " COI only ", " Both ")) +
  theme_classic() + 
  theme(
    text = element_text(size = 18),
    axis.title.x = element_text(size = 45),
    axis.title.y = element_text(size = 45, hjust = 0.5),
    axis.text.x = element_text(size = 45),
    axis.text.y = element_text(size = 45),
    
    # Add border around the plot
#    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),  # Add a black border
    
    # Adjust legend size and position
    legend.position = "top",  # Position legend at the top
    legend.justification = c("center", "top"),
    legend.text = element_text(size = 45),  # Larger text in legend
    legend.key.size = unit(3, "lines"),  # Larger legend keys for more spacing
    legend.key.height = unit(1.5, "cm"),  # Adjust key height
    legend.key.width = unit(1.5, "cm"),  # Adjust key width
    legend.title = element_text(size = 45, hjust = 0.5),  # Center the legend title horizontally
    legend.box = "vertical"  # Stack the legend title above the keys
  ) +
  geom_text(aes(y = phylum, label = Count), 
            position = position_stack(vjust = 0.5), size = 12, color = "black") +  # Add counts as text labels
  coord_cartesian(xlim = c(0, 50))

#####
p7
#####
ggsave("plots/Taxonomic_recovery_by_phyum.pdf", plot = p7, width = 30, height = 20, bg = "white")

########## Ranks by method bar plot ############################################

ranks_by_method <- read_xlsx("plots/plot_data/Ranks_by_method.xlsx")

str(ranks_by_method)

ranks_by_method$Rank <- factor(ranks_by_method$Rank, 
                               levels = c("Phylum",
                                          "Class",
                                          "Order",
                                          "Family",
                                          "Genus",
                                          "Species"))

ranks_by_method$Method <- factor(ranks_by_method$Method, 
                                           levels = c("Morph",
                                                      "18S",
                                                      "COI"))


p8 <- ggplot(ranks_by_method, aes(x = Count, y = Method, fill = Rank)) +
  geom_bar(stat = "identity", position = "stack", colour = "black", width = 0.7, linewidth = 1.5) +
  labs(x = "Number of OTUs", y = "Identificaion Method", fill = "") +
#  scale_fill_manual(values = c("#737373", "#D9D9De", "#FF654B","#3C9AB2", "#EBCC2A", "#169678"), 
  scale_fill_manual(values = c("#737373", "#3C9AB2", "#38C682","#FF654B", "#EBCC2A", "#D9D9De"),
                    labels = c(" Phylum ", " Class ", " Order ", " Family ", " Genus ", " Species ")) +
  theme_classic() + 
  theme(
    text = element_text(size = 18),
    axis.title.x = element_text(size = 45),
    axis.title.y = element_text(size = 45, hjust = 0.5),
    axis.text.x = element_text(size = 45),
    axis.text.y = element_text(size = 45),
    
    # Add border around the plot
#    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),  # Add a black border
    
    # Adjust legend size and position
    legend.position = "top",  # Position legend at the top
    legend.justification = c("center", "top"),
    legend.text = element_text(size = 45),  # Larger text in legend
    legend.key.size = unit(3, "lines"),  # Larger legend keys for more spacing
    legend.key.height = unit(1.5, "cm"),  # Adjust key height
    legend.key.width = unit(1.5, "cm"),  # Adjust key width
    legend.title = element_text(size = 45, hjust = 0.5),  # Center the legend title horizontally
    legend.box = "vertical"  # Stack the legend title above the keys
  ) +
  guides(fill = guide_legend(reverse = TRUE)) + # reverses the order of the legend
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(
    data = subset(ranks_by_method, Count > 1),  # Exclude rows where Count is 0
    aes(label = Count), 
    position = position_stack(vjust = 0.5), 
    size = 15, 
    color = "black") +
  annotate(
    "text", 
    x = 132.5,               # The Count value (x-axis position)
    y = "COI",           # The Method value (y-axis position, change if different)
    label = "1",         # The text to display
    hjust = -0.5,        # Adjust text position horizontally
    size = 15,           # Text size
    color = "black"
  )

p8

ggsave("plots/Ranks_by_method.pdf", plot = p8, width = 30, height = 12.5, bg = "white")
ggsave("plots_PDF/Figure5.pdf", plot = p8, width = 30, height = 12.5, bg = "white")

############################ Pores vs reads ####################################

pores_vs_reads <- read_xlsx("plots/plot_data/Active_pores_vs_read_recovery.xlsx")
names(pores_vs_reads)

m1 <- lm(Sequence_Count ~ Pores, data = pores_vs_reads)
m1
par(mfrow = c(2, 2))
plot(m1)

summary(m1)
anova(m1)

m2 <- lm(Sequence_Count ~ Pores + Run_Time, data = pores_vs_reads)
m2
par(mfrow = c(2, 2))
plot(m2)

summary(m2)

m3 <- lm(Sequence_Count ~ Pores*Run_Time, data = pores_vs_reads)
m3
par(mfrow = c(2, 2))
plot(m3)

summary(m3)

library(lme4)
library(lmerTest)



mem1 <- lmer(Sequence_Count ~ Pores + (1|Run_Time), data = pores_vs_reads)

# Extract residuals
residuals <- resid(mem1)
# Create QQ plot
qqnorm(residuals)
qqline(residuals)
plot(fitted(mem1), resid(mem1))

m1 <- lm(Sequence_Count ~ Pores, data = pores_vs_reads)
m1
par(mfrow = c(2, 2))
plot(m1)

summary(m1)




