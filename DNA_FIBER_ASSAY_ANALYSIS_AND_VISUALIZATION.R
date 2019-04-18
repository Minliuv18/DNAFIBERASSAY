####DNA Fiber Assay Analysis####

# Created a new file with only id, ratio and group(treatment)
  # Named "data1_30_march.xlsx", save as "data1_30_march.csv"

#Packages may be used#####
library(RColorBrewer) # Colourful
library(ggplot2) # Draw a plot
library(ggsignif) # Could be signed the significant on the plot

##Set work diection and attach the data####
setwd("PATH/TO/DATA/")
# Set the work direction, aka where the file is.
d <- read.csv("NAME_OF_DATAFILE.csv", header = TRUE, sep = ",", dec = ".")
# Create a variable called "d", is the data.frame from the data
attach(d)
# attach the various in data
  # so "id" is the number of fibers, 
  # "ratio" is the ratio of "green/red", 
  # "group" is different treatment.

##Prepare the order of group in plot#####
levels(group)
# Check the order of groups (will show later on x axis)
group_in_order <- factor(d$group, levels = c("NAME_OF_UNTREATMENT","NAME_OF_TREAT1", "NAME_OF_TREAT2","..."))
# If the order of groups is not same like we want,
  # Run code above

###### Output Plot######
png(filename = "NAME_OF_FIGURE.png", units = "in", 
    width = 12, height = 10, res = 300)
# If only wanna saw the plot run below, if wanna output the png figure, REMEMBER RUN ABOVE

ggplot(d, aes(x = group_in_order, y = ratio, fill = group_in_order)) + 
  scale_fill_brewer(palette = "Dark2") + # could be palette or scale_fill-grey() or fill_manual
  geom_violin(trim = TRUE) + # trim the tail or not, default is TRUE
  geom_boxplot(width = 0.1, fill = "white") + # add a boxplot in violin plot, otherwise could add dots in violinplot
  #OR
  ## geom_jitter(shape = 16, position = position_jitter(0.2)) + 
  labs(title = "HEADING", 
       x = "NAME_OF_X_AXIS", 
       y = "NAME_OF_Y_AXIS", 
       caption = "CAPTION",
       fill = "TITLE_OF_LEGEND") + # main, xlab, ylab and title of legend
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(hjust = 0.5, size = 15),
        axis.title.y = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right", 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.box.margin = margin(0.8,0.8,0.8,0.8,"cm"))  + 
  geom_signif(comparisons = list(c("NAME_OF_TREAT1","NAME_OF_TREAT2")),
              map_signif_level = TRUE, test = "t.test",
              y_position = 3.25, tip_length = 0.01, vjust = 0.2,
              annotations = "Annotations of t.test result") + # Significant between HU and IGF1+HU
  geom_signif(comparisons = list(c("NAME_OF_UNTREATMENT","NAME_OF_TREAT1"),c("NAME_OF_TREAT2","NAME_OF_TREAT3"),
                                 c("NAME_OF_TREAT4","NAME_OF_TREAT5"), c("NAME_OF_TREAT6", "NAME_OF_TREAT7")),
              map_signif_level = TRUE, test = "t.test",
              y_position = 3, tip_length = 0.01, vjust = 0.2) + 
  geom_signif(comparisons = list(c("NAME_OF_TREAT1", "NAME_OF_TREAT2")),
              map_signif_level = TRUE, test = "t.test",
              y_position = 3.15, tip_length = 0.01, vjust = 0.2)# Significant between every two groups

# If wanna finish outputing, run dev.off() to close the procession of output. If you are not run png(), ignore it.
dev.off()

####For the accurate number of t.test######
t.test( ratio[group == "NAME_OF_TREAT1"],ratio[group == "NAME_OF_TREAT2"], alternative = "l")$p.value # Single tail, l of less, g for great
t.test(ratio[group == "NAME_OF_TREAT1"], ratio[group == "NAME_OF_TREAT2"])$p.value # Double tail.




