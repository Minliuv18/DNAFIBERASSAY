####DNA Fiber Assay Analysis####

# Created a new file with only id, ratio and group(treatment)
  # Named "data1_30_march.xlsx", save as "data1_30_march.csv"

#Packages may be used#####
library(RColorBrewer) # Colourful
library(ggplot2) # Draw a plot
library(ggsignif) # Could be signed the significant on the plot

##Set work diection and attach the data####
setwd("path/to/files/")
# Set the work direction, aka where the file is.
d <- read.csv("name_of_file.csv", header = TRUE, sep = ",", dec = ".")
# Create a variable called "d", is the data.frame from the data
attach(d)
# attach the various in data
  # so "id" is the number of fibers, 
  # "ratio" is the ratio of "green/red", 
  # "group" is different treatment.

##Prepare the order of group in plot#####
levels(group)
# Check the order of groups (will show later on x axis)
group_in_order <- factor(d$group, levels = c("UNT","0.2mM HU", "1uM NVP", "NVP + HU", 
                                             "100ng/ml IGF1","IGF1 + HU", "IGF1 + NVP", 
                                             "IGF1 + NVP + HU"))
# If the order of groups is not same like we want,
  # Run code above

###### Output Plot######
png(filename = "NAME_of_Figure.png", units = "in", 
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
       fill = "Treatment") + # main, xlab, ylab and title of legend
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(hjust = 0.5, size = 15),
        axis.title.y = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right", 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.box.margin = margin(0.8,0.8,0.8,0.8,"cm"))  + 
  geom_signif(comparisons = list(c("0.2mM HU","IGF1 + HU")),
              map_signif_level = TRUE, test = "t.test",
              y_position = 3.25, tip_length = 0.01, vjust = 0.2,
              annotations = "Annotations of t.test result") + # Significant between HU and IGF1+HU
  geom_signif(comparisons = list(c("UNT","0.2mM HU"),c("1uM NVP","NVP + HU"),
                                 c("100ng/ml IGF1","IGF1 + HU"), c("IGF1 + NVP", "IGF1 + NVP + HU")),
              map_signif_level = TRUE, test = "t.test",
              y_position = 3, tip_length = 0.01, vjust = 0.2) + 
  geom_signif(comparisons = list(c("IGF1 + HU", "IGF1 + NVP + HU")),
              map_signif_level = TRUE, test = "t.test",
              y_position = 3.15, tip_length = 0.01, vjust = 0.2)# Significant between every two groups

# If wanna finish outputing, run dev.off() to close the procession of output. If you are not run png(), ignore it.
dev.off()

####For the accurate number of t.test######
t.test( ratio[group == "NAME_OF_GROUP1"],ratio[group == "NAME_OF_GROUP2"], alternative = "l")$p.value # Single tail, l of less, g for great
t.test(ratio[group == "NAME_OF_GROUP1"], ratio[group == "NAME_OF_GROUP2"])$p.value # Double tail.




