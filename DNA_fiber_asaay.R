#Packages may be used#####
library(RColorBrewer) # Colourful
library(ggplot2) # Draw a plot
library(ggsignif) # Could be signed the significant on the plot
##Set work diection and attach the data####
setwd("/Users/minliuv18/Desktop/DNA_fiber_assay_results")
# Set the work direction, aka where the file is.
hela <- read.csv("20190327data_HeLa.csv", header = TRUE, sep = ",", dec = ".") # read the data of HeLa
rp <- read.csv("20190411data_R+.csv", header = TRUE, sep = ",", dec = ".") # read the data of R+ ps. rp stand for R plus
rm <- read.csv("20190415data_R-.csv", header = TRUE, sep = ",", dec = ".") # read the data of R- ps. rm stand for R minus
# Create a variable called "d", is the data.frame from the data

##Plot the boxplot in Basic####

# Check the order of groups (will show later on x axis)
group_hela <- factor(hela$group, levels = c("UNT","0.2mM HU", "1uM NVP", "NVP + HU", 
                                            "100ng/ml IGF1","IGF1 + HU", "IGF1 + NVP", 
                                            "IGF1 + NVP + HU"))
group_rp <- factor(rp$group, levels = c("UNT","0.2mM HU", "1uM NVP", "NVP + HU", 
                                        "100ng/ml IGF1","IGF1 + HU", "IGF1 + NVP", 
                                        "IGF1 + NVP + HU"))
group_rm <- factor(rm$group, levels = c("UNT","0.2mM HU", "1uM NVP", "NVP + HU", 
                                        "100ng/ml IGF1","IGF1 + HU", "IGF1 + NVP", 
                                        "IGF1 + NVP + HU"))
# If the order of groups is not same like we want,
# Run code above

##t.test####
t.test(hela$ratio[hela$group == "IGF1 + HU"], hela$ratio[hela$group == "0.2mM HU"], alternative = "two.sided")$p.value
t.test(rp$ratio[rp$group == "IGF1 + HU"], rp$ratio[rp$group == "0.2mM HU"], alternative = "two.sided")$p.value
t.test(rm$ratio[rm$group == "IGF1 + HU"], rm$ratio[rm$group == "0.2mM HU"], alternative = "two.sided")$p.value

##Plotting HeLa#####

png(filename = "HeLa_DNA_Fiber_Assay_2.png", units = "in", 
    width = 12, height = 12, res = 300)

ggplot(hela, aes(x = group_hela, y = hela$ratio, fill = group_hela)) + 
  scale_fill_brewer(palette = "Dark2") + # could be palette or scale_fill-grey() or fill_manual
  geom_violin(trim = TRUE) + # trim the tail or not, default is TRUE
  geom_boxplot(width = 0.1, fill = "grey") + # add a boxplot in violin plot, otherwise could add dots in violinplot
  #OR
  ## geom_jitter(shape = 16, position = position_jitter(0.2))
  labs(title = "Ratio by treatment (HeLa)", 
       y = "Ratio", 
       caption = "P < 0.001 of t-test between HU and IGF1 + HU",
       fill = "Treatment") + # main, xlab, ylab and title of legend
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.x  = element_text(size = 20,angle = 45, hjust = 1, face = "bold"),
        axis.text.y  = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        legend.position = "none", 
        legend.margin = margin(0, 0.2, 0.2, 0.2, "cm"),
        legend.box.margin = margin(0,0.8,0.8,0.8,"cm"),
        legend.title = element_text(size = 20),
        plot.caption = element_text(size = 22))  + 
  geom_signif(comparisons = list(c("0.2mM HU","IGF1 + HU")),
              map_signif_level = TRUE, textsize=16, test = "t.test",
              y_position = 3.25, tip_length = 0.01, vjust = 0.6) + # Significant between HU and IGF1+HU
  geom_signif(comparisons = list(c("UNT","0.2mM HU"), c("1uM NVP","NVP + HU"), 
                                 c("100ng/ml IGF1","IGF1 + HU"), c("IGF1 + NVP", "IGF1 + NVP + HU")),
              map_signif_level = TRUE, textsize=16, test = "t.test",
              y_position = 2.95, tip_length = 0.01, vjust = 0.6) + 
  geom_signif(comparisons = list(c("IGF1 + HU", "IGF1 + NVP + HU")),
              map_signif_level = TRUE, textsize=16, test = "t.test",
              y_position = 3.15, tip_length = 0.01, vjust = 0.6)# Significant between every two groups

dev.off()

##Plotting for R+#####


png(filename = "R+_DNA_Fiber_Assay.png", units = "in", 
    width = 12, height = 12, res = 300)

ggplot(rp, aes(x = group_rp, y = rp$ratio, fill = group_rp)) + 
  scale_fill_brewer(palette = "Dark2") + # could be palette or scale_fill-grey() or fill_manual
  geom_violin(trim = TRUE) + # trim the tail or not, default is TRUE
  geom_boxplot(width = 0.1, fill = "grey") + # add a boxplot in violin plot, otherwise could add dots in violinplot
  #OR
  ## geom_jitter(shape = 16, position = position_jitter(0.2))
  labs(title = "Ratio by treatment (R+)", 
       y = "Ratio", 
       caption = "P < 0.001 of t-test between HU and IGF1 + HU",
       fill = "Treatment") + # main, xlab, ylab and title of legend
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.x  = element_text(size = 20,angle = 45, hjust = 1, face = "bold"),
        axis.text.y  = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        legend.position = "none", 
        legend.margin = margin(0, 0.2, 0.2, 0.2, "cm"),
        legend.box.margin = margin(0,0.8,0.8,0.8,"cm"),
        legend.title = element_text(size = 20),
        plot.caption = element_text(size = 22))  + 
  geom_signif(comparisons = list(c("0.2mM HU","IGF1 + HU")),
              map_signif_level = TRUE, textsize=16, test = "t.test",
              y_position = 2, tip_length = 0.01, vjust = 0.6) + # Significant between HU and IGF1+HU
  geom_signif(comparisons = list(c("UNT","0.2mM HU"), c("1uM NVP","NVP + HU"), 
                                 c("100ng/ml IGF1","IGF1 + HU"), c("IGF1 + NVP", "IGF1 + NVP + HU")),
              map_signif_level = TRUE, textsize=16, test = "t.test",
              y_position = 1.7, tip_length = 0.01, vjust = 0.6) + 
  geom_signif(comparisons = list(c("IGF1 + HU", "IGF1 + NVP + HU")),
              map_signif_level = TRUE, textsize=16, test = "t.test",
              y_position = 1.9, tip_length = 0.01, vjust = 0.6)# Significant between every two groups

dev.off()

##Plotting for R-#####

png(filename = "R-_DNA_Fiber_Assay.png", units = "in", 
    width = 12, height = 12, res = 300)

ggplot(rm, aes(x = group_rm, y = rm$ratio, fill = group_rm)) + 
  scale_fill_brewer(palette = "Dark2") + # could be palette or scale_fill-grey() or fill_manual
  geom_violin(trim = TRUE) + # trim the tail or not, default is TRUE
  geom_boxplot(width = 0.1, fill = "grey") + # add a boxplot in violin plot, otherwise could add dots in violinplot
  #OR
  ## geom_jitter(shape = 16, position = position_jitter(0.2))
  labs(title = "Ratio by treatment (R-)", 
       y = "Ratio", 
       caption = "P = 0.074 of t-test between HU and IGF1 + HU",
       fill = "Treatment") + # main, xlab, ylab and title of legend
  ylim(0 , 3.3) + 
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.x  = element_text(size = 20,angle = 45, hjust = 1, face = "bold"),
        axis.text.y  = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        legend.position = "none", 
        legend.margin = margin(0, 0.2, 0.2, 0.2, "cm"),
        legend.box.margin = margin(0,0.8,0.8,0.8,"cm"),
        legend.title = element_text(size = 20),
        plot.caption = element_text(size = 22))  + 
  geom_signif(comparisons = list(c("0.2mM HU","IGF1 + HU")),
              map_signif_level = TRUE, textsize=9, test = "t.test",
              y_position = 3.25, tip_length = 0.01, vjust = 0) + # Significant between HU and IGF1+HU
  geom_signif(comparisons = list(c("UNT","0.2mM HU"), c("1uM NVP","NVP + HU"), 
                                 c("100ng/ml IGF1","IGF1 + HU"), c("IGF1 + NVP", "IGF1 + NVP + HU")),
              map_signif_level = TRUE, textsize=16, test = "t.test",
              y_position = 2.95, tip_length = 0.01, vjust = 0.6) + 
  geom_signif(comparisons = list(c("IGF1 + HU", "IGF1 + NVP + HU")),
              map_signif_level = TRUE, textsize=9, test = "t.test",
              y_position = 3.15, tip_length = 0.01, vjust = 0)# Significant between every two groups

dev.off()

