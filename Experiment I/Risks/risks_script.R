source("Common_files/load_packages.R")
source("Common_files/statistics_analysis.R")
source("Common_files/utils.R")

risks_info.df <- read.csv("Experiment I/Risks/Data/risks_info.csv", sep = ";")
risks.df <- read.csv("Experiment I/Risks/Data/risks_data.csv", sep = ";")

users_count <- max(risks.df$user, na.rm = TRUE)

simple.df <- risks_info.df[which(risks_info.df$type == "simple"),]
composite.df <- risks_info.df[which(risks_info.df$type == "composite"),]

display.df <- get_display_info()

count.df <- data.frame(rep(c(1:users_count), each = 2),
                       rep(display.df$output, each = 2),
                       rep(c("simple", "composite"), each = 1),
                       matrix(0L, ncol = 2, nrow = (users_count * 2)))
colnames(count.df) <- c("user", "display", "variable", "value", "number")

user = 1
row = 1
i = 1
while (i <= length(risks.df$user)){
  while(risks.df$user[i] == user && i <= length(risks.df$user)){
    if (risks.df$id[i] %in% simple.df$id){
      count.df$value[row] = count.df$value[row] + 1
    }else if (risks.df$id[i] %in% composite.df$id){
      count.df$value[row + 1] = count.df$value[row + 1] + 1
    }
    i = i + 1
  }
  total = count.df$value[row] + count.df$value[row + 1]
  count.df$number[row] = count.df$value[row];
  count.df$number[row + 1] = count.df$value[row + 1];
  count.df$value[row] = count.df$value[row] / total;
  count.df$value[row + 1] = count.df$value[row + 1] / total;
  
  user = user + 1
  row = row + 2
}

rm(user, row, i)

#################################################################################
############ statistical analysis ###############################################
#################################################################################
risks_categories <- data.frame(c("Monitor", "Display Wall", "Oculus Rift"),
                                matrix(nrow = 3, ncol = 2))
colnames(risks_categories) <- c("display", "p-value", "star")

shapiro.test(count.df$value)

temp.df <- count.df[which(count.df$display == "Monitor"),]
risks_categories[1,2] <- nonparametric_dependent_analysis(temp.df$number, temp.df$variable, NULL, 2)
risks_categories[1,3] <- significance_level_star(risks_categories[1,2])

temp.df <- count.df[which(count.df$display == "Display Wall"),]
risks_categories[2,2] <- nonparametric_dependent_analysis(temp.df$number, temp.df$variable, NULL, 2)
risks_categories[2,3] <- significance_level_star(risks_categories[2,2])

temp.df <- count.df[which(count.df$display == "Oculus Rift"),]
risks_categories[3,2] <- nonparametric_dependent_analysis(temp.df$number, temp.df$variable, NULL, 2)
risks_categories[3,3] <- significance_level_star(risks_categories[3,2])

################################################################################################

risks_cat_display <- data.frame(c("Composite", "Simple"),
                               matrix(nrow = 2, ncol = 2))
colnames(risks_cat_display) <- c("category", "p-value", "star")

shapiro.test(count.df$value)

temp.df <- count.df[which(count.df$variable == "composite"),]
risks_cat_display[1,2] <- nonparametric_independent_analysis(temp.df$number, temp.df$display, NULL, 3)
risks_cat_display[1,3] <- significance_level_star(risks_cat_display[1,2])

temp.df <- count.df[which(count.df$variable == "simple"),]
risks_cat_display[2,2] <- nonparametric_independent_analysis(temp.df$number, temp.df$display, NULL, 3)
risks_cat_display[2,3] <- significance_level_star(risks_cat_display[2,2])

rm(temp.df)

#################################################################################
########### charts ##############################################################
#################################################################################

foo = rep(0, nrow(count.df))

foo[with(count.df, display == "Monitor")] = 1
foo[with(count.df, display == "Display Wall")] = 2
foo[with(count.df, display == "Oculus Rift")] = 3

count.df$display = with(count.df, reorder(display, foo))

rm(foo)

# Define arc coordinates
r <- 0.15
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 0.15 * sin(t)

arc.df <- data.frame(display = x, value = y)
######################################################################################################################
#bar plots

drops <- "user"
melted.df <- count.df[ , !(names(count.df) %in% drops)]
rm(drops)

melted.df <- ddply(melted.df, .(display, variable), summarise,
                   sd = sd(value), se = sd/sqrt(length(value)),
                   value = mean(value))

lty = 6
label.df <- data.frame(display = c(1, 2, 3),
                       value = c(0.8, 0.9, 0.8))

p1 <- ggplot(melted.df, aes(display, value)) +
  geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
  geom_errorbar(aes(fill = variable, ymin=value-se, ymax=value+se), 
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_text(data = label.df, size = 5,
            label = risks_categories$star) +
  geom_line(data = arc.df, aes(display+1, value+0.77), lty = lty) +
  geom_line(data = arc.df, aes(display+2, value+0.87), lty = lty) +
  geom_line(data = arc.df, aes(display+3, value+0.77), lty = lty) +
  theme(legend.position = "bottom",
        title = element_text(size = 14),
        axis.text = element_text(size=14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14))+ 
  g_theme() + 
  labs(x = "Display Condition", y = "Risks Selection Rate", title = "") +
  scale_fill_brewer(guide = guide_legend(title = "Categories"), 
                    labels = c("Composite", "Simple")) +
  scale_x_discrete(labels = c("Desktop", "Display-Wall", "HMD")) +
  scale_y_continuous(limits = c(0,1), labels = percent) +
  ggsave("Experiment I/Risks/Charts/categories.eps", width = 5, height = 4)


r <- 0.25
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 0.1 * sin(t)
arc.df <- data.frame(variable = x, value = y)
rm(r, t, x, y)

label.df <- data.frame(variable = c(1, 2),
                       value = c(0.5, 0.9))

p2 <- ggplot(melted.df, aes(variable, value)) +
  geom_bar(aes(fill = display), stat = "identity", position = "dodge") +
  geom_errorbar(aes(fill = display, ymin=value-se, ymax=value+se), 
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_text(data = label.df, size = 4,
            label = risks_cat_display$star) +
  geom_line(data = arc.df, aes(variable+1, value+0.47), lty = lty) +
  geom_line(data = arc.df, aes(variable+2, value+0.85), lty = lty) +
  #geom_line(data = arc.df, aes(variable+3, value+0.77), lty = lty) +
  theme(legend.position = "bottom")+ g_theme() + 
  labs(x = "Categories", y = "Risks Selection Rate", title = "") +
  scale_fill_brewer(guide = guide_legend(title = "VR Displays"), 
                 labels = c("Desktop", "Display-Wall", "HMD")) +
  scale_x_discrete(labels = c("Composite", "Simple")) +
  scale_y_continuous(limits = c(0,1), labels = percent) +
  ggsave("Experiment I/Risks/Charts/risk_cond_cat.eps", width = 5, height = 4)

rm(count.df, label.df, arc.df, lty, simple.df, composite.df, display.df,
   users_count, p1, p2)

##################################################################################################################


risks_info <- risks_info.df[complete.cases(risks_info.df),]

total_risks = 53

sum.df <- ddply(risks.df, .(user, display), summarise, value = length(id))
sum.df <- sum.df[with(sum.df, order(display)),]
sum.df$user <- c(1:22, 1:20, 1:19)

mean.df <- ddply(sum.df, .(display), summarise, 
                 sd = (sd(value) / total_risks), 
                 se = (sd(value)/sqrt(length(value)) / total_risks), 
                 mean = (mean(value) / total_risks))

foo = rep(0, nrow(mean.df))

foo[with(mean.df, display == "Monitor")] = 1
foo[with(mean.df, display == "DisplayWall")] = 2
foo[with(mean.df, display == "Oculus Rift")] = 3

mean.df$display = with(mean.df, reorder(display, foo))

rm(foo)

colnames(mean.df) <- c("variable", "se", "sd", "value")

p_value <- nonparametric_independent_analysis(sum.df$value, sum.df$display, NULL, 3)
star <- significance_level_star(p_value)

label.df <- data.frame(variable = 2,
                       value = .45)

r <- 1
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 0.05 * sin(t)

arc.df <- data.frame(variable = x, value = y)

rm(r, t, x, y)

ggplot(mean.df, aes(variable, value)) +
  geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
  geom_errorbar(aes(fill = variable, ymin=value-se, ymax=value+se), 
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_text(data = label.df, label = star) +
  geom_line(data = arc.df, aes(variable+2, value+0.38), lty = 6) +
  scale_fill_brewer() +
  theme(legend.position = "none") + g_theme() +
  scale_x_discrete(labels = c("Low", "Moderate", "High")) +
  scale_y_continuous(limits = c(0,0.5), labels = percent) +
  labs(y = "Risk Selection Rate", x="Display Fidelity") +
  ggsave("Experiment I/Risks/Charts/risks_general.eps", width = 6, height = 4)


img <- readPNG("Experiment I/Map/map_background.png")

ggplot(risks_info, aes(y, x, color = factor(name))) +
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")),
                    -Inf, Inf, -Inf, Inf) +
  geom_point(size = 2) +
  guides(col = guide_legend(nrow = 18, title = "Risks", title.position = "top")) +
  labs(y = "", x="") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        legend.position = "left") +
  scale_x_continuous(expand=c(0,0), limits = c(-32.4,0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-28.7,0)) +
  ggsave("Experiment I/Risks/Charts/risks_map.eps", width = 10, height = 5)

rm(img, label.df, arc.df, mean.df, sum.df, total_risks, risks_info)


############################################################################################

rbyi.df <- read.csv("Experiment I/Risks/Data/risk_by_inst.csv", sep = ';')
#rbyi.df <- melt(rbyi.df)

temp.df <- melt(rbyi.df)
temp.df <- temp.df[complete.cases(temp.df),]
count.df <- ddply(temp.df, .(variable), summarise, value = length(!is.na(value)))
rm(temp.df)

rate.df <- data.frame(matrix(0L,nrow = 62, ncol = 8))
colnames(rate.df) <- c("user", "display", colnames(rbyi.df))

user = 1
i = 1
while (i <= length(risks.df$user)){
  rate.df$user[user] <- user
  rate.df$display[user] <- risks.df$display[i]
  while(risks.df$user[i] == user){
    if(risks.df$id[i] %in% rbyi.df$inst1)
      rate.df$inst1[user] <- rate.df$inst1[user] + 1
    if(risks.df$id[i] %in% rbyi.df$inst2)
      rate.df$inst2[user] <- rate.df$inst2[user] + 1
    if(risks.df$id[i] %in% rbyi.df$inst3)
      rate.df$inst3[user] <- rate.df$inst3[user] + 1
    if(risks.df$id[i] %in% rbyi.df$inst4)
      rate.df$inst4[user] <- rate.df$inst4[user] + 1
    if(risks.df$id[i] %in% rbyi.df$inst5)
      rate.df$inst5[user] <- rate.df$inst5[user] + 1
    if(risks.df$id[i] %in% rbyi.df$inst6)
      rate.df$inst6[user] <- rate.df$inst6[user] + 1
    i <- i+1
    if(i > length(risks.df$user)) break
  }
  
  rate.df$inst1[user] <- rate.df$inst1[user] / count.df$value[1]
  rate.df$inst2[user] <- rate.df$inst2[user] / count.df$value[2]
  rate.df$inst3[user] <- rate.df$inst3[user] / count.df$value[3]
  rate.df$inst4[user] <- rate.df$inst4[user] / count.df$value[4]
  rate.df$inst5[user] <- rate.df$inst5[user] / count.df$value[5]
  rate.df$inst6[user] <- rate.df$inst6[user] / count.df$value[6]
  
  user <- user + 1
}
rm(i, user)

drops <- "user"
rate.df <- melt(rate.df, id.vars = c("user", "display"))

foo = rep(0, nrow(rate.df))

foo[with(rate.df, display == "2")] = 1
foo[with(rate.df, display == "1")] = 2
foo[with(rate.df, display == "3")] = 3

rate.df$display = with(rate.df, reorder(display, foo))

rm(foo)

###############################################################################################
###### statistics analysis ####################################################################
###############################################################################################

risks_by_inst <- data.frame(c("inst1", "inst2", "inst3", "inst4", "inst5", "inst6"),
                            matrix(nrow = 6, ncol = 2))
colnames(risks_by_inst) <- c("inst", "p-value", "star")

temp_rate.df <- rate.df[which(rate.df$variable == "inst1"),]
risks_by_inst[1,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$display, NULL, 3)
risks_by_inst[1,3] <- significance_level_star(risks_by_inst[1,2])                                                   

temp_rate.df <- rate.df[which(rate.df$variable == "inst2"),]
risks_by_inst[2,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$display, NULL, 3)
risks_by_inst[2,3] <- significance_level_star(risks_by_inst[2,2])

temp_rate.df <- rate.df[which(rate.df$variable == "inst3"),]
risks_by_inst[3,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$display, NULL, 3)
risks_by_inst[3,3] <- significance_level_star(risks_by_inst[3,2])

temp_rate.df <- rate.df[which(rate.df$variable == "inst4"),]
risks_by_inst[4,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$display, NULL, 3)
risks_by_inst[4,3] <- significance_level_star(risks_by_inst[4,2])

temp_rate.df <- rate.df[which(rate.df$variable == "inst5"),]
risks_by_inst[5,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$display, NULL, 3)
risks_by_inst[5,3] <- significance_level_star(risks_by_inst[5,2])

temp_rate.df <- rate.df[which(rate.df$variable == "inst6"),]
risks_by_inst[6,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$display, NULL, 3)
risks_by_inst[6,3] <- significance_level_star(risks_by_inst[6,2])


risks_display <- data.frame(c("monitor", "displaywall", "oculus"),
                            matrix(nrow = 3, ncol = 2))
colnames(risks_display) <- c("display", "p-value", "star")

temp_rate.df <- rate.df[which(rate.df$display == 2),]
risks_display[1,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$variable, NULL, 6)
risks_display[1,3] <- significance_level_star(risks_display[1,2])

temp_rate.df <- rate.df[which(rate.df$display == 1),]
risks_display[2,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$variable, NULL, 6)
risks_display[2,3] <- significance_level_star(risks_display[2,2])

temp_rate.df <- rate.df[which(rate.df$display == 3),]
risks_display[3,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$variable, NULL, 6)
risks_display[3,3] <- significance_level_star(risks_display[3,2])

########################################################################################################################
################  CHARTS ###############################################################################################
########################################################################################################################

plot.df <- rate.df[ , !(names(rate.df) %in% drops)]
plot.df <- ddply(plot.df, .(display, variable), summarise, 
                 sd = sd(value), se = sd / sqrt(length(value)), 
                 value = mean(value))

# Define arc coordinates
r <- 0.25
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 0.05 * sin(t)

arc.df <- data.frame(variable = x, value = y)

######################################################################################################################
#bar plots

lty = 6
label.df <- data.frame(variable = c(1, 2, 3, 4, 5, 6),
                       value = c(.38, .43, .38, .33, .31, .26))

p1 <- ggplot(plot.df, aes(variable, value)) + 
  geom_bar(aes(fill = factor(display)), position = "dodge", stat = "identity") +
  geom_errorbar(aes(fill = factor(display), ymin = value-se, ymax=value+se), 
                stat = "identity", 
                width = .1, position=position_dodge(width=0.9)) +
  geom_text(data = label.df, size = 4,
            label = risks_by_inst$star) +
  geom_line(data = arc.df, aes(variable+1, value+.35), lty = lty) +
  geom_line(data = arc.df, aes(variable+2, value+.41), lty = lty) +
  geom_line(data = arc.df, aes(variable+3, value+.35), lty = lty) +
  geom_line(data = arc.df, aes(variable+4, value+.30), lty = lty) +
  geom_line(data = arc.df, aes(variable+5, value+.28), lty = lty) +
  geom_line(data = arc.df, aes(variable+6, value+.23), lty = lty) +
  labs(y = "Risks Selection Rate", x = "Instructions", title = "(a)") +
  theme(plot.margin=unit(c(0,0.5,0,0), "cm"), 
        legend.position = "bottom") + g_theme() +
  scale_fill_brewer(guide = guide_legend(title = "VR Displays"), 
                    labels = c("Desktop", "Display-Wall", "HMD")) +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5", "6")) +
  scale_y_continuous(limits = c(0,0.5), labels = percent) +
  ggsave("Experiment I/Risks/Charts/risks_by_inst.eps", width = 10, height = 5)


###################################################################################

###################################################################################
################# risks by place ##################################################
###################################################################################

###################################################################################

places.df <- read.csv("Experiment I/Risks/Data/risks_by_place.csv", sep = ';')
#rbyi.df <- melt(rbyi.df)

temp.df <- melt(places.df)
temp.df <- temp.df[complete.cases(temp.df),]
count.df <- ddply(temp.df, .(variable), summarise, value = length(!is.na(value)))
rm(temp.df)

rate.df <- data.frame(matrix(0L,nrow = 62, ncol = 6))
colnames(rate.df) <- c("user", "display", colnames(places.df))

user = 1
i = 1
while (i <= length(risks.df$user)){
  rate.df$user[user] <- user
  rate.df$display[user] <- risks.df$display[i]
  while(risks.df$user[i] == user){
    if(risks.df$id[i] %in% places.df$reception)
      rate.df$reception[user] <- rate.df$reception[user] + 1
    if(risks.df$id[i] %in% places.df$parking)
      rate.df$parking[user] <- rate.df$parking[user] + 1
    if(risks.df$id[i] %in% places.df$kitchen)
      rate.df$kitchen[user] <- rate.df$kitchen[user] + 1
    if(risks.df$id[i] %in% places.df$office)
      rate.df$office[user] <- rate.df$office[user] + 1
    i <- i+1
    if(i > length(risks.df$user)) break
  }
  
  rate.df$reception[user] <- rate.df$reception[user] / count.df$value[1]
  rate.df$parking[user] <- rate.df$parking[user] / count.df$value[2]
  rate.df$kitchen[user] <- rate.df$kitchen[user] / count.df$value[3]
  rate.df$office[user] <- rate.df$office[user] / count.df$value[4]
  
  user <- user + 1
}
rm(i, user)

drops <- "user"
rate.df <- melt(rate.df, id.vars = c("user", "display"))

foo = rep(0, nrow(rate.df))

foo[with(rate.df, display == "2")] = 1
foo[with(rate.df, display == "1")] = 2
foo[with(rate.df, display == "3")] = 3

rate.df$display = with(rate.df, reorder(display, foo))

rm(foo)

foo = rep(0, nrow(rate.df))

foo[with(rate.df, variable == "reception")] = 1
foo[with(rate.df, variable == "parking")] = 2
foo[with(rate.df, variable == "kitchen")] = 3
foo[with(rate.df, variable == "office")] = 4

rate.df$variable = with(rate.df, reorder(variable, foo))

rm(foo)

###############################################################################################
###### statistics analysis ####################################################################
###############################################################################################

risks_by_place <- data.frame(c("reception", "parking", "kitchen", "office"),
                             matrix(nrow = 4, ncol = 2))
colnames(risks_by_place) <- c("inst", "p-value", "star")

temp_rate.df <- rate.df[which(rate.df$variable == "reception"),]
risks_by_place[1,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$display, NULL, 3)
risks_by_place[1,3] <- significance_level_star(risks_by_place[1,2])                                                   

temp_rate.df <- rate.df[which(rate.df$variable == "parking"),]
risks_by_place[2,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$display, NULL, 3)
risks_by_place[2,3] <- significance_level_star(risks_by_place[2,2])

temp_rate.df <- rate.df[which(rate.df$variable == "kitchen"),]
risks_by_place[3,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$display, NULL, 3)
risks_by_place[3,3] <- significance_level_star(risks_by_place[3,2])

temp_rate.df <- rate.df[which(rate.df$variable == "office"),]
risks_by_place[4,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$display, NULL, 3)
risks_by_place[4,3] <- significance_level_star(risks_by_place[4,2])


risks_place_display <- data.frame(c("monitor", "displaywall", "oculus"),
                                  matrix(nrow = 3, ncol = 2))
colnames(risks_place_display) <- c("display", "p-value", "star")

temp_rate.df <- rate.df[which(rate.df$display == 2),]
risks_place_display[1,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$variable, NULL, 6)
risks_place_display[1,3] <- significance_level_star(risks_place_display[1,2])

temp_rate.df <- rate.df[which(rate.df$display == 1),]
risks_place_display[2,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$variable, NULL, 6)
risks_place_display[2,3] <- significance_level_star(risks_place_display[2,2])

temp_rate.df <- rate.df[which(rate.df$display == 3),]
risks_place_display[3,2] <- nonparametric_independent_analysis(temp_rate.df$value, temp_rate.df$variable, NULL, 6)
risks_place_display[3,3] <- significance_level_star(risks_place_display[3,2])

########################################################################################################################
################  CHARTS ###############################################################################################
########################################################################################################################

plot.df <- rate.df[ , !(names(rate.df) %in% drops)]
plot.df <- ddply(plot.df, .(display, variable), summarise, 
                 sd = sd(value), se = sd / sqrt(length(value)), 
                 value = mean(value))

foo = rep(0, nrow(plot.df))

foo[with(plot.df, variable == "reception")] = 1
foo[with(plot.df, variable == "parking")] = 2
foo[with(plot.df, variable == "kitchen")] = 3
foo[with(plot.df, variable == "office")] = 4

plot.df$variable = with(plot.df, reorder(variable, foo))

rm(foo)

# Define arc coordinates
r <- 0.25
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 0.05 * sin(t)

arc.df <- data.frame(variable = x, value = y)

######################################################################################################################
#bar plots

lty = 6
label.df <- data.frame(variable = c(1, 2, 3, 4),
                       value = c(0.4, 0.4, 0.28, 0.28))

p3 <- ggplot(plot.df, aes(variable, value)) + 
  geom_bar(aes(fill = factor(display)), position = "dodge", stat = "identity") +
  geom_errorbar(aes(fill = factor(display), ymin = value-se, ymax=value+se), 
                stat = "identity", 
                width = .1, position=position_dodge(width=0.9)) +
  geom_text(data = label.df, size = 4,
            label = risks_by_place$star) +
  geom_line(data = arc.df, aes(variable+1, value+0.38), lty = lty) +
  geom_line(data = arc.df, aes(variable+2, value+0.36), lty = lty) +
  geom_line(data = arc.df, aes(variable+3, value+0.25), lty = lty) +
  geom_line(data = arc.df, aes(variable+4, value+0.25), lty = lty) +
  labs(y = "Risks Selection Rate", x = "Places", title = "(b)") +
  theme(plot.margin=unit(c(0,0.5,0,0), "cm"), 
        legend.position = "bottom") + g_theme() +
  scale_fill_brewer(guide = guide_legend(title = "VR Displays"), 
                    labels = c("Desktop", "Display-Wall", "HMD")) +
  scale_y_continuous(limits = c(0,0.5), labels = percent) +
  scale_x_discrete(labels = c("Reception", "Parking Lot", "Kitchen", "Office")) 
#ggsave("Risks/Charts/risks_by_inst.eps", width = 10, height = 5)

label.df <- data.frame(display = c(1, 2, 3),
                       value = c(0.4, 0.33, 0.4))

arc.df <- data.frame(display = x, value = y)

rm(r, t, x, y)

p4 <- ggplot(plot.df, aes(display, value)) + 
  geom_bar(aes(fill = factor(variable)), position = "dodge", stat = "identity") +
  geom_errorbar(aes(fill = factor(variable), ymin = value-se, ymax=value+se), 
                stat = "identity", 
                width = .1, position=position_dodge(width=0.9)) +
  geom_text(data = label.df, size = 4,
            label = risks_place_display$star) +
  geom_line(data = arc.df, aes(display+1, value+0.38), lty = lty) +
  geom_line(data = arc.df, aes(display+2, value+0.3), lty = lty) +
  geom_line(data = arc.df, aes(display+3, value+0.38), lty = lty) +
  labs(y = "Risks Selection Rate", x = "VR Displays", title = "(d)") +
  theme(plot.margin=unit(c(0,0.5,0,0), "cm"), 
        legend.position = "bottom") + g_theme() +
  scale_fill_brewer(guide = guide_legend(title = "Places", nrow = 1), 
                    labels = c("Reception", "Parking Lot", "Kitchen", "Office")) +
  scale_y_continuous(limits = c(0,0.5), labels = percent) +
  scale_x_discrete(labels = c("Desktop", "Display-Wall", "HMD")) 
#ggsave("Risks/Charts/risks_by_inst_display.eps", width = 10, height = 5)

g <- grid.arrange(arrangeGrob(p1, p3, nrow = 1))

ggsave("Experiment I/Risks/Charts/risks.eps", g, width = 10, height = 5)

rm(arc.df, count.df, label.df, rate.df, rbyi.df, risks.df,
   temp_rate.df, p1, p2, g, lty, p3, p4, places.df, risks_info.df)