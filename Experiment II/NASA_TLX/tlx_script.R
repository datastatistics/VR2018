source("Common_files/load_packages.R")
source("Common_files/statistics_analysis.R")
source("Common_files/utils.R")

tlx_data.df <- read.csv("Experiment II/NASA_TLX/Data/tlx_data.csv", sep = ";")
tlx_scale.df <- read.csv("Experiment II/NASA_TLX/Data/tlx_scale.csv", sep = ";")

scales <- c("Mental Demand", "Physical Demand", "Temporal Demand", "Performance", "Effort", "Frustration")

tlx_temp.df <- data.frame(scales,
                           matrix(0L,nrow = 6, ncol = 2))
colnames(tlx_temp.df) <- c("scale", "weight", "rating")

tlx_score.df <- data.frame(tlx_data.df$setup,
                           matrix(nrow = length(tlx_data.df$user), ncol = 1))
colnames(tlx_score.df) <- c("setup", "value")

for (i in 1:length(tlx_data.df$user)){
  for (j in 3:16){
    value <- tlx_data.df[i,j]
    if(value == "A"){
      row = tlx_scale.df[j-2,2]
      tlx_temp.df$weight[row] = tlx_temp.df$weight[row] + 1
    }else if(value == "B"){
      row = tlx_scale.df[j-2,3]
      tlx_temp.df$weight[row] = tlx_temp.df$weight[row] + 1
    }
  }
  x = 1
  for (j in 17:22){
    tlx_temp.df$rating[x] = tlx_temp.df$weight[x] * ((tlx_data.df[i,j] * 100)/10)
    x = x + 1
  }
  tlx_score.df$value[i] = sum(tlx_temp.df$rating) / 14
  
  tlx_temp.df$weight <- 0L
  tlx_temp.df$rating <- 0L
}

rm(i, j, x, value, row)

######################################################################################

tlx <- data.frame(c("int_cross", "int_hand", "loc_joy", "loc_wip"),
                           matrix(nrow = 4, ncol = 2))

colnames(tlx) <- c("setup", "value", "star")

res = shapiro.test(tlx_score.df$value)
print(res)

setup.df <- tlx_score.df[which(tlx_score.df$setup == "1" | tlx_score.df$setup == "2"),]
tlx[1,2] <- parametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx[1,3] <- significance_level_star(tlx[1,2])

setup.df <- tlx_score.df[which(tlx_score.df$setup == "3" | tlx_score.df$setup == "4"),]
tlx[2,2] <- parametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx[2,3] <- significance_level_star(tlx[2,2])

setup.df <- tlx_score.df[which(tlx_score.df$setup == "1" | tlx_score.df$setup == "3"),]
tlx[3,2] <- parametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx[3,3] <- significance_level_star(tlx[3,2])

setup.df <- tlx_score.df[which(tlx_score.df$setup == "2" | tlx_score.df$setup == "4"),]
tlx[4,2] <- parametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx[4,3] <- significance_level_star(tlx[4,2])

#######################################################################################

melted.df <- ddply(tlx_score.df, .(setup), summarise, sd = sd(value), 
                   se = sd/sqrt(length(value)), value = mean(value))


melted.df[melted.df == "1"] = "Joystick"
melted.df[melted.df == "2"] = "WIP"
melted.df[melted.df == "3"] = "Joystick"
melted.df[melted.df == "4"] = "WIP"

melted.df$int <- c("Crosshair", "Crosshair", "Pointing Finger", "Pointing Finger")

ggplot(melted.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                 labels = c("Head orientation", "Ray-casting")) +
  theme(legend.position = "top", 
        title = element_text(size = 14),
        axis.text = element_text(size=14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14)) + 
  g_theme() +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  labs(y = "Score", x = "Locomotion", title = "NASA TLX") +
  ggsave("Experiment II/NASA_TLX/Charts/tlx_score.eps", width = 6, height = 5)

#######################################################################################
######### workload by tlx scale #################################################


tlx_score.df <- data.frame(scales,
                           rep(tlx_data.df$setup, each = 6),
                           matrix(nrow = length(tlx_data.df$user), ncol = 1))
colnames(tlx_score.df) <- c("scale", "setup", "value")

r = 1
for (i in 1:length(tlx_data.df$user)){
  x = 1
  for (j in 17:22){
    tlx_score.df$value[r] = (tlx_data.df[i,j] * 100)/10
    x = x + 1
    r = r + 1
  }
}

rm(i, j, x, value, row, r)

######################################################################################

tlx_by_scale <- data.frame(c("int_cross", "int_hand", "loc_joy", "loc_wip"),
                           rep(scales, each = 4),
                           matrix(nrow = 24, ncol = 2))

colnames(tlx_by_scale) <- c("setup", "scale", "value", "star")

temp.df <- tlx_score.df[which(tlx_score.df$scale == "Mental Demand"),]
melted.df <- ddply(temp.df, .(setup), summarise, sd = sd(value), 
                   se = sd/sqrt(length(value)), value = mean(value))

#################################################################################
res = shapiro.test(temp.df$value)
print(res)

setup.df <- temp.df[which(temp.df$setup == "1" | temp.df$setup == "2"),]
tlx_by_scale[1,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx_by_scale[1,4] <- significance_level_star(tlx_by_scale[1,3])

setup.df <- temp.df[which(temp.df$setup == "3" | temp.df$setup == "4"),]
tlx_by_scale[2,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx_by_scale[2,4] <- significance_level_star(tlx_by_scale[2,3])

setup.df <- temp.df[which(temp.df$setup == "1" | temp.df$setup == "3"),]
tlx_by_scale[3,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx_by_scale[3,4] <- significance_level_star(tlx_by_scale[3,3])

setup.df <- temp.df[which(temp.df$setup == "2" | temp.df$setup == "4"),]
tlx_by_scale[4,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx_by_scale[4,4] <- significance_level_star(tlx_by_scale[4,3])


melted.df[melted.df == "1"] = "Joystick"
melted.df[melted.df == "2"] = "WIP"
melted.df[melted.df == "3"] = "Joystick"
melted.df[melted.df == "4"] = "WIP"

melted.df$int <- c("Crosshair", "Crosshair", "Pointing Finger", "Pointing Finger")

p1 <- ggplot(melted.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Head Orientation", "Ray-casting")) +
  theme(legend.position = "none") + g_theme() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  labs(y = "Score", x = "Locomotion", title = "Mental Demand") 
#ggsave("NASA_TLX/Charts/tlx_mental_demand.eps", width = 6, height = 5)

###################################################################################

temp.df <- tlx_score.df[which(tlx_score.df$scale == "Physical Demand"),]
melted.df <- ddply(temp.df, .(setup), summarise, sd = sd(value), 
                   se = sd/sqrt(length(value)), value = mean(value))
##################################################################################
res = shapiro.test(temp.df$value)
print(res)

setup.df <- temp.df[which(temp.df$setup == "1" | temp.df$setup == "2"),]
tlx_by_scale[5,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx_by_scale[5,4] <- significance_level_star(tlx_by_scale[5,3])

setup.df <- temp.df[which(temp.df$setup == "3" | temp.df$setup == "4"),]
tlx_by_scale[6,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx_by_scale[6,4] <- significance_level_star(tlx_by_scale[6,3])

setup.df <- temp.df[which(temp.df$setup == "1" | temp.df$setup == "3"),]
tlx_by_scale[7,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx_by_scale[7,4] <- significance_level_star(tlx_by_scale[7,3])

setup.df <- temp.df[which(temp.df$setup == "2" | temp.df$setup == "4"),]
tlx_by_scale[8,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx_by_scale[8,4] <- significance_level_star(tlx_by_scale[8,3])


melted.df[melted.df == "1"] = "Joystick"
melted.df[melted.df == "2"] = "WIP"
melted.df[melted.df == "3"] = "Joystick"
melted.df[melted.df == "4"] = "WIP"

melted.df$int <- c("Crosshair", "Crosshair", "Pointing Finger", "Pointing Finger")

p2 <- ggplot(melted.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Head Orientation", "Ray-casting")) +
  theme(legend.position = "none", 
        axis.text.y = element_blank()) + g_theme() +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = "Score", x = "Locomotion", title = "Physical Demand") 
#ggsave("NASA_TLX/Charts/tlx_physical_demand.eps", width = 6, height = 5)

###################################################################################

temp.df <- tlx_score.df[which(tlx_score.df$scale == "Temporal Demand"),]
melted.df <- ddply(temp.df, .(setup), summarise, sd = sd(value), 
                   se = sd/sqrt(length(value)), value = mean(value))

##################################################################################
res = shapiro.test(temp.df$value)
print(res)

setup.df <- temp.df[which(temp.df$setup == "1" | temp.df$setup == "2"),]
tlx_by_scale[9,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                        NULL, 2)
tlx_by_scale[9,4] <- significance_level_star(tlx_by_scale[9,3])

setup.df <- temp.df[which(temp.df$setup == "3" | temp.df$setup == "4"),]
tlx_by_scale[10,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[10,4] <- significance_level_star(tlx_by_scale[10,3])

setup.df <- temp.df[which(temp.df$setup == "1" | temp.df$setup == "3"),]
tlx_by_scale[11,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[11,4] <- significance_level_star(tlx_by_scale[11,3])

setup.df <- temp.df[which(temp.df$setup == "2" | temp.df$setup == "4"),]
tlx_by_scale[12,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[12,4] <- significance_level_star(tlx_by_scale[12,3])

melted.df[melted.df == "1"] = "Joystick"
melted.df[melted.df == "2"] = "WIP"
melted.df[melted.df == "3"] = "Joystick"
melted.df[melted.df == "4"] = "WIP"

melted.df$int <- c("Crosshair", "Crosshair", "Pointing Finger", "Pointing Finger")

p3 <- ggplot(melted.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Head Orientation", "Ray-casting")) +
  theme(legend.position = "none", 
        axis.text.y = element_blank()) + g_theme() +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = "Score", x = "Locomotion", title = "Temporal Demand") 
#ggsave("NASA_TLX/Charts/tlx_temporal_demand.eps", width = 6, height = 5)

###################################################################################

temp.df <- tlx_score.df[which(tlx_score.df$scale == "Performance"),]
melted.df <- ddply(temp.df, .(setup), summarise, sd = sd(value), 
                   se = sd/sqrt(length(value)), value = mean(value))

##################################################################################
res = shapiro.test(temp.df$value)
print(res)

setup.df <- temp.df[which(temp.df$setup == "1" | temp.df$setup == "2"),]
tlx_by_scale[13,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[13,4] <- significance_level_star(tlx_by_scale[13,3])

setup.df <- temp.df[which(temp.df$setup == "3" | temp.df$setup == "4"),]
tlx_by_scale[14,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[14,4] <- significance_level_star(tlx_by_scale[14,3])

setup.df <- temp.df[which(temp.df$setup == "1" | temp.df$setup == "3"),]
tlx_by_scale[15,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[15,4] <- significance_level_star(tlx_by_scale[15,3])

setup.df <- temp.df[which(temp.df$setup == "2" | temp.df$setup == "4"),]
tlx_by_scale[16,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[16,4] <- significance_level_star(tlx_by_scale[16,3])


melted.df[melted.df == "1"] = "Joystick"
melted.df[melted.df == "2"] = "WIP"
melted.df[melted.df == "3"] = "Joystick"
melted.df[melted.df == "4"] = "WIP"

melted.df$int <- c("Crosshair", "Crosshair", "Pointing Finger", "Pointing Finger")

p4 <- ggplot(melted.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Head Orientation", "Ray-casting")) +
  theme(legend.position = "none") + g_theme() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  labs(y = "Score", x = "Locomotion", title = "Own Performance") 
#ggsave("NASA_TLX/Charts/tlx_performance.eps", width = 6, height = 5)

###################################################################################

temp.df <- tlx_score.df[which(tlx_score.df$scale == "Effort"),]
melted.df <- ddply(temp.df, .(setup), summarise, sd = sd(value), 
                   se = sd/sqrt(length(value)), value = mean(value))

##################################################################################
res = shapiro.test(temp.df$value)
print(res)

setup.df <- temp.df[which(temp.df$setup == "1" | temp.df$setup == "2"),]
tlx_by_scale[17,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[17,4] <- significance_level_star(tlx_by_scale[17,3])

setup.df <- temp.df[which(temp.df$setup == "3" | temp.df$setup == "4"),]
tlx_by_scale[18,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[18,4] <- significance_level_star(tlx_by_scale[18,3])

setup.df <- temp.df[which(temp.df$setup == "1" | temp.df$setup == "3"),]
tlx_by_scale[19,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[19,4] <- significance_level_star(tlx_by_scale[19,3])

setup.df <- temp.df[which(temp.df$setup == "2" | temp.df$setup == "4"),]
tlx_by_scale[20,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[20,4] <- significance_level_star(tlx_by_scale[20,3])

melted.df[melted.df == "1"] = "Joystick"
melted.df[melted.df == "2"] = "WIP"
melted.df[melted.df == "3"] = "Joystick"
melted.df[melted.df == "4"] = "WIP"

melted.df$int <- c("Crosshair", "Crosshair", "Pointing Finger", "Pointing Finger")

p5 <- ggplot(melted.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Head Orientation", "Ray-casting")) +
  theme(legend.position = "none", 
        axis.text.y = element_blank()) + g_theme() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  labs(y = "Score", x = "Locomotion", title = "Effort") 
#ggsave("NASA_TLX/Charts/tlx_effort.eps", width = 6, height = 5)

###################################################################################

temp.df <- tlx_score.df[which(tlx_score.df$scale == "Frustration"),]
melted.df <- ddply(temp.df, .(setup), summarise, sd = sd(value), 
                   se = sd/sqrt(length(value)), value = mean(value))

##################################################################################
res = shapiro.test(temp.df$value)
print(res)

setup.df <- temp.df[which(temp.df$setup == "1" | temp.df$setup == "2"),]
tlx_by_scale[21,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[21,4] <- significance_level_star(tlx_by_scale[21,3])

setup.df <- temp.df[which(temp.df$setup == "3" | temp.df$setup == "4"),]
tlx_by_scale[22,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[22,4] <- significance_level_star(tlx_by_scale[22,3])

setup.df <- temp.df[which(temp.df$setup == "1" | temp.df$setup == "3"),]
tlx_by_scale[23,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[23,4] <- significance_level_star(tlx_by_scale[23,3])

setup.df <- temp.df[which(temp.df$setup == "2" | temp.df$setup == "4"),]
tlx_by_scale[24,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                         NULL, 2)
tlx_by_scale[24,4] <- significance_level_star(tlx_by_scale[24,3])

melted.df[melted.df == "1"] = "Joystick"
melted.df[melted.df == "2"] = "WIP"
melted.df[melted.df == "3"] = "Joystick"
melted.df[melted.df == "4"] = "WIP"

melted.df$int <- c("Crosshair", "Crosshair", "Pointing Finger", "Pointing Finger")

p6 <- ggplot(melted.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Head Orientation", "Ray-casting")) +
  theme(legend.position = "none", 
        axis.text.y = element_blank()) + g_theme() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  labs(y = "Score", x = "Locomotion", title = "Frustration") 
# ggsave("NASA_TLX/Charts/tlx_frustration.eps", width = 6, height = 5)

p1.leg <-  ggplot(melted.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Head Orientation", "Ray-casting")) +
  theme(legend.position = "top") + g_theme() 

leg <- g_legend(p1.leg)

g <- grid.arrange(arrangeGrob(p1, p2, p3, nrow = 1),
                  arrangeGrob(p4, p5, p6, nrow = 1),
                  leg, nrow=3, heights=c(2, 2, 0.5))

ggsave("Experiment II/NASA_TLX/Charts/tlx_by_scale.eps", g, width = 8, height = 5)

rm(melted.df, tlx_score.df, tlx_temp.df, tlx_scale.df, tlx_data.df,
   scales, temp.df, p1, p2, p3, p4, p5, p6, leg, g, p1.leg)

rm(res, setup.df)