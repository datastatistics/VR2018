source("Common_files/load_packages.R")
source("Common_files/statistics_analysis.R")
source("Common_files/utils.R")

time.df <- read.csv("Experiment II/Time/Data/time_data.csv", sep = ";")

time_data.df <- ddply(time.df, .(setup, stage), summarise, sd = sd(time), 
                   se = sd/sqrt(length(time)), value = mean(time))

time_data.df[time_data.df == "1"] = "Joystick"
time_data.df[time_data.df == "2"] = "WIP"
time_data.df[time_data.df == "3"] = "Joystick"
time_data.df[time_data.df == "4"] = "WIP"

time_data.df$int <- c("Crosshair", "Crosshair", "Crosshair",
                      "Crosshair", "Crosshair", "Crosshair",
                      "Pointing Finger", "Pointing Finger", "Pointing Finger",
                      "Pointing Finger", "Pointing Finger", "Pointing Finger")

time_temp.df <- time_data.df[which(time_data.df$stage == "learning"),]
p1 <- ggplot(time_temp.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Head Orientation", "Ray-casting")) +
  theme(plot.margin = unit(c(0.5,0,0,0), "cm"),
        legend.position = "none") + g_theme() +
  scale_y_continuous(limits = c(250, 900)) +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  labs(y = "Time (in seconds)", x = "Locomotion", title = "Learning") 

time_temp.df <- time_data.df[which(time_data.df$stage == "evaluation"),]
p2 <- ggplot(time_temp.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Head Orientation", "Ray-casting")) +
  theme(plot.margin = unit(c(0.5,0,0,0), "cm"),
        legend.position = "none",
        axis.text.y = element_blank()) + g_theme() +
  scale_y_continuous(limits = c(250, 900)) +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  labs(y = "", x = "Locomotion", title = "Evaluation Part 1") 

time_temp.df <- time_data.df[which(time_data.df$stage == "evaluation2"),]
p3 <- ggplot(time_temp.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Head Orientation", "Ray-casting")) +
  theme(plot.margin = unit(c(0.5,0,0,0), "cm"),
        legend.position = "none",
        axis.text.y = element_blank()) + g_theme() +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  scale_y_continuous(limits = c(250, 900)) +
  labs(y = "", x = "Locomotion", title = "Evaluation Part 2") 

#######################################################################################

time_data.df <- ddply(time.df, .(user, setup), summarise, value = sum(time))

melted.df <- ddply(time_data.df, .(setup), summarise, sd = sd(value), 
                   se = sd/sqrt(length(value)), value = mean(value))

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
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none") + g_theme() +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  # scale_y_continuous(limits = c(250, 900)) +
  labs(y = "Time (in seconds)", x = "Locomotion", title = "Trial Total Time") 
  #ggsave("Time/Charts/total_time.eps", width = 6, height = 5)

#######################################################################################


p1.leg <- ggplot(time_temp.df, aes(x = setup, y = value)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction", nrow = 1),
                  labels = c("Head Orientation", "Ray-casting")) +
  theme(legend.position = "bottom") + g_theme() 

leg <- g_legend(p1.leg)

g <- grid.arrange(arrangeGrob(p4, nrow = 1),
                  arrangeGrob(p1, p2, p3, nrow = 1),
                  leg, nrow=3,heights=c(2, 2, 0.5))


ggsave("Experiment II/Time/Charts/time_between_setups.eps", g, width = 5, height = 5)

rm(leg, g, p1.leg, p1, p2, p3, p4)

#########################################################################################

time_levels <- data.frame(c("int_cross", "int_hand", "loc_joy", "loc_wip"),
                          rep(c("learn", "eval", "eval2"), each = 4),
                          matrix(nrow = 12, ncol = 2))
colnames(time_levels) <- c("setup", "level", "value", "star")

temp_level.df <- time.df[which(time.df$stage == "learning"),]
temp.df <- temp_level.df[which(temp_level.df$setup == "1" | temp_level.df$setup == "2"),]
time_levels[1,3] <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                                    NULL, 2)
time_levels[1,4] <- significance_level_star(time_levels[1,3])

temp.df <- temp_level.df[which(temp_level.df$setup == "3" | temp_level.df$setup == "4"),]
time_levels[2,3] <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                                    NULL, 2)
time_levels[2,4] <- significance_level_star(time_levels[2,3])

temp.df <- temp_level.df[which(temp_level.df$setup == "1" | temp_level.df$setup == "3"),]
time_levels[3,3] <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                                    NULL, 2)
time_levels[3,4] <- significance_level_star(time_levels[3,3])

temp.df <- temp_level.df[which(temp_level.df$setup == "2" | temp_level.df$setup == "4"),]
time_levels[4,3] <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                                    NULL, 2)
time_levels[4,4] <- significance_level_star(time_levels[4,3])

temp_level.df <- time.df[which(time.df$stage == "evaluation"),]
temp.df <- temp_level.df[which(temp_level.df$setup == "1" | temp_level.df$setup == "2"),]
time_levels[5,3] <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                                    NULL, 2)
time_levels[5,4] <- significance_level_star(time_levels[5,3])

temp.df <- temp_level.df[which(temp_level.df$setup == "3" | temp_level.df$setup == "4"),]
time_levels[6,3] <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                                    NULL, 2)
time_levels[6,4] <- significance_level_star(time_levels[6,3])

temp.df <- temp_level.df[which(temp_level.df$setup == "1" | temp_level.df$setup == "3"),]
time_levels[7,3] <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                                    NULL, 2)
time_levels[7,4] <- significance_level_star(time_levels[7,3])

temp.df <- temp_level.df[which(temp_level.df$setup == "2" | temp_level.df$setup == "4"),]
time_levels[8,3] <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                                    NULL, 2)
time_levels[8,4] <- significance_level_star(time_levels[8,3])

temp_level.df <- time.df[which(time.df$stage == "evaluation2"),]
temp.df <- temp_level.df[which(temp_level.df$setup == "1" | temp_level.df$setup == "2"),]
time_levels[9,3] <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                                    NULL, 2)
time_levels[9,4] <- significance_level_star(time_levels[9,3])

temp.df <- temp_level.df[which(temp_level.df$setup == "3" | temp_level.df$setup == "4"),]
time_levels[10,3] <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                                    NULL, 2)
time_levels[10,4] <- significance_level_star(time_levels[10,3])

temp.df <- temp_level.df[which(temp_level.df$setup == "1" | temp_level.df$setup == "3"),]
time_levels[11,3] <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                                    NULL, 2)
time_levels[11,4] <- significance_level_star(time_levels[11,3])

temp.df <- temp_level.df[which(temp_level.df$setup == "2" | temp_level.df$setup == "4"),]
time_levels[12,3] <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                                    NULL, 2)
time_levels[12,4] <- significance_level_star(time_levels[12,3])

##############################################################################################


time_data.df <- ddply(time.df, .(setup, stage), summarise, sd = sd(time), 
                      se = sd/sqrt(length(time)), value = mean(time))

foo = rep(0, nrow(time_data.df))

foo[with(time_data.df, stage == "learning")] = 1
foo[with(time_data.df, stage == "evaluation")] = 2
foo[with(time_data.df, stage == "evaluation2")] = 3

time_data.df$stage = with(time_data.df, reorder(stage, foo))

rm(foo)

#################################################################

statistics.result <- data.frame(matrix(nrow = 4, ncol = 1))
colnames(statistics.result) <- c("value")

res = shapiro.test(time.df$value)
print(res)

temp.df <- time.df[which(time.df$setup == "1"),]
p <- nonparametric_dependent_analysis(temp.df$time, temp.df$stage,
                                      temp.df$user, 3)
statistics.result[1,1] <- significance_level_star(p)

temp.df <- time.df[which(time.df$setup == "2"),]
p <- nonparametric_dependent_analysis(temp.df$time, temp.df$stage,
                                      temp.df$user, 3)
statistics.result[2,1] <- significance_level_star(p)

temp.df <- time.df[which(time.df$setup == "3"),]
p <- nonparametric_dependent_analysis(temp.df$time, temp.df$stage,
                                      temp.df$user, 3)
statistics.result[3,1] <- significance_level_star(p)

temp.df <- time.df[which(time.df$setup == "4"),]
p <- nonparametric_dependent_analysis(temp.df$time, temp.df$stage,
                                      temp.df$user, 3)
statistics.result[4,1] <- significance_level_star(p)

##################################################################

# Define arc coordinates
r <- 0.3
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 100 * sin(t)

arc.df <- data.frame(setup = x, value = y)
rm(r, t, x, y)

lty = 6
label.df <- data.frame(setup = c(1, 2, 3, 4),
                       value = c(635, 700, 800, 885))

ggplot(time_data.df, aes(x = factor(setup), y = value)) +
  geom_bar(aes(fill = factor(stage)), stat = "identity", position = "dodge") +
  geom_errorbar(aes(fill = factor(stage), ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_text(data = label.df, size = 4,
            label = statistics.result$value) +
  geom_line(data = arc.df, aes(setup+1, value+600), lty = lty) +
  geom_line(data = arc.df, aes(setup+2, value+665), lty = lty) +
  geom_line(data = arc.df, aes(setup+3, value+765), lty = lty) +
  geom_line(data = arc.df, aes(setup+4, value+850), lty = lty) +
  scale_fill_brewer(guide = guide_legend(title = "Level"),
                    labels = c("Learning", "Evaluation Part 1", "Evaluation Part 2")) +
  theme(legend.position = "bottom") + g_theme() +
  scale_x_discrete(labels = c("D", "C", "B", "A")) +
  labs(y = "Time (in seconds)", x = "Condition", title = "Completion Time") +
  ggsave("Experiment II/Time/Charts/time_levels.png", width = 8, height = 5)

########################################################################################

#####################################################################################
######### messages time #############################################################
#####################################################################################
messages.df <- read.csv("Experiment II/Time/Data/message_data.csv", sep = ";")

time_messages_setups <- data.frame(matrix(nrow = 4, ncol = 1))
colnames(time_messages_setups) <- c("value")

temp.df <- messages.df[which(messages.df$setup == "1"),]
p <- nonparametric_dependent_analysis(temp.df$time, temp.df$name,
                                      temp.df$user, 6)
time_messages_setups[1,1] <- significance_level_star(p)

temp.df <- messages.df[which(messages.df$setup == "2"),]
p <- nonparametric_dependent_analysis(temp.df$time, temp.df$name,
                                      temp.df$user, 6)
time_messages_setups[2,1] <- significance_level_star(p)

temp.df <- messages.df[which(messages.df$setup == "3"),]
p <- nonparametric_dependent_analysis(temp.df$time, temp.df$name,
                                      temp.df$user, 6)
time_messages_setups[3,1] <- significance_level_star(p)

temp.df <- messages.df[which(messages.df$setup == "4"),]
p <- nonparametric_dependent_analysis(temp.df$time, temp.df$name,
                                      temp.df$user, 6)
time_messages_setups[4,1] <- significance_level_star(p)

######################################################################################

time_messages <- data.frame(matrix(nrow = 6, ncol = 1))
colnames(time_messages) <- c("value")

temp.df <- messages.df[which(messages.df$name == "Message03"),]
p <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                        NULL, 4)
time_messages[1,1] <- significance_level_star(p)

temp.df <- messages.df[which(messages.df$name == "Message04"),]
p <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                        NULL, 4)
time_messages[2,1] <- significance_level_star(p)

temp.df <- messages.df[which(messages.df$name == "Message05"),]
p <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                        NULL, 4)
time_messages[3,1] <- significance_level_star(p)

temp.df <- messages.df[which(messages.df$name == "Message06"),]
p <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                        NULL, 4)
time_messages[4,1] <- significance_level_star(p)

temp.df <- messages.df[which(messages.df$name == "Message07"),]
p <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                        NULL, 4)
time_messages[5,1] <- significance_level_star(p)

temp.df <- messages.df[which(messages.df$name == "Message08"),]
p <- nonparametric_independent_analysis(temp.df$time, temp.df$setup,
                                        NULL, 4)
time_messages[6,1] <- significance_level_star(p)

##############################################################################


temp_message.df <- ddply(messages.df, .(setup, name), summarise, sd = sd(time), 
                         se = sd/sqrt(length(time)), value = mean(time))

# Define arc coordinates
r <- 0.3
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 20 * sin(t)

arc.df <- data.frame(setup = x, value = y)
rm(r, t, x, y)

lty = 6
label.df <- data.frame(setup = c(1, 2, 3, 4),
                       value = c(250, 250, 250, 250))

p1 <- ggplot(temp_message.df, aes(x = factor(setup), y = value)) +
  geom_bar(aes(fill = factor(name)), stat = "identity", position = "dodge") +
  geom_errorbar(aes(fill = factor(name), ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_text(data = label.df, size = 4,
            label = time_messages_setups$value) +
  geom_line(data = arc.df, aes(setup+1, value+240), lty = lty) +
  geom_line(data = arc.df, aes(setup+2, value+240), lty = lty) +
  geom_line(data = arc.df, aes(setup+3, value+240), lty = lty) +
  geom_line(data = arc.df, aes(setup+4, value+240), lty = lty) +
  scale_fill_brewer(guide = guide_legend(title = "Instructions", nrow = 1),
                    labels = c("2", "3", "4", "5", "6", "7")) +
  theme(legend.position = "bottom") + g_theme() +
  scale_x_discrete(labels = c("D", "C", "B", "A")) +
  labs(y = "Time (in seconds)", x = "Conditions", title = "(a) Group by Condition") 
#ggsave("Time/Charts/time_messages.eps", width = 8, height = 5)

# Define arc coordinates
r <- 0.3
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 20 * sin(t)

arc.df <- data.frame(name = x, value = y)
rm(r, t, x, y)

lty = 6
label.df <- data.frame(name = c(1, 2, 3, 4, 5, 6),
                       value = c(250, 125, 125, 175, 100, 250))

p2 <- ggplot(temp_message.df, aes(x = name, y = value)) +
  geom_bar(aes(fill = factor(setup)), stat = "identity", position = "dodge") +
  geom_errorbar(aes(fill = factor(setup), ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_text(data = label.df, size = 4,
            label = time_messages$value) +
  geom_line(data = arc.df, aes(name+1, value+240), lty = lty) +
  geom_line(data = arc.df, aes(name+2, value+115), lty = lty) +
  geom_line(data = arc.df, aes(name+3, value+115), lty = lty) +
  geom_line(data = arc.df, aes(name+4, value+165), lty = lty) +
  geom_line(data = arc.df, aes(name+5, value+90), lty = lty) +
  geom_line(data = arc.df, aes(name+6, value+240), lty = lty) +
  scale_fill_brewer(guide = guide_legend(title = "Instructions", nrow = 1),
                    labels = c("D", "C", "B", "A")) +
  theme(legend.position = "bottom") + g_theme() +
  scale_x_discrete(labels = c("2", "3", "4", "5", "6", "7")) +
  labs(y = "Time (in seconds)", x = "Instructions", title = "(b) Group by Instruction") 
#ggsave("Time/Charts/time_mes_setups.eps", width = 8, height = 5)

g <- grid.arrange(arrangeGrob(p1, p2, nrow = 1))

ggsave("Experiment II/Time/Charts/time_instructions.eps", g, width = 10, height = 5)

rm(arc.df, label.df, lty, p, temp.df, messages.df, temp_message.df, p1, p2, g)

rm(time.df, time_data.df, statistics.result)

rm(time_temp.df, temp_level.df)