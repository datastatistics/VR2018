source("Common_files/load_packages.R")
source("Common_files/statistics_analysis.R")
source("Common_files/utils.R")

data.df <- read.csv("Experiment II/Knowledge/Data/knowledge_data.csv", sep = ";")

score.df <- data.frame(rep(data.df$user, each = 3),
                       rep(data.df$setup, each = 3),
                        rep(c("before", "after", "three_week"), each = 1), 
                        matrix(nrow = length(data.df$user) * 3, ncol = 1))
colnames(score.df) <- c("user", "setup", "moment", "value")

x = 1
for (i in 1:length(data.df$user)){
  score.df$value[x] = data.df$before[i]
  score.df$value[x+1] = data.df$after[i]
  score.df$value[x+2] = data.df$three_week[i]
  x = x + 3
}

rm(i,x)

shapiro.test(score.df$value)

####################################################################################
########### pre -test ###############################################################
####################################################################################
temp_result.df <- score.df[which(score.df$moment == "before"),]
result <- nonparametric_independent_analysis(temp_result.df$value, 
                                    temp_result.df$setup,
                                    NULL, 4)

knowledge_after <- data.frame(c("1", "2", "3", "4"),
                                matrix(nrow = 4, ncol = 2))
colnames(knowledge_after) <- c("setup", "p-value", "star")

temp_score.df <- score.df[which(score.df$moment != "before"),]
temp_result.df <- temp_score.df[which(temp_score.df$setup == "1"),]
knowledge_after[1,2] <- nonparametric_dependent_analysis(temp_result.df$value,
                                                    temp_result.df$moment,
                                                    temp_result.df$user, 2)
knowledge_after[1,3] <- significance_level_star(knowledge_after[1,2])

temp_result.df <- temp_score.df[which(temp_score.df$setup == "2"),]
knowledge_after[2,2] <- nonparametric_dependent_analysis(temp_result.df$value,
                                                    temp_result.df$moment,
                                                    temp_result.df$user, 2)
knowledge_after[2,3] <- significance_level_star(knowledge_after[2,2])

temp_result.df <- temp_score.df[which(temp_score.df$setup == "3"),]
knowledge_after[3,2] <- nonparametric_dependent_analysis(temp_result.df$value,
                                                    temp_result.df$moment,
                                                    temp_result.df$user, 2)
knowledge_after[3,3] <- significance_level_star(knowledge_after[3,2])

temp_result.df <- temp_score.df[which(temp_score.df$setup == "4"),]
knowledge_after[4,2] <- nonparametric_dependent_analysis(temp_result.df$value,
                                                    temp_result.df$moment,
                                                    temp_result.df$user, 2)
knowledge_after[4,3] <- significance_level_star(knowledge_after[4,2])

#######################################################################################

knowledge_all <- data.frame(c("1", "2", "3", "4"),
                              matrix(nrow = 4, ncol = 2))
colnames(knowledge_all) <- c("setup", "p-value", "star")

temp_result.df <- score.df[which(score.df$setup == "1"),]
knowledge_all[1,2] <- nonparametric_dependent_analysis(temp_result.df$value,
                                                         temp_result.df$moment,
                                                         temp_result.df$user, 3)
knowledge_all[1,3] <- significance_level_star(knowledge_all[1,2])

temp_result.df <- score.df[which(score.df$setup == "2"),]
knowledge_all[2,2] <- nonparametric_dependent_analysis(temp_result.df$value,
                                                         temp_result.df$moment,
                                                         temp_result.df$user, 3)
knowledge_all[2,3] <- significance_level_star(knowledge_all[2,2])

temp_result.df <- score.df[which(score.df$setup == "3"),]
knowledge_all[3,2] <- nonparametric_dependent_analysis(temp_result.df$value,
                                                         temp_result.df$moment,
                                                         temp_result.df$user, 3)
knowledge_all[3,3] <- significance_level_star(knowledge_all[3,2])

temp_result.df <- score.df[which(score.df$setup == "4"),]
knowledge_all[4,2] <- nonparametric_dependent_analysis(temp_result.df$value,
                                                         temp_result.df$moment,
                                                         temp_result.df$user, 3)
knowledge_all[4,3] <- significance_level_star(knowledge_all[4,2])

rm(temp_result.df)

########################################################################################
##################### between before and after ########################################
########################################################################################

drops <- "user"
melted.df <- score.df[ , !(names(score.df) %in% drops)]
rm(drops)

melted.df <- melt(melted.df, 
                  id.vars = c("setup", "moment"))

#reorder data by moment that ssq was applied in the experiment
foo = rep(0, nrow(melted.df))

foo[with(melted.df, moment == "before")] = 1
foo[with(melted.df, moment == "after")] = 2
foo[with(melted.df, moment == "three_week")] = 3

melted.df$moment = with(melted.df, reorder(moment, foo))

rm(foo)

melted.df <- ddply(melted.df, .(setup, moment, variable), summarise,
                   sd = sd(value), se = sd/sqrt(length(value)),
                   value = mean(value))

# Define arc coordinates
r <- 0.3
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 0.2 * sin(t)

arc.df <- data.frame(setup = x, value = y)
rm(r, t, x, y)

lty = 6
label.df <- data.frame(setup = c(1, 2, 3, 4),
                       value = c(1, 1, 1, 1))
ggplot(melted.df, aes(x = factor(setup), y = value)) +
  geom_bar(aes(fill = factor(moment)), stat="identity", position = position_dodge()) +
  geom_errorbar(aes(fill = factor(moment), ymin=value-se, ymax=value+se), 
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  labs(y = "Correct Answers Rate", x = "Condition", title = "Knowledge") +
  geom_text(data = label.df, size = 5,
            label = knowledge_all$star) +
  geom_line(data = arc.df, aes(setup+1, value+0.9), lty = lty) +
  geom_line(data = arc.df, aes(setup+2, value+0.9), lty = lty) +
  geom_line(data = arc.df, aes(setup+3, value+0.9), lty = lty) +
  geom_line(data = arc.df, aes(setup+4, value+0.9), lty = lty) +
  scale_y_continuous(limits = c(0,1), labels = percent) +
  theme(legend.position = "bottom",
        title = element_text(size = 14),
        axis.text = element_text(size=14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14)) + 
  g_theme() +
  scale_fill_brewer(guide = guide_legend(title = "Time of Measurement"),
                  labels = c("Before", "After", "3 Week After")) +
  scale_x_discrete(labels = c("A", "B", "C", "D")) +
  ggsave("Experiment II/Knowledge/Charts/knowledge_before_after.eps", width = 6, height = 5)


########################################################################################
############# between all setups #######################################################
########################################################################################

range.df <- ddply(data.df, .(user, setup), summarise, value = three_week - after)

knowledge_setups_after <- data.frame(c("int_cross", "int_hands", "loc_joy", "loc_wip"),
                           matrix(nrow = , ncol = 2))
colnames(knowledge_setups_after) <- c("setup", "p-value", "star")

############################################################################################
#temp_score.df <- score.df[which(score.df$moment == "after"),]
setup.df <- range.df[which(range.df$setup == "1" | range.df$setup == "2"),]
knowledge_setups_after[1,2] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup, NULL, 2)
knowledge_setups_after[1,3] <- significance_level_star(knowledge_setups_after[1,2])

setup.df <- range.df[which(range.df$setup == "3" | range.df$setup == "4"),]
knowledge_setups_after[2,2] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup, NULL, 2)
knowledge_setups_after[2,3] <- significance_level_star(knowledge_setups_after[2,2])

setup.df <- range.df[which(range.df$setup == "1" | range.df$setup == "3"),]
knowledge_setups_after[3,2] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup, NULL, 2)
knowledge_setups_after[3,3] <- significance_level_star(knowledge_setups_after[3,2])

setup.df <- range.df[which(range.df$setup == "2" | range.df$setup == "4"),]
knowledge_setups_after[4,2] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup, NULL, 2)
knowledge_setups_after[4,3] <- significance_level_star(knowledge_setups_after[4,2])

###########

knowledge_setups_3week <- data.frame(c("int_cross", "int_hands", "loc_joy", "loc_wip"),
                                     matrix(nrow = , ncol = 2))
colnames(knowledge_setups_3week) <- c("setup", "p-value", "star")

############################################################################################
temp_score.df <- score.df[which(score.df$moment == "three_week"),]
setup.df <- temp_score.df[which(temp_score.df$setup == "1" | temp_score.df$setup == "2"),]
knowledge_setups_3week[1,2] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup, NULL, 2)
knowledge_setups_3week[1,3] <- significance_level_star(knowledge_setups_3week[1,2])

setup.df <- temp_score.df[which(temp_score.df$setup == "3" | temp_score.df$setup == "4"),]
knowledge_setups_3week[2,2] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup, NULL, 2)
knowledge_setups_3week[2,3] <- significance_level_star(knowledge_setups_3week[2,2])

setup.df <- temp_score.df[which(temp_score.df$setup == "1" | temp_score.df$setup == "3"),]
knowledge_setups_3week[3,2] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup, NULL, 2)
knowledge_setups_3week[3,3] <- significance_level_star(knowledge_setups_3week[3,2])

setup.df <- temp_score.df[which(temp_score.df$setup == "2" | temp_score.df$setup == "4"),]
knowledge_setups_3week[4,2] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup, NULL, 2)
knowledge_setups_3week[4,3] <- significance_level_star(knowledge_setups_3week[4,2])


range.df <- ddply(data.df, .(user, setup), summarise, value = after - before)

melted.df <- ddply(score.df[which(score.df$moment == "after"),], 
                   .(setup), summarise, sd = sd(value), 
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
  theme(legend.position = "top") + g_theme() +
  labs(y = "Score", x = "Locomotion", title = "Knowledge") +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  ggsave("Experiment II/Knowledge/Charts/knowledge_score.eps", width = 6, height = 5)


##########################################################################
after.df <- ddply(score.df[which(score.df$moment == "three_week"),], 
                  .(setup), summarise, sd = sd(value), 
                   se = sd/sqrt(length(value)), value = mean(value))

after.df[after.df == "1"] = "Joystick"
after.df[after.df == "2"] = "WIP"
after.df[after.df == "3"] = "Joystick"
after.df[after.df == "4"] = "WIP"

after.df$int <- c("Crosshair", "Crosshair", "Pointing Finger", "Pointing Finger")

ggplot(after.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Head orientation", "Ray-casting")) +
  theme(legend.position = "top") + g_theme() +
  labs(y = "Score", x = "Locomotion", title = "Knowledge") +
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  ggsave("Experiment II/Knowledge/Charts/knowledge_after.eps", width = 6, height = 5)

rm(data.df, result, score.df, melted.df, range.df, label.df, arc.df, lty, after.df, temp_score.df)
