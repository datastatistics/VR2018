source("Common_files/load_packages.R")
source("Common_files/statistics_analysis.R")
source("Common_files/utils.R")

eng_data.df <- read.csv("Experiment II/Engagement/Data/engagement_data.csv", sep = ";")
eng_data.df <- melt(eng_data.df, id.vars = c("user", "setup"))

melted.df <- ddply(eng_data.df, .(user, setup), summarise, value = mean(value))

engagement <- data.frame(c("int_cross", "int_hand", "loc_joy", "loc_wip"),
                  matrix(nrow = 4, ncol = 2))

colnames(engagement) <- c("setup", "value", "star")

shapiro.test(melted.df$value)

setup.df <- eng_data.df[which(eng_data.df$setup == "1" | eng_data.df$setup == "2"),]
engagement[1,2] <- parametric_independent_analysis(setup.df$value, setup.df$setup,
                                            NULL, 2)
engagement[1,3] <- significance_level_star(engagement[1,2])

setup.df <- eng_data.df[which(eng_data.df$setup == "3" | eng_data.df$setup == "4"),]
engagement[2,2] <- parametric_independent_analysis(setup.df$value, setup.df$setup,
                                            NULL, 2)
engagement[2,3] <- significance_level_star(engagement[2,2])

setup.df <- eng_data.df[which(eng_data.df$setup == "1" | eng_data.df$setup == "3"),]
engagement[3,2] <- parametric_independent_analysis(setup.df$value, setup.df$setup,
                                            NULL, 2)
engagement[3,3] <- significance_level_star(engagement[3,2])

setup.df <- eng_data.df[which(eng_data.df$setup == "2" | eng_data.df$setup == "4"),]
engagement[4,2] <- parametric_independent_analysis(setup.df$value, setup.df$setup,
                                            NULL, 2)
engagement[4,3] <- significance_level_star(engagement[4,2])

#######################################################################################

melted.df <- ddply(melted.df, .(setup), summarise, sd = sd(value), 
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
  scale_x_discrete(labels = c("Joystick", "WIP")) +
  labs(y = "Score", x = "Locomotion", title = "Game Engagement") +
  ggsave("Experiment II/Engagement/Charts/engagement.eps", width = 6, height = 5)


rm(melted.df, eng_data.df, setup.df)