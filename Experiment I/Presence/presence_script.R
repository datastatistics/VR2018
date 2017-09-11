source("Common_files/load_packages.R")
source("Common_files/statistics_analysis.R")
source("Common_files/utils.R")

original.df <- read.csv("Experiment I/Presence/Data/presence.csv", sep = ';')

drops <- "immersion"
presence.df <- original.df[ , !(names(original.df) %in% drops)]
presence.df <- melt(presence.df, id.vars = c("user", "output"))

presence.df$variable = gsub("1", "", presence.df$variable)
presence.df$variable = gsub("2", "", presence.df$variable)
presence.df$variable = gsub("3", "", presence.df$variable)
presence.df$variable = gsub("4", "", presence.df$variable)
presence.df$variable = gsub("5", "", presence.df$variable)
presence.df$variable = gsub("6", "", presence.df$variable)

melted.df <- ddply(presence.df, .(user, output, variable), summarise, mean = mean(value))

foo = rep(0, nrow(melted.df))

foo[with(melted.df, variable == "rec")] = 1
foo[with(melted.df, variable == "est")] = 2
foo[with(melted.df, variable == "coz")] = 3
foo[with(melted.df, variable == "esc")] = 4

melted.df$variable = with(melted.df, reorder(variable, foo))

###############################################################################################
########### statistics ########################################################################
###############################################################################################
presence_setups <- data.frame(c("rec", "est", "coz", "esc"),
                              matrix(nrow = 4, ncol = 2))
colnames(presence_setups) <- c("place", "p-value", "star")

shapiro.test(melted.df$mean)

temp.df <- melted.df[which(melted.df$variable == "rec"),]
presence_setups[1,2] <- nonparametric_independent_analysis(temp.df$mean, temp.df$output, temp.df$user, 3)
presence_setups[1,3] <- significance_level_star(presence_setups[1,2])

temp.df <- melted.df[which(melted.df$variable == "est"),]
presence_setups[2,2] <- nonparametric_independent_analysis(temp.df$mean, temp.df$output, temp.df$user, 3)
presence_setups[2,3] <- significance_level_star(presence_setups[2,2])

temp.df <- melted.df[which(melted.df$variable == "coz"),]
presence_setups[3,2] <- nonparametric_independent_analysis(temp.df$mean, temp.df$output, temp.df$user, 3)
presence_setups[3,3] <- significance_level_star(presence_setups[3,2])

temp.df <- melted.df[which(melted.df$variable == "esc"),]
presence_setups[4,2] <- nonparametric_independent_analysis(temp.df$mean, temp.df$output, temp.df$user, 3)
presence_setups[4,3] <- significance_level_star(presence_setups[4,2])

##############################################################################################

presence_places <- data.frame(c("Monitor", "Display Wall", "Oculus Rift"),
                              matrix(nrow = 3, ncol = 2))
colnames(presence_places) <- c("output", "p-value", "star")

temp.df <- melted.df[which(melted.df$output == "Monitor"),]
presence_places[1,2] <- nonparametric_dependent_analysis(temp.df$mean, temp.df$variable, temp.df$user, 4)
presence_places[1,3] <- significance_level_star(presence_places[1,2])

temp.df <- melted.df[which(melted.df$output == "DisplayWall"),]
presence_places[2,2] <- nonparametric_dependent_analysis(temp.df$mean, temp.df$variable, temp.df$user, 4)
presence_places[2,3] <- significance_level_star(presence_places[2,2])

temp.df <- melted.df[which(melted.df$output == "Oculus Rift"),]
presence_places[3,2] <- nonparametric_dependent_analysis(temp.df$mean, temp.df$variable, temp.df$user, 4)
presence_places[3,3] <- significance_level_star(presence_places[3,2])

rm(temp.df)
###############################################################################################
drops <- "user"
melted.df <- presence.df[ , !(names(presence.df) %in% drops)]
melted.df <- ddply(presence.df, .(output, variable), summarise, sd = sd(value), 
                   se = sd/sqrt(length(value)), value = mean(value))

foo = rep(0, nrow(melted.df))

foo[with(melted.df, variable == "rec")] = 1
foo[with(melted.df, variable == "est")] = 2
foo[with(melted.df, variable == "coz")] = 3
foo[with(melted.df, variable == "esc")] = 4

melted.df$variable = with(melted.df, reorder(variable, foo))

foo = rep(0, nrow(melted.df))

foo[with(melted.df, output == "Monitor")] = 1
foo[with(melted.df, output == "DisplayWall")] = 2
foo[with(melted.df, output == "Oculus Rift")] = 3

melted.df$output = with(melted.df, reorder(output, foo))

rm(foo)

# Define arc coordinates
r <- 0.25
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 0.7 * sin(t)

arc.df <- data.frame(variable = x, value = y)

######################################################################################################################
#bar plots

lty = 6
label.df <- data.frame(variable = c(1, 2, 3, 4),
                       value = c(6, 6, 6, 6))
p1 <- ggplot(melted.df, aes(x = variable, y = value)) +
  geom_bar(aes(fill = output), stat="identity", position = position_dodge()) +
  geom_errorbar(aes(fill = output, ymin=value-se, ymax=value+se), stat = "identity", 
                width = .1, position=position_dodge(width=0.9)) +
  labs(y = "Score", x = "Places", fill = "", title = "") +
  geom_text(data = label.df, size = 4,
            label = presence_setups$star) +
  geom_line(data = arc.df, aes(variable+1, value+5.7), lty = lty) +
  geom_line(data = arc.df, aes(variable+2, value+5.7), lty = lty) +
  geom_line(data = arc.df, aes(variable+3, value+5.7), lty = lty) +
  geom_line(data = arc.df, aes(variable+4, value+5.7), lty = lty) +
  scale_y_continuous(limits = c(0, 6)) +
  scale_fill_brewer(guide = guide_legend(title = "VR Displays"), 
                    labels = c("Desktop", "Display-Wall", "HMD")) +
  scale_x_discrete(labels = c("Reception", "Parking Lot", "Kitchen", "Office")) +
  theme(legend.position = "bottom") + g_theme() +
  ggsave("Experiment I/Presence/Charts/presence.eps", width = 7, height = 5)

#############################################################################################

# Define arc coordinates
r <- 0.3
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 0.7 * sin(t)

arc.df <- data.frame(output = x, value = y)

######################################################################################################################
#bar plots

lty = 6
label.df <- data.frame(output = c(1, 2, 3),
                       value = c(5, 5.5, 6))
p2 <- ggplot(melted.df, aes(x = output, y = value)) +
  geom_bar(aes(fill = variable), stat="identity", position = position_dodge()) +
  geom_errorbar(aes(fill = variable, ymin=value-se, ymax=value+se), stat = "identity", 
                width = .1, position=position_dodge(width=0.9)) +
  labs(y = "Score", x = "VR Displays", fill = "", title = "(b)") +
  geom_text(data = label.df, size = 4,
            label = presence_places$star) +
  geom_line(data = arc.df, aes(output+1, value+4.5), lty = lty) +
  geom_line(data = arc.df, aes(output+2, value+5), lty = lty) +
  geom_line(data = arc.df, aes(output+3, value+5.6), lty = lty) +
  scale_y_continuous(limits = c(0, 7)) +
  scale_fill_brewer(guide = guide_legend(title = "Places"), 
                 labels = c("Reception", "Parking Lot", "Kitchen", "Office")) +
  scale_x_discrete(labels = c("Desktop", "Display-Wall", "HMD")) +
  theme(legend.position = "bottom") + g_theme()

g <- grid.arrange(arrangeGrob(p1, p2, ncol=2))

#ggsave("Presence/Charts/presence.eps", g, width = 8, height = 5)

##############################################################################################
################ immersion ####################################################################
###############################################################################################

immersion.df <- data.frame(original.df$output, original.df$immersion)
colnames(immersion.df) <- c("output", "value")

p_value <- nonparametric_independent_analysis(immersion.df$value, immersion.df$output, NULL, 3)
star <- significance_level_star(p_value)

melted.df <- ddply(presence.df, .(output), summarise, sd = sd(value), 
                   se = sd/sqrt(length(value)), value = mean(value))

foo = rep(0, nrow(melted.df))

foo[with(melted.df, output == "Monitor")] = 1
foo[with(melted.df, output == "DisplayWall")] = 2
foo[with(melted.df, output == "Oculus Rift")] = 3

melted.df$output = with(melted.df, reorder(output, foo))

rm(foo)

# Define arc coordinates
r <- 1
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 0.3 * sin(t)

arc.df <- data.frame(output = x, value = y)

label.df <- data.frame(output = c(2),
                       value = c(5.4))
ggplot(melted.df, aes(x = output, y = value)) +
  geom_bar(aes(fill = output), stat="identity", position = position_dodge()) +
  geom_errorbar(aes(fill = output, ymin=value-se, ymax=value+se), stat = "identity", 
                width = .1, position=position_dodge(width=0.9)) +
  labs(y = "Score", x = "VR Displays", fill = "", title = "How much immersed did you feel?") +
  geom_text(data = label.df, size = 4,
            label = star) +
  scale_fill_brewer() +
  geom_line(data = arc.df, aes(output+2, value+5), lty = lty) +
  #scale_y_continuous(limits = c(0, 10)) +
  scale_x_discrete(labels = c("Desktop", "Display-Wall", "HMD")) +
  theme(legend.position = "none") + g_theme() +
  ggsave("Experiment I/Presence/Charts/immersion.eps", width = 8, height = 5)
