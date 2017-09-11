source("Common_files/load_packages.R")
source("Common_files/statistics_analysis.R")
source("Common_files/utils.R")
source("Common_files/ssq_score.R")

ssq.df <- read.csv("Experiment II/SSQ/Data/ssq_data.csv", sep = ';')

#############################################################################################################
########### SSQ SCORES ######################################################################################
#############################################################################################################
result.df <- calculate_ssq_scores(ssq.df, "setup")

range.df <- ddply(result.df, .(user, setup), summarise,
                  n = an - bn, o = ao - bo, d = ad - bd, ts = ats - bts)

#####################################################################################################
drops <- "user"
range.df <- range.df[ , !(names(range.df) %in% drops)]

range.df <- melt(range.df, 
                  id.vars = "setup")

melted.df <- ddply(range.df, .(setup, variable), summarise,
                   sd = sd(value), se = sd/sqrt(length(value)),
                   value = mean(value))

melted.df[melted.df == "1"] = "Joystick"
melted.df[melted.df == "2"] = "WIP"
melted.df[melted.df == "3"] = "Joystick"
melted.df[melted.df == "4"] = "WIP"

melted.df$int <- c("Crosshair", "Crosshair", "Crosshair", "Crosshair",
                  "Crosshair", "Crosshair", "Crosshair", "Crosshair",
                  "Pointing Finger", "Pointing Finger", "Pointing Finger", "Pointing Finger",
                  "Pointing Finger", "Pointing Finger", "Pointing Finger", "Pointing Finger")

temp_range.df <- melted.df[which(melted.df$variable == "n"),]

p1 <- ggplot(temp_range.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Crosshair", "Pointing Finger")) +
  theme(legend.position = "none") + g_theme() +
  scale_y_continuous(limits = c(0, 70)) +
  scale_x_discrete(labels = c("Low", "High")) +
  labs(y = "Score", x = "", title = "Nausea")

temp_range.df <- melted.df[which(melted.df$variable == "o"),]

p2 <- ggplot(temp_range.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Crosshair", "Pointing Finger")) +
  theme(legend.position = "none") + g_theme() +
  scale_y_continuous(limits = c(0, 70)) +
  scale_x_discrete(labels = c("Low", "High")) +
  labs(y = "Score", x = "", title = "Oculomotor")

temp_range.df <- melted.df[which(melted.df$variable == "d"),]


p3 <- ggplot(temp_range.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Crosshair", "Pointing Finger")) +
  theme(legend.position = "none") + g_theme() +
  scale_y_continuous(limits = c(0, 70)) +
  scale_x_discrete(labels = c("Low", "High")) +
  labs(y = "Score", x = "Locomotion", title = "Disorientation")

temp_range.df <- melted.df[which(melted.df$variable == "ts"),]

p4 <- ggplot(temp_range.df, aes(x = setup, y = value)) +
  geom_point() +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Crosshair", "Pointing Finger")) +
  theme(legend.position = "none") + g_theme() +
  scale_y_continuous(limits = c(0, 70)) +
  scale_x_discrete(labels = c("Low", "High")) +
  labs(y = "Score", x = "Locomotion", title = "Total Score")

p1.leg <- ggplot(temp_range.df, aes(x = setup, y = value)) +
  geom_line(aes(group = int, color = factor(int)), size = 1) +
  scale_color_hue(guide = guide_legend(title = "Interaction"),
                  labels = c("Low", "High")) +
  theme(legend.position = "top") +
  g_theme()

leg_1 <- g_legend(p1.leg)

g <- grid.arrange(arrangeGrob(p1, p2, nrow = 1),
                  arrangeGrob(p3, p4, nrow = 1),
                  leg_1, nrow=3,heights=c(2, 2, 0.2))


ggsave("Experiment II/SSQ/Charts/ssq_score.eps", g, width = 8, height = 5)

###########################################################################################
######## analysis #########################################################################
###########################################################################################

ssq_between_setups <- data.frame(c("int_cross", "int_hand", "loc_joy", "loc_wip"),
                                 rep(c("n", "o", "d", "ts"), each = 4),
                                 matrix(nrow = 16, ncol = 2))
colnames(ssq_between_setups) <- c("setup", "scale", "p-value", "star")

setup.df <- range.df[which((range.df$setup == "1" | range.df$setup == "2") & range.df$variable == "n"),]
ssq_between_setups[1,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                              NULL, 2)
ssq_between_setups[1,4] <- significance_level_star(ssq_between_setups[1,3])

setup.df <- range.df[which((range.df$setup == "3" | range.df$setup == "4") & range.df$variable == "n"),]
ssq_between_setups[2,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                              NULL, 2)
ssq_between_setups[2,4] <- significance_level_star(ssq_between_setups[2,3])

setup.df <- range.df[which((range.df$setup == "1" | range.df$setup == "3") & range.df$variable == "n"),]
ssq_between_setups[3,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                              NULL, 2)
ssq_between_setups[3,4] <- significance_level_star(ssq_between_setups[3,3])

setup.df <- range.df[which((range.df$setup == "2" | range.df$setup == "4") & range.df$variable == "n"),]
ssq_between_setups[4,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                              NULL, 2)
ssq_between_setups[4,4] <- significance_level_star(ssq_between_setups[4,3])

###########################################################################################

setup.df <- range.df[which((range.df$setup == "1" | range.df$setup == "2") & range.df$variable == "o"),]
ssq_between_setups[5,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                              NULL, 2)
ssq_between_setups[5,4] <- significance_level_star(ssq_between_setups[5,3])

setup.df <- range.df[which((range.df$setup == "3" | range.df$setup == "4") & range.df$variable == "o"),]
ssq_between_setups[6,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                              NULL, 2)
ssq_between_setups[6,4] <- significance_level_star(ssq_between_setups[6,3])

setup.df <- range.df[which((range.df$setup == "1" | range.df$setup == "3") & range.df$variable == "o"),]
ssq_between_setups[7,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                              NULL, 2)
ssq_between_setups[7,4] <- significance_level_star(ssq_between_setups[7,3])

setup.df <- range.df[which((range.df$setup == "2" | range.df$setup == "4") & range.df$variable == "o"),]
ssq_between_setups[8,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                              NULL, 2)
ssq_between_setups[8,4] <- significance_level_star(ssq_between_setups[8,3])

###########################################################################################

setup.df <- range.df[which((range.df$setup == "1" | range.df$setup == "2") & range.df$variable == "d"),]
ssq_between_setups[9,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                              NULL, 2)
ssq_between_setups[9,4] <- significance_level_star(ssq_between_setups[9,3])

setup.df <- range.df[which((range.df$setup == "3" | range.df$setup == "4") & range.df$variable == "d"),]
ssq_between_setups[10,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                               NULL, 2)
ssq_between_setups[10,4] <- significance_level_star(ssq_between_setups[10,3])

setup.df <- range.df[which((range.df$setup == "1" | range.df$setup == "3") & range.df$variable == "d"),]
ssq_between_setups[11,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                               NULL, 2)
ssq_between_setups[11,4] <- significance_level_star(ssq_between_setups[11,3])

setup.df <- range.df[which((range.df$setup == "2" | range.df$setup == "4") & range.df$variable == "d"),]
ssq_between_setups[12,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                               NULL, 2)
ssq_between_setups[12,4] <- significance_level_star(ssq_between_setups[12,3])

###########################################################################################

setup.df <- range.df[which((range.df$setup == "1" | range.df$setup == "2") & range.df$variable == "ts"),]
ssq_between_setups[13,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                               NULL, 2)
ssq_between_setups[13,4] <- significance_level_star(ssq_between_setups[13,3])

setup.df <- range.df[which((range.df$setup == "3" | range.df$setup == "4") & range.df$variable == "ts"),]
ssq_between_setups[14,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                               NULL, 2)
ssq_between_setups[14,4] <- significance_level_star(ssq_between_setups[14,3])

setup.df <- range.df[which((range.df$setup == "1" | range.df$setup == "3") & range.df$variable == "ts"),]
ssq_between_setups[15,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                               NULL, 2)
ssq_between_setups[15,4] <- significance_level_star(ssq_between_setups[15,3])

setup.df <- range.df[which((range.df$setup == "2" | range.df$setup == "4") & range.df$variable == "ts"),]
ssq_between_setups[16,3] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                               NULL, 2)
ssq_between_setups[16,4] <- significance_level_star(ssq_between_setups[16,3])

#########################################################################################
### ssq variation among setups ########################################################

setup.df <- range.df[which(range.df$variable == "n"),]
statistics.result[1,1] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                               NULL, 4)
statistics.result[1,1] <- significance_level_star(statistics.result[1,1])

setup.df <- range.df[which(range.df$variable == "o"),]
statistics.result[2,1] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                              NULL, 4)
statistics.result[2,1] <- significance_level_star(statistics.result[2,1])

setup.df <- range.df[which(range.df$variable == "d"),]
statistics.result[3,1] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                              NULL, 4)
statistics.result[3,1] <- significance_level_star(statistics.result[3,1])

setup.df <- range.df[which(range.df$variable == "ts"),]
statistics.result[4,1] <- nonparametric_independent_analysis(setup.df$value, setup.df$setup,
                                                              NULL, 4)
statistics.result[4,1] <- significance_level_star(statistics.result[4,1])

range.df <- ddply(result.df, .(user, setup), summarise,
                  n = an - bn, o = ao - bo, d = ad - bd, ts = ats - bts)

drops <- "user"
melted.df <- range.df[ , !(names(range.df) %in% drops)]

melted.df <- melt(melted.df, 
                  id.vars = "setup")

melted.df <- ddply(melted.df, .(setup, variable), summarise,
                   sd = sd(value), se = sd/sqrt(length(value)),
                   value = mean(value))

r <- 0.3
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 5 * sin(t)

arc.df <- data.frame(variable = x, value = y)

lty = 6

drops <- "user"
boxplot.df <- range.df[ , !(names(range.df) %in% drops)]
rm(drops)

boxplot.df <- melt(boxplot.df, 
                   id.vars = "setup")

boxplot.df[boxplot.df == "1"] = "D"
boxplot.df[boxplot.df == "2"] = "C"
boxplot.df[boxplot.df == "3"] = "B"
boxplot.df[boxplot.df == "4"] = "A"

label.df <- data.frame(variable = c(1, 2, 3, 4),
                       value = c(104, 48, 125, 107))

ggplot(boxplot.df, aes(variable, value)) + 
  geom_boxplot(aes(fill = factor(setup)), position = "dodge") +
  geom_text(data = label.df, size = 4, label = statistics.result$value) +
  geom_line(data = arc.df, aes(variable+1, value+98), lty = lty) +
  geom_line(data = arc.df, aes(variable+2, value+42), lty = lty) +
  geom_line(data = arc.df, aes(variable+3, value+120), lty = lty) +
  geom_line(data = arc.df, aes(variable+4, value+101), lty = lty) +
  scale_fill_hue(guide = guide_legend(title = "Setup")) + 
                # labels = c("A", "B", "C", "D")) +
  theme(legend.position = "bottom",
        title = element_text(size = 14),
        axis.text = element_text(size=14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14)) +
  labs(y = "Score", x = "Scales", fill = "", title = "Simulator Sickness") +
  ggsave("Experiment II/SSQ/Charts/ssq_variation.eps", width = 6, height = 5)

######################################################################################
############### ssq measurement time


temp.df <- calculate_ssq_scores(ssq.df, "setup")


users_count <- length(ssq.df$user)

#transforrmar a tabela em data frame com o momento sendo uma coluna

result.df <- data.frame(rep(ssq.df$user, each = 2),
                        rep(c("before", "after"), each = 1), 
                        rep(ssq.df$setup, each = 2),
                        matrix(nrow = users_count, ncol = 4))

result.df <- result.df[1:(users_count * 2),]
colnames(result.df) <- c("user", "moment", "setup", "n", "o", "d", "ts")

x = 1
for (i in 1:length(temp.df$user)){
  result.df$n[x] = temp.df$bn[i]
  result.df$n[x+1] = temp.df$an[i]
  result.df$o[x] = temp.df$bo[i]
  result.df$o[x+1] = temp.df$ao[i]
  result.df$d[x] = temp.df$bd[i]
  result.df$d[x+1] = temp.df$ad[i]
  result.df$ts[x] = temp.df$bts[i]
  result.df$ts[x+1] = temp.df$ats[i]
  x = x + 2
}

rm(i,x)

#############################################################################################################
########### before ###############################################################################
#############################################################################################################
temp_result.df <- result.df[which(result.df$moment == "before"),]
res = shapiro.test(temp_result.df$n)
print(res)
res = shapiro.test(temp_result.df$o)
print(res)
res = shapiro.test(temp_result.df$d)
print(res)
res = shapiro.test(temp_result.df$ts)
print(res)

result <- nonparametric_independent_analysis(temp_result.df$n,
                                             temp_result.df$setup,
                                             NULL, 4)

result <- nonparametric_independent_analysis(temp_result.df$o,
                                             temp_result.df$setup,
                                             NULL, 4)

result <- nonparametric_independent_analysis(temp_result.df$d,
                                             temp_result.df$setup,
                                             NULL, 4)

result <- nonparametric_independent_analysis(temp_result.df$ts,
                                             temp_result.df$setup,
                                             NULL, 4)

drops <- "user"
temp_result.df <- temp_result.df[ , !(names(temp_result.df) %in% drops)]
temp_result.df = ddply(temp_result.df, .(moment), summarise, mean_n = mean(n), sdn = sd(n),
                       mean_o = mean(o), sdo = sd(o), mean_d = mean(d), sdd = sd(d), mean_ts = mean(ts), sdts = sd(ts))


#############################################################################################################
########### VARIANCE ANALYSIS ###############################################################################
#############################################################################################################
ssq_moment <- data.frame(c("1", "2", "3", "4"),
                         matrix(nrow = 4, ncol = 8))
colnames(ssq_moment) <- c("setup", "n", "nstar", "o", "ostar", "d", "dstar", "ts", "tsstar")

temp_result.df <- result.df[which(result.df$setup == "1"),]
ssq_moment[1,2] <- nonparametric_dependent_analysis(temp_result.df$n, temp_result.df$moment, NULL, 2)
ssq_moment[1,3] <- significance_level_star(ssq_moment[1,2])
ssq_moment[1,4] <- nonparametric_dependent_analysis(temp_result.df$o, temp_result.df$moment, NULL, 2)
ssq_moment[1,5] <- significance_level_star(ssq_moment[1,4])
ssq_moment[1,6] <- nonparametric_dependent_analysis(temp_result.df$d, temp_result.df$moment, NULL, 2)
ssq_moment[1,7] <- significance_level_star(ssq_moment[1,6])
ssq_moment[1,8] <- nonparametric_dependent_analysis(temp_result.df$ts, temp_result.df$moment, NULL, 2)
ssq_moment[1,9] <- significance_level_star(ssq_moment[1,8])

temp_result.df <- result.df[which(result.df$setup == "2"),]
ssq_moment[2,2] <- nonparametric_dependent_analysis(temp_result.df$n, temp_result.df$moment, NULL, 2)
ssq_moment[2,3] <- significance_level_star(ssq_moment[2,2])
ssq_moment[2,4] <- nonparametric_dependent_analysis(temp_result.df$o, temp_result.df$moment, NULL, 2)
ssq_moment[2,5] <- significance_level_star(ssq_moment[2,4])
ssq_moment[2,6] <- nonparametric_dependent_analysis(temp_result.df$d, temp_result.df$moment, NULL, 2)
ssq_moment[2,7] <- significance_level_star(ssq_moment[2,6])
ssq_moment[2,8] <- nonparametric_dependent_analysis(temp_result.df$ts, temp_result.df$moment, NULL, 2)
ssq_moment[2,9] <- significance_level_star(ssq_moment[2,8])

temp_result.df <- result.df[which(result.df$setup == "3"),]
ssq_moment[3,2] <- nonparametric_dependent_analysis(temp_result.df$n, temp_result.df$moment, NULL, 2)
ssq_moment[3,3] <- significance_level_star(ssq_moment[3,2])
ssq_moment[3,4] <- nonparametric_dependent_analysis(temp_result.df$o, temp_result.df$moment, NULL, 2)
ssq_moment[3,5] <- significance_level_star(ssq_moment[3,4])
ssq_moment[3,6] <- nonparametric_dependent_analysis(temp_result.df$d, temp_result.df$moment, NULL, 2)
ssq_moment[3,7] <- significance_level_star(ssq_moment[3,6])
ssq_moment[3,8] <- nonparametric_dependent_analysis(temp_result.df$ts, temp_result.df$moment, NULL, 2)
ssq_moment[3,9] <- significance_level_star(ssq_moment[3,8])

temp_result.df <- result.df[which(result.df$setup == "4"),]
ssq_moment[4,2] <- nonparametric_dependent_analysis(temp_result.df$n, temp_result.df$moment, NULL, 2)
ssq_moment[4,3] <- significance_level_star(ssq_moment[4,2])
ssq_moment[4,4] <- nonparametric_dependent_analysis(temp_result.df$o, temp_result.df$moment, NULL, 2)
ssq_moment[4,5] <- significance_level_star(ssq_moment[4,4])
ssq_moment[4,6] <- nonparametric_dependent_analysis(temp_result.df$d, temp_result.df$moment, NULL, 2)
ssq_moment[4,7] <- significance_level_star(ssq_moment[4,6])
ssq_moment[4,8] <- nonparametric_dependent_analysis(temp_result.df$ts, temp_result.df$moment, NULL, 2)
ssq_moment[4,9] <- significance_level_star(ssq_moment[4,8])

stars <- data.frame(ssq_moment$setup, ssq_moment$nstar, ssq_moment$ostar, ssq_moment$dstar, ssq_moment$tsstar)
colnames(stars) <- c("setup", "n", "o", "d", "ts")
stars <- melt(stars, id.vars = "setup")

rm(temp_result.df)

#############################################################################################################
########### CHARTS ##########################################################################################
#############################################################################################################

# Define arc coordinates
r <- 0.15
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 5 * sin(t)

arc.df <- data.frame(variable = x, value = y)

######################################################################################################################
#bar plots

drops <- "user"
boxplot.df <- result.df[ , !(names(result.df) %in% drops)]

boxplot.df <- melt(boxplot.df, 
                   id.vars = c("setup", "moment"))

boxplot.df <- ddply(boxplot.df, .(setup, moment, variable), summarise,
                    sd = sd(value), se = sd/sqrt(length(value)),
                    value = mean(value))

#reorder data by moment that ssq was applied in the experiment
foo = rep(0, nrow(boxplot.df))

foo[with(boxplot.df, moment == "before")] = 1
foo[with(boxplot.df, moment == "after")] = 2

boxplot.df$moment = with(boxplot.df, reorder(moment, foo))

rm(foo)

lty = 6

colnames(arc.df) <- c("setup", "value")

label.df <- data.frame(setup = c(1, 2, 3, 4),
                       value = c(35, 50, 50, 53))
p1 <- ggplot(boxplot.df[which(boxplot.df$variable == "n"),], 
             aes(x = factor(setup), y = value)) +
  geom_bar(aes(fill = moment), stat="identity", position = position_dodge()) +
  #geom_boxplot(aes(fill = moment), position = position_dodge()) +
  geom_errorbar(aes(fill = moment, ymin=value-se, ymax=value+se), stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  labs(y = "Score", x = "Condition", fill = "", title = "Nausea") +
  geom_text(data = label.df, size = 4,
            label = stars[which(stars$variable == "n"),]$value) +
  geom_line(data = arc.df, aes(setup+1, value+31), lty = lty) +
  geom_line(data = arc.df, aes(setup+2, value+48), lty = lty) +
  geom_line(data = arc.df, aes(setup+3, value+48), lty = lty) +
  geom_line(data = arc.df, aes(setup+4, value+51), lty = lty) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_discrete(labels = c("D", "C", "B", "A")) +
  scale_fill_brewer() +
  theme(legend.position = "none") + g_theme()
#ggsave("SSQ/ssq_monitor.png", width = 8, height = 5)

label.df <- data.frame(setup = c(1, 2, 3, 4),
                       value = c(30, 40, 50, 50))
p2 <- ggplot(boxplot.df[which(boxplot.df$variable == "o"),],
             aes(factor(setup), value)) +
  #geom_boxplot(aes(fill = moment), position = position_dodge()) +
  geom_bar(aes(fill = moment), stat="identity", position = "dodge") +
  geom_errorbar(aes(fill = moment, ymin=value-se, ymax=value+se), stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  labs(y = "Score", x = "Condition", fill = "", title = "Oculomotor") +
  geom_text(data = label.df, size = 4,
            label = stars[which(stars$variable == "o"),]$value) +
  geom_line(data = arc.df, aes(setup+1, value+26), lty = lty) +
  geom_line(data = arc.df, aes(setup+2, value+38), lty = lty) +
  geom_line(data = arc.df, aes(setup+3, value+48), lty = lty) +
  geom_line(data = arc.df, aes(setup+4, value+48), lty = lty) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_discrete(labels = c("D", "C", "B", "A")) +
  scale_fill_brewer() +
  theme(legend.position = "none") + g_theme() 
#ggsave("SSQ/ssq_oculus.png", width = 8, height = 5)

label.df <- data.frame(setup = c(1, 2, 3, 4),
                       value = c(58, 60, 80, 62))
p3 <- ggplot(boxplot.df[which(boxplot.df$variable == "d"),],
             aes(factor(setup), value)) +
  #geom_boxplot(aes(fill = moment), position = position_dodge()) +
  geom_bar(aes(fill = moment), stat="identity", position = "dodge") +
  geom_errorbar(aes(fill = moment, ymin=value-se, ymax=value+se), stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  labs(y = "Score", x = "Condition", fill = "", title = "Disorientation") +
  geom_text(data = label.df, size = 4,
            label = stars[which(stars$variable == "d"),]$value) +
  geom_line(data = arc.df, aes(setup+1, value+56), lty = lty) +
  geom_line(data = arc.df, aes(setup+2, value+58), lty = lty) +
  geom_line(data = arc.df, aes(setup+3, value+78), lty = lty) +
  geom_line(data = arc.df, aes(setup+4, value+60), lty = lty) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_discrete(labels = c("D", "C", "B", "A")) +
  scale_fill_brewer() +
  theme(legend.position = "none") + g_theme()
#ggsave("SSQ/ssq_displaywall.png", width = 8, height = 5)

label.df <- data.frame(setup = c(1, 2, 3, 4),
                       value = c(40, 52, 65, 62))

p4 <- ggplot(boxplot.df[which(boxplot.df$variable == "ts"),],
             aes(factor(setup), value)) +
  #geom_boxplot(aes(fill = moment), position = position_dodge()) +
  geom_bar(aes(fill = moment), stat="identity", position = "dodge") +
  geom_errorbar(aes(fill = moment, ymin=value-se, ymax=value+se), stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  labs(y = "Score", x = "Condition", fill = "", title = "Total Score") +
  geom_text(data = label.df, size = 4,
            label = stars[which(stars$variable == "ts"),]$value) +
  geom_line(data = arc.df, aes(setup+1, value+38), lty = lty) +
  geom_line(data = arc.df, aes(setup+2, value+50), lty = lty) +
  geom_line(data = arc.df, aes(setup+3, value+63), lty = lty) +
  geom_line(data = arc.df, aes(setup+4, value+60), lty = lty) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_discrete(labels = c("D", "C", "B", "A")) +
  scale_fill_brewer() +
  theme(legend.position = "none") + g_theme()

p1.leg <- ggplot(boxplot.df[which(boxplot.df$variable == "n"),], 
                 aes(x = variable, y = value)) +
  geom_bar(aes(fill = moment), stat="identity", position = "dodge") +
  theme(legend.position = "bottom") + g_theme() +
  scale_fill_brewer(guide = guide_legend(title = "Time of Measurement"),
                    labels = c("Before", "After")) 

leg <- g_legend(p1.leg)

g <- grid.arrange(arrangeGrob(p1, p2, nrow = 1),
                  arrangeGrob(p3, p4, nrow = 1),
                  leg, nrow=3,heights=c(2.2, 2.2, 0.5))

ggsave("Experiment II/SSQ/Charts/ssq_before_after.eps", g, width = 8, height = 5)


rm(users_count, label.df, stars, res, boxplot.df,
   arc.df, r, t, x, y, alpha, lty, ssq.df, result.df,
   p1, p2, p3, p4, p1.leg, g_legend, leg, g, temp.df)



rm(range.df, result.df, ssq.df, setup.df,
   g, p1, p2, leg_1, p1.leg, g_legend,
   p3, p4, temp_range.df, drops, melted.df)
