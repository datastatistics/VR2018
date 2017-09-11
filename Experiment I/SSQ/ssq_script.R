source("Common_files/load_packages.R")
source("Common_files/statistics_analysis.R")
source("Common_files/utils.R")
source("Common_files/ssq_score.R")

ssq.df <- read.csv("Experiment I/SSQ/Data/ssq_data.csv", sep = ';')

#############################################################################################################
########### SSQ SCORES ######################################################################################
#############################################################################################################
result.df <- calculate_ssq_scores(ssq.df, "output")

range.df <- ddply(result.df, .(user, output), summarise,
                  n = an - bn, o = ao - bo, d = ad - bd, ts = ats - bts)

################################################################################
################# Statistical analysis #########################################
################################################################################

ssq_setups <- data.frame(c("n", "o", "d", "ts"),
                                matrix(nrow = 4, ncol = 2))
colnames(ssq_setups) <- c("scale", "p-value", "star")

ssq_setups[1,2] <- nonparametric_independent_analysis(range.df$n, range.df$output, NULL, 3)
ssq_setups[1,3] <- significance_level_star(ssq_setups[1,2])
                                                    
ssq_setups[2,2] <- nonparametric_independent_analysis(range.df$o, range.df$output, NULL, 3)
ssq_setups[2,3] <- significance_level_star(ssq_setups[2,2])

ssq_setups[3,2] <- nonparametric_independent_analysis(range.df$d, range.df$output, NULL, 3)
ssq_setups[3,3] <- significance_level_star(ssq_setups[3,2])

ssq_setups[4,2] <- nonparametric_independent_analysis(range.df$ts, range.df$output, NULL, 3)
ssq_setups[4,3] <- significance_level_star(ssq_setups[4,2])

#####################################################################################################

foo = rep(0, nrow(range.df))

foo[with(range.df, output == "Monitor")] = 1
foo[with(range.df, output == "Display Wall")] = 2
foo[with(range.df, output == "Oculus Rift")] = 3

range.df$output = with(range.df, reorder(output, foo))

rm(foo)

drops <- "user"
melted.df <- range.df[ , !(names(range.df) %in% drops)]

melted.df <- melt(melted.df, 
                  id.vars = "output")

melted.df <- ddply(melted.df, .(output, variable), summarise,
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
                  id.vars = "output")

label.df <- data.frame(variable = c(1, 2, 3, 4),
                       value = c(91, 82, 152, 106))

ggplot(boxplot.df, aes(variable, value)) + 
  geom_boxplot(aes(fill = factor(output)), position = "dodge") +
  geom_text(data = label.df, size = 4, label = ssq_setups$star) +
  geom_line(data = arc.df, aes(variable+1, value+87), lty = lty) +
  geom_line(data = arc.df, aes(variable+2, value+76), lty = lty) +
  geom_line(data = arc.df, aes(variable+3, value+147), lty = lty) +
  geom_line(data = arc.df, aes(variable+4, value+103), lty = lty) +
  scale_fill_brewer(guide = guide_legend(title = "VR Displays"), 
                    labels = c("Desktop", "Display-Wall", "HMD")) +
  theme(legend.position = "bottom") + g_theme() +
  labs(y = "Score", x = "Scales", fill = "", title = "Simulator Sickness") 
  ggsave("Experiment I/SSQ/Charts/ssq_variation.eps", width = 5, height = 4)

rm(boxplot.df, lty, label.df, range.df, r, t, x, y, 
   arc.df, result.df)


#######################################################################################
########## SSQ between measurement time ###############################################

temp.df <- calculate_ssq_scores(ssq.df, "output")

#transforrmar a tabela em data frame com o momento sendo uma coluna
users_count <- length(ssq.df$user)
result.df <- data.frame(rep(ssq.df$user, each = 2),
                        rep(c("before", "after"), each = 1), 
                        rep(ssq.df$output, each = 2),
                        matrix(nrow = users_count, ncol = 4))

result.df <- result.df[1:(users_count * 2),]
colnames(result.df) <- c("user", "moment", "output", "n", "o", "d", "ts")

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
rm(x,i)

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
                                             temp_result.df$output,
                                             NULL, 3)

result <- nonparametric_independent_analysis(temp_result.df$o,
                                             temp_result.df$output,
                                             NULL, 3)

result <- nonparametric_independent_analysis(temp_result.df$d,
                                             temp_result.df$output,
                                             NULL, 3)

result <- nonparametric_independent_analysis(temp_result.df$ts,
                                             temp_result.df$output,
                                             NULL, 3)

drops <- "user"
temp_result.df <- temp_result.df[ , !(names(temp_result.df) %in% drops)]
temp_result.df = ddply(temp_result.df, .(moment), summarise, mean_n = mean(n), sdn = sd(n),
                       mean_o = mean(o), sdo = sd(o), mean_d = mean(d), sdd = sd(d), mean_ts = mean(ts), sdts = sd(ts))


#############################################################################################################
########### VARIANCE ANALYSIS ###############################################################################
#############################################################################################################
ssq_moment <- data.frame(c("Monitor", "Display Wall", "Oculus Rift"),
                         matrix(nrow = 3, ncol = 8))
colnames(ssq_moment) <- c("output", "n", "nstar", "o", "ostar", "d", "dstar", "ts", "tsstar")

temp_result.df <- result.df[which(result.df$output == "Monitor"),]
ssq_moment[1,2] <- nonparametric_dependent_analysis(temp_result.df$n, temp_result.df$moment, NULL, 2)
ssq_moment[1,3] <- significance_level_star(ssq_moment[1,2])
ssq_moment[1,4] <- nonparametric_dependent_analysis(temp_result.df$o, temp_result.df$moment, NULL, 2)
ssq_moment[1,5] <- significance_level_star(ssq_moment[1,4])
ssq_moment[1,6] <- nonparametric_dependent_analysis(temp_result.df$d, temp_result.df$moment, NULL, 2)
ssq_moment[1,7] <- significance_level_star(ssq_moment[1,6])
ssq_moment[1,8] <- nonparametric_dependent_analysis(temp_result.df$ts, temp_result.df$moment, NULL, 2)
ssq_moment[1,9] <- significance_level_star(ssq_moment[1,8])

temp_result.df <- result.df[which(result.df$output == "Display Wall"),]
ssq_moment[2,2] <- nonparametric_dependent_analysis(temp_result.df$n, temp_result.df$moment, NULL, 2)
ssq_moment[2,3] <- significance_level_star(ssq_moment[2,2])
ssq_moment[2,4] <- nonparametric_dependent_analysis(temp_result.df$o, temp_result.df$moment, NULL, 2)
ssq_moment[2,5] <- significance_level_star(ssq_moment[2,4])
ssq_moment[2,6] <- nonparametric_dependent_analysis(temp_result.df$d, temp_result.df$moment, NULL, 2)
ssq_moment[2,7] <- significance_level_star(ssq_moment[2,6])
ssq_moment[2,8] <- nonparametric_dependent_analysis(temp_result.df$ts, temp_result.df$moment, NULL, 2)
ssq_moment[2,9] <- significance_level_star(ssq_moment[2,8])

temp_result.df <- result.df[which(result.df$output == "Oculus Rift"),]
ssq_moment[3,2] <- nonparametric_dependent_analysis(temp_result.df$n, temp_result.df$moment, NULL, 2)
ssq_moment[3,3] <- significance_level_star(ssq_moment[3,2])
ssq_moment[3,4] <- nonparametric_dependent_analysis(temp_result.df$o, temp_result.df$moment, NULL, 2)
ssq_moment[3,5] <- significance_level_star(ssq_moment[3,4])
ssq_moment[3,6] <- nonparametric_dependent_analysis(temp_result.df$d, temp_result.df$moment, NULL, 2)
ssq_moment[3,7] <- significance_level_star(ssq_moment[3,6])
ssq_moment[3,8] <- nonparametric_dependent_analysis(temp_result.df$ts, temp_result.df$moment, NULL, 2)
ssq_moment[3,9] <- significance_level_star(ssq_moment[3,8])

stars <- data.frame(ssq_moment$output, ssq_moment$nstar, ssq_moment$ostar, ssq_moment$dstar, ssq_moment$tsstar)
colnames(stars) <- c("output", "n", "o", "d", "ts")
stars <- melt(stars, id.vars = "output")

rm(temp_result.df)

#############################################################################################################
########### CHARTS ##########################################################################################
#############################################################################################################

drops <- "user"
boxplot.df <- result.df[ , !(names(result.df) %in% drops)]

boxplot.df <- melt(boxplot.df, 
                   id.vars = c("output", "moment"))

boxplot.df <- ddply(boxplot.df, .(output, moment, variable), summarise,
                    sd = sd(value), se = sd/sqrt(length(value)),
                    value = mean(value))

#reorder data by moment that ssq was applied in the experiment
foo = rep(0, nrow(boxplot.df))

foo[with(boxplot.df, moment == "before")] = 1
foo[with(boxplot.df, moment == "after")] = 2

boxplot.df$moment = with(boxplot.df, reorder(moment, foo))

foo[with(boxplot.df, output == "Monitor")] = 1
foo[with(boxplot.df, output == "Display Wall")] = 2
foo[with(boxplot.df, output == "Oculus Rift")] = 3

boxplot.df$output = with(boxplot.df, reorder(output, foo))

rm(foo)


# Define arc coordinates
r <- 0.15
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 5 * sin(t)

arc.df <- data.frame(output = x, value = y)

######################################################################################################################
#bar plots

lty = 6
label.df <- data.frame(output = c(1, 2, 3),
                       value = c(18, 18, 37))
p1 <- ggplot(boxplot.df[which(boxplot.df$variable == "n"),], 
             aes(x = output, y = value)) +
  geom_bar(aes(fill = moment), stat="identity", position = position_dodge()) +
  #geom_boxplot(aes(fill = moment), position = position_dodge()) +
  geom_errorbar(aes(fill = moment, ymin=value-se, ymax=value+se), stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  labs(y = "Score", x = "Display Fidelity", fill = "", title = "Nausea") +
  geom_text(data = label.df, size = 4,
            label = stars[which(stars$variable == "n"),]$value) +
  geom_line(data = arc.df, aes(output+1, value+14), lty = lty) +
  geom_line(data = arc.df, aes(output+2, value+14), lty = lty) +
  geom_line(data = arc.df, aes(output+3, value+35), lty = lty) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_discrete(labels = c("Low", "Moderate", "High")) +
  scale_fill_brewer() +
  theme(legend.position = "none") + g_theme() +
  ggsave("Experiment I/SSQ/Charts/nausea.eps", width = 5, height = 4)

label.df <- data.frame(output = c(1, 2, 3),
                       value = c(22, 26, 37))
p2 <- ggplot(boxplot.df[which(boxplot.df$variable == "o"),],
             aes(output, value)) +
  #geom_boxplot(aes(fill = moment), position = position_dodge()) +
  geom_bar(aes(fill = moment), stat="identity", position = "dodge") +
  geom_errorbar(aes(fill = moment, ymin=value-se, ymax=value+se), stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  labs(y = "Score", x = "Display Fidelity", fill = "", title = "Oculomotor") +
  geom_text(data = label.df, size = 4,
            label = stars[which(stars$variable == "o"),]$value) +
  geom_line(data = arc.df, aes(output+1, value+18), lty = lty) +
  geom_line(data = arc.df, aes(output+2, value+22), lty = lty) +
  geom_line(data = arc.df, aes(output+3, value+33), lty = lty) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_discrete(labels = c("Low", "Moderate", "High")) +
  scale_fill_brewer() +
  theme(legend.position = "none") + g_theme() +
  ggsave("Experiment I/SSQ/Charts/oculomotor.eps", width = 5, height = 4)


label.df <- data.frame(output = c(1, 2, 3),
                       value = c(22, 28, 75))
p3 <- ggplot(boxplot.df[which(boxplot.df$variable == "d"),],
             aes(output, value)) +
  #geom_boxplot(aes(fill = moment), position = position_dodge()) +
  geom_bar(aes(fill = moment), stat="identity", position = "dodge") +
  geom_errorbar(aes(fill = moment, ymin=value-se, ymax=value+se), stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  labs(y = "Score", x = "Display Fidelity", fill = "", title = "Disorientation") +
  geom_text(data = label.df, size = 4,
            label = stars[which(stars$variable == "d"),]$value) +
  geom_line(data = arc.df, aes(output+1, value+18), lty = lty) +
  geom_line(data = arc.df, aes(output+2, value+24), lty = lty) +
  geom_line(data = arc.df, aes(output+3, value+73), lty = lty) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_discrete(labels = c("Low", "Moderate", "High")) +
  scale_fill_brewer() +
  theme(legend.position = "none") + g_theme() +
  ggsave("Experiment I/SSQ/Charts/disorientation.eps", width = 5, height = 4)

label.df <- data.frame(output = c(1, 2, 3),
                       value = c(22, 26, 52))
p4 <- ggplot(boxplot.df[which(boxplot.df$variable == "ts"),],
             aes(output, value)) +
  #geom_boxplot(aes(fill = moment), position = position_dodge()) +
  geom_bar(aes(fill = moment), stat="identity", position = "dodge") +
  geom_errorbar(aes(fill = moment, ymin=value-se, ymax=value+se), stat = "identity", width = .1, position=position_dodge(width=0.9)) +
  labs(y = "Score", x = "Display Fidelity", fill = "", title = "Total Score") +
  geom_text(data = label.df, size = 4,
            label = stars[which(stars$variable == "ts"),]$value) +
  geom_line(data = arc.df, aes(output+1, value+18), lty = lty) +
  geom_line(data = arc.df, aes(output+2, value+22), lty = lty) +
  geom_line(data = arc.df, aes(output+3, value+50), lty = lty) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_discrete(labels = c("Low", "Moderate", "High")) +
  scale_fill_brewer() +
  theme(legend.position = "none") + g_theme() +
  ggsave("Experiment I/SSQ/Charts/total_score.eps", width = 5, height = 4)

p1.leg <- ggplot(boxplot.df[which(boxplot.df$variable == "n"),], 
                 aes(x = output, y = value)) +
  geom_bar(aes(fill = moment), stat="identity", position = "dodge") +
  theme(legend.position = "bottom") + g_theme() +
  scale_fill_brewer() +
  guides(fill = guide_legend(nrow = 1, title = "Moment"))

leg <- g_legend(p1.leg)

g <- grid.arrange(arrangeGrob(p1, p2, nrow = 1),
                  arrangeGrob(p3, p4, nrow = 1),
                  leg, nrow=3,heights=c(2, 2, 0.5))

ggsave("Experiment I/SSQ/Charts/ssq_before_after.eps", g, width = 7, height = 5)


rm(users_count, label.df,
   arc.df, r, t, x, y, alpha, drops, lty, ssq.df, boxplot.df, result.df,
   p1, p2, p3, p4, p1.leg, g_legend, leg, g, stars, ssq.df)
