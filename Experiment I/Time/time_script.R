source("Common_files/load_packages.R")
source("Common_files/statistics_analysis.R")
source("Common_files/utils.R")

risks.df <- read.csv("Experiment I/Time/Data/risks_data.csv", sep = ";")
risks.df <- ddply(risks.df, .(user, display), summarise, value = length(id))

drops <- "user"
risks.df <- risks.df[ , !(names(risks.df) %in% drops)]

plot.risks.df <- ddply(risks.df, .(display), summarise, mean = mean(value),
                       sd = sd(value), se = sd/sqrt(length(value)))

#################################################################################
############# TIME DATA #########################################################
#################################################################################

times.df <- read.csv("Experiment I/Time/Data/time_data.csv", sep = ";")

foo = rep(0, nrow(times.df))

foo[with(times.df, display == "Monitor")] = 1
foo[with(times.df, display == "DisplayWall")] = 2
foo[with(times.df, display == "Oculus Rift")] = 3

times.df$display = with(times.df, reorder(display, foo))

rm(foo)

shapiro.test(times.df$time)

p_value <- nonparametric_independent_analysis(times.df$time, times.df$display, NULL, 3)
star <- significance_level_star(p_value) 

drops <- "user"
times.df <- times.df[ , !(names(times.df) %in% drops)]
plot.time.df <- ddply(times.df, .(display), summarise, value = mean(time),
                       sd = sd(time), se = sd/sqrt(length(time)))

label.df <- data.frame(display = 2,
                       value = 850)

r <- 1
t <- seq(0, 180, by = 1) * pi / 180
x <- r * cos(t)
y <- r * 5 * sin(t)

arc.df <- data.frame(display = x, value = y)

rm(r, t, x, y)

ggplot(plot.time.df, aes(display, value)) + 
  geom_bar(aes(fill = display), stat = "identity", position = "dodge") +
  geom_errorbar(aes(fill = display, ymin=value-se, ymax=value+se), stat = "identity", 
                width = .1, position=position_dodge(width=0.9)) +
  scale_fill_brewer() +
  geom_text(data = label.df, size = 4, label = star) +
  geom_line(data = arc.df, aes(display+2, value+830), lty = 6) +
  labs(y = "Time (in seconds)", x="VR Displays", title = "") +
  theme(legend.position = "none") + g_theme() +
  scale_x_discrete(labels = c("Desktop", "Display-Wall", "HMD")) +
  ggsave("Experiment I/Time/Charts/time.eps", width = 6, height = 4)

rm(label.df, drops, times.df, plot.time.df,
   risks.df, plot.risks.df, arc.df, temp.df, p_value, star)