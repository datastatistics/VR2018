source("Common_files/load_packages.R")

img <- readPNG("Experiment I/Map/map_background.png")

df <- read.csv("Experiment I/Map/Data/data.csv", sep = ";")

p3 <- ggplot(df[which(df$display == "Oculus Rift"),], aes(x = y, y = x, colour = factor(user))) + 
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point(size = 0.5) + geom_path() + 
  guides(col = guide_legend(nrow = 10, title = "Users", title.position = "top")) +
  labs(y = "", x="", title = "(c) HMD") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        plot.title = element_text(size = 30),
        legend.position = "none",
        plot.margin=unit(c(0.5,-0.5,-0.5,-0.4), "cm")) + 
  scale_x_continuous(expand=c(0,0), limits = c(-32.4,0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-28.7,0)) +
  scale_color_hue(labels = c(1:20)) +
  ggsave("Experiment I/Map/Charts/map_HMD.png")

p1 <- ggplot(df[which(df$display == "Monitor"),], aes(x = y, y = x, colour = factor(user))) + 
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point(size = 0.5) + geom_path() + 
  guides(col = guide_legend(nrow = 10, title = "Users", title.position = "top")) +
  labs(y = "", x="", title = "(a) Desktop") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        plot.title = element_text(size = 30),
        legend.position = "none",
        plot.margin=unit(c(0.5,-0.5,-0.5,-0.5), "cm")) + 
  scale_x_continuous(expand=c(0,0), limits = c(-32.4,0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-28.7,0)) +
  scale_color_hue(labels = c(1:20)) +
  ggsave("Experiment I/Map/Charts/map_desktop.png")

p2 <- ggplot(df[which(df$display == "DisplayWall"),], aes(x = y, y = x, colour = factor(user))) + 
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point(size = 0.5) + geom_path() + 
  guides(col = guide_legend(nrow = 11, title = "Users", title.position = "bottom")) +
  labs(y = "", x="", title = "(b) Display-Wall") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        plot.title = element_text(size = 30),
        legend.position = "none",
        plot.margin=unit(c(0.5,-0.5,-0.5,-0.4), "cm")) + 
  scale_x_continuous(expand=c(0,0), limits = c(-32.4,0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-28.7,0)) +
  scale_color_hue(labels = c(1:22)) +
  ggsave("Experiment I/Map/Charts/map_displaywall.png")

g <- grid.arrange(arrangeGrob(p1, p2, p3, ncol = 3))

ggsave("Experiment I/Map/Charts/path.eps", g, width = 25, height = 10)

rm(p1, p2, p3, g, df, img)