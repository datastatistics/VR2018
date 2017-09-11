get_display_info <- function(){
  display.df <- read.csv("Common_files/display_info.csv", sep = ";")
  return(display.df)
}

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

g_theme <- function(){
  return(theme(panel.background = element_rect(fill = "white", color = "black"),
               panel.grid.major = element_line(colour = "gray"),
               panel.grid.minor = element_line(colour = "gray", linetype = "dotted"),
               panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()))
}