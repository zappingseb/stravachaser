
#' @import grid
#' @import gridExtra
#' @import ggplot
#' 
#' from: https://stackoverflow.com/questions/18265941/two-horizontal-bar-charts-with-shared-axis-in-ggplot2-similar-to-population-pyr
left_right_plot <- function(label="# of segments",
                            left=500,
                            right=200,
                            unit = ""
){
  if(length(left)==0)left<-0
  if(length(right)==0)right<-0
  
  DATA<-data.frame(state=c(label),sales=c(right),sales_staff=c(left))
  
  library(grid)
  g.mid<-ggplot(DATA,aes(x=1,y=state))+geom_text(aes(label=state),angle = 90)+
    geom_segment(aes(x=0.94,xend=0.96,yend=state))+
    geom_segment(aes(x=1.04,xend=1.065,yend=state))+
    ggtitle("")+
    ylab(NULL)+
    scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
    theme(axis.title=element_blank(),
          panel.grid=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          axis.text.x=element_text(color=NA),
          axis.ticks.x=element_line(color=NA),
          plot.margin = unit(c(1,-1,1,-1), "mm"))
  g1 <- ggplot(data = DATA, aes(x = state, y = sales_staff)) +
    geom_bar(stat = "identity",fill="#fc4c02") +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(1,-1,1,0), "mm"),
          panel.background = element_blank())+ ylim(0,max(left,right)) + 
    geom_text(aes(label = paste(sales_staff,unit)),
              position = position_dodge(width = .5), hjust = -2, size = 6,
              angle=00,color="white") + 
    scale_y_reverse() + coord_flip()
  
  g2 <- ggplot(data = DATA, aes(x = state, y = sales)) +xlab(NULL)+
    geom_bar(stat = "identity",fill="#fc4c02") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,0,1,-1), "mm"),panel.background = element_blank()
    ) + ylim(0,max(left,right)) +
    geom_text(aes(label = paste0(sales," ",unit)),
              position = position_dodge(width = .5), hjust = 2, size = 6,
              angle=00,color="white") + 
    coord_flip()
  
  
  gg1 <- ggplot_gtable(ggplot_build(g1))
  gg2 <- ggplot_gtable(ggplot_build(g2))
  gg.mid <- ggplot_gtable(ggplot_build(g.mid))
  
  grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/9,4/9))
}
