#' Plots a basic graph showing desired relative humidity vs LiCl concentration
#'
#' @param desired_rh The percentage value of the desired relative humidity
#' @param theme chose between light and dark plot background
#' @return a plot
#' @import magrittr ggplot2 dplyr units
#' @export
#' @examples
#' plot_rh_scratch(50,theme="dark")

plot_rh_scratch<-function(desired_rh,theme="light"){

  #Main curve

  predict<-rh_bonferroni()

  #data point

  data_point<-rh_scratch(desired_rh,volume=1,verbose = F)

  # Plot

  if (theme=="light") {

    plot<- ggplot(predict,aes(x=gl,y=rh))+
      geom_line(colour="black")+

      geom_segment(x=as.numeric(data_point),xend=0,y=desired_rh,yend=desired_rh,linetype="dashed" )+
      geom_segment(x=as.numeric(data_point),xend=as.numeric(data_point),y=desired_rh,yend=0,linetype="dashed")+
      geom_point(x=as.numeric(data_point),y=desired_rh,size=4,fill="#ea801c",shape=21)+
      scale_y_continuous(limits = c(0,100))+

      theme_classic()+
      theme(text = element_text(size=18))+

      labs(x="LiCl (g/L)",y="RH (%)")

  }else{

    plot<-ggplot(predict,aes(x=gl,y=rh))+
      geom_line(colour="white")+

      geom_segment(x=as.numeric(data_point),xend=0,y=desired_rh,yend=desired_rh,linetype="dashed",colour="white" )+
      geom_segment(x=as.numeric(data_point),xend=as.numeric(data_point),y=desired_rh,yend=0,linetype="dashed",colour="white")+
      geom_point(x=as.numeric(data_point),y=desired_rh,size=4,fill="#ea801c",shape=21)+

      theme_classic()+
      theme(panel.background =element_rect(fill="#212529"),
            plot.background = element_rect(fill="#212529",colour="#212529"),
            axis.title=element_text(colour = "white"),
            axis.text=element_text(colour = "white"),
            axis.line = element_line(colour = "white"),
            text = element_text(size=18))+
      scale_y_continuous(limits = c(0,100))+

      labs(x="LiCl (g/L)",y="RH (%)")


  }

  plot
}
