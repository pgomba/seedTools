#' Plots a basic graph showing desired changes in LiCl solution
#'
#' @param rh_initial The relative humidity percentage value of the solution to be modified
#' @param desired_rh The percentage value of the desired relative humidity
#' @param theme chose between light and dark plot background
#' @param units select LiCl concentration to be "g/L" or "g/100ml)
#' @return a plot
#' @import magrittr ggplot2 dplyr
#' @export
#' @examples
#' plot_rh_modify(40,60)

plot_rh_modify<-function(rh_initial,desired_rh,theme="light",units="g/L"){

  gl_initial<-(28.6565 + 16.8639 * log((107.7549/(rh_initial - 8.3123)) - 1))
  gl_final<-(28.6565 + 16.8639 * log((107.7549/(desired_rh - 8.3123)) - 1))


  #Curve

  predict<-rh_bonferroni(bf_units=units)

  if (units == "g/L") {
    multi <- 10
    x_axis<-"LiCl (g/L)"
    nudg<- 40
  } else if (units == "g/100ml") {
    multi <- 1
    x_axis<-"LiCl (g/100ml)"
    nudg<- 4
  } else {
    stop("Invalid unit. Please specify 'g/L' or 'g/100ml'.")
  }

  #Change curve

  predict2<-data.frame(rh=seq(rh_initial,desired_rh,by=sign(desired_rh-rh_initial)))%>%
    mutate(gl=(28.6565 + 16.8639 * log((107.7549/(rh - 8.3123)) - 1)))*multi

  #Input points

  input<-data.frame(rh=c(rh_initial,desired_rh),
                    value=c("initial","final"))%>%
    mutate(gl=(28.6565 + 16.8639 * log((107.7549/(rh - 8.3123)) - 1))*multi)

  # Plot

  if (theme=="light") {

    plot<-ggplot(predict,aes(x=gl,y=rh))+
      geom_line(colour="black")+
      geom_line(data=predict2,linewidth=1.1,colour="darkred",linetype="dashed")+


      geom_segment(data=input%>%filter(value=="initial"),aes(x=gl,xend=gl,y=0,yend=rh),
                   linetype="dashed",colour="#ea801c")+
      geom_point(data=input%>%filter(value=="initial"),aes(x=gl,y=rh),
                 size=4,colour="#ea801c")+
      geom_text(data=input%>%filter(value=="initial"),aes(x=gl),label="Initial",size=5,nudge_x = nudg)+


      geom_segment(data=input%>%filter(value=="final"),aes(x=gl,xend=gl,y=0,yend=rh),
                   linetype="dashed",colour="#1a80bb")+
      geom_point(data=input%>%filter(value=="final"),aes(x=gl,y=rh),
                 size=4,colour="#1a80bb")+
      geom_text(data=input%>%filter(value=="final"),aes(x=gl),label="Final",size=5,nudge_x = nudg)+
      scale_y_continuous(limits = c(0,100))+

      theme_classic()+
      theme(text = element_text(size=18))+

      labs(x=x_axis,y="RH (%)")

  }else{

    plot<-ggplot(predict,aes(x=gl,y=rh))+
      geom_line(colour="white")+
      geom_line(data=predict2,linewidth=1.1,colour="darkred",linetype="dashed")+

      geom_segment(data=input%>%filter(value=="initial"),aes(x=gl,xend=gl,y=0,yend=rh),
                   linetype="dashed",colour="#ea801c")+
      geom_point(data=input%>%filter(value=="initial"),aes(x=gl,y=rh),
                 size=4,colour="#ea801c")+
      geom_text(data=input%>%filter(value=="initial"),aes(x=gl),label="Initial",colour="white",size=5,nudge_x = nudg)+


      geom_segment(data=input%>%filter(value=="final"),aes(x=gl,xend=gl,y=0,yend=rh),
                   linetype="dashed",colour="#1a80bb")+
      geom_point(data=input%>%filter(value=="final"),aes(x=gl,y=rh),
                 size=4,colour="#1a80bb")+
      geom_text(data=input%>%filter(value=="final"),aes(x=gl),label="Final",colour="white",size=5,nudge_x = nudg)+
      theme_classic()+
      theme(panel.background =element_rect(fill="#212529"),
            plot.background = element_rect(fill="#212529",colour="#212529"),
            axis.title=element_text(colour = "white"),
            axis.text=element_text(colour = "white"),
            axis.line = element_line(colour = "white"),
            text = element_text(size=18))+

      scale_y_continuous(limits = c(0,100))+

      labs(x=x_axis,y="RH (%)")

  }

  plot
}





