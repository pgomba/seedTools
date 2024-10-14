#' Plots a basic graph showing desired changes in LiCl solution
#'
#' @param rh_initial The relative humidity percentage value of the solution to be modified
#' @param desired_rh The percentage value of the desired relative humidity
#' @return a plot
#' @import magrittr ggplot2 dplyr
#' @export
#' @examples
#' plot_rh_modify(40,60)

plot_rh_modify<-function(rh_initial,desired_rh){

  gl_initial<-(28.6565 + 16.8639 * log((107.7549/(rh_initial - 8.3123)) - 1)*10)
  gl_final<-(28.6565 + 16.8639 * log((107.7549/(desired_rh - 8.3123)) - 1)*10)

  #Curve

  predict<-data.frame(rh=seq(11,99,1))%>%
    mutate(gl=(28.6565 + 16.8639 * log((107.7549/(rh - 8.3123)) - 1))*10)

  #Change curve

  predict2<-data.frame(rh=seq(rh_initial,desired_rh,by=sign(desired_rh-rh_initial)))%>%
    mutate(gl=(28.6565 + 16.8639 * log((107.7549/(rh - 8.3123)) - 1))*10)

  #Input points

  input<-data.frame(rh=c(rh_initial,desired_rh),
                    value=c("initial","final"))%>%
    mutate(gl=(28.6565 + 16.8639 * log((107.7549/(rh - 8.3123)) - 1))*10)

  # Plot

  plot<-ggplot(predict,aes(x=gl,y=rh))+
    geom_line(colour="black")+
    geom_line(data=predict2,linewidth=1.1,colour="darkred",linetype="dashed")+
    theme_classic()+

    geom_segment(data=input%>%filter(value=="initial"),aes(x=gl,xend=gl,y=0,yend=rh),
                 linetype="dashed",colour="#ea801c")+
    geom_point(data=input%>%filter(value=="initial"),aes(x=gl,y=rh),
               size=4,colour="#ea801c")+
    geom_text(data=input%>%filter(value=="initial"),aes(x=gl+40),label="Initial")+


    geom_segment(data=input%>%filter(value=="final"),aes(x=gl,xend=gl,y=0,yend=rh),
                 linetype="dashed",colour="#1a80bb")+
    geom_point(data=input%>%filter(value=="final"),aes(x=gl,y=rh),
               size=4,colour="#1a80bb")+
    geom_text(data=input%>%filter(value=="final"),aes(x=gl+40),label="Final")+

    labs(x="LiCl (g/l)",y="Relative Humidity (%)")

  plot
}





