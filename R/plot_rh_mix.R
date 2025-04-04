#' Plots a basic graph showing LiCl after mixing two solutions
#'
#' @param rh1 The relative humidity percentage value of solution number one
#' @param vol1 The volume 'in Litres' of solution rh1
#' @param rh2 The relative humidity percentage value of solution number two
#' @param vol2 The volume 'in Litres' of solution rh2#'
#' @param theme chose between light and dark plot background
#' @param units select LiCl concentration to be "g/L" or "g/100ml)
#' @return a plot
#' @import magrittr ggplot2 dplyr
#' @export
#' @examples
#' plot_rh_mix(40,3,60,1)


plot_rh_mix<-function(rh1,vol1,rh2,vol2,theme="light",units="g/L"){

  # data for plot
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

  # predicting new mix

  lc_conc1<-(28.6565 + 16.8639 * log((107.7549/(rh1 - 8.3123)) - 1))*multi
  lc_g1<-lc_conc1*vol1

  lc_conc2<-(28.6565 + 16.8639 * log((107.7549/(rh2 - 8.3123)) - 1))*multi
  lc_g2<-lc_conc2*vol2

  new_conc<-(lc_g1+lc_g2)/(vol1+vol2)

  new_rh<- (116.0672 - 8.3123) / (1 + exp(((new_conc/multi) - 28.6565) / 16.8639)) + 8.3123

  data_to_plot<-data.frame(rh=c(rh1,rh2,new_rh),
                           concentration=c(lc_conc1,lc_conc2,new_conc),
                           solution=c("Solution 1", "Solution 2", "Final"))

  #Change curve

  predict2<-data.frame(rh=seq(rh1,new_rh,by=sign(new_rh-rh1)))%>%
    mutate(gl=(28.6565 + 16.8639 * log((107.7549/(rh - 8.3123)) - 1))*multi)

  predict3<-data.frame(rh=seq(rh2,new_rh,by=sign(new_rh-rh2)))%>%
    mutate(gl=(28.6565 + 16.8639 * log((107.7549/(rh - 8.3123)) - 1))*multi)


  ## Plot

  if (theme=="light") {

 plot<- ggplot(predict,aes(x=gl,y=rh))+

   geom_line()+

   geom_line(data=predict2,linewidth=1.1,colour="#BB5566",linetype="dashed")+
   geom_point(data=data_to_plot%>%filter(solution=="Solution2"), aes(y=rh1,x=lc_conc1),size=3,
              shape=21, colour="black", fill="#EE7733")+

   geom_line(data=predict3,linewidth=1.1,colour="#009988",linetype="dashed")+
   geom_point(data=data_to_plot%>%filter(solution=="Solution1"), aes(y=rh2,x=lc_conc2),size=3,
              shape=21,colour="black",fill="#EE7733")+

   geom_point(data=data_to_plot%>%filter(solution=="Final"), aes(y=new_rh,x=new_conc),size=4,
              shape=21,colour="black",fill="#4477AA")+
   geom_text(data=data_to_plot%>%filter(solution=="Final"), aes(y=new_rh+1,x=new_conc+nudg),label="Final",size=5)+
   geom_segment(data=data_to_plot%>%filter(solution=="Final"),aes(x=0,xend=new_conc,y=new_rh,yend=new_rh),
                linetype="dashed",colour="#4477AA")+
   geom_segment(data=data_to_plot%>%filter(solution=="Final"),aes(x=new_conc,xend=new_conc,y=0,yend=new_rh),
                linetype="dashed",colour="#4477AA")+

   theme_classic()+
   theme(text = element_text(size=18))+
   scale_y_continuous(limits = c(0,100))+
   labs(y="RH (%)", x=x_axis)

  }else{

    plot<- ggplot(predict,aes(x=gl,y=rh))+

      geom_line(colour="white")+

      geom_line(data=predict2,linewidth=1.1,colour="#BB5566")+
      geom_point(data=data_to_plot%>%filter(solution=="Solution2"), aes(y=rh1,x=lc_conc1),size=3,
                 shape=21, colour="black", fill="#EE7733")+

      geom_line(data=predict3,linewidth=1.1,colour="#009988")+
      geom_point(data=data_to_plot%>%filter(solution=="Solution1"), aes(y=rh2,x=lc_conc2),size=3,
                 shape=21, colour="black", fill="#EE7733")+

      geom_point(data=data_to_plot%>%filter(solution=="Final"), aes(y=new_rh,x=new_conc),size=4,
                 shape=21,colour="black", fill="#4477AA")+
      geom_text(data=data_to_plot%>%filter(solution=="Final"), aes(y=new_rh+1,x=new_conc+nudg,),label="Final",colour="white",size=5)+
      geom_segment(data=data_to_plot%>%filter(solution=="Final"),aes(x=0,xend=new_conc,y=new_rh,yend=new_rh),
                   linetype="dashed",colour="#4477AA")+
      geom_segment(data=data_to_plot%>%filter(solution=="Final"),aes(x=new_conc,xend=new_conc,y=0,yend=new_rh),
                   linetype="dashed",colour="#4477AA")+

      theme_classic()+
      labs(y="RH (%)", x=x_axis)+
      scale_y_continuous(limits = c(0,100))+
      theme(panel.background =element_rect(fill="#212529"),
            plot.background = element_rect(fill="#212529",colour="#212529"),
            axis.title=element_text(colour = "white"),
            axis.text=element_text(colour = "white"),
            axis.line = element_line(colour = "white"),
            text = element_text(size=18))

 }

 plot
}

