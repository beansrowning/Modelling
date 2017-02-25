#output using ggplot2
mytheme <- theme_bw() +
  theme(text=element_text(colour="black")) +
  theme(panel.grid = element_line(colour = "black")) +
  theme(panel.background = element_rect(fill = "white"))+
  theme(plot.title = element_text(hjust = 0.5))
theme_set(mytheme)
title <-bquote("SIR model")
subtitle <-bquote(list(~beta [1]==.(parameters$beta[1]),~beta [2]==.(parameters$beta[2]),~gamma==.(parameters$gamma)))
results <- ggplot(s, aes(x=time))+
  ggtitle(bquote(atop(bold(.(title)),atop(.(subtitle)))))+
  geom_line(aes(y=S,colour="Susceptible"))+
  geom_line(aes(y=I,colour="Infected"))+
  geom_line(aes(y=R,colour="Recovered"))+
  ylab(label = "Count")+
  xlab(label = "Time")+
  theme(legend.justification=c(1,0), legend.position=c(1,0.25))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',size=0.5,linetype="solid"),
        legend.text=element_text(size=10),legend.key=element_rect(colour="#FFFFFF",
                                                                  fill='#C2C2C2',size=0.25,linetype="solid"))+
  scale_colour_manual("Divisions",
                      breaks=c("Susceptible","Infected","Recovered"),
                      values=c("blue","red","darkgreen"))
ggsave(plot = results, filename = paste("SIR",runid,"png", sep = "."), #saves a new file each run
scale = 1) 

#I originally did matplot:
#matplot(r[,"time"], r[,c("S2","I2","R2")], type = 'l',
#xlab = "Time", ylab = "Count (log Scale)", log = 'y', lty = 1:2) 
#title(main = "Subpopulation 2")
#legend("bottomright", legend = c("S","I","R"), lty = 1:3, col = 1:3)