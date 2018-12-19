#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Plot reflectance red faces: painted vs unmanipulated
#	 Start : 09/11/2018
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table <- read.table("Reflectance_withoutSpikes.txt", header=TRUE)
head(table)

library(ggplot2)


  
ggplot(data=table, aes(x=Wavelength, y=Reflectance, colour = Red, fill = Red)) +
  scale_color_manual(values = c("Natural"="firebrick","Painted" ="red")) +
  geom_line() +
  geom_ribbon(aes(x = Wavelength, ymin=LowCI, ymax=UpperCI), linetype=1, alpha=0.2, color=NA) +
  scale_fill_manual(values = c("Natural"="firebrick","Painted" ="red"), guide = FALSE)



