library(tidyverse)
library(ggplot2)
dataurl="https://data.giss.nasa.gov/tmp/gistemp/STATIONS/tmp_USW00014733_14_0_1/station.csv"
temp=read_csv(dataurl,
              skip=1, #skip the first line which has column names
              na="999.90", # tell R that 999.90 means missing in this dataset
              col_names = c("YEAR","JAN","FEB","MAR", # define column names 
                            "APR","MAY","JUN","JUL",  
                            "AUG","SEP","OCT","NOV",  
                            "DEC","DJF","MAM","JJA",  
                            "SON","metANN"))
view(temp)
summary(temp)
glimpse(temp)
cs2plot <- ggplot(temp, aes(YEAR,JJA)) +
  xlab("Year")+
  ylab("Mean Summer Temperatures(C)")+
  ggtitle("Mean Summer Temperatures in Buffalo, NY")+
  labs(subtitle = "Summer includes June, July, and August \nData from the Global Historical Climate Network \nRed line is a LOESS smooth")+ #took this line from Collin's code shared during class
  geom_line()+
  geom_smooth(color = "red")
cs2plot
ggsave("cs2_plot.png", plot = cs2plot)
