library('tidyverse')
library('reprex') 
library('sf') 
library('ggplot2')
library('spData') 
data(world) 
ggplot(world,aes(x=gdpPercap, y=continent, color=continent))+
   geom_density(alpha=0.5,color=F) 
 
reprex()

