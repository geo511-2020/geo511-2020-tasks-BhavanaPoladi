library(tidyverse)
library(nycflights13)
library(dplyr)

View(flights)
View(airports)

#sort for max distance
df1 <- flights[order(-flights$distance),]
view(df1)

#slice the farthest dest
df2=slice(df1,1)

#join  dest&faa from df2 & airports
df3=inner_join(df2,airports, by=c("dest"="faa"))

print(df3$name)