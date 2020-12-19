library(ggplot2)
library(gapminder) 
library(dplyr)

##### removing Kuwait ######
rem_Kuwait <- gapminder %>%
  filter(country != "Kuwait")

###### plot 1 ########
plot1 <- ggplot(rem_Kuwait, aes(lifeExp, gdpPercap, color = continent, size = pop/100000)) +
  geom_point() + 
  facet_wrap(~year, nrow = 1) +
  scale_y_continuous(trans = "sqrt") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 22)) +
  labs(title = "Wealth and life expectancy through time", x = "Life Expectancy", 
       y = "GDP per capita", size = "Population (100k)", color = "Continent")
ggsave(filename = "Plot1_casestudy3.png", device = "png", width = 15, units = c("in"))
print(plot1)

##### plot 2 #######
gapminder_continent <- rem_Kuwait %>%
  group_by(continent, year) %>%
  summarize(gdpPercapweighted = weighted.mean(x = gdpPercap, w = pop), 
            pop = sum(as.numeric(pop)))

plot2 <- ggplot(rem_Kuwait, aes(year, gdpPercap, color = continent)) + 
  geom_line(aes(group = country)) + 
  geom_point(aes(size = pop/100000, group = country)) + 
  geom_line(data = gapminder_continent, color = "black", aes(year, gdpPercapweighted)) + 
  geom_point(data = gapminder_continent, color = "black", aes(year, gdpPercapweighted,     #TingChang helped with the black lines
                                                              size = pop/100000)) +
  facet_wrap(~continent, nrow = 1) + theme_bw() + labs(x = "Year", y = "GDP per capita", 
                                                       size = "Population (100k)")
ggsave(filename = "Plot2_casestudy3.png", device = "png", width = 15, units = c("in"))
print(plot2)

