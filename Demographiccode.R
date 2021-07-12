library(tidyverse)
library(ggplot2)
library(patchwork)


# read the file
setwd("C:/Users/nancy/OneDrive/Desktop/ICS/")
demographic <- read.csv(file = 'census_2020_2000.csv')


demographic <- demographic %>% drop_na("Total.Fertility.Rate","Life.Expectancy.at.Birth..Both.Sexes","Life.Expectancy.at.Birth..Males","Life.Expectancy.at.Birth..Females")

colSums(is.na(demographic$Subregion))


#Part1
#Describe the frequency distributions of the variables. Consider also the differences between the sexes.

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


gr1 <- ggplot(filter(demographic, Year == 2020)) +
  geom_histogram(aes(x = Total.Fertility.Rate, y = ..density..),
                 bins=50, fill = "grey", color = "black") +  xlab("Fertility rate per woman") +
  ylab("Density") + ggtitle("Fertility rate per woman") +
  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))

max(filter(demographic, Year == 2020)$Total.Fertility.Rate) # 7
quantile(filter(demographic, Year==2020)$Total.Fertility.Rate)
getmode(filter(demographic, Year == 2020)$Total.Fertility.Rate)

gr2 <- ggplot(filter(demographic, Year == 2020)) +
  geom_histogram(aes(x = Life.Expectancy.at.Birth..Males, y = ..density..),
                 binwidth = 1, fill = "grey", color = "black") +  xlab("Male life expectancy") + 
  ylab("Density") + ggtitle("Life expectancy males") +
  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))

max(filter(demographic, Year == 2020)$Life.Expectancy.at.Birth..Males) 
quantile(filter(demographic, Year==2020)$Life.Expectancy.at.Birth..Males)
getmode(filter(demographic, Year == 2020)$Life.Expectancy.at.Birth..Males)

gr3 <- ggplot(filter(demographic, Year == 2020)) +
  geom_histogram(aes(x = Life.Expectancy.at.Birth..Females, y = ..density..),
                 binwidth = 1, fill = "grey", color = "black") +  xlab("Female life expectancy") + 
  ylab("Density") + ggtitle("Life expectancy females") +
  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))

max(filter(demographic, Year == 2020)$Life.Expectancy.at.Birth..Females) 
quantile(filter(demographic, Year==2020)$Life.Expectancy.at.Birth..Females)
getmode(filter(demographic, Year == 2020)$Life.Expectancy.at.Birth..Females)

gr4 <- ggplot(filter(demographic, Year == 2020)) +
  geom_histogram(aes(x = Life.Expectancy.at.Birth..Both.Sexes, y = ..density..),
                 binwidth = 1, fill = "grey", color = "black") +  xlab("Life expectancy both sexes") + 
  ylab("Density") + ggtitle("Life expectancy both sexes") +
  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15)) 

max(filter(demographic, Year == 2020)$Life.Expectancy.at.Birth..Sexes) 
quantile(filter(demographic, Year==2020)$Life.Expectancy.at.Birth..Sexes)
getmode(filter(demographic, Year == 2020)$Life.Expectancy.at.Birth..Sexes)

combined <- gr1 + gr2 + gr3 + gr4 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

# Part 2
# Are there bivariate correlations between the variables?

graph1 <- filter(demographic, Year == 2020) %>% 
  ggplot( aes(Total.Fertility.Rate, Life.Expectancy.at.Birth..Females)) +
  ylab("Life expectancy females") + xlab("Fertility rate") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8) 


graph2 <- filter(demographic, Year == 2020) %>% 
  ggplot( aes(Total.Fertility.Rate, Life.Expectancy.at.Birth..Males)) +
  ylab("Life expectancy males") + xlab("Fertility rate") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)

graph3 <- filter(demographic, Year == 2020) %>% 
  ggplot( aes(Total.Fertility.Rate, Life.Expectancy.at.Birth..Both.Sexes)) +
  ylab("Life expectancy both sexes") + xlab("Fertility rate") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)

cor(demographic$Total.Fertility.Rate, demographic$Life.Expectancy.at.Birth..Both.Sexes , method = c("pearson"))
cor(demographic$Total.Fertility.Rate, demographic$Life.Expectancy.at.Birth..Males , method = c("pearson"))
cor(demographic$Total.Fertility.Rate, demographic$Life.Expectancy.at.Birth..Females , method = c("pearson"))


combined <- graph1 + graph2 + graph3 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

grapha1 <- filter(demographic, Year == 2020) %>% 
  ggplot( aes(Life.Expectancy.at.Birth..Both.Sexes, Life.Expectancy.at.Birth..Females)) +
  ylab("Life expectancy females") + xlab("Life expectancy both sexes") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)

grapha2 <- filter(demographic, Year == 2020) %>% 
  ggplot( aes(Life.Expectancy.at.Birth..Both.Sexes, Life.Expectancy.at.Birth..Males)) +
  ylab("Life expectancy males") + xlab("Life expectancy both sexes") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)

grapha3 <- filter(demographic, Year == 2020) %>% 
  ggplot( aes(Life.Expectancy.at.Birth..Males, Life.Expectancy.at.Birth..Females)) +
  ylab("Life expectancy female") + xlab("Fertility rate male") +  theme(plot.title = element_text(size=15,hjust = 0.5,face = "bold"),text = element_text(size=15))+
  geom_point(position = position_jitter(w = 0.04, h = 0.02), size = 1.8)

combined <- grapha1 + grapha2 + grapha3 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

cor(demographic$Life.Expectancy.at.Birth..Females, demographic$Life.Expectancy.at.Birth..Both.Sexes , method = c("pearson"))
cor(demographic$Life.Expectancy.at.Birth..Males, demographic$Life.Expectancy.at.Birth..Females , method = c("pearson"))
cor(demographic$Life.Expectancy.at.Birth..Males, demographic$Life.Expectancy.at.Birth..Both.Sexes , method = c("pearson"))

#Part 3
#Are the values of the individual variables comparatively homogeneous within
#subregions and heterogeneous between different subregions?

gr1 <-  ggplot(filter(demographic, Year == 2020)[order(demographic$Region),], aes(x=fct_inorder(Subregion), y=Total.Fertility.Rate, fill=Region))+
  geom_boxplot() + xlab("Subregions") + ylab("Female Fertility") + labs(fill = "Regions")+
  theme(plot.title = element_text(size=12,hjust = 0.5,face = "bold"),text = element_text(size=12)) + coord_flip() 

# Male and female life expectancy per region per continent in 2019
gr3 <- ggplot(filter(demographic, Year == 2020)[order(demographic$Region),], aes(x=fct_inorder(Subregion), y=Life.Expectancy.at.Birth..Both.Sexes, fill=Region))+
  geom_boxplot()+  xlab("Subregions") + ylab("life expectancy both sexes") + labs(fill = "Regions")+
  theme(plot.title = element_text(size=12,hjust = 0.5,face = "bold"),text = element_text(size=12)) + coord_flip() 


# Part 4
#How have the values of the variables changed over the last 20 years, i.e. comparing
#2000 with 2020?

Y2020<-filter(demographic, Year==2020)
Y2000<-filter(demographic, Year==2000)

df <- data.frame(matrix(ncol =7, nrow = 228))
x <- c("Country","Region","Subregion",  "fertility_rate","life_expectancy_both_sexes","life_expectancy_females","life_expectancy_males")
colnames(df) <- x
df[c("Country", "Region","Subregion")]<-unique(demographic[c("Country.Area.Name", "Region","Subregion")])
df$fertility_rate<-Y2020$Total.Fertility.Rate-Y2000$Total.Fertility.Rate
df$life_expectancy_both_sexes<-Y2020$Life.Expectancy.at.Birth..Both.Sexes-Y2000$Life.Expectancy.at.Birth..Both.Sexes
df$life_expectancy_females<-Y2020$Life.Expectancy.at.Birth..Females-Y2000$Life.Expectancy.at.Birth..Females
df$life_expectancy_males<-Y2020$Life.Expectancy.at.Birth..Males-Y2000$Life.Expectancy.at.Birth..Males

spec1 <- ggplot(df) +
  geom_histogram(aes(x = fertility_rate, y = ..density..),
                 bins=50, fill = "grey", color = "black") +  xlab("Fertility rate per woman") +
  ylab("Density") + ggtitle("Fertility rate difference on 2020 and 2000") +
  theme(plot.title = element_text(size=12,hjust = 0.5,face = "bold"),text = element_text(size=15))

quantile(df$fertility_rate)
skewness(df$fertility_rate)
getmode(df$fertility_rate)

spec2 <- ggplot(df) +
  geom_histogram(aes(x = life_expectancy_both_sexes, y = ..density..),
                 bins=50, fill = "grey", color = "black") +  xlab("Life expectancy both sexes") +
  ylab("Density") + ggtitle("Life expectancy difference on 2020 and 2000") +
  theme(plot.title = element_text(size=12,hjust = 0.5,face = "bold"),text = element_text(size=15))

quantile(df$life_expectancy_both_sexes)
skewness(df$life_expectancy_both_sexes)
getmode(df$life_expectancy_both_sexes)

combined <- spec1+spec2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")