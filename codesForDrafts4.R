# clean memory -----------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(rio)
library("reshape2")
library(BBmisc)
library(patchwork)

#set working directory

# read in data 1 ---------------------------------------------------------------

location="https://github.com/lallen-student/HW4/raw/main/"

file1= "Mass_Arrests.xlsx"

link=paste0(location,file1)

download.file(link,
              destfile = file1,
              mode = "wb")

mydata1 <- readxl::read_xlsx(file1)

# see data ---------------------------------------------------------------------

head(mydata1)

glimpse(mydata1)

summary(mydata1)

# see data types ---------------------------------------------------------------

str(mydata1)

# deliverable 1 categorical variable/column ------------------------------------

catcat_data <- mydata1 |>
  count(Sex, Race)

catcat_plot <- ggplot(catcat_data,
                      aes(x = Race,
                          y = n,
                          fill = Sex)) +
  
  geom_col(position = "dodge") +
  
  labs(
    title = "Arrests by Race and Sex",
    x = "Race",
    y = "Number of Arrests",
    fill = "Sex",
    caption = "Source: Mass_Arrests.xlsx"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

catcat_plot

# save del1Draft ------------------------------------------------------------

saveRDS(catcat_plot, file = "del1Draft.rds")

# read in data 2 -------------------------------------------------------------

filename= "https://github.com/lallen-student/HW4/raw/main/covid-19.csv"

mydata2=read.csv(filename)

# see data -------------------------------------------------------------------

head(mydata2)

glimpse(mydata2)

summary(mydata2)

# see data types --------------------------------------------------------------

str(mydata2)
names(mydata2)

# deliverable 2 numerical variable/column -------------------------------------

num_num <- ggplot(mydata2,
                  aes(x = daily_cases_mean,
                      y = daily_deaths_mean)) +
  
  geom_point(alpha = 0.4,
             color = "darkred") +
  
  geom_smooth(method = "lm",
              se = FALSE,
              color = "blue") +
  
  labs(
    title = "COVID-19 Daily Cases and Daily Deaths",
    x = "Daily Cases Mean",
    y = "Daily Deaths Mean",
    caption = "Source: covid-19.csv"
  ) +
  
  theme_minimal()

num_num

# save del2Draft --------------------------------------------------------------

saveRDS(num_num, file = "del2Draft.rds")

# deliverable 3 numerical variable/column -------------------------------------

numcat_plot <- ggplot(mydata1,
                      aes(x = Race,
                          y = Age,
                          fill = Race)) +
  geom_boxplot() +
  
  labs(
    title = "Age Distribution by Race",
    x = "Race",
    y = "Age",
    caption = "Source: Mass_Arrests.xlsx" ) +
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none")

numcat_plot

# save del3Draft --------------------------------------------------------------

saveRDS(numcat_plot, file = "del3Draft.rds")

# read in data 4 --------------------------------------------------------------

location="https://github.com/lallen-student/HW4/raw/main/"

file4= "safeCities_ecoUI.xlsx"

link2=paste0(location,file4)

download.file(link2,
              destfile = file4,
              mode = "wb")

mydata4 <- readxl::read_xlsx(file4)

# see data  ------------------------------------------------------------------

head(mydata4)

glimpse(mydata4)

summary(mydata4)

# see data types ----------------------------------------------------------

str(mydata4,nchar.max = 60,strict.width='cut')

names(mydata4)

#data prep ---------------------------------------------------------------

drop <- c("Overall_Score")

keep <- setdiff(names(mydata4), drop)

safe <- mydata4[, keep]

safeL=melt(safe, id.vars = "city")

safeL$type <- ifelse(grepl("_In_", safeL$variable),
                     "input",
                     "output")

safeL$dimension <- substr(safeL$variable, 1, 1)

safe_avg <- aggregate(value ~ city + type + dimension,
                      data = safeL,
                      FUN = mean,
                      na.rm = TRUE)

newLevels <- c("DIGI","ENVIR","HEALTH","INFRA","PERS")

safe_avg$dimension <- factor(safe_avg$dimension,
                             levels = c("D","E","H","I","P"),
                             labels = newLevels,
                             ordered = FALSE)
# see data II ----------------------------------------------------------

head(safe_avg)

# deliverable 4 numerical variable/column --------------------------------
## original heat map ------------------------------------------------------------

safe_avg_out=safe_avg[safe_avg$type=='output',]

base_ave_out= ggplot(data = safe_avg_out)

vars_ave_out= base_ave_out + 
  aes(x = reorder(dimension, value,median),
      y =reorder(city, value, median))

geoms_ave_out=vars_ave_out + geom_tile(aes(fill = value))

original_plot <- geoms_ave_out + 
  scale_fill_gradient(low = 'white',
                      high = "black") +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   size = 8),
        axis.text.y = element_text(size = 6)) + labs(x="",y='')


original_plot

## improved heat map ------------------------------------------------------------

improved_plot <-  original_plot + 
  labs(
    title = "Safety Scores by City and Dimension",
    fill = "Average Score",
    caption = "Source: safeCities_ecoUI.xlsx") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 90,
                               hjust = 1,
                               size = 9),
    axis.text.y = element_text(size = 9) )

improved_plot

# save del4&5Draft ----------------------------------------------------------

saveRDS(original_plot, file = "del4Draft.rds")
saveRDS(improved_plot, file = "del5Draft.rds")
