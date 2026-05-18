# clean memory ------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(rio)
library(patchwork)

#set working directory

# read in data 1 ------------------------------------------------------------

location="https://github.com/lallen-student/HW4/raw/main/"

file1= "Mass_Arrests.xlsx"

link=paste0(location,file1)

download.file(link,
              destfile = file1,
              mode = "wb")

mydata1 <- readxl::read_xlsx(file1)

# see data ----------------------------------------------------------

head(mydata1)

glimpse(mydata1)

summary(mydata1)

# see data types ----------------------------------------------------------

str(mydata1)

# deliverable 1 categorical variable/column ----------------------------------------------------------

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

# save del1Draft ----------------------------------------------------------

saveRDS(catcat_plot, file = "del1Draft.rds")

# read in data 2 ------------------------------------------------------------

filename= "https://github.com/lallen-student/HW4/raw/main/covid-19.csv"

mydata2=read.csv(filename)

# see data ----------------------------------------------------------

head(mydata2)

glimpse(mydata2)

summary(mydata2)

# see data types ----------------------------------------------------------

str(mydata2)
names(mydata2)

# deliverable 2 numerical variable/column --------------------------------

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

# save del2Draft ----------------------------------------------------------

saveRDS(num_num, file = "del2Draft.rds")

# deliverable 3 numerical variable/column --------------------------------

numcat_plot <- ggplot(mydata1,
                      aes(x = Race,
                          y = Age,
                          fill = Race)) +
  
  geom_boxplot() +
  
  labs(
    title = "Age Distribution by Race",
    x = "Race",
    y = "Age",
    caption = "Source: Mass_Arrests.xlsx"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

numcat_plot

# save del3Draft ----------------------------------------------------------

saveRDS(numcat_plot, file = "del3Draft.rds")

# read in data for del4 ------------------------------------------------------------

location="https://github.com/lallen-student/HW4/raw/main/"

file4= "safeCities_ecoUI.xslx"
# clean memory ------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

#set working directory

# read in data 1 ------------------------------------------------------------

location="https://github.com/lallen-student/HW4/raw/main/"

file1= "Mass_Arrests.xlsx"

link=paste0(location,file1)

download.file(link,
              destfile = file1,
              mode = "wb")

mydata1 <- readxl::read_xlsx(file1)

# see data ----------------------------------------------------------

head(mydata1)

glimpse(mydata1)

summary(mydata1)

# see data types ----------------------------------------------------------

str(mydata1)

# deliverable 1 categorical variable/column ----------------------------------------------------------

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

# save del1Draft ----------------------------------------------------------

saveRDS(catcat_plot, file = "del1Draft.rds")

# read in data 2 ------------------------------------------------------------

filename= "https://github.com/lallen-student/HW4/raw/main/covid-19.csv"

mydata2=read.csv(filename)

# see data ----------------------------------------------------------

head(mydata2)

glimpse(mydata2)

summary(mydata2)

# see data types ----------------------------------------------------------

str(mydata2)
names(mydata2)

# deliverable 2 numerical variable/column --------------------------------

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

# save del2Draft ----------------------------------------------------------

saveRDS(num_num, file = "del2Draft.rds")

# deliverable 3 numerical variable/column --------------------------------

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

# save del3Draft ----------------------------------------------------------

saveRDS(numcat_plot, file = "del3Draft.rds")

# read in data for del4 ------------------------------------------------------------

location="https://github.com/lallen-student/HW4/raw/main/"

file4= "safeCities_ecoUI.xlsx"

link2=paste0(location,file4)

download.file(link2,
              destfile = file4,
              mode = "wb")

mydata4 <- readxl::read_xlsx(file4)

# see data  ----------------------------------------------------------

head(mydata4)

glimpse(mydata4)

summary(mydata4)

# see data types ----------------------------------------------------------

str(mydata4)

names(mydata4)

# deliverable 4 numerical variable/column --------------------------------

original_plot <- ggplot(mydata4,
                        aes(x = Overall_Score,
                            y = H_Out_LifeExpectancyYears)) +
  geom_point() +
  
  labs(
    title = "Original Plot"
  )



improved_plot <- ggplot(mydata4,
                        aes(x = Overall_Score,
                            y = H_Out_LifeExpectancyYears,
                            color = city)) +
  geom_point(size = 3,
             alpha = 0.7) +
  labs(
    title = "Improved Plot of Overall Safety and Life Expectancy",
    x = "Overall Safety Score",
    y = "Life Expectancy",
    color = "City",
    caption = "Source: safeCities_ecoUI.xlsx"
  ) +
  
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",
                                  size = 14))
  
original_plot

improved_plot

# side by side ------------------------------------------------------------

del4Draft <- original_plot + improved_plot

del4Draft

# save del4Draft ----------------------------------------------------------

saveRDS(del4Draft, file = "del4Draft.rds")
