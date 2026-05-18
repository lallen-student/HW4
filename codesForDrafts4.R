
# clean memory ------------------------------------------------------------
rm(list = ls())
library(dplyr)
library(ggplot2)
library(tidyr)

# read in data ------------------------------------------------------------
#set working directory

location="https://github.com/lallen-student/HW3/raw/main/"

file= "ncesdata_MASS_202425.xlsx"

link=paste0(location,file)

download.file(link,
              destfile = file,
              mode = "wb")

mydata <- readxl::read_xlsx(file, skip = 11)

# see data ----------------------------------------------------------


head(mydata)

glimpse(mydata)

summary(mydata)


# see data types ----------------------------------------------------------

str(mydata)

# deliverable 1 categorical variable/column ----------------------------------------------------------


cat_data <- mydata |> 
  mutate(
    locale_group = case_when(
      grepl("City", Locale) ~ "City",
      grepl("Suburban", Locale) ~ "Suburban",
      grepl("Town", Locale) ~ "Town",
      grepl("Rural", Locale) ~ "Rural")) |> 
  mutate(
    size_group = case_when(
      grepl("Large", Locale) ~ "Large",
      grepl("Midsize", Locale) ~ "Midsize",
      grepl("Small", Locale) ~ "Small",
      TRUE ~ "Other"
    )
  ) |>
  count(locale_group, size_group)

totals <- cat_data |>
  group_by(locale_group) |>
  summarize(total = sum(n))

cat_data2 <- ggplot(cat_data,
       aes(x = reorder(locale_group, n),
           y = n,
           fill = size_group)) +
  
  geom_col() +
  
  geom_text(data = totals,
            aes(x = locale_group,
                y = total,
                label = total),
            vjust = -0.5,
            inherit.aes = FALSE) + 
  
  labs(
    title = "Amount of Massachusetts Schools by Locale Group",
    x = "Locale Type",
    y = "Number of Schools",
    fill = "Locale Size",
    caption = "Source: NCES Massachusetts School Data"
  ) +
  
  theme_minimal()

cat_data2

# save del1Draft ----------------------------------------------------------

saveRDS(cat_data2, file = "del1Draft.rds")

# deliverable 2 numerical variable/column --------------------------------

num_data <- mydata |>
  filter(Students != "†",
         Students != "–")

num_data2 <- ggplot(num_data,
       aes(x = as.numeric(Students))) +
  
  geom_histogram(
    bins = 30,
    fill = "lightblue",
    color = "black"
  ) +
  
  labs(
    title = "Distribution of Student Enrollment in Massachusetts",
    x = "Amount of Students",
    y = "Amount of Schools",
    caption = "Source: NCES Massachusetts School Data"
  ) +
  
  theme_minimal()

num_data2

# save del2Draft ----------------------------------------------------------

saveRDS(num_data2, file = "del2Draft.rds")
