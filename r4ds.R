# R for Data Science
# https://r4ds.hadley.nz/
# Data science: Turning data into understanding, insight, and knowledge.

# Data frame: Collection of variables (columns) and observations (rows)

library(tidyverse)

# Packages are simply units of reproducible R code. 
# However, we don't want to spend more time coding or copying and pasting, so
# we import a package and call the functions coded in the package.

# Here are other packages that will be used at some point:
install.packages(
  c("arrow", "babynames", "curl", "duckdb", "gapminder", 
    "ggrepel", "ggridges", "ggthemes", "hexbin", "janitor", "Lahman", 
    "leaflet", "maps", "nycflights13", "openxlsx", "palmerpenguins", 
    "repurrrsive", "tidymodels", "writexl"))


# 1 Data Visualization ----------------------------------------------------

library(palmerpenguins) # Penguin body measurements
library(ggthemes) # Colorblind safe color palette

# Describing relationships:
# Direction - positive, negative
# Form - linear, quadratic, exponential
# Strength - strong, moderate, weak

penguins
glimpse(penguins) # See all variables and the first few observations

# Scatterplot of body mass and flipper length, colored by species
# Moderately strong, positive, linear relationship between flipper length and body mass
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

# Scatterplot of bill depth and bill length
# No obvious relationship between bill depth and bill length
ggplot(
  data = penguins,
  mapping = aes(x = bill_depth_mm, y = bill_length_mm)
) +
  geom_point() +
  labs(
    caption = "Data comes from the palmerpenguins package"
  )

# Scatterplot of species and bill depth
# Boxplot may be more applicable
ggplot(
  data = penguins,
  mapping = aes(x = species, y = bill_depth_mm)
) +
  geom_point()

# geom_point(na.ra = TRUE) -> removes missing values without a warning

# Scatterplot of flipper length and body mass, colored by bill depth.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = bill_depth_mm)) +
  geom_smooth() # If no method, default = loess (local regression)

# Scatterplot of flipper length and body mass, colored by island
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE) # se displays the confidence interval, default is TRUE

# Alternative ways of calling plot codes
# 1 - Drop data and mapping
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

# 2 - Pipe
penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

# Bar charts are best used for categorical variables - takes one of small set of values
# Bar chart with counts of species
ggplot(penguins, aes(x = fct_infreq(species))) + 
  geom_bar()

ggplot(penguins, aes(y = fct_infreq(species))) + 
  geom_bar(color = "black", fill = "red")
# fct_infreq orders the categories from most to least count

# Histograms are best used for numerical variables - takes a wide range of numeric values
# Histogram of body mass
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
# Size of the bin width helps us determine the shape of the distribution

# Histogram of body mass with 10 bins
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(bins = 10)

# Alternative for numerical is the density plot - smoothed out histogram
# "Dropping a cooked spaghetti string over the histogram"
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

# Distribution of body mass, colored by species
ggplot(penguins, aes(x = body_mass_g, color = species)) + 
  geom_density(linewidth = 0.75)

ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) + 
  geom_density(alpha = 0.50)

# Boxplot - Measure of position, shows the distribution for a numerical attribute
# Compare across categorical variable by stacking side-by-side
# Outlier = Greater than 75th + 1.5 IQR or less than 25th - 1.5 IQR
# Whisker ends at farthest non-outlier point

ggplot(penguins, aes(x = species, y = body_mass_g)) + 
  geom_boxplot()

# Distribution of species within each island
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill") # relative frequency of each fill
# We see that gentoo are on Biscoe Island and make up 75% of penguins there.
# Chinstrap are on Dream Island and make up roughly 50% of penguins there.
# Adelie are on all three islands and make up all of Torgersen Island.

# 3+ variables - We can just keep mapping them to additional aesthetics.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))

# Some graphs can become too busy. We split into facets or subplots.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)
# If there are 3 island cateogries, then facet_wrap will create 3 different plots.

# Scatterplot of hwy miles per gallon and engine displacement
# color and size by cty miles per gallon
# shape by type of drive train
ggplot(mpg, aes(x = hwy, y = displ, color = cty, size = cty, shape = drv)) + 
  geom_point()

# Scatterplot of bill depth and bill length, colored or faceted by species
ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm, color = species)) + 
  geom_point()

ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) + 
  geom_point() +
  facet_wrap(~species)

# Saving a plot
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin-plot.png", width = 6, height = 4)
# Image will save the last executed plot to the current working directory

# Alternative:
scatter <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin-plot.png", plot = scatter)


# 2 Workflow: Basics ------------------------------------------------------

# Assigning a variable or vector
x <- 10
prime <- c(2,3,5,7,11,13)
prime * 2
prime - 1

x <- "hello world"
seq(from = 1, to = 10)
seq(1,10)


# 3 Data Transformation ---------------------------------------------------

library(nycflights13)
flights
glimpse(flights)

# Average delay time for flights going to IAH, grouped by year, month, and day
flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(arr_delay = mean(arr_delay, na.rm = TRUE))

# All flights that departed more than 2 hours late
flights |> 
  filter(dep_delay > 120)

# Use & for AND and | for OR in condition statement.
# All flights departing on January 1
flights |> 
  filter(day == 1 & month == 1)

# All flights departing in January or February
flights |>
  filter(month == 1 | month == 2)
# Alternative: Use in
flights |>
  filter(month %in% c(1,2))

# Save after filter into new tibble since flights won't be overwritten
jan1 <- flights |> 
  filter(day == 1 & month == 1)

# Arrange flights by departure time
flights |>
  arrange(year, month, day, year, dep_time)

# Ordering flights from most to least delayed
flights |>
  arrange(desc(dep_delay))

# Remove duplicate rows, if any
flights |> 
  distinct()

# Find unique origin-destination pairs
flights |>
  distinct(origin, dest)

# How to keep other columns when manipulating the rows?
# It will find the first occurrence of distinct and then discard the rest.
# This is why most of the rows are from Jan 1.
flights |>
  distinct(origin, dest, .keep_all = TRUE)

# Number of occurrences of origin-destination pairs
flights |>
  count(origin, dest, sort = TRUE)

# Arrival delay of 2+ hours
flights |>
  filter(arr_delay >= 120) |>
  arrange(desc(arr_delay))

# Operated by United, American, or Delta
flights |>
  filter(carrier %in% c("UA","DL","AA"))

# Arrvied more than 2 hours late but didn't leave late
flights |>
  filter(dep_delay <= 0 & arr_delay >= 120)

# Delayed by at least an hour but made up over 30 minutes during the flight
flights |>
  filter(dep_delay >= 60 & dep_delay - arr_delay > 30)

# Sort flights but those that left earliest in the morning
flights |> 
  arrange(sched_dep_time)

# Fastest flights
flights |> 
  mutate(speed = distance / (air_time / 60)) |>
  arrange(desc(speed)) |>
  relocate(speed, distance, air_time)

# Was there a flight on everyday of 2013?
flights |>
  distinct(year, month, day) |>
  nrow()

# Flights that traveled the furthest/least
flights |>
  arrange(desc(distance)) |>
  relocate(distance)

flights |>
  arrange(distance) |>
  relocate(distance)

# Create new variables for time gained on a delay and speed
# .before = 1 adds them to the left of the tibble
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance /(air_time * 60),
    .before = 1
  )

# Alternative: Move after the day variable
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance /(air_time * 60),
    .after = day
  )

# Keep the variables involved in the mutate step
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

# Select year, month, and day variables
flights |>
  select(year, month, day)

flights |>
  select(year:day)

# Select all but year, month, and day
flights |>
  select(!year:day)

# Select all character columns
flights |> 
  select(where(is.character))

# Rename variables
flights |>
  select(tail_num = tailnum)

flights |>
  rename(tail_num = tailnum)

# Look into janitor::clean_names() for automatically cleaning variable names.

# Moving variables to the front
flights |> 
  relocate(time_hour, air_time)

# Select all arrival and departing variables
flights |> 
  select(starts_with("dep"), starts_with("arr"))

# Select a group of variables
variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |> 
  select(any_of(variables))

# Fastest flights to Houston's IAH airport
flights |>
  filter(dest == "IAH") |> 
  mutate (speed = distance / air_time * 60) |>
  select(year:day, dep_time, carrier, flight, speed) |>
  arrange(desc(speed))

# Average departure delay by month
flights |>
  group_by(month) |>
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE))

# Number of flights by month
flights |>
  group_by(month) |>
  summarize(n = n())

# df |> slice_head(n = 1) takes the first row from each group
# df |> slice_tail(n = 1) takes the last row in each group
# df |> slice_min(x, n = 1) takes the row with the smallest value of column x
# df |> slice_max(x, n = 1) takes the row with the largest value of column x
# df |> slice_sample(n = 1) takes one random row

# Flights that are most delayed upon arrival at each destination
flights |>
  group_by(dest) |>
  slice_max(arr_delay, n = 1) |>
  relocate(dest)

# Number of flights by day
flights |>
  group_by(year, month, day) |>
  summarize(n = n())

# Average delay and total number of flights
flights |>
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE),
            n = n())

# Alternative to group_by
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = c(origin, dest)
  )

# Carrier with the worst average delays
flights |>
  group_by(carrier) |>
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  arrange(desc(avg_dep_delay))

# Top 5 most departure delayed flights by destination
flights |>
  group_by(dest) |>
  arrange(dest, desc(dep_delay)) |>
  slice_head(n = 5) |>
  relocate(dest, dep_delay)

# How do delays vary over course of a day?
flights |>
  group_by(hour) |>
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot(aes(x = hour, y = avg_dep_delay)) + 
  geom_smooth()

library(Lahman)
batters <- Batting |> 
  group_by(playerID) |> 
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
batters

# Law of large numbers: Variation decreases as the sample size increases
batters |> 
  filter(n > 100) |> 
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 1 / 10) + 
  geom_smooth(se = FALSE)

career <- Batting %>%
  filter(AB > 0) %>%
  anti_join(Pitching, by = "playerID") %>%
  group_by(playerID) %>%
  summarize(H = sum(H), AB = sum(AB)) %>%
  mutate(average = H / AB)

# Use names along with the player IDs
career <- Master %>%
  tbl_df() %>%
  select(playerID, nameFirst, nameLast) %>%
  unite(name, nameFirst, nameLast, sep = " ") %>%
  inner_join(career, by = "playerID") %>%
  select(-playerID)

career_filtered <- career |>
  filter(AB >= 500)

# Find coefficients for the fitted beta distribution
m <- MASS::fitdistr(career_filtered$average, dbeta, 
                    start = list(shape1 = 1, shape2 = 10))
alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

ggplot(career_filtered) +
  geom_histogram(aes(x = average, y = ..density..), binwidth = .005) +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), color = "red",
                size = 1) +
  xlab("Batting average")

# Estimating batting average
career_eb <- career |>
  mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0)) |>
  arrange(desc(eb_estimate))
career_eb

# Ouliers are brought closer to average
ggplot(career_eb, aes(average, eb_estimate, color = AB)) +
  geom_hline(yintercept = alpha0 / (alpha0 + beta0), color = "red", lty = 2) +
  geom_point() +
  geom_abline(color = "red") +
  scale_colour_gradient(trans = "log", breaks = 10 ^ (1:5)) +
  xlab("Batting average") +
  ylab("Empirical Bayes batting average")

# Integer division and modulus
flights |> 
  mutate(
    speed = distance / air_time,
    dep_hour = dep_time %/% 100, # Integer division -> 517 / 100 = 5
    dep_minute = dep_time %%  100 # Modulus (remainder) -> 517 mod 100 = 17
  ) |>
  relocate(speed, dep_hour, dep_minute)

# Comparison of speed versus distances
flights |> 
  group_by(dest) |> 
  summarize(
    distance = mean(distance),
    speed = mean(distance / air_time, na.rm = TRUE)
  ) |> 
  ggplot(aes(x = distance, y = speed)) +
  geom_smooth(
    method = "loess",
    span = 0.5,
    se = FALSE, 
    color = "white", 
    linewidth = 4
  ) +
  geom_point()


# 5 Data Tidying -------------------------------------------------------------
# Make a new section using Cmmd + Shift + R

# What makes a dataset tidy?
# Each variable is a column. Each column is a variable. 
# Each observation is a row. Each row is an observation.
# Each value is a cell. Each cell is a single value.

country <- c("Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", 
             "Brazil", "Brazil", "Brazil", "Brazil",
             "China", "China", "China", "China")
year <- c(1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000)
type <- c("cases", "population", "cases", "population", "cases", "population",
          "cases", "population", "cases", "population", "cases", "population")
count <- c(745, 19987071, 2666, 20595360, 37737, 172006362, 80488, 174504898,
           212258, 1272915272, 213766, 1280428583)

table2 <- tibble(country, year, type, count)

country2 <- c("Afghanistan", "Afghanistan", "Brazil", "Brazil", "China", "China")
year2 <- c(1999, 2000, 1999, 2000, 1999, 2000)
rate <- c("745/19987071", "2666/20595360", "37737/172006362", "80488/174504898",
          "212258/1272915272", "213766/1280428583")

table3 <- tibble(country2, year2, rate)


# pivot_wider -> Increases the number of columns and decreases the number of rows
table2 |>
  pivot_wider(
    names_from = type,
    values_from = count
  ) |> 
  mutate(rate = cases / population * 10000)

# separate_wider_delim -> Splits a string column into multiple new columns
table3 |>
  separate_wider_delim(
    cols = rate, 
    delim = "/", 
    names = c("cases", "population"),
  ) |>
  mutate(
    cases = as.numeric(cases),
    population = as.numeric(population),
    rate = cases / population * 10000)

billboard
glimpse(billboard)

# The problem with the wk variables is the cell value is itself another variable,
# the rank of the song.

billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )

billboard_longer <- billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week) # Takes only the number out of the string
  )
billboard_longer

# How the rank of a song changes over time?
billboard_longer |>
  ggplot(aes(x = week, y = rank, group = track)) + 
  geom_line(alpha = 0.25) + 
  scale_y_reverse()

# Problem where values end up in column names
df <- tribble(
  ~id,  ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
)

df |> 
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )

# Using a separator to break up column into multiple columns
who2
glimpse(who2)

who2 |> 
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"), 
    names_sep = "_",
    values_to = "count"
  )

household
glimpse(household)

household |> 
  pivot_longer(
    cols = !family, 
    names_to = c(".value", "child"), 
    names_sep = "_", 
    values_drop_na = TRUE
  )

cms_patient_experience
glimpse(cms_patient_experience)

# Appears to be six surveys, creating six rows for each organization
cms_patient_experience |> 
  distinct(measure_cd, measure_title)

# Making it wider
cms_patient_experience |> 
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )

df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "B",        "bp1",    140,
  "B",        "bp2",    115, 
  "A",        "bp2",    120,
  "A",        "bp3",    105
)

df |> 
  pivot_wider(
    names_from = measurement,
    values_from = value
  )


# 7 Data Import -----------------------------------------------------------

ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_hex()
ggsave("diamonds.png")

write_csv(diamonds, "data/diamonds.csv")

# You should never use absolute paths in scripts, because they hinder sharing. 
# No one else will have the same working directory as you.

# For importing, need readr package, which is part of the tidyverse.
students <- read_csv("data/students.csv")
students <- read_csv("https://pos.it/r4ds-students-csv")

# Properly address NA arguments
students <- read_csv("data/students.csv", na = c("N/A", ""))

# Rename non-syntactic variables
students |>
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )

# Right now, mealPlan is a character variable, however, there are limited choices.
# So, it is a categorical variable (factor in R)
students |>
  rename(meal_plan = mealPlan) |>
  mutate(meal_plan = factor(meal_plan))

# Age is a character variable since one of the observations was "five"
# Change this to numeric
students <- students |>
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`,
    meal_plan = mealPlan,
    age = AGE
  ) |>
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(ifelse(age == "five", 5, age))
  )

# read_csv can also read input text strings
read_csv(
  "a,b,c
  1,2,3
  4,5,6"
)

# Use skip to skip the first n lines
read_csv(
  "The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3",
  skip = 2
)

# Use comment to drop all lines start with a particular symbol
read_csv(
  "# A comment I want to skip
  x,y,z
  1,2,3",
  comment = "#"
)

# Use col_names = FALSE to indicate that the csv has no column names
read_csv(
  "1,2,3
  4,5,6",
  col_names = FALSE
)

# Or you can pass your own column names
read_csv(
  "1,2,3
  4,5,6",
  col_names = c("x", "y", "z")
)

# read_csv2() is for semicolon separated files
# read_tsv() is for tab separated files
# read_delim() guesses the delimiter or add argument, delim = "|"

# Practice with non-syntactic names
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)

# Extracting var `1`
annoying |>
  select(`1`)

# Scatterplot of `1` vs. `2`
ggplot(annoying, aes(x = `2`, y = `1`)) + 
  geom_point()

# New column `3` which is `2` divided by `1`
annoying <- annoying |>
  mutate(`3` = `2` / `1`)

# Renaming columns
annoying |>
  rename(
    one = `1`,
    two = `2`,
    three = `3`
  )

# Example showing readr's data type guessing heuristics
read_csv("
  logical,numeric,date,string
  TRUE,1,2021-01-15,abc
  false,4.5,2021-02-15,def
  T,Inf,2021-02-16,ghi
")

simple_csv <- "
  x
  10
  .
  20
  30"

read_csv(simple_csv)
# This makes x a chr column because of the period.
# Override it to be a float variable.
df <- read_csv(
  simple_csv, 
  col_types = list(x = col_double())
)

# It warns us about problems. Find out more with problems() command
problems(df)
# Tells us that there is a . in (3,1). So we set na equal to this symbol.
read_csv(simple_csv, na = ".")

# Some variations:
# col_logical()
# col_integer() -> No diff from float other than using less memory
# col_character() -> Useful for number than won't involve arithmetic operations
#                    like credit cards, student ids
# col_factor()
# col_date()
# col_datetime()
# col_number() -> Like parse_number, useful for currencies
# col_skip() -> Skips variables that you don't want to include

# Override all columns to character
another_csv <- "
x,y,z
1,2,3"

read_csv(
  another_csv, 
  col_types = cols(.default = col_character()))

# Use cols_only to read in specified columns
read_csv(
  another_csv,
  col_types = cols_only(x = col_character())
)

# Importing multiple files at once - think monthly sales numbers in multiple files
sales_files <- c(
  "https://pos.it/r4ds-01-sales",
  "https://pos.it/r4ds-02-sales",
  "https://pos.it/r4ds-03-sales"
)
read_csv(sales_files, id = "file")

# Writing to a file
write_csv(students, "data/students-2.csv")

# write_rds() and read_rds() store data in R's custom binary format. 
# When you reload, you are reloading the exact same object. 
# (Won't have to convert to factor again)

write_rds(students, "data/students.rds")
read_rds("data/students.rds")

# Data entry by columns
tibble(
  x = c(1, 2, 5), 
  y = c("h", "m", "g"),
  z = c(0.08, 0.83, 0.60)
)

# Data entry by rows
tribble(
  ~x, ~y, ~z,
  1, "h", 0.08,
  2, "m", 0.83,
  5, "g", 0.60
)


# 8 Workflow: Getting Help ------------------------------------------------

library(reprex)

# To make example reproducible: 
# Packages loaded at the top of script.
# Include data - try to use smallest subset of data that still reveals the problem. 
# Ensure code is easy for others to read - variable names, comments, removing unrelated info.
# Use reprex::reprex()

y <- 1:4
mean(y)
# Copy this to clipboard and then run reprex::reprex() to get nicely rendered format


# 9 Visualize - Layers ----------------------------------------------------

mpg
glimpse(mpg)

# Scatterplot of hwy fuel efficiency versus engine size
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "red")

# Scatterplot of hwy fuel efficiency versus engine size, coloured by class
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

# Same scatterplot but with pink filled in triangles
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(shape = 17, color = "pink")

# Mapping color to a condition rather than a variable
ggplot(mpg, aes(x = hwy, y = displ, color = (class == "suv"))) + 
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_smooth()

# Class is local to the point, not the smooth line (abides to global)
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth()

# Overriding an argument - Two-seaters in red
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_point(
    data = mpg |> filter(class == "2seater"), 
    color = "red"
  )

# Facet_wrap to split into subplots
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~cyl)

# Subplots for combination of two variables - facet_grid()
# Split by type of drive and number of cylinders
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl)

# Allow for different scales on the subplots
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl, scales = "free_y")

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# Highlighting subcompact cars
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "black") +
  geom_point(
    data = mpg |> filter(class == "compact"),
    color = "pink"
  )

# Compare engine size across cars with different drive trains
ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ .)

# Count of diamonds by cut
ggplot(diamonds, aes(x = cut)) + 
  geom_bar()

# Proportion of diamonds by cut - using after_stat
ggplot(diamonds, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar()

# Stat summary of depth, grouped by cut
ggplot(diamonds) + 
  stat_summary(
    aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

# Equivalent
diamonds |>
  group_by(cut) |>
  summarize(
    lower = min(depth),
    upper = max(depth),
    midpoint = median(depth)
  ) |>
  ggplot(aes(x = cut, y = midpoint)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  labs(y = "depth")

# Counts of drive and class
ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar()

# Positioned side-by-side instead of stacked
ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "dodge")

# Some values are rounded to avoid overlapping on grid.
# Use position = "jitter" to remove
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(position = "jitter")

# Alternative
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_jitter()

# Number of points in a given location
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_count()

# Map of New Zealand
nz <- map_data("nz")
ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

# Polar coordinates
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = clarity, fill = clarity), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1)

bar + coord_flip()
bar + coord_polar()

# Pie chart
ggplot(diamonds, aes(x = "", fill = cut)) +
  geom_bar() + 
  coord_polar(theta = "y")


# 10 Visualize - Exploratory Data Analysis --------------------------------

# John Tukey:
# The greatest value of a picture is when it forces us to notice what we 
# never expected to see.
# 
# Far better an approximate answer to the right question than an exact answer 
# to the wrong question. 


# Like most creative processes, the key to asking quality questions is to 
# generate a large quantity of questions.

# Two types:
# What type of variation occurs within the variables?
# What type of covariation occurs between the variables?

# Other questions:
# Which values are most common? Most rare? Why? Does it match expectations?
# Which values are outliers? Are they reasonable or potential entry mistakes 
# that need to be tidied (with NAs)?

# Once you find a pattern:
# Describe the relationship - direction, form, strength
# Reasoning/context for relationship.
# What other variables might affect it?
# Does the relationship change across individual subgroups?

# Distribution of weights of diamonds
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.5)

# Distribution of weights of diamonds (smaller bin width and filtered for < 3)
smaller <- diamonds |> 
  filter(carat < 3)

ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)
# Why are there more diamonds at whole carats?
# Why are there more diamonds to the right of peaks than to the left?

# Zooming in to look for outliers in the histogram
ggplot(diamonds, aes(x = y)) + 
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds |> 
  filter(y < 3 | y > 20) |> 
  select(price, x, y, z) |>
  arrange(y)
unusual

ggplot(diamonds, aes(x = x)) +
  geom_histogram(binwidth = 0.5)
ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 0.5)
ggplot(diamonds, aes(x = z)) +
  geom_histogram(binwidth = 0.5)

# Playing around with different bin widths
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 0.5)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 5)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 10)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 50)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 100)
# Seems to be a big gap missing at around 1500
ggplot(diamonds, aes(x = price)) + 
  geom_histogram(binwidth = 100) +
  coord_cartesian(xlim = c(500, 5000))

diamonds |>
  filter(carat %in% c(0.99, 1.00)) |>
  group_by(carat) |>
  summarise(n = n())

# Mutate unusual values with NA
diamonds2 <- diamonds |> 
  mutate(y = if_else(y < 3 | y > 20, NA, y))

flights |> 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  ) |> 
  ggplot(aes(x = sched_dep_time)) + 
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4) +
  facet_wrap(~cancelled)

# Comparing price of the diamond with its quality (cut)
ggplot(diamonds, aes(x = price)) + 
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

# Use the density instead - count is standardized so that the area underneath is 1
ggplot(diamonds, aes(x = price, y = after_stat(density))) + 
  geom_freqpoly(aes(color = cut), binwidth = 500, linewidth = 0.75)

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot()

# Reorder the boxes in ascending order of median highway efficiency
ggplot(mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) +
  geom_boxplot() +
  labs(x = "class")

ggplot(diamonds, aes(x = cut, y = price)) + 
  geom_violin()

# Count observations of categorical variables
ggplot(diamonds, aes(x = cut, y = color)) +
  geom_count()

# Alternative
diamonds |> 
  count(color, cut)

diamonds |> 
  count(color, cut) |>  
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = n))

ggplot(diamonds) +
  geom_bar(aes(x = color, fill = cut))

# Add transparency to scatterplots with lots of points
ggplot(smaller, aes(x = carat, y = price)) + 
  geom_point(alpha = 1 / 100)

library(hexbin)
ggplot(smaller, aes(x = carat, y = price)) +
  geom_bin2d()

ggplot(smaller, aes(x = carat, y = price)) +
  geom_hex()

# Turn a continuous into a categorical with cut_width
ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

ggplot(smaller, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_number(carat, 20))) # 20 points in each

ggplot(smaller, aes(x = price, y = carat)) + 
  geom_boxplot(aes(group = cut_width(price, 1000)))

# Basic models
diamonds <- diamonds |>
  mutate(
    log_price = log(price),
    log_carat = log(carat)
  )

library(tidymodels)

diamonds_fit <- linear_reg() |>
  fit(log_price ~ log_carat, data = diamonds)

diamonds_aug <- augment(diamonds_fit, new_data = diamonds) |>
  mutate(.resid = exp(.resid))

# Residuals for price versus carat model
ggplot(diamonds_aug, aes(x = carat, y = .resid)) + 
  geom_point()


# 11 Visualize - Communication --------------------------------------------

library(scales)
library(ggrepel)
library(patchwork)

# Labelling plots
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    color = "Car type",
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )

# Incorporate math equations using quote
df <- tibble(
x = 1:10,
y = cumsum(x^2)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(x[i]),
    y = quote(sum(x[i] ^ 2, i == 1, n))
  )

ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  labs(x = "City MPG",
       y = "Highway MPG",
       color = "Type of drive train",
       shape = "Type of drive train")

label_info <- mpg |>
  group_by(drv) |>
  arrange(desc(displ)) |>
  slice_head(n = 1) |>
  mutate(
    drive_type = case_when(
      drv == "f" ~ "front-wheel drive",
      drv == "r" ~ "rear-wheel drive",
      drv == "4" ~ "4-wheel drive"
    )
  ) |>
  select(displ, hwy, drv, drive_type)

label_info

# Labels directly on the graph
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  geom_text(
    data = label_info, 
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold", size = 5, hjust = "right", vjust = "bottom"
  ) +
  theme(legend.position = "none")

# Use geom_label_repel for better fit
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  geom_label_repel(
    data = label_info, 
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold", size = 5, nudge_y = 2
  ) +
  theme(legend.position = "none")

# Highlighting the outliers
potential_outliers <- mpg |>
  filter(hwy > 40 | (hwy > 20 & displ > 5))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_text_repel(data = potential_outliers, aes(label = model)) +
  geom_point(data = potential_outliers, color = "red") +
  geom_point(
    data = potential_outliers,
    color = "red", size = 3, shape = "circle open"
  ) +
  geom_vline(xintercept = 4)

# Annotating plots
trend_text <- "Larger engine sizes tend to have lower fuel economy." |>
  str_wrap(width = 30)
trend_text

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  annotate(
    geom = "label", x = 3.5, y = 38,
    label = trend_text,
    hjust = "left", color = "red"
  ) +
  annotate(
    geom = "segment",
    x = 3, y = 35, xend = 5, yend = 25, color = "red",
    arrow = arrow(type = "closed")
  )

# Playing around with the scales
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  scale_color_discrete(labels = c("4" = "4-wheel", "f" = "front", "r" = "rear"))

ggplot(diamonds, aes(x = price, y = cut)) +
  geom_boxplot(alpha = 0.05) +
  scale_x_continuous(
    labels = label_dollar(scale = 1/1000, suffix = "K"), 
    breaks = seq(1000, 19000, by = 6000)
  )

ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Percentage", labels = label_percent())

# Lengths of presidency terms
presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(name = NULL, breaks = presidential$start, date_labels = "'%y")

base <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class))
base + 
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 3, override.aes = list(size = 4)))

# Color blindness friendly
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  scale_color_brewer(palette = "Set1")

# Coloring difference between democrats and republicans
presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(values = c(Republican = "#E81B23", Democratic = "#00AEF3"))

# Heatmaps
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = year)) +
  scale_colour_gradient2(midpoint = 2004)

# Various themes to plots
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme_bw()

# Arranging plots beside each other
p1 <- ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 1")
p2 <- ggplot(mpg, aes(x = drv, y = hwy)) + 
  geom_boxplot() + 
  labs(title = "Plot 2")
p1 + p2

p3 <- ggplot(mpg, aes(x = cty, y = hwy)) + 
  geom_point() + 
  labs(title = "Plot 3")
(p1 | p3) / p2

p1 <- ggplot(mpg, aes(x = drv, y = cty, color = drv)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 1")

p2 <- ggplot(mpg, aes(x = drv, y = hwy, color = drv)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "Plot 2")

p3 <- ggplot(mpg, aes(x = cty, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 3")

p4 <- ggplot(mpg, aes(x = hwy, color = drv, fill = drv)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Plot 4")

p5 <- ggplot(mpg, aes(x = cty, y = hwy, color = drv)) + 
  geom_point(show.legend = FALSE) + 
  facet_wrap(~drv) +
  labs(title = "Plot 5")

(guide_area() / (p1 + p2) / (p3 + p4) / p5) +
  plot_annotation(
    title = "City and highway mileage for cars with different drive trains",
    caption = "Source: https://fueleconomy.gov."
  ) +
  plot_layout(
    guides = "collect",
    heights = c(1, 3, 2, 4)
  ) &
  theme(legend.position = "top")


# 12 Transform - Logical Vectors ------------------------------------------

# Any manipulation that can be done to a vector can be done inside a data frame with mutate
x <- c(1, 2, 3, 5, 7, 11, 13)
x * 2

df <- tibble(x)
df |> 
  mutate(y = x * 2)

# Daytime departures that roughly arrive on time
flights |> 
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)

# You can mutate first to see for which observations the conditions hold
flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
    .keep = "used"
  )

# Almost any option involving a missing value will be unknown.
NA > 5
NA == 0

# is.na returns true for missing values and false for everything else
is.na(c(TRUE, NA, FALSE))

# Find missing departure times
flights |> 
  filter(is.na(dep_time))

# Exclusive or - one or the other
xor(2 == 2, 4 == 4)
# Returns FALSE because both are true

flights |> 
  filter(month %in% c(11, 12))

# Flights where arr_delay is missing but dep_delay is not
flights |>
  filter(is.na(arr_delay) & is.na(dep_delay) == FALSE)

# Check if all or any within a group satisfy a condition
flights |> 
  group_by(year, month, day) |> 
  summarize(
    all_delayed = all(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

# Numeric summaries of boolean might be more helpful
flights |> 
  group_by(year, month, day) |> 
  summarize(
    proportion_delayed = mean(dep_delay <= 60, na.rm = TRUE),
    count_long_delay = sum(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

# Number of and average delays
flights |> 
  filter(arr_delay > 0) |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay),
    n = n(),
    .groups = "drop"
  )

# What if you want those that arrive early too - filter within the summarize
flights |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    ahead = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Proportion of flights with missing arr_delay
flights |> 
  group_by(year, month, day) |> 
  summarize(
    missing = mean(is.na(arr_delay)),
    .groups = "drop"
  )

# Using if_else
x <- c(-3:3, NA)
if_else(x > 0, "+ve", "-ve")

# Imitating absolute value
if_else(x < 0, -x, x)

# Nested if_else
if_else(x == 0, "0", if_else(x < 0, "-ve", "+ve"), "???")

# When it gets too complex, use case_when
case_when(
  x == 0   ~ "0",
  x < 0    ~ "-ve", 
  x > 0    ~ "+ve",
  is.na(x) ~ "???"
)

flights |> 
  mutate(
    status = case_when(
      is.na(arr_delay)      ~ "cancelled",
      arr_delay < -30       ~ "very early",
      arr_delay < -15       ~ "early",
      abs(arr_delay) <= 15  ~ "on time",
      arr_delay < 60        ~ "late",
      arr_delay < Inf       ~ "very late",
    ),
    .keep = "used"
  )

x <- c(0:20)
if_else(x %% 2 == 0, x, FALSE)


# 13 Transform - Numbers --------------------------------------------------

# Converting strings to numeric
x <- c("1.2", "5.6", "1e3")
parse_double(x)

# parse_number() for non-numeric text like currency or percetn
x <- c("$1,234", "USD 3,513", "59%")
parse_number(x)

# Counts - flights by destination, sort makes it most to least
flights |> count(dest, sort = TRUE) |>
  view()

flights |> count(dest, sort = TRUE) |>
  print(n = Inf)

# Do counts within the summarize function
flights |> 
  group_by(dest) |> 
  summarize(
    n = n(),
    delay = mean(arr_delay, na.rm = TRUE)
  )

# Number of distinct carriers by destination
flights |> 
  group_by(dest) |> 
  summarize(carriers = n_distinct(carrier)) |> 
  arrange(desc(carriers))

# Number of miles that each plane flew
flights |> 
  group_by(tailnum) |> 
  summarize(miles = sum(distance)) |>
  arrange(desc(miles))

# Alternative
flights |> 
  filter(is.na(tailnum) == FALSE) |>
  count(tailnum, wt = distance, sort = TRUE)

flights |> 
  filter(is.na(tailnum) == TRUE) |>
  count(tailnum, wt = distance, sort = TRUE)

# Count of missing dep_times by destination
flights |> 
  group_by(dest) |> 
  summarize(n_cancelled = sum(is.na(dep_time)))

# Finding the minimum or maximum of a row
df <- tribble(
  ~x, ~y,
  1,  3,
  5,  2,
  7, NA,
)

df |> 
  mutate(
    min = pmin(x, y, na.rm = TRUE),
    max = pmax(x, y, na.rm = TRUE)
  )

# Min and max of all observations in x and y
df |> 
  mutate(
    min = min(x, y, na.rm = TRUE),
    max = max(x, y, na.rm = TRUE)
  )

# Min and max of a variable
df |> 
  mutate(
    minx = min(x, na.rm = TRUE),
    maxx = max(x, na.rm = TRUE),
    miny = min(y, na.rm = TRUE),
    maxy = max(y, na.rm = TRUE)
  )

# Modular arithmetic - integer division and remainder
flights |> 
  mutate(
    hour = sched_dep_time %/% 100,
    minute = sched_dep_time %% 100,
    .keep = "used"
  )

# Proportion of cancelled flights over the course of a day
flights |>
  group_by(hour = sched_dep_time %/% 100) |>
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |>
  filter(hour > 1) |>
  ggplot(aes(x = hour, y = prop_cancelled)) + 
  geom_line(color = "grey50") + 
  geom_point(aes(size = n))

# Rounding
round(123.456, 2)  # two digits
round(123.456, 1)  # one digit
round(123.456, 0) # nearest one
round(123.456, -1) # nearest ten

# Uses Banker’s rounding: If a number is half way between two integers, rounded to the even integer.

# Rounding to a different scale
x <- 123.456
round(x / 4) * 4 # nearest multiple of 4
round(x / 0.25) * 0.25 # nearest 0.25

# cut() puts a numeric vector in buckets
x <- c(1, 2, 5, 10, 15, 20)
cut(x, breaks = c(0, 5, 10, 15, 20))

cut(x, 
    breaks = c(0, 5, 10, 15, 20), 
    labels = c("sm", "md", "lg", "xl")
)

# Values outside of range become NA
y <- c(NA, -10, 5, 10, 30)
cut(y, breaks = c(0, 5, 10, 15, 20))

# Cumulative sums
x <- 1:10
cumsum(x)

# Rounding flights to nearest five minutes
flights |>
  mutate(dep_time = round(dep_time / 5) * 5,
         arr_time = round(arr_time / 5) * 5,
         .keep = "used")

# Ranking numbers
x <- c(1, 2, 2, 4, 3, NA)
min_rank(x)

# Other ranks
df <- tibble(x = x)
df |> 
  mutate(
    row_number = row_number(x),
    dense_rank = dense_rank(x),
    percent_rank = percent_rank(x),
    cume_dist = cume_dist(x)
  )

# Making groups
df <- tibble(id = 1:10)

df |> 
  mutate(
    row0 = row_number() - 1,
    three_groups = row0 %% 3,
    three_in_each_group = row0 %/% 3
  )

# Difference between current and previous value
x <- c(2, 5, 11, 11, 19, 35)
x - lag(x)

# Suppose you track times someone visits a website
# Define session as a visit after more than 5 hours of not visiting
events <- tibble(
  time = c(0, 1, 2, 3, 5, 10, 12, 15, 17, 19, 20, 27, 28, 30)
)
events <- events |> 
  mutate(
    diff = time - lag(time, default = first(time)),
    has_gap = diff >= 5
  )
events

# Keep track of new sessions
events |> mutate(
  group = cumsum(has_gap)
)

# 10 most delayed flights
flights |>
  arrange(desc(dep_delay)) |>
  slice_head(n = 10)

# Departure delays by day
flights |> 
  mutate(hour = dep_time %/% 100) |> 
  group_by(year, month, day, hour) |> 
  summarize(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  filter(n > 5)

# Median is usually smaller than the mean
flights |>
  group_by(year, month, day) |>
  summarize(
    mean = mean(dep_delay, na.rm = TRUE),
    median = median(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = mean, y = median)) + 
  geom_abline(slope = 1, intercept = 0, color = "white", linewidth = 2) +
  geom_point()

# Summary statistics
flights |>
  group_by(year, month, day) |>
  summarize(
    max = max(dep_delay, na.rm = TRUE),
    q95 = quantile(dep_delay, 0.95, na.rm = TRUE), # 95th quartile from dep_delay
    .groups = "drop"
  )

# Distribution of departure delays by day
flights |>
  filter(dep_delay < 120) |> 
  ggplot(aes(x = dep_delay, group = interaction(day, month))) + 
  geom_freqpoly(binwidth = 5, alpha = 1/5)

# Extracting a value at a specific position
flights |> 
  group_by(year, month, day) |> 
  summarize(
    first_dep = first(dep_time, na_rm = TRUE), 
    fifth_dep = nth(dep_time, 5, na_rm = TRUE),
    last_dep = last(dep_time, na_rm = TRUE)
  )

# Other mutations
flights1 <- flights |>
  group_by(year, month, day) |>
  summarize(avg_dep_delay = mean(dep_delay, na.rm = TRUE))

flights1 |>
  mutate(
    # Calculates the proportion of a total
    prop = avg_dep_delay / sum(avg_dep_delay), 
    # Computes a Z-score (standardized to mean 0 and sd 1)
    z_score = (avg_dep_delay - mean(avg_dep_delay)) / sd(avg_dep_delay), 
    # Standardizes to range [0, 1]
    standard = (avg_dep_delay - min(avg_dep_delay)) / (max(avg_dep_delay) - min(avg_dep_delay)), 
    # Computes an index based on the min observation
    avg_dep_delay / min(avg_dep_delay)) 


# 14 Transform - Strings --------------------------------------------------

library(babynames)

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'
double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"
backslash <- "\\"

# To see raw contents when dealing with escapes, use str_view()
str_view(double_quote)
str_view(backslash)

# Sometimes it can get confused with all the escaping and backslashing
# Start with r"( and finishes with )"
tricky <- r"(double_quote <- "\"" # or '"'single_quote <- '\'' # or "'")"
str_view(tricky)

# Other special characters - new line, tab, unicode
x <- c("one\ntwo", "one\ttwo", "\u00b5", "\U0001f604")
str_view(x)

# What is unicode? Universal text encoding standard.

# Joining strings
str_c("x", "y", "z")
str_c("Hello ", c("John", "Susan"))

df <- tibble(name = c("Flora", "David", "Terra", NA))
df |> mutate(greeting = str_c("Hi ", name, "!"))

# Use coalesce to add a default for NAs
df |> 
  mutate(
    greeting1 = str_c("Hi ", coalesce(name, "you"), "!"),
    greeting2 = coalesce(str_c("Hi ", name, "!"), "Hi you!")
  )

# Alternative is to use str_glue
df |> mutate(greeting = str_glue("Hi {name}!"))
df |> mutate(greeting = str_glue("{{Hi {name}!}}"))

# Converting to single string
str_flatten(c("x", "y", "z"))
str_flatten(c("x", "y", "z"), ", ")
str_flatten(c("x", "y", "z"), ", ", last = ", and ")

# str_flatten works well with summarize
df <- tribble(
  ~ name, ~ fruit,
  "Carmen", "banana",
  "Carmen", "apple",
  "Marvin", "nectarine",
  "Terence", "cantaloupe",
  "Terence", "papaya",
  "Terence", "mandarin"
)
df |>
  group_by(name) |> 
  summarize(fruits = str_flatten(fruit, ", "))

# Separating into rows
df1 <- tibble(x = c("a,b,c", "d,e", "f"))
df1 |> 
  separate_longer_delim(x, delim = ",") # by comma delimiter

df2 <- tibble(x = c("1211", "131", "21"))
df2 |> 
  separate_longer_position(x, width = 1) # by position width = 1 delimiter

# Separating into columns
df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))
df3 |> 
  separate_wider_delim(
    x,
    delim = ".", # by period delimiter
    names = c("code", "edition", "year")
  )

# If a specific piece is not useful, you can use NA in col name to omit
df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", NA, "year")
  )

df4 <- tibble(x = c("202215TX", "202122LA", "202325CA")) 
df4 |> 
  separate_wider_position(
    x,
    widths = c(year = 4, age = 2, state = 2) # Assigns the length for each var
  )

# Diagnosing problems
df <- tibble(x = c("1-1-1", "1-1-2", "1-3", "1-3-2", "1")) # Too few

debug <- df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "debug"
  )
debug |> filter(!x_ok) # Find the problems

# Assign NAs
df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "align_start" # Or use align_end
  )

df <- tibble(x = c("1-1-1", "1-1-2", "1-3-5-6", "1-3-2", "1-3-5-7-9")) # Too many

debug <- df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "debug"
  )

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "drop" # Drop them or "merge" them into last column
  )

# String length
str_length(c("a", "R for data science", NA))

babynames |>
  count(length = str_length(name), wt = n)

# Most popular longest baby names
babynames |> 
  filter(str_length(name) == 15) |> 
  count(name, wt = n, sort = TRUE)

# Substring
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)

# First and last letter of each baby name
babynames |> 
  mutate(
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1)
  )

# Character to the string code (ASCII)
charToRaw("Hadley")

# Some problems with letter variation
u <- c("\u00fc", "u\u0308")
str_view(u)
str_length(u) # The second one considers the accent as another character
str_sub(u, 1, 1)


# 15 Transform - Regular Expressions --------------------------------------

str_view(fruit)
# All strings in fruit with the expression "berry"
str_view(fruit, "berry")

# a followed by another character
str_view(c("a", "ab", "ae", "bd", "ea", "eab"), "a.")

# a followed by three characters and then an e
str_view(fruit, "a...e")

# Quantifiers control how many times a pattern can match
# ? makes the pattern optional
# + makes one or more of the pattern
# * makes any number of the pattern, including 0

str_view(c("a", "ab", "abb"), "ab?")
str_view(c("a", "ab", "abb"), "ab+")
str_view(c("a", "ab", "abb"), "ab*")

# Match by a set of character (or not set of characters)
str_view(words, "[aeiou]x[aeiou]") # Vowel, x, vowel
str_view(words, "[^aeiou]y[^aeiou]") # Not vowel, y, not vowel

# Fruit that contain apple, melon, or nut
str_view(fruit, "apple|melon|nut")

# Words that contain a repeated vowel
str_view(fruit, "aa|ee|ii|oo|uu")

# Returns true if match
str_detect(c("a", "b", "c"), "[aeiou]")

# Babynames that contain x
babynames |> 
  filter(str_detect(name, "x")) |> 
  count(name, wt = n, sort = TRUE)

# Proportion of baby names that contain x, broken down by year
# Recall that mean of a boolean produces proportion
babynames |> 
  group_by(year) |> 
  summarize(prop_x = mean(str_detect(name, "x"))) |> 
  ggplot(aes(x = year, y = prop_x)) + 
  geom_line()

str_subset(c("Alexander", "Xander"), "Alex")
str_which(c("Alexander", "Xander"), "Alex")

# Count of matches
x <- c("apple", "banana", "pear")
str_count(x, "p")

str_count("abababa", "aba")
str_view("abababa", "aba")

# Number of vowels and consonants
babynames |> 
  count(name) |> 
  mutate(
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

# To deal with case:
babynames |> 
  count(name) |> 
  mutate(
    vowels = str_count(name, "[aeiouAEIOU]"),
    consonants = str_count(name, "[^aeiouAEIOU]")
  )

babynames |> 
  count(name) |> 
  mutate(
    name = str_to_lower(name),
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

# Replace/remove vowels with dash
x <- c("apple", "pear", "banana")
str_replace_all(x, "[aeiou]", "-")
str_remove_all(x, "[aeiou]")

df <- tribble(
  ~str,
  "<Sheryl>-F_34",
  "<Kisha>-F_45", 
  "<Brandon>-N_33",
  "<Sharon>-F_38", 
  "<Penny>-F_58",
  "<Justin>-M_41", 
  "<Patricia>-F_84", 
)

# Extracting the data from one string
df |> 
  separate_wider_regex(
    str,
    patterns = c(
      "<", 
      name = "[A-Za-z]+", 
      ">-", 
      gender = ".",
      "_",
      age = "[0-9]+"
    )
  )

# Match the start/end/whole of string
str_view(fruit, "^a")
str_view(fruit, "a$")
str_view(fruit, "^apple$")

x <- "abcd ABCD 12345 -!@#%."
str_view(x, "\\d+") # matches digits
str_view(x, "\\D+") # matches not digits
str_view(x, "\\s+") # matches whitespace
str_view(x, "\\S+") # matches not whitespace
str_view(x, "\\w+") # matches words
str_view(x, "\\W+") # matches not words

# Fruits with repeated pair of letters
str_view(fruit, "(..)\\1")

# Words that start and end with the same two letters
str_view(words, "^(..).*\\1$")

# Switch the second and third word around
sentences |> 
  str_replace("(\\w+) (\\w+) (\\w+)", "\\1 \\3 \\2") |> 
  str_view()

# British/American spelling of words - use optional ?
str_view(c("colour", "color"), "colou?r")

# Regex expressions
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))

phone <- regex(
  r"(
    \(?     # optional opening parens
    (\d{3}) # area code
    [)\-]?  # optional closing parens or dash
    \ ?     # optional space
    (\d{3}) # another three numbers
    [\ -]?  # optional space or dash
    (\d{4}) # four more numbers
  )", 
  comments = TRUE
)
str_extract(c("514-791-8141", "(123) 456 7890", "123456"), phone)

# Sentences that start with "The" (and end with the e - not They or These)
str_view(sentences, "^The\\b")

# Sentences that begin with a pronoun
str_view(sentences, "^(She|He|It|They)\\b")

# Words with only consonants/no vowels
str_view(words[!str_detect(words, "[aeiou]")])

# Words that contain a and b
words[str_detect(words, "a") & str_detect(words, "b")]

# Sentences that mention a colour
str_view(sentences, "\\b(red|green|blue)\\b")

rgb <- c("red", "green", "blue")
col <- str_c("\\b(", str_flatten(rgb, "|"), ")\\b")

str_view(sentences, col)

# All colours
str_view(colors())

cols <- colors()
cols <- cols[!str_detect(cols, "\\d")]
str_view(cols)
pattern <- str_c("\\b(", str_flatten(cols, "|"), ")\\b")
str_view(sentences, pattern)


# 16 Transform - Factors --------------------------------------------------

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

gss_cat

# To see levels of a factor
gss_cat |>
  count(race)

gss_cat |>
  count(race, denom)

# Mean TV hours by religion
relig_summary <- gss_cat |>
  group_by(relig) |>
  summarize(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()

# Alternative
relig_summary |>
  mutate(
    relig = fct_reorder(relig, tvhours)
  ) |>
  ggplot(aes(x = tvhours, y = relig)) +
  geom_point()

# Average age compared to reported income
rincome_summary <- gss_cat |>
  group_by(rincome) |>
  summarize(
    age = mean(age, na.rm = TRUE),
    n = n()
  )
ggplot(rincome_summary, aes(x = age, y = fct_reorder(rincome, age))) + 
  geom_point()

# Reverse because reported income already has a useful factor
ggplot(rincome_summary, aes(x = age, y = fct_relevel(rincome, "Not applicable"))) +
  geom_point()

# Marital proportions by age
by_age <- gss_cat |>
  filter(!is.na(age)) |> 
  count(age, marital) |>
  group_by(age) |>
  mutate(
    prop = n / sum(n)
  )
ggplot(by_age, aes(x = age, y = prop, color = marital)) +
  geom_line(linewidth = 1) + 
  scale_color_brewer(palette = "Set1")

# The category with the high prop at the far right will be highest in the legend
ggplot(by_age, aes(x = age, y = prop, color = fct_reorder2(marital, age, prop))) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") + 
  labs(color = "marital") 

# Increasing frequency
gss_cat |>
  mutate(marital = marital |> fct_infreq() |> fct_rev()) |>
  ggplot(aes(x = marital)) +
  geom_bar()

# Decreasing frequency
gss_cat |>
  mutate(marital = marital |> fct_infreq()) |>
  ggplot(aes(x = marital)) +
  geom_bar()

# Count of political party affiliations
gss_cat |> count(partyid)

# Recode some of them for better publication
gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat"
    )
  ) |>
  count(partyid)

# Combine groups
gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat",
                         "Other"                 = "No answer",
                         "Other"                 = "Don't know",
                         "Other"                 = "Other party"
    )
  )

# Collapse a lot of variables
gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
                           "other" = c("No answer", "Don't know", "Other party"),
                           "rep" = c("Strong republican", "Not str republican"),
                           "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                           "dem" = c("Not str democrat", "Strong democrat")
    )
  ) |>
  count(partyid)

# Lump the smallest groups into other
gss_cat |>
  mutate(relig = fct_lump_lowfreq(relig)) |>
  count(relig)

# Lump such that there are 10 groups
gss_cat |>
  mutate(relig = fct_lump_n(relig, n = 10)) |>
  count(relig, sort = TRUE)


# 17 Transform - Dates & Times --------------------------------------------

# Current date or date-time
today()
now()

as_datetime(today())
as_date(now())

# Reading from import is automatic
csv <- "
  date,datetime
  2022-01-02,2022-01-02 05:12
"
read_csv(csv)

# If ambiguous - use col_types
csv <- "
  date
  01/02/15
"
read_csv(csv, col_types = cols(date = col_date("%m/%d/%y")))
read_csv(csv, col_types = cols(date = col_date("%y/%m/%d"))) # You specify the format

# Creating dates/date-times from string
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

# Creating dates/date-times from individual components
flights |> 
  select(year, month, day, hour, minute)

flights |> 
  select(year, month, day, hour, minute) |> 
  mutate(departure = make_datetime(year, month, day, hour, minute)) # Could be make_date as well

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights |> 
  filter(!is.na(dep_time), !is.na(arr_time)) |> 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) |> 
  select(origin, dest, ends_with("delay"), ends_with("time"))

# Summary of flights by day
by_day <- flights |>
  group_by(month,day) |>
  summarize(n = n())
view(by_day)

# Distribution of departure times
flights_dt |> 
  ggplot(aes(x = dep_time)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

# Distribution within a single day
flights_dt |> 
  filter(dep_time < ymd(20130102)) |> 
  ggplot(aes(x = dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

# Grabbing individual components
datetime <- ymd_hms("2026-07-08 12:34:56")
year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)

month(datetime, label = TRUE) # Title instead of number
wday(datetime, label = TRUE, abbr = FALSE) # No abbreviation for title

# Flights by day of the week
flights_dt |> 
  mutate(wday = wday(dep_time, label = TRUE)) |> 
  ggplot(aes(x = wday)) +
  geom_bar()

# Average departure delay by minute within the hour
flights_dt |> 
  mutate(minute = minute(dep_time)) |> 
  group_by(minute) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  ) |> 
  ggplot(aes(x = minute, y = avg_delay)) +
  geom_line()

# Average arrival delay by scheduled departure time
sched_dep <- flights_dt |> 
  mutate(minute = minute(sched_dep_time)) |> 
  group_by(minute) |> 
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(sched_dep, aes(x = minute, y = avg_delay)) +
  geom_line()

# Count of flights by minute within the hour
ggplot(sched_dep, aes(x = minute, y = n)) +
  geom_line()
# This shows that we like to schedule things at "nice" times

# Rounding dates - floor_date, round_date, ceiling_date
# Number of flights by week
flights_dt |> 
  count(week = floor_date(dep_time, "week")) |> 
  ggplot(aes(x = week, y = n)) +
  geom_line() + 
  geom_point()

weeks <- flights_dt |> 
  count(week = floor_date(dep_time, "week"))

view(weeks) # Why is there a big drop at the beginning of February?

# Distribution of flights during the day
flights_dt |> 
  mutate(dep_hour = hms::as_hms(dep_time - floor_date(dep_time, "day"))) |> 
  ggplot(aes(x = dep_hour)) +
  geom_freqpoly(binwidth = 60 * 30)

# Modifying components
datetime <- ymd_hms("2026-07-08 12:34:56")

year(datetime) <- 2030
datetime
month(datetime) <- 01
datetime
hour(datetime) <- hour(datetime) + 1
datetime

# Alternatively, use update
update(datetime, year = 2030, month = 2, mday = 2, hour = 2)

# Durations: Record time span in seconds
# How old is Hadley?
h_age <- today() - ymd("1979-10-14")
h_age
as.duration(h_age)

# Problem with time zones
one_am <- ymd_hms("2026-03-08 01:00:00", tz = "America/New_York")
one_am
one_am + ddays(1)
# [1] "2026-03-09 02:00:00 EDT" which is 25 hours later (because of daylight savings)

# Solutions: Use periods which are human units like weeks and months
one_am
one_am + days(1) # Instead of ddays

# Some flights arrive before they depart
flights_dt |> 
  filter(arr_time < dep_time) 
# These are overnight flights - fix it by adding one day to arrive time
flights_dt <- flights_dt |> 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight), # days(TRUE) = days(1)
    sched_arr_time = sched_arr_time + days(overnight)
  )

# Creating intervals
y2023 <- ymd("2023-01-01") %--% ymd("2024-01-01")
y2024 <- ymd("2024-01-01") %--% ymd("2025-01-01")

y2023 / days(1)
y2024 / days(1) # Leap year

# Complete list of all time zone names
length(OlsonNames())
head(OlsonNames())

# Three locations - same instance in time
x1 <- ymd_hms("2024-06-01 12:00:00", tz = "America/New_York")
x1
x2 <- ymd_hms("2024-06-01 18:00:00", tz = "Europe/Copenhagen")
x2
x3 <- ymd_hms("2024-06-02 04:00:00", tz = "Pacific/Auckland")
x3

# To verify, take the difference
x1 - x2
x1 - x3

# Making a vector often drops the time zone
x4 <- c(x1, x2, x3)
x4


# 18 Transform - Missing Values -------------------------------------------

# NAs commonly represent the last observation carried forward rather than repeating data entry
treatment <- tribble(
  ~person,           ~treatment, ~response,
  "Derrick Whitmore", 1,         7,
  NA,                 2,         10,
  NA,                 3,         NA,
  "Katherine Burke",  1,         4
)

# Use fill() to fill in these missing values
treatment |>
  fill(everything())

# Sometimes, NA represents a fixed value. Use coalesce()
x <- c(1, 4, 5, 7, NA)
coalesce(x, 0)

# Sometimes a known value like -99 represents a missing value. Use na_if()
x <- c(1, 4, 5, 7, -99)
na_if(x, -99)

# Most indeterminable results produce an NaN (not a number)
0 / 0
Inf - Inf
sqrt(-1)

# Explicit - Presence of an absence
# Implicit - Absence of a presence
stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr   = c(   1,    2,    3,    4,    2,    3,    4),
  price = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
# We see Q4 of 2020 has an explicit missing.
# We see Q! of 2021 has an implicit missing.

# Move from implicit to explicit through pivoting
stocks |>
  pivot_wider(
    names_from = qtr, 
    values_from = price
  )

# complete() fills in the missing combinations of year and qtr
stocks |>
  complete(year, qtr)

# If the individual variables themselves are incomplete, you can be more specific
stocks |>
  complete(year = 2019:2021, qtr)

# All airports without a match in flights
flights |> 
  distinct(faa = dest) |> 
  anti_join(airports)

# All planes without a match in flights
flights |> 
  distinct(tailnum) |> 
  anti_join(planes)

# Groups that don't have any observations
health <- tibble(
  name   = c("Ikaia", "Oletta", "Leriah", "Dashay", "Tresaun"),
  smoker = factor(c("no", "no", "no", "no", "no"), levels = c("yes", "no")),
  age    = c(34, 88, 75, 47, 56),
)

health |> count(smoker)

# To keep the yes smokers even though there are none:
health |> count(smoker, .drop = FALSE)

ggplot(health, aes(x = smoker)) +
  geom_bar()

ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE) # Keeps the yes smokers category

# Same application for group_by and summarize
health |> 
  group_by(smoker, .drop = FALSE) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  )

# Because the results of the summary statistics for 0 yes smokers are confusing,
# we can compute without the category and then add in using complete()
health |> 
  group_by(smoker) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  ) |> 
  complete(smoker)


# 19 Transform - Joins ----------------------------------------------------

# Primary key: Variable that uniquely identifies each observation
# Foreign key: Variable that corresponds to a primary key in another table
# flights$origin is a foreign key that corresponds to airports$faa

# Checking primary keys - if empty, then each row is uniquely identified
planes |> 
  count(tailnum) |> 
  filter(n > 1)

weather |> 
  count(time_hour, origin) |> 
  filter(n > 1)

# Also check for missing values since these cannot identify an observation
planes |> 
  filter(is.na(tailnum))

weather |> 
  filter(is.na(time_hour) | is.na(origin))

# Introducing a surrogate key to make a simple primary key
flights2 <- flights |> 
  mutate(id = row_number(), .before = 1)
flights2

# Mutating join: Adds variables to the right on observations that match
# Narrow the original dataset
flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)
flights2

# Add additional airline information through left_join()
# In this case, airlines only has two variables so the name gets joined
flights2 |>
  left_join(airlines)

# The temperature and wind speed when each flight departed
flights2 |> 
  left_join(weather |> select(origin, time_hour, temp, wind_speed))

# Details about the plane flying during the flight
flights2 |> 
  left_join(planes |> select(tailnum, type, engines, seats))
# If no match, fills in with missing values

# There is no plane information for N3ALAA
flights2 |> 
  filter(tailnum == "N3ALAA") |> 
  left_join(planes |> select(tailnum, type, engines, seats))

# left_join() uses natural join. But if we join flights with complete planes dataset,
# then lots of missing values because it is trying to match on (year, tailnum)
flights2 |> 
  left_join(planes)

# We only want to match on tailnum so we specify
flights2 |> 
  left_join(planes, join_by(tailnum), suffix = c(".flight", ".created"))

flights2 |> 
  left_join(airports, join_by(dest == faa)) # Join airport info for destination
flights2 |> 
  left_join(airports, join_by(origin == faa)) # Join airport info for origin

# Other joins:
# right_join() -> Keeps all rows in y
# full_join() -> Keeps all rows in either x or y
# inner_join() -> Keeps all rows that occur in x and y
# semi_join() -> Keeps all rows in x that have a match in y
# Filters out the non-matches instead of NA

# Airport information for just origins/destinations
airports |> 
  semi_join(flights2, join_by(faa == origin))

airports |> 
  semi_join(flights2, join_by(faa == dest))

# anti_join() is the opposite - keeps all rows in x that don't have a match in y
# Destinations without destination airport info
flights2 |> 
  anti_join(airports, join_by(dest == faa)) |> 
  distinct(dest)

# Tailnums with missing plane information
flights2 |>
  anti_join(planes, join_by(tailnum)) |> 
  distinct(tailnum)

# All flights to the top 10 most popular destinations
top_dest <- flights2 |>
  count(dest, sort = TRUE) |>
  head(10)
flights2 |>
  inner_join(top_dest |> select(dest))

# Compare the worst delays with weather patterns (in progress)
delay_flights <- flights |> 
  mutate(fdate = date(time_hour)) |>
  group_by(fdate) |>
  summarize(
    avg_adelay = mean(arr_delay, na.rm = TRUE),
    avg_ddelay = mean(dep_delay, na.rm = TRUE)
  ) |>
  arrange(desc(avg_adelay))
delay_weather <- weather |>
  mutate(wdate = date(time_hour)) |>
  group_by(wdate) |>
  summarize(
    avg_temp = mean(temp, na.rm = TRUE),
    avg_wind = mean(wind_speed, na.rm = TRUE),
    avg_prec = mean(precip, na.rm = TRUE),
    avg_visb = mean(visib, na.rm = TRUE)
  )
delay_flights |>
  left_join(delay_weather, join_by(fdate == wdate))

# Map of airports in the United States
airports |>
  semi_join(flights, join_by(faa == dest)) |>
  ggplot(aes(x = lon, y = lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

# Understanding how joins work through simple examples
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

# Non-equi join while keeping both keys
x |> inner_join(y, join_by(key == key), keep = TRUE)

# Join on condition instead of equality
x |> inner_join(y, join_by(key > key), keep = TRUE)

# Cross join - Same as Cartesian product
# Useful for permutations
df <- tibble(name = c("John", "Simon", "Tracy", "Max"))
df |> cross_join(df)

# Inequality joins - Useful for generating all combinations (no repeats)
df <- tibble(id = 1:4, name = c("John", "Simon", "Tracy", "Max"))
df |> inner_join(df, join_by(id < id))

# Rolling joins - Closest y that satisfies the inequality
# Suppose you have 4 party dates
parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03"))
)

# A list of employees birthdays
set.seed(123)
employees <- tibble(
  name = sample(babynames::babynames$name, 100),
  birthday = ymd("2022-01-01") + (sample(365, 100, replace = TRUE) - 1)
)
employees

# Each employee's birthday will be celebrated on the party date that happens closest 
# before their actual birthday
employees |> 
  left_join(parties, join_by(closest(birthday >= party)))

# Some will be left out
employees |> 
  anti_join(parties, join_by(closest(birthday >= party)))

# Explicitly state the date ranges for each party
parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03")),
  start = ymd(c("2022-01-01", "2022-04-04", "2022-07-11", "2022-10-03")),
  end = ymd(c("2022-04-03", "2022-07-10", "2022-10-02", "2022-12-31"))
)
parties

employees |> 
  inner_join(parties, join_by(between(birthday, start, end)), unmatched = "error")


# 20 Import - Spreadsheets ------------------------------------------------

# Best practices of data organization in spreadsheets
# https://doi.org/10.1080/00031305.2017.1375989

library(readxl)
library(writexl)

# Reading in an Excel file
students <- read_excel("data/students.xlsx")

# Clean up column names
read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1
)

# Deal with missing values
read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A")
)

# Clean up variable types
students <- read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A"),
  col_types = c("numeric", "text", "text", "text", "text")
)

students <- students |>
  mutate(
    age = if_else(age == "five", "5", age),
    age = parse_number(age)
  )

# Reading a single worksheet
read_excel("data/penguins.xlsx", sheet = "Torgersen Island")

# Dealing with NAs
penguins_torgersen <- read_excel("data/penguins.xlsx", sheet = "Torgersen Island", na = "NA")

# Call all sheets of the spreadsheet and then read them in 
excel_sheets("data/penguins.xlsx")
penguins_biscoe <- read_excel("data/penguins.xlsx", sheet = "Biscoe Island", na = "NA")
penguins_dream  <- read_excel("data/penguins.xlsx", sheet = "Dream Island", na = "NA")

# Check dimensions:
dim(penguins_torgersen)
dim(penguins_biscoe)
dim(penguins_dream)

# Same number of columns => combine all 3 using bind_rows()
penguins <- bind_rows(penguins_torgersen, penguins_biscoe, penguins_dream)
penguins

deaths_path <- readxl_example("deaths.xlsx")
deaths <- read_excel(deaths_path)

# Eliminate extraneous rows
read_excel(deaths_path, range = "A5:F15")

# Writing to excel
bake_sale <- tibble(
  item     = factor(c("brownie", "cupcake", "cookie")),
  quantity = c(10, 5, 8)
)
bake_sale
write_xlsx(bake_sale, path = "data/bake-sale.xlsx")

# Dealing with merged cells
library(openxlsx)
roster <- read.xlsx(xlsxFile = "data/roster.xlsx", fillMergedCells = TRUE)
roster

# How to pivot_wider() (in progress)
sales <- read_excel("data/sales.xlsx", col_names = c("id", "n"), range = "A5:B13")
sales
sales |>
  pivot_wider(names_from = ,
              values_from = )

# 21 Import - Databases ---------------------------------------------------

library(DBI)
library(dbplyr)

# Database: Collection of data frames called tables. Much larger than individual data frames. 
# Arbitrarily large, stored on disks instead of memory, have indexes to find data

# Creating a temporary database
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "duckdb")

# Loading in some data
dbWriteTable(con, "mpg", ggplot2::mpg)
dbWriteTable(con, "diamonds", ggplot2::diamonds)

# List of tables in the database
dbListTables(con)

# Retriving contents of the table
con |> 
  dbReadTable("diamonds") |> 
  as_tibble()

# Performing SQL query
sql <- "
  SELECT carat, cut, clarity, color, price 
  FROM diamonds 
  WHERE price > 15000
"
as_tibble(dbGetQuery(con, sql))

# To use dbplyr (translates dplyr to SQL in the background), we must first create
# object that represents a database table.
diamonds_db <- tbl(con, "diamonds")
diamonds_db

# Now you can use dplyr commands
big_diamonds_db <- diamonds_db |> 
  filter(price > 15000) |> 
  select(carat:clarity, price)

# See how it is translated to SQL
big_diamonds_db |>
  show_query()

# To get the data back into R, use collect()
big_diamonds <- big_diamonds_db |> 
  collect()
big_diamonds

dbplyr::copy_nycflights13(con)
flights <- tbl(con, "flights")
planes <- tbl(con, "planes")

# Filter and arrange is like WHERE and ORDER BY
flights |> 
  filter(dest == "IAH") |> 
  arrange(dep_delay) |>
  show_query()

flights |> 
  group_by(dest) |> 
  summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) |> 
  show_query()

# In SQL, case doesn't matter but order of the keywords matter.

# Use NULL in SQL which behave like NA. Removed during aggregation functions.
flights |> 
  group_by(dest) |> 
  summarize(delay = mean(arr_delay))

flights |> 
  filter(!is.na(dep_delay)) |> 
  show_query()

# Filtering new variables is like HAVING
diamonds_db |> 
  group_by(cut) |> 
  summarize(n = n()) |> 
  filter(n > 100) |> 
  show_query()

# Subqueries
flights |> 
  mutate(
    year1 = year + 1,
    year2 = year1 + 1
  ) |> 
  show_query()

# Joins
flights |> 
  left_join(planes |> rename(year_built = year), by = "tailnum") |> 
  show_query()


# 22 Import - Arrow -------------------------------------------------------

library(arrow)

# Downloading Seattle Public Library book checkouts dataset
dir.create("data", showWarnings = FALSE)

curl::multi_download(
  "https://r4ds.s3.us-west-2.amazonaws.com/seattle-library-checkouts.csv",
  "data/seattle-library-checkouts.csv",
  resume = TRUE
)

# Opening a dataset
seattle_csv <- open_dataset(
  sources = "data/seattle-library-checkouts.csv", 
  col_types = schema(ISBN = string()),
  format = "csv"
)

# Use collect() to force arrow to perform computation
seattle_csv |> 
  group_by(CheckoutYear) |> 
  summarise(Checkouts = sum(Checkouts)) |> 
  arrange(CheckoutYear) |> 
  collect()

# Parquet files have more efficient codings (binary data instead of string) 
# than csv so better for large data.

# Writing parquet files through partitioning
pq_path <- "data/seattle-library-checkouts"
seattle_csv |>
  group_by(CheckoutYear) |>
  write_dataset(path = pq_path, format = "parquet")
# Creates 18 files for each checkout year

# Turn arrow dataset into a DuckDB database
seattle_pq |> 
  to_duckdb() |>
  filter(CheckoutYear >= 2018, MaterialType == "BOOK") |>
  group_by(CheckoutYear) |>
  summarize(TotalCheckouts = sum(Checkouts)) |>
  arrange(desc(CheckoutYear)) |>
  collect()

# 23 Import - Hierarchical Data -------------------------------------------

library(repurrrsive)
library(jsonlite)

# Use list to store elements of different types in the same vector
x1 <- list(1:4, "a", TRUE)
x1

# Naming the components of a list
x2 <- list(a = 1:2, b = 1:3, c = 1:4)
x2

# Alternative way of displaying the structure of the list
str(x1)
str(x2)

# Representing hierarchy through list of lists
x3 <- list(list(1, 2), list(3, 4))
str(x3)

# Different from c() which just generates a flat vector
x4 <- c(list(1, 2), list(3, 4))
str(x4)

x5 <- list(1, list(2, list(3, list(4, list(5)))))
str(x5)
View(x5)

# Lists inside of tibbles are called list-columns
df <- tibble(
  x = 1:2, 
  y = c("a", "b"),
  z = list(list(1, 2), list(3, 4, 5))
)
df

# Behave similar to any other column
df |> 
  filter(x == 1)

data.frame(x = list(1:3, 3:5))

# Named children
df1 <- tribble(
  ~x, ~y,
  1, list(a = 11, b = 12),
  2, list(a = 21, b = 22),
  3, list(a = 31, b = 32),
)
df1
df1$y

# Unnamed children
df2 <- tribble(
  ~x, ~y,
  1, list(11, 12, 13),
  2, list(21),
  3, list(31, 32),
)
df2
df2$y

# unnest_wider() for named children - components get their own column
df1 |> 
  unnest_wider(y)

# Combine column name and element name
df1 |> 
  unnest_wider(y, names_sep = "_")

# unnest_longer() for unnamed children - components get their own row
df2 |> 
  unnest_longer(y)

# What happens if an element is empty? Zero rows in output
df6 <- tribble(
  ~x, ~y,
  "a", list(1, 2),
  "b", list(3),
  "c", list()
)
df6 |> unnest_longer(y)

# Could force to keep these rows
df6 |> unnest_longer(y, keep_empty = TRUE)

# Inconsistent types - output contains a list-column, each with one element
df4 <- tribble(
  ~x, ~y,
  "a", list(1),
  "b", list("a", TRUE, 5)
)
df4 |> 
  unnest_longer(y)

# Unnesting multiple columns simultaneously
df5 <- tribble(
  ~x, ~y, ~z,
  "a", list("y-a-1", "y-a-2"), list("z-a-1", "z-a-2"),
  "b", list("y-b-1", "y-b-2", "y-b-3"), list("z-b-1", "z-b-2", "z-b-3")
)
df5 |>
  unnest_longer(c(y, z))

# Case study - Collection of GitHub repositories
View(gh_repos)

repos <- tibble(json = gh_repos)
repos

# Since each row contains an unnamed list, we use unnest_longer()
repos |> 
  unnest_longer(json)

# Now each element is named so we use unnest_wider()
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json)

# Lot of variables - look at the first 10
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  names() |> 
  head(10)

# Pull out the interesting columns
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description)

# Owner is another named list-column
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description) |> 
  unnest_wider(owner)
# Error - this would result in two id columns so we need to use names_sep

repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description) |> 
  unnest_wider(owner, names_sep = "_")

# Case study - Data about characters that appear in Game of Thrones book and tv
chars <- tibble(json = got_chars)
chars

chars |> 
  unnest_wider(json)

characters <- chars |> 
  unnest_wider(json) |> 
  select(id, name, gender, culture, born, died, alive)
characters

# Find the list-columns
chars |> 
  unnest_wider(json) |> 
  select(id, where(is.list))

# Explore the "titles" list
chars |> 
  unnest_wider(json) |> 
  select(id, titles) |> 
  unnest_longer(titles)

# Clean up by removing blanks and renaming variables
titles <- chars |> 
  unnest_wider(json) |> 
  select(id, titles) |> 
  unnest_longer(titles) |> 
  filter(titles != "") |> 
  rename(title = titles)
titles

# Case study: 5 city names and Google's geocoding API
gmaps_cities

gmaps_cities |> 
  unnest_wider(json)

gmaps_cities |> 
  unnest_wider(json) |> 
  select(-status) |> 
  unnest_longer(results)

locations <- gmaps_cities |> 
  unnest_wider(json) |> 
  select(-status) |> 
  unnest_longer(results) |> 
  unnest_wider(results)
locations

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry)

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> 
  unnest_wider(location)

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> 
  # focus on the variables of interest
  select(!location:viewport) |>
  unnest_wider(bounds)

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> 
  select(!location:viewport) |>
  unnest_wider(bounds) |> 
  rename(ne = northeast, sw = southwest) |> 
  unnest_wider(c(ne, sw), names_sep = "_") 

# Extracting directly using hoist()
locations |> 
  select(city, formatted_address, geometry) |> 
  hoist(
    geometry,
    ne_lat = c("bounds", "northeast", "lat"),
    sw_lat = c("bounds", "southwest", "lat"),
    ne_lng = c("bounds", "northeast", "lng"),
    sw_lng = c("bounds", "southwest", "lng"),
  )

# JSON = Javascript object notation
# The way that most web APIs return data
# Array: Unnamed list -> [1, 2, 3]
# Object: Named list -> {"x": 1, "y": 2}

# The path to a JSON file from inside the package jsonlite
gh_users_json()
# Reading it into R
gh_users2 <- read_json(gh_users_json())

# Check that it's the same data we used previously
identical(gh_users, gh_users2)

# Three simple JSON datasets
str(parse_json('1'))
str(parse_json('[1, 2, 3]'))
str(parse_json('{"x": [1, 2, 3]}'))

# Rectangling process from JSON files
json <- '[
  {"name": "John", "age": 34},
  {"name": "Susan", "age": 27}
]'
df <- tibble(json = parse_json(json))
df
df |> 
  unnest_wider(json)

json <- '{
  "status": "OK", 
  "results": [
    {"name": "John", "age": 34},
    {"name": "Susan", "age": 27}
 ]
}
'
df <- tibble(json = list(parse_json(json)))
df

df |> 
  unnest_wider(json) |> 
  unnest_longer(results) |> 
  unnest_wider(results)

# Alternative: Reach inside the JSON and start with the bit you only care about
df <- tibble(results = parse_json(json)$results)
df |> 
  unnest_wider(results)

# 24 Import - Web Scraping ------------------------------------------------

library(rvest)

# Ethics on web scraping: If the data is public, non-personal, and factual, you are good.

# HTML: HyperText Markup Language - the language that describes web pages
# Sample text:
# <html>
# <head>
#   <title>Page title</title>
# </head>
# <body>
#   <h1 id='first'>A heading</h1>
#   <p>Some text &amp; <b>some bold text.</b></p>
#   <img src='myimg.png' width='100' height='100'>
# </body>

# HTML is formed by elements which consist of a start tag, optional attributes,
# an end tag, and contents.

# HTML Guide - https://developer.mozilla.org/en-US/docs/Web/HTML

# Read in web page
html <- read_html("http://rvest.tidyverse.org/")
html

# Write HTML inline
html <- minimal_html("
  <p>This is a paragraph</p>
  <ul>
    <li>This is a bulleted list</li>
  </ul>
")
html

# CSS: Cascading style sheets -> Tool for defining visual styling of HTML docs
html <- minimal_html("
  <h1>This is a heading</h1>
  <p id='first'>This is a paragraph</p>
  <p class='important'>This is an important paragraph</p>
")

# Find all paragraph elements
html |> html_elements("p")

# Find all elements with class "important"
html |> html_elements(".important")

# Find the element with id "first"
html |> html_elements("#first")

# Difference between elements and element
html |> html_elements("b") # Returns vector of length 0
html |> html_element("b") # Returns NA

# Unordered list where each list item contains information about four characters from Star Wars
html <- minimal_html("
  <ul>
    <li><b>C-3PO</b> is a <i>droid</i> that weighs <span class='weight'>167 kg</span></li>
    <li><b>R4-P17</b> is a <i>droid</i></li>
    <li><b>R2-D2</b> is a <i>droid</i> that weighs <span class='weight'>96 kg</span></li>
    <li><b>Yoda</b> weighs <span class='weight'>66 kg</span></li>
  </ul>
  ")

# Make a vector for each character
characters <- html |> html_elements("li")
characters

# Extract the name of each character
characters |> html_element("b")

# Extract the weight of each character
characters |> html_element(".weight")
characters |> html_elements(".weight")

# Extracting the text contents using html_text2()
characters |> 
  html_element("b") |> 
  html_text2()

characters |> 
  html_element(".weight") |> 
  html_text2()

# Extracting data from attributes
html <- minimal_html("
  <p><a href='https://en.wikipedia.org/wiki/Cat'>cats</a></p>
  <p><a href='https://en.wikipedia.org/wiki/Dog'>dogs</a></p>
")

html |> 
  html_elements("p") |> 
  html_element("a") |> 
  html_attr("href")

# Reading in an HTML table
html <- minimal_html("
  <table class='mytable'>
    <tr><th>x</th>   <th>y</th></tr>
    <tr><td>1.5</td> <td>2.7</td></tr>
    <tr><td>4.9</td> <td>1.3</td></tr>
    <tr><td>7.2</td> <td>8.1</td></tr>
  </table>
  ")

html |> 
  html_element(".mytable") |> 
  html_table()

# Find the right selectors using SelectorGadget
# https://rvest.tidyverse.org/articles/selectorgadget.html

# When you are on a web page, right click to "Inspect" and highlight sections
# of the source code to see what it corresponds to on the web page.

# Case study: Star Wars films
url <- "https://rvest.tidyverse.org/articles/starwars.html"
html <- read_html(url)

# Breaking down by section
section <- html |> html_elements("section")
section

# Titles
section |> html_element("h2") |> html_text2()

# Director
section |> html_element(".director") |> html_text2()

# Once we have each component, we wrap in a tibble
tibble(
  title = section |> 
    html_element("h2") |> 
    html_text2(),
  released = section |> 
    html_element("p") |> 
    html_text2() |> 
    str_remove("Released: ") |> 
    parse_date(),
  director = section |> 
    html_element(".director") |> 
    html_text2(),
  intro = section |> 
    html_element(".crawl") |> 
    html_text2()
)

# Case Study: IMDb top films
url <- "https://web.archive.org/web/20220201012049/https://www.imdb.com/chart/top/"
html <- read_html(url)

table <- html |> 
  html_element("table") |> 
  html_table()
table

ratings <- table |>
  select(
    rank_title_year = `Rank & Title`,
    rating = `IMDb Rating`
  ) |> 
  mutate(
    rank_title_year = str_replace_all(rank_title_year, "\n +", " ")
  ) |> 
  separate_wider_regex(
    rank_title_year,
    patterns = c(
      rank = "\\d+", "\\. ",
      title = ".+", " +\\(",
      year = "\\d+", "\\)"
    )
  )
ratings

# Extra data not found in table
html |> 
  html_elements("td strong") |> 
  head() |> 
  html_attr("title")

# Adding to our existing data
ratings |>
  mutate(
    rating_n = html |> html_elements("td strong") |> html_attr("title")
  ) |> 
  separate_wider_regex(
    rating_n,
    patterns = c(
      "[0-9.]+ based on ",
      number = "[0-9,]+",
      " user ratings"
    )
  ) |> 
  mutate(
    number = parse_number(number)
  )


# 25 Functions ------------------------------------------------------------

# Vector functions
df <- tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5),
  d = rnorm(5),
)

df |> mutate(
  a = (a - min(a, na.rm = TRUE)) / 
    (max(a, na.rm = TRUE) - min(a, na.rm = TRUE)),
  b = (b - min(b, na.rm = TRUE)) / 
    (max(b, na.rm = TRUE) - min(b, na.rm = TRUE)),
  c = (c - min(c, na.rm = TRUE)) / 
    (max(c, na.rm = TRUE) - min(c, na.rm = TRUE)),
  d = (d - min(d, na.rm = TRUE)) / 
    (max(d, na.rm = TRUE) - min(d, na.rm = TRUE)),
)

# Because we repeat this call, we can create a rescaling function
rescale01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
# Testing the function
rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))

# Applying the function in mutate
df |> mutate(
  a = rescale01(a),
  b = rescale01(b),
  c = rescale01(c),
  d = rescale01(d),
)

# Using iteration to make function call one line
df |> mutate(across(a:d, rescale01))

# Ways to improve the function
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE) # Using range() to compute min and max
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE) # Ignore infinity values
  (x - rng[1]) / (rng[2] - rng[1])
}

z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Use case when to ensure all values lie between min and max
clamp <- function(x, min, max) {
  case_when(
    x < min ~ min,
    x > max ~ max,
    .default = x
  )
}
clamp(1:10, min = 3, max = 7)

# Capitalize first character
first_upper <- function(x) {
  str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 1, 1))
  x
}
first_upper("hello")

# Clean up numbers
clean_number <- function(x) {
  is_pct <- str_detect(x, "%")
  num <- x |> 
    str_remove_all("%") |> 
    str_remove_all(",") |> 
    str_remove_all(fixed("$")) |> 
    as.numeric()
  if_else(is_pct, num / 100, num)
}
clean_number("$12,300")
clean_number("45%")

# Clean up NAs
fix_na <- function(x) {
  if_else(x %in% c(997, 998, 999), NA, x)
}

# Adding commas to word vector
commas <- function(x) {
  str_flatten(x, collapse = ", ", last = " and ")
}
commas(c("cat", "dog", "pigeon"))

# Coefficient of variation
cv <- function(x, na.rm = FALSE) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}
cv(runif(100, min = 0, max = 50))
cv(runif(100, min = 0, max = 500))

# Count of the number of missing values
n_missing <- function(x) {
  sum(is.na(x))
}

# Multiple vextor inputs - Mean absolute percentage error
mape <- function(actual, predicted) {
  sum(abs((actual - predicted) / actual)) / length(actual)
}

# Embracing a variable to use the value stored inside the argument
grouped_mean <- function(df, group_var, mean_var) {
  df |> 
    group_by({{ group_var }}) |> 
    summarize(mean({{ mean_var }}))
}

# Helper function for a set of summaries
summary6 <- function(data, var) {
  data |> summarize(
    min = min({{ var }}, na.rm = TRUE),
    mean = mean({{ var }}, na.rm = TRUE),
    median = median({{ var }}, na.rm = TRUE),
    max = max({{ var }}, na.rm = TRUE),
    n = n(),
    n_miss = sum(is.na({{ var }})),
    .groups = "drop"
  )
}
diamonds |> summary6(carat)

# Ability to use on grouped data
diamonds |> 
  group_by(cut) |> 
  summary6(carat)

diamonds |> 
  group_by(cut) |> 
  summary6(log10(carat))

# Computes proportions
count_prop <- function(df, var, sort = FALSE) {
  df |>
    count({{ var }}, sort = sort) |>
    mutate(prop = n / sum(n))
}
diamonds |> count_prop(clarity)

# Count unique, given a condition
unique_where <- function(df, condition, var) {
  df |> 
    filter({{ condition }}) |> 
    distinct({{ var }}) |> 
    arrange({{ var }})
}
# All the destinations in December
flights |> unique_where(month == 12, dest)

# Function for one specific data frame
subset_flights <- function(rows, cols) {
  flights |> 
    filter({{ rows }}) |> 
    select(time_hour, carrier, flight, {{ cols }})
}

# Use pick() for tidy selection inside data-masking functions
count_missing <- function(df, group_vars, x_var) {
  df |> 
    group_by(pick({{ group_vars }})) |> 
    summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
    )
}
flights |> 
  count_missing(c(year, month, day), dep_time)

# Counts based on variables in rows and columns
count_wide <- function(data, rows, cols) {
  data |> 
    count(pick(c({{ rows }}, {{ cols }}))) |> 
    pivot_wider(
      names_from = {{ cols }}, 
      values_from = n,
      names_sort = TRUE,
      values_fill = 0
    )
}
diamonds |> count_wide(c(clarity, color), cut)

# Histogram function
histogram <- function(df, var, binwidth = NULL) {
  df |> 
    ggplot(aes(x = {{ var }})) + 
    geom_histogram(binwidth = binwidth)
}
diamonds |> histogram(carat, 0.1)

# Add on after calling the function
diamonds |> 
  histogram(carat, 0.1) +
  labs(x = "Size (in carats)", y = "Number of diamonds")

# Linearity check
linearity_check <- function(df, x, y) {
  df |>
    ggplot(aes(x = {{ x }}, y = {{ y }})) +
    geom_point() +
    geom_smooth(method = "loess", formula = y ~ x, color = "red", se = FALSE) +
    geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) 
}
starwars |> 
  filter(mass < 1000) |> 
  linearity_check(mass, height)

# Barplot based on some condition
conditional_bars <- function(df, condition, var) {
  df |> 
    filter({{ condition }}) |> 
    ggplot(aes(x = {{ var }})) + 
    geom_bar()
}
diamonds |> conditional_bars(cut == "Good", clarity)

# Labelling plots in function using englue
histogram <- function(df, var, binwidth) {
  label <- rlang::englue("A histogram of {{var}} with binwidth {binwidth}")
  
  df |> 
    ggplot(aes(x = {{ var }})) + 
    geom_histogram(binwidth = binwidth) + 
    labs(title = label)
}
diamonds |> histogram(carat, 0.1)

# More info on data masking - https://rlang.r-lib.org/reference/topic-data-mask.html


# 26 Iteration ------------------------------------------------------------

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# Median for each column
df |> summarize(
  n = n(),
  a = median(a),
  b = median(b),
  c = median(c),
  d = median(d),
)

# Use across() to iterate for selection of columns
df |> summarize(
  n = n(),
  across(a:d, median),
)

sample(2, 10, replace = TRUE)
df <- tibble(
  grp = sample(2, 10, replace = TRUE),
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# Summarize median across everything
df |> 
  group_by(grp) |> 
  summarize(across(everything(), median))

df |> 
  group_by(grp) |> 
  summarize(across(starts_with("a"), median))

# Playing with the function call
rnorm_na <- function(n, n_na, mean = 0, sd = 1) {
  sample(c(rnorm(n - n_na, mean = mean, sd = sd), rep(NA, n_na)))
}
df_miss <- tibble(
  a = rnorm_na(5, 1),
  b = rnorm_na(5, 1),
  c = rnorm_na(5, 2),
  d = rnorm(5)
)
df_miss |> 
  summarize(
    across(a:d, median),
    n = n()
  )

# Create a new function that removes NA inside across()
df_miss |> 
  summarize(
    across(a:d, function(x) median(x, na.rm = TRUE)),
    n = n()
  )

# Calling multiple functions
df_miss |> 
  summarize(
    across(a:d, list(
      median = \(x) median(x, na.rm = TRUE),
      n_miss = \(x) sum(is.na(x))
    )),
    n = n()
  )

# Name columns using the .names argument
df_miss |> 
  summarize(
    across(
      a:d,
      list(
        median = \(x) median(x, na.rm = TRUE),
        n_miss = \(x) sum(is.na(x))
      ),
      .names = "{.fn}_{.col}"
    ),
    n = n(),
  )

# Replace NAs with 0
df_miss |> 
  mutate(
    across(a:d, \(x) coalesce(x, 0))
  )

# Rename to see old output and new output
df_miss |> 
  mutate(
    across(a:d, \(x) coalesce(x, 0), .names = "{.col}_na_zero")
  )

# Filter if any rows have NAs
df_miss |> filter(if_any(a:d, is.na))
# Filter if all rows have NAs
df_miss |> filter(if_all(a:d, is.na))

# Iterating to grab year, month, and day
expand_dates <- function(df) {
  df |> 
    mutate(
      across(where(is.Date), list(year = year, month = month, day = mday))
    )
}
df_date <- tibble(
  name = c("Amy", "Bob"),
  date = ymd(c("2009-08-03", "2010-01-16"))
)
df_date |> 
  expand_dates()

# Using across() in a function
summarize_means <- function(df, summary_vars = where(is.numeric)) {
  df |> 
    summarize(
      across({{ summary_vars }}, \(x) mean(x, na.rm = TRUE)),
      n = n(),
      .groups = "drop"
    )
}
diamonds |> 
  group_by(cut) |> 
  summarize_means()
diamonds |> 
  group_by(cut) |> 
  summarize_means(c(carat, x:z))

# Reading multiple files
paths <- list.files("data/gapminder", pattern = "[.]xlsx$", full.names = TRUE)
paths
#  [1] "data/gapminder/1952.xlsx" "data/gapminder/1957.xlsx"
#  [3] "data/gapminder/1962.xlsx" "data/gapminder/1967.xlsx"
#  [5] "data/gapminder/1972.xlsx" "data/gapminder/1977.xlsx"
#  [7] "data/gapminder/1982.xlsx" "data/gapminder/1987.xlsx"
#  [9] "data/gapminder/1992.xlsx" "data/gapminder/1997.xlsx"
# [11] "data/gapminder/2002.xlsx" "data/gapminder/2007.xlsx"

# map() applies a function to each element of a vector
files <- map(paths, readxl::read_excel)
length(files)

# Combine data frames into a single one
purrr::list_rbind(files)

# Both steps in a pipeline
paths |> 
  map(readxl::read_excel) |> 
  list_rbind()

# Sometimes, the name of the file is data itself like year
# Extract the file name
paths |> set_names(basename) 

files <- paths |> 
  set_names(basename) |> 
  map(readxl::read_excel)

paths |> 
  set_names(basename) |> 
  map(readxl::read_excel) |> 
  list_rbind(names_to = "year") |>  # Name of the files go to variable "year"
  mutate(year = parse_number(year))

# If there are other important information in the file location
paths |> 
  set_names() |> 
  map(readxl::read_excel) |> 
  list_rbind(names_to = "year") |> 
  separate_wider_delim(year, delim = "/", names = c(NA, "dir", "file")) |> 
  separate_wider_delim(file, delim = ".", names = c("file", "ext"))

# Save your work by writing to a csv
gapminder <- paths |> 
  set_names(basename) |> 
  map(readxl::read_excel) |> 
  list_rbind(names_to = "year") |> 
  mutate(year = parse_number(year))
write_csv(gapminder, "gapminder.csv")

# Writing multiple csv files - Using diamonds data, one file for each clarity
# Use group_nest()
by_clarity <- diamonds |> 
  group_nest(clarity)
by_clarity

# To get the first tibble
by_clarity$data[[1]]

# Create a column for the name of the output file
by_clarity <- by_clarity |> 
  mutate(path = str_glue("diamonds-{clarity}.csv"))
by_clarity

# Map the output of all the files
walk2(by_clarity$data, by_clarity$path, write_csv)

# Instead of exporting csv, suppose we want to export plots, one for each clarity
carat_histogram <- function(df) {
  ggplot(df, aes(x = carat)) + geom_histogram(binwidth = 0.1)  
}
# To get the first plot
carat_histogram(by_clarity$data[[1]])

# Apply map() to create many plots and many file paths
by_clarity <- by_clarity |> 
  mutate(
    plot = map(data, carat_histogram),
    path = str_glue("clarity-{clarity}.png")
  )

# Apply walk2 to save each plot
walk2(
  by_clarity$path,
  by_clarity$plot,
  \(path, plot) ggsave(path, plot, width = 6, height = 6)
)


# 27 Base R ---------------------------------------------------------------

# Subsetting a vector
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]

# Dropping elements at specfic positions
x[c(-1, -3, -5)]

# Subsetting w/ a logical factor keeps the TRUE instances
x <- c(10, 3, NA, 5, 8, 1, NA)
x[!is.na(x)]
x[x %% 2 == 0]

# Selecting the names of elements
x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

df <- tibble(
  x = 1:3, 
  y = c("a", "e", "f"), 
  z = runif(3)
)

# First row, second column
df[1, 2]
# All rows, column x and y
df[, c("x" , "y")]
# Rows where x is greater than 1, all columns (Similar to filter)
df[df$x > 1, ]

# Difference between data frame and tibble
df1 <- data.frame(x = 1:3)
df1[, "x"] # Returns vector

df2 <- tibble(x = 1:3)
df2[, "x"] # Returns tibble

# Use which() to condition and drop missing values
df[which(df$x > 1), ]

# Ordering rows
df[order(df$x, df$y, decreasing = TRUE), ]

# Subset - combination of filter and select
df |> subset(x > 1, c(y, z))

tb <- tibble(
  x = 1:4,
  y = c(10, 4, 1, 21)
)

# Extract column by position
tb[[1]]

# Extract column by name
tb[["x"]]
tb$x

# Create new columns - like mutate
tb$z <- tb$x + tb$y
tb

# Performing quick summaries
max(diamonds$carat)
levels(diamonds$cut)

l <- list(
  a = 1:3, 
  b = "a string", 
  c = pi, 
  d = list(-1, -5)
)

# Extract a sub-list
str(l[1:2])
str(l[4])

# Extract a single component of a list - removing hierarchy
str(l[[1]])
str(l[[4]])
str(l$a)

df["x"] # Returns a one column data frame
df[["x"]] # Returns a vector

# Applying a function across vector
df <- tibble(a = 1, b = 2, c = "a", d = "b", e = 4)
num_cols <- sapply(df, is.numeric)
num_cols

# Double numeric values
df[, num_cols] <- lapply(df[, num_cols, drop = FALSE], \(x) x * 2)
df

# Apply a summary to a grouped variable
tapply(diamonds$price, diamonds$cut, mean)

# For loop set up
for (element in vector) {
  # do something with element
}

# Plotting in base R
hist(diamonds$carat)
plot(diamonds$carat, diamonds$price)
