# Post 3: Basic data exploration
# Henriette L. Arndt, November 10 2023
# https://www.henriettearndt.com/blog

## Data ---------------------------------------------------
# Load
library(tidyverse)
data(starwars)

# View var info
glimpse(starwars)

## Summarize data  ----------------------------------------
### Categorical vars --------------------------------------
# https://cran.r-project.org/web/packages/janitor/vignettes/tabyls.html
library(janitor)

# One-way table
one_way <- starwars %>% 
  janitor::tabyl(species)
one_way

# Two-way table
two_way <- starwars %>% 
  filter(species %in% c("Human", "Droid")) %>% 
  janitor::tabyl(species, gender) %>%
  # add total at bottom
  janitor::adorn_totals("row") %>% 
  # calculate proportions by row
  janitor::adorn_percentages("row") %>% 
  # round percentages to whole number
  janitor::adorn_pct_formatting(digits = 0) %>% 
  # include raw counts
  janitor::adorn_ns() %>%
  # include both var names in first cell
  janitor::adorn_title("combined")
two_way

# Generate print-ready table with kableExtra
# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
library(kableExtra)

two_way %>% 
  # generate table
  kableExtra::kbl() %>% 
  # apply print theme
  kableExtra::kable_classic(
    html_font = "Times New Roman",
    font_size = 12)


### Continuous vars ---------------------------------------
# Calculate descriptive stats with psych
library(psych)

descr <- starwars %>% 
  select(where(is.numeric)) %>% 
  psych::describe() %>% 
  as_tibble(rownames = "varname") %>% 
  select(-vars) %>% 
  # count NAs
  mutate(missing = nrow(starwars) - n, .after = n) %>% 
  # calculate 95% CI around mean
  mutate(CImin = mean - (1.96 * se),
         CImax = mean + (1.96 * se),
        .after = mean)
descr

# Generate print-ready table with kableExtra
descr %>% 
  # select and order summary stats
  select(varname, n, missing, min, max, mean, CImin, CImax, sd, skew, kurtosis) %>%
  # adjust varnames
  mutate(varname = factor(varname,
                       levels = c("height", "mass", "birth_year"),
                       labels = c("Height", "Weight", "Birth Year"))) %>% 
  # generate table
  kableExtra::kbl(
    digits = 2,
    col.names = c("Variable", "n", "NA", "min", "max", "mean", 
                  "CImin", "CImax", "sd", "skewn.", "kurt.")) %>% 
  # apply print theme
  kableExtra::kable_classic(
    html_font = "Times New Roman",
    font_size = 12)

## Visual exploration -------------------------------------
library(GGally)

# Not including species due to many var levels 
# (default max. 15 levels)
pairs1 <- starwars %>%
  select(height, mass, birth_year, gender) %>% 
  GGally::ggpairs()
pairs1
ggsave("3a_pairs1.png",
       # image size
       width = 10, height = 10,
       # background colour
       bg='#ffffff')

# Removing outliers in weight and birth year
pairs2 <- starwars %>% 
  filter(mass < 1000,
         birth_year < 250) %>% 
  select(height, mass, birth_year, gender) %>% 
  GGally::ggpairs()
pairs2
ggsave("3a_pairs2.png",
       # image size
       width = 10, height = 10,
       # background colour
       bg='#ffffff')