# Post 2: Summarize and Visualize Likert scale data
# Henriette L. Arndt, January 9 2023
# https://www.henriettearndt.com/blog

library(likert)

## Likert scale data --------------------------------------
# Response scale labels
scale = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")

# Simulate data
data <- data.frame(
  q1 = sample(scale, size = 1000, replace = TRUE,
              prob = c(0.07, 0.11, 0.08, 0.28, 0.46)), 
  q2 = sample(scale, size = 100, replace = TRUE,
              prob = c(0.19, 0.12, 0.11, 0.23, 0.35)),    
  q3 = sample(scale, size = 100, replace = TRUE,
              prob = c(0.15, 0.25, 0.14, 0.21, 0.25)),  
  q4 = sample(scale, size = 100, replace = TRUE,
              prob = c(0.38, 0.18, 0.13, 0.16, 0.15)),  
  q5 = sample(scale, size = 100, replace = TRUE,
              prob = c(0.46, 0.26, 0.07, 0.14, 0.07))
  )

# Set factors
col_names <- names(data)
data[,col_names] <- lapply(data[,col_names], factor, levels = scale)


### Side note: Transform tidy data ------------------------
# library(tidyverse)
# # Response scale labels
# scale = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")
# 
# data <- tibble %>% 
#   # set factors
#   mutate(
#     across(
#       everything(),
#       ~ fct(.x, scale)
#     )
#   ) %>% 
#   # convert to data frame
#   as.data.frame()


## Summarize data -----------------------------------------
data.summary <- likert(data)
  # add grouping var with argument: grouping = var
data.summary


## Visualize data -----------------------------------------
### Plot 1: 100% stacked bars -----------------------------
plot(data.summary, type = "bar", centered = FALSE)
  # save as png
ggsave("2a_justified.png",
       # image size
       width = 10,
       # background colour
       bg='#ffffff')

### Plot 2: Centered bars with split neutrals -------------
plot(data.summary, 
     type = "bar")
     # overwrite default center category with argument: center = 3.5
     # exclude center category with argument: include.center = FALSE
  # save as png
ggsave("2b_centered.png",
       # image size
       width = 10,
       # background colour
       bg='#ffffff')


## Customize plots ----------------------------------------
# Question labels (bottom to top in the plot)
questions <- c(
  "Question 5: This is the question that most people strongly disagreed with",
  "Question 4: Now there is definitely more disagreement",
  "Question 3: The numbers are evening out",
  "Question 2: Slightly fewer people agreed with this question",
  "Question 1: Most people strongly agreed with this"
)

### Plot 3: Customized bar plot ---------------------------
# likert package syntax
plot(data.summary, 
     type = "bar",
     # legend
     legend.position = "top",
     # bar colours
     low.color = "#FDE725FF", #viridis yellow
     high.color = "#21918c", #viridis green
     # numerical labels
     plot.percents = TRUE,
     plot.percent.high = FALSE,
     plot.percent.low = FALSE
     ) +
# ggplot2 syntax
  # change axis label
  labs(y = "Proportion of total responses") +
  # insert full-length question label
  scale_x_discrete(labels = 
                     #line break after 25 characters
                     stringr::str_wrap(questions, 25))
# save as png
ggsave("2c_customized.png",
       # image size
       width = 10,
       # background colour
       bg='#ffffff')
