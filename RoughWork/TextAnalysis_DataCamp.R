---
title: "Text Analysis Course Datacamp"
author: "Shane Crinion"
date: "4 June 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

The twitter_data data frame has over 7,000 tweets about airlines. The tweets have already been classified as either complaints or non-complaints in the complaint_label column. Let's get a sense of how many of these tweets are complaints

```{r lesson 1}
## TEXT ANALYSIS COURSE DATACAMP

# Load the tidyverse packages
library(tidyverse)

# Print twitter_data
print(twitter_data)

# Print just the complaints in twitter_data
twitter_data %>% 
  filter(complaint_label == "Complaint")


# Start with the data frame
twitter_data %>% 
  # Group the data by whether or not the tweet is a complaint
  group_by(complaint_label) %>% 
  # Compute the mean, min, and max follower counts
  summarize(
    avg_followers = mean(usr_followers_count),
    min_followers = min(usr_followers_count),
    max_followers = max(usr_followers_count)
  )

```



```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
