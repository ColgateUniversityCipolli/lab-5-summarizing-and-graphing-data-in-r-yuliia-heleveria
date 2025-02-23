#Write headers!!!


#load packages
library("tidyverse")

#Coding Task
#Step 1
#Part 1 - group data by artist
#load data frames for all tracks and allentown
essentia.data.allentown <- read_csv("data/essentia.data.allentown.csv")
essentia.data <- read_csv("data/essentia.data.csv") 
essentia.statistics <- essentia.data |>
  group_by(artist) |>
  summarize(min = min(overall_loudness), #Part 2 - summarize the data by computing statistics
            LF = quantile(overall_loudness, 0.25) - 1.51*IQR(overall_loudness), 
            UF = quantile(overall_loudness, 0.75) + 1.51*IQR(overall_loudness), 
            max = max(overall_loudness)) |>
  #Part 3 - Create two new columns - out of range and unusual
  mutate(out.of.range = if_else(essentia.data.allentown$overall_loudness < min | 
                                   essentia.data.allentown$overall_loudness > max,
                                 T, F)) |>
  mutate(unusual =  if_else(essentia.data.allentown$overall_loudness < LF | 
                                   essentia.data.allentown$overall_loudness > UF,
                                 T, F)) |>
  #Part 4 - create a column description
  rowwise()|> #apply row by row
  mutate(description = case_when(
    out.of.range ~ "Out of Range",
    unusual ~ "Outlying",
    TRUE ~ "Within Range"))|>
  ungroup()

#Create the function for analyzing each feature
for (i in 1:length(essentia.data)){
  print(i)
}

#function for analyzing each feature of the dataset
feature_analysis <- function(feature){
  essentia.statistics <- essentia.data |>
    group_by(artist) |>
    summarize(min = min(get(feature)), #Part 2 - summarize the data by computing statistics
              LF = quantile(get(feature), 0.25) - 1.51*IQR(get(feature)), 
              UF = quantile(get(feature), 0.75) + 1.51*IQR(get(feature)), 
              max = max(get(feature))) |>
    #Part 3 - Create two new columns - out of range and unusual
    mutate(out.of.range = if_else(essentia.data.allentown[[feature]] < min | 
                                    essentia.data.allentown[[feature]] > max,
                                  T, F)) |>
    mutate(unusual =  if_else(essentia.data.allentown[[feature]] < LF | 
                                essentia.data.allentown[[feature]] > UF,
                              T, F)) |>
    #Part 4 - create a column description
    rowwise()|> #apply row by row
    mutate(description = case_when(
      out.of.range ~ "Out of Range",
      unusual ~ "Outlying",
      TRUE ~ "Within Range"))|>
    ungroup()
}

stats.feature <- feature_analysis("overall_loudness")
