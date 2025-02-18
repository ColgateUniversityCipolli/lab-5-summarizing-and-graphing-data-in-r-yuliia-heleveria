#Write headers?


#load packages
library("tidyverse")

#Coding Task
#Step 1
#Part 1 - group data by artist
#load data frames for all tracks and allentown
essentia.data.allentown <- read_csv("data/essentia.data.allentown.csv")
essentia.data <- read_csv("data/essentia.data.csv") %>%
  group_by(artist) |>
  summarize(min = min(overall_loudness), #Part 2 - summarize the data by computing statistics
            LF = quantile(overall_loudness, 0.25) - 1.51*IQR(overall_loudness), 
            UF = quantile(overall_loudness, 0.75) + 1.51*IQR(overall_loudness), 
            max = max(overall_loudness)) |>
  #Part 3 - Create two new columns - out of range and unusual
  mutate(out.of.range =  if_else(essentia.data.allentown$overall_loudness < min | 
                                   essentia.data.allentown$overall_loudness > max,
                                 T, F)) |>
  mutate(unusual =  if_else(essentia.data.allentown$overall_loudness < LF | 
                                   essentia.data.allentown$overall_loudness > UF,
                                 T, F)) |>
  #Part 4 - create a column description
  rowwise()|> #apply row by row
  mutate(description = if (out.of.range){
    description = "Out of Range"
    }else if(unusual){
      description = "Outlying"
    }else{
      description = "Within Range"
    })
  
  
