#Write headers!!!


#load packages
library("tidyverse")
library("xtable")
library("patchwork")

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

#function for analyzing each feature of the dataset
feature_analysis <- function(feature){
  essentia.statistics <- essentia.data |>
    group_by(artist) |>
    summarize(min = min(get(feature), na.rm = T), #Part 2 - summarize the data by computing statistics, remove NA
              LF = quantile(get(feature), 0.25, na.rm = T) - 1.51*IQR(get(feature), na.rm = T), 
              UF = quantile(get(feature), 0.75, na.rm = T) + 1.51*IQR(get(feature), na.rm = T), 
              max = max(get(feature), na.rm = T)) |>
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

#Test if the function works for overall_loudness first
stats.feature <- feature_analysis("overall_loudness")

#craete a tibble to store feature comarison
stats.analysis.allentown <- tibble(Feature = character(), All.Get.Out = character(),
                                   Manchester.Orchestra = character(), 
                                   The.Front.Bottoms = character())


#Apply function to essentia.data
for (i in 4:length(colnames(essentia.data))){
  feature <- colnames(essentia.data)[i]
  if(feature != "chords_scale" & feature != "chords_key" & feature != "key" & feature != "mode"){
    feature.analysis.allentown <- feature_analysis(feature)
    new_row <- tibble(Feature = feature,
                      All.Get.Out = feature.analysis.allentown |> slice(1) |> pull("description"), 
                      Manchester.Orchestra = feature.analysis.allentown |> slice(2) |> pull("description"), 
                      The.Front.Bottoms = feature.analysis.allentown |> slice(3) |> pull("description"))
    stats.analysis.allentown <- bind_rows(stats.analysis.allentown, new_row)
  }
}

#summarize the features to create a table
allentown.summary <- stats.analysis.allentown |>
  select(-Feature) |>
  pivot_longer(cols = everything(),
               names_to = "Artist",
               values_to = "Category") |>
  count(Artist, Category) |>
  pivot_wider(names_from = Artist, values_from = n)

#write a csv file to create a table from it
write_csv(allentown.summary, "allentown.summary.csv", col_names = T)
#allentown.summary <- read.csv("allentown.summary.csv")
table.allentown <-xtable(allentown.summary, label = "allentown.tab",
                         caption = "Comparison of Allentown's audio features with the range of band's features")
align(table.allentown) <- "c|l|c|c|c|"
print(table.allentown, include.rownames = F)

#create a column plot to summarize the data
#create threww column plots one for each of three categories and all bands

#pivot data longer to flip the coordinates
long.allentown.summary <- allentown.summary |>
  pivot_longer(cols = -Category, 
               names_to = "Band",
               values_to = "Count")

#filter out of range variables
out.of.range.filter <- long.allentown.summary|>
  filter(Category == 'Out of Range')

#out of range column plot
allentown.out.of.range.col.plot <- ggplot(out.of.range.filter)+
  geom_col(aes(x = Band,
               y = Count,
               fill = Band))+
  scale_fill_manual(values = c("All.Get.Out" = "royalblue1", 
                               "Manchester.Orchestra" = "plum2", 
                               "The.Front.Bottoms" = "purple"))+
  ylab("Out of Range Count")+
  guides(fill = "none")+
  theme_bw()

#filter outlying variables
outlying.filter <- long.allentown.summary|>
  filter(Category == 'Outlying')

#outlying column plot
allentown.outlying.col.plot <- ggplot(outlying.filter)+
  geom_col(aes(x = Band,
               y = Count,
               fill = Band))+
  scale_fill_manual(values = c("All.Get.Out" = "royalblue1", 
                               "Manchester.Orchestra" = "plum2", 
                               "The.Front.Bottoms" = "purple"))+
  ylab("Outlying Count")+
  guides(fill = "none")+
  theme_bw()

#filter within range variables
within.range.filter <- long.allentown.summary|>
  filter(Category == 'Within Range')

#within range column plot
allentown.within.range.col.plot <- ggplot(within.range.filter)+
  geom_col(aes(x = Band,
               y = Count,
               fill = Band))+
  scale_fill_manual(values = c("All.Get.Out" = "royalblue1", 
                               "Manchester.Orchestra" = "plum2", 
                               "The.Front.Bottoms" = "purple"))+
  ylab("Within Range Count")+
  guides(fill = "none")+
  theme_bw()

#3 plots combines using facet_wrap()
allentown.wrapped <- ggplot(long.allentown.summary)+
  geom_col(aes(x = Band,
               y = Count,
               fill = Band))+
  scale_fill_manual(values = c("All.Get.Out" = "royalblue1", 
                               "Manchester.Orchestra" = "plum2", 
                               "The.Front.Bottoms" = "purple"))+
  ylab("Range Check")+
  guides(fill = "none")+
  facet_wrap(~Category)+
  theme_bw()

#combine graphs using patchwork library
combined.graphs <- allentown.out.of.range.col.plot + allentown.outlying.col.plot +allentown.within.range.col.plot
