####################################################################################################
## Data analysis for research paper "Body-based units of measure in cultural evolution" 
## Kaaronen, Manninen and Eronen 2023.
## Contact: roope.kaaronen@helsinki.fi
## Date: 17.3.2023
####################################################################################################

# Download (or install) the following packages:
library(readxl)
library(tidyverse)
library(maps)
library(gtools)
library(qdap)
library(tm)
library(stringr)
library(ggpubr)
library(lsa)

# Download Dataset.xslx from https://doi.org/10.17605/OSF.IO/FEGVR
# Then, read data:

setwd("~/") # Set working directory to the folder containing Dataset.xlsx
df <- read_excel("Dataset.xlsx")

## Set the cultures in dataset on a world map.
####################################################################

# Draw the world map template
world <- map_data("world")

# Add cultures (longitude and latitude) on the map
map <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = df,
    aes(Long, Lat, colour = factor(SCCS)), size = 1.4, shape = 18, alpha = 0.5
  ) +
  theme_bw() +
  ggtitle("All body-based units") +
  scale_color_manual(breaks = c("T", "F"), labels = c("Yes", "No"), values = c("#005AB5", "#DC3220")) +
  labs(colour = "SCCS") +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 90)) +
  scale_y_continuous(breaks = seq(from = -90, to = 90, by = 45)) +
  xlab("Longitude") +
  ylab("Latitude") + theme(text=element_text(size=7), #change font size of all text
                           axis.text=element_text(size=7), #change font size of axis text
                           axis.title=element_text(size=7), #change font size of axis titles
                           plot.title=element_text(size=7), #change font size of plot title
                           legend.text=element_text(size=7), #change font size of legend text
                           legend.title=element_text(size=7)) #change font size of legend title  



## Descriptive analysis: counting occurrence of body-based 
## units and coded themes.
####################################################################

# Step 1. Analyse frequencies of body-based units of measure

# Ensure data is in character format
df$`Body dimension` <- as.character(df$`Body dimension`) 

# Prepare the text by lower casing, removing numbers, etc.
df$`Body dimension` <- tolower(df$`Body dimension`)
df$`Body dimension` <- tm::removeNumbers(df$`Body dimension`)
df$`Body dimension` <- str_replace_all(df$`Body dimension`, ";", "")

corpus <- Corpus(VectorSource(df$`Body dimension`)) # turn into corpus

# Create tdm from the corpus
tdm <- TermDocumentMatrix(corpus) 
bodydimensions <- as.matrix(tdm)
bodydimensionsList <- sort(rowSums(bodydimensions), decreasing=TRUE)

# View all body dimension codes and frequencies thereof
View(bodydimensionsList)

# Select only SCCS cultures and repeat
bodySCCS <- df[df$SCCS == "T",]
corpusSCCS <- Corpus(VectorSource(bodySCCS$`Body dimension`)) # turn into corpus
tdmSCCS <- TermDocumentMatrix(corpusSCCS) 
bodydimensionsSCCS <- as.matrix(tdmSCCS)
bodydimensionsListSCCS <- sort(rowSums(bodydimensionsSCCS), decreasing=TRUE)
View(bodydimensionsListSCCS)
roundSCCS <- bodydimensionsListSCCS/186 # Count proportion of total SCCS (186 cultures)
roundSCCS <- round(roundSCCS, digits = 3) # Round up to 3 digits
View(roundSCCS) # View as proportion of total SCCS

# Step 2. Analyse frequencies of coded themes (cultural domains body-based units are used in)

# Prepare the text for analysis
df$Themes <- as.character(df$Themes)
df$Themes <- tolower(df$Themes)
df$Themes <- tm::removeNumbers(df$Themes)
df$Themes <- str_replace_all(df$Themes, ";", "")

corpus2 <- Corpus(VectorSource(df$Themes)) # turn into corpus
tdm2 <- TermDocumentMatrix(corpus2) # create tdm from the corpus
Themes <- as.matrix(tdm2)
ThemesList <- sort(rowSums(Themes), decreasing=TRUE)

# View all coded themes and frequencies thereof
View(ThemesList)

# Select cultures in SCCS and repeat
themeSCCS <- df[df$SCCS == "T",]
corpus2SCCS <- Corpus(VectorSource(themeSCCS$Themes)) # turn into corpus
tdm2SCCS <- TermDocumentMatrix(corpus2SCCS) # create tdm from the corpus
ThemesSCCS <- as.matrix(tdm2SCCS)
ThemesListSCCS <- sort(rowSums(ThemesSCCS), decreasing=TRUE)
View(ThemesListSCCS)

roundSCCS2 <- ThemesListSCCS/186 # Count proportion of total SCCS
roundSCCS2 <- round(roundSCCS2, digits = 3)
View(roundSCCS2)

## Draw more maps of specific body-based units.
####################################################################

# Draw a map with all cultures with 'fathom' as a body-based unit

df$fathom <- str_detect(df$`Body dimension`, "fathom") # Extract matching rows
fathom <- df[df$fathom == 'TRUE', ]
fathom <- fathom %>% drop_na(fathom)

# Add cultures (longitude and latitude) on the map
mapFathom <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = fathom,
    aes(Long, Lat, colour = factor(SCCS)), size = 1.4, shape = 18, alpha = 0.5
  ) +
  theme_bw() +
  scale_color_manual(breaks = c("T", "F"), labels = c("Yes", "No"), values = c("#005AB5", "#DC3220")) +
  labs(colour = "SCCS") +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 90)) +
  scale_y_continuous(breaks = seq(from = -90, to = 90, by = 45)) +
  ggtitle("Fathom") +
  xlab("Longitude") +
  ylab("Latitude") + theme(text=element_text(size=7), #change font size of all text
                           axis.text=element_text(size=7), #change font size of axis text
                           axis.title=element_text(size=7), #change font size of axis titles
                           plot.title=element_text(size=7), #change font size of plot title
                           legend.text=element_text(size=7), #change font size of legend text
                           legend.title=element_text(size=7)) #change font size of legend title  


# Draw a map with all cultures with 'cubit' as a body-based unit

df$cubit <- str_detect(df$`Body dimension`, "cubit") # Extract matching rows
cubit <- df[df$cubit == 'TRUE', ]
cubit <- cubit %>% drop_na(cubit)

# Add cultures (longitude and latitude) on the map
mapCubit <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = cubit,
    aes(Long, Lat, colour = factor(SCCS)), size = 1.4, shape = 18, alpha = 0.5
  ) +
  theme_bw() +
  scale_color_manual(breaks = c("T", "F"), labels = c("Yes", "No"), values = c("#005AB5", "#DC3220")) +
  labs(colour = "SCCS") +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 90)) +
  scale_y_continuous(breaks = seq(from = -90, to = 90, by = 45)) +
  ggtitle("Cubit") +
  xlab("Longitude") +
  ylab("Latitude") + theme(text=element_text(size=7), #change font size of all text
                           axis.text=element_text(size=7), #change font size of axis text
                           axis.title=element_text(size=7), #change font size of axis titles
                           plot.title=element_text(size=7), #change font size of plot title
                           legend.text=element_text(size=7), #change font size of legend text
                           legend.title=element_text(size=7)) #change font size of legend title  



# Draw a map with all cultures with 'hand span' as a body-based unit

df$span <- str_detect(df$`Body dimension`, "hand-span") # Extract matching rows with str_detect
span <- df[df$span == 'TRUE', ]
span <- span %>% drop_na(span)

# Add cultures (longitude and latitude) on the map
mapSpan <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = span,
    aes(Long, Lat, colour = factor(SCCS)), size = 1.4, shape = 18, alpha = 0.5
  ) +
  theme_bw() +
  scale_color_manual(breaks = c("T", "F"), labels = c("Yes", "No"), values = c("#005AB5", "#DC3220")) +
  labs(colour = "SCCS") +
  scale_x_continuous(breaks = seq(from = -180, to = 180, by = 90)) +
  scale_y_continuous(breaks = seq(from = -90, to = 90, by = 45)) +
  ggtitle("Hand span") +
  xlab("Longitude") +
  ylab("Latitude") + theme(text=element_text(size=7), #change font size of all text
                           axis.text=element_text(size=7), #change font size of axis text
                           axis.title=element_text(size=7), #change font size of axis titles
                           plot.title=element_text(size=7), #change font size of plot title
                           legend.text=element_text(size=7), #change font size of legend text
                           legend.title=element_text(size=7)) #change font size of legend title  


# Print all four maps (Figure 2)
FourMaps <- ggarrange(map, mapFathom, mapCubit, mapSpan, ncol = 2, nrow = 2, common.legend = TRUE,   font.label = list(size = 14, color = "black", face = "bold", family = NULL),
                      labels = c("A", "B", "C", "D"))

tiff(filename = "Figure2.tiff",
     width = 18.4, height = 13, units = "cm", pointsize = 7, res=300)
FourMaps
dev.off()

######################
## END
######################
