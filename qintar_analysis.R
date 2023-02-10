library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemes)

source("~/Desktop/Quran/Qintar Analysis/themes.R")

filepath <- "/Users/yahiasalem/Desktop/Quran/Qintar Analysis/res/qintar_dataset.csv"
filename <- basename(filepath)

data <- read.csv(filepath, na.strings=c("","NA"), encoding = 'UTF-8')

data$length <- sapply(data$aya_text_emlaey, str_replace_all, fixed(" "), "") %>% sapply(nchar)

data_sorted <- data[order(data$length),]


# --------- functions ---------

p_length_calc <- function(df, surah_no) {
  df <- data[data$sora == surah_no, ] %>% drop_na()
  
  total_length <- 0.0
  p <- unique(df$page)
  
  for (i in 1:length(p)) {
    
    s <- df[df$page == p[i], ]
    
    l1 <- s$line_start[1]
    l2 <- tail(s$line_end, n=1)
    
    total_length <- total_length + (l2 - l1 + 1) / 15
  }
  
  return(total_length)
}

a_length_calc <- function(df, surah_no) {
  df <- data[data$sora == surah_no, ] %>% drop_na()
  return(length(df$id))
}

l_length_calc <- function(df, surah_no) {
  df <- data[data$sora == surah_no, ] %>% drop_na()
  return(sum(df$length))
}

remove_na <- function(v) {
  return(v[!is.na(v)])
}

# -----------------------------

density_a <- vector("numeric", 114)
pages_count <- vector("numeric", 114)

for (i in 1:114) {
  density_a[i] <- a_length_calc(data, i) / p_length_calc(data, i)
  pages_count[i] <- p_length_calc(data, i)
}

surah_df <- data.frame(
  ayah_density = density_a,
  surah_no = unique(data$sora) %>% remove_na(),
  surahs_ar = unique(data$sora_name_ar) %>% remove_na() %>% factor(),
  surahs_en = unique(data$sora_name_en) %>% remove_na() %>% factor(),
  ayah_count = aggregate(data$aya_no, by=list(data$sora), FUN=length)$x,
  pages_count = pages_count
)

write.csv(surah_df, file="~/Desktop/Quran/Qintar Analysis/data_pure.csv")

surah_df <- surah_df[order(surah_df$ayah_density, decreasing = TRUE),]

par(mar = c(8, 8, 4, 0), mgp = c(5, 1, 0), family = "Times")

my_bar <- barplot(
  surah_df$ayah_density,
  main = "Ayah Density in Every Surah of The Holy Quran",
  xlab = "Surah No.",
  ylab = "Density (ayah/page)",
  names.arg = paste0(surah_df$surahs_en, " ", surah_df$surah_no),
  cex.names = .6,
  cex.lab = 1.3,
  cex.main = 2.5,
  font.lab = 2,
  las = 2,
  ylim = c(0, max(surah_df$ayah_density) + 1),
  border="coral",
  col="coral"
  )

text(my_bar, surah_df$ayah_density + 0.5, round(surah_df$ayah_density, 1), cex = .5)

# png("/Users/yahiasalem/Desktop/Quran/Quran0.png")

dev.off()

ggplot(surah_df, aes(x=reorder(paste0(surahs_en, " (", surah_no, ")"), -ayah_density), y=ayah_density)) + 
  geom_bar(stat="identity") + 
  geom_text(
    aes(label=paste0(round(ayah_density, 1), " (", ayah_count, ")")),
    nudge_y = 1.75,
    size = 2.5,
    col="grey70",
    angle = 90
    ) +
  labs(
    x = "Surah No.", 
    y = "Density (ayah/page)",
    title = "Ayah Density in Every Surah of The Holy Quran",
    caption = "Yahia Salem"
    ) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(mult=c(0,0.05))) +
  scale_colour_Publication() +
  theme_dark_grey()

count_until <- function(ayahs_count, position=1) {
  surah_df <- surah_df[order(surah_df$ayah_density, decreasing = TRUE),]
  current_count = 0
  total_pages = 0
  i = 1
  while (current_count < ayahs_count) {
    current_surah <- surah_df[i,]
    current_count = current_count + current_surah$ayah_count
    print(paste(i, current_count))
    total_pages = total_pages + current_surah$pages_count
    i = i + 1
  }
  print(total_pages)
  return(surah_df$surahs_ar[1:i])
}
