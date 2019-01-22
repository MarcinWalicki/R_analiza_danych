setwd("C:/Users/tomas/OneDrive/Documents/Studies/PW-IAD/PADwR/zadania_domowe/03")
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

options(stringsAsFactors=FALSE)

df <- read.csv(file="C:/Users/tomas/Downloads/Apple/apple.stackexchange.com/Users.csv", header=TRUE, sep=",")
countries <- read.csv(file="PADwR---PD3---Data-Science/countries.csv", header=TRUE, sep=",")
states <- read.csv(file="PADwR---PD3---Data-Science/states.csv", header=TRUE, sep=",")


# Przypisanie userom odpowiednich lokalizacji
df$country <- NA
df$state <- NA
for(i in 1:length(df$Location)){
  location_split <- unlist(strsplit(df$Location[i], ", "))
  location_position <- location_split %in% countries$country
  if (any(location_split %in% states$code)) {
    df$country[i] <- "United States"
    df$state[i] <- location_split[location_split %in% states$code]
  } else {
    if (identical(character(0),location_split[location_position])){
      df$country[i] <- NA
    } else {
      df$country[i] <- location_split[location_position]
    }
    
  }
}

#write.csv(df,"C:/Users/tomas/Downloads/Apple/apple.stackexchange.com/Users_countries.csv", sep = ",")

### Analiza ilościowa i przedstawienie na mapie iloścy userów na świecie
df_tmp <- df[,c("Location","country","state")]
countries_grouped <- inner_join(df_tmp,countries, by = c("country" = "country")) %>%
  group_by(country,lat,long) %>%
  summarise(cnt = n())

countries_grouped_no_usa <- countries_grouped %>% 
  filter(country != "United States")

countries_grouped_just_usa <- inner_join(df_tmp,states, by = c("state" = "code")) %>%
  group_by(state=state.y,lat,long) %>%
  summarise(cnt = n()) %>%
  filter(long > -130)

world <- map_data("world")
ggplot() + geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  geom_point(data = countries_grouped, aes(x = long, y = lat, size = cnt, color = cnt)) +
  scale_color_gradient(low="deepskyblue", high="firebrick1") + 
  ylim(-60,90) + 
  theme_minimal() + 
  guides(size=FALSE) +
  borders("world", colour = "white") +
  labs(color='Ilość użytkowników')  +
  theme(
    plot.title = element_text(hjust=0.5, size = 25),      # ustawienia tytul wykresu
    axis.title.x = element_blank(),               # ustawienia nazwy osi x
    axis.text.x = element_blank(),                # ustawienia labeli przy osi x
    axis.text.y = element_blank(),
    axis.title.y = element_blank())

### Analiza ilościowa i przedstawienie na mapie iloścy userów w USA
usa <- map_data("usa")
ggplot() + geom_polygon(data = usa, aes(x = long, y = lat, group = group)) +
  geom_point(data = countries_grouped_just_usa, aes(x = long, y = lat, size = cnt, color = cnt)) +
  scale_color_gradient(low="blue", high="firebrick1") +
  borders("state", colour = "white") +
  ylim(23,50) +
  theme_minimal() + 
  guides(size=FALSE) +
  labs(color='Ilość użytkowników')  +
  theme(
    plot.title = element_text(hjust=0.5, size = 25),      # ustawienia tytul wykresu
    axis.title.x = element_blank(),               # ustawienia nazwy osi x
    axis.text.x = element_blank(),                # ustawienia labeli przy osi x
    axis.text.y = element_blank(),
    axis.title.y = element_blank())


### Analiza reputacji
# Tworzenie kubełków do "histogramu"
# n <- 10
# br = as.integer(seq(1,as.integer(quantile(df$Reputation,0.99)),length.out = n))
# br[n+1] <- as.integer(max(df$Reputation))
# 
# ranges = paste(head(br,-1), br[-1], sep=" - ")
# range_from = br[1:n]
# range_to = br[2:(n+1)]
# freq = hist(df$Reputation, breaks=br, include.lowest=TRUE, plot=FALSE)
# reputation_frequency <- data.frame(range_from = range_from, range_to = range_to, range = ranges, frequency = freq$counts)

reputation_aggregated <- inner_join(df,countries,by = c("country" = "country")) %>%
  group_by(country,lat,long) %>%
  summarise(avg = mean(Reputation, na.rm = TRUE),cnt = n(),med = median(Reputation, na.rm = TRUE))
reputation_aggregated$avg_basket <- case_when(
  between(reputation_aggregated$avg,reputation_frequency$range_from[1],reputation_frequency$range_to[1]) ~ reputation_frequency$range_from[1],
  between(reputation_aggregated$avg,reputation_frequency$range_from[2],reputation_frequency$range_to[2]) ~ reputation_frequency$range_from[2],
  between(reputation_aggregated$avg,reputation_frequency$range_from[3],reputation_frequency$range_to[3]) ~ reputation_frequency$range_from[3],
  between(reputation_aggregated$avg,reputation_frequency$range_from[4],reputation_frequency$range_to[4]) ~ reputation_frequency$range_from[4],
  between(reputation_aggregated$avg,reputation_frequency$range_from[5],reputation_frequency$range_to[5]) ~ reputation_frequency$range_from[5],
  between(reputation_aggregated$avg,reputation_frequency$range_from[6],reputation_frequency$range_to[6]) ~ reputation_frequency$range_from[6],
  between(reputation_aggregated$avg,reputation_frequency$range_from[7],reputation_frequency$range_to[7]) ~ reputation_frequency$range_from[7],
  between(reputation_aggregated$avg,reputation_frequency$range_from[8],reputation_frequency$range_to[8]) ~ reputation_frequency$range_from[8],
  between(reputation_aggregated$avg,reputation_frequency$range_from[9],reputation_frequency$range_to[9]) ~ reputation_frequency$range_from[9],
  between(reputation_aggregated$avg,reputation_frequency$range_from[10],reputation_frequency$range_to[10]) ~ reputation_frequency$range_from[10]
)

# World
world <- map_data("world")
# ggplot() + geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
#   geom_point(data = reputation_aggregated, aes(x = long, y = lat, size = avg_basket, color = avg_basket)) +
#   scale_color_gradient(low="darkolivegreen1", high="darkolivegreen4") + theme_minimal()
ggplot() + geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  geom_point(data = reputation_aggregated %>% filter(cnt > 100), aes(x = long, y = lat, size = avg_basket, color = avg_basket)) +
  scale_color_gradient(low="yellow", high="firebrick") + 
  theme_minimal() + 
  ylim(-60,90) +
  guides(size=FALSE) +
  borders("world", colour = "white") +
  labs(color='Średni ranking')  +
  theme(
    plot.title = element_text(hjust=0.5, size = 25),      # ustawienia tytul wykresu
    axis.title.x = element_blank(),               # ustawienia nazwy osi x
    axis.text.x = element_blank(),                # ustawienia labeli przy osi x
    axis.text.y = element_blank(),
    axis.title.y = element_blank())


# Europe
europ_countries <- read.csv(file="C:/Users/tomas/OneDrive/Documents/Studies/PW-IAD/PADwR/zadania_domowe/03/PADwR---PD3---Data-Science/european_countries.csv",header = TRUE)
europe_map <- map_data("world", region = europ_countries$country)

reputation_aggregated2 <- reputation_aggregated %>% 
  ungroup() %>%
  filter(country %in% c(europ_countries$country,"United Kingdom")) %>%
  mutate(long=ifelse(country == "Russia",45,long)) %>% 
  mutate(lat=ifelse(country == "Russia",55,lat))


ggplot() + geom_polygon(data = europe_map, aes(x = long, y = lat, group = group)) + 
  borders("world", colour = "white")+
  geom_point(data = reputation_aggregated2, aes(x = long, y = lat, size = avg_basket, color = avg_basket)) +
  scale_color_gradient(low="yellow", high="firebrick") + 
  theme_minimal() + 
  xlim(-25,60) + 
  ylim(30,80) +
  guides(size=FALSE) +
  labs(color='Średni ranking')  +
  theme(
    plot.title = element_text(hjust=0.5, size = 25),      # ustawienia tytul wykresu
    axis.title.x = element_blank(),               # ustawienia nazwy osi x
    axis.text.x = element_blank(),                # ustawienia labeli przy osi x
    axis.text.y = element_blank(),
    axis.title.y = element_blank())

# Pokazanie słupkowo reputacji ze świata
reputation_aggregated3 <- reputation_aggregated %>%
  filter(avg > 110)
ggplot(reputation_aggregated3, aes(x = reorder(country, -cnt), y = avg)) + geom_bar(stat = "identity",aes(fill = cnt)) +
  scale_fill_gradient(low="gray", high="firebrick") +
  theme_minimal() +
  coord_flip()+
  theme(
    panel.background = element_blank(),    
    panel.grid.major.x =  element_blank(),
    panel.grid.major.y =  element_line(color = "grey", size = 1),
    panel.grid.minor.y =  element_line(color = "grey", size = 0.2),
    axis.text.x = element_text(angle = 45, hjust = 0.5, size = 13),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 13),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.title=element_blank(),
    legend.text = element_text(size = 13)
  )

