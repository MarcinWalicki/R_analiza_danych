library(dplyr)
#install.packages("Kendall")
options(stringsAsFactors=FALSE)

df <- read.csv(file="C:/Users/Marcin/Desktop/Stosowana analiza regresji_gagolewski/Zadanie 3/Pobrane biblioteki/chess.stackexchange.com/chess_csv/Users.csv", header=TRUE, sep=",")


countries <- read.csv(file="C:/Users/Marcin/Desktop/Stosowana analiza regresji_gagolewski/Zadanie 3/Pobrane biblioteki/apple.stackexchange.com/apple_csv/countries.csv", header=TRUE, sep=",")
states <- read.csv(file="C:/Users/Marcin/Desktop/Stosowana analiza regresji_gagolewski/Zadanie 3/Pobrane biblioteki/apple.stackexchange.com/apple_csv/states.csv", header=TRUE, sep=",")
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

n <- 10
br = as.integer(seq(1,as.integer(quantile(df$Reputation,0.99)),length.out = n))
br[n+1] <- as.integer(max(df$Reputation))

ranges = paste(head(br,-1), br[-1], sep=" - ")
range_from = br[1:n]
range_to = br[2:(n+1)]
freq = hist(df$Reputation, breaks=br, include.lowest=TRUE, plot=FALSE)
reputation_frequency <- data.frame(range_from = range_from, range_to = range_to, range = ranges, frequency = freq$counts)
#koniec potem zakomentowania

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
reputation_sorted <- reputation_aggregated %>% arrange(desc(avg))
head(reputation_sorted)
reputation_average <- reputation_sorted[,-2]
reputation_average <- reputation_average[,-2]
reputation_average <- reputation_average[,-3]
reputation_average <- reputation_average[,-3]
reputation_average <- reputation_average[,-3]
reputation_average <- cbind(reputation_average,1:nrow(reputation_average))
reputation_average <- data.frame(reputation_average)

######### Spearman
Spearman_reputation <- function(a)
{
  ranking_krajow <- read.csv(file="C:/Users/Marcin/Desktop/Stosowana analiza regresji_gagolewski/Zadanie 3/Pobrane biblioteki/chess.stackexchange.com/chess_csv/best_countries.csv", header=TRUE, sep=",")
  countries_rating <- ranking_krajow[ranking_krajow$place<=a,]
  countries_rating <- data.frame(countries_rating)
  countries_rating$place <- as.numeric(countries_rating$place)
  
  AIWA <- reputation_average
  colnames(AIWA) <- c("country","reputation_avg","rank")
  AIWA <- left_join(countries_rating,AIWA)
  ST <- AIWA %>% arrange(rank)
  ST <- cbind(ST,1:nrow(ST))
  ST <- ST[,-2]
  ST <- ST[,-2]
  ST <- ST[,-2]
  colnames(ST) <- c("place","rank")
  ST <- cbind(ST,(ST$place-ST$rank)^2)
  colnames(ST) <- c("place","rank","D_i")
  m <- nrow(ST)
  T <- 1-(6*sum(ST$D_i)/(m*((m^2)-1)))
  c(T,m)
}

######### Kendall

Kendall_reputation <- function(a)
{
  ranking_krajow <- read.csv(file="C:/Users/Marcin/Desktop/Stosowana analiza regresji_gagolewski/Zadanie 3/Pobrane biblioteki/chess.stackexchange.com/chess_csv/best_countries.csv", header=TRUE, sep=",")
  countries_rating <- ranking_krajow[ranking_krajow$place<=a,]
  countries_rating <- data.frame(countries_rating)
  countries_rating$place <- as.numeric(countries_rating$place)
  
  AIWA <- reputation_average
  colnames(AIWA) <- c("country","reputation_avg","rank")
  AIWA <- left_join(countries_rating,AIWA)
  ST <- AIWA %>% arrange(rank)
  ST <- cbind(ST,1:nrow(ST))
  ST <- ST[,-2]
  ST <- ST[,-2]
  ST <- ST[,-2]
  colnames(ST) <- c("place","rank")
  m <- nrow(ST)
  K <- Kendall::Kendall(ST$place,ST$rank)
  c(K$tau[1],m)
}


######### Wykresy

Spearman <- rep(0,40)
Kendall <- rep(0,40)
for(i in 3:40){
  Spearman[Spearman_reputation(i)[2]] <- Spearman_reputation(i)[1]
  Kendall[Kendall_reputation(i)[2]] <- Kendall_reputation(i)[1]
}
Spearman[1] <- 0
Spearman[2] <- 0
Kendall[1] <-0
Kendall[2] <-0
plot(1:40,Spearman,col="blue",type="l",ylim=c(-1.1,1.1)
     ,xlab="Liczba najlepszych krajów wziêta pod uwagê",
     ylab="Wsp. korelacji")
lines(1:40,Kendall,col="green",type="l")
######### linia, pod nia nad nia uznamy, ze zalezne. Pod nia, ze niezalezne
# do ilosci u¿ytkowników na forum szachowym

z <- qnorm(0.95,mean=0,sd=1)
vector_odrz_S <- rep(0,40)
vector_odrz_K <- rep(0,40)
vector_odrz_S_2 <- rep(0,40)
vector_odrz_K_2 <- rep(0,40)
for (j in 3:40){
  vector_odrz_S[j] <- z/(sqrt(j-1))
  vector_odrz_K[j] <- z/(sqrt((9*j*(j-1))/(2*(2*j+5))))
}
vector_odrz_K[1] <- 10000
vector_odrz_K[2] <- 5000
vector_odrz_S[1] <- 10000
vector_odrz_S[2] <- 5000
vector_odrz_S_2 <- (-1)*vector_odrz_S
vector_odrz_K_2 <- (-1)*vector_odrz_K
lines(1:40,vector_odrz_S,col="red",type="l")
lines(1:40,vector_odrz_K,col="black",type="l")
lines(1:40,vector_odrz_S_2,col="red",type="l")
lines(1:40,vector_odrz_K_2,col="black",type="l")
legend("topright",col=c("blue","green","red","black"),
     legend=c("Spearman","Kendall","funkcja krytyczna Spearman","funkcja krytyczna Kendall"),
     pch=15,cex=0.8)