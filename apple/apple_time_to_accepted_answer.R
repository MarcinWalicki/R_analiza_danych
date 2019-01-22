library(dplyr)
library(stringi)
library(ggplot2)
options(stringsAsFactors=FALSE)

setwd("C:/Users/tomas/OneDrive/Documents/Studies/PW-IAD/PADwR/zadania_domowe/03")

# Załadowanie potrzebnych danych
posts <- read.csv(file="C:/Users/tomas/Downloads/Apple/apple.stackexchange.com/Posts.csv", header=TRUE, sep=",")
users_countries <- read.csv(file = "C:/Users/tomas/Downloads/Apple/apple.stackexchange.com/Users_countries.csv", header = TRUE, sep =",")

# Przefiltrowanie interesujacych nas danych
posts2 <- posts %>% select(Id, CreationDate, PostTypeId, AcceptedAnswerId, OwnerUserId)
users2 <- users_countries %>% select(Id, Reputation, CreationDate, Country, State)
rm(posts,users_countries)
posts3 <- inner_join(posts2,posts2, by = c("AcceptedAnswerId" = "Id")) %>%
  select(CreationDate = CreationDate.x, 
         OwnerQuestionUserId = OwnerUserId.x, 
         CommentDate = CreationDate.y,
         OwnerAnswerUserId = OwnerUserId.y
         )
posts4 <- inner_join(posts3,users2, by = c("OwnerQuestionUserId" = "Id")) %>% 
  select(CreationDate = CreationDate.x,
         OwnerQuestionUserId,
         CommentDate,
         OwnerAnswerUserId,
         OwnerReputation = Reputation,
         OwnerJoinDate = CreationDate.y,
         OwnerCountry = Country,
         OwnerState = State)
posts5 <- inner_join(posts4,users2, by = c("OwnerAnswerUserId" = "Id")) %>% 
  select(CreationDate = CreationDate.x,
         OwnerQuestionUserId,
         OwnerReputation,
         OwnerJoinDate,
         OwnerCountry,
         OwnerState,
         CommentDate,
         AnswerUserId = OwnerAnswerUserId,
         AnswerReputation = Reputation,
         AnswerJoinDate = CreationDate.y,
         AnswerCountry = Country,
         AnswerState = State)



# Operacje potrzebne do wyłuskania daty
posts5$CreationDate <- stri_join(stri_sub(posts5$CreationDate,1,10),stri_sub(posts5$CreationDate,12,19),sep = " ")          
posts5$CreationDate <- stri_datetime_parse(posts5$CreationDate, format = "yyyy-MM-dd HH:mm:ss")
posts5$CommentDate <- stri_join(stri_sub(posts5$CommentDate,1,10),stri_sub(posts5$CommentDate,12,19),sep = " ")          
posts5$CommentDate <- stri_datetime_parse(posts5$CommentDate, format = "yyyy-MM-dd HH:mm:ss")
posts5$AnswerJoinDate <- stri_join(stri_sub(posts5$AnswerJoinDate,1,10),stri_sub(posts5$AnswerJoinDate,12,19),sep = " ")          
posts5$AnswerJoinDate <- stri_datetime_parse(posts5$AnswerJoinDate, format = "yyyy-MM-dd HH:mm:ss")
posts5$OwnerJoinDate <- stri_join(stri_sub(posts5$OwnerJoinDate,1,10),stri_sub(posts5$OwnerJoinDate,12,19),sep = " ")          
posts5$OwnerJoinDate <- stri_datetime_parse(posts5$OwnerJoinDate, format = "yyyy-MM-dd HH:mm:ss")

posts5$AcceptedTimeDiff <- difftime(posts5$CommentDate,posts5$CreationDate, units = "hours")


# Analiza szybkosci odpowiedzi w podziale na kraj odpowiadajacego - usuwamy kraje count() < 100, bo dają sztuczne odpowiedzi
df_quickness <- posts5 %>%
  group_by(AnswerCountry) %>%
  summarise(AvgAnswerTime = mean(AcceptedTimeDiff, na.rm = TRUE), 
            MedianAnswerTime = quantile(AcceptedTimeDiff,0.5,na.rm = TRUE),
            Quantile75AnswerTime = quantile(AcceptedTimeDiff,0.75,na.rm = TRUE),
            CntAnswer = n()) %>%
  filter(CntAnswer > 100, !is.na(AnswerCountry))

# Wykres średniego czasu odpowiedzi per kraj
ggplot(df_quickness) + 
  geom_bar(aes(x = reorder(AnswerCountry,AvgAnswerTime), y = AvgAnswerTime), stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
# Wykres mediany czasu odpowiedzi
area.colors <- c(rep("darkgrey",1),"darkgreen","firebrick","firebrick1",rep("darkgrey",6),"dodgerblue4","darkred",rep("darkgrey",1),"gold",rep("darkgrey",9))
ggplot(df_quickness) + 
  geom_bar(aes(x = reorder(AnswerCountry,MedianAnswerTime), y = MedianAnswerTime), stat = "identity", fill = area.colors) +
  scale_y_continuous(limits = c(0,5.5),
                     expand = c(0,0),
                     breaks = c(0,1,2,3,4,5), 
                     labels = c("0","1","2","3","4","5")) +
  labs(x = "Kraj", y = "Mediana czasu odpowiedzi")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

# Wykres kwantyla 0.75 czasu odpowiedzi
area.colors2 <- c(rep("darkgrey",2),"firebrick","darkgrey","darkgreen","darkred",rep("darkgrey",2),"firebrick1",rep("darkgrey",1),"dodgerblue4",rep("darkgrey",1),"gold",rep("darkgrey",10))
ggplot(df_quickness) + 
  geom_bar(aes(x = reorder(AnswerCountry,Quantile75AnswerTime), y = Quantile75AnswerTime), stat = "identity", fill = area.colors2) +
  scale_y_continuous(limits = c(0,120),
                     expand = c(0,0),
                     breaks = c(0,20,40,60,80,100,120), 
                     labels = c("0","20","40","60","80","100","120")) +
  labs(x = "Kraj", y = "0.75 kwantyl czasu odpowiedzi")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))


