library(stringi)
library(ggplot2)
library(dplyr)
library(reshape)

options(stringsAsFactors=FALSE)

tags_datascience <- read.csv(file="C:/Users/tomas/Downloads/DataScience/Tags.csv", header=TRUE, sep=",")
tags_stack <- read.csv(file="C:/Users/tomas/Downloads/Tags.csv", header=TRUE, sep=",")
users <- read.csv(file = "C:/Users/tomas/Downloads/DataScience/Users.csv")
posts_datascience <- read.csv(file = "C:/Users/tomas/Downloads/DataScience/Posts.csv")
posts_german_stack <- read.csv(file = "C:/Users/tomas/Downloads/german/Posts.csv")


tags <- inner_join(tags_datascience, tags_stack, by = c("TagName" = "TagName")) %>%
          select(TagName, CountDataScience = Count.x, CountStack = Count.y)
tags$FracDataScience <- (tags$CountDataScience / sum(tags_datascience$Count))
tags$FracStack <- (tags$CountStack / sum(tags_stack$Count))
tags$FracInDataScience <- tags$CountDataScience / sum(tags$CountDataScience)
tags$FracInStack <- tags$CountStack / sum(tags$CountStack)

ggplot(tags %>% filter(FracDataScience > 0.01)) + 
  geom_bar(aes(x = reorder(TagName,-FracDataScience), 
               y = FracDataScience),
           stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))
ggplot(tags %>% filter(FracStack > 0.001)) + 
  geom_bar(aes(x = reorder(TagName,-FracStack), 
               y = FracStack),
           stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))

tags_stack %>% filter(TagName == "data science")


### Ilość użytkowników zarejestrowanych w danym miesiącu na forum Data Science
users$CreationDate <- stri_join(stri_sub(users$CreationDate,1,10),stri_sub(users$CreationDate,12,19),sep = " ")          
users$CreationDate <- stri_datetime_parse(users$CreationDate, format = "yyyy-MM-dd HH:mm:ss")

users2 <- users %>%
            group_by(CreationDateDay = stri_sub(CreationDate,1,7)) %>%
            summarise(stacked_count = n()) %>%
            filter(CreationDateDay < "2018-12")
ggplot(users2) +
  geom_bar(aes(x = CreationDateDay, y = stacked_count, fill = -stacked_count), stat = "identity") +
  labs(x = "Miesiąc", y = "Ilość zarejestrowanych użytkowników")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

### Jezyk programowania na forum

posts_ds2 <- posts_datascience %>% select(CreationDate, Tags)
posts_ds2$Python <- stri_detect_regex(posts_ds2$Tags,"<python>")
posts_ds2$R <- stri_detect_regex(posts_ds2$Tags,"<r>")
posts_ds2[is.na(posts_ds2)] <- FALSE 

posts_ds3 <- posts_ds2 %>%
              group_by(CreationMonth = stri_sub(CreationDate,1,7)) %>%
              summarise(CntPython = sum(Python),
                        CntR = sum(R)) %>% 
              filter(CreationMonth < "2018-12") 

posts_ds4 <- rbind(posts_ds3 %>% select(CreationMonth, Cnt = CntPython) %>% mutate(Language = "Python"),posts_ds3 %>% select(CreationMonth, Cnt = CntR) %>% mutate(Language = "R"))

ggplot(data = unique(posts_ds4), aes(x = CreationMonth, y = Cnt, col = Language, group = Language)) +
  geom_line(size = 2) +
  geom_point(size = 2) +
  labs(x = "Miesiąc", y = "Ilość tagów")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text=element_text(size=15),
        legend.title = element_text(size=15))

### Popularność tagów w zależnosci od czasu - jak sie zmieniala

posts_ds3 <- posts_german_stack %>% select(CreationDate, Tags)
tags_stack <- read.csv(file="C:/Users/tomas/Downloads/german/Tags.csv", header=TRUE, sep=",")
posts_ds3$Python <- stri_detect_regex(posts_ds3$Tags,"<data-science>")
posts_ds2$R <- stri_detect_regex(posts_ds2$Tags,"<r>")
posts_ds2[is.na(posts_ds2)] <- FALSE 
