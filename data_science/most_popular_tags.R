library(dplyr)
library(stringi)
library(ggplot2)
options(stringsAsFactors=FALSE)

setwd("C:/Users/tomas/OneDrive/Documents/Studies/PW-IAD/PADwR/zadania_domowe/03")

# Załadowanie potrzebnych danych

posts <- read.csv(file="C:/Users/tomas/Downloads/Apple/apple.stackexchange.com/Posts.csv", header=TRUE, sep=",")
tags <- read.csv(file="C:/Users/tomas/Downloads/Apple/apple.stackexchange.com/Tags.csv", header=TRUE, sep=",")
users <- read.csv(file="C:/Users/tomas/Downloads/Apple/apple.stackexchange.com/Users_countries.csv", header=TRUE, sep=",")

