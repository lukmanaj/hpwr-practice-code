# Project One: Weighted Dice
library(ggplot2)
qplot
roll <- function() {
  die <- 1:6
  dice <- sample(die,size = 2,replace = TRUE,prob = c(1/8,1/8,1/8,1/8,1/8,3/8))
  sum(dice)
}

replicate(10,roll())
rolls <- replicate(10000,roll())
qplot(rolls,binwidth = 1)


x <- c(1, 2, 2, 2, 3, 3)
qplot(x,binwidth=1)
x3 <- c(0, 1, 1, 2, 2, 2, 3, 3, 4)
qplot(x3,binwidth=1)
replicate(10,roll())
rolls <- replicate(10000,roll())
qplot(rolls,binwidth = 1)
roll <- function() {
  die <- 1:6
  dice <- sample(die,size = 2,replace = TRUE,prob = c(1/8,1/8,1/8,1/8,1/8,3/8))
  sum(dice)
  }
 
replicate(10,roll())
rolls <- replicate(10000,roll())
qplot(rolls,binwidth = 1)
is.vector(die)
length(dice)
length(die)
typeof(die)
typeof(dice)


# Project Two: Playing Cards

int <- c(-1L,2L,3L)
typeof(int)
text <- c('Hello','World')
typeof(text)
logic <- c(TRUE,T,F)
typeof(logic)
raw(3)
typeof(raw(3))
hand <- c("ace","king","queen","jack","ten")
typeof(hand)
names(die) <- c("one","two","three","four","five","six")
names(die) <- c("un","deux","trois","quatre","cinq","six")
dim(die) <- c(3,2)
dim(die) <- c(1,2,3)
m <- matrix(die,nrow = 2)
m <- matrix(die,nrow = 2,byrow = TRUE)
flush <- c("ace","king","queen","jack","ten",rep("spades",5))
dim(flush) <- c(5,2)
flush
typeof(flush)
class(flush)
class(die)
now <- Sys.time()

typeof(now)
class(now)
unclass(now)
mil <- 1000000
class(mil) <- c("POSIXct", "POSIXt")
mil
gender <- c("male","female","male","female")
typeof(gender)
gender <- factor(gender)
typeof(gender)
attributes(gender)
unclass(gender)
as.character(gender)
card <- list("ace","hearts",1)
card


df <- data.frame(face = c("ace", "two", "six"),
            suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3))
df
typeof(df)
class(df)
str(df)

library(readr)
deck <- read_csv("~/deck.csv")
View(deck)
head(deck)
write.csv(deck,"cards.csv",row.names = FALSE)


# Complete the following code to make a function that returns the first row of a data
# frame:
deal <- function(cards) {
   cards[1,]
}

shuffle <- function(cards) {
  random <- sample(1:52,size = 52,replace = TRUE)
  cards[random,]
}

deck2 <- deck

show_env <- function(){
  list(ran.in = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}
show_env <- function(){
  a <- 1
  b <- 2
  c <- 3
  list(ran.in = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment()))
}

deal <- function() {
  card <- deck[1, ]
  assign("deck", deck[-1, ], envir = globalenv())
  card
}

shuffle <- function(){
  random <- sample(1:52, size = 52)
  assign("deck", DECK[random, ], envir = globalenv())
}


setup <- function(deck) {
  DECK <- deck
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = parent.env(environment()))
    card
  }
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = parent.env(environment()))
  }
  list(deal = DEAL, shuffle = SHUFFLE)
}
cards <- setup(deck)
deal <- cards$deal
shuffle <- cards$shuffle

