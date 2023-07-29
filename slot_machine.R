# Project Three : Slut Machine
# play <- function() {
#   
#   # step 1: generate symbols
#   symbols <- get_symbols()
#   
#   # step 2: display the symbols
#   print(symbols)
#   
#   # step 3: score the symbols
#   score(symbols)
# }




get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE,
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}



# if ( # Case 1: all the same <1>) {
#   prize <- # look up the prize <3>
#   } else if ( # Case 2: all bars <2> ) {
#     prize <- # assign $5 <4>
#     } else {
#       # count cherries <5>
#       prize <- # calculate a prize <7>
#     }

# count diamonds <6>
# double the prize if necessary <8>

# if (same) {
#   symbol <- symbols[1]
#   if (symbol == "DD") {
#     prize <- 800
#   } else if (symbol == "7") {
#     prize <- 80
#   } else if (symbol == "BBB") {
#     prize <- 40
#   } else if (symbol == "BB") {
#     prize <- 5
#   } else if (symbol == "B") {
#     prize <- 10
#   } else if (symbol == "C") {
#     prize <- 10
#   } else if (symbol == "0") {
#     prize <- 0
#   }
# }
score <- function(symbols){
# identify case
same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
bars <- symbols %in% c("B","BB","BBB")
payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
             "B" = 10, "C" = 10, "0" = 0)

# get prize
if (same) {
  prize <- unname(payouts[symbols[1]])
} else if (all(bars) ) {
  prize <- 5
  } else {
    cherries <- sum(symbols=="C")
    prize <- c(0,2,5)[cherries + 1]
  }

# adjust for diamonds
diamonds <- sum(symbols=="D")
prize * 2 ^ diamonds

}

# play <- function() {
#   symbols <- get_symbols()
#   prize <- score(symbols)
#   attr(prize,"symbols") <- symbols
#   prize
# }

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols),symbols=symbols,class="slots")
  
}

slot_display <- function(prize){
  
  # extract symbols
  symbols <- attr(prize, "symbols")
  
  # collapse symbols into single string
  symbols <- paste(symbols, collapse = " ")
  
  # combine symbol with prize as a character string
  # \n is special escape sequence for a new line (i.e. return or enter)
  string <- paste(symbols, prize, sep = "\n$")
  
  # display character string in console without quotes
  cat(string)
}
print.slots <- function(x, ...){
  slot_display(x)
}


wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, 
          "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)
combos <- expand.grid(wheel,wheel,wheel,stringsAsFactors = FALSE)
prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06,
"BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
head(combos)
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3
head(combos)
sum(combos$prob)
symbols <- c(combos[1, 1], combos[1, 2], combos[1, 3])
score(symbols)
combos$prize <- NA
for (i in 1:nrow(combos)) {
  symbols <- c(combos[i,1],combos[i,2],combos[i,3])
  combos$prize[i] <- score(symbols)
}



# To account for the fact that diamonds are wild cards, we could adjust score
score <- function(symbols) {
  
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  
  # identify case
  # since diamonds are wild, only nondiamonds 
  # matter for three of a kind and all bars
  slots <- symbols[symbols != "DD"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B", "BB", "BBB")
  
  # assign prize
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slots[1]])
  } else if (all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    # diamonds count as cherries
    # so long as there is one real cherry
    prize <- c(0, 2, 5)[cherries + diamonds + 1]
  } else {
    prize <- 0
  }
  
  # double for each diamond
  prize * 2^diamonds
}
sum(combos$prize * combos$prob)
# 0.934356

# For loop, While loop & Repeat Loop

# While loop example
plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  while (cash > 0) {
    cash <- cash - 1 + play()
    n <- n + 1
  }
  n
}


# Repeat Loops example
plays_till_broke <- function(start_with) {
  cash <- start_with
  n <- 0
  repeat {
    cash <- cash - 1 + play()
    n <- n + 1
    if (cash <= 0) {
      break
    }
  }
  n
}
