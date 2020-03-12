# Title : BareBones
#    By : bjhufstetler
#  Date : 2020-03-06

# Uses: {base, magick, shiny, glue, dplyr}

# Packages

#library("shiny")
#library("glue")
#library("dplyr")

# Modules

source("modules/hexModule.R")
source("modules/timeModule.R")
source("modules/scoreModule.R")
source("modules/welcomeModule.R")
source("modules/chitModule.R")

# Functions

source("functions/utils.R")

# Global Variables

cardsPerPlayer <- 6
playerCount <- 2
boardSize <- 21 # number of places a card can be placed
cardEdgeCount <- 6 # number of edges a card has
peaceRounds <- 4
intelligence <- 1
leftOffset <- 260
bottomOffset <- 10

relationships <- as.matrix(
  data.frame(
    placecard = rep(1:boardSize, 
                    each = cardEdgeCount),
    placeedge = rep(1:cardEdgeCount, 
                    boardSize),
    neighborcard = c(2,3,4,5,6,7,       #1
                     8,9,3,1,7,19,      #2
                     9,10,11,4,1,2,     #3
                     3,11,12,13,5,1,    #4
                     1,4,13,14,15,6,    #5
                     7,1,5,15,16,17,    #6
                     19,2,1,6,17,18,    #7
                     20,21,9,2,19,37,   #8
                     21,22,10,3,2,8,    #9
                     22,23,24,11,3,9,   #10
                     10,24,25,12,4,3,   #11
                     11,25,26,27,13,4,  #12
                     4,12,27,28,14,5,   #13
                     5,13,28,29,30,15,  #14
                     6,5,14,30,31,16,   #15
                     17,6,15,31,32,33,  #16
                     18,7,6,16,33,34,   #17
                     36,19,7,17,34,35,  #18
                     37,8,2,7,18,36,    #19
                     0, 0, 0, 0, 0, 0,  #Player Card 1
                     0, 0, 0, 0, 0, 0), #Player Card 2
    neighboredge = rep(c(4,5,6,1,2,3),
                       boardSize),
    xLoc = leftOffset + c(208.5, 208.5, 312.75, 312.75, 208.5,
                          104.25, 104.25, 208.5, 312.75, 417,
                          417, 417, 312.75, 208.5, 104.25,
                          0, 0, 0, 104.25, 
                          -120, -120),
    yLoc = bottomOffset + c(240, 360, 300, 180, 120,
                            180, 300, 480, 420, 360,
                            240, 120, 60, 0, 60,
                            120, 240, 360, 420, 
                            300, 180),
    axLoc = leftOffset + c(208.5, 208.5, 312.75, 312.75, 208.5,
                           104.25, 104.25, 208.5, 312.75, 417,
                           417, 417, 312.75, 208.5, 104.25,
                           0, 0, 0, 104.25, 
                           -120, -120),
    ayLoc = bottomOffset + c(240, 360, 300, 180, 120,
                             180, 300, 480, 420, 360,
                             240, 120, 60, 0, 60,
                             120, 240, 360, 420, 
                             300, 180)
    )
  )


# distribution stores the lower and upper edge number bounds and the level distros
distribution <- data.frame(
  level = 1:10,
  pdf = c(.13,.12,.12,.12,.11,.10,.09,.08,.07,.06),
  lb1 = c(1,1,1,1,1,1,1,1,1,5),
  ub1 = c(3,3,3,3,3,3,3,3,3,6),
  lb2 = c(2,2,2,3,3,3,4,4,5,6),
  ub2 = c(4,4,4,5,5,5,6,6,7,7),
  lb3 = c(4,4,5,5,6,6,7,7,8,8),
  ub3 = c(5,6,6,7,7,8,8,9,9,9)
  )
distribution <- as.matrix(
  dplyr::mutate(distribution, cdf = cumsum(pdf))
)

#shareurl <- "https://twitter.com/intent/tweet?text=Play%20Bare%20Bones%20today%20here:&url=https://bjhufstetler.shinyapps.io/BareBones"