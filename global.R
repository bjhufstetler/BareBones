# Title : BareBones
#    By : bjhufstetler
#  Date : 2020-03-06

# Uses: {base, magick, shiny, glue, dplyr}

# Packages

library("shiny")
library("glue")
library("dplyr")

# Modules

source("modules/hexModule.R")
source("modules/timeModule.R")
source("modules/welcomeModule.R")

# Functions

source("functions/utils.R")

# Global Variables

n_hex <- 10

boardSize <- 37 # number of places a card can be placed
cardEdgeCount <- 6 # number of edges a card has
leftOffset <- 200
bottomOffset <- 10

relationships <- base::as.matrix(
  base::data.frame(
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
                     0,0,21,8,37,0,     #20
                     0,0,22,9,8,20,     #21
                     0,0,23,10,9,21,    #22
                     0,0,0,24,10,22,    #23
                     23,0,0,25,11,10,   #24
                     24,0,0,26,12,11,   #25
                     25,0,0,0,27,12,    #26
                     12,26,0,0,28,13,   #27
                     13,27,0,0,29,14,   #28
                     14,28,0,0,0,30,    #29
                     15,14,29,0,0,31,   #30
                     16,15,30,0,0,32,   #31
                     33,16,31,0,0,0,    #32
                     34,17,16,32,0,0,   #33
                     35,18,17,33,0,0,   #34
                     0,36,18,34,0,0,    #35
                     0,37,19,18,35,0,   #36
                     0,20,8,19,36,0),   #37
    neighboredge = rep(c(4,5,6,1,2,3),
                       boardSize),
    xLoc = leftOffset + c(208.5, 208.5, 312.75, 312.75, 208.5,
                          104.25, 104.25, 208.5, 312.75, 417,
                          417, 417, 312.75, 208.5, 104.25,
                          0, 0, 0, 104.25, rep(NA, 18)),
    yLoc = bottomOffset + c(240, 360, 300, 180, 120,
                            180, 300, 480, 420, 360,
                            240, 120, 60, 0, 60,
                            120, 240, 360, 420, rep(NA, 18))
    )
  )


# distribution stores the lower and upper edge number bounds and the level distros
distribution <- base::data.frame(
  level = 1:10,
  pdf = c(.13,.12,.12,.12,.11,.10,.09,.08,.07,.06),
  lb1 = c(1,1,1,1,1,1,1,1,1,5),
  ub1 = c(3,3,3,3,3,3,3,3,3,6),
  lb2 = c(2,2,2,3,3,3,4,4,5,6),
  ub2 = c(4,4,4,5,5,5,6,6,7,7),
  lb3 = c(4,4,5,5,6,6,7,7,8,8),
  ub3 = c(5,6,6,7,7,8,8,9,9,9)
  )
distribution <- base::as.matrix(
  dplyr::mutate(distribution, cdf = cumsum(pdf))
)

#shareurl <- "https://twitter.com/intent/tweet?text=Play%20Bare%20Bones%20today%20here:&url=https://bjhufstetler.shinyapps.io/BareBones"