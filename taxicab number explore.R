
pow <- 5
w <- 232
x <- 145
y <- 222
z <- 199

w <- 101
x <- 102
y <- 26
z <- 103

w <- 1029
x <- 1020
y <- 1030
z <- 1019


w^pow
x^pow 
y^pow
z^pow

A <- w^pow+x^pow 
B <- y^pow+z^pow


A
B
A-B


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://stackoverflow.com/questions/11410798/finding-taxicab-numbers
# Create an hash table of cubes from 1 to 100

numbers <- 1:100
cubes <- numbers ^3

# The possible pairs of numbers
pairs <- combn(numbers, 2)
dim(pairs)

pairs[,4900:4950]
pairs[1:2,99:101]

# Now sum the cubes of the combinations
# This takes every couple and sums the values of the cubes
# with the appropriate index 
sums <- apply(pairs, 2, function(x){sum(cubes[x])})
sums[4900:4950]

doubles <- which(table(sums) == 2)

as.integer(names(doubles))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~trying power 5

numberz <-110
z <-numberz-100+1
numbers <- (z):numberz
cubes <- numbers ^5

# The possible pairs of numbers
pairs <- combn(numbers, 2)
dim(pairs)

pairs[,4900:4950]
pairs[1:2,99:101]

# Now sum the cubes of the combinations
# This takes every couple and sums the values of the cubes
# with the appropriate index 
sums <- apply(pairs, 2, function(x){sum(cubes[x])})
sums[1:4950]

doubles <- which(table(sums) == 2)

as.integer(names(doubles))




