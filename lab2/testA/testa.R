res <- rep(0, 10)
time <- rep(0, 10)
num <- seq(1, 10, by = 1)

# 1
t0 <- Sys.time()
xA <- seq(100, 200, by=5)
sumxA <- sum(xA)
res[1] <- sumxA
t1 <- Sys.time()
time[1] <- t1 - t0

print(res[1])

# 2
t0 <- Sys.time()

col<-length(xA)
mn<-mean(xA)

res[2] <- col
res[3] <- mn

t1 <- Sys.time()

time[2] <- t1 - t0

#3 

t0 <- Sys.time()

xNorm <- rnorm(length(col) + 7, mean = 5)
sdxNorm <- round(sd(xNorm))

res[4] <- sdxNorm

t1 <- Sys.time()

time[3] <- t1 - t0

#4 

t0 <- Sys.time()

arr <- array(xA, dim = c(5, round(col / 5)))
sumArrSin <- round(sum(sin(arr)), 4)

res[5] <- sumArrSin

print(sumArrSin)

t1 <- Sys.time()

time[4] <- t1 - t0

#5
t0 <- Sys.time()

matr <- matrix(xA[1:(round(col / 5) * 5)], nrow = 5)
matr <- matr[-c(2, 5),]

res[6] <- ncol(matr) + nrow(matr)


t1 <- Sys.time()

time[5] <- t1 - t0

#6

t0 <- Sys.time()

lst <- list(rep(c(TRUE, FALSE), each = 5), rep(c(TRUE, FALSE), each = 5), rep(c(TRUE, FALSE), each = 5))
resLogSum <- Reduce('|', Reduce('|', lst, FALSE), FALSE)
res[7] <- resLogSum

t1 <- Sys.time()

time[6] <- t1 - t0

#7

t0 <- Sys.time()

isIdent<-identical(arr,matr)

res[8] <- isIdent

t1 <- Sys.time()

time[7] <- t1 - t0


#8

t0 <- Sys.time()

newArray = array(matr, dim(matr))
isIdent <- identical(newArray, matr)
res[9] <- isIdent

t1 <- Sys.time()

time[8] <- t1 - t0

#9

t0 <- Sys.time()

df <- data.frame(num, res, time)

t1 <- Sys.time()

time[9] <- t1 - t0

total_time <- sum(time)

time[10] <- total_time

res[10] <- total_time
print(res)
print(time)
print(df)




