for (i in 1:8){
  print(i)
}
i <-3
while (i<= 5){
  print(i)
  i <-i + 1
}
n <- 6
for (i in 1:n){
  for (j in 1:n){
    result <- i * j
    cat(result, "\t")
  }
  cat("\n")
}
for (i in 1:10) {
  if (i==5){
    cat("Breaking the loop at i =", i,"\n")
    break
  }
for (i in 1:10){
  if (i %% 2 == 0){
    next
  }
  print(i)
}
  
}