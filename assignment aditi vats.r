x<-60
if (x>55) {print("x is greater than 55")}else{print("x is not grater than 55")}
evaluate_grade <- function(grade) {
  result <- switch(grade,
                         "10" = "A",
                         "9" = "B",
                         "8" = "C",
                         "7" = "D",
                         "2" = "F",
                         "1" = "F",
                         "0" = "F",
                         "Invalid grade")
  return(result)
}
grades <- c("A", "C", "F")
for (grade in grades) {
  message(paste("Grade:", grade, "-Result:", evaluate_grade(grade)))
}

 


