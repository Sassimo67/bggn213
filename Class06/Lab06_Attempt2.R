gradebook <- read.csv("eGrade-Student-BGGN213_W22.csv")
totalpoints <- c(rowSums(gradebook, na.rm=TRUE))
totalpossiblepoints <- 94
grade <- function(a=totalpoints, b=totalpossiblepoints) {
  result <- round((a/b)*100, digits = 1)
  print(result)
}
grade()

#Q2 Who was the top scoring student?
which.max(totalpoints)

#Q3 Which homework was toughest on students?
assignmentsums <- colSums(gradebook[,-1], na.rm = TRUE)
assignmentsums
which.min(assignmentsums)