calcConfInt <- function(data, level) {
  print(mean(data) + (sd(data)*qnorm(((1-level)/2 )+ level)) / (sqrt(length(data))))
  mean(data) - (sd(data)*qnorm(((1-level)/2 )+ level)) / (sqrt(length(data)))
}