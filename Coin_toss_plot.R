


coin.toss.plot <- function(sample.size = 100 , samples.n = 10000 , prob = c(0.5, 0.5)) {
  
  #Generate vector for possible outcomes
  coin <- c("heads", "tails")
  
  #Generate vector to store outcome = heads of the sample.size number of tosses
  heads <- c()
  
  #Generate samples.n number of sample.size tosses and stores the number of heads outcome
  for (i in 1:samples.n) {
    coin.toss <- sample(coin, sample.size, replace = TRUE, prob)
    heads[i] <- sum(coin.toss == "heads")
  }
 
  #Plot the number of heads outcome by frequency 
  hist(heads, breaks = 15,
       main = paste("Distribuição de ", samples.n, "amostras de ", sample.size , " jogadas de moeda"),
       xlab = "Número de vezes que caiu em 'Cara' ",
       ylab = "Número de amostras")
  
}

pnorm(800 , mean = mean(heads), sd = sd(heads), lower.tail = FALSE)


