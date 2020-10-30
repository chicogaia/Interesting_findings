
coin.plot <- function(sample.size = 100 , samples.n = 10000 , prob = c(0.5, 0.5)) {
        
        #Generate vector for possible outcomes
        coin <- c("heads", "tails")
        
        #Generate vector to store outcome = heads of the sample.size number of tosses
        heads <- c()
        
        #Generate samples.n number of sample.size tosses and stores the number of heads outcome
        for (i in 1:samples.n) {
                coin.toss <- sample(coin, sample.size, replace = TRUE, prob)
                heads[i] <- sum(coin.toss == "heads")
        }
        
        #Generate .jpg file
        setwd("C:/Users/Francisco Gaia/Documents/interesting_findings/LinkedIn Articles/Coin Toss pvalue")
        jpeg("cointoss.jpeg", height = 600 , width = 800)
        
        
        #Plot the number of heads outcome by frequency 
        p.fair <- hist(heads, breaks = 15, col=rgb(0,0,1,1/3),
                       xlim = c(20,80), ylim = c(0,2000) ,
                       main = paste("Distribuição de ", samples.n, "amostras de ", sample.size , " jogadas de moeda"),
                       xlab = "Número de vezes que caiu em 'Cara' ",
                       ylab = "Número de amostras")
        text(x = 70 , y = 1750, "Simulação em R por Francisco Gaia")
        
        dev.off()
}   


coin.plot2 <- function(sample.size = 100 , samples.n = 10000 , prob_unfair = c(0.8, 0.2)) {
  
  #Generate vector for possible outcomes
  coin <- c("heads", "tails")
  
  
  #FAIR
  
  #Generate vector to store outcome = heads of the sample.size number of tosses
  heads.fair <- c()
  
  #Generate samples.n number of sample.size tosses and stores the number of heads outcome
  for (i in 1:samples.n) {
    coin.toss <- sample(coin, sample.size, replace = TRUE, c(0.5,0.5))
    heads.fair[i] <- sum(coin.toss == "heads")
  }
  
  
  #UNFAIR
  
  #Generate vector to store outcome = heads of the sample.size number of tosses
  heads.unfair <- c()
  
  #Generate samples.n number of sample.size tosses and stores the number of heads outcome
  for (i in 1:samples.n) {
    coin.toss <- sample(coin, sample.size, replace = TRUE, prob_unfair)
    heads.unfair[i] <- sum(coin.toss == "heads")
  }
  
  #Generate .jpg file
  setwd("C:/Users/Francisco Gaia/Documents/interesting_findings/LinkedIn Articles/Coin Toss pvalue")
  jpeg("cointoss2.jpeg", height = 600 , width = 800)

 
  #Plot the number of heads outcome by frequency 
  p.fair <- hist(heads.fair, breaks = 15)
  p.unfair <- hist(heads.unfair, breaks = 15)
  plot(p.fair, col=rgb(0,0,1,1/3), xlim = c(20,100), ylim = c(0,2000), 
       main = paste("Distribuição de ", samples.n, "amostras de ", sample.size , " jogadas de moeda"),
  xlab = "Número de vezes que caiu em 'Cara' ",
  ylab = "Número de amostras")
  plot(p.unfair, col=rgb(1,0,0,1/3), xlim = c(20,100), ylim = c(0,2000), add = T)
  legend("topleft" ,
         col = c(rgb(0,0,1,1/3) , rgb(1,0,0,1/3)),
         legend = c("Moeda Justa", paste("Moeda Injusta | p=" , prob_unfair[1])),
         pch = 15,
         bty = "n",
         cex = 1.25,
         pt.cex = 2.5)
  
  dev.off()
       
}   




