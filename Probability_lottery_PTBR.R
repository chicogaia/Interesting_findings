#The function below will plot a chart for which
#the x-axis shows the duration of a life-time, in years
#the y-axis shows the probability of winning the lottery at least once in a life-time
#the intercept between the two dashed lines show the probability of winning the lottery at least once for that
#amount of years of gambling

#Establish the max life.time you will want to plot
max.life <- 100

#Establish for how many years you will gamble
gamble.life <- 100-18

#Establish the probability of winning that specific lottery (ex. below: Mega Sena)
prob.lottery <- 1/choose(60,6)

#Establish the weekly frequency of plays (model assumes playing every week through a life-time)
week.freq <- 2

#Function that calculates and plots the probability of winning the specified lottery at least once in a 
#life-time, for each life-time periods until the max defined in "life" argument
win.lotto <- function(gamble.life , max.life = 100, week.freq = 2 , prob.lottery = 1/choose(60,6)) {
        
        #Generate vector with all life-times to be plotted  
        life.time <- c(1:max.life)
        
        #Generate empty vector "p" inside which the probabilities of winning at least once will be stored
        #for each life-time period of gambling
        p <- c()
        
        #Fill the vector "p" with the calculations
        for (i in 1:max.life) {
        p <- rbind(p , sum(dbinom(1:(i*week.freq*52), i*week.freq*52, prob.lottery)))
        }
        
        #Generate df with 2 columns: life-time periods and probability of winning the specified lottery
        #at least once in each life-time period
        probwin.data <- data.frame(life.time , p)
        
        #Load plot library
        library(ggplot2)
        
        #Plot the data
        ggplot(probwin.data, aes(x = life.time, y = p*100)) +
        geom_line() +
        labs(
          title = "A Realidade que vivemos é uma função de Probabilidades",
             x = "Duração total de uma vida (anos) | escala log",
             y = "Probabilidade de ganhar na loteria pelo menos uma vez (%)",
             caption = paste(
               "Você tem",
               round(p[gamble.life]*100,2) ,
               "% chance de ganhar na loteria pelo menos uma vez na vida.  |  Simulação em R por Francisco Gaia")
          ) +
        geom_vline(xintercept = gamble.life, linetype = "dashed") +
        geom_hline(yintercept = p[gamble.life]*100, linetype = "dashed") +
        coord_cartesian(ylim = c(0,max(probwin.data$p*100))) +
        scale_x_log10()
}

#The idea is that the reality we experience as human beings is a function of probability of events occuring
#Live a regular <100 years life, and you will probably never win the lottery
#Live enough (>1M years), and you will most likely win the lottery at least once in that life-time

win.lotto.big <- function(gamble.life = 10001 , max.life = 1000000, week.freq = 2 , prob.lottery = 1/choose(60,6)) {
        by <- 100
        life.time <- seq(from = 1 , to = max.life , by = by)
        p <- c()
        for (i in life.time) {
                p <- rbind(p , sum(dbinom(1:(i*week.freq*52), i*week.freq*52, prob.lottery)))
        }
        probwin.data <- data.frame(life.time , p)

        library(ggplot2)

        ggplot(probwin.data, aes(x = life.time, y = p*100)) +
                geom_line() +
                labs(
                        title = "A Realidade que vivemos é uma função de Probabilidades",
                        x = "Duração total de uma vida (anos) | escala log",
                        y = "Probabilidade de ganhar na loteria pelo menos uma vez (%)",
                        caption = paste(
                                "Você tem",
                                round(p[(gamble.life-1)/by+1]*100,2) ,
                                "% chance de ganhar na loteria pelo menos uma vez na vida, vivendo",
                                gamble.life-1,
                                "anos. |  Simulação em R por Francisco Gaia")
                ) +
                geom_vline(xintercept = gamble.life, linetype = "dashed") +
                geom_hline(yintercept = p[(gamble.life-1)/by+1]*100, linetype = "dashed") +
                coord_cartesian(ylim = c(0,max(probwin.data$p*100))) +
                coord_cartesian(xlim = c(1,max.life+by)) +
                scale_x_log10()
}

#JÁ TÁ PRONTO - SÓ BOTA PRA RODAR ANTES DE DORMIR!


