#' ---
#' title: "Lab 3"
#' subtitle: "Objavovanie znalostí"
#' author: "William Brach"
#' date: "03.03.2021"
#' output: 
#'    prettydoc::html_pretty:
#'      toc: true    
#'      theme: default
#' ---
#+  include=FALSE
# obsah tohoto bloku sa v riešení nezobrazí
knitr::opts_chunk$set(collapse = TRUE)  # zlúčenie regiónov vstupu a výstupu
`%>%` <- magrittr::`%>%`  # pipe operátor
library(tictoc)
#' 
#' # Uhloha 1 Hod mincou
#' 
#' **Zadanie:** 1. Budeme hádzať mincou pomocou príkazu rbinom().Testujte postupne situáciu pri 10 hodoch, pri 100, 1000, 10 000, 100 000 hodoch, aká je pravdepodobnosť, že hodíte „hlavu“.
#'Fyzické sčítanie pozitívnych výsledkov realizujte minimálne 5-timi rôznymi spôsobmi (aspoň jeden cyklus, aspoň dva spôsoby cez funkčné programovanie).  
#' Sledujte aká je časová a pamätová náročnosť pri každom spôsobe, ktorý ste naprogramovali je potrebná. Sledujte ako sa jednotlivé programátorské štýly správajú v tomto a aj v nasledujúcich algortimoch. Sledujte, kde sú ich slabé a silné stránky.

#' 
#' **Riešenie:** 


for (len in c(100, 1000, 10000, 100000)){
  tosses <-rbinom(len, 1, 0.5)
  # 1. 
  tic()
  prob1 <- sum(tosses)/len
  toc(log=T,quiet=T)
  time1 <- tic.log()
  tic.clearlog()
  # 2.
  tic()
  heads = 0
  for (x in tosses){
    if (x == 1){
      heads = heads + 1
    }
  }
  prob2 <- heads/len
  toc(log=T,quiet=T)
  time2 <- tic.log()
  tic.clearlog()
  
  
  # 3.
  tic()
  prob3 <- length(which(tosses == 1))/len
  toc(log=T,quiet=T)
  time3 <- tic.log()
  tic.clearlog()
  # 4. 
  tic()
  prob4 <- Reduce(
    "+",
    tosses
  )/len
  toc(log=T,quiet=T)
  time4 <- tic.log()
  tic.clearlog()
  #' 5.
  tic()
  getCoinTossProb <- function(toss, len){
    return (length(
      Filter( 
        function(x) x==1,
        toss))
      /len) 
  } 
  prob5 <- getCoinTossProb(tosses, len)
  toc(log=T,quiet=T)
  time5 <- tic.log()
  tic.clearlog()
  print(paste("For length :",len))
  print(paste("Prob of case 1 : ",prob1, "Time for case 1 :",time1))
  print(paste("Prob of case 2 : ",prob2, "Time for case 2 :",time2))
  print(paste("Prob of case 3 : ",prob3, "Time for case 3 :",time3))
  print(paste("Prob of case 4 : ",prob4, "Time for case 4 :",time4))
  print(paste("Prob of case 5 : ",prob5, "Time for case 5 :",time5))
}





#' 
#' # Príklad 2 (sinus)
#' 
#' **Zadanie:** Zobrazte sin(x) v intervale $x\in(-\pi,\pi)$
#' 
#' **Riešenie:**
seq(from = -pi, to = pi, length.out = 100) %>% 
  plot(sin(.), type = "l", col = "red")
