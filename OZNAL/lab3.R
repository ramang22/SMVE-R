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
#' # Uloha 1 Hod mincou
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
#' # Uloha 2.	„Priemer“ kladných hodnôt
#' 
#' **Zadanie:** 
#' 
#' **Riešenie:**




#' 
#' # Uloha 3.	
#' 
#' **Zadanie:** Zopakujte úlohu 2 pre vektor náhodne generovaných reálnych čísel z intervalu (0,1) .
#'Kedy a v ktorých prípadoch budete pozorovať výraznú zmenu oproti predchádzajúcemu
#'prípadu (spotreba pamäte, čas potrebný na výpočet...) Ako overíte, či sa sa na vypočítaný výsledok môžete spoľahnúť?
#' 
#' **Riešenie:**


#' 
#' # Uloha 4.	RMSD – root mean square deviation s podmienkou
#' 
#' **Zadanie:** Opäť vytvorte vektor náhodne generovaných reálnych čísel z intervalu (−1,1) s normálnym rozdelením – opäťpostupne s dĺžkou 100 – 1 000 000 čísel. Naučte sa
#'v tomto príklade používať funkcionálnu podmienku! To znamená, že vymyslíte aspoň 5 spôsobom (jeden cyklus, aspoň dva rôzne funkcionálne prístupy) ako z vami vygenerovanej dátovej množiny vyberiete len kladné čísla (len záporné čísla, len čísla
#'z nejakého intervalu ...) a pre tento výber vypočítate aká bude RMSD
#'Pre každý prístup (každý naprogramovaný spôsob) a každú dĺžku vektora testujte aj #'pamätové aj časové nároky a overte hraničné limity použitia jednotlivých programátorských štýlov.
#' 
#' **Riešenie:**


#' 
#' # Uloha 5.	. Trochu genomiky - (práca s typom char)
#' 
#' **Zadanie:** Stiahnite si z drivu kompletnú genetickú informáciu pre covid 19 (NCBI Reference Sequence: NC_045512.2)
#' a urobte jeho analýzu pre jednoduché základné úlohy. Pre každú úlohu (ako obvykle) aspoň 5 rôznych spôsobov ako sa to dá naprogramovať a minimálne dva (tri?) funkcionálne
#' 
#' **Riešenie:**
library(seqinr)
setwd("C:/Users/ramang/Developer/SMVE-R/OZNAL/")
getwd()
dat <- read.fasta(file = "sequence_covid")

