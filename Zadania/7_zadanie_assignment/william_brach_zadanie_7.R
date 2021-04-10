#' ---
#' title: "7. Testy dobrej zhody"
#' subtitle: "Štatistické metódy vyhodnocovania experimentov"
#' author: "William Brach"
#' date: "12.04.2021"
#' output: 
#'    prettydoc::html_pretty:
#'      toc: true    
#'      theme: default
#' ---
#+  include=FALSE
# obsah tohoto bloku sa v riešení nezobrazí
knitr::opts_chunk$set(collapse = TRUE)  # zlúčenie regiónov vstupu a výstupu
`%>%` <- magrittr::`%>%`  # pipe operátor

#' 
#' # Príklad 1 (absencia)
#' 
#' **Zadanie:** 1. V tabuľke sú počty chýbajúcich žiakov v škole počas pracovných dní. Predpokladáme, že nie je dôvod aby v niektorých dňoch chýbalo viac detí. Vyberte vhodné teoretické rozdelenie, ktorého zhodu s rozdelením dát budete testovať.
#' 
#' **H0** : Nie je toto rozdelenie normálove? 

#' 
#' | deň	|pondelok	|utorok	|streda|	štvrtok|	piatok |
#' |---------------|:-------------:|:-------------:|:-------------:|:-------------:|------:|
#'  | počet |	125-k	|88+k	|85	|94	|108 |
#'  
#' **Riešenie:** 
k <- 8
x <- c(125+k, 88+k, 85, 94, 108)

#' Pre vizualizaciu dat si vykreslime histogram
hist(x)

#' Vypocitame si mean, sd aby sme dostali informacie a nasich vstupoch
xmean <- mean(x)
xsd <- sd(x)
n <- length(x)
c(xmean = xmean, xsd = xsd, n = n, range = range(x)) %>% round(2)

#' Pouzijeme Anderson-Darling test, ktory testuje ci H0: X pochadza z N(xmean,xsd).
goftest::ad.test(x, null = pnorm, mean = mean(x), sd = sd(x), estimated = F)  
#' Na zaklade Anderson-Darling testu mozeme zamietnut nasu H0. Kedze z testu nam vyslo ze rozdelenie je normalovej distribucie.

#' 
#' # Príklad 2 (domáci poriadok)
#' 
#' **Zadanie:** Bolo skúmané dodržiavanie šiestich pravidiel domáceho poriadku nájomníkmi v sociálnych bytoch pridelených mestom. Jednoduchý náhodný výber 200 bytov odhalil nasledujúce skutočnosti o počte priestupkov.
#' 
#' 
#' |počet_priestupkov	|0	|1	|2	|3	|4	|5	|6 |
#' |------|:------:|:------:|:------:|:------:|:------:|:------:|------:|
#'  |počet_bytov	|31-k	|51	|70	|32	|9+k	|5	|2 |
#'  
#'  Testujte, že vzorka pochádza z rozdelenia, v ktorom počet priestupkov (zo šiestich možných priestupkov, n=6, p=?) na 1 byt je binomicky rozdelená náhodná premenná. Porovnajte teoretické a empirické početnosti graficky.
#'  
#' **Riešenie:**

k <- 8
pocet_priestupkov <- c(0,	1,	2,	3,	4,	5,	6)
x <- c(31-k,	51,	70,	32,	9+k,	5,	2)


chisq.test(x, p=rep(1/length(x), length(x)))
#' Na zaklade vysledkov testu mozeme zamietnut hypotezu 0. Ze rozdelenie priestupkov na byt je binomicky nahodna premenna. 

#' Empiricka pocestnost
xsort <- sort(x)
Fn <- rank(xsort, ties.method="max")/length(x)
Ft <- pnorm(xsort, mean=mean(x), sd=sd(x))
plot(xsort, Fn, main="Empirická a parametrická distribučná funkcia ")
lines(xsort, Ft)  
#' Histogram
hist(x)


his = hist(x)

hranice <- his$breaks
hranice[1] <- -Inf; hranice[k+1] <- Inf
pravdep <- diff( pnorm(hranice, mean=xmean, sd=xsd) )

pocetnost <- list(
  skutocna = his$counts,
  teoreticka = pravdep*length(x)
); pocetnost
#' Pri porovnani empirickej a teoretickej pocetnosti vidime ze su opacne. Empiricka pocetnost rastie, zatial co pri histograme su hodnoty s najvacsou frekvenciou na zaciatku

#' 
#' # Príklad 3 (životnosť žiaroviek)
#' 
#' **Zadanie:** Pri sledovaní životnosti 33 ks žiaroviek do dataprojektorov boli zistené výsledky (doba činnosti v jednotke času):
#' 
#' | | | | | | | | | | | |
#' |--|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
#' |7+k	|15	|16	|17	|19	|20	|21	|21	|22	|23	|23 |
#' | 23	|23	|24	|24	|24	|24	|24	|25	|25	|26	|27|
#' |28	|29	|29	|30	|30	|31	|32	|33-k	|34	|34	|35|
#' 
#' Na hladine významnosti α=0.1 testujte tieto hypotézy, že výber je:
#'
#'A = výberom z exponenciálneho rozdelenia s očakávanou hodnotou 25. Nakreslite histogram relatívnych početností a preložte #' ním krivku hustoty-empirickú aj teoretickú.
#'
#'B = výberom z normálneho rozdelenia s neznámymi parametrami. Nakreslite histogram relatívnych početností a preložte ním krivku hustoty – empirickú aj teoretickú. Nakreslite QQ graf.
#' 
#' **Riešenie:**
#' 

#' ## A
#' 

require('fitdistrplus')
k <- 8
x <- c(7+k,	15,	16,	17,	19,	20,	21,	21,	22,	23,	23,
       23,	23,	24,	24,	24,	24,	24,	25,	25,	26,	27,
       28,	29,	29,	30,	30,	31,	32,	33-k,	34,	34,	35)
fit1 <- fitdistr(x, "exponential") 
ks.test(x, "pexp", fit1$estimate)
#' Zamietame h0, rozdelenie nieje z exponencialneho rozdelenia.


#' Histogram
#' 
#' 
his = hist(x)


#' ## B
#' 
xmean <- mean(x)
xsd <- sd(x)
n <- length(x)
ks.test((x - xmean) / xsd, pnorm)  
#' Na základe testu mozeme potvrdit ze rozdelenie pochádza z normálového rozdelenia.

#' 
#' # Príklad 4 (účinnosť obojkov)
#' 
#' **Zadanie:** Porovnali sme dĺžku účinnosti antiparazitných obojkov dvoch značiek určených pre menšie plemená psov. Merania účinnosti v dňoch sú dané tabuľkou. Na hladine významnosti *α=0.05* testujte hypotézu, že výbery sú z rovnakého rozdelenia. V prípade zamietnutia nulovej hypotézy rozhodnite, ktorú značku by ste preferovali, ak ceny obojkov oboch značiek sú rovnaké. Porovnajte aj graficky.
#' 
#' | | | | | | | | | | | |
#' |--|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
#' |obojok_A	|94	|86	|90	|92	|91	|75+k	|86	|87	|97	|91	|92	|97 |
#' | obojok_B	|84	|97	|89	|94	|81+k	|83	|90	|96	|100	|82| | |
#' 
#' **Riešenie:**
k <- 8
x <- c(94	,86	,90	,92	,91	,75+k	,86	,87	,97	,91	,92	,97) %>% sort()
y <- c(84	,97	,89	,94	,81+k	,83	,90	,96	,100	,82) %>% sort()
Fx <- ecdf(x)
Fy <- ecdf(y)
z <- seq(80, 100, 1)
plot(z, Fx(z), type = "s", lwd = 2, ylab = NA)
lines(z, Fy(z), type = "s", col = 2, lwd = 2)
ks.test(x, y, alternative = "less")
#' Pri hladine vyznamosti mozeme potvrdit ze vybery su z rovnakeho rozdelenia. 
