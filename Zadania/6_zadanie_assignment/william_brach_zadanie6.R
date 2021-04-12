#' ---
#' title: "6. Neparametrické testy"
#' subtitle: "Štatistické metódy vyhodnocovania experimentov"
#' author: "William Brach"
#' date: "05.04.2021"
#' output: 
#'    prettydoc::html_pretty:
#'      toc: true    
#'      theme: default
#' ---
#+  include=FALSE
# obsah tohoto bloku sa v riešení nezobrazí
knitr::opts_chunk$set(collapse = TRUE)  # zlúčenie regiónov vstupu a výstupu
`%>%` <- magrittr::`%>%`  # pipe operátor
library("readxl")
library(magrittr)

k = 9
m = k%%3 + 1
n = k%%5 + 5

#' 
#' # Príklad 1 (plechovice farby)
#' 
#' **Zadanie:** Kvalitári v závode na výrobu farieb v stanovených intervaloch kontrolne z pásového dopravníka odchytili a premerali váhu 16 plechových nádob, ktoré vyšli z novej plniacej linky.
#' 
#' **Riešenie:** H0 vybraté hodnoty budú od seba štatisticky odlišné čím môžeme dokázať že nastáva systémový problém s výrobnou linkou. 

x <- c(68.2, 71.6, 69.3, 71.6, 70.4, 65.0, 63.6, 64.7, 65.3, 64.2, 67.6, 68.6, 66.8, 70.1)

randtests::runs.test(x, plot = T)

#' Test funguje tak ze prebehne vstupne hodnoty a porovna ich s nejakym kriteriom a nalsedne rozdeli vstupne hodnoty podla tohoto kriteria. V nasom pripade je kriterium median.
#' 
#' https://support.minitab.com/en-us/minitab-express/1/help-and-how-to/basic-statistics/inference/how-to/one-sample/runs-test/methods-and-formulas/methods-and-formulas/
#' 
randtests::turning.point.test(x)


#' Vybratá sekvencia nieje náhodná. Pričom počeť skupín nad aj pod mediánopm nieje dostatočne veľký a test kritických bodov nám tiež zamietol náhodnosť výberu. Na základe týchto zistení môžeme tvrdiť že na linke nedochádza k žiadnym systematickým poruchám a linka funguje správne. Pri hladine významnosti 5%.
#' 

med = median(x)
up = x[which(x > med)]
down = x[which(x < med)]

length(up)
length(down)

#' Vypocitali sme si median a potom pomocou neho sme rozdelili vstupne hodnoty na hodnoty ktore su viac a menej ako median. Nasledne som porovnal velkosti jednotlivych skupin. Pre tento priklad boli skupine rovnake. Obi dve maju velkost 7. Toto nam takiez potvrdil runs.test.
#' 

#' 
#' #  Príklad 9 (poistenie)
#' 
#' **Zadanie:** V snahe prešetriť ceny havarijných poistiek úrad pre ochranu spotrebiteľa náhodne vybral niekoľko vodičov poistených v jednej z troch hlavných poisťovní. Predpokladajme, že títo vodiči majú podobné autá, záznamy a poistné pokrytie. Na základe ročných nákladov na poistenie rozhodnite, či sa ceny líšia, a prípadne medzi ktorými poisťovňami.
#' 
#' **Riešenie:** H0 - Všetky poisťovne budú rovnaké a nebudú sa od seba významne líšiť. 
#' 
#' 

p1 <- c(396, 438, 336, 318)
p2 <- c(348, 360, 522)
p3 <- c(378, 330, 294, 474, 432)

dat <- stack(list(A=p1, B=p2, C=p3))
names(dat) <- c("naklad", "poistovna")

plot(dat$poistovna, dat$naklad, main="Rozdelenie hodnot poisťovní",
     xlab="Poistovne", ylab="Cena rocnych nakladov", pch=19)

#' Pripravili sme si data z poistovni. Pouzijeme Kruskal-Walisov test lebo mame velmi malo testovacich vzoriek.

kruskal.test(naklad ~ poistovna, dat)

#' Na hladine významnosti 5% môžeme zamietnuť že poistovne sú podobné. Líšia sa.
#' Overíme sa ktoré druhy sa líšia.
#' 
#' Kruskal.test sa sklada z viacerych krokov. Najskor si utriedime data do skupin od najmensej po najvacsiu. Potom jednotlivym skupinam pridame numericku hodnotu. Pre kazdu nami hladanu skupinu. V tomto pripade poistovne spocitame hodnotu ktoru sme pridali jednotlivym skupinam a zapadaju pod konkretnu poistovnu. Vypocitame H statistiku podla vzorca. Vypocitame kriticku chi-squred hodnotu. Nakoniec porovname H hodnotu s chi-squared hodnotou. 
#' Ked je chi-squared hodnota mensia ako H hodnota tak mozeme zamietnut H0 ale ked chi-squre hodnota nieje mensia tak nemame dostatok dokazov ze jednotlive hodnoty niesu rozdielne.
#' 
#' https://www.statisticshowto.com/kruskal-wallis/

tapply(dat$naklad, INDEX=dat$poistovna, FUN=median)

#' Vyzerá to tak že 3 poistovňa, poistovňa C sa bude líšiť od poistovne A a B. Overíme to pomocou Dunnov testu.

dunn.test::dunn.test(dat$naklad, g = dat$poistovna, method = "bonferroni")

#' Dunnov test nám potvrdil že sa dané poistovne líšia kde p-value je 0.81 a môžeme zamietnuť H0.