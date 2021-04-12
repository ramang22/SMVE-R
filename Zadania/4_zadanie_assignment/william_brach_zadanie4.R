#' ---
#' title: "4. Jednovýberové parametrické testy"
#' subtitle: "Štatistické metódy vyhodnocovania experimentov"
#' author: "William Brach"
#' date: "22.03.2021"
#' output: 
#'    prettydoc::html_pretty:
#'      toc: true    
#'      theme: default
#' ---
#+  include=FALSE
# obsah tohoto bloku sa v riešení nezobrazí
knitr::opts_chunk$set(collapse = TRUE)  
`%>%` <- magrittr::`%>%` 
library("readxl")
library(magrittr)
library(ggplot2)
#'  
#' # Setup
#' 

#' Nevedel som, ktoré poradové číslo. Tak som zobral poradové číslo na predmete.
k <- 9 %% 4

model <- read_excel("C://Users//ramang//Developer//SMVE-R//Zadania//4_zadanie_assignment//modely.xlsx")
data <-  read_excel("C://Users//ramang//Developer//SMVE-R//Zadania//4_zadanie_assignment//baterky.xlsx")

#'  
#' # Príklad 1 (bezpečnosť modelu)
#' 
#' **Zadanie:** Graficky zistite aké je rozdelenie vyšetrovaných dát. Vhodným parametrickým testom na hladine významnosti α=0.05 overte, že model je bezpečný. Napíšte nulovú a alternatívnu hypotézu a slovne zhodnoťte výsledky šetrenia (Je príslušný model bezpečný?).
#' 
#' **Riešenie:** 
#'

#' ## Grafike zobrazenie distribucie 
#' Podľa grafu možeme tvrdiť že distribúcia dát je normálnová. Keby sme mali viac dát normálové rozdelenie by bolo krajšie znázornené.
#' 
ggplot(model, aes(x = MCLoAI)) +
  geom_histogram(
    aes(y = ..density..),
    binwidth = .2,
    colour = "black",
    fill = "white"
  ) +
  geom_density(alpha = .2, fill = "#FF6666") 
#'
#' ## Nulová hypotéza
#' Vypočítaná hodnota úspešnosti bude >= ako 1. Čím bude zabezpečené že test je bezpečný. Pri hladine významonosti 0.05. 
#' 
#'
#' ## Alternatívna hypotéza
#' Vypočítaná hodnota úspešnosti bude menšia ako 1. Vďaka čomu môžeme prehlásiť daný test za nebezpečný. Pri hladine významnosti 0.05.
#' 

#' ## Test pre k=1 a α=0.05
#' 
#' Zvolil som test podla mediánu lebo mi test podľa podielu nefungoval korektne. 
#' Najskôr som si vypočítal hodnotu presnosti pre všetky merania a pomocou nich som vykonal wilcox test. Vykonal som test pre väčšie ako 1 a zároveň aj rovné 1 lebo daná hodnota v intervale musí byť >= 1 aby bol test bezpečný. Použil som hodnotu presnosti merania lebo je smerodajné pre moje hypotézy. 
x <- model['Eexp'] / model['MCLoAI']
stem(x[['Eexp']]) 
wilcox.test(x[['Eexp']], mu=1, alt="two.sided",conf.level=0.95)
wilcox.test(x[['Eexp']], mu=1, alt="greater",conf.level=0.95)
#' ## Je príslušný model bezpečný?
#' 
#' Podľa vykonaných testov je daný model bezpečný lebo v oboch testoch nám vyšlo že median hodnosti úspešnosti je nad 1.
#' 


 



#' 
#' #  Príklad 2 (konzistentnosť modelu)
#' 
#' **Zadanie:** Okrem požiadavky bezpečnosti modelu je v praxi dôležitá aj požiadavka konzistentnosti modelu (materiál, množstvo práce). Odporúča sa, aby disperzia dátového súboru podielov bola ideálne 0.01 a nepresiahla 0.05. Vhodným testom zistite, či príslušné dva modely tieto požiadavky spĺňajú (nutnú a ideálnu). Napíšte nulovú a alternatívnu hypotézu a slovne zhodnoťte výsledky analýzy (Ktorý model by ste preferovali, ak podmienkou by bola táto požiadavka?).
#' 
#' **Riešenie:** pre k = 1, model Eurokód a MC LoA II
#' 
#' 

#' ## Nulová hypotéza
#' 
#' Hodnota rozpytlu dátového súboru bude v intervale od vrátane 0.01 po 0.05.
#' 
 
#' ## Alternatívna hypotéza
#' 
#' Hodnota rozpytlu dátového súboru bude mimo internval od vrátane 0.01 po 0.05.
#'
  
#'
#' ## Test 
#' Neviem ci som pochopil dobre zadanie ale kvoli "disperzia dátového súboru podielov" som vypocital rovnky podiel ako v prvej ulohe (exp/model). A nasledne pre tieto podiely som vypocital test rozptylu. Pricom som pocital nech je rozptyl idealny
#' .
eurokod <- model[['Eexp']] / model[['Eurokod']] 
mc2 <- model[['Eexp']] / model[['MCLoAII']]

func1 <-  function(df){
  x <- df
  alpha <- 0.05
  n <- length(x)
  TS <- (n-1) * var(x) / 0.01
  output <- c(var_hat = var(x),
              test_statistika = TS,
              krit_hodnota = qchisq(1-alpha, df = n-1),
              p_hodnota = 1 - pchisq(TS, df = n-1)
  ) %>% round(5)
  print(output)  
}
func1(mc2)
func1(eurokod)

#' ## Ktorý model by ste preferovali, ak podmienkou by bola táto požiadavka?
#' Podla vypočítaného rozptylu by som si vybral model eurokod. Pricom ani jeden model nesplnil ideálnu požiadavku ale model eurokod splnil nutnú požiadavku pre rozpyl a preto by som si ho vybral.  

#' 
#' # Príklad 3 (životnosť bateriek)
#' 
#' **Zadanie:**  V súbore data.xlsx sú zaznamenané údaje o životnosti bateriek (v dňoch) do prístrojov na geotechnický monitoring tunelov a prieskumných štôlní. Dáta sú normálne rozdelené. Vhodným parametrickým testom na hladine významnosti α=0.05 overte pravdivosť tvrdenia príslušného výrobcu a svoje zistenia slovne zhodnoťte. Aká je p-hodnota?
#' 
#' Výrobca X tvrdí, že jeho baterky majú životnosť aspoň Y dní so smerodajnou odchýlkou Z dní.
#' 
#' 
#' |k|	X	|Y	|Z|
#' |-|:-:|:-:|-:|
#' |0|	A	|46	|1.0
#' |1|	B	|45	|0.5
#' |2|	C	|45	|1.2
#' |3|	D	|47	|1.0
#' 
#' **Riešenie:** pre k=1, X=B, Y=45, Z=0.5
#' 

#'
#' ## Test  
#' Hypoteza : Výrobca B tvrdí, že jeho baterky majú životnosť aspoň 45 dní so smerodajnou odchýlkou 0.5 dna
#'

xdata <- data[['B']]
dlzka <- length(xdata)
xmean <- mean(xdata)
xsd <- 0.5
(TS <- (xmean-45)/(xsd/sqrt(dlzka)))
pt(TS, df=dlzka-1) 
#' p-hodnota je mensia ako 0.05 tak danu hypotezu musime zamietnut. Vyrobca B nevyrába baterky ktoré majú životnosť aspoň 45 dní.
