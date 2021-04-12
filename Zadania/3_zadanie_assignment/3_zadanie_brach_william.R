#' ---
#' title: "3. Intervaly spoľahlivost"
#' subtitle: "Štatistické metódy vyhodnocovania experimentov"
#' author: "William Brach"
#' date: "15.03.2021"
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
#' Dobry den,
#' tuto problematiku som bohuzial nepochopil dostatocne podla mna. Bolo by mozne keby sme si na cviceni presli toto zadanie alebo 
#' popripadne by ste mohli zverejnit zadanie s spravnymi odpovedami? Aby som sa mohol danu problematiku doucit?
#' 

#' 
#' # Príklad 1 (podiel konzerv)
#' 
#' **Zadanie:** Pri kontrole dátumu spotreby určitého druhu mäsovej konzervy v skladoch bolo náhodne vybraných 320 z 20 000 konzerv a zistené, že 59 z nich má expirovanú záručnú dobu. Stanovte so spoľahlivosťou 95% intervalový odhad podielu expirovaných mäsových konzerv.
#' 
#' **Riešenie:** 

p <- 59/320 #vypocet prob
se <- sqrt(p*(1-p)/100)
alpha <-  0.05
p + c(-1,1)*qnorm(1-alpha/2)*se # hodnoty medzi hranicami

prop.test(59, n=320, conf.level=1-alpha)$conf.int # hodnoty s presnou SE


#' 
#' # Príklad 2 (obsah hnojiva)
#' 
#' **Zadanie:** Urobilo sa šesť paralelných stanovení obsahu P2O5 vo vzorke hnojiva s nasledujúcimi výsledkami:
#' 16.5, 15.9, 16.6, 15.8, 16.4, 16.0, 15 + k10.
#' 
#' **Predpokladajme, že ide o výber z normálneho rozdelenia N(μ,σ2). Vypočítajte:**
#' 
#' a, Obojstranný 90% interval spoľahlivosti pre strednú hodnotu μ obsahu P2O5
#' 
#' b, Dolný (ľavostranný, teda zľava ohraničený) 99%-ný interval spoľahlivosti pre smerodajnú odchýlku σ.
#' 
#' **Na základe vhodných intervalov spoľahlivosti odpovedzte na otázky:**
#' 
#' c, Dá sa spoľahlivo (s 95%-nou spoľahlivosťou) tvrdiť, že stredná hodnota obsahu P2O5 v hnojive nie je rovná 16.4 (inak povedané: líši sa stredná hodnota štatisticky významne od 16.4)?
#' 
#' d, Je rozptyl obsahu P2O5 v hnojive štatisticky významne väčší než 0.05?
#'
#' **Riešenie:** 

#' 
#' A
k <- nchar("BRACH")
x <- c(16.5, 15.9, 16.6, 15.8, 16.4, 16.0, 15 + k/10)

m <- mean(x)  # bodový odhad strednej hodnoty
s <- sd(x)  # bodový odhad smerodajnej odchýlky
n <- length(x)  # rozsah výberu
alfa <- 0.10  # hladina významnosti
q <- qt(1-alfa, df=n-1) 

#' Interval spoľahlivosti
m + c(D=-1, H=1) * q * s / sqrt(n)  
#' 
#' B
#' 
#' Dolny odhad pre 99%
c(D = m - qt(0.99, df=n-1) * s / sqrt(n), H = Inf)

#' C
q <- qchisq(c(alfa/2, 1-alfa/2), df=n-1)
odchylka <-  c(D = sqrt( (n-1)/qchisq(1-alfa,df=n-1) ) * s, H = Inf)  # pre stand.odchylku
#' Nemozeme to tvrdit lebo odchylka plus stredna hodnota je viac ako 16.4
mean(x) + odchylka['D']

#' D
#' 
#' Porovname vysledok testu ci je vacsi ako 0.05 a ak ano tak je statisky vyznamny
value <- t.test(x, conf.level = 0.95)$p.value
value > 0.05
#' 
#' # Príklad 3 (obsah železa)
#' 
#' **Zadanie:** Kontroloval sa percentuálny obsah železa vo vzorkách železnej rudy. Z predošlých etáp monitoringu bola zistená hodnota smerodajnej odchýlky merania σ=3.3, ktorá sa ukázala ako stabilná a pomerne presná. Výsledky aktuálnej etapy merania sú v tabuľke početností, kde x je hodnota premennej a n jej početnosť.
#' 
#' a, Vypočítajte bodový odhad strednej hodnoty a s akou pravdepodobnosťou sa skutočná stredná hodnota obsahu železa nachádza v intervale (-Inf, 16.04), ak predpokladáme stabilný rozptyl.
#' 
#' b, Skonštruujte taký interval spoľahlivosti, aby sme na základe neho mohli posúdiť, či s pravdepodobnosťou (100-k)% mohlo dôjsť ku zmenšeniu rozptylu obsahu železa vo vzorkách oproti predošlým etapám. Zhodnoťte.
#' 
#' c, Ak došlo ku zníženiu rozptylu, skorigujte pravdepodobnosť z úlohy a).
#' 
#' **Riešenie:** 
x <- c(9, 11,11,12,12,12,14,14,14,14,15,15,15,15,15,15,15,16,16,16,16,16,17,17,17,17,18,18,18,20,20,21)
xx <- unique(x)
#'
#' A
#' 
#' bodovy odhad
mean(x)
#' skutocna stredna hodnota
median(x)


#'
#' B 
#' 
#' odhad spolahlivosti 
mean(x) + c(-1,1)*qnorm(1/2)*1.5/sqrt(length(x)) 

#'
#' C 
#' 

