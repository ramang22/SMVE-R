#' ---
#' title: "1. Rozdelenie pravdepodobnosti"
#' subtitle: "Štatistické metódy vyhodnocovania experimentov"
#' author: "William Brach"
#' date: "23.02.2021"
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
#' # Príklad 1 (klíčivosť semien)
#' 
#' **Zadanie:** Výrobca garantuje, že klíčivosť semien špeciálnej odrody hrachu je 80%. Záhradkár kúpil 20 semien hrachu. Vypočítajte pravdepodobnosť, že
#' 
#' * všetky semená vyklíčia
#' * najviac k 1 semien vyklíči
#' * vyklíči aspoň k−1 semien
#' * Vypočítajte strednú hodnotu a disperziu
#' * zostrojte graf rozdelenia pravdepodobnosti
#' **Riešenie:** 

k <- 5 # BRACH
n <- 20 # pocet pokusov
p <- 0.8 # pravdepodobnost nastatia jedneho pokusu

#' 1. Všetky semená vyklíčia
dbinom(20, size = n, prob = p)  

#' 2. Najviac k semien vyklíči
pbinom(k, size = n, prob = p)  # pravdepodobnosť úspechu najviac v 5 pokusoch 

#' 3. Vyklíči aspoň k−1 semien.
1-pbinom(3, size = n, prob = p)  # pravdepodobnosť uspechu v aspon 4 (5-1) pokusoch

#' 4. Vypočítajte strednú hodnotu a disperziu
n*p  # stredna hodnota
n*p*(1-p)  # disperzia

#' 5. Zostrojte graf rozdelenia pravdepodobnosti
x <- 0:n
plot(x, 
     y = dbinom(x, size = n, prob =p),
     xlab = "pocet uspesnych pokusov", 
     ylab = "pravdepodobnost(na funkcia)", 
     main="Graf rozdelenia pravdepodobnosti"
)

#' 
#' # Príklad 2 (pomocou hypergeometrického rozdelenia)
#' 
#' **Zadanie:** Riešte predošlý príklad pomocou hypergeometrického rozdelenia, ak vieme, že záhradkár vyberal zo 400 semien hrachu. Porovnajte grafy rozdelenia pravdepodobnosti.
#' 
#' **Riešenie:**
#' 
k <- 5 # BRACH
whiteBalls <- 400 * 0.8 # vypocitame si kolko je dobrych semien hrachu je vo vzorke
blackBalls <- 400 - (400 * 0.8) #vypocime si kolko zlych semien hrachu je vo vzorke
N <- 400 #velkost vzorky

#' 1. Všetky semená vyklíčia
dhyper(20, m = whiteBalls, n = blackBalls,  k = 20)  # x = 20 lebo chceme aby vsetkych 20 vybratych vyklicilo
#' 2. Najviac k semien vyklíči
#' 1 - phyper(20-5) kvoli tomu aby sme od 1 odcitali aspon 15 vyklicenych
1 - phyper(20-k-1, m = whiteBalls, n = blackBalls,  k = 20)
#' 3. Vyklíči aspoň k−1 (4) semien.
phyper(3, m = whiteBalls, n = blackBalls,  k = 20, lower.tail = F )
#' 4. Vypočítajte strednú hodnotu a disperziu
N <- 400
M <- whiteBalls
n <- 20
n*M/N  # stredna hodnota
n*M/N*(1-M/N)*(N-n)/(N-1)  # disperzia

#' 5. Zostrojte graf rozdelenia pravdepodobnosti
x <- 0:n
plot(x, 
     y = dhyper(x, m = whiteBalls, n = blackBalls, k = n),
     xlab = "pocet uspesnych pokusov", 
     ylab = "pravdepodobnostná funkcia", 
     main="Hypergeometrické rozdelenie"
)

plot(x, 
     y = dbinom(x, size = 20, prob =0.8),
     xlab = "pocet uspesnych pokusov", 
     ylab = "pravdepodobnost(na funkcia)", 
     main="Graf rozdelenia pravdepodobnosti"
)
#' Grafy binomickeho a hypergeometrickeho rozdelenia su totozne.

#' 
#' # Príklad 3 (návštevnosť stránky)
#' 
#' **Zadanie:** Stránku internetového obchodu navštívi za sledované obdobie v priebehu jednej hodiny v priemere 30 záujemcov. Uvažujme časový interval 20 minút. Aká je pravdepodobnosť, že
#' 
#' * stránku navštívi 15 záujemcov
#' * stránku navštívi aspoň jeden záujemca
#' * Aký je najpravdepodobnejší počet návštev za 10 minút?
#' * Zostrojte graf rozdelenia pravdepodobnosti pre prvých 15 hodnôt náhodnej premennej (uvažujeme časový interval 20 minút).
#' * Generujte k náhodných čísiel rozdelenia (pre interval 20 minút).
#' **Riešenie:**
#' 
k <- 5
lambda <- 20*30/60
#' 1. Stránku navštívi 15 záujemcov
ppois(15,lambda) # p pois lebo chceme pravedpodobnost pre 15 a nie sumu od 0 po 15
#' 2. Stránku navštívi aspoň jeden záujemca
1 - ppois(1,lambda)
#' 3. Aký je najpravdepodobnejší počet návštev za 10 minút?
# neviem :(
#' 4. Zostrojte graf rozdelenia pravdepodobnosti pre prvých 15 hodnôt náhodnej premennej (uvažujeme časový interval 20 minút).
x <- 0:15
plot(x, 
     y = dpois(x,lambda),
     xlab = "pocet uspesnych pokusov", 
     ylab = "pravdepodobnostná funkcia", 
     main=paste("Poissonovo rozdelenie, lambda = ",lambda)
)
#' 5. Generujte k náhodných čísiel rozdelenia (pre interval 20 minút).
rpois(n=5, lambda=lambda)

#' 
#' # Príklad 4 (trvanie skúšky)
#' 
#' **Zadanie:** Dĺžka vypracovania skúšky zo SMVE sa riadi normálnym rozdelením so strednou hodnotou 240 minút a smerodajnou odchýlkou 10 minút.
#' 
#' * Koľko % študentov dokončí test do 220 minút?
#' * Koľko % študentov ukončí test v časovom intervale 210-230 minút?
#' * Koľko % študentov bude potrebovať na ukončenie testu aspoň 240 minút?
#' * Stanovte optimálnu dobu vypracovania skúšky, aby zadanie stihlo v tom čase odovzdať aspoň 75% študentov.
#' 
#' **Riešenie:**
#' 
sd1 <- 10
mean1 <- 240
#' 1.  Koľko % študentov dokončí test do 220 minút?
dnorm(220, mean=mean1, sd=sd1 ) * 100

#' 2.  K Koľko % študentov ukončí test v časovom intervale 210-230 minút?
(dnorm(230, mean=mean1, sd=sd1 ) - dnorm(210, mean=mean1, sd=sd1 )) * 100 
#' 3. Koľko % študentov bude potrebovať na ukončenie testu aspoň 240 minút?
(1 - pnorm(240,mean=mean1, sd=sd1)) * 100
#' 4. Stanovte optimálnu dobu vypracovania skúšky, aby zadanie stihlo v tom čase odovzdať aspoň 75% študentov.
qnorm(0.75,mean=mean1, sd=sd1)

#' 
#' # Príklad 5 (doba životnosti)
#' 
#' **Zadanie:** Doba životnosti PC komponentu má exponenciálne rozdelenie pravdepodobnosti so strednou hodnotou 2000 dní. Vypočítajte pravdepodobnosť, že komponent.
#' 
#' * bude funkčný aspoň 3000 dní,
#' * sa pokazí ešte v jeho priemernej dobe životnosti.
#' * Určte maximálnu záručnú dobu, ktorú chce poskytnúť jeho výrobca, ak pripúšťa iba 15 % reklamácii.
#'
#' **Riešenie:**
#' 
#' 1.  Bude funkčný aspoň 3000 dní
pexp(3000, 1/2000)
#' 2.  Sa pokazí ešte v jeho priemernej dobe životnosti
lambda  <-  1/2000 # definujeme si lambdu
medianLifeCycle <-  qexp(0.5, rate=lambda) #vypocitame priemernu zivotnost pomocou medianu
dexp(medianLifeCycle,lambda) #sucet vsetkych dni od dna 0 po medianLifeCycle

#' 3.  Určte maximálnu záručnú dobu, ktorú chce poskytnúť jeho výrobca, ak pripúšťa iba 15 % reklamácii.
qexp(0.15, rate=lambda)
