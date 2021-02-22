#' --- 
#' title: "Úvod do R"
#' author: "Tomáš Bacigál"
#' date: "16.2.2021"
#' output: 
#'   html_document:
#'      toc: true
#' ---


#+  include=FALSE
knitr::opts_chunk$set(
  collapse = TRUE
)


#' # Úvod do R, prostredia pre štatistické výpočty a vizualizáciu 
#' 
#' R je prostredie pre štatistické výpočty a vizualizáciu údajov, vyvíjané pod slobodnou licenciou a pre rôzne platformy, inštalačné súbory resp. návod na inštaláciu ako aj dokumentáciu je možné nájsť na domovskej stránke projektu [www.r-project.org](www.r-project.org). Základom je interpretovaný počítačový jazyk R umožňujúci vetvenie, cyklenie a modulárne programovanie pomocou funkcií. Distribúcia R obsahuje funkcionalitu pre veľký počet štatistických metód ako lineárne a zovšeobecnené lineárne modely, nelineárnu regresiu, analýzu časových radov, klasické parametrické a neparametrické testy, zhlukovú analýzu a vyhladzovanie, ďalej množstvo funkcií poskytuje flexibilné prostredie pre grafickú reprezentáciu údajov, no najväčšou výhodou R je obrovská a voľne dostupná databáza prídavných balíčkov tvorených komunitou používateľov z akademickej i komerčnej sféry. Základné prostredie R (inštalácie v systéme Windows) tvorí textový editor, pomocou ktorého používateľ píše zdrojový kód v jazyku R, a príkazový riadok (*konzola*), ktorý odoslaný kód interpretuje. Grafické výstupy sú presmerované do samostatných okien. Pre pohodlnú prácu odporúčame použitie integrovaného vývojového prostredia [RStudio](www.rstudio.com), ktoré farebne zvýrazňuje syntax, poskytuje nápovedu, sprístupňuje zoznam definovaných objektov, uľahčuje tvorbu dokumentácie a veľa ďaľších užitočných nástrojov.
#' Zdrojový kód sa na interpretáciu do príkazového riadku posiela typicky buď po riadkoch alebo vyznačením jeho časti, a stlačením kombinácie kláves *Ctrl-R* (v prostredí RStudio *Ctrl-Enter*). 
#' 
#' ## R ako lepšia kalkulačka
#' 
#' R sa dá použiť podobne ako kalkulačka na rôzne jednoduché výpočty a zobrazenie, pričom základný výstup z príkazového riadku je čisto textový a tak tradičný matematický zápis matematických symbolov ako napr. $\sqrt{x^3}$ nepodporuje. 
#' 
#' ### Základné matematické operátory a funkcie 
#' 
#' Najčastejšou operáciou je sčítanie. Po prijatí textového vstupu v príkazovom riadku (začína sa znakom `>`) systém R vypíše výsledok, v nasledujúcom príklade je to číslo 5. Keďže však v R je všetko nejaká forma poľa, aj jediné číslo je uložené vo forme vektora, a to dĺžky 1. Výpis dlhších vektorov sa v konzole zalamuje do riadkov, pričom každý začína indexom prvého elementu v danom riadku. To vysvetľuje reťazec `[1]` pred výsledkom nášho príkladu.
#+ comment="", prompt=TRUE, collapse=FALSE
2+3

#' V nasledujúcom texte prispôsobíme formátovanie textu tak, aby vstup a výstup nezaberal príliš veľa miesta v našej publikácii (zlúčime okienka do jedného), zároveň aby sa vstup dal pohodlne kopírovať do svojho skúšobného scriptového súboru (odstránime `>`) a aby sa výstup líšil od vstupu (bude začínať znakmi `##`). Krátke poznámky budú uvádzané za znakom pre komentár (`#`).
#' 
#' Pokračujme základnými matematickými operátormi a matematickými funkciami, ktoré sú dostupné na bežných kalkulačkách:
#+ 
3/2

2^3

4 ^ 2 - 3 * 2 # násobenie má prednosť pred odčítaním

(56-14)/6 - 4*7*10/(5^2-5) # často je potrebné použiť zátvorky

sqrt(2)  # pomenovaná funkcia pre odmocnenie 

abs(2-4)        # |2-4|

cos(4*pi)       # ďaľšie sú sin(), tan(), atan(), atan2() ...

log(0)		# funkcia pre tento argument nie je definovaná
exp(1)    # eulerovo číslo

factorial(6)   # 6!

choose(52,5)   # kombinačné číslo 52!/(47!*5!)


#' ### Priradenie hodnoty premennej
#' 
#' Často treba s hodnotami počítať viackrát, vtedy sa hodí uložiť ich do premennej. Možností uloženia je v R niekoľko:
#+ 
n <- 5   # klávesová skratka [Alt] + [-]
n

15 -> n; n

assign("n", 25)

n = 35      # radšej nepoužívať, (esteticky) vyhradené pre zadávanie argumentov funkcie

(n <- 2*3)   # tri v jednom: vypočíta, priradí a vypíše hodnotu


#' ### Vektory
#' 
#' Podobne ako s jednoprvkovým vektorom sa pracuje s ľubovoľne dlhým vektorom. Najprv hodnoty skombinujeme do vektorov:
#+ 
x <- c(1,2,3,4)      
y <- c(5,6,7,8)

#' Väčšina operácií v R je vektorizovaná, preto sa aplikujú postupne na všetky prvky vektora, resps. zodpovedajúce si prvky viacerých vektorov:

#+ 
x*y; y/x; y-x; x^y   # väčšina funkcií v R je vektorizovaná

cos(x*pi) + cos(y*pi)

s <- c(1,1,3,4,7,11)
length(s)       # dĺžka vektora

sum(s)     		# 1+1+3+4+7+11

prod(s) 		# 1*1*3*4*7*11

cumsum(s)  # 1, 1+1, 1+1+3, ...

diff(s)        # 1-1, 3-1, 4-3, 7-4, 11-7

diff(s, l=2)  # 3-1, 4-1, 7-3, 11-4


#' ### Matice a polia 
#' 
#' Matice ako dvojrozmerné pole sa zväčša vytvára z vektorov
#+ 
a <- c(1,2,3,4,5,6,7,8,9,10)
A <- matrix(a, nrow = 5, ncol = 2)  # napĺňanie matice sa štandardne deje po stĺpcoch
A

B <- matrix(a, nrow = 5, ncol = 2, byrow = TRUE) # napĺňanie po riadkoch
B

C <- a
dim(C) <- c(5,2)	# matica je vektor, ktorý dostal dva rozmery

C <- t(C)       # transponovanie
C

B %*% C       		# násobenie

D <- C %*% B
D

det(D)

solve(D)		# inverzia matice (riešenie D*x=I, kde I je jednotková m.)

cbind(x,y)      # lepenie vektorov do stĺpcov, resp. do riadkov matice
rbind(x,y)

E <- letters[1:(4*2*3)]   # `letters` je zabudovaná databáza písmen v abecednom poradí
dim(E) <- c(4,2,3)    # z vektora trojrozmerné pole
E   # výpis vo forme dvojrozmerných rezov


#' ### Vstup údajov

#+ 
ovocie <- c("jablko", "hruška", "pomaranč")

1:9         # sekvencia s krokom 1

1.5:10      # nedostaneme sa az po koniec, no ten môžeme pridať
c(1.5:10,10)

seq(-2,5)    # to isté ako -2:5, ale seq() dokáže i viac...

seq(-2,5,by=.5)     # prírastok o 0.5

seq(-2,5,length=4)  # dĺžka sekvencie bude práve 4

rep(9,5) 	# opakuj číslo 9 päť-krát

rep(1:4,2)
rep(1:4, each = 2)
rep(1:4, each=2, times=3)
rep(1:4,1:4)

matrix(rep(c(1,rep(0,4)),4), nrow=4, ncol=4)      # i takto sa dá vytvoriť jednotková matica 4x4
diag(1, nrow=4) 	# no takto je to predsalen jednoduchšie :)


#' Iné spôsoby vloženia údajov zahŕňajú vstup z klávesnice
#+ eval=FALSE
vstup <- scan()     # postupne zadavanie z klavesnice (na konci Enter naprazdno)

#' alebo zo súboru, kedy je dobré najprv nastaviť pracovný adresár
#+ eval=FALSE
setwd(choose.dir(default = "", caption = "Select folder")) # interaktívne
setwd("F:\\cesta\\ku/súboru")  # explicitne

#' potom použiť `scan`, `read.table` alebo iné funkcie
#+ eval=FALSE
vstup <- read.table("subor.txt", header = TRUE, sep = "", dec=".", skip=1)

#' kde začne druhým riadkom a hodnoty by mali byť oddelené "bielymi' znakmi (medzera, tab, nový riadok).
#' 
#' 
#' ### Manipulácia s prvkami pola
#' 
#' Výber prvkov poľa (subsetting) sa dá mnohými spôsobmi
#+ 
z <- c(y,x); z

z[1]
z[5:8]          # výpis konkretnych prvkov
z[c(5,8)]

z[-(2:8)] 	    # vyberie všetky okrem prvkov zadaných zápornym indexom
z[-c(5,8)]

z[8] <- 10      # ôsmemu prvku je priradená ina hodnota

z > 5           # vysledkom je vektor logických hodnôt

z[z>5 & z<7]          # výber všetkých prvkov väčších ako 5

(1:20)[c(TRUE,FALSE)]   # všetky nepárne (lebo vektor logických hodnôt sa replikuje)

A
A[1,2]          # prvok z 1.riadku a 2.stĺpca matice A
A[1,,drop=F]    # prvý riadok
A[,2]           # druhý stlpec
A[,2,drop=F]


#' 
#' 
#' ## Jednoduché grafy

#+ 
plot(sin, from=0, to=2*pi, xlab="os x")  # použije plot.function

par(mfrow=c(1,2))      # rozdelenie zobrazovanej oblasti do matice 1x2

x <- seq(0, 2*pi, length=16+1)    # diskrétne body
plot(x,sin(x),type="o", lty="dashed")

plot(x, sin(x), type="p", pch="+")      # graf dvojic {x[i],sin(x[i])}
lines(x, sin(x), col="red", lty="dashed")     # čiary spájajúce diskrétne body sú pridané do predošlého grafu

par(mfrow=c(1,1))     # nastavenie mriežky grafického okna vrátime na pôvodné hodnoty


#' Export obrázku do súboru sa deje presmerovaním grafického výstupu do tzv. grafického *zariadenia* (*device*), napr. pre formáty PNG, BMP, JPEG, PDF a ďalšie. Prvý príkaz dané zariadenie otvorí, posledný zavrie.
#+ eval=FALSE
png(file="sinus.png", width=600, height=400, units="px")
plot(sin, from=0, to=2*pi) 
dev.off()

#' Demonštračné ukážky základnej grafiky, 2D či 3D, si možno pozrieť pomocou generickej funkcie `demo`.
#+ eval=FALSE
demo(graphics)
demo(persp)


#' Ukážky pokročilejšej grafiky nájdeme najmä na internete:
#' 
#' * http://www.r-graph-gallery.com/
#' * http://rgraphgallery.blogspot.sk/
#' * vyhladaním "r graphics" v skupine Images
#' * galéria interaktívnej grafiky https://shiny.rstudio.com/gallery/
#' 
#' 
#' ## Dátove objekty
#' 
#' Základnými dátovými objektami používanými v R sú *vector*, *factor*, *matrix*, *array*, *data frame*, *ts* a *list*.
#' 
#' Sú charakterizované menom, obsahom ale aj atribútmi, ktoré špecifikujú typ obsahu.
#' Základnými atribútmi sú dĺžka (*length*) a typ (*mode*). Ďalšími sú napr. rozmer (*dimension*) pri viacrozmerných pravidelných objektoch ako matrixm, array či data.frame, ďalej trieda (*class*), či 
#' 
#' Typy dátových objektov sú *numeric*, *character*, *complex*, *logical*, *function*, *expression*, a ďalšie.
#' 
#' Iba *data frame* a *list* môžu obsahovať viac ako jeden typ.
#' 
#' 
#' ### vector
#' 
#' 
#' V  nasledujúcich príkladoch vytvoríme nový vektor zadaním dĺžky, módu a konkrétneho prvku. Nezadané hodnoty sú doplnené prednastavenými, napr. dĺžka rovná nule alebo ostatné prvky rovné FALSE (ekvivalentom v numerickom móde je nula), prípadne nie sú definované vôbec (NA vo význame "not available"). 
#+ 
v <- vector(mode="logical", length=0); v

#' Nový vektor dĺžky 0 je predurčený obsahovať logické hodnoty "ano" alebo "nie", alternatívne aj `v <- logical()`.
#+ 
v <- logical(); v[2] <- TRUE; v              
v <- logical(3); v[2] <- TRUE; v


#' ### factor
#' 
#' Dátový typ *factor* je (interne) vektor obsahujúci iba prirodzené čísla (poradové čísla, indexy) a atribút *levels* (úrovne), v ktorom sú uložené jedinečné hodnoty vo forme znakových reťazcov. Navonok sa zobrazuje ako vektor týchto reťazcov postupne vyberaných z *levels* svojim indexom.

#+ 
v <- c(10,40,40,30,40,30)
fv <- factor(v); fv
str(fv)  # úspornejšie uskladnenie v pamäti
fv*2  # factor je hybrid medzi character a numeric
as.numeric(fv)*2  # konverzia vráti iba indexy úrovní
as.numeric(as.character(fv))


#' ### matrix
#' 
#' Dátový typ matica je (interne) len vektor s atribútom *dim* (dimensions, rozmery) obsahujúcim počet riadkov a počet stĺpcov matice, pričom hodnoty vektora sú do matice napĺňané po stĺpcoch

#+ 
A <- 1:12
dim(A) <- c(2,6)	# ďalší atribút, nepatri medzi základné (non-intrinsic)
A

matrix(1:12, nrow=2)


#' ### data frame
#' 
#' Dátový objekt *data frame* sa na prvý pohľad tvári ako matica, ale na rozdiel od nej môže mať stĺpce odlišného *typu*. Interne je to skôr list s rovnakou dĺžkou všetkých prvkov, takže sa dá zobraziť ako tabuľka. Každý stĺpec zvyčajne predstavuje jednu pozorovanú veličinu a každý riadok jedno pozorovanie (pre každú veličinu). Ak napríklad na križovatke pozorujeme, koľko pasažierov je v každom aute a či sú všetci pripútaní bezpečnostným pásom, výsledný súbor údajov vo forme tabuľky *data frame* môže vyzerať nasledovne:
#+ 
pocet_pasazierov <- c(1,3,2,5,2,2,1,1,2,1)
priputani <- c(T,T,F,T,F,F,T,F,F,T)
auta <- data.frame(pocet_pasazierov,priputani)
auta

#' Interaktívna editácia je možná, ale málokedy používaná:
#+ eval=FALSE
auta <- edit(auta)	#vytvorte tretiu premennu "mesta" so skratkami miest
auta

#' Interná štruktúra dátového objektu:
#+ 
attributes(auta)
str(auta)

#' R obsahuje mnoho vlastných súborov dát, príkazom `data()` sa otvorí ich zoznam v samostatnom okne (na samostatnej záložke), vyberieme z nich napríklad merania priemeru, výšky a objemu na vzorke stromov.
#+ 
data()	
data(trees)  # načítanie dátového súboru, ktorý je súčasťou R
trees

#' Z tabuľky potom možno prvky vyberať rôznymi spôsobmi (subsetting)
#+ 
trees[c("Girth","Height")]  # konkrétne stĺpce ich názvom
trees[1]  # konkrétny stĺpec jeho poradím; alebo napr. trees[c(1,2)]
trees[[1]]  # výsledkom nie je data.frame ale vektor
trees$Girth  # opäť vektor


#' Premennú *Girth* v globálnom prostredí nemáme ...
#+ error=TRUE
Girth

#' ... až pokým doň nepripojíme data.
#+ 
attach(trees)  
Girth
detach(trees)

#' Prístup k premenným môže byť aj lokálne.
#+ 
with(trees, head(Girth) )  

#' Výber riadkov podla zadaných podmienok a výber stĺpcov:
#+ 
subset(trees, subset = Height>80 & Girth>10.0, select=Height:Volume)
trees[trees$Height>80 & trees$Girth>10.0, c("Height","Volume")]


#' ### list
#' 
#' *List* je najvšeobecnejší, môže obsahovať rôzne dátové objekty, nielen vektor.

#+ 
List1 <- list(polozka1=c(1,2,3), polozka2=c("hruska","jablko"), polozka3=FALSE)
List1  # list nemusí mať rovnaký počet riadkov/stĺpcov (na rozdiel od predošlých objektov)


#' 
#' Subsetting funguje podobne ako pri *data frame*.
#' 
#' ### ts (time series)
#' 
#' Dátový typ *ts* je iba vektor s atribútom *tsp*, ktorý obsahuje začiatočný čas, koncový čas a niečo ako základnú periódu, zvyčajne sezónnu.

#+ 
ts(1:10, start=1959)

cr <- ts(rnorm(20), frequency = 12, start=c(1959,1))

plot(cr)   # lepši výsledok externým balíkom pomocou plot(xts::as.xts(cr))


#' ### expression
#' 
#' Tento dátový typ je užitočný napr. pri symbolických výpočtoch alebo zobrazení vzorcov v grafoch.

#+ 
x <- 3; y <- 2.5; z <- 1
výraz <- expression(x/(y+exp(z)))
výraz
eval(výraz)

D(výraz, "y")


#' ### Konverzia medzi dátovymi objektami

#+ 
as.numeric(c(TRUE,FALSE))
as.numeric("4")
as.numeric(c("A","Z"))


#+ 
as.logical(c(-1,0,1,2))
as.logical(c("FALSE","F"))
as.logical("A")


#+ 
as.character(1)
as.character(TRUE)


#+ 
as.numeric( factor(c(0,100,36.7,100)) )
as.numeric(as.character( factor(c(0,100,36.7,100)) ))


#' ## Operátory 
#' 
#' * aritmetické: + - * / ^ %%(modulo) %/% (celočíselné delenie)
#' * porovnávacie: < > <= >= == !=
#' * logické: ! & && | || xor
#' * ďalšie operátory R: $ @ [ [[ : ? <- <<- :: 
#' 
#' Prioritu pri vyhodnocovaní príkazov dokumentuje stránka v nápovede:
#+ eval=FALSE
?Syntax     # zoznam operátorov podla priority

#' Používanie logických operátorov:
#+ 
x <- 2
y <- z <- 1:3
LO <- c(F,F,F)

0 < x & x < 1     # skrátený zápis 0 < x < 1 NEfunguje

x < y | LO        
x < y || LO       # zdvojený logický operátor si všíma iba prvý prvok vektorov

#' Porovnávanie 
#+ 
y == z            # porovnanie po prvkoch
identical(y, z)   # porovnanie objektov ako 'celku'
all.equal(y, z)

#' môže byť pri zaokrúhľovaní na úrovni strojovej presnosti zradné
#+ 
0.9 == 1.1 - 0.2            # porovnanie numerických hodnôt
identical(0.9, 1.1 - 0.2)
all.equal(0.9, 1.1 - 0.2)
all.equal(0.9, 1.1 - 0.2, tolerance = 1e-16)


#' 
#' ## Programovanie
#' 
#' ### Cykly a podmienky 
#' 
#' Podmienky a cykly sa dajú zapísať pomocou rovnakých slov ako v známom jazyku C: `if`, `else`, `for`, `while`, `repeat`, `break`, `next`.
#' 
#' Majme údaje uložené v data frame

#+ 
data <- data.frame(
  a = 1:5,
  b = runif(5),
  c = c(2,0.3,4,-1,0)
); data


#' pre ktorý chceme zistiť medián každého stĺpca. Môžme na to ísť so sekerou

#+ 
median(data$a)
median(data$b)
median(data$c)


#' alebo motorovou pílou,

#+ 
vysledok <- vector("double", ncol(data))     
for (i in 1:length(vysledok)) {          
  vysledok[i] <- median(data[[i]])      
}
vysledok


#' prípadne laserovým mečom ako v Hviezdnych vojnách.

#+ 
sapply(data, median)


#' Príklad s kontrol-slovami `if`, `next` a `break`:

#+ 
a = 0
for (i in 1:20) {
  a = i^2
  if(a <= 10 ) {
    cat('a = ', a, '(<= 10)'); cat('\n')
    next
  }
  if(a == 144) {
    cat('a = ', a); cat('\n')
    break
  }
}
i     # hodnota iteracnej premennej nie je po skonceni cyklu vymazana


#' Monte-Carlo odhad hodnoty Ludolfovho čísla:

#+ 
eps <- 1; s <- 0; n <- 0   # inicializačné hodnoty
while(eps > .001) {
  n <- n + 1                    # počet generovaných bodov
  x <- runif(1,-1,1)       			# súradnice náhodného bodu v štvorci
  y <- runif(1,-1,1)       			# [-1,-1][1,-1][1,1][-1,1]
  if(x^2 + y^2 < 1) s <- s + 1  # počet bodov ležiacich vnútri kruhu s polomerom 1
  pihat <- 4*s/n                # odhad 'pi'
  eps = abs(pihat - pi)         # s toleranciou 'eps'
}
pihat     # odhad
n         # počet iterácií


#' Náhodný výpis z N(0,1) rozdelenia, kým čislo nepresiahne hodnotu 1.6 (funkcia `set.seed` nastaví generátor pseudonáhodných čísel, aby bolo možné neskôr zreprodukovať výsledok):

#+ 
set.seed(123)  
repeat {a <- rnorm(1); if (a > 1.6) break; cat(a); cat("\n")}


#' ### Vlastné funkcie
#' 
#' Vytvoríme jednoduchú kvadratickú funkciu a zavoláme ju s argumentom x = 3:

#+ 
f <- function(x) x^2
f(3)


#' Táto funkcia vráti maximum dvoch skalárnych čísel alebo hlášku o ich rovnosti:
#+ 
f1 <- function(a, b) {
  if(is.numeric(c(a,b))) {
    if(a < b) return(b)
    if(a > b) return(a)
    else print("Hodnoty su rovnake")
  }
  else print("Akceptujem iba cisla.")
}
f1(4,7)
f1(0,exp(log(0)))
f1("Adam","Eva")


#' ### Vyhnutie sa cyklom -> vektorizácia
#' 
#' Ak chceme vypočítať priemer stĺpcov matice M
#+ 
M <- cbind(rnorm(20,0,1), rnorm(20,-5,1))

#' namiesto klasických vnorených for-cyklov
#+ 
suma <- numeric(m <- NCOL(M)); n <- NROW(M);
for(i in 1:n) {
  for(j in 1:m) {
    suma[j] <- suma[j] + M[i,j]
  }
}
suma/n   # priemer hodnôt v stĺpcoch


#' je efektívnejšie použiť funkciu `apply` (v tomto prípade existuje náhrada `colMeans(M)`)

#+ 
apply(M, MARGIN=2, FUN=mean)  # pre MARGIN=1 by FUN bola aplikovaná na riadky

#' Funkcia `apply` m8 niekoľko modifikácií: `lapply`, `sapply`, `mapply`, `tapply` a `replicate`.
#' 
#' Namiesto vstavanej funkcie možno ako argument `FUN` aplikovať vlastnú (aj nepomenovanú) funkciu
#+ 
sapply(c(-1,0,5,-5,9), function(x) if(x>0) x else 0)


#' alebo túto funkciu pomenovať a namiesto vloženia do `sapply` ju explicitne vektorizovať, napr. 
#+ 
f2 <- function(prvy, druhy) {
  if(prvy > druhy) prvy else if(druhy>prvy) druhy else NA
}
f2(prvy = c(-1,2,10), druhy = c(2,2,2))   # testuje iba prvy prvok z oboch argumentov
f2 <- Vectorize(f2)
f2(dru = c(2,2,2), pr = c(-1,2,10))  # argumenty stačí písať unikátnou skratkou


#' Vektorizovanou náhradou za `if(cond) expr1 else expr2` je `ifelse(cond,expr1,expr2)`.

#+ 
a <- c(-1,2,10); b <- c(2,2,2)
ifelse(a > b, a, b)           # v tomto prípade ma rovnaký efekt aj pmax(a,b)


#' 
#' ### Prostredie lokálnych premenných
#' 
#' Niekedy chceme urobiť výpočet s pomocnými premennými, ktoré nemajú byť globálne viditeľné, napríklad preto, aby neprepísali hodnotu nejakej existujúcej s rovnakým menom:

#+ 
a <- 0
local({
  b <- a
  a <- 8
  a + b
})
a


#' Ako lokálne premenné sa dajú použiť aj prvky objektu typu list alebo data.frame:
#+ 
with(data, a+1)


#' ## Poznámky
#' 
#' ### Rôzne
#' 
#' Ak uložíme definície funkcií a premenných do súboru, povedzme `mojeprogramy.r`, môžme ich kedykoľvek použiť v inom skriptovom súbore pripojením `source("mojeprogramy.r")`.
#' 
#' Užitočná vec pri hľadaní chýb vo vlastnej funkcii (debugging) je umiestniť funkciu `browser()` do tela funkcie. Po zavolaní našej funkcie nám sprístupní jej prostredie so všetkými lokálnymi premennými.
#' 
#' ### Pôvod jazyka
#' 
#' Čo je R? Odpoveď je jednoduchá. R je dialekt jazyka S.
#' 
#' Jazyk S neprišiel spomedzi tradičných programovacích jazykov, cieľom jeho autorov bolo vymyslieť, ako uľahčiť analýzu údajov ... kľúčovým tu bol prechod od používateľa ku vývojárovi. 
#' 
#' R si zachovalo pôvodnú filozofiu jazyka S, teda poskytnúť jednak interaktivitu pri práci, ale aj možnosti vývoja nových nástrojov.  Technicky je bližšie skôr ku jazyku Scheme než ku S.
#' 
#' Výhody:  
#' - R je voľne dostupný pod licenciou GNU GPL,  
#' - beží na ľubovoľnej štandardnej počítačovej platforme a operačnom systéme,  
#' - aktívny vývoj,  
#' - sofistikované zobrazovacie schopnosti,  
#' - aktívna komunita používateľov  
#' 

#' 
#' 
#' ## Študijná literatúra
#' 
#' Manuály v slovenčine/češtine:   
#'  * seriál na IT spravodajskom portáli Root.cz  https://www.root.cz/serialy/programovaci-jazyk-r/ 
#'  * od študentov http://rmanual.fri.uniza.sk/  
#'  * Jak pracovat s jazykem R https://www.math.muni.cz/~kolacek/vyuka/vypsyst/navod_R.pdf
#' 
#' Manuály a tématicky zameraná literatúra vo svetovom jazyku:  
#'  * https://cran.r-project.org/other-docs.html  
#'  * https://bookdown.org/  
#'  
#' Zábavná literatúra:   
#'   * YaRrr! The Pirate’s Guide to R https://bookdown.org/ndphillips/YaRrr/  
#'   * The R inferno https://www.burns-stat.com/pages/Tutor/R_inferno.pdf  
#' 
#' Odporúčané:   
#'  * Advanced R http://adv-r.had.co.nz/
#'  * R for data science http://r4ds.had.co.nz/  
#'  * R style guide https://style.tidyverse.org  
#' 
#' Prehľad balíkov: 
#'   * Tématický https://cran.r-project.org/web/views/
#'   * Najpopulárnejšie https://support.rstudio.com/hc/en-us/articles/201057987-Quick-list-of-useful-R-packages
#'   * Výber balíkov aj iného: https://awesome-r.com/
#' 
#' História a stručný pohlad napr. https://bookdown.org/rdpeng/rprogdatascience/history-and-overview-of-r.html   
#' Úvod: https://moderndive.com/1-getting-started.html
#' 
#' Niektoré návody na vytvorenie reportu z výpočtov pomocou značkovacieho jazyka Rmarkdown:  
#' * https://bookdown.org/yihui/rmarkdown  
#' * https://rmarkdown.rstudio.com  
#' * http://www.stat.cmu.edu/~cshalizi/rmarkdown/  
#' * https://ismayc.github.io/rbasics-book/  (videá)    
#' * https://holtzy.github.io/Pimp-my-rmd  
#' * https://yongfu.name/Rmd_ref/  
#' * http://www.datadreaming.org/post/r-markdown-theme-gallery/  
#' 
#' 
#' ## Cvičenie
#' 
#' 1. Načítajte súbor údajov *mtcars* z balíka *datasets* a uložte ho do premennej s názvom **dat**. 
#'  2. Zobrazte štruktúru objektu *dat* a prvých 5 riadkov.
#'  3. Preveďte premennú *mpg* na jednotky km/l a uložte ako novú premennú *kml* do toho istého objektu. 
#'  4. Vytvorte logický vektor *aut* indikujúci, či ide o auto s automatickou prevodovkou a pomocou neho vypočítajte priemerný dojazd (v km na 1l paliva) automobilov zvlášť s automatickou a zvlášť s manuálnou prevodovkou.
#'  5. Zobrazte tabuľku všetkých áut s piatimi rýchlostnými stupňami a hmotnosťou do 3000 libier, ktorá obsahuje iba údaje o počte valcov, zdvihovom objeme a výkone motora.
#'  6. Vytvorte funkciu na prevod jednotiek, ktorá bude mať 3 argumenty (s názvom)[s hodnotami]: prevádzanú hodnotu (x), imperiálnu jednotku (impunit)[míľa, galón, palec, libra], smer prevodu do SI (toSI)[TRUE,FALSE], pričom zodpovedajúcimi jednotkami v metrickej sústave SI budú km, l, dm, kg. (Využite pri tom funkciu *switch* a automatickú konverziu módu vektora *toSI* z logického na numerický.)
#'  7. Pomocou *for* cyklu skonvertujte hodnoty zdvihového objemu valcov z kubických palcov na litre. Pomocou funkcie *sapply* preveďte hmotnosť vozidiel na tony. Zachovajte pri tom pôvodné názvy premenných a použite funkciu na prevod jednotiek z predošlej úlohy. 
#'  8. Nastavte pracovný adresár a načítajte tabuľku údajov zo súboru [mtcars.txt](data/mtcars.txt) (uloženého v pracovnom adresári) do objektu typu *data.frame*. Dbajte pri tom na správne nastavenie parametrov importu ako počet riadkov neštrukturovaného popisu, prítomnosť názvu stĺpcov, oddelovací znak desatinných miest, znak oddelujúci stĺpce tabuľky a znak chýbajúcich hodnôt (NA). Porovnajte načítaný data frame s pôvodným *dat*. 
#'  


