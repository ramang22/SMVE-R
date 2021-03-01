#' ---
#' title: "Štatistické metódy vyhodnocovania experimentov"
#' author: "Tomáš Bacigál"
#' date: "23.2.2021"
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 4
#'      toc_float: 
#'        collapsed: false
#'        smooth_scroll: false 
#'      number_sections: true
#' ---
#+ echo=F
knitr::opts_chunk$set(collapse = TRUE) 
#' 
#' # Základné štatistické metódy
#' 

# ---- rozdelenie pravdepodobnosti ---- 
#' 
#' ## Náhodná premenná, rozdelenie pravdepodobnosti 
#' 
#' Náhodná premenná je funkcia, ktorá náhodnej udalosti priradí číselnú alebo inú formálnu hodnotu. Väčšinou sa v inžinierskej praxi zaoberáme tými číselnými (=numerickými, kvantitatívnymi) náhodnými premennými (NP), neskôr si ukážeme, že sa pri riešení mnohých problémov nevyhneme ani tým nenumerickým (=nominálnym, kvalitatívnym) NP.   
#' Každá hodnota NP môže nastať s nejakou pravdepodobnosťou, napr. ak náhodného okoloidúceho vezmeme za štatistickú jednotku (na ktorej vykonám pozorovanie) tak napr. jeho výška je náhodná udalosť, ktorá môže byť formálne zobrazená numerickou NP ako výška v cm, ktorá formálne nadobúda hodnoty z R₊, reálne iba z intervalu povedzme (30, 250), ale tiež môže byť zobrazená nominálnou náhodnou premennou s troma hodnotami: "nízky", "stredný", "vysoký". Pravdepodobnosť, že okoloidúci bude vysoký je intuitívne nižšia, než že bude strednej výšky, podobne pravdepodobnosť 170cm človeka je vyššia ako 200cm človeka.   
#' Súbor pravdepodobností pre všetky hodnoty NP sa nazýva rozdelenie pravdepodobnosti a plne charakterizuje náhodnú premennú, či už je dané pravdepodobnostnou funkciou, hustotou pravdepodobnosti alebo distribučnou funkciou. Ak poznáme rozdelenie pravdepodobnosti, dokážeme vypočítať akékoľvek charakteristiky NP. Podľa oboru hodnôt náhodnej premennej aj rozdelenia rozlišujeme diskrétne a spojité. 

#'
#' ### Najbežnejšie diskrétne rozdelenia pravdepodobnosti
#' 
#' #### Binomické rozdelenie
#' 
#' Je to rozdelenie NP, ktorá predstavuje napr. počet úspechov v sérii n pokusov.   
#' 
#' parametre rozdelenia:
n <- 20  # pocet pokusov
p <- 0.25  # pravdepodobnost udalosti v jednom pokuse
#' graf pravdepodobnostnej funkcie:
x <- 0:n
plot(x, 
     y = dbinom(x, size = n, prob =p),
     xlab = "pocet uspesnych pokusov", 
     ylab = "pravdepodobnost(na funkcia)", 
     main="Binomicke rozdelenie, n=20, p=0.25"
)
#' charakteristiky NP:
n*p  # stredna hodnota, E(X)
n*p*(1-p)  # disperzia, D(X)
#' ilustračné  porovnanie teoretickej a výberovej charaktersitiky:
qbinom(0.5, size=n, prob=p)  # teoreticky median
y <- rbinom(30, size=n, prob=p)  # nahodny vyber z binomickeho rozdelenia
median(y)  # vyberovy median
#' výpočet pravdepodobnosti náhodnej udalosti pomocou rozdelenia zodpovedajúcej NP:
pbinom(3,n,p)  # pravdepodobnosť úspechu najviac v 3 pokusoch
1-pbinom(9,n,p)  # pravdepodobnosť uspechu v aspon 10 pokusoch
1-pbinom(10,n,p)  # pravdepodobnosť úspechu vo viac ako 10 pokusoch
qbinom(0.90, n, p) # nanajvýš koľko úspešných pokusov možno očakávať s 90% pravdepodobnosťou

#' ##### Cvičenie
#' 
#' Banka uvádza, že 7% držiteľov debetných kariet niekedy počas obdobia, v ktorom kartu používajú, využije službu zablokovania karty. Ak vyberieme vzorku 12 držiteľov karty, aká je pravdepodobnosť, že aspoň jeden z nich túto službu využije?   Ak vyberieme vzorku 12 držiteľov karty, aká je pravdepodobnosť, že práve 8 z nich túto službu využije? 
#' 
1-pbinom(0, size = 12, prob =0.07) # nenastane ani jedna
dbinom(8, 12, 0.07) # prave 8


#' 
#' #### Hypergeometrické rozdelenie
#' 
#' Ak náhodne vyberáme (a už nevrátime) _n_ prvkov zo základného súboru, ktorý celkovo obsahuje N prvkov, ale iba M s určitou vlastnosťou, potom _počet prvkov s touto vlastnosťou v našom výbere_ (náhodná premenná X) bude mať hypergeometrické rozdelenie. 
#' X ~ Hg(N, M, n),  M<N, n<N, X<=n  
#' E(X) = n M/N, D(X) = n M/N (1-M/N) (N-n)/(N-1)  
#' Jeho limitným prípadom je binomické rozdelenie, a to práve vtedy, keď x->infty a M/N->0, pretože sa stráca rozdiel medzi výberom s vrátením a výberom bez vrátenia, takže Hg(N, M, n) $\approx$ Bi(n, p=M/N).  
#' 
#' parametre rozdelenia:
n <- 20  # počet pokusov
N <- 100  # rozsah základného súboru
M <- 25  # počet priaznivých prvkov v základnom súbore
#' graf pravdepodobnostnej funkcie:
x <- 0:n
plot(x, 
     y = dhyper(x, m = M, n = N-M, k = n),
     xlab = "pocet uspesnych pokusov", 
     ylab = "pravdepodobnostná funkcia", 
     main="Hypergeometrické rozdelenie Hg(N=100, M=25, k=20)"
)
#' charakteristiky NP:
n*M/N  # stredna hodnota, E(X)
n*M/N*(1-M/N)*(N-n)/(N-1)  # disperzia, D(X)

#' ##### Cvičenie
#' 
#' Študent sa naučil iba 5 otázok z 20, z ktorých na skúške dostane náhodne 3. Aká je pravdepodobnosť, že bude vedieť aspoň 2 z nich a skúškou prejde?
n <- 3 # kolko bude pokusov
N <- 20 #kolko otazok
M <- 5 # kolko vieme otazok
1 - phyper(1, m = M, n = N-M, k = n )
phyper(1, m = M, n = N-M, k = n, lower.tail = F ) # pravdepobonost prezitia, survive

#' 
#' #### Poissonovo rozdelenie
#' 
#' Je aproximáciou Binomickeho rozdelenia pre malé p a veľké n, pričom parameter rozdelenia je lambda = n*p. 

lambda <- 5

x <- 0:30
plot(x, 
     y = dpois(x, lambda),   
     xlab = "pocet uspesnych pokusov", 
     ylab = "pravdepodobnost(na funkcia)", 
     main=paste("Poissonovo rozdelenie, lambda = ",lambda)
)

#' ##### Príklad 
#' 
#' Podla štatistických tabuliek pravdepodobnosť dožitia sa nasledujúceho roku u 25-ročného muža je približne 0.998. Poisťovňa ponúka mužom tohoto veku, ze pri ročnom poistnom 50 EUR vyplatí pozostalým v prípade úmrtia poisteného sumu 8000 EUR. Je poistených tisíc 25-ročných mužov. Aká je pravdepodobnosť, ze ku koncu roka bude zisk poisťovne aspon 30 000 EUR?
#' Riešenie: Nech X je nah.premenná počet úmrtí poistencov a Z náhodna premenná  zisk poistovne. Potom platí Z = 50*1000 - 8000*X. Hladáme pravdepodobnosť P(Z >= 30 000) = P(50 000 - 8 000*X >= 30 000) = P(X <= 2.5) = F(2.5) = 
ppois(2.5, lambda=1000*(1-0.998))
#' kedže X má Poisonovo rozdelenie s parametrom lambda=1000*(1-0.998).
#' 

#' ##### Cvičenie
#' 
#' V pristroji je 2000 suciastok. Pravdepodobnost zlyhania jednej je 0.005. Nahodna premenna X je pocet zlyhanych suciastok, X ma Poisonovo rozdelenie pravdepodobnosti.
#' 
#' 1. Vypocitajte parameter Poisonovho rozdelenia a vygenerujte z neho 20 nahodnych cisel. 
#' 
#'
dat <- rpois(n=20, lambda=2000*0.005)
dat
#' 2. Vypocitajte vyberovy median a porovnajte ho s teoretickym medianom. 
#' 
median(dat)
qpois(0.5,lambda=2000*0.005)
#' 3. Aka je pravdepod. ze zlyha viac ako 10 suciastok?
#' 
1 - ppois(10, lambda=2000*0.005)


#' ##### Cvičenie
#' 
#' V banke vybavia priemerne 72 zákazníkov za 1 hod. Aká je pravdepodobnosť p, že 
#' 
#'  1. vybavia 4 zákazníkov za 3 minúty? 
#'  2. vybavia viac ako 4 zákazníkov za 3 min?
lambda <- 3*72/60
dpois(4,lambda)
1 - ppois(4, lambda)
#'  
#' ### Najbežnejšie spojité rozdelenia pravdepodobnosti
#' 
#' #### Normálne rozdelenie
#' 
#' Normálne rozdelenie je limitným prípadom binomického, keď n->infty.

p <- 0.25
n <- 20 
x <- 0:n
plot(x, 
     y = dbinom(x, size = n, prob =p), 
     ylab = "pravdepodobnost(na funkcia)", 
     main=paste("Binomicke rozdelenie n=", n, ", p=", p)
)
plot(function(x) dnorm(x, mean=n*p, sd=sqrt(n*p*(1-p))), from=min(x), to=max(x), add=T)
legend("topright", paste("N(",n*p,",",round(sqrt(n*p*(1-p)),2),")"), lty=1)


n <- 100 
x <- 0:n
mu <- n*p
sig <- sqrt(n*p*(1-p))
plot(x, 
     y = dbinom(x, size = n, prob =p), 
     ylab = "pravdepodobnost(na funkcia)", 
     main=paste("Binomicke rozdelenie n=", n, ", p=", p)
)
plot(function(x) dnorm(x, mean=mu, sd=sig), from=min(x), to=max(x), add=T)
legend("topright", paste("N(",mu,",",round(sig,2),")"), lty=1)

#' V štatistike sa často používa normované normálne rozdelenie. To je rozdelenie náhodnej premennej Z=(X-E[X])/SD(X), teda Z~N(0,1) ak X~N(E(X),SD(X)).
plot(function(x) dnorm(x, mean=mu, sd=sig),
     xlim=c(-20,50), ylim=c(0,0.2),
     ylab = "hustota pravepodobnosti", 
     main=paste("Normálne rozdelenie mean=", mu, ", sd=", round(sig,2))
)
plot(function(x) dnorm(x, mean=0, sd=sig ), 
     from=-20, to=50, col="blue", add=T)
plot(function(x) dnorm(x, mean=0, sd=1), 
     from=-20, to=50, col="red", add=T)
legend("topright", legend=c(paste("N(0,",round(sig,2),")"), "N(0,1)"), 
       col=c("blue", "red"), lty=c(1,1))

#' Ak majú dve nezávislé náhodné premenné X1 a X2 normálne rozdelenie, X1~N(m1,s1) a X2~N(m2,s2), potom ich lineárna kombinácia Y = a*X1 + b*X2 + c je tiez nahodna premenna s normalnym rozdelenim, Y~N(a*m1+b*m2+c, a^2 * s1^2 + b^2 * s2^2).  
#' Zachovanie triedy rozdelenia po lineárnej transformácii/kombinácii jednej/viacerých náhodných premenných je špeciálna vlastnosť (nie výlučne) normálneho rozdelenia.
#' 

#' #### log-normálne rozdelenie
#' 
#' Ak X~N(m,s), potom náhodná premenná Y = e^X má LN(m,s).
#' 
plot(function(x) dnorm(x, mean=0, sd=1 ), 
     xlim = c(-3,5), ylim = c(0,0.7),
     ylab = "hustota pravepodobnosti",
     xlab = "X, Y",
     main="Normálne a log-normálne rozdelenie"
)
plot(function(x) dlnorm(x, meanlog=0, sdlog=1), 
     from=-3, to=5, col="red", add=T)
legend("topright", legend=c("N(0,1)","LN(0,1)"), 
       col=c("black", "red"), lty=c(1,1))
#' Veľa náhodných veličín v prírode má rozdelenie blízke lognormálnemu (definované na kladnej reálnej osi), ich logaritmovaním môžme na analýzu/modelovanie použiť množstvo štatistických metód vyžadujúcich normalitu náhodných premenných (napr. t-test, regresia).

#' 
#' #### Chi-kvadrat rozdelenie
#' 
#' Ak X1, X2 ... Xn maju normované normálne rozdelenie N(0,1), potom náhodná premenná Y = X1^2 + X2^2 + ... Xn^2 má Chi^2(n) rozdelenie s n-stupňami voľnosti.
#' E(Y) = n, D(Y) = 2n
#' 
plot(function(x) dnorm(x, mean=0, sd=1 ), 
     xlim = c(-3,5), ylim = c(0,0.6),
     ylab = "hustota pravepodobnosti",
     xlab = "X1,...,Xn, Y",
     main="Normálne a chi-kvadrát rozdelenie"
)
plot(function(x) dchisq(x, df=2), 
     from=-3, to=5, col="blue", add=T)
plot(function(x) dchisq(x, df=3), 
     from=-3, to=5, col="red", add=T)
legend("topright", legend=c("N(0,1)", expression(paste(chi^2,"(2)")), expression(paste(chi^2,"(3)"))), 
       col=c("black", "blue","red"), lty=c(1,1,1))

#' #### Studentovo t rozdelenie
#' 
#' Ak X1~N(0,1) a X2~Chi^2(n), potom Y = X1/sqrt(X2/n) ma t(n) rozdelenie s _n_ stupňami voľnosti.
#' E(Y) = 0, D(Y) = n/(n-2)
#' 
plot(function(x) dnorm(x, mean=0, sd=1),
     xlim=c(-5,5),
     ylab = "hustota pravepodobnosti", 
     main="Normované normálne a t-rozdelenie"
)
plot(function(x) dt(x, df=2), 
     from=-5, to=5, col="blue", add=T
)
plot(function(x) dt(x, df=20), 
     from=-5, to=5, col="red", add=T
)
legend("topright", legend=c("N(0,1)", "t(df=2)", "t(df=20)"), 
       col=c("black", "blue", "red"), lty=c(1,1,1))


#' #### Fisher-Snedecorovo F-rozdelenie
#' 
#' Ak X1~Chi^2(m) a X2~Chi^2(n), potom Y = (X1/m)/(X2/n) ma F(m,n) rozdelenie s m a n stupnami volnosti.
#' E(Y) = 0, D(Y) = n/(n-2)
#' 
plot(function(x) dchisq(x, df=3),
     xlim=c(0,5), ylim=c(0,0.7),
     ylab = "hustota pravepodobnosti", 
     main="Chi-kvadrat a F-rozdelenie"
)
plot(function(x) df(x, df1=3,df=5), 
     xlim=c(0,5), col="blue", add=T
)
plot(function(x) df(x, df1=5,df=3), 
     xlim=c(0,5), col="red", add=T
)
legend("topright", legend=c("Chi^2(3)", "F(3,5)", "F(5,3)"), 
       col=c("black", "blue", "red"), lty=c(1,1,1))


#' #### Exponenciálne rozdelenie
#' 
#' Napr. doba životnosti zariadenia alebo doba čakania zákazníkov v rade.
#' 
plot(function(x) dexp(x, rate=1),   # rate > 0  (lambda)
     xlim=c(0,3), ylim=c(0,2),
     ylab = "hustota pravepodobnosti", 
     main="Exponenciálne rozdelenie"
)
plot(function(x) dexp(x, rate=2), 
     xlim=c(0,5), col="blue", add=T
)
legend("topright", legend=c("Exp(1)", "Exp(2)"), 
       col=c("black", "blue"), lty=c(1,1))

#' stredná hodnota E(X) = 1/lambda
integrate(function(x) x*dexp(x, rate=2), -Inf, Inf)$value
#' medián  1/lambda * log(2)
qexp(0.5, rate=2)
#' disperzia D(X) = (1/lambda)^2
integrate(function(x) ((x-1/2)^2)*dexp(x, rate=2), -Inf, Inf)$value

#' Spolu s normálnym rozdelením a chi-kvadrát, ale aj binomickým (pre fixné n) a Poissonovým rozdelením patrí do tzv. exponenciálnej rodiny rozdelení. Ďalšími v rodine sú beta a gamma (zovšeobecnenie exponenciálneho) rozdelenie. Hustota má tvar f(x|par)=g(x)*h(par)*exp(a(x)*b(par)).
#' 

#' #### Weibulovo rozdelenie
#'
#' Zovšeobecňuje exponenciálne rozdelenie, keď "failure rate" zavisi od času:  
#' ak shape<1, failure rate sa s postupom času znižuje, keďže výrazne slabšie články systému zlyhali  
#' ak shape=1, model "bez pamäte" t.j. redukcia na exponenciálne rozdelenie  
#' ak shape>1, naopak, keď miera zlyhania sa postupom času zvyšuje
#' 
plot(function(x) dexp(x, rate=2),
     xlim=c(0,2), ylim=c(0,3.5),
     ylab = "hustota pravepodobnosti", 
     main="Weibullovo rozdelenie, scale=1/lambda=1/2"
)
plot(function(x) dweibull(x, shape=0.5, scale=1/2), 
     xlim=c(0,3), col="blue", add=T
)
plot(function(x)  dweibull(x, shape=1, scale=1/2), 
     xlim=c(0,5), col="black", add=T
)
plot(function(x)  dweibull(x, shape=1.5, scale=1/2), 
     xlim=c(0,5), col="red", add=T
)
legend("topright", legend=c("Weib(0.5,1/2)","Weib(1,1/2), Exp(2)", "Weib(1.5,1/2)"), 
       col=c("blue", "black", "red"), lty=c(1,1,1))
#' Používa sa na modelovanie rozdelennia NP: prežitie (poist.), výskyt prepätia v el.sieti, rýchlosť vetra, extrémne udalosti ako zrážky a prietoky (hydrol.)
#' Ďalšie rozdelenia definované na <0,inf) a používané napr. v hydrológii sú Gumbel, GEV, generalized Pareto...
#' 

#' ##### Cvičenie:
#' 
#' Dojazd auta (po najbližšiu poruchu) je náhodná premenná s exponenciálnym rozdelením a strednou hodnotou 5000 km. Na dobrodružnej ceste nás po prejdení 2000km čaká aj púšť s priemerom 1000km, kde niet žiadneho autoservisu. 
#' 
#' 1. Aká je pravdepodobnosť, že uviazneme práve na tej púšti?
pexp(3000, 1/5000) - pexp(2000, 1/5000)
#' 2. Koľko kilometrov bezporuchovej prevádzky nám auto "garantuje" so spoľahlivosťou 90%?
#' 3. Aké rozdelenie by ste zvolili, aby lepšie vystihovalo prirodzené starnutie vozidla?
#' 
#' Príklady zobrazenia plochy pod hustotou pravdepodobnosti:
#+ eval=F
x <- seq(0, 10000, length.out = 1000)
dens <- dexp(x, rate = 2e-04)
plot(x, pexp(x,1/5000))
RcmdrMisc::plotDistr(x, dens, regions=list(c(2000,3000)),
                     ylim=c(0,2e-04)
)
fastGraph::shadeDist(xshade=c(2000, 3000), ddist = "dexp", parm1 = 2e-04, lower.tail = F, xmin=0, xmax=10000)

ind <- which(x>=2000 & x<=3000)
plot(x, dens, type="l")
polygon(x = c(2000, x[ind], 3000), y = c(0, dens[ind], 0), col="lightblue")

library(ggplot2)
ggplot(data = data.frame(x,dens), mapping = aes(x=x, y=dens)) + 
  geom_line() + 
  geom_ribbon(mapping = aes(x=ifelse(x>2000 & x<3000, x, NA), ymin=0, ymax=dens), fill = "lightblue") + 
  ggtitle(paste("P(2000<x<3000) =", round(diff(pexp(c(2000,3000),2e-04)),digits=4)))
detach("package:ggplot2")

