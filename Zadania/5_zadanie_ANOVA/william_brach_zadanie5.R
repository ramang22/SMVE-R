#' ---
#' title: "2. Popisná štatistika"
#' subtitle: "Štatistické metódy vyhodnocovania experimentov"
#' author: "William Brach"
#' date: "08.03.2021"
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
library(ggplot2)

#' 
#' # Príklad 1 (diéta)
#' 
#' **Zadanie:** Dietologická poradkyňa zostavila jedálniček a fitness plán pre 202 pacientov z vekovej kategórie 40-50 rokov, zvlášť pre ženy a zvlášť pre mužov. V súbore dieta.xlsx je zaznamenaná váha pacientov na začiatku programu a po trištvrte roku. Dietologička predpokladá, že muži stratia v priemere 15% váhy a ženy 13% váhy. Vhodným testom na danej hladine významnosti overte, či existuje štatisticky významný rozdiel medzi hodnotami pred a po programe bez ohľadu na pohlavie. Okrem toho testujte, či (k = 1) bol predpoklad dietologičky správny pre váhu mužov po diéte.
#' 
#' **Riešenie:** 
#' 
k <- 1
alpha <- 0.05
df <- read_excel("C://Users//ramang//Developer//SMVE-R//Zadania//5_zadanie_ANOVA//dieta.xlsx")

#' 
#' ### A
#' 

pred <- df$`Váha pred`
po <- df$`Váha po` 

sx2 <- var(pred)
sy2 <- var(po)
TS <- sx2/sy2
c(F_test_statistika = TS,
  krit_hodnota = qf(1-alpha, df1 = length(pred), df2 = length(po)),
  p_hodnota = pf(1/TS, df1 = length(pred), df2 = length(po)) + 1 - pf(TS, df1 = length(pred), df2 = length(po))
)

( sp2 <- (length(pred)-1*sx2+length(po)-1*sy2)/(length(pred)+length(po)-2) )  

var.test(pred,po)  

t.test(pred,po, alternative = "greater", var.equal = TRUE)

#' Na základe testu nezávislých pozorovaní pomocou testu variance môžeme tvrdiť že existuje štatisticky významný rozdiel medzi týmito dvoma hodnotami.


#' 
#' ### B
#' 
x <- df[df$Pohlavie != "F",]
xPred <- x$`Váha pred`
xPo <-  x$`Váha po`

t.test(xPred, xPo, paired=T)

#' Na základe vykonaného testu nemôžeme tvrdiť že tvrdenie poradkyňe je správne. Priemerná percento chudnutia u mužov je 12.93%. Avšak s 95% presnosťou vieme tvrdiť že toto percento sa pochybuje v rozmedzí 10.33305% až 15.53785%.

#'
#'  #  Príklad 2 (životnosť bateriek)
#' 
#' **Zadanie:** Na základe pozorovaní v baterky.xlsx na danej hladine významnosti rozhodnite, či je v životnosti bateriek (udávanej v dňoch) medzi výrobcami nejaký štatisticky významný rozdiel. Ak áno, post hoc testami nájdite, medzi ktorými, a dajte odporúčanie.
#' 
#' **Riešenie:**
#' 
#' 

k <- 1
alpha <- 0.05
df <- read_excel("C://Users//ramang//Developer//SMVE-R//Zadania//5_zadanie_ANOVA//baterky.xlsx")

dat <- stack(list(A=df$A, B=df$B, C=df$C, D=df$D))
names(dat) <- c("hodnota", "typ")

bartlett.test(hodnota ~ typ, dat)

oneway.test(hodnota ~ typ, dat, var.equal=TRUE)

ANOVA <- aov(hodnota ~ typ, dat)
summary(ANOVA)

kruskal.test(hodnota ~ typ, dat)

#' Vsetky testy zamietli nezávislosť životnosti od výrobcu.

tapply(dat$hodnota, INDEX=dat$typ, FUN=mean) 

TukeyHSD(ANOVA)

plot( TukeyHSD(ANOVA) ) 
#' Pomocou post hoc testom sme zistili ze najviac rozdielni vyrobcovia su vyrobca D a C. Pritom podla mean hodnoty ma najlepsie vysledky vyrobca D. 


#' 
#' # Príklad 3 (príjem)
#' 
#' **Zadanie:** Súbor údajov prijem.xlsx obsahuje záznamy o 256 osobách. Na danej hladine významnosti testujte, či majú na príjem štatisticky významný vplyv súčasne obe premenné vek a miesto. Pokiaľ áno, post hoc testom určite, ktoré úrovne faktorov sa významne odlišujú. Pokiaľ nie, popíšte, ako by ste to určili. Pohľadom na vhodný graf zhodnoťte, či je interakcia medzi faktormi badateľná. Potom jej významnosť otestujte a interpretujte.
#' 
#' **Riešenie:**
#' 

k <- 1
alpha <- 0.05
df <- read_excel("C://Users//ramang//Developer//SMVE-R//Zadania//5_zadanie_ANOVA//prijem.xlsx")



ANOVA <- aov(Prijem ~ (as.factor(Vek) + as.factor(Miesto)), df)
ANOVA %>% summary()
#' 
#' 
TukeyHSD(ANOVA)
ANOVA %>% TukeyHSD() %>% plot()
#' 
#' Podľa anova majú Vek a Miesto významný vlpyv na príjem.
#'

tmp <- df %>% 
  dplyr::group_by(Vek, Miesto) %>% 
  dplyr::summarize(Prijem = mean(Prijem))

ggplot(tmp, aes(x = Vek, y = Prijem, color = Miesto, group = Miesto)) + 
  geom_point() + geom_line()  

ggplot(tmp, aes(x = Miesto, y = Prijem, color = Vek, group = Vek)) + 
  geom_point() + geom_line()

#' Keď sa pozrieme na dané grafy vidíme že miesto má väčšiu váhu ako vek. Ľudia, ktorý pracujú vo väčších mestách majú väčšie príjmi 

ANOVA <- aov(Prijem ~ (as.factor(Vek) * as.factor(Miesto)), df)
ANOVA %>% summary()

ANOVA %>% summary() %>% getElement(1) %>% {sapply(row.names(.)[-4], function(x) c(
  eta2 = .[x,"Sum Sq"]/sum(.["Sum Sq"]),
  partial.eta2 = .[x,"Sum Sq"]/sum(.[c(x,"Residuals        "),"Sum Sq"])
))} %>% round(3) 
#' Tu vidíme že Miesto má aj väčšiu efektivitu na príjem ako Vek. Pomocou hodnoty partial eta.