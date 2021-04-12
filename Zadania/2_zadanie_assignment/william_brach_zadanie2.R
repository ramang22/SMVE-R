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

#' 
#' # Setup
#' 

# Nevedel som podla coho je poradove cislo v kruzku. Tak som to zobral abecedne, som 5 v abecede == 1. uloha
# nacitanie datasetu
df <- read_excel("C://Users//ramang//Developer//SMVE-R//Zadania//2_zadanie_assignment//data_popisna.xlsx")
# stale ked pracujem s datami si rad na zaciatku vypisem info o datasete
## Info o df
summary(df)
## Nazvy stlpcov
colnames(df)
## a este si rad vypisem prvych 10 prvkov nech viem ako vyzeraju data
head(df, n= 10)
## Skontrolovanie ci sa v datasete nachadzaju nejake NAN hodnoty
apply(df, 2, function(x) any(is.na(x)))
#' 
#' # Príklad 1 (iba príjem)
#' 
#' **Zadanie:** 1. Pomocou popisnej štatistiky vykonajte prvotnú analýzu pre premennú Príjem – bez ohľadu na ostatné premenné. Výsledky aj ich vhodné grafické znázornenie slovne popíšte.
#' 
#' **Riešenie:** 

# summary pekne vypise nasladujuce hodnoty Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
summary(df$Prijem)
# scatterplot rad pouzivam ako prvy aby som mal aspon trosku predstavu o rozmiestneni premennych. Neni prave najvhodnejsi pri jednej premmennej
plot(df$Prijem)
# hist nam krajsie ukaze co nam povedal scatterplot. Ze ktore Primy su najcastejsie, maju najvacsiu frekvenciu
hist(df$Prijem)
# box plot by nam mal ukazat vizualne co nam hovori summary. Pekne vidime ako su cele data rozdele plus, vidime aj jedneho outlayera co v tomto pripade bude maximum.
boxplot(df$Prijem, horizontal = T)

#' 
#' #  Príklad 2 (príjem ~ vzdelanie)
#' 
#' **Zadanie:** Vykonajte prvotnú štatistickú analýzu pre premennú Príjem vzhľadom na dané dve úrovne Vzdelania, popíšte premennú príjem pre každú úroveň zvlášť. Výsledky aj ich vhodné grafické znázornenie slovne popíšte. Porovnajte príjem vzhľadom na dané dve úrovne (kategórie) vzdelania.
#' 
#' **Riešenie:**
#' 
#' 


# summary pre jednotlive urovne vzdelania
summary(df[df$Vzdelanie == 2, "Prijem"])
summary(df[df$Vzdelanie == 3, "Prijem"])
# Vidime ze hodnota prijmu kladne koreluje s hodnotou vzdelania. Cim vacsia dosiahnuta uroven vzdelania tym je vacsi prijem.

# nachadza sa v nej 58 zaznamov
prijem2 <- data.frame(df[df$Vzdelanie == 2, "Prijem"])
# nachadza sa v nej 81 zaznamov
prijem3 <-  data.frame(df[df$Vzdelanie == 3, "Prijem"])

# hist pre hodnotu vzdelania 2
hist(prijem2[['Prijem']] )
# hist pre hodnotu vzdelania 2
hist(prijem3[['Prijem']] )

# porovnanie prijem2 a prijem3 cez boxplot, tu vidime ako sa meni prijem podla ohranicenia boxplotu
boxplot(prijem2[['Prijem']],prijem3[['Prijem']])



#' 
#' # Príklad 3 (príjem ~ vek | vzdelanie | miesto | sektor)
#' 
#' **Zadanie:** Graficky porovnajte príjem vzhľadom na rôzne úrovne (kategórie) premennej
#' 
#' **Riešenie:**
#' 

# toto trosku vyzera ako jednotlive boxploty
plot( df$Prijem ~ df$Vzdelanie,
      col = as.integer(as.factor(df$Vzdelanie))
)

# toto je asi krajsie vykresleny predchadzajuci graf
# vidime v nom ze priemerny plat rastie od 1-3 a potom je priblizne rovnaky v 3-4-5
prijem1 <- data.frame(df[df$Vzdelanie == 1, "Prijem"])
prijem2 <-  data.frame(df[df$Vzdelanie == 2, "Prijem"])
prijem3 <- data.frame(df[df$Vzdelanie == 3, "Prijem"])
prijem4 <-  data.frame(df[df$Vzdelanie == 4, "Prijem"])
prijem5 <- data.frame(df[df$Vzdelanie == 5, "Prijem"])
boxplot(prijem1[['Prijem']],prijem2[['Prijem']],prijem3[['Prijem']],prijem4[['Prijem']],prijem5[['Prijem']])


#' 
#' # Príklad 5 (početnosť)
#' 
#' **Zadanie:** Zostrojte tabuľku početností, relatívnych početností, koláčový graf a stĺpcový graf, ktoré budú podávať informáciu o počte respondentov v rámci jednotlivých úrovní (kategórií) premennej
#' 
#' **Riešenie:**
#' 

# tabuľku početností
table(df$Miesto) 
# tabuľka relatívnych početností tabulka pocetnosti - pocet vsetkych prvkov
table(df$Miesto) - c(length(df[[1]]))
# kolacovy graf
pie(table(df$Miesto))
# stlpcovy graf
barplot(table(df$Miesto))
# tu som skusal vykreslit tabulku pocestnosti pomoocu aggregate
aggregate(df$Respondent , by=list(Category=df$Miesto), FUN = length) 
