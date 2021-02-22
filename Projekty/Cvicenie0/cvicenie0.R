#' ---
#' title: "0 Úvod do R"
#' subtitle: "Štatistické metódy vyhodnocovania experimentov"
#' author: "William Brach"
#' date: "22.02.2021"
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
#' # Príklad 1 
#' 
#' **Zadanie:** Načítajte súbor údajov *mtcars* z balíka *datasets* a uložte ho do premennej s názvom **dat**.
#' 
#' **Riešenie:** 
data("mtcars")
dat <- mtcars



#' 
#' # Príklad 2 
#' 
#' **Zadanie:** Zobrazte štruktúru objektu *dat* a prvých 5 riadkov
#' 
#' **Riešenie:**
head(dat,5)
str(dat)

#' 
#' # Príklad 3 
#' 
#' **Zadanie:** Preveďte premennú *mpg* na jednotky km/l a uložte ako novú premennú *kml* do toho istého objektu.
#' 
#' **Riešenie:**
dat["kml"] <- dat["mpg"] * (1.609344	/ 3.785411784)

#' 
#' # Príklad 4
#' 
#' **Zadanie:** Vytvorte logický vektor *aut* indikujúci, či ide o auto s automatickou prevodovkou a pomocou neho vypočítajte priemerný dojazd (v km na 1l paliva) automobilov zvlášť s automatickou a zvlášť s manuálnou prevodovkou.
#' 
#' **Riešenie:**
automatic <- dat[dat$"am" == c(T),]
manual <- dat[dat$"am" == c(F),]

#' 
#' # Príklad 5
#' 
#' **Zadanie:** Zobrazte tabuľku všetkých áut s piatimi rýchlostnými stupňami a hmotnosťou do 3000 libier, ktorá obsahuje iba údaje o počte valcov, zdvihovom objeme a výkone motora.
#' 
#' **Riešenie:**
dat[dat$"gear" == 5 & dat$"wt" < 3.0,][c("cyl","disp","hp")]

#' 
#' # Príklad 6
#' 
#' **Zadanie:** Vytvorte funkciu na prevod jednotiek, ktorá bude mať 3 argumenty (s názvom)[s hodnotami]: prevádzanú hodnotu (x), imperiálnu jednotku (impunit)[míľa, galón, palec, libra], smer prevodu do SI (toSI)[TRUE,FALSE], pričom zodpovedajúcimi jednotkami v metrickej sústave SI budú km, l, dm, kg. (Využite pri tom funkciu *switch* a automatickú konverziu módu vektora *toSI* z logického na numerický.)
#' 
#' **Riešenie:**
chillin  <- function(x, impunit,toSI) {
  toSI_numeric <- sapply(toSI, is.logical)
  switch(impunit, 
         mile={
           if (toSI_numeric == c(1)){
             x <- x * 1.609344
           }else {
             x <- x / 1.609344
           }
         },
         galon={
           if (toSI_numeric == c(1)){
             x <- x / 0.26417
           }else {
             x <- x * 0.26417
           }
         },
         inch={
           if (toSI_numeric == c(1)){
             x <- x * 0.016387064
           }else {
             x <- x / 0.016387064
           }
         },
         libra={
           if (toSI_numeric == c(1)){
             x <- x / 3.1456560643963
           }else {
             x <- x * 3.1456560643963
           }
         },
         {
           print('default')
         }
  )
  return(x)
}
#' 
#' # Príklad 7
#' 
#' **Zadanie:** Pomocou *for* cyklu skonvertujte hodnoty zdvihového objemu valcov z kubických palcov na litre. Pomocou funkcie *sapply* preveďte hmotnosť vozidiel na tony. Zachovajte pri tom pôvodné názvy premenných a použite funkciu na prevod jednotiek z predošlej úlohy.
#' 
#' **Riešenie:**
data("mtcars")
df <- mtcars
disp <- df["disp"]
for (value in disp){
  kew <-chillin(x=value,impunit="inch",toSI=c(T))
}
df["disp"] <- kew

sapply(df["wt"],FUN=function(x2) chillin(x=x2,impunit="libra",toSI=c(T)))
