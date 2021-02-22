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
#' # Príklad 1 (súčet)
#' 
#' **Zadanie:** 1. Načítajte súbor údajov *mtcars* z balíka *datasets* a uložte ho do premennej s názvom **dat**.
#' 
#' **Riešenie:** 
dat <- data("mtcars")






#' 
#' # Príklad 2 (sinus)
#' 
#' **Zadanie:** Zobrazte sin(x) v intervale $x\in(-\pi,\pi)$
#' 
#' **Riešenie:**
seq(from = -pi, to = pi, length.out = 100) %>% 
  plot(sin(.), type = "l", col = "red")
