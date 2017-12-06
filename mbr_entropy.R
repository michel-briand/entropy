
library(readr)

with_spaces <- FALSE

specials <- read_file("caractères_spéciaux")
nc <- nchar(specials)
sc <- strsplit(specials, "")
# rempli un tableau de nc fois la valeur 0
a <- 0.1
z <- rep(a, nc)
# constuit un tableau avec les valeurs de z et les noms issus des valeurs de sc
scp<-array(c(z),dimnames = sc)

dict <- "lettres_avec_espaces"

entropy_init <- function (dict, spaces=FALSE) {

  with_spaces <<- spaces
  
  #string <<- scan(dict, character(), quote = "", sep = NULL)
  string <<- read_file(dict)
  N <<- nchar(string)
  chars <<- strsplit(string, "")
  occurs <<- table(chars)
  probs <<- occurs / N
  probs_c <<- append(probs, scp)

}

entropy <- function (txt) {

  H <- 0

  if (!with_spaces) {
    print("Suppression des espaces du texte d'entrée...")
    txt <- gsub("[:space:]+", "", txt)
  }
  
  s <- strsplit(txt, "")

  #n <- names(probs)
  
  print("Calcul de l'entropie de Shannon...")
  
  h <- - sum( probs_c[ s[[1]] ] * log2( probs_c[ s[[1]] ]) )
  
  print(h)
  
  return (h)
}

#sum(-(probs[s[[1]]]/log2(probs[s[[1]]])))
