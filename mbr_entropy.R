library(readr)

mbr_e_initialized <- FALSE
mbr_e_with_spaces <- FALSE

dict<-"~/conf/git/entropy/lettres_avec_espaces"

mbr_entropy_init <- function (dict, with_spaces) {

  #string <<- scan(dict, character(), quote = "", sep = NULL)
  string <<- read_file(dict)
  N <<- nchar(string)
  chars <<- strsplit(string, "")
  occurs <<- table(chars)
  probs <<- occurs / N
  mbr_e_initialized <<- TRUE
  mbr_e_with_spaces <<- with_spaces

}

#mbr_entropy <- function (txt) {

  H <- 0

  #if (!mbr_e_with_spaces) {
  #  txt <- gsub("[:space:]+", "", txt)
  #}
  txt<-"un texte"
  print(paste("Texte d'entrée : ",txt))

  s <- strsplit(txt, "")

  print(paste("Splited : ",s))

  for (i in s) {

    print(paste("Lettre :", i))

    if (!i %in% probs) {
      p <- probs[i]
      print(paste("Lettre : ", i, " de probabilité : ", p))
      print(H)
      H <- H + p * log2(p)
      print(H)
    }
  }

  return(H)
#}
