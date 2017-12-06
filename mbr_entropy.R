
library(readr)

with_spaces <- FALSE

specials <- read_file("caractères_spéciaux")
nc <- nchar(specials)
sc <- strsplit(specials, "")
# rempli un tableau de nc fois la valeur 0
a <- 0.001
z <- rep(a, nc)
# constuit un tableau avec les valeurs de z et les noms issus des valeurs de sc
scp<-array(c(z),dimnames = sc)

dict <- "lettres_avec_espaces"

entropy_init <- function (dict, spaces=FALSE) {

  with_spaces <<- spaces
  
  string <<- read_file(dict)
  N <<- nchar(string)
  chars <<- strsplit(string, "")
  occurs <<- table(chars)
  probs <<- occurs / N
  probs_c <<- append(probs, scp)

}

entropy <- function (txt) {
  
  print(paste("Texte d'entrée:", txt))

  if (!with_spaces) {
    print("Suppression des espaces du texte d'entrée...")
    txt <- gsub("[:space:]+", "", txt)
  }
  
  s <- strsplit(txt, "")

  print("Calcul de l'entropie de Shannon...")
  
  # L'entropie de Shannon est H = - somme de 1 à N de [ p * log2(p) ]
  # p étant la probabilité du caractère i
  
  # L'astuce consiste à "indicer" le tableau des probabilités avec le
  # tableau des caractères :
  # s est une liste, s[[1]] est un tableau
  # l'appel à probs_c[ s[[1]] ]
  # donne un tableau des probabilités des caractères dans le tableau s[[1]]
  
  h <- - sum( probs_c[ s[[1]] ] * log2( probs_c[ s[[1]] ]) )
  
  return (h)
}

# Exemple:
# entropy_init(dict)
#
#> entropy("coucou*")
#[1] "Suppression des espaces du texte d'entrée..."
#[1] "Calcul de l'entropie de Shannon..."
#[1] 1.091486
#[1] 1.091486
#> entropy("coucou")
#[1] "Suppression des espaces du texte d'entrée..."
#[1] "Calcul de l'entropie de Shannon..."
#[1] 0.759293
#[1] 0.759293

# $ pwgen 16
# ka9phielaethaeWe Fe5Doo0Fea7di1wa keu9Ahnei8ohw5fo ij2soChaza7eolie
# ievooSho4Ahch8ph bai3Phiexaeng2pe Taepahfiong9chaN Queengeixiu2ohch
# aghie4top4Ohpagh ChooBohbapiech9o Im1Ilae1pie2irae Meibiel9etie0koo
# hoKoozahkec3Ueva ucujax5feNie1ahj shoBoaqueyu3shuz fohh9jiM6giel5iX

# $ pwgen -cny 16
# queeghuTae9Yee!s doo5phee3Hei[sh6 iiG6me,p6uph3ieX io'thee4mauy9Pha
# eo[ng?aing*eeB8g chah[R0ie<J1Foeh ou}wie5boogiy2Ah ari9tez>iQu9aeya
# eJ0igoh}jid6aiji zerok.oTh1ieh1Ee Iep4sudeo~chuc5P Iej8lahK%onga:o2
# cae8ugoo{sh>oo7O rei8Ohav6the~i>R Eejaiqueiv6vohk" leu2aePoe9ohXep)

# $ pwgen -s 16
# EtgFrpqF26tI5vU6 bt2txyoBDUvrgFz4 ZAABAGE4c0tGePkN 8JrfmdAxN030nMVq
# EqzJdbdJ1aVkHi2s YvyWANIWO89LZ92A orknzkSoe9H4Jj6V bljtHwL8VzZxfQ08
# 47BBmp1NwnZ0SHdI 9halFQ41dmzhJtaI V7Xa5IpprcF29MFo AiVWjVfQp2IZgM6g
# 63bVfvcTo85zeB2c ga6CP0s85durWFO3 w3k1ZBmYF9KPHoRH Dkp2pBb0yXRsp63f

# entropy("*udqsklj&jkfé4334(t")
# 3.378129

# entropy("chaise humidité pipe réveillon")
# 4.832968