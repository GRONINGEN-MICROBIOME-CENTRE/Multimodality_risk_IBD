

GIEQfull$brestfeedever <- ifelse(GIEQfull$BIRTH151 == 1, 0, ifelse(GIEQfull$BIRTH151 == 9, "NA", 1))
table(GIEQfull$borstvoedingever, GIEQfull$BIRTH151)

GIEQfull$homesharingyesno <- ifelse(GIEQfull$Housemates == 0, 0, 1)
table(GIEQfull$homesharingyesno,GIEQfull$Housemates)

GIEQfull$urbanliving <- ifelse(GIEQfull$Woonplaats == 2, 1, 0)
table(GIEQfull$urbanliving,GIEQfull$Woonplaats)

env_score <- 0.74*GIEQfull$brestfeedever + 0.76*GIEQfull$PetsNow + 0.53*GIEQfull$homesharingyesno 
        + 0.66*GIEQfull$Bedpartner + 1.35*GIEQfull$urbanliving + 1.57*GIEQfull$SUPP91 + 1.31*GIEQfull$FEM81
  # variables not present: farm animals, access to personal toilet
  # many NA: bedpartner