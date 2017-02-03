library(rpart)
tennis <- read.table("tennum.txt")
tennis
ad.tennis <- rpart (Jouer ~ Ciel + Temperature + Humidite + Vent, tennis)
ad.tennis

ad.tennis.cnt <- rpart.control (minsplit = 1)

ad.tennis <- rpart (Jouer ~ Ciel + Temperature + Humidite + Vent, tennis, control = ad.tennis.cnt)
ad.tennis

plot (ad.tennis)
text (ad.tennis)


plot (ad.tennis, uniform=T)
text (ad.tennis, use.n=T, all=T)
plot (ad.tennis, branch=0)
plot (ad.tennis, branch=.7)
text (ad.tennis, use.n=T)
plot (ad.tennis, branch=.4, uniform=T, compress=T)
text (ad.tennis, all=T,use.n=T)
plot (ad.tennis, branch=.2, uniform=T, compress=T, margin=.1)
text (ad.tennis, all=T, use.n=T, fancy=T)
