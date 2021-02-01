setwd("~/Università/SGN/Tesi/Digitalizzazione rilievi")

library(readxl)
tab <- list()

for(a in 1:31){
tab[[a]] <- read_xlsx("Cartel1.xlsx", col_names = F, sheet = a)
colnames(tab[[a]]) <- c("Specie", 1:(dim(tab[[a]])[2]-1))
for(b in 1:(dim(tab[[a]])[2]-1)){
  tab[[a]][which(tab[[a]][,b+1]=="-"),b+1] <- 0 # provare con .... <- ""
  tab[[a]][which(tab[[a]][,b+1]=="+"),b+1] <- 0.7
}
}

nomi <- tab[[1]][,1]
for(a in 2:31){
  nomi <- rbind(nomi, tab[[a]][,1])
}
spp <- as.factor(nomi$Specie)
oo <- levels(spp)


library(stringr)
uuu <- word(string = oo, start = 1, end = 2)


dati <- matrix(nrow = nlevels(spp), ncol = (sum(as.integer(summary(tab)[,1])) - length(tab)))


steps <- vector(mode = "integer", length = (length(tab)+1))
steps[1] <- 1
steps[length(steps)] <- sum(as.integer(summary(tab)[,1])) - length(tab) + 1
for(a in 2:(length(steps)-1)){
  steps[a] <- steps[a-1] + as.integer(summary(tab)[a-1, 1]) - 1
}

for(a in 1:nlevels(spp)){
for(b in 1:length(tab)){
  if(any(tab[[b]][,1]==levels(spp)[a])==T){
    dati[a, steps[b]:(steps[b+1]-1)] <- as.character(tab[[b]][which(tab[[b]][,1]==levels(spp)[a]),2:(steps[b+1]-steps[b]+1)])
  }
  else{
    dati[a, steps[b]:(steps[b+1]-1)] <- 0
  }
}
}


# correzioni

for(a in 1:426){
  dati[a, which(dati[a,]==0.7)] <- "+"
}

for(a in 1:426){
  dati[a, which(dati[a,]==0)] <- ""
}




tab.finale <- data.frame(oo, uuu, dati)
write.csv(tab.finale, file = "Tab_unica.csv", quote = F)
