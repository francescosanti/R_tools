## Merging abundance releves in a single phytosociological (or presence/absence) table

# set your working directory
# setwd(...)


# importing a file containing different sheets with one (or more) releve(s) each
# in each sheet, the first column contains the taxa, the others the abundance (or presence/absence) values in each releve

library(readxl) # if you have an excel spreadsheet

tab <- list()

for(a in 1:31){
  tab[[a]] <- read_xlsx("Cartel1.xlsx", # if your file is not ".xlsx", use another function to import the file
                        col_names = F,  # if your spreadsheets have column names, put col_names = T
                        sheet = a) 
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
