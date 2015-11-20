library(reshape2)
library(data.table)

setwd("C:\\Users\\Hernan\\Desktop\\Elecciones 2015")

files <- list.files(pattern = "\\.txt$")

filescandidatura <- grep("^(MesasCandidatura)", files, value = T)

tablescandidatura <- lapply(filescandidatura, read.csv2, header = T)

for(i in 1:length(tablescandidatura)){
 
  tablescandidatura[[i]][["vot_mesCodSexo"]] <- NULL
  tablescandidatura[[i]][["votOrdenActa"]] <- NULL
  tablescandidatura[[i]][["ordenPartidos"]] <- NULL
}

 
fla <- paste0(names(tablescandidatura[[1]])[1:5], collapse = "+")
fla <- as.formula(paste(fla, names(tablescandidatura[[1]])[6], sep = "~"))

casted.tables <- lapply(tablescandidatura, function(x) dcast(x, fla, value.var = "votVotosPartido"))

#Armo los porcentuales de los votos

for(i in 1:length(casted.tables)){
  
  partidos.vars <- grep("^[0-9]+$", names(casted.tables[[i]]), value = T)
  total.partidos <- apply(casted.tables[[i]][,partidos.vars],1,sum,na.rm  =T)
  casted.tables[[i]]["totalpartidos"] <- total.partidos
  partidos.porc.vars <- paste(partidos.vars,"porc", sep = ".")
  casted.tables[[i]][,partidos.porc.vars] <- round(casted.tables[[i]][,partidos.vars]/total.partidos,4)
}



#Ponerle el nombre de lo que se elige

for(i in 1:length(casted.tables)){
  
  puesto <- gsub("MesasCandidatura","",filescandidatura[i])
  puesto <- gsub("\\.txt","",puesto)
  
  names(casted.tables[[i]])[grep("(^[0-9]+)|(^totalpartidos$)", names(casted.tables[[i]]))] <- paste(puesto,grep("(^[0-9]+)|(^totalpartidos$)", names(casted.tables[[i]]), value = T), sep = "_")
  
}

# Levanto los totales

filesmesas <- files[!grepl("Candidatura", files)]
tablesmesas <- lapply(filesmesas, read.csv2, header = T)

#Le saco porqueria al dataset

for(i in 1:length(tablesmesas)){
  
  tablesmesas[[i]][["mesCodSexo"]] <- NULL
  tablesmesas[[i]][["mesVotosEscrutar"]] <- NULL
  tablesmesas[[i]][["mesVotosPreferencia"]] <- NULL
  tablesmesas[[i]][["mesVotosWarning_XP"]] <- NULL
  tablesmesas[[i]][["mesVotosWarning_M3"]] <- NULL
  tablesmesas[[i]][["mesEstado"]] <- NULL
}

#Agrego vars a tablesmesas

for(i in 1:length(tablesmesas)){
 
  tablesmesas[[i]]["participacion"] <- round(tablesmesas[[i]]$mesTotalVotantes/tablesmesas[[i]]$mesElectores,4)
  tablesmesas[[i]]["positivo.ratio"] <- round(tablesmesas[[i]]$mesVotosPositivos/tablesmesas[[i]]$mesTotalVotantes,4)
  tablesmesas[[i]]["blanco.ratio"] <- round(tablesmesas[[i]]$mesVotosEnBlanco/tablesmesas[[i]]$mesTotalVotantes,4)
  
}


#Le cambio los nombres como antes


for(i in 1:length(tablesmesas)){
  
  puesto <- gsub("Mesas","",filesmesas[i])
  puesto <- gsub("\\.txt","",puesto)
  
  names(tablesmesas[[i]])[6:ncol(tablesmesas[[i]])] <- paste(puesto,
        names(tablesmesas[[i]])[6:ncol(tablesmesas[[i]])], sep = "_")
  
}



#Armo las keys

casted.tables.keys <- names(casted.tables[[1]])[1:5]
tablesmesas.keys <- names(tablesmesas[[1]])[1:5]

#Joineo la data

mesas.joined <- lapply(1:length(casted.tables), function(x) 
                merge(casted.tables[[x]], tablesmesas[[x]], 
                      by.x = casted.tables.keys,
                      by.y = tablesmesas.keys))


common.key <- Reduce("intersect", lapply(mesas.joined, names))

one.table <- Reduce(function(...) merge(...,all=T,by=common.key),mesas.joined)


rm(list=setdiff(ls(),"one.table"))

#Check if participation values match and leave one

participacion.vars <- grep("participacion", names(one.table), value = T)

is.equal <- apply(one.table[,participacion.vars],1, function(x) all(x[!is.na(x)] == x[!is.na(x)][1]))

all(is.equal)

president <- grep("Presidente",participacion.vars)

keep.vars <- setdiff(names(one.table),participacion.vars[-president])

one.table <- one.table[,keep.vars]

names(one.table)[grep(participacion.vars[president],names(one.table))] <- "Participacion"

#Check if electors values match and leave one

electors.vars <- grep("mesElectores", names(one.table), value = T)

is.equal <- apply(one.table[,electors.vars],1, function(x) all(x[!is.na(x)] == x[!is.na(x)][1]))

all(is.equal)

president <- grep("Presidente",electors.vars)

keep.vars <- setdiff(names(one.table),electors.vars[-president])

one.table <- one.table[,keep.vars]

names(one.table)[grep(electors.vars[president],names(one.table))] <- "Electores"

#Check if voters values match and leave one

voters.vars <- grep("mesTotalVotantes", names(one.table), value = T)

is.equal <- apply(one.table[,voters.vars],1, function(x) all(x[!is.na(x)] == x[!is.na(x)][1]))

all(is.equal)

president <- grep("Presidente",voters.vars)

keep.vars <- setdiff(names(one.table),voters.vars[-president])

one.table <- one.table[,keep.vars]

names(one.table)[grep(voters.vars[president],names(one.table))] <- "TotalVotantes"


#Order Vars

vars.order <- setdiff(names(one.table),c("Electores","TotalVotantes","Participacion"))

vars.order <- c(vars.order[1:5],c("Electores","TotalVotantes","Participacion"),vars.order[6:length(vars.order)])

one.table <- one.table[,vars.order]

#Agregar nombre provincia

provinces <- readLines("provincias.txt")

one.table$vot_proCodigoProvincia <- as.factor(one.table$vot_proCodigoProvincia)
levels(one.table$vot_proCodigoProvincia) <- provinces

save(one.table, file = "table processed.rda")
