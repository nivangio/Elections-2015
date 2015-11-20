library(data.table)
library(cluster)

setwd("C:\\Users\\Hernan\\Desktop\\Elecciones 2015")

load("table processed.rda")

reference.vars <- names(one.table)[1:5]

#Drop less than 100 cases tables

one.table <- subset(one.table, Electores > 50)

# See what are the outliers with 0 Voters

one.table$zerovoters <- ifelse(one.table$TotalVotantes == 0,"Yes","No")

#See if there is significance of zerovoters per Province
  
zerovoters.per.province <- with(one.table, table(vot_proCodigoProvincia,zerovoters))

totals <- table(one.table$zerovoters)

prop.tests <- apply(zerovoters.per.province,1,function(x) prop.test(c(x["Yes"],sum(x)), c(totals["Yes"], nrow(one.table)), alternative = "g"))

significant <- which(sapply(prop.tests, "[[", "p.value") < 0.05)

significant.provinces <- names(significant)

#See if significant provinces have significat departments

zerovoters.province.sset <- subset(one.table, vot_proCodigoProvincia %in% significant.provinces)

zerovoters.province.sset <- droplevels(zerovoters.province.sset)

zerovoters.province.sset_splitted <- split(zerovoters.province.sset,zerovoters.province.sset$vot_proCodigoProvincia)

zerovoters.department <- lapply(zerovoters.province.sset_splitted, function(x){
  
  zerovoters.per.department <- with(x, table(vot_depCodigoDepartamento,zerovoters))
  
  totals <- table(x$zerovoters)
  
  prop.tests <- apply(zerovoters.per.department,1,function(y) prop.test(c(y["Yes"],sum(y)), c(totals["Yes"], nrow(x)), alternative = "g"))
  
  significant <- which(sapply(prop.tests, "[[", "p.value") < 0.05)
  
  return(names(significant))
  
  
})


#### Analyze presidential results

#Get Totals

president.vars <- grep("Presidente_[0-9]+$",names(one.table), value = T)

president.am <-apply(one.table[,president.vars],2,sum,na.rm = T)
president.am <- round(president.am/sum(president.am),4)

#Prin comp

president.vars.porc <- grep("Presidente_[0-9]+\\.porc$",names(one.table), value = T)

presidential.set <- one.table[,c(reference.vars,president.vars.porc)]

drop <- apply(presidential.set[,president.vars.porc],1, function(x) all(is.na(x)))

presidential.set <- presidential.set[!drop,]

princomp.analysis <- princomp(presidential.set[,president.vars.porc], cor = T)

pca.scores.set <- cbind(presidential.set[,reference.vars],princomp.analysis$scores)

pca.scores.set <- data.table(pca.scores.set)

pca.vars <- grep("Comp", names(pca.scores.set), value = T)

means <- pca.scores.set[,lapply(.SD,mean),by="vot_proCodigoProvincia", .SDcols = pca.vars]

#### Analyze Buenos Aires ####

buenosaires.sset <- subset(one.table, vot_proCodigoProvincia == "Buenos Aires")

governor.vars.porc <- grep("Gobernador_[0-9]+\\.porc$",names(one.table), value = T)

buenosaires.sset <- buenosaires.sset[,c(reference.vars,president.vars.porc,governor.vars.porc)]

#Check which are not buenos aires governors

drop <- which(sapply(buenosaires.sset, function(x) all(is.na(x))))

buenosaires.sset <- buenosaires.sset[,-drop]

buenosaires.sset$vot_depCodigoDepartamento <- as.factor(buenosaires.sset$vot_depCodigoDepartamento)

#Add levels to department

dep.codes <- readLines("municipios BA.txt")
levels(buenosaires.sset$vot_depCodigoDepartamento) <- dep.codes


#Drop NAs

governor.vars.porc <- grep("Gobernador_[0-9]+\\.porc$",names(buenosaires.sset), value = T)
drop <- apply(buenosaires.sset[,c(governor.vars.porc,president.vars.porc)],1, function(x) any(is.na(x)))

buenosaires.sset <- buenosaires.sset[!drop,]

keep.vars <- c("Presidente_131.porc","Presidente_135.porc","Presidente_138.porc",
               "Gobernador_131.porc","Gobernador_145.porc", "Gobernador_138.porc")

buenosaires.sset_clus <- buenosaires.sset[,keep.vars]

clustering.ba <- clara(buenosaires.sset_clus,6)

export <- round(prop.table(table(buenosaires.sset$vot_depCodigoDepartamento, clustering.ba$clustering),1),4)

write.csv2(export, file ="tables department.csv")
