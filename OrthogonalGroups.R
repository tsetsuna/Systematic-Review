library(dplyr)
library(ggplot2)
#library(MASS)
#library(ISLR)
require(cluster)
require(gridExtra)
library(splines)
library(gdata)
library(gtools)
library(stringr)
library(Rtsne)

#################################################################################
#Programa 0 - Identificar terminos ortogonales
#################################################################################

fileKeywords <- "C:\\Mente\\Doctorado\\Stage-2019\\SystematicMap\\finalkeywordsV5csv.csv"
keyw <- read.table(fileKeywords, sep= ",", na.strings = "", stringsAsFactors = T, header=T)
datosKeyword <-  data.frame(keyw)

summary(datosKeyword)

#datosKeyword %>% 
#  group_by(Dominio, individualword2) %>%
#  summarise(number = n())

groupbykey <- datosKeyword %>% 
     group_by(individualword2) %>%
     summarise(number = n())

groupbykey <- groupbykey[groupbykey$number>=2,]

#groupbydomain <- groupbykey %>% 
#  group_by(individualword2) %>%
#  summarise(number = n())

groupbydomain <- groupbykey[groupbykey$number>=2,]
wordstobeexcluded <- unlist(groupbydomain[,1])


datosKeyword2 <- datosKeyword[datosKeyword$individualword2 %in% wordstobeexcluded, ]
datosKeyword2

datosKeyword23 <- setdiff(datosKeyword, datosKeyword2)
dominioant <- ""
dominioact <- ""
query <- "" 
separador <- "'"
or <- " OR "

write.csv(datosKeyword23,'C:\\Mente\\Doctorado\\Stage-2019\\SystematicMap\\orthogonalgroupsfINAL.csv')

#################################################################################
#Programa 1 - Generacion de Consultas con terminos requeridos y no requeridos
#################################################################################

##################################################
#Useful functions
##################################################
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

averageTerms<- function(dataframe){
  groupbykey <- dataframe %>% 
    group_by(Dominio) %>%
    summarise(number = n())
  mean <- round(mean(unlist(groupbykey[,2])))
  return(mean)
}

randomTerms <- function(datosNR, domain, size, query,queryandword,queryName){
  datosNoReq <- datosNR
  datosNR<-subset(datosNR,Dominio==domain, select=c("individualword2"))
  tamanioDF <- nrow(datosNR)
  if(tamanioDF < size ){
    size <- tamanioDF
  }
  sampleNR <- sample(datosNR[,1],size)
  for(i in 1:length(sampleNR)){
    query<- paste0(query," OR ")
    keyword = toString(sampleNR[i])
    keywordNR <- paste0(separador,sampleNR[i])
    keywordNR <- paste0(keywordNR,separador)
    #register word in database
    
    qawtemp<-data.frame(query=c(queryName),domain=c(domain),keyword=c(keyword),required=c("no"))
    queryandword<-rbind(queryandword,qawtemp)
    query<- paste0(query,keywordNR)
  }
  
  newList <- list("tquery"=query, "tqueryandword"=queryandword)
  return(newList)
}




combinedDomains <- function(){
  #level1_terms <- c("Energy", "Decision Making", "Artificial Intelligence")
  level1_terms <- c("Energy", "Decision Making","Data Science")
  level2_terms <- c("Data Mining","Policy","Machine Learning","Multi Criteria Decision Making")
  #level2_terms <- c("Data Analytics", "Machine Learning", "Data Visualization","Data Mining","Policy","Data Science","Multi Criteria Decision Making")
  level3_terms <- c("Smart Grid","Big Data","Data Visualization","Data Analytics")
  firstlevelselection <- ""
  secondlevelselection <- ""
  functiontoexceute <- "subset(datosRequeridos,"
  queryL2<- ""
  queryL3<- ""
  queries <- vector()
  
  for(i in 1:length(level1_terms)){
    
    functiontoexceute <- paste0(functiontoexceute,"Dominio=='")
    functiontoexceute <- paste0(functiontoexceute,level1_terms[i])
    functiontoexceute <- paste0(functiontoexceute,"'")
    if(i == length(level1_terms )){
      firstlevelselection <- functiontoexceute
      functiontoexceute <- paste0(functiontoexceute,",select=c(1,2))")
    }
    else{
      functiontoexceute <- paste0(functiontoexceute," | ")
    }
    
  }
  
  #Queries de nivel 2
  for(i in 1:length(level2_terms)){
    termsL2 <- combinations(n=length(level2_terms), r=i, v=level2_terms)
    for (r in 1:nrow(termsL2)){
      for (c in 1:ncol(termsL2)){
        queryL2 <- paste0(queryL2," | Dominio=='")
        queryL2 <- paste0(queryL2,termsL2[r,c])
        queryL2 <- paste0(queryL2,"'")
      }
      queryL2 <- paste0(firstlevelselection,queryL2)
      if(r==nrow(termsL2)){
        secondlevelselection<- queryL2
      }
      queryL2 <- paste0(queryL2,",select=c(1,2))")
      queries<-c(queries,queryL2)
      queryL2<-""
    }   
    
  }
  
  
  #Queries de nivel 3
  for(i in 1:length(level3_terms)){
    termsL3 <- combinations(n=length(level3_terms), r=i, v=level3_terms)
    for (r in 1:nrow(termsL3)){
      for (c in 1:ncol(termsL3)){
        queryL3 <- paste0(queryL3," | Dominio=='")
        queryL3 <- paste0(queryL3,termsL3[r,c])
        queryL3 <- paste0(queryL3,"'")
      }
      queryL3 <- paste0(secondlevelselection,queryL3)
      queryL3 <- paste0(queryL3,",select=c(1,2))")
      #print("*********************************")
      queries<-c(queries,queryL3)
      #print(queryL3)
      #print("*********************************")
      queryL3<-""
    }   
    
  }
  return(queries) 
}

#Separate the words that include the following characters "&"
cleanData<- function(df,character){
  print(nrow(df))
  newdf <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),c("Dominio","individualword2"))
  print("newdf")
  print(newdf)
  rowsWithAnd <- dplyr::filter(df, grepl(character, individualword2))  
  print(rowsWithAnd)
  #print(rowsWithAnd)
  for(i in 1:nrow(rowsWithAnd)){
    word<- rowsWithAnd[i,c(2)]
    dominiotmp <-as.character(rowsWithAnd[i,c(1)])
    print("Dominio TM")
    print(dominiotmp)
    word <- as.character(word)
    print("word")
    print(word)
    wordlist <- strsplit(word,character)
    wordlist<- lapply(wordlist,trim)
    #Aqui un ciclo para crear un data frame con los nuevos datos
    print(wordlist)
    print(length(wordlist))
    for(j in 1:length(wordlist)){
      #nw<-wordlist[j]
      #print(nw)
      individualwordtmp<- wordlist[j]
      #temporalDf<- setNames(data.frame(matrix(ncol = 2, nrow = 0)),c("Dominio","individualword2"))
      temporalDf<-data.frame(Dominio=c(dominiotmp),individualword2=c(individualwordtmp))
      names(temporalDf)[1] <- "Dominio"
      names(temporalDf)[2] <- "individualword2"
      print("Temporal df")
      print(temporalDf)
      newdf<-rbind(newdf,temporalDf)
    }
    #df[df$individualword2,]
  }
  #print (nrow(newdf))
  names(df)[1] <- "Dominio"
  names(df)[2] <- "individualword2"
  print(colnames(df))
  print(colnames(newdf))
  #print(newdf)
  #rowsNoAND <- setdiff(df, newdf)
  rowsNoAND<- dplyr::filter(df, !grepl(character, individualword2))
  resultdf<- rbind(rowsNoAND,newdf)
  #print (nrow(rowsNoAND))  
  print(nrow(resultdf))
  return(resultdf)
}

#Delete the words that include the following characters "/"
deleteData<- function(df,character){
  rowsNoAND<- dplyr::filter(df, !grepl(character, individualword2))
  return(rowsNoAND)
}

#receive a list of words
averageWords<- function(df){
  #finalwordnumber <- data.frame()
  finalwordnumber <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),c("Dominio","individualword2","length"))
  names(finalwordnumber)[1] <- "Dominio"
  names(finalwordnumber)[2] <- "individualword2"
  names(finalwordnumber)[3] <- "length"
  
  for(i in 1:nrow(df)){
    word <- as.character(df[i,c(2)])
    domain <- df[i,c(1)]
    l<- lengths(gregexpr("\\W+", word)) + 1
    print("words in a sentence")
    print(l)
    wordnumberdf <- data.frame(Dominio=(domain),individualword2=c(word),length=c(l))
    names(wordnumberdf)[1] <- "Dominio"
    names(wordnumberdf)[2] <- "individualword2"
    names(wordnumberdf)[3] <- "length"
    finalwordnumber <- rbind(finalwordnumber,wordnumberdf)
    #wordnumber <- c(wordnumber,l)
    #wordnumber <- wordnumber.append(l)
  }
  #print(mean(wordnumber))
  return(finalwordnumber)
  
}

#Delete parenthesis from words
deleteParenthesis<- function(df){
  newdf <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),c("Dominio","individualword2"))
  #print("newdf")
  #print(newdf)
  rowsWithP <- dplyr::filter(df, grepl('\\(', individualword2)& grepl('\\)', individualword2))  
  #print(rowsWithAnd)
  #print(rowsWithAnd)
  for(i in 1:nrow(rowsWithP)){
    word<- as.character(rowsWithP[i,c(2)])
    word<- gsub("\\("," ",word)
    word<- gsub("\\)"," ",word)
    dominiotmp <-as.character(rowsWithP[i,c(1)])
    names(newdf)[1] <- "Dominio"
    names(newdf)[2] <- "individualword2"
    dftemp <- data.frame(Dominio=(dominiotmp),individualword2=c(word))
    names(dftemp)[1] <- "Dominio"
    names(dftemp)[2] <- "individualword2"
    newdf <- rbind(newdf,dftemp)
    
  }  
  names(df)[1] <- "Dominio"
  names(df)[2] <- "individualword2"
  rowsNoP<- dplyr::filter(df, !(grepl('\\(', individualword2)& grepl('\\)', individualword2)))
  resultdf<- rbind(rowsNoP,newdf)
  #print (nrow(rowsNoAND))  
  print(nrow(resultdf))
  return(resultdf)
}




##########################################################
#END OF FUNCTIONS
##########################################################

###IEEE Queries########################33

dominioant <- ""
dominioact <- ""
query <- "" 
separador <- "\""
or <- " OR "


groupsRequiredAndNonrequired <- "C:\\Mente\\Doctorado\\Stage-2019\\SystematicMap\\orthogonalgroupsfINALRequired.csv"
grupousRN <- read.table(groupsRequiredAndNonrequired, sep= ",", na.strings = "", stringsAsFactors = T, header=T)
RN <-  data.frame(grupousRN)

#Quitar los terminos que tienen caracteres especiales o que 

#Selecciona los registros de los terminos requeridos
datosNoRequeridos <- RN[RN$Required==0,c(3,9)]
#Los datos requeridos se van a filtrar con la funcion que voy a construir
cleanedData <- cleanData(datosNoRequeridos,' & ')
cleanedData <- deleteData(cleanedData,'/')
cleanedData <- deleteParenthesis(cleanedData)
av<- averageWords(cleanedData)
#seleccionar solo las palabras que tengan menos de 7 terminos
datosNoRequeridos<- av[av$length<7,]


datosRequeridos <- RN[RN$Required==1,c(3,9)]

cleanedDatar <- cleanData(datosRequeridos,' & ')
cleanedDatar <- deleteData(cleanedDatar,'/')
cleanedDatar <- deleteParenthesis(cleanedDatar)
avReq<- averageWords(cleanedDatar)
datosRequeridos<- avReq[avReq$length<6,]

#datosRequeridos <- datosRequeridos[sample(nrow(datosRequeridos), round(nrow(datosRequeridos*(sample(0:100,1)/100)))),]
datosRequeridos <- datosRequeridos[order(datosRequeridos$Dominio),]

#Quitar terminos con caracteres especiales


#Relacion query-keyword
queryandword <-setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("query","domain","keyword","required"))

#Por cada uno de los queries de dominios
queries <- combinedDomains()

queriesForDBs<- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("queryNo","query"))


queryName<-""

for(q in 1:length(queries)){
  #print(queries[q])
  #text=paste0(queries[q])
  rdata <- eval(parse(text=paste0(queries[q])))
  
  dominioact <- ""
  dominioant <- ""
  query <- ""
  datosNR <- data.frame()
  averageMean <- 0
  queryName <- paste0("Query",q)
  
  for( i in 1:nrow(rdata) ){
    #Por cada dominio
    dominioact <- toString(rdata[i,1])
    keyword <- toString(rdata[i,2])
    qawtemp <-data.frame(query=c(queryName),domain=c(dominioact),keyword=c(keyword),required=c("yes"))
    queryandword<- rbind(queryandword,qawtemp)
    keyword <- paste0(separador,keyword)
    keyword <- paste0(keyword,separador)
    #calcula el promedio de terminos por dominio
    averageMean <- averageTerms(rdata)
    if(i == nrow(rdata))
    {#Ultimo elemento
      if(dominioact == dominioant ){
        query<- paste0(query," OR ")
        query <- paste0(query,keyword)
      }
      else{
        query<- paste0(query,") AND (")
        query <- paste0(query,keyword)
      }
      
      mylist <-randomTerms(datosNoRequeridos,dominioact,averageMean, query,queryandword,queryName)  
      query<- mylist$tquery
      queryandword<- mylist$tqueryandword
      query<- paste0(query,")")
      qdbtmp=data.frame(queryNo=c(queryName),query=c(trim(query)))
      queriesForDBs<- rbind(queriesForDBs,qdbtmp)
      query<-""
    }
    else{#No es el primer elemento
      if(trim(dominioact) != trim(dominioant)){
        if(nchar(query)== 0){
          query<-"("  
          query <- paste0(query,keyword)
        }
        else{
          mylist <-randomTerms(datosNoRequeridos,dominioant,averageMean, query,queryandword,queryName)
          query<- mylist$tquery
          queryandword<- mylist$tqueryandword
          query<- paste0(query,") AND (")  
          query <- paste0(query,keyword)
          
        }
        
      }
      else{
        query<- paste0(query," OR ")
        query<- paste0(query,keyword)
      }
    }
    
    dominioant <- dominioact
    
  }
}

#PROXIMO CODIGO PARA SCIENCE DIRECT
dominioant <- ""
dominioact <- ""
query <- "" 
separador <- "'"
or <- " OR "


groupsRequiredAndNonrequired <- "C:\\Mente\\Doctorado\\Stage-2019\\SystematicMap\\orthogonalgroupsfINALRequired.csv"
grupousRN <- read.table(groupsRequiredAndNonrequired, sep= ",", na.strings = "", stringsAsFactors = T, header=T)
RN <-  data.frame(grupousRN)

#Quitar los terminos que tienen caracteres especiales o que 

#Selecciona los registros de los terminos requeridos
datosNoRequeridos <- RN[RN$Required==0,c(3,9)]
#Los datos requeridos se van a filtrar con la funcion que voy a construir
cleanedData <- cleanData(datosNoRequeridos)
av<- averageWords(cleanedData)
#seleccionar solo las palabras que tengan menos de 7 terminos
datosRequeridos<- av[av$length<7,]


datosRequeridos <- RN[RN$Required==1,c(3,9)]
#datosRequeridos <- datosRequeridos[sample(nrow(datosRequeridos), round(nrow(datosRequeridos*(sample(0:100,1)/100)))),]
datosRequeridos <- datosRequeridos[order(datosRequeridos$Dominio),]


#Relacion query-keyword
queryandword <-setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("query","domain","keyword","required"))

#Por cada uno de los queries de dominios
queries <- combinedDomains()

queriesForDBs<- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("queryNo","query"))


queryName<-""

for(q in 1:length(queries)){
  #print(queries[q])
  #text=paste0(queries[q])
  rdata <- eval(parse(text=paste0(queries[q])))
  
  dominioact <- ""
  dominioant <- ""
  query <- ""
  datosNR <- data.frame()
  averageMean <- 0
  queryName <- paste0("Query",q)
  
  for( i in 1:nrow(rdata) ){
    #Por cada dominio
    dominioact <- toString(rdata[i,1])
    keyword <- toString(rdata[i,2])
    qawtemp <-data.frame(query=c(queryName),domain=c(dominioact),keyword=c(keyword),required=c("yes"))
    queryandword<- rbind(queryandword,qawtemp)
    keyword <- paste0(separador,keyword)
    keyword <- paste0(keyword,separador)
    #calcula el promedio de terminos por dominio
    averageMean <- averageTerms(rdata)
    if(i == nrow(rdata))
    {#Ultimo elemento
      if(dominioact == dominioant ){
        query<- paste0(query," OR ")
        query <- paste0(query,keyword)
      }
      else{
        query<- paste0(query,") AND (")
        query <- paste0(query,keyword)
      }
      
      mylist <-randomTerms(datosNoRequeridos,dominioact,averageMean, query,queryandword,queryName)  
      query<- mylist$tquery
      queryandword<- mylist$tqueryandword
      query<- paste0(query,")")
      qdbtmp=data.frame(queryNo=c(queryName),query=c(trim(query)))
      queriesForDBs<- rbind(queriesForDBs,qdbtmp)
      query<-""
    }
    else{#No es el primer elemento
      if(trim(dominioact) != trim(dominioant)){
        if(nchar(query)== 0){
          query<-"("  
          query <- paste0(query,keyword)
        }
        else{
          mylist <-randomTerms(datosNoRequeridos,dominioant,averageMean, query,queryandword,queryName)
          query<- mylist$tquery
          queryandword<- mylist$tqueryandword
          query<- paste0(query,") AND (")  
          query <- paste0(query,keyword)
          
        }
        
      }
      else{
        query<- paste0(query," OR ")
        query<- paste0(query,keyword)
      }
    }
    
    dominioant <- dominioact
    
  }
}



##########################################
##CLUSTER VALIDATION
##########################################

results <- clara(queryandword, 8, metric = "euclidean", stand = FALSE, 
      samples = 8, pamLike = FALSE)
dd <- cbind(queryandword, cluster = results$cluster)
head(dd, n = 5)

gowerdist <- daisy(queryandword, metric = "gower")
summary(gowerdist)
gower_mat <- as.matrix(gowerdist)
queryandword[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

queryandword[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]

sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gowerdist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

k <- 8
pam_fit <- pam(gowerdist, diss = TRUE, k)
pam_results <- queryandword %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

clusters <- queryandword %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster)

tsne_obj <- Rtsne(gowerdist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))



write.csv(queriesForDBs,'C:\\Mente\\Doctorado\\Stage-2019\\SystematicMap\\initialqueries.csv')
###########################################################
#Program 2: Filter queries by specified clusters
##########################################################

#Define combinations

#level1_terms <- c("Energy", "Decision Making","Data Science")
#level2_terms <- c("Data Mining","Policy","Machine Learning","Multi Criteria Decision Making")
#level2_terms <- c("Data Analytics", "Machine Learning", "Data Visualization","Data Mining","Policy","Data Science","Multi Criteria Decision Making")
#level3_terms <- c("Smart Grid","Big Data","Data Visualization","Data Analytics")


dcombinations <-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("domain","queryNo","required"))

termsq30 <- c("Data Mining","Data Analytics","Machine Learning","Policy","Multi Criteria Decision Making","Data Visualization")
queries30 <- c("Query30","Query30","Query30","Query30","Query30","Query30")
requerido30<- c("si","si","si","si","si","si")
df<- data.frame(domain=c(termsq30),queryNo =c(queries30),required=c(requerido30))
dcombinations<-rbind(dcombinations,df)

termsq23 <- c("Data Science","Machine Learning","Policy","Multi Criteria Decision Making","Data Analytics","Energy")
queries23 <- c("Query23","Query23","Query23","Query23","Query23","Query23")
requerido23<- c("si","si","si","si","si","si")
df2<- data.frame(domain=c(termsq23),queryNo =c(queries23),required=c(requerido23))
dcombinations<-rbind(dcombinations,df2)


termsq30bis <- c( "Data Mining","Data Analytics","Data Visualization","Smart Grid","Machine Learning","Multi Criteria Decision Making")
queries30bis <- c("Query30","Query30","Query30","Query30","Query30","Query30")
requerido30bis<- c("no","no","no","no","no","no")
df4<- data.frame(domain=c(termsq30bis),queryNo =c(queries30bis),required=c(requerido30bis))
dcombinations<-rbind(dcombinations,df4)

termsq29 <- c( "Decision Making","Data Mining","Data Analytics","Machine Learning","Policy","Multi Criteria Decision Making")
queries29 <- c("Query29","Query29","Query29","Query29","Query29","Query29")
requerido29<- c("si","si","si","si","si","si")
df5<- data.frame(domain=c(termsq29),queryNo =c(queries29),required=c(requerido29))
dcombinations<-rbind(dcombinations,df5)

termsq23bis <- c("Data Science","Policy","Machine Learning","Smart Grid","Data Analytics","Data Visualization")
queries23bis <- c("Query23","Query23","Query23","Query23","Query23","Query23")
requerido23bis<- c("no","no","no","no","no","no")
df6<- data.frame(domain=c(termsq23bis),queryNo =c(queries23bis),required=c(requerido23bis))
dcombinations<-rbind(dcombinations,df6)

termsq26 <- c("Energy","Data Mining","Data Analytics","Machine Learning","Decision Making","Policy")
queries26 <- c("Query26","Query26","Query26","Query26","Query26","Query26")
requerido26<- c("si","si","si","si","si","si")
df7<- data.frame(domain=c(termsq26),queryNo =c(queries26),required=c(requerido26))
dcombinations<-rbind(dcombinations,df7)

termsq29bis <- c("Decision Making","Data Mining","Data Analytics","Data Visualization","Smart Grid","Machine Learning")
queries29bis <- c("Query29","Query29","Query29","Query29","Query29","Query29")
requerido29bis<- c("no","no","no","no","no","no")
df8<- data.frame(domain=c(termsq29bis),queryNo =c(queries29bis),required=c(requerido29bis))
dcombinations<-rbind(dcombinations,df8)

termsq26bis <- c("Energy","Data Analytics","Data Visualization","Data Mining","Decision Making","Policy")
queries26bis <- c("Query26","Query26","Query26","Query26","Query26","Query26")
requerido26bis<- c("no","no","no","no","no","no")
df3<- data.frame(domain=c(termsq26bis),queryNo =c(queries26bis),required=c(requerido26bis))
dcombinations<-rbind(dcombinations,df3)


#Selecciona los queries a filtrar

v<- c("Query30","Query23","Query29","Query26")
queriesForDBs<- queriesForDBs[queriesForDBs$queryNo %in% v,]

myqueries <- dcombinations[,c(2,3)]
myqueries <- myqueries %>% group_by(queryNo,required) %>% summarise(numer = n())
myqueries <- myqueries[,c(1,2)]
dcomb <- vector()
processesqueries<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("queryNo","query","required"))
processesqueriesP2<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("queryNo","query","required"))
domaincombinations <- data.frame()

#Por cada uno de los queries
print(myqueries)
for(m in 1:nrow(myqueries)){
  #Valida que terminos son validos para este query  
  queryNo <- as.character(myqueries[m,c(1)]$queryNo)
  print("Required al principio del ciclo")
  required <- as.character(myqueries[m,c(2)]$required)
  print(required)
  qtmp <- queriesForDBs[queriesForDBs$queryNo==queryNo,]
  query <- as.character(qtmp[,c(2)])
  print(queryNo)
  print(required)
  domaincombinations <- dcombinations[dcombinations$queryNo==queryNo & dcombinations$required==required,]
  dcomb <- as.character(unlist(domaincombinations[,1]))
  terms <- c("Energy","Decision Making","Data Analytics","Machine Learning", "Data Visualization","Data Mining","Policy","Data Science","Multi Criteria Decision Making","Smart Grid")
  completeTerms <- c("Energy","Decision Making","Data Analytics","Machine Learning", "Data Visualization","Data Mining","Policy","Data Science","Multi Criteria Decision Making","Smart Grid","Big Data")
  excludeddomains <- terms[-pmatch(dcomb,terms)]
  #********************AQUI RESTAR LOS DOMINIOS ASOCIADOS A LOS DOMINIOS PRINCIPALES
  if("Data Analytics" %in% excludeddomains && !("Big Data" %in% excludeddomains)){
    excludeddomains<- append(excludeddomains,"Big Data") 
  }
  #seleccionar los dominios que no deberian estar en el query
  requiredwords<- data.frame()
  #Por cada dominio qe NO deberia de estar en el query
  for(i in 1:length(excludeddomains)){
    #Selecciona todas las palabras que esten asociadas al dominio
    edomain<- excludeddomains[i]
    if(edomain=="Data mining"){
      edomain <- "Data Mining"
    }
    if(required=="si"){
      excludedwords<-datosRequeridos[datosRequeridos$Dominio==edomain,]
      for(j in 1:nrow(excludedwords)){
        #Poner la logica de la sustitucion de terminos 
        excludedword <- as.character(excludedwords[j,c(2)])
        expression1 <- paste0("AND \\(\"",excludedword)
        expression1 <- paste0(expression1,"\"\\)")
        expression2 <- paste0("\\('",excludedword)
        expression2 <- paste0(expression2,"\"\\) AND")
        expression3 <- paste0("AND \\(\"",excludedword)
        expression3 <- paste0(expression3,"\" OR ")
        expression4 <- paste0("OR \"",excludedword)
        expression4 <- paste0(expression4,"\"\\)")
        expression5 <- paste0("OR \"",excludedword)
        expression5 <- paste0(expression5,"\" OR")
        expression6 <- paste0("OR \"",excludedword)
        expression6 <- paste0(expression6,"\" \\)")
        print(regexpr(expression1,query)[1])
        if(regexpr(expression1,query)[1]>-1)
          query<- gsub(pattern = expression1, replacement = "", query)
        else if(regexpr(expression2,query)[1]>-1)
          query<- gsub(pattern = expression2, replacement = "", query)
        else if(regexpr(expression3,query)[1]>-1)
          query<- gsub(pattern = expression3, replacement = "AND (", query)
        else if(regexpr(expression4,query)[1]>-1)
          query<- gsub(pattern = expression4, replacement = ")", query)
        else if(regexpr(expression5,query)[1]>-1)
          query<- gsub(pattern = expression5, replacement = "OR", query)
        else if(regexpr(expression6,query)[1]>-1)
          query<- gsub(pattern = expression6, replacement = ")", query)
        #Termina for terminos requeridos excluidos
      }
      
    }
    else{
      #Not required terms
      #Aqui hay que quitar los terminos No requridos asociados a los temas excluidos
      #Selecciona todos los terminos de nivel 1 y 2  
      #Quita los terminos que pertenezcan que esten en esos niveles de query
      excludedwords<-datosNoRequeridos[datosNoRequeridos$Dominio==edomain,]
      for(j in 1:nrow(excludedwords)){
        #Logica de la sustitucion de terminos 
        excludedword <- as.character(excludedwords[j,c(2)])
        expression1 <- paste0("AND \\(\"",excludedword)
        expression1 <- paste0(expression1,"\"\\)")
        expression2 <- paste0("\\('",excludedword)
        expression2 <- paste0(expression2,"\"\\) AND")
        expression3 <- paste0("AND \\(\"",excludedword)
        expression3 <- paste0(expression3,"\" OR ")
        expression4 <- paste0("OR \"",excludedword)
        expression4 <- paste0(expression4,"\"\\)")
        expression5 <- paste0("OR \"",excludedword)
        expression5 <- paste0(expression5,"\" OR")
        expression6 <- paste0("OR \"",excludedword)
        expression6 <- paste0(expression6,"\" \\)")
        print(regexpr(expression1,query)[1])
        if((regexpr(expression1,query)[1])>-1)
          query<- gsub(pattern = expression1, replacement = "", query)
        else if(regexpr(expression2,query)[1]>-1)
          query<- gsub(pattern = expression2, replacement = "", query)
        else if(regexpr(expression3,query)[1]>-1)
          query<- gsub(pattern = expression3, replacement = "AND (", query)
        else if(regexpr(expression4,query)[1]>-1)
          query<- gsub(pattern = expression4, replacement = ")", query)
        else if(regexpr(expression5,query)[1]>-1)
          query<- gsub(pattern = expression5, replacement = "OR", query)
        else if(regexpr(expression6,query)[1]>-1)
          query<- gsub(pattern = expression6, replacement = ")", query)
        
      }#Termina for de terminos excluidos
      
    }#Es query con terminos no requeridos (aqui deberia ir el else)

  }#Termina for de terminos excluidos  
  domaincombinations<- data.frame()
  pqueriestmp<- data.frame(queryNo=c(queryNo),query=c(trim(query)),required=c(required))
  processesqueries <- rbind(processesqueries,pqueriestmp)

}#Fin del for por cada query y si es requerido

write.csv(processesqueries,'C:\\Mente\\Doctorado\\Stage-2019\\SystematicMap\\intermediatequeries.csv')

###############################################
#PARTE 2
###############################################
#Quitar todos los terminos extras
#Si es query debe tener solo terminos requeridos quitar todos los terminos NO requeridos
#Si el query debe tener solo terminos no requeridos quitar los terminos requeridos


for(m in 1:nrow(myqueries)){
  #Valida que terminos son validos para este query  
  queryNo <- as.character(myqueries[m,c(1)]$queryNo)
  #print("Required al principio del ciclo")
  required <- as.character(myqueries[m,c(2)]$required)
  print(required)
  #qtmp<- queriesForDBs %>% filter(queryNo==queryNo,required==required)
  qtmp <- processesqueries[processesqueries$queryNo==queryNo & processesqueries$required==required ,]
  query <- as.character(qtmp[,c(2)])
  completeTerms <- c("Energy","Decision Making","Data Analytics","Machine Learning", "Data Visualization","Data Mining","Policy","Data Science","Multi Criteria Decision Making","Smart Grid","Big Data")
  #completeTerms <- c("Energy","Decision Making","Data Analytics","Machine Learning", "Data Visualization","Data Mining","Policy","Smart Grid","Big Data")
    for(k in 1:length(completeTerms)){
      edomain<- completeTerms[k]
      #if(edomain=="Data mining"){
      #  edomain <- "Data Mining"
      #}
      if(required=="si"){#Quitar terminos en la categoria de no requeridos
        excludedwords<- datosNoRequeridos[datosNoRequeridos$Dominio==edomain,]
        #por cada palabra validar si debe estar en el query
        for(l in 1:nrow(excludedwords)){
          excludedword <- as.character(excludedwords[l,c(2)])
          #Poner la logica de la sustitucion de terminos 
          expression1 <- paste0("AND \\(\"",excludedword)
          expression1 <- paste0(expression1,"\"\\)")
          expression2 <- paste0("\\('",excludedword)
          expression2 <- paste0(expression2,"\"\\) AND")
          expression3 <- paste0("AND \\(\"",excludedword)
          expression3 <- paste0(expression3,"\" OR ")
          expression4 <- paste0("OR \"",excludedword)
          expression4 <- paste0(expression4,"\"\\)")
          expression5 <- paste0("OR \"",excludedword)
          expression5 <- paste0(expression5,"\" OR")
          expression6 <- paste0("OR \"",excludedword)
          expression6 <- paste0(expression6,"\" \\)")
          expression7 <- paste0("AND \\(\"",excludedword)
          expression7 <- paste0(expression7,"\" \\)")
          if(regexpr(expression1,query)[1]>-1){
            query<- gsub(pattern = expression1, replacement = "", query)
          }else if(regexpr(expression2,query)[1]>-1){
            query<- gsub(pattern = expression2, replacement = "", query)
          }else if(regexpr(expression3,query)[1]>-1){
            query<- gsub(pattern = expression3, replacement = "AND (", query)
          }else if(regexpr(expression4,query)[1]>-1){
            query<- gsub(pattern = expression4, replacement = ")", query)
          }else if(regexpr(expression5,query)[1]>-1){
            query<- gsub(pattern = expression5, replacement = "OR", query)
          }else if(regexpr(expression6,query)[1]>-1){
            query<- gsub(pattern = expression6, replacement = ")", query)
          }else if(regexpr(expression7,query)[1]>-1){
            query<- gsub(pattern = expression7, replacement = "", query)
            print(expression7)
          }
        }
      
      }
      else{#Quitar todos los terminos requeridos de la consulta
        
        excludedwords<- datosRequeridos[datosRequeridos$Dominio==edomain,]
        #Por palabra validar si debe estar en el query
        for(l in 1:nrow(excludedwords)){
          excludedword <- as.character(excludedwords[l,c(2)])
          print("excludedword")
          print(excludedword)
          #Logica de la sustitucion de terminos 
          expression1 <- paste0("AND \\(\"",excludedword)
          expression1 <- paste0(expression1,"\"\\)")
          expression2 <- paste0("\\('",excludedword)
          expression2 <- paste0(expression2,"\"\\) AND")
          expression3 <- paste0("AND \\(\"",excludedword)
          expression3 <- paste0(expression3,"\" OR ")
          expression4 <- paste0("OR \"",excludedword)
          expression4 <- paste0(expression4,"\"\\)")
          expression5 <- paste0("OR \"",excludedword)
          expression5 <- paste0(expression5,"\" OR")
          expression6 <- paste0("OR \"",excludedword)
          expression6 <- paste0(expression6,"\" \\)")
          expression7 <- paste0("AND \\(\"",excludedword)
          expression7 <- paste0(expression7,"\" \\)")
          if(regexpr(expression1,query)[1]>-1){
            query<- gsub(pattern = expression1, replacement = "", query)
          }else if(regexpr(expression2,query)[1]>-1){
            query<- gsub(pattern = expression2, replacement = "", query)
          }else if(regexpr(expression3,query)[1]>-1){
            query<- gsub(pattern = expression3, replacement = "AND (", query)
          }else if(regexpr(expression4,query)[1]>-1){
            query<- gsub(pattern = expression4, replacement = ")", query)
          }else if(regexpr(expression5,query)[1]>-1){
            query<- gsub(pattern = expression5, replacement = "OR", query)
          }else if(regexpr(expression6,query)[1]>-1){
            query<- gsub(pattern = expression6, replacement = ")", query)
          }else if(regexpr(expression7,query)[1]>-1){
            query<- gsub(pattern = expression7, replacement = "", query)
            print(expression7)
          }
        } 
        
      }
    }

  #Queries finales
  pqueriestmp<- data.frame(queryNo=c(queryNo),query=c(trim(query)),required=c(required))
  processesqueriesP2 <- rbind(processesqueriesP2,pqueriestmp)
  }


write.csv(processesqueriesP2,'C:\\Mente\\Doctorado\\Stage-2019\\SystematicMap\\queriesfinalesV7.csv',quote=FALSE)

#Las palabras unicas salen de los quotes
#Por cada uno de los queries
processesqueriesP3<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("queryNo","query","required"))
for(x in 1:nrow(processesqueriesP2)){
  query <- processesqueriesP2[x,c(2)]
  required<- processesqueriesP2[x,c(3)]
  queryNo <- as.character(processesqueriesP2[x,c(1)])
  if(!any(is.na(unlist(stringi::stri_extract_all_regex(query, '(?<=")\\w*(?=")'))))){
    terminosunicos<- unlist(stringi::stri_extract_all_regex(query, '(?<=")\\w*(?=")'))
    #sub("\"","",terminosunicos[i])
    print(terminosunicos)
    for(i in 1:length(terminosunicos)){
      expression1<- paste0('"',terminosunicos[i])
      expression1<- paste0(expression1,'"')
      print(expression1)
      if(regexpr(expression1,query)[1]>-1){
        query<- gsub(pattern = expression1, replacement = terminosunicos[i] , query)
      }
      
      
    }
    quniqueterms<- data.frame(queryNo=c(queryNo),query=c(trim(query)),required=c(required))
    processesqueriesP3<- rbind(processesqueriesP3,quniqueterms)
    #Print Final query
    #put final query in a list
    
  }
    
}

write.csv(processesqueriesP3,'C:\\Mente\\Doctorado\\Stage-2019\\SystematicMap\\queriesfinalesV8.csv',quote=FALSE)

#quitarle todos los quotes a los queries

processesqueriesP4<-setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("queryNo","query","required"))
for(x in 1:nrow(processesqueriesP3)){
  query <- processesqueriesP2[x,c(2)]
  required<- processesqueriesP2[x,c(3)]
  queryNo <- as.character(processesqueriesP2[x,c(1)])
  query<- gsub(pattern = '"', replacement =""  , query)
  
  freeterms<- data.frame(queryNo=c(queryNo),query=c(trim(query)),required=c(required))
  processesqueriesP4<- rbind(processesqueriesP4,freeterms)
  
  }
      
  
    

write.csv(processesqueriesP4,'C:\\Mente\\Doctorado\\Stage-2019\\SystematicMap\\queriesfinalesV11.csv',quote=FALSE)

