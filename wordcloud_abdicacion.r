

#----- Libraries
library(stringr)

#----- Directorio PON TU DIRECTORIO
setwd("C:/Users/pedroc/Desktop/madRid-R/discursos rey")

#----- URLs for the speeches 
dis.dir <- c(
  'http://www.casareal.es/ES/Actividades/Paginas/actividades_discursos_detalle.aspx?data=5345')

anio <- 2014

for(i in 1:length(dis.dir)) {
  print(i)
  #dis.txt <- readLines(dis.dir[i], ok=TRUE)
  dis.txt <- readLines(url(dis.dir[i], encoding="UTF-8"), ok=TRUE)
  lines.gd <- grep("<p>", dis.txt)
  dis.gd <- dis.txt[lines.gd]
  
  ext.ini <- as.data.frame(str_locate(dis.gd, "\\<p\\>"))
  ext.end <- as.data.frame(str_locate(dis.gd, "\\/p\\>"))
  val.ext <- data.frame(start=(ext.ini$end + 2) , end=(ext.end$start - 2))
  dis.end <- str_sub(dis.gd, start=val.ext$start, end=val.ext$end)
  dis.tmp <- dis.end[grep("http", dis.end, invert=T)]
  dis.tmp2 <- gsub("<span class=\\\"","",dis.tmp)
  dis.tmp3 <- gsub("capital\\\"","",dis.tmp2)
  dis.tmp4 <- gsub("</span><p>","",dis.tmp3)
  dis.tmp5 <- gsub(">","",dis.tmp4)
  dis.tmp6 <- gsub("\\&quot","",dis.tmp5)
  dis.tmp7 <- gsub("</span","",dis.tmp6)
  dis.tmp8 <- gsub("<U+200B>","",dis.tmp7)
  dis.fin <- dis.tmp8
  
  fil.dat <- paste(anio,".txt", sep="")
  writeLines(dis.fin, con=fil.dat)
  
  #read.val[i] <- readability(fil.dat, index="Flesch.Kincaid", force.lang="es", tagger="tokenize")
  #read.val[i] <- readability(fil.dat, index="Flesch.Kincaid", force.lang="es-utf8")
  #read.val[i] <- flesch(fil.dat, parameters="es")
  #tok.obj <- tokenize(fil.dat, format="file", lang="es")
}

#-----------------------------------
#-------- WordClouds....
# Output: Files as png 
#-----------------------------------
library(wordcloud)
library(tm)
library(RColorBrewer)

d.all <- data.frame()

  fil.dat <- paste(anio,".txt",sep="")  
  tmp<-readLines(fil.dat)
  text.to <- Corpus(VectorSource(tmp))
  
  text.to <- tm_map(text.to, removePunctuation)
  text.to <- tm_map(text.to,removeNumbers)
  text.to <- tm_map(text.to, function(x){
    removeWords(x
                ,c(stopwords(kind="es"),
                   "españa", 
                   "España",
                   "españolas",
                   "españoles"))
          })

  
  tdm <- TermDocumentMatrix(text.to)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d.tmp <- data.frame(word = names(v),freq=v, Anio=i)
  d.all <- rbind(d.all,d.tmp)
  
  # Ojo que he cambiado d en rbind por d.all porque me daba error   
  
#  png(filename=paste("Discurso-",i,".png",sep=""), width=900, height=900) 
  pal <- brewer.pal(3,"Dark2")
#  pal <- pal[-(1)]
  wordcloud(d.tmp$word,
            d.tmp$freq,
            c(8,.3),
            min.freq = 2,
            colors = pal)
