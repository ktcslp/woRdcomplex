# woRdcomplex version 1.1 (09 September 2020)--an R software script for
#automated phonetic transcription analysis by Kevin T Cunningham. Copyright (C)
#2020. Kevin T Cunningham
#This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. AMDG. 
#Script calculates the edit distance ratio for intelligible words in a sample. Requires CSV file "mrc2.csv"

batch<-{}
ppa<-{}

library(tidyr)
library(tidytext)
library(stringr)
phonetic<-tibble()
stress<-tibble()
mrc<-read.csv('mrc2.csv', na.strings=c("", "NA"))
fileNames = dir(pattern = ".txt")
for (fileName in fileNames){
  phonetic<-tibble()
  stress<-tibble()
  data<-{}
  sample <- readChar(fileName, file.info(fileName)$size)
  sample<-as.character(sample)
  sample<-str_to_upper(sample, locale="en")
  #sample<-toupper(sample)
  text_df<-tibble(text=sample)
  text_df <-text_df%>%
    unnest_tokens(word, text)
  ##text_df<- text_df %>%
    #anti_join(stop_words)
  tibbletest <-tibble(mrc$word, mrc$phon)
  #stresstest<-tibble(mrc$word, mrc$stress)
  tibbletest <- na.omit(tibbletest)
  concrete <-na.omit(tibble(mrc$word, mrc$conc))
  tibbletest <- na.omit(tibbletest)
  
  #which(toupper(text_df$word[100])==tibbletest$`mrc$word`, arr.ind = TRUE)
  #i=1
  for (i in 1:nrow(text_df)){
    r<-which(toupper(text_df$word[i])==tibbletest$`mrc$word`, arr.ind = TRUE)
    phonetic <- paste(phonetic, tibbletest$`mrc$phon`[r[1]])
    
   ## b<-which(toupper(text_df$word[i])==tibbletest$`mrc$stress`, arr.ind = TRUE)
    #stress <- paste(stress, stresstest$`mrc$stress`[r[1]])
    
    
    #stess<-paste(stress, tibbletest$`mrc$phon`[r[1]]))
    #i=i+1
  }
  phonetic<-gsub("NA", "", phonetic)
  phonetic<-str_split(string=phonetic, pattern=" ")
  phonetic<-as.data.frame(phonetic, stringsAsFactors=FALSE)
  
  tibbletest$`mrc$phon`[30407]
  points<-0
  #phonetic<-phonetic$c......wel....De.....Iz....eI....m.n....De.....wID...........
  for (j in 1:nrow(phonetic)){
    #if more than 1 syllabe, add 1 point
    if(str_count(phonetic[j,], "/")>0){
      points=points+1
    }
    #add a point for each velar consonant
    points=points+str_count(phonetic[j,], "k")
    points=points+str_count(phonetic[j,], "g")
    
    #add a point for each liquid
    points=points+str_count(phonetic[j,], "l")
    points=points+str_count(phonetic[j,], "r")
    
    #Need to add voiced fricatve or affricate
    points=points+str_count(phonetic[j,], "v")
    points=points+str_count(phonetic[j,], "z")
    points=points+str_count(phonetic[j,], "Q")
    
    #Need to add fricative or affricate
    points=points+str_count(phonetic[j,], "v")
    points=points+str_count(phonetic[j,], "z")
    points=points+str_count(phonetic[j,], "Q")
    points=points+str_count(phonetic[j,], "f")
    points=points+str_count(phonetic[j,], "s")
    points=points+str_count(phonetic[j,], "S")
    
    # Consonant clusters approximation
    #b-blends
    points=points+str_count(phonetic[j,], "bd")+str_count(phonetic[j,], "bl")+str_count(phonetic[j,], "br")+str_count(phonetic[j,], "bz")
    #d blends
    points=points+str_count(phonetic[j,], "dr")+str_count(phonetic[j,], "dw")+str_count(phonetic[j,], "dz")
    points=points+str_count(phonetic[j,], "dr")+str_count(phonetic[j,], "dw")+str_count(phonetic[j,], "dz")
    # f blends
    points=points+str_count(phonetic[j,], "fl")+str_count(phonetic[j,], "fr")+str_count(phonetic[j,], "fs")+str_count(phonetic[j,], "ft")
    #g blends
    points=points+str_count(phonetic[j,], "gd")+str_count(phonetic[j,], "gl")+str_count(phonetic[j,], "gr")+str_count(phonetic[j,], "gw")+str_count(phonetic[j,], "gz")
    # Z blend
    points=points+str_count(phonetic[j,], "Zd")+str_count(phonetic[j,], "dZd")
    # k blends
    points=points+str_count(phonetic[j,], "kl")+str_count(phonetic[j,], "kr")+str_count(phonetic[j,], "ks")+str_count(phonetic[j,], "kt")+str_count(phonetic[j,], "kw")
    
    #l blends
    points=points+str_count(phonetic[j,], "lb")+str_count(phonetic[j,], "ltS")+str_count(phonetic[j,], "ltSt")+str_count(phonetic[j,], "ld")+str_count(phonetic[j,], "lf")+str_count(phonetic[j,], "ldZ")+str_count(phonetic[j,], "ldZd")+str_count(phonetic[j,], "lk")+str_count(phonetic[j,], "lkt")+str_count(phonetic[j,], "lm")+str_count(phonetic[j,], "lp")+str_count(phonetic[j,], "lpt")+str_count(phonetic[j,], "ls")+str_count(phonetic[j,], "ltf")+str_count(phonetic[j,], "lv")+str_count(phonetic[j,], "lz")+str_count(phonetic[j,], "lvd")
    
    # m blends
    points=points+str_count(phonetic[j,], "md")+str_count(phonetic[j,],"mf")+str_count(phonetic[j,], "mft")+str_count(phonetic[j,], "mp")+str_count(phonetic[j,], "mpt")+str_count(phonetic[j,], "mz")
    
    # n blends
    points=points+str_count(phonetic[j,], "ntS")+str_count(phonetic[j,],"ntSt")+str_count(phonetic[j,], "ns")+str_count(phonetic[j,], "nd")+str_count(phonetic[j,], "ndZ")+str_count(phonetic[j,], "ndZd")+str_count(phonetic[j,], "nt")+str_count(phonetic[j,], "nz")
    
    #velar nasal blends
    points=points+str_count(phonetic[j,], "9d")+str_count(phonetic[j,], "9k")+str_count(phonetic[j,], "9z")
    
    #p blends
    points=points+str_count(phonetic[j,], "ps")+str_count(phonetic[j,], "pt")
    
    #s blends
    points=points+str_count(phonetic[j,], "sf")+str_count(phonetic[j,], "sk")+str_count(phonetic[j,], "skr")+str_count(phonetic[j,], "skw")+str_count(phonetic[j,], "sl")+str_count(phonetic[j,], "sm")+str_count(phonetic[j,], "sn")+str_count(phonetic[j,], "sp")+str_count(phonetic[j,], "spl")+str_count(phonetic[j,], "spr")+str_count(phonetic[j,], "st")+str_count(phonetic[j,], "str")+str_count(phonetic[j,], "sw")
    
    #T blends
    points=points+str_count(phonetic[j,], "Tr")+str_count(phonetic[j,], "Ts")+str_count(phonetic[j,], "Tw")
    
    #S blends
    points=points+str_count(phonetic[j,], "Sr")+str_count(phonetic[j,], "St")
    
    #D blends
    points=points+str_count(phonetic[j,], "Dd")+str_count(phonetic[j,], "Dz")
    
    #p blends
    points=points+str_count(phonetic[j,], "pl")+str_count(phonetic[j,], "pr")+str_count(phonetic[j,], "ps")+str_count(phonetic[j,], "pt")
    
    #v blends
    points=points+str_count(phonetic[j,], "vd")+str_count(phonetic[j,], "vz")
    
    #z blends
    points=points+str_count(phonetic[j,], "zd")
    #syllable stress
    
  }
  #phonetic$c..........n.U....D.t....It....ne.v.....keIm....b.k....aI....maIt...!=""
  #enominator <- sum(phonetic$c..........n.U....D.t....It....ne.v.....keIm....b.k....aI....maIt...!="")
  phonetic[!apply(phonetic == "", 1, all),]
  
  score=points/nrow(phonetic)
  #phonetic<-as.data.frame(phonetic)
  #phonetic[!is.na(phonetic$c......wel....De.....Iz....eI....m.n....De.....wID....NA....NA...),]
 #for (i in text_df){
    #print(which(i==mrcword, arr.ind = TRUE))}
 # for (i in nrow(text_df)){
  #  print((which(text_df$word[i]==tibbletest$`mrc$word`, arr.ind = TRUE)))
  #}
  data<-cbind(fileName, score, points, nrow(phonetic))
  write.table(data, file="WCD_data.csv", append=TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
  
}
#fiat mihi secundum verbum tuum

