setwd("C:/Users/dge-usuario/Documents/Egresados2025")
options(java.parameters = "-Xmx8000m")
library(readxl)
library(dplyr)
library(stringr)
library(xtable)
library(ggplot2)
library(tidyr)
library(ggrepel)

cvlb<-function(lab){
  lab<-round(lab*100,digits = 2)
  r<-as.character(lab)
  temp<-rep(0,length(r))
  temp[lab==round(lab,digits = 1)]<-1
  temp[lab==round(lab,digits = 0)]<-2
  r[temp==1]<-paste0(r[temp==1],"0")
  r[temp==2]<-paste0(r[temp==2],".00")
  return(r)
}
sat_table<-function(Base){
  allcolnam<-c(paste0("X_81_",seq(1,50)))
  temp<-setNames(data.frame(matrix(ncol = length(setdiff(allcolnam,colnames(Base))), nrow = nrow(Base))), setdiff(allcolnam,colnames(Base)))
  Sat<-cbind(Base[,intersect(allcolnam,colnames(Base))],temp)[,allcolnam]
  for(i in 1:50){
    Sat[,i]<-as.numeric(Sat[,i])
  }
  Sat[Sat==0]<-NA
  
  UAEH<-20
  PE<-19
  DES<-33
  DesPer<-c(16,17,18)
  Ens<-38:44
  ALL<-1:50
  
  RSat<-data.frame(`Programas y Servicios`=apply(Sat[,ALL[-c(UAEH,PE,DES,DesPer,Ens)]],1,function(X) mean(X,na.rm=T)),
                   `Aspectos de la enseñanza`=apply(Sat[,Ens],1,function(X) mean(X,na.rm=T)),
                   Institución=apply(Sat[,c(UAEH,DES,PE)],1,function(X) mean(X,na.rm=T)),check.names = F)
  RSat[which(RSat<2.5,arr.ind = T)]<-1
  RSat[RSat!=1]<-0
  RSat[is.na(RSat)]<-0
  RSat<-cbind(Base[,c("centroCostoDes","programaAcademico")],RSat)
  
  return(RSat)
  #RIS<-data.frame(apply(RSat,2,function(X) length(which(X<2.5))))
  #rownames(RIS)<-str_replace_all(rownames(RIS),"\\."," ")
  #colnames(RIS)<-"N"
  #RIS$`%`<-round(RIS$N/nrow(RSat)*100,digits = 2)
  #return(RIS)
}
razones_table<-function(BDF,N){
  R<-data.frame(cbind(table(unlist(BDF)),round((table(unlist(BDF))/N)*100,digits = 2)))
  colnames(R)<-c("N","%")
  R<-R[order(R$N,decreasing = T),]
  return(R)
}
create_table<-function(v,t,d=NULL,e=NULL,k=NULL){
  if(t==1){
    temp<-table(factor(v,e))
    R<-data.frame(cbind(temp,prop.table(temp)))
    colnames(R)<-c("N","%")
    R$`%`<-round(R$`%`*100,digits = 2)
    R<-R[e,]
  }else if (t==2){
    v[v!="Si"]<-"No"
    R<-data.frame(t(rbind(apply(v,2,function(X) table(factor(X,c(e,"No")))),apply(v,2,function(x) prop.table(table(factor(x,c(e,"No"))))))))[,c(1,3)]
    colnames(R)<-c("N","%")
    R$`%`<-round(R$`%`*100,digits = 2)
    rownames(R)<-d[match(rownames(R), d$NOMENCLATURA),1]
  }else if (t==3){
    R<-data.frame(t(rbind(apply(v,2,function(X) table(factor(X,e))),apply(v,2,function(X) round(prop.table(table(factor(X,e)))*100,digits=2)))))
    rownames(R)<-d[match(rownames(R), d$NOMENCLATURA),1]
    colnames(R)[1:(ncol(R)/2)]<-e
    colnames(R)[(ncol(R)/2+1):ncol(R)]<-paste0(e," %")
    temp1<-rep(1:(ncol(R)/2), each = 2)
    temp1[seq(2,length(temp1),by=2)]<-temp1[seq(2,length(temp1),by=2)]+length(temp1)/2
    R<-R[,temp1]
  }else{
    e<-e[length(e):1]
    R<-data.frame(apply(v,2,function(X) prop.table(table(factor(X,e)))))
    colnames(R)[1:ncol(R)]<-d[match(colnames(R), d$NOMENCLATURA),1]
    R$Resp<-e
    rownames(R)<-1:nrow(R)
    R<-R[,c(ncol(R),1:(ncol(R)-1))]
    R<-gather(R,key="Aspec",value = "Freq",-Resp)
    if(length(unique(R$Aspec))==4){
      cols<-c("#ED7D31","#","","")
    }
    temp<-R
    temp$Resp<-factor(temp$Resp, levels=e)
    temp$labs<-cvlb(temp$Freq)
    temp$poslb<-0
    for (j in unique(temp$Aspec)) {
      temp1<-c(0,cumsum(temp$Freq[temp$Aspec==j][length(temp$Freq[temp$Aspec==j]):1]))
      temp1<-temp1[length(temp1):1]
      temp$poslb[temp$Aspec==j]<-(temp1[1:(length(temp1)-1)]+temp1[2:length(temp1)])/2
    }
    temp$Resp<-as.factor(temp$Resp)
    temp$Aspec<-as.factor(temp$Aspec)
    #R[order(unlist(sapply(R$Resp, function(x) which(e == x)))),]
    R<-ggplot(temp ,aes(x = Aspec, fill = Resp, y = Freq)) +
      #labs(title = str_wrap(preg[i],width = 60)) +
      geom_bar(stat = "identity") + 
      scale_fill_brewer(palette = "Oranges") +
      #scale_fill_manual(values = c("#DADAEB", "#9E9AC8", "#6A51A3")) +
      guides(fill = guide_legend(title = "")) +
      geom_label_repel(aes(x=Aspec,y=poslb,label = labs), colour = "black",fill="white",size=3) +
      scale_y_continuous(labels = scales::percent) +
      #geom_text_repel(aes(x=Aspec,y=poslb,label = labs), color = "white",bg.color = "black",bg.r = 0.08,size=3.5) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 22,hjust = 0.5,colour = "black"),
            axis.text.x = element_text(vjust = 0.5,colour = "black",size=17),
            axis.text.y = element_text(size=15),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "#D3D3D3"),
            panel.background = element_rect(fill = "#D3D3D3"),
            legend.box = ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 50)) +
      coord_flip()
    #geom_text(position = position_fill(.5))
    ggsave(paste0("Preg",k,".pdf"),R,width = 12,height=1+length(unique(temp$Aspec))*0.7)
  }
  return(R)
}
exportReport <- function(x) {
  id <<- x[1]
  id2 <<- x[2]
  fileStem <- "Report_"
  file.copy("Report_TemplateXP.Rnw",
            paste("output/", fileStem, id," - ",id2, ".Rnw", sep =""),
            overwrite = TRUE)
  file.copy("Sweave.sty", "output/Sweave.sty", overwrite = TRUE)
  setwd("output")
  Sweave(paste(fileStem, id," - ",id2, ".Rnw", sep =""))
  tools::texi2pdf(paste(fileStem, id," - ",id2, ".tex", sep =""))
  setwd("..")
  if(!dir.exists(paste0("pdf/",id))){
    dir.create(paste0("pdf/",id))
  }
  file.copy(paste("output/", fileStem, id," - ",id2, ".pdf", sep =""),
            paste("pdf/",id,"/", fileStem, id," - ",id2, ".pdf", sep =""),
            overwrite = TRUE)
  file.copy(paste("output/", fileStem, id," - ",id2, ".pdf", sep =""),
            paste("enviar pe/", id," - ",id2, ".pdf", sep =""),
            overwrite = TRUE)
}

BaseG<-data.frame(read_excel("001-23 Seguimiento de Egresados UAEH-(Licenciatura) 2025 (respuestas).xlsx",guess_max = 50000),check.names = F)
Clave<-data.frame(read_excel("ClaveEgresados.xlsx"))
#CategPE<-data.frame(read_excel("Insatisfacción PE 2024 Categorias.xlsx"))
#CategDES<-data.frame(read_excel("Insatisfacción DES 2024 Categorias.xlsx"))
#CategUAEH<-data.frame(read_excel("Insatisfacción UAEH 2024 Categorias.xlsx"))
#LisPE<-data.frame(read_excel("Lista Programas Trayectorias.xlsx"))

#Clave$NOMENCLATURA[grep("_",Clave$NOMENCLATURA)]<-paste0("X",Clave$NOMENCLATURA[grep("_",Clave$NOMENCLATURA)])
Colut<-Clave[!is.na(Clave$NÚMERO),]
Colut$NÚMERO<-as.numeric(Colut$NÚMERO)

T2<-unique(Clave$NOMENCLATURA[which(Clave$TIPO==2)])
T2

###########
#PRUEBA T2#
###########

Binaria <- BaseG %>%
  select(all_of(T2)) %>%
  mutate(across(everything(), ~strsplit(as.character(.), ",\\s*")))

###
subtablas<- list()
for(col in T2){
  titulo<-col
  temp<- Binaria %>%
    select(all_of(col)) %>%
    mutate(row = row_number()) %>%
    unnest(all_of(col)) %>%
    mutate(valor=1) %>%
    pivot_wider(
      names_from=all_of(col),
      values_from=valor,
      values_fill=list(valor=0)
    ) %>%
    arrange(row) %>%
    select(-row)
  subtablas[[titulo]] <- temp
}

###########
####      #
###########


#BaseG<-BaseG[BaseG$centroCostoDes!="NULL",]
#BaseG[,c("centroCostoDes", "programaAcademico")] <- LisPE[match(do.call(paste0,BaseG[, c("centroCostoDes", "programaAcademico")]), do.call(paste0,LisPE[,1:2])), 4:5]
#LisPE<-LisPE[!is.na(LisPE$RT),c(4:5,3,11)]
#LisPE$NC<-round((LisPE$N/LisPE$RT)*100,digits = 2)
#colnames(LisPE)<-c("centroCostoDes","programaAcademico","Participantes","Matrícula Reingreso","% de Participación")
#LisPE$Nivel<-"Posgrado"
#LisPE$Nivel[LisPE$programaAcademico=="Bachillerato"]<-"Bachillerato"
#LisPE$Nivel[substr(LisPE$programaAcademico,1,5)=="Licen"]<-"Licenciatura"
#Participa<-data.frame(LisPE %>% group_by(Nivel) %>% summarise(Reingreso=sum(`Matrícula Reingreso`)))
#AllSatG<-sat_table(BaseG)
id <- NULL
id2<- NULL

PE<-data.frame(BaseG %>% distinct(centroCostoDes,programaAcademico))
#basee<-read_excel("D:/Documents/APDT/Trayectorias/2024/Informes/Enviar correos Trayectorias 2024.xlsm")
#basee$ATTACHMENT <- gsub("\\\\", "/", basee$ATTACHMENT)
#file.exists(basee$ATTACHMENT)

#setwd("ResXP")
apply(PE[1,],1, exportReport)



Egresados<-read_excel("C:/Users/dge-usuario/Documents/Cobos/Egresados/001-23 Seguimiento de Egresados UAEH-(Licenciatura) 2025 (respuestas).xlsx",guess_max = 2000)
Sep<-str_split_fixed(Egresados$`2.6 Por favor señala cuáles fueron las tres razones más importantes para la elección de la licenciatura`,", ",3)

