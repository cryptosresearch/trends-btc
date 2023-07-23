
# PARTE01 GOOGLE TRENDS COLETAR DADOS

{
  library(gtrendsR)
  library(plotly)
  library(dplyr)
  library(readxl)
  library(ggplot2)
  library(tidyverse)
  library(waffle) # remotes::install_github("hrbrmstr/waffle")
  library(patchwork)
  library(lubridate)
  library(extrafont)
  library(ggtext)
  library(Cairo)
  library(ggthemes)
  library(BBmisc) # for function is.error
}


{
  # Limpeza dos dados
  rm(list = ls(all.names = TRUE)) # Limpar todos objetos do R
  getwd() # Verificar local do diretorio
}

# CRIAR BANCO DE DADOS
FILE00<-paste("DATABASE/03 Trends Analysis/",Sys.Date(),sep = "")
FILE00
dir.create(FILE00)

##################################################################################################################
# 01 FUNCTION


GT_SCORE<-function(PHRASE,TITLE,timeGTname){
  
  #PHRASE<-"BTC"
  #timeGTname<-"12M"
  #TITLE<-"TESTE"
  #Dados_GT_CRYPTOS<-POS_GT_BUY_00
  
  
  GT_TABELA<-gtrends(keyword =PHRASE, gprop ="web", time="today 12-m", onlyInterest=TRUE)
  GT_TABELA_00<-GT_TABELA$interest_over_time
  class(GT_TABELA_00$hits)<-"numeric"
  
  Dados_GT_CRYPTOS<-GT_TABELA_00
  
  if(class(Dados_GT_CRYPTOS)=="data.frame"){
    class(Dados_GT_CRYPTOS$hits)<-"numeric"
    FILE01<-paste(paste( FILE00,"/",PHRASE,timeGTname,sep = ""),"rds",sep='.')
    FILE02<-paste(paste( FILE00,"/",PHRASE,timeGTname,sep = ""),"png",sep='.')
    FILE03<-paste( FILE00,"/",PHRASE,timeGTname,"_BR.png",sep = "")
    
    saveRDS(Dados_GT_CRYPTOS, file=FILE01, version=3)
    
    #inicio tabela
    Dados_GT_B3_360D<-Dados_GT_CRYPTOS
    Dados_GT_B3_360D<-Dados_GT_B3_360D %>% filter(!is.na(hits))
    linha360D<-nrow(Dados_GT_B3_360D)
    #Tabela de classificacao
    media_01<-round(mean(Dados_GT_B3_360D$hits[1:(round(linha360D*0.2,0))]),2)
    media_02<-round(mean(Dados_GT_B3_360D$hits[(round(linha360D*0.2,0)):(round(linha360D*.4,0))]),2)
    media_03<-round(mean(Dados_GT_B3_360D$hits[(round(linha360D*0.4,0)):(round(linha360D*0.6,0))]),2)
    media_04<-round(mean(Dados_GT_B3_360D$hits[(round(linha360D*0.6,0)):(round(linha360D*0.8,0))]),2)
    media_05<-round(mean(Dados_GT_B3_360D$hits[(round(linha360D*0.8,0)):(round(linha360D,0))]),2)
    media<-c("media_01","media_02","media_03","media_04","media_05")
    Valor<-c(media_01,media_02,media_03,media_04,media_05)
    tab_media<-data.frame(media,Valor)
    tab_media$CC<-tab_media$Valor/media_04
    tab_media<-arrange( tab_media,desc( Valor))
    tab_media$Classe<-c("Alto","Mediano","Neutro","Baixo","Muito Baixo")
    tab_media$nota_02_Min<-c(8.01,6.01,4.01,2.01,0.01)
    # pontos da regressao
    Dados_GT_B3_360D$num<-c(1:nrow(Dados_GT_B3_360D))
    Dados_RG<-Dados_GT_B3_360D[(round(linha360D*0.8,0)):(round(linha360D,0)),]
    lmHeight = lm(num~hits, data = Dados_RG)
    alfa20<-lmHeight$coefficients[2]
    # primeira opcao
    if(is.na(alfa20)!=TRUE){
      if(alfa20>= -0.5 & alfa20<= 0.5){ pontos<-alfa20*2 + 1  }
      if(alfa20< -0.5){ pontos<- 0  }
      if(alfa20> 0.5){ pontos<- 2  }
      tab_final<-tab_media[tab_media$media=="media_05",]
      nota_02<-round(as.numeric(tab_final$nota_02_Min+pontos),2)
      status<-as.character(tab_final$Classe)
      print(paste(status,nota_02))
    }
    
    # reclassificacao
    # outros fatores
    last<-max(Dados_GT_B3_360D$hits[linha360D])
    maior_last3<-max(Dados_GT_B3_360D$hits[linha360D],
                     Dados_GT_B3_360D$hits[linha360D-1],
                     Dados_GT_B3_360D$hits[linha360D-2]
    )
    
    quartil90<- round(quantile(Dados_GT_B3_360D$hits, prob=c(.90))[[1]],2)
    quartil50_80<- round(quantile(Dados_GT_B3_360D$hits[(round(linha360D*0.8,0)):(round(linha360D,0))], prob=c(.50))[[1]],2)
    # reclassificacao
    if(maior_last3<60 & status=="Alto"){
      nota_02<-nota_02-2
      status<-"Mediano"
    }
    if(last<1 & status=="Alto"){
      nota_02<-nota_02-2
      status<-"Mediano"
    }
    if(maior_last3<40 & status=="Mediano"){
      nota_02<-nota_02-2
      status<-"Neutro"
      
    }
    if(last<1 & status=="Mediano"){
      nota_02<-nota_02-2
      status<-"Neutro"
    }
    if(maior_last3<20 & status=="Neutro"){
      nota_02<-nota_02-2
      status<-"Baixo"
    }
    if(maior_last3<5 ){
      nota_02<-round(nota_02/5,2)
      status<-"Muito Baixo"
    }
    if(quartil90<0.1 ){
      nota_02<-round(nota_02/5,2)
      status<-"Muito Baixo"
    }
    if(quartil50_80<1 ){
      nota_02<-round(nota_02/5,2)
      status<-"Muito Baixo"
    }
    print(paste(status,nota_02))
    
    #SALVAR NOTA
    
    # plotar grafico
    PL00<-ggplot(Dados_GT_CRYPTOS,aes(y = Dados_GT_CRYPTOS$hits, x = Dados_GT_CRYPTOS$date)) +
      geom_point(size=2, color="#003f5c") +
      geom_smooth(method = "loess",span = 0.3,color="#ff6e54")+
      xlab(paste("Crypto:",PHRASE,"/ Intensidade Atual:",status,"(",nota_02,")","/ Elaborado por: CryptosResearch.com"))+
      labs(y = NULL,subtitle = paste("**<span style='color:#003f5c;font-size:10px;'>Intensidade de Pesquisa no Google**",TITLE))+
      annotate("text",x=Dados_GT_CRYPTOS$date[nrow(Dados_GT_CRYPTOS)/2],y=-3,
               label="www.cryptosresearch.com",angle=0,alpha=.4,size=2)+ 
      coord_cartesian( ylim = c(-5, 100))+
      scale_y_continuous(breaks=c(0,50,100),label = c("0%", "50%", "100%"))+
      
      theme(axis.text.x = element_text(color="#003f5c", size=7),
            axis.text.y = element_text(face="bold", color="#003f5c", size=7),
            axis.text=element_text(size=5,),
            axis.title=element_text(size=6,face="italic",color="#003f5c"),
            plot.subtitle = element_markdown(color = "#003f5c"))
    PL00
    
    PL01<-ggplot(Dados_GT_CRYPTOS,aes(y = Dados_GT_CRYPTOS$hits, x = Dados_GT_CRYPTOS$date)) +
      geom_point(size=2, color="#003f5c") +
      geom_smooth(method = "loess",span = 0.3,color="#ff6e54")+
      labs(y = NULL,x=NULL,subtitle = NULL)+
      annotate("text",x=Dados_GT_CRYPTOS$date[nrow(Dados_GT_CRYPTOS)/2],y=-4,
               label="www.cryptosresearch.com (Fonte: Google Trends)",angle=0,alpha=.5,size=2)+ 
      annotate("text",x=Dados_GT_CRYPTOS$date[nrow(Dados_GT_CRYPTOS)/2],y=108,
               label=paste("Intensidade:",status,"(",nota_02,")"),angle=0,alpha=.5,size=2)+ 
      coord_cartesian( ylim = c(-6, 110))+
      theme_fivethirtyeight() + scale_fill_fivethirtyeight() +
      scale_y_continuous(breaks=c(0,50,100),label = c("0%", "50%", "100%"))+
      theme(axis.text.x = element_text(color="#003f5c", size=6),
            axis.text.y = element_text(face="bold", color="#003f5c", size=6),
            axis.text=element_text(size=5,),
            axis.title=element_text(size=6,face="italic",color="#003f5c"),
            plot.subtitle = element_markdown(color = "#003f5c"),
            plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill='transparent'),
            legend.background = element_rect(fill='transparent'),
            legend.box.background = element_rect(fill='transparent'))
    
    
    PL01
    
    ggsave(FILE02,plot = PL00,width = 16, height = 4,units = "cm",dpi = 300,limitsize = TRUE)
    ggsave(FILE03,plot = PL01, width = 9.017, height = 2.5,units = "cm",dpi = 300,limitsize = TRUE)
    
    return(nota_02)
  }
}

##################################################################################################################
# 01 ANALISE GLOBAL POSITIVA

POS_GT_BUY<-gtrends(keyword ="BUY BITCOIN", gprop ="web", time="today 12-m", onlyInterest=TRUE)
POS_GT_BUY_00<-POS_GT_BUY$interest_over_time
class(POS_GT_BUY_00$hits)<-"numeric"

Word_crypto<-GT_SCORE("crypto","Cripto","today 12-m")
Word_crypto
Word_Bitcoin<-GT_SCORE("Bitcoin","Bitcoin","today 12-m")
Word_Bitcoin
Word_Blockchain<-GT_SCORE("Blockchain","Blockchain","today 12-m")
Word_Blockchain
Word_Altcoin<-GT_SCORE("Altcoin","Altcoin","today 12-m")
Word_Altcoin

Word_bear_Market_crypto<-GT_SCORE("bear market crypto","BUY BITCOIN","today 12-m")
Word_bear_Market_crypto
Word_bull_Market_crypto<-GT_SCORE("bull market crypto","BUY BITCOIN","today 12-m")
Word_bull_Market_crypto

Word_Bear_Market<-GT_SCORE("bear market","bear market","today 12-m")
Word_Bear_Market
Word_Bull_Market<-GT_SCORE("bull market","bull market","today 12-m")
Word_Bull_Market
Word_Recession<-GT_SCORE("recession","Recessão","today 12-m")
Word_Recession

words<-c("crypto","Bitcoin","Blockchain","Altcoin","bear market crypto","bull market crypto","bear market"
         ,"bull market","recession")
Score_GT_macro<-c(Word_crypto,Word_Bitcoin,Word_Blockchain,Word_Altcoin,Word_bear_Market_crypto,
                  Word_bull_Market_crypto,Word_Bear_Market,Word_Bull_Market,Word_Recession)

Macro_Economic_GT<-data.frame(words,Score_GT_macro)
write.csv2(Macro_Economic_GT,paste0(FILE00,"/#Macro_Economic_GT.csv"))

res <- gtrends(c("bull market", "bear market"), geo = c("US"))
res$related_queries
plot(res)
