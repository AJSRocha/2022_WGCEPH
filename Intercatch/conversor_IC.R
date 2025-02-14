## Landings para WGCEPH2022

library(dplyr)
source('C://repos/path.R'); path('local')

ano = 2021

# Importa dados
land = read.csv(paste0(dados, 'desembarques//ceph_gux.txt'),
               sep = ",", dec = ".")

slv = read.csv(paste0(dados, 'especies_slv/slv.csv'))
portos = read.csv(paste0(dados, 'portos_slv//codigos_portos.csv'))

# acrescenta codigos fao da tabela SLV 
# land<-merge(land,slv[,c("ESPECIE_SLV","COD_FAO","FAMILIA")],all.x=T,all.y=F,by="ESPECIE_SLV")

# acrescenta portos slv
land =merge(land,portos[,c("codporto","nome","zona")],
      all.x = T,
      all.y = F,
      by.x = "PORTO_SLV",
      by.y = "codporto")

# restringe desembarques a codigos fao tirados do ASFIS
## no dia 26.04 foi acrescentado o SQU, que foi alterado para OMZ
fao = c("SQC","OCT","OMZ","CTC",
        "OCC","EOI","SQF","OFJ",
        "OUL","OUW","SQI","SQR",
        "OUM","OCM","OCZ","SQE",
        "SQM","YHT","OQD","SQZ",
        "CTL","TDQ","EDT","SQA",
        "OJJ","SQL","CTR","SQU")

# transforma tabela
land_export =
land %>%
  select(nome, zona, COD_FAO, ANO, MES, ARTE_EU, DESEMBARQUE) %>%
  filter(ANO == ano) %>%
  # remove artes espanholas
  filter(ARTE_EU %in% unique(land$ARTE_EU)[!grepl("SP_", unique(land$ARTE_EU))]) %>%
  filter(COD_FAO %in% fao) %>%
  mutate(zona = factor(case_when(zona == "NW" ~ "27.9.a.c.n",
                                 zona == "SW" ~ "27.9.a.c.s",
                                 T ~ "27.9.a.s.a")),
         # Acerta niveis com formato intercatch
         ARTE_EU = factor(case_when(ARTE_EU == 'DTRAWL' ~ "OTB",
                                    ARTE_EU == 'PSEINERS' ~ 'PS_SPF_0_0_0',
                                    T ~ 'MIS_MIS_0_0_0'))) %>%
  group_by(COD_FAO, zona, MES, ARTE_EU) %>%
  # desembarques à zona, em kg
  summarise(QESTIMADA = sum(DESEMBARQUE, na.rm = T))

# altera 'SQU' para 'OMZ' para ficar de acordo com o nome do stock do ICES
land_export[land_export$COD_FAO == 'SQU',]$COD_FAO = 'OMZ'  

# save(land, file="C://Google Drive//Polvices//WGCEPH 2020//desemb_mes_2019.Rdata")

for(j in unique(land_export$COD_FAO)){
  teste =data.frame()
  occ =land_export[land_export$COD_FAO==j,]
  for(i in 1:nrow(occ)){
      paste("HI,","PT,",ano,",","Month,",
        occ$MES[i],",",
        occ$ARTE_EU[i],",",
        "AreaUnit,",
        occ$zona[i],",",
        "NA,","NA,","-9,","NA",
        "\n",
        "SI,","PT,",ano,",","Month,",
        occ$MES[i],",",
        occ$ARTE_EU[i],",",
        "AreaUnit,",
        occ$zona[i],",",
        "NA,",
        occ$COD_FAO[i],",",
        "NA,","L,","R,","NA,","H,","U,","NA,","t,",
        occ$QESTIMADA[i]/1000,",",
        occ$QESTIMADA[i]/1000,",",
        "-9,",",,",
        sep="") -> teste[i,1]
  }
  
write.table(teste,
            file=paste0('Intercatch/IC',ano,j,"27_9a_PT_landings.dat",sep="_")
            ,sep="",row.names = F,col.names = F,quote=F)
}

# resumo de desembarques OTB
land_export %>% 
  filter(ARTE_EU == 'MIS_MIS_0_0_0') %>% 
  group_by(COD_FAO) %>%
  summarise(tons = sum(QESTIMADA/1000))
