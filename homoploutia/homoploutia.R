library(tidyverse)
library(dineq)
library(questionr)
library(extrafont)

rm(list=ls())

Data<-data.frame(matrix(nrow = length(2007:2018)*2,ncol = 4))
colnames(Data)<-c("año","Homoploutia","Renta autónomos","Base Datos")

for(i in 8:19){

  H<-read_csv(paste("esudb",i,"h.csv", sep=""))
  P<-read_csv(paste("esudb",i,"p.csv", sep=""))
  
  P<-P %>%
  mutate(HB030=PB030%/%100)
  
  PH <- P %>%
  left_join(y=H, by = "HB030")
  
  rm(list = setdiff(ls(), c("PH", "Data","i")))

# Calculos ----------------------------------------------------------------
Adultos<-PH%>%mutate(filaU=1)%>%group_by(HB030)%>%summarise(Adultos=sum(filaU))
PH <- PH %>%
  left_join(y=Adultos, by = "HB030")


if(i%in%c(10:19)){
Variables<-PH%>%dplyr::select(IdentificadorHogar=HB030,
                              IdentificadorPersona=PB030,
                              Adultos,
                              RentaDisponibleHogarINE=HY020,
                              SalarioBruto=PY010G,
                              SalarioEspecie=PY021G,
                              Autonomos=PY050G,
                              BInversiones=HY090G,
                              BAlquiler=HY040G,
                              PensionesPrivadas=PY080G,
                              Rentamenores=HY110G,
                              TransfJubilacion=PY100G,
                              TransfSupervivencia=PY110G,
                              AyudaNinnos=HY050G,
                              AsistenciaSocial=HY060G,
                              AyudaVivir=HY070G,
                              TransfDesempleo=PY090G,
                              TransfInvalidez=PY130G,
                              TransfEstudios=PY140G,
                              TransfEnfermedad=PY120G,
                              TransfOtrosHogares=HY080G,
                              IPatrimonio=HY120G,
                              IRPFCotizaciones=HY140G,
                              AOtrosHogares=HY130G,
                              Autoempleo=HY170N)


Variables<-Variables%>%transmute(IdentificadorHogar,
                                 IdentificadorPersona,
                                 RentasSalariales=SalarioBruto+SalarioEspecie+(Rentamenores/Adultos),
                                 RentasAutonomos=Autonomos+(Autoempleo/Adultos),
                                 RentasCapital=(BInversiones/Adultos)+(BAlquiler/Adultos)+PensionesPrivadas,
                                 PrestacionesJubilación=(TransfJubilacion+TransfSupervivencia),
                                 PrestacionesDesempleo=TransfDesempleo,
                                 OtrasPrestaciones=(AyudaNinnos/Adultos)+(AsistenciaSocial/Adultos)+(AyudaVivir/Adultos)+TransfEnfermedad+TransfInvalidez+TransfEstudios+(TransfOtrosHogares/Adultos),
                                 ImpuestosDirectos=(IPatrimonio+IRPFCotizaciones+AOtrosHogares)/Adultos)}else{
                                   Variables<-PH%>%dplyr::select(IdentificadorHogar=HB030,
                                IdentificadorPersona=PB030,
                                Adultos,
                                RentaDisponibleHogarINE=HY020,
                                SalarioBruto=PY010G,
                                SalarioEspecie=PY021G,
                                Autonomos=PY050G,
                                BInversiones=HY090G,
                                BAlquiler=HY040G,
                                PensionesPrivadas=PY080G,
                                Rentamenores=HY110G,
                                TransfJubilacion=PY100G,
                                TransfSupervivencia=PY110G,
                                AyudaNinnos=HY050G,
                                AsistenciaSocial=HY060G,
                                AyudaVivir=HY070G,
                                TransfDesempleo=PY090G,
                                TransfInvalidez=PY130G,
                                TransfEstudios=PY140G,
                                TransfEnfermedad=PY120G,
                                TransfOtrosHogares=HY080G,
                                IPatrimonio=HY120G,
                                IRPFCotizaciones=HY140G,
                                AOtrosHogares=HY130G)
  
  
  Variables<-Variables%>%transmute(IdentificadorHogar,
                                   IdentificadorPersona,
                                   RentasSalariales=SalarioBruto+SalarioEspecie+(Rentamenores/Adultos),
                                   RentasAutonomos=Autonomos,
                                   RentasCapital=(BInversiones/Adultos)+(BAlquiler/Adultos)+PensionesPrivadas,
                                   PrestacionesJubilación=(TransfJubilacion+TransfSupervivencia),
                                   PrestacionesDesempleo=TransfDesempleo,
                                   OtrasPrestaciones=(AyudaNinnos/Adultos)+(AsistenciaSocial/Adultos)+(AyudaVivir/Adultos)+TransfEnfermedad+TransfInvalidez+TransfEstudios+(TransfOtrosHogares/Adultos),
                                   ImpuestosDirectos=(IPatrimonio+IRPFCotizaciones+AOtrosHogares)/Adultos)}

VariablesHog<-Variables%>%group_by(IdentificadorHogar)%>%summarise(RentasSalariales=sum(RentasSalariales),
                                                                   RentasAutonomos=sum(RentasAutonomos),
                                                                   RentasCapital=sum(RentasCapital),
                                                                   PrestacionesJubilación=sum(PrestacionesJubilación),
                                                                   PrestacionesDesempleo=sum(PrestacionesDesempleo),
                                                                   OtrasPrestaciones=sum(OtrasPrestaciones),
                                                                   ImpuestosDirectos=sum(ImpuestosDirectos)
                                                                   )



D<-read_csv(paste("esudb",i,"d.csv", sep=""))

D1<-D%>%dplyr::select(IdentificadorHogar=DB030,
                      Ponderacion=DB090)

DF <- D1 %>%
  left_join(y=VariablesHog, by = "IdentificadorHogar")




# Autonomos en Trabajo ------------------------------------


Data[((i-7)*2-1),1]<-1999+i
DF1<-DF%>%mutate(RentasSalariales=RentasSalariales+RentasAutonomos+PrestacionesJubilación+PrestacionesDesempleo,
                 RentasCapital=RentasCapital,
                 LPosition=ntiles.wtd(x=RentasSalariales, w=Ponderacion, n = 10),
                 KPosition=ntiles.wtd(x=RentasCapital, w=Ponderacion, n = 10),
                 Homo=ifelse(KPosition==10,1,0))


Data[((i-7)*2-1),2]<-as.data.frame(prop.table(wtd.table((DF1%>%filter(LPosition==10))$Homo, w=(DF1%>%filter(LPosition==10))$Ponderacion))*100)[2,2]
Data[((i-7)*2-1),3]<-"En Trabajo"
Data[((i-7)*2-1),4]<-"ECV2013"

# Autónomos en capital -----------------------
Data[(i-7)*2,1]<-1999+i


DF2<-DF%>%mutate(RentasSalariales=RentasSalariales+PrestacionesJubilación+PrestacionesDesempleo,
                 RentasCapital=RentasCapital+RentasAutonomos,
                 LPosition=ntiles.wtd(x=RentasSalariales, w=Ponderacion, n = 10),
                 KPosition=ntiles.wtd(x=RentasCapital, w=Ponderacion, n = 10),
                 Homo=ifelse(KPosition==10,1,0))


Data[(i-7)*2,2]<-as.data.frame(prop.table(wtd.table((DF2%>%filter(LPosition==10))$Homo, w=(DF2%>%filter(LPosition==10))$Ponderacion))*100)[2,2]
Data[(i-7)*2,3]<-"En Capital"
Data[(i-7)*2,4]<-"ECV2013"
}

#font_import()
#loadfonts(device = "win")

Data%>%ggplot(aes(x=as.integer(año), y=Homoploutia, linetype=factor(`Renta autónomos`, levels = c("En Trabajo", "En Capital"))))+geom_line(size=0.75)+theme_classic()+
  labs(title="Homoploutia en España entre 2007-2018", subtitle="Rentas Pre-taxes", x="",y="top10K-top10L (%)", linetype="¿Rentas autónomos?")+
  theme(plot.title=element_text(h=0.5),
        plot.subtitle=element_text(h=0.5),
        axis.title.x=element_blank(),
        text = element_text(family = "serif", size = 16))




