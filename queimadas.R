library(stringr)
library(tidyverse)
library(readr)
library(lubridate)

#BAIXANDO A BASE

pantanal <- read_delim("historico_bioma_pantanal.csv", delim = ",", na = c("-", "NA"))

pantanal2 <- pantanal %>% 
  rename(Ano= X1)


pantanal_anos <- pantanal2%>%
  filter(str_detect(Ano, "19|20"))%>%
  dplyr::mutate(Ano = as.numeric(Ano))%>%
  select(!Total)

AnoMes1 <- lubridate::dmy("01-01-1998")+ months(0:275)


pantanal_long <- pantanal_anos%>%
  gather(key = "Meses", value= "Focos",-Ano)%>%
  arrange(Ano)%>%
  rename("mes_original"= Meses, "ano_original"= Ano)%>%
  mutate(AnoMes= AnoMes1, Mes= as.factor(month(AnoMes)), Ano=as.factor(year(AnoMes)))



#SIMBORA

meses_por_ano <- ggplot(pantanal_long, aes(x= AnoMes , y= Focos, color= mes_original)) + geom_line () + labs(title="Focos de Incêndio no Pantanal",caption = "Fonte: INPE")
meses_por_ano



longo_dos_anos <- ggplot(pantanal_long, aes(x= AnoMes , y= Focos)) + geom_line () +
  labs(title="Focos de Incêndio no Pantanal",caption = "Fonte: INPE", subtitle = "1998-2020") + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
longo_dos_anos

ano_por_mes <- ggplot(pantanal_long, aes(x= Mes , y= Focos, color= Ano, group= Ano)) + geom_line( size=1.2) + labs(title="Focos de Incêndio no Pantanal",caption = "Fonte: INPE")

ano_por_mes + theme_bw()
