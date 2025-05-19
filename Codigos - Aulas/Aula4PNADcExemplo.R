library(PNADcIBGE)
library(survey)
dadosPNADc <- get_pnadc(year = 2025, quarter = 1,
                        vars = c('V2007','V2010','VD4020'))
mediarenda <- svymean(x=~VD4020, design=dadosPNADc, na.rm=TRUE)
confint(object=mediarenda)
propsexo <- svymean(x=~V2007, design=dadosPNADc, na.rm=TRUE)
propsexoraca <- svytotal(x=~V2007+V2010, design=dadosPNADc, 
                         na.rm=TRUE)
medianarenda <- svyquantile(x=~VD4020, design=dadosPNADc,
                            quantiles=0.5, ci=FALSE, na.rm=TRUE)