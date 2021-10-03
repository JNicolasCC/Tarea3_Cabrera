library(tidyverse)
library(magrittr)
library(dplyr)
library(visdat)
a <- WDI::WDI(country = 'all', indicator = c("NY.ADJ.NNTY.PC.KD", "SP.DYN.LE00.IN"),
                     start = 2000, end = 2020, extra = TRUE)
a <- a[a$region != 'Aggregates', ]
library(countrycode)
a$country <- countrycode::countryname(sourcevar = a$country, destination = "un.name.es")
a %<>% rename(ing_percapita_ajustado=NY.ADJ.NNTY.PC.KD)
a %<>% rename(expectativa_vida= SP.DYN.LE00.IN)
library(naniar)
a %<>% drop_na(region)
a %<>% relocate(c(region,income, country, year, ing_percapita_ajustado,expectativa_vida))
bm_principales = a %>% select(country, year,region, income, ing_percapita_ajustado , expectativa_vida) 
(naniar::miss_var_summary(bm_principales))
gg_miss_fct(bm_principales,region)
gg_miss_fct(bm_principales,income)
c = bm_principales %>% filter(year==2015)
library(ggplot2)
ggplot(c,aes(ing_percapita_ajustado,expectativa_vida,color=region))+
  geom_point()
d = bm_principales %>% filter(country=="Colombia")
ggplot(d,aes(year,expectativa_vida)) + geom_line(color="purple")
e = bm_principales %>% filter(year==c(2000,2015))
ggplot(e,aes(expectativa_vida)) + geom_density(aes(color=year))+facet_wrap(vars(year))
ggplot(c,aes(expectativa_vida)) + geom_density(aes(color=region))+facet_wrap(vars(region))
library(scales)
f = readRDS("C:/Users/jnico/Downloads/saberTyTplot.Rds")
f %<>% drop_na(estu_estadocivil)
f %<>% filter(estu_estadocivil==c("soltero","unión libre","casado","separado y/o viudo","separado / viudo"))
levels(f$estu_estadocivil) = c(levels(f$estu_estadocivil),"sepoviu")
f = f %>% mutate(estu_estadocivil = replace(estu_estadocivil, estu_estadocivil == "separado y/o viudo","sepoviu"))
f = f %>% mutate(estu_estadocivil = replace(estu_estadocivil, estu_estadocivil == "separado / viudo","sepoviu"))
f$estu_estadocivil=factor(f$estu_estadocivil,levels = c("soltero","unión libre","casado","sepoviu"))
table(f$estu_estadocivil)
options(scipen = 99)
ggplot(f,aes(estu_estadocivil,stat(prop),group=1))+geom_bar(aes(fill="estu_estadocivil")) +
  scale_x_discrete(labels=c("Soltero","Union libre","Casado","Separado o viudo"),na.translate= FALSE)+
  scale_y_continuous(labels = label_percent()) + 
  scale_fill_brewer(palette = "Blues") + geom_text(aes(label = percent(..count../sum(..count..))),
                                        stat="count",position = position_stack(0.96),color="Black") +
  labs(title = "Estado civil de la muestra (%)",subtitle = "Estudiantes Saber 11",
       x="Estado civil",y="Porcentaje")