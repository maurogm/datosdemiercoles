library(tidyverse)
library(stringr)

tiempo_pantalla <- data.table::fread("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/tiempo_pantalla.csv", encoding = 'UTF-8')
cambio_lealtades <- data.table::fread("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/cambio_lealtades.csv", encoding = 'UTF-8')
personajes_libros <- data.table::fread("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/personajes_libro.csv", encoding = 'UTF-8')

str(tiempo_pantalla)
str(cambio_lealtades)
str(personajes_libros)

lealtades_tidy <- cambio_lealtades %>% 
  gather(key = momento, value = lealtad, lealtad_inicial:fin_t7)

lealtades_tidy %>% 
  count(lealtad) %>% 
  arrange(desc(n)) %>% 
  print(n=Inf)
  
# Podría evitar sobreesoecificidad, como diferneciar a Joffrey de Tommen, o a Viserys de Dany

personajes_libros %>% 
  head()
  filter(str_detect(nombre, 'Aegon'))
