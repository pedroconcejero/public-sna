# instalar slidify

# slidify creada por Ramnath Vaidyanathan
# http://ramnathv.github.io/slidify/

install.packages("devtools", dependencies = T)
library(devtools)

install_github('slidify', 'ramnathv')
install_github('slidifyLibraries', 'ramnathv')

# usar slidify

library(slidify)

# primero selecionamos un dir. de trabajo

setwd("C:/Users/pedroc/Desktop/madRid-R/slidify")

# Asignamos un nombre al proyecto c on author

author("gRupo_R_madRid_2")

# RECUERDA

library(knitr)

?browseURL
