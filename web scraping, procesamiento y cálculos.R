
library(rvest)
library(RSelenium)
library(wdman)
library(stringr)
library(reshape2)

server <- phantomjs(port=5004L)
remDr <- remoteDriver(browserName = "phantomjs", port=5004L)
remDr$open()

partidos<-c("ACCIÓN POPULAR","APP","AVANZA PAÍS",
            "DEMOCRACIA DIRECTA","FRENTE AMPLIO",
            "FREPAP","FUERZA POPULAR","JUNTOS POR EL PERÚ",
            "SOMOS PERÚ","PARTIDO MORADO",
            "PARTIDO NACIONALISTA","CONTIGO","PPC",
            "PERÚ LIBRE","PERÚ PATRIA SEGURA",
            "PODEMOS PERÚ","RUNA","RENOVACIÓN POPULAR",
            "UNIÓN POR EL PERÚ","VICTORIA NACIONAL")

num_candidatos<-c(6,6,4,3,5,6,6,6,5,6,5,0,6,6,0,4,5,6,5,5)

for (j in which(num_candidatos==6)) {
  for (i in 1:6) {
    remDr$navigate("https://votoinformado.jne.gob.pe/voto/Home/Listaop")
    #remDr$setWindowSize(400, 600L)
    #remDr$screenshot(display = TRUE)

    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = paste0("body > div.vi-wrapper > div.vi-content-right > div > div.vi-wrapper-op > div:nth-child(",j,") > div > div.vi-item-back > a"))$clickElement()
    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = "body > div.vi-wrapper > div.vi-content-right > div > div.vi-wrapper-op-white > div.vi-wrapper-tabs > ul > li:nth-child(2)")$clickElement()
    Sys.sleep(4)
    remDr$findElement(using = "xpath", "//select[@id='cmbubigeos']/option[@value='110000']")$clickElement()

    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = "#btnBuscar")$clickElement()
    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = paste0("#infoCongresal > div > div:nth-child(",i,") > div.content-nombre-candidato > div.vi-nombre-candidato"))$clickElement()
    Sys.sleep(4)

    # Datos generales.
    tabla<-remDr$findElements(using = 'xpath',"/html/body/div[1]/div[2]/div/div[2]/div[2]/div[2]")
    tablas<-sapply(tabla, function(x) as.character(x$getElementText()))
    tablas<-unlist(str_split(tablas, "\n"))
    tablas<-tablas[-c(2,3,4,7)]
    tablas[1]<-paste("nombre:", tablas[1])
    tab1<-unlist(str_split(tablas, ":"))
    tab1<-as.data.frame(t(data.frame(var1=tab1[seq(1,14,2)], var2=tab1[seq(2,14,2)])))
    names(tab1)<-toupper(tab1[1,])
    tab1<-tab1[-1,]

    # Educación básica.
    tab2<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-basica") %>%
      html_table() %>% as.data.frame()

    tab2<-tab2[-1,]

    if(tab2[1,1]=="No registra información"){
      tab2$NOMBRE<- tab1$NOMBRE
      tab2<-tab2[,-1]
      names(tab2)[1]<-"NO_EDUC_BASIC"
    } else{
      names(tab2)<-c("GRADO_EDUC_B", "CONDICION")
      tab2$NOMBRE<-tab1$NOMBRE
      tab2<-dcast(tab2, NOMBRE~ GRADO_EDUC_B, value.var="CONDICION")
    }

    # Educación técnica.
    tab3<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-tecnico") %>%
      html_table() %>% as.data.frame()

    tab3<-tab3[-1,]

    if(tab3[1,1]=="No registra información"){
      tab3$NOMBRE<- tab1$NOMBRE
      tab3<-tab3[,-1]
      names(tab3)[1]<-"NO_TECN"
    } else{
      names(tab3)<-c("VAR1", "VAR2")
      tab3$NOMBRE<-tab1$NOMBRE
      tab3$VAR1<-paste0(tab3$VAR1,"_TECN",rep(seq(dim(tab3)[1]/3), each=3))
      tab3<-dcast(tab3, NOMBRE~ VAR1, value.var="VAR2")
    }

    # Educación no universitaria.
    tab4<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-nouniversitario") %>%
      html_table() %>% as.data.frame()

    tab4<-tab4[-1,]

    if(tab4[1,1]=="No registra información"){
      tab4$NOMBRE<- tab1$NOMBRE
      tab4<-tab4[,-1]
      names(tab4)[1]<-"NO_NOUNIV"
    } else{
      names(tab4)<-c("VAR1", "VAR2")
      tab4$NOMBRE<-tab1$NOMBRE
      tab4$VAR1<-paste0(tab4$VAR1,"_NOUNIV",rep(seq(dim(tab4)[1]/3), each=3))
      tab4<-dcast(tab4, NOMBRE~ VAR1, value.var="VAR2")
    }

    # Educación  universitaria.
    tab5<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-universitario") %>%
      html_table() %>% as.data.frame()

    tab5<-tab5[-1,]

    if(tab5[1,1]=="No registra información"){
      tab5$NOMBRE<- tab1$NOMBRE
      tab5<-tab5[,-1]
      names(tab5)[1]<-"NO_UNIVER"
    } else{
      names(tab5)<-c("VAR1", "VAR2")
      tab5$NOMBRE<-tab1$NOMBRE
      tab5$VAR1<-paste0(tab5$VAR1,"_UNIV",rep(seq(dim(tab5)[1]/3), each=3))
      tab5<-dcast(tab5, NOMBRE~ VAR1, value.var="VAR2")
    }

    # Educación  postgrado
    tab6<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-postgrado") %>%
      html_table() %>% as.data.frame()

    tab6<-tab6[-1,]

    if(tab6[1,1]=="No registra información"){
      tab6$NOMBRE<- tab1$NOMBRE
      tab6<-tab6[,-1]
      names(tab6)[1]<-"NO_POSTGRADO"
      tab6$PARTIDO<-partidos[j]
    } else{
      names(tab6)<-c("VAR1", "VAR2")
      tab6$NOMBRE<-tab1$NOMBRE
      tab6$VAR1<-paste0(tab6$VAR1,"_POSTGRADO",rep(seq(dim(tab6)[1]/4), each=4))
      tab6<-dcast(tab6, NOMBRE~ VAR1, value.var="VAR2")
      tab6$PARTIDO<-partidos[j]
    }

    # EXPERIENCIA LABORAL
    tab7<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-experiencia") %>%
      html_table() %>% as.data.frame()

    tab7<-tab7[-1,]

    if(tab7[1,1]=="No registra información"){
      tab7$NOMBRE<- tab1$NOMBRE
      tab7<-tab7[,-c(1:2)]
      names(tab7)[1]<-"NO_EXPERIENCIA"
    } else{
      names(tab7)<-c("CENTRO_DE_TRABAJO", "OCUPACION", "PERIODO")
      tab7$NOMBRE<-tab1$NOMBRE
      tab7<-melt(tab7, id.vars = "NOMBRE")
      tab7$variable<-paste0(tab7$variable,rep(seq(dim(tab7)[1]/3), 3))
      tab7<-dcast(tab7, NOMBRE~variable, value.var="value")
    }

    # INGRESOS.
    tab8<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-ingresos") %>%
      html_table() %>% as.data.frame()

    tab8<-tab8[-c(1:2),]

    if(tab8[1,1]=="No registra información"){
      tab8$NOMBRE<- tab1$NOMBRE
      tab8<-tab8[,-c(1:3)]
      names(tab8)[1]<-"NO_INGRESOS"
    } else{
      names(tab8)<-c("CONCEPTO", "SECT_PUBL","SECT_PRIV","TOTAL")
      tab8$NOMBRE<-tab1$NOMBRE
      tab8<-melt(tab8, id.vars = c("NOMBRE","CONCEPTO"))
      tab8$variable<-paste0(tab8$variable,rep(seq(dim(tab8)[1]/4), 4))
      tab8<-dcast(tab8, NOMBRE~CONCEPTO+variable, value.var=c("value"))
    }

    # BIENES INMUEBLES.
    tab9<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-inmuebles") %>%
      html_table() %>% as.data.frame()

    tab9<-tab9[-c(1:2),]

    if(tab9[1,1]=="No registra información"){
      tab9$NOMBRE<- tab1$NOMBRE
      tab9<-tab9[,-1]
      names(tab9)[1]<-"NO_BN_INMUEBLES"
    } else{
      names(tab9)<-c("TIPO_BN", "VALOR")
      tab9$NOMBRE<-tab1$NOMBRE
      tab9<-melt(tab9, id.vars = c("NOMBRE","TIPO_BN"))
      tab9$variable<-paste0(tab9$variable,"BN_INM_",rep(seq(dim(tab9)[1])))
      tab9<-dcast(tab9, NOMBRE~TIPO_BN+variable, value.var=c("value"))
    }

    # BIENES MUEBLES.
    tab10<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-muebles") %>%
      html_table() %>% as.data.frame()

    tab10<-tab10[-c(1:2),]

    if(tab10[1,1]=="No registra información"){
      tab10$NOMBRE<- tab1$NOMBRE
      tab10<-tab10[,-1]
      names(tab10)[1]<-"NO_BN_MUEBLES"
    } else{
      names(tab10)<-c("BIEN", "VALOR")
      tab10$NOMBRE<-tab1$NOMBRE
      tab10<-melt(tab10, id.vars = c("NOMBRE","BIEN"))
      tab10$variable<-paste0(tab10$variable,"BN_MUEB",rep(seq(dim(tab10)[1])))
      tab10<-dcast(tab10, NOMBRE~BIEN+variable, value.var=c("value"))
    }

    # BIENES OTROS.
    tab11<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-otros") %>%
      html_table() %>% as.data.frame()

    tab11<-tab11[-c(1:2),]

    if(is.na(tab11[1,1])){
      tab11<-data.frame(NOMBRE=tab1$NOMBRE, NO_BN_OTROS=NA)
    } else{
      names(tab11)<-c("BIEN", "VALOR")
      tab11$NOMBRE<-tab1$NOMBRE
      tab11<-melt(tab11, id.vars = c("NOMBRE","BIEN"))
      tab11$variable<-paste0(tab11$variable,"_OTROS",rep(seq(dim(tab11)[1])))
      tab11<-dcast(tab11, NOMBRE~BIEN+variable, value.var=c("value"))
    }

    assign(paste0("BASE_",j,"_",i),
           plyr::join_all(list(tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8,tab9,tab10,tab11),
                          by="NOMBRE", type = "left"))
  }
}

#------------------------------------
# Para los que tienen 5 candidatos.
#------------------------------------

for (j in which(num_candidatos==5)) {
  for (i in 1:5) {
    remDr$navigate("https://votoinformado.jne.gob.pe/voto/Home/Listaop")
    #remDr$setWindowSize(400, 600L)
    #remDr$screenshot(display = TRUE)

    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = paste0("body > div.vi-wrapper > div.vi-content-right > div > div.vi-wrapper-op > div:nth-child(",j,") > div > div.vi-item-back > a"))$clickElement()
    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = "body > div.vi-wrapper > div.vi-content-right > div > div.vi-wrapper-op-white > div.vi-wrapper-tabs > ul > li:nth-child(2)")$clickElement()
    Sys.sleep(4)
    remDr$findElement(using = "xpath", "//select[@id='cmbubigeos']/option[@value='110000']")$clickElement()

    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = "#btnBuscar")$clickElement()
    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = paste0("#infoCongresal > div > div:nth-child(",i,") > div.content-nombre-candidato > div.vi-nombre-candidato"))$clickElement()
    Sys.sleep(4)

    # Datos generales.
    tabla<-remDr$findElements(using = 'xpath',"/html/body/div[1]/div[2]/div/div[2]/div[2]/div[2]")
    tablas<-sapply(tabla, function(x) as.character(x$getElementText()))
    tablas<-unlist(str_split(tablas, "\n"))
    tablas<-tablas[-c(2,3,4,7)]
    tablas[1]<-paste("nombre:", tablas[1])
    tab1<-unlist(str_split(tablas, ":"))
    tab1<-as.data.frame(t(data.frame(var1=tab1[seq(1,14,2)], var2=tab1[seq(2,14,2)])))
    names(tab1)<-toupper(tab1[1,])
    tab1<-tab1[-1,]

    # Educación básica.
    tab2<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-basica") %>%
      html_table() %>% as.data.frame()

    tab2<-tab2[-1,]

    if(tab2[1,1]=="No registra información"){
      tab2$NOMBRE<- tab1$NOMBRE
      tab2<-tab2[,-1]
      names(tab2)[1]<-"NO_EDUC_BASIC"
    } else{
      names(tab2)<-c("GRADO_EDUC_B", "CONDICION")
      tab2$NOMBRE<-tab1$NOMBRE
      tab2<-dcast(tab2, NOMBRE~ GRADO_EDUC_B, value.var="CONDICION")
    }

    # Educación técnica.
    tab3<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-tecnico") %>%
      html_table() %>% as.data.frame()

    tab3<-tab3[-1,]

    if(tab3[1,1]=="No registra información"){
      tab3$NOMBRE<- tab1$NOMBRE
      tab3<-tab3[,-1]
      names(tab3)[1]<-"NO_TECN"
    } else{
      names(tab3)<-c("VAR1", "VAR2")
      tab3$NOMBRE<-tab1$NOMBRE
      tab3$VAR1<-paste0(tab3$VAR1,"_TECN",rep(seq(dim(tab3)[1]/3), each=3))
      tab3<-dcast(tab3, NOMBRE~ VAR1, value.var="VAR2")
    }

    # Educación no universitaria.
    tab4<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-nouniversitario") %>%
      html_table() %>% as.data.frame()

    tab4<-tab4[-1,]

    if(tab4[1,1]=="No registra información"){
      tab4$NOMBRE<- tab1$NOMBRE
      tab4<-tab4[,-1]
      names(tab4)[1]<-"NO_NOUNIV"
    } else{
      names(tab4)<-c("VAR1", "VAR2")
      tab4$NOMBRE<-tab1$NOMBRE
      tab4$VAR1<-paste0(tab4$VAR1,"_NOUNIV",rep(seq(dim(tab4)[1]/3), each=3))
      tab4<-dcast(tab4, NOMBRE~ VAR1, value.var="VAR2")
    }

    # Educación  universitaria.
    tab5<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-universitario") %>%
      html_table() %>% as.data.frame()

    tab5<-tab5[-1,]

    if(tab5[1,1]=="No registra información"){
      tab5$NOMBRE<- tab1$NOMBRE
      tab5<-tab5[,-1]
      names(tab5)[1]<-"NO_UNIVER"
    } else{
      names(tab5)<-c("VAR1", "VAR2")
      tab5$NOMBRE<-tab1$NOMBRE
      tab5$VAR1<-paste0(tab5$VAR1,"_UNIV",rep(seq(dim(tab5)[1]/3), each=3))
      tab5<-dcast(tab5, NOMBRE~ VAR1, value.var="VAR2")
    }

    # Educación  postgrado
    tab6<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-postgrado") %>%
      html_table() %>% as.data.frame()

    tab6<-tab6[-1,]

    if(tab6[1,1]=="No registra información"){
      tab6$NOMBRE<- tab1$NOMBRE
      tab6<-tab6[,-1]
      names(tab6)[1]<-"NO_POSTGRADO"
      tab6$PARTIDO<-partidos[j]
    } else{
      names(tab6)<-c("VAR1", "VAR2")
      tab6$NOMBRE<-tab1$NOMBRE
      tab6$VAR1<-paste0(tab6$VAR1,"_POSTGRADO",rep(seq(dim(tab6)[1]/4), each=4))
      tab6<-dcast(tab6, NOMBRE~ VAR1, value.var="VAR2")
      tab6$PARTIDO<-partidos[j]
    }

    # EXPERIENCIA LABORAL
    tab7<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-experiencia") %>%
      html_table() %>% as.data.frame()

    tab7<-tab7[-1,]

    if(tab7[1,1]=="No registra información"){
      tab7$NOMBRE<- tab1$NOMBRE
      tab7<-tab7[,-c(1:2)]
      names(tab7)[1]<-"NO_EXPERIENCIA"
    } else{
      names(tab7)<-c("CENTRO_DE_TRABAJO", "OCUPACION", "PERIODO")
      tab7$NOMBRE<-tab1$NOMBRE
      tab7<-melt(tab7, id.vars = "NOMBRE")
      tab7$variable<-paste0(tab7$variable,rep(seq(dim(tab7)[1]/3), 3))
      tab7<-dcast(tab7, NOMBRE~variable, value.var="value")
    }

    # INGRESOS.
    tab8<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-ingresos") %>%
      html_table() %>% as.data.frame()

    tab8<-tab8[-c(1:2),]

    if(tab8[1,1]=="No registra información"){
      tab8$NOMBRE<- tab1$NOMBRE
      tab8<-tab8[,-c(1:3)]
      names(tab8)[1]<-"NO_INGRESOS"
    } else{
      names(tab8)<-c("CONCEPTO", "SECT_PUBL","SECT_PRIV","TOTAL")
      tab8$NOMBRE<-tab1$NOMBRE
      tab8<-melt(tab8, id.vars = c("NOMBRE","CONCEPTO"))
      tab8$variable<-paste0(tab8$variable,rep(seq(dim(tab8)[1]/4), 4))
      tab8<-dcast(tab8, NOMBRE~CONCEPTO+variable, value.var=c("value"))
    }

    # BIENES INMUEBLES.
    tab9<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-inmuebles") %>%
      html_table() %>% as.data.frame()

    tab9<-tab9[-c(1:2),]

    if(tab9[1,1]=="No registra información"){
      tab9$NOMBRE<- tab1$NOMBRE
      tab9<-tab9[,-1]
      names(tab9)[1]<-"NO_BN_INMUEBLES"
    } else{
      names(tab9)<-c("TIPO_BN", "VALOR")
      tab9$NOMBRE<-tab1$NOMBRE
      tab9<-melt(tab9, id.vars = c("NOMBRE","TIPO_BN"))
      tab9$variable<-paste0(tab9$variable,"BN_INM_",rep(seq(dim(tab9)[1])))
      tab9<-dcast(tab9, NOMBRE~TIPO_BN+variable, value.var=c("value"))
    }

    # BIENES MUEBLES.
    tab10<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-muebles") %>%
      html_table() %>% as.data.frame()

    tab10<-tab10[-c(1:2),]

    if(tab10[1,1]=="No registra información"){
      tab10$NOMBRE<- tab1$NOMBRE
      tab10<-tab10[,-1]
      names(tab10)[1]<-"NO_BN_MUEBLES"
    } else{
      names(tab10)<-c("BIEN", "VALOR")
      tab10$NOMBRE<-tab1$NOMBRE
      tab10<-melt(tab10, id.vars = c("NOMBRE","BIEN"))
      tab10$variable<-paste0(tab10$variable,"BN_MUEB",rep(seq(dim(tab10)[1])))
      tab10<-dcast(tab10, NOMBRE~BIEN+variable, value.var=c("value"))
    }

    # BIENES OTROS.
    tab11<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-otros") %>%
      html_table() %>% as.data.frame()

    tab11<-tab11[-c(1:2),]

    if(is.na(tab11[1,1])){
      tab11<-data.frame(NOMBRE=tab1$NOMBRE, NO_BN_OTROS=NA)
    } else{
      names(tab11)<-c("BIEN", "VALOR")
      tab11$NOMBRE<-tab1$NOMBRE
      tab11<-melt(tab11, id.vars = c("NOMBRE","BIEN"))
      tab11$variable<-paste0(tab11$variable,"_OTROS",rep(seq(dim(tab11)[1])))
      tab11<-dcast(tab11, NOMBRE~BIEN+variable, value.var=c("value"))
    }

    assign(paste0("BASE_",j,"_",i),
           plyr::join_all(list(tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8,tab9,tab10,tab11),
                          by="NOMBRE", type = "left"))
  }
}

#------------------------------------
# Para los que tienen 4 candidatos.
#------------------------------------

for (j in which(num_candidatos==4)) {
  for (i in 1:4) {
    remDr$navigate("https://votoinformado.jne.gob.pe/voto/Home/Listaop")
    #remDr$setWindowSize(400, 600L)
    #remDr$screenshot(display = TRUE)

    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = paste0("body > div.vi-wrapper > div.vi-content-right > div > div.vi-wrapper-op > div:nth-child(",j,") > div > div.vi-item-back > a"))$clickElement()
    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = "body > div.vi-wrapper > div.vi-content-right > div > div.vi-wrapper-op-white > div.vi-wrapper-tabs > ul > li:nth-child(2)")$clickElement()
    Sys.sleep(4)
    remDr$findElement(using = "xpath", "//select[@id='cmbubigeos']/option[@value='110000']")$clickElement()

    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = "#btnBuscar")$clickElement()
    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = paste0("#infoCongresal > div > div:nth-child(",i,") > div.content-nombre-candidato > div.vi-nombre-candidato"))$clickElement()
    Sys.sleep(4)

    # Datos generales.
    tabla<-remDr$findElements(using = 'xpath',"/html/body/div[1]/div[2]/div/div[2]/div[2]/div[2]")
    tablas<-sapply(tabla, function(x) as.character(x$getElementText()))
    tablas<-unlist(str_split(tablas, "\n"))
    tablas<-tablas[-c(2,3,4,7)]
    tablas[1]<-paste("nombre:", tablas[1])
    tab1<-unlist(str_split(tablas, ":"))
    tab1<-as.data.frame(t(data.frame(var1=tab1[seq(1,14,2)], var2=tab1[seq(2,14,2)])))
    names(tab1)<-toupper(tab1[1,])
    tab1<-tab1[-1,]

    # Educación básica.
    tab2<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-basica") %>%
      html_table() %>% as.data.frame()

    tab2<-tab2[-1,]

    if(tab2[1,1]=="No registra información"){
      tab2$NOMBRE<- tab1$NOMBRE
      tab2<-tab2[,-1]
      names(tab2)[1]<-"NO_EDUC_BASIC"
    } else{
      names(tab2)<-c("GRADO_EDUC_B", "CONDICION")
      tab2$NOMBRE<-tab1$NOMBRE
      tab2<-dcast(tab2, NOMBRE~ GRADO_EDUC_B, value.var="CONDICION")
    }

    # Educación técnica.
    tab3<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-tecnico") %>%
      html_table() %>% as.data.frame()

    tab3<-tab3[-1,]

    if(tab3[1,1]=="No registra información"){
      tab3$NOMBRE<- tab1$NOMBRE
      tab3<-tab3[,-1]
      names(tab3)[1]<-"NO_TECN"
    } else{
      names(tab3)<-c("VAR1", "VAR2")
      tab3$NOMBRE<-tab1$NOMBRE
      tab3$VAR1<-paste0(tab3$VAR1,"_TECN",rep(seq(dim(tab3)[1]/3), each=3))
      tab3<-dcast(tab3, NOMBRE~ VAR1, value.var="VAR2")
    }

    # Educación no universitaria.
    tab4<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-nouniversitario") %>%
      html_table() %>% as.data.frame()

    tab4<-tab4[-1,]

    if(tab4[1,1]=="No registra información"){
      tab4$NOMBRE<- tab1$NOMBRE
      tab4<-tab4[,-1]
      names(tab4)[1]<-"NO_NOUNIV"
    } else{
      names(tab4)<-c("VAR1", "VAR2")
      tab4$NOMBRE<-tab1$NOMBRE
      tab4$VAR1<-paste0(tab4$VAR1,"_NOUNIV",rep(seq(dim(tab4)[1]/3), each=3))
      tab4<-dcast(tab4, NOMBRE~ VAR1, value.var="VAR2")
    }

    # Educación  universitaria.
    tab5<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-universitario") %>%
      html_table() %>% as.data.frame()

    tab5<-tab5[-1,]

    if(tab5[1,1]=="No registra información"){
      tab5$NOMBRE<- tab1$NOMBRE
      tab5<-tab5[,-1]
      names(tab5)[1]<-"NO_UNIVER"
    } else{
      names(tab5)<-c("VAR1", "VAR2")
      tab5$NOMBRE<-tab1$NOMBRE
      tab5$VAR1<-paste0(tab5$VAR1,"_UNIV",rep(seq(dim(tab5)[1]/3), each=3))
      tab5<-dcast(tab5, NOMBRE~ VAR1, value.var="VAR2")
    }

    # Educación  postgrado
    tab6<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-postgrado") %>%
      html_table() %>% as.data.frame()

    tab6<-tab6[-1,]

    if(tab6[1,1]=="No registra información"){
      tab6$NOMBRE<- tab1$NOMBRE
      tab6<-tab6[,-1]
      names(tab6)[1]<-"NO_POSTGRADO"
      tab6$PARTIDO<-partidos[j]
    } else{
      names(tab6)<-c("VAR1", "VAR2")
      tab6$NOMBRE<-tab1$NOMBRE
      tab6$VAR1<-paste0(tab6$VAR1,"_POSTGRADO",rep(seq(dim(tab6)[1]/4), each=4))
      tab6<-dcast(tab6, NOMBRE~ VAR1, value.var="VAR2")
      tab6$PARTIDO<-partidos[j]
    }

    # EXPERIENCIA LABORAL
    tab7<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-experiencia") %>%
      html_table() %>% as.data.frame()

    tab7<-tab7[-1,]

    if(tab7[1,1]=="No registra información"){
      tab7$NOMBRE<- tab1$NOMBRE
      tab7<-tab7[,-c(1:2)]
      names(tab7)[1]<-"NO_EXPERIENCIA"
    } else{
      names(tab7)<-c("CENTRO_DE_TRABAJO", "OCUPACION", "PERIODO")
      tab7$NOMBRE<-tab1$NOMBRE
      tab7<-melt(tab7, id.vars = "NOMBRE")
      tab7$variable<-paste0(tab7$variable,rep(seq(dim(tab7)[1]/3), 3))
      tab7<-dcast(tab7, NOMBRE~variable, value.var="value")
    }

    # INGRESOS.
    tab8<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-ingresos") %>%
      html_table() %>% as.data.frame()

    tab8<-tab8[-c(1:2),]

    if(tab8[1,1]=="No registra información"){
      tab8$NOMBRE<- tab1$NOMBRE
      tab8<-tab8[,-c(1:3)]
      names(tab8)[1]<-"NO_INGRESOS"
    } else{
      names(tab8)<-c("CONCEPTO", "SECT_PUBL","SECT_PRIV","TOTAL")
      tab8$NOMBRE<-tab1$NOMBRE
      tab8<-melt(tab8, id.vars = c("NOMBRE","CONCEPTO"))
      tab8$variable<-paste0(tab8$variable,rep(seq(dim(tab8)[1]/4), 4))
      tab8<-dcast(tab8, NOMBRE~CONCEPTO+variable, value.var=c("value"))
    }

    # BIENES INMUEBLES.
    tab9<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-inmuebles") %>%
      html_table() %>% as.data.frame()

    tab9<-tab9[-c(1:2),]

    if(tab9[1,1]=="No registra información"){
      tab9$NOMBRE<- tab1$NOMBRE
      tab9<-tab9[,-1]
      names(tab9)[1]<-"NO_BN_INMUEBLES"
    } else{
      names(tab9)<-c("TIPO_BN", "VALOR")
      tab9$NOMBRE<-tab1$NOMBRE
      tab9<-melt(tab9, id.vars = c("NOMBRE","TIPO_BN"))
      tab9$variable<-paste0(tab9$variable,"BN_INM_",rep(seq(dim(tab9)[1])))
      tab9<-dcast(tab9, NOMBRE~TIPO_BN+variable, value.var=c("value"))
    }

    # BIENES MUEBLES.
    tab10<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-muebles") %>%
      html_table() %>% as.data.frame()

    tab10<-tab10[-c(1:2),]

    if(tab10[1,1]=="No registra información"){
      tab10$NOMBRE<- tab1$NOMBRE
      tab10<-tab10[,-1]
      names(tab10)[1]<-"NO_BN_MUEBLES"
    } else{
      names(tab10)<-c("BIEN", "VALOR")
      tab10$NOMBRE<-tab1$NOMBRE
      tab10<-melt(tab10, id.vars = c("NOMBRE","BIEN"))
      tab10$variable<-paste0(tab10$variable,"BN_MUEB",rep(seq(dim(tab10)[1])))
      tab10<-dcast(tab10, NOMBRE~BIEN+variable, value.var=c("value"))
    }

    # BIENES OTROS.
    tab11<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-otros") %>%
      html_table() %>% as.data.frame()

    tab11<-tab11[-c(1:2),]

    if(is.na(tab11[1,1])){
      tab11<-data.frame(NOMBRE=tab1$NOMBRE, NO_BN_OTROS=NA)
    } else{
      names(tab11)<-c("BIEN", "VALOR")
      tab11$NOMBRE<-tab1$NOMBRE
      tab11<-melt(tab11, id.vars = c("NOMBRE","BIEN"))
      tab11$variable<-paste0(tab11$variable,"_OTROS",rep(seq(dim(tab11)[1])))
      tab11<-dcast(tab11, NOMBRE~BIEN+variable, value.var=c("value"))
    }

    assign(paste0("BASE_",j,"_",i),
           plyr::join_all(list(tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8,tab9,tab10,tab11),
                          by="NOMBRE", type = "left"))
  }
}

#------------------------------------
# Para los que tienen 3 candidatos.
#------------------------------------

for (j in which(num_candidatos==3)) {
  for (i in 1:3) {
    remDr$navigate("https://votoinformado.jne.gob.pe/voto/Home/Listaop")
    #remDr$setWindowSize(400, 600L)
    #remDr$screenshot(display = TRUE)

    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = paste0("body > div.vi-wrapper > div.vi-content-right > div > div.vi-wrapper-op > div:nth-child(",j,") > div > div.vi-item-back > a"))$clickElement()
    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = "body > div.vi-wrapper > div.vi-content-right > div > div.vi-wrapper-op-white > div.vi-wrapper-tabs > ul > li:nth-child(2)")$clickElement()
    Sys.sleep(4)
    remDr$findElement(using = "xpath", "//select[@id='cmbubigeos']/option[@value='110000']")$clickElement()

    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = "#btnBuscar")$clickElement()
    Sys.sleep(4)
    remDr$findElement(using = 'css',
                      value = paste0("#infoCongresal > div > div:nth-child(",i,") > div.content-nombre-candidato > div.vi-nombre-candidato"))$clickElement()
    Sys.sleep(4)

    # Datos generales.
    tabla<-remDr$findElements(using = 'xpath',"/html/body/div[1]/div[2]/div/div[2]/div[2]/div[2]")
    tablas<-sapply(tabla, function(x) as.character(x$getElementText()))
    tablas<-unlist(str_split(tablas, "\n"))
    tablas<-tablas[-c(2,3,4,7)]
    tablas[1]<-paste("nombre:", tablas[1])
    tab1<-unlist(str_split(tablas, ":"))
    tab1<-as.data.frame(t(data.frame(var1=tab1[seq(1,14,2)], var2=tab1[seq(2,14,2)])))
    names(tab1)<-toupper(tab1[1,])
    tab1<-tab1[-1,]

    # Educación básica.
    tab2<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-basica") %>%
      html_table() %>% as.data.frame()

    tab2<-tab2[-1,]

    if(tab2[1,1]=="No registra información"){
      tab2$NOMBRE<- tab1$NOMBRE
      tab2<-tab2[,-1]
      names(tab2)[1]<-"NO_EDUC_BASIC"
    } else{
      names(tab2)<-c("GRADO_EDUC_B", "CONDICION")
      tab2$NOMBRE<-tab1$NOMBRE
      tab2<-dcast(tab2, NOMBRE~ GRADO_EDUC_B, value.var="CONDICION")
    }

    # Educación técnica.
    tab3<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-tecnico") %>%
      html_table() %>% as.data.frame()

    tab3<-tab3[-1,]

    if(tab3[1,1]=="No registra información"){
      tab3$NOMBRE<- tab1$NOMBRE
      tab3<-tab3[,-1]
      names(tab3)[1]<-"NO_TECN"
    } else{
      names(tab3)<-c("VAR1", "VAR2")
      tab3$NOMBRE<-tab1$NOMBRE
      tab3$VAR1<-paste0(tab3$VAR1,"_TECN",rep(seq(dim(tab3)[1]/3), each=3))
      tab3<-dcast(tab3, NOMBRE~ VAR1, value.var="VAR2")
    }

    # Educación no universitaria.
    tab4<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-nouniversitario") %>%
      html_table() %>% as.data.frame()

    tab4<-tab4[-1,]

    if(tab4[1,1]=="No registra información"){
      tab4$NOMBRE<- tab1$NOMBRE
      tab4<-tab4[,-1]
      names(tab4)[1]<-"NO_NOUNIV"
    } else{
      names(tab4)<-c("VAR1", "VAR2")
      tab4$NOMBRE<-tab1$NOMBRE
      tab4$VAR1<-paste0(tab4$VAR1,"_NOUNIV",rep(seq(dim(tab4)[1]/3), each=3))
      tab4<-dcast(tab4, NOMBRE~ VAR1, value.var="VAR2")
    }

    # Educación  universitaria.
    tab5<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-universitario") %>%
      html_table() %>% as.data.frame()

    tab5<-tab5[-1,]

    if(tab5[1,1]=="No registra información"){
      tab5$NOMBRE<- tab1$NOMBRE
      tab5<-tab5[,-1]
      names(tab5)[1]<-"NO_UNIVER"
    } else{
      names(tab5)<-c("VAR1", "VAR2")
      tab5$NOMBRE<-tab1$NOMBRE
      tab5$VAR1<-paste0(tab5$VAR1,"_UNIV",rep(seq(dim(tab5)[1]/3), each=3))
      tab5<-dcast(tab5, NOMBRE~ VAR1, value.var="VAR2")
    }

    # Educación  postgrado
    tab6<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-postgrado") %>%
      html_table() %>% as.data.frame()

    tab6<-tab6[-1,]

    if(tab6[1,1]=="No registra información"){
      tab6$NOMBRE<- tab1$NOMBRE
      tab6<-tab6[,-1]
      names(tab6)[1]<-"NO_POSTGRADO"
      tab6$PARTIDO<-partidos[j]
    } else{
      names(tab6)<-c("VAR1", "VAR2")
      tab6$NOMBRE<-tab1$NOMBRE
      tab6$VAR1<-paste0(tab6$VAR1,"_POSTGRADO",rep(seq(dim(tab6)[1]/4), each=4))
      tab6<-dcast(tab6, NOMBRE~ VAR1, value.var="VAR2")
      tab6$PARTIDO<-partidos[j]
    }

    # EXPERIENCIA LABORAL
    tab7<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-experiencia") %>%
      html_table() %>% as.data.frame()

    tab7<-tab7[-1,]

    if(tab7[1,1]=="No registra información"){
      tab7$NOMBRE<- tab1$NOMBRE
      tab7<-tab7[,-c(1:2)]
      names(tab7)[1]<-"NO_EXPERIENCIA"
    } else{
      names(tab7)<-c("CENTRO_DE_TRABAJO", "OCUPACION", "PERIODO")
      tab7$NOMBRE<-tab1$NOMBRE
      tab7<-melt(tab7, id.vars = "NOMBRE")
      tab7$variable<-paste0(tab7$variable,rep(seq(dim(tab7)[1]/3), 3))
      tab7<-dcast(tab7, NOMBRE~variable, value.var="value")
    }

    # INGRESOS.
    tab8<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-ingresos") %>%
      html_table() %>% as.data.frame()

    tab8<-tab8[-c(1:2),]

    if(tab8[1,1]=="No registra información"){
      tab8$NOMBRE<- tab1$NOMBRE
      tab8<-tab8[,-c(1:3)]
      names(tab8)[1]<-"NO_INGRESOS"
    } else{
      names(tab8)<-c("CONCEPTO", "SECT_PUBL","SECT_PRIV","TOTAL")
      tab8$NOMBRE<-tab1$NOMBRE
      tab8<-melt(tab8, id.vars = c("NOMBRE","CONCEPTO"))
      tab8$variable<-paste0(tab8$variable,rep(seq(dim(tab8)[1]/4), 4))
      tab8<-dcast(tab8, NOMBRE~CONCEPTO+variable, value.var=c("value"))
    }

    # BIENES INMUEBLES.
    tab9<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-inmuebles") %>%
      html_table() %>% as.data.frame()

    tab9<-tab9[-c(1:2),]

    if(tab9[1,1]=="No registra información"){
      tab9$NOMBRE<- tab1$NOMBRE
      tab9<-tab9[,-1]
      names(tab9)[1]<-"NO_BN_INMUEBLES"
    } else{
      names(tab9)<-c("TIPO_BN", "VALOR")
      tab9$NOMBRE<-tab1$NOMBRE
      tab9<-melt(tab9, id.vars = c("NOMBRE","TIPO_BN"))
      tab9$variable<-paste0(tab9$variable,"BN_INM_",rep(seq(dim(tab9)[1])))
      tab9<-dcast(tab9, NOMBRE~TIPO_BN+variable, value.var=c("value"))
    }

    # BIENES MUEBLES.
    tab10<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-muebles") %>%
      html_table() %>% as.data.frame()

    tab10<-tab10[-c(1:2),]

    if(tab10[1,1]=="No registra información"){
      tab10$NOMBRE<- tab1$NOMBRE
      tab10<-tab10[,-1]
      names(tab10)[1]<-"NO_BN_MUEBLES"
    } else{
      names(tab10)<-c("BIEN", "VALOR")
      tab10$NOMBRE<-tab1$NOMBRE
      tab10<-melt(tab10, id.vars = c("NOMBRE","BIEN"))
      tab10$variable<-paste0(tab10$variable,"BN_MUEB",rep(seq(dim(tab10)[1])))
      tab10<-dcast(tab10, NOMBRE~BIEN+variable, value.var=c("value"))
    }

    # BIENES OTROS.
    tab11<-remDr$getPageSource()[[1]] %>%
      read_html() %>% html_nodes("#tab-bienes-otros") %>%
      html_table() %>% as.data.frame()

    tab11<-tab11[-c(1:2),]

    if(is.na(tab11[1,1])){
      tab11<-data.frame(NOMBRE=tab1$NOMBRE, NO_BN_OTROS=NA)
    } else{
      names(tab11)<-c("BIEN", "VALOR")
      tab11$NOMBRE<-tab1$NOMBRE
      tab11<-melt(tab11, id.vars = c("NOMBRE","BIEN"))
      tab11$variable<-paste0(tab11$variable,"_OTROS",rep(seq(dim(tab11)[1])))
      tab11<-dcast(tab11, NOMBRE~BIEN+variable, value.var=c("value"))
    }

    assign(paste0("BASE_",j,"_",i),
           plyr::join_all(list(tab1,tab2,tab3,tab4,tab5,tab6,tab7,tab8,tab9,tab10,tab11),
                          by="NOMBRE", type = "left"))
  }
}


# Uniendo todo.

datosd<-lapply(ls(pattern ="^BASE_.*"), get)

BFINAL<-plyr::join_all(datosd, type = "full")

names(BFINAL)<-toupper(names(BFINAL))
names(BFINAL)<-gsub(" ","_",names(BFINAL))

#openxlsx::write.xlsx(BFINAL,"F:/info_candidatos3.xlsx")

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

BFINAL<-readxl::read_xlsx("F:/info_candidatos3.xlsx")

bdata<-BFINAL
library(datametria)
bdata[,1:length(bdata)]<-apply(bdata[,1:length(bdata)], 2, limpiecito)
bdata<-as.data.frame(bdata)

#--------------------
# Educación Básica.
#--------------------
bdata$SECUNDARIA<-ifelse(bdata$SECUNDARIA=="","NO",bdata$SECUNDARIA)

#---------------------
# Educación Técnica.
#---------------------
# Número de estudios técnicos.
bdata$NUM_EDUC_TEC<-ifelse(!is.na(bdata$CENTRO_DE_ESTUDIOS_TECN1), 1, 0)

bdata$NUM_EDUC_TEC<-as.numeric(bdata$NUM_EDUC_TEC)

#----------------------------------------------------------------------------------------
# Cargando la base de clasificador de carreras de la ENAHO.
enaho_6<-readxl::read_xlsx("F:/ENAHO-CLASIFICADOR DE CARRERAS DE EDUC SUPERIOR Y TECNICO PRODUCTIVAS.xlsx",
                           sheet = "Nivel 6")

enaho_5<-readxl::read_xlsx("F:/ENAHO-CLASIFICADOR DE CARRERAS DE EDUC SUPERIOR Y TECNICO PRODUCTIVAS.xlsx",
                           sheet = "Nivel 5")

enaho_4<-readxl::read_xlsx("F:/ENAHO-CLASIFICADOR DE CARRERAS DE EDUC SUPERIOR Y TECNICO PRODUCTIVAS.xlsx",
                           sheet = "Nivel 4")

enaho_3<-readxl::read_xlsx("F:/ENAHO-CLASIFICADOR DE CARRERAS DE EDUC SUPERIOR Y TECNICO PRODUCTIVAS.xlsx",
                           sheet = "Nivel 3")

enaho<-rbind(enaho_6,enaho_5, enaho_4, enaho_3)
enaho<-enaho[!is.na(enaho$fefe),]
enaho<-enaho[!duplicated(enaho),]
enaho<-enaho[is.na(enaho$cod),]
enaho$cod<-NULL
enaho<-enaho[!grepl("^\\d{1}/",enaho$fefe),]

enaho$CARRERA<-gsub("\\d{6} ","", enaho$fefe)
enaho$CODIGO<-substring(enaho$fefe, 1,6)
enaho$fefe<-NULL

enaho[,1:2]<-apply(enaho[,1:2],2,limpiecito)

enaho$CARRERA<-gsub("\\d{1}/","",enaho$CARRERA)
enaho[,1:2]<-apply(enaho[,1:2],2,limpiecito)
enaho<-enaho[!duplicated(enaho$CARRERA),]
#----------------------------------------------------------------------------------------

# Codificando carreras técnicas.
bdata$CARRERA_TECN1_ST<-bdata$CARRERA_TECN1

bdata$CARRERA_TECN1<-gsub(".*TECNIC(O|A) EN |DOCENTE |TECNICO |TECNICO DE ","", bdata$CARRERA_TECN1)
bdata$CARRERA_TECN1<-gsub("AGROPECUARIO","AGROPECUARIA",bdata$CARRERA_TECN1)
library(dplyr)
bdata<-left_join(bdata, enaho, by=c("CARRERA_TECN1"="CARRERA"))

sum(table(bdata$CODIGO))

bdata[!is.na(bdata$CARRERA_TECN1) & is.na(bdata$CODIGO),][,c("CARRERA_TECN1","CODIGO")]

bdata$CODIGO<-ifelse(bdata$CARRERA_TECN1=="OPERADOR DE MICROCOMPUTADORAS","441135",
                     ifelse(bdata$CARRERA_TECN1=="SUB OFICIAL", "021015",bdata$CODIGO))

# NO ES UNA CARRERA "-"
names(bdata)[length(bdata)]<-"CODIGO_TEC1"

#------------------------------
# Carreras no universitarias.
#------------------------------

# Número de estudios no universitarios.
bdata$NUM_EDUC_NOUNIV<-ifelse(!is.na(bdata$CENTRO_DE_ESTUDIOS_NOUNIV1), 1, 0)

bdata$NUM_EDUC_NOUNIV<-as.numeric(bdata$NUM_EDUC_NOUNIV)

# Codificación de carreras.
bdata$CARRERA_NOUNIV1_ST<-bdata$CARRERA_NOUNIV1

bdata$CARRERA_NOUNIV1<-gsub("PROFESOR DE |PROFESORA DE |TECNICO EN |PROFESORA ","", bdata$CARRERA_NOUNIV1)

bdata<-left_join(bdata, enaho, by=c("CARRERA_NOUNIV1"="CARRERA"))

sum(table(bdata$CODIGO))

bdata[!is.na(bdata$CARRERA_NOUNIV1) & is.na(bdata$CODIGO),][,c("CARRERA_NOUNIV1","CODIGO")]

bdata$CODIGO<-ifelse(bdata$CARRERA_NOUNIV1=="SECRETARIADO MEDICO EJECUTIVO","337025",
                     ifelse(grepl("OFICIAL", bdata$CARRERA_NOUNIV1), "021015", bdata$CODIGO))
# NO ES UNA CARRERA EL "DIPLOMADO EN RECURSOS HUMANOS POR COPENTECIAS".

names(bdata)[length(bdata)]<-"CODIGO_NOUNIV1"

#---------------------------
# Carreras Universitarias.
#---------------------------

# Para el número de carreras.
for (i in 1:5) {
  bdata[,paste0("num_uni",i)]<-ifelse(!is.na(bdata[,paste0("CARRERA_UNIV",i)]), 1, 0)
}

bdata$NUM_EDUC_UNIV<-rowSums(bdata[,126:130])

bdata$NUM_EDUC_UNIV<-as.numeric(bdata$NUM_EDUC_UNIV)

for (i in 1:5) {
  bdata[,paste0("num_uni",i)]<-NULL
}

# Codificación de carreras.
bdata$CARRERA_UNIV1_ST<-bdata$CARRERA_UNIV1

bdata$CARRERA_UNIV1<-gsub("LICENCIAD(O|A) EN |BACHILLER EN |.*MENCION|.*PROFESIONAL |ESPECIALISTA EN ",
                          "", bdata$CARRERA_UNIV1)
bdata$CARRERA_UNIV1<-gsub("INGENIER(O|A)", "INGENIERIA",bdata$CARRERA_UNIV1)
bdata$CARRERA_UNIV1<-gsub("BACHILLER Y |.* ESPECIALIDAD DE |CIRUJANO |.*ESPECIALIDAD: |FACULTAD DE |LIC ",
                          "", bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("ABOGAD(O|A)|POLITICAS",bdata$CARRERA_UNIV1),
                            "DERECHO",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("PSICOLOGA",bdata$CARRERA_UNIV1),
                            "PSICOLOGIA",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("CIRUGIA|RADIOLOGIA|CIRUJ",bdata$CARRERA_UNIV1),
                            "MEDICINA HUMANA",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("CONTADOR",bdata$CARRERA_UNIV1),
                            "CONTABILIDAD",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("PEDAGOGIA Y HUMANIDADES|EDUCACION Y CIENCIAS HUMANAS|INGLES",bdata$CARRERA_UNIV1),
                            "EDUCACION",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("CIENCIAS SOCIALES E HISTORIA",bdata$CARRERA_UNIV1),
                            "EDUCACION SECUNDARIA - CIENCIAS SOCIALES - HISTORIA",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("INGENIERIA EN INDUSTRIAS ALIMENTARIAS",bdata$CARRERA_UNIV1),
                            "INGENIERIA DE INDUSTRIAS ALIMENTARIAS",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("ECONOMIA Y FINANSAS",bdata$CARRERA_UNIV1),
                            "ECONOMIA Y FINANZAS",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("OBSTETRIZ",bdata$CARRERA_UNIV1),
                            "OBSTETRICIA",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("DENTISTA",bdata$CARRERA_UNIV1),
                            "ODONTOLOGIA",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("DENTISTA",bdata$CARRERA_UNIV1),
                            "ODONTOLOGIA",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("METALURGICO",bdata$CARRERA_UNIV1),
                            "INGENIERIA METALURGICA",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("QUIMICO",bdata$CARRERA_UNIV1),
                            "INGENIERIA QUIMICA",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("AGRONOMO",bdata$CARRERA_UNIV1),
                            "INGENIERIA AGRONOMA",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("ELECTRICISTA",bdata$CARRERA_UNIV1),
                            "INGENIERIA ELECTRICA",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("IDIOMAS Y TURISMO|TURISMO Y ADMINISTRACION HOTELERA",bdata$CARRERA_UNIV1),
                            "TURISMO",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("INGLES",bdata$CARRERA_UNIV1),
                            "TURISMO",bdata$CARRERA_UNIV1)

bdata$CARRERA_UNIV1<-ifelse(grepl("CIENCIAS MILITARES",bdata$CARRERA_UNIV1),
                            "CIENCIAS MILITARES (EJERCITO)",bdata$CARRERA_UNIV1)

bdata<-left_join(bdata, enaho, by=c("CARRERA_UNIV1"="CARRERA"))

sum(table(bdata$CODIGO))

bdata[!is.na(bdata$CARRERA_UNIV1) & is.na(bdata$CODIGO),][,c("CARRERA_UNIV1","CODIGO")]

names(bdata)[length(bdata)]<-"CODIGO_UNIV1"

# HAY CARRERAS CON EL NOMBRE, SIN GRADO, BACHILLER, 8 CICLO, PROCESO Y "-". QUE NO SE TOMAN EN CUENTA.

#------------------------------------
# Segundas carreras universitarias.
#------------------------------------
bdata$CARRERA_UNIV2_ST<-bdata$CARRERA_UNIV2

bdata$CARRERA_UNIV2<-gsub("LICENCIAD(O|A) EN |BACHILLER EN |.*MENCION|BACH ",
                          "", bdata$CARRERA_UNIV2)

bdata$CARRERA_UNIV2<-ifelse(grepl("ABOGAD(O|A)|DERECHO",bdata$CARRERA_UNIV2),
                            "DERECHO",bdata$CARRERA_UNIV2)

bdata$CARRERA_UNIV2<-ifelse(grepl("ECONOMISTA",bdata$CARRERA_UNIV2),
                            "ECONOMIA",bdata$CARRERA_UNIV2)

bdata$CARRERA_UNIV2<-gsub("INGENIER(O|A)", "INGENIERIA",bdata$CARRERA_UNIV2)

bdata$CARRERA_UNIV2<-ifelse(grepl("QUIMICO",bdata$CARRERA_UNIV2),
                            "INGENIERIA QUIMICA",bdata$CARRERA_UNIV2)

bdata$CARRERA_UNIV2<-ifelse(grepl("MEDICO",bdata$CARRERA_UNIV2),
                            "MEDICINA",bdata$CARRERA_UNIV2)

bdata$CARRERA_UNIV2<-ifelse(grepl("MATEMATICA Y FISICA",bdata$CARRERA_UNIV2),
                            "EDUCACION",bdata$CARRERA_UNIV2)

bdata<-left_join(bdata, enaho, by=c("CARRERA_UNIV2"="CARRERA"))

sum(table(bdata$CODIGO))

bdata[!is.na(bdata$CARRERA_UNIV2) & is.na(bdata$CODIGO),][,c("CARRERA_UNIV2","CODIGO")]

# HAY CARRERA CON NOMBRE INCONCLUSA.

names(bdata)[length(bdata)]<-"CODIGO_UNIV2"

#------------------------------------
# Terceras carreras universitarias.
#------------------------------------

bdata$CARRERA_UNIV3_ST<-bdata$CARRERA_UNIV3

bdata$CARRERA_UNIV3<-ifelse(grepl("ENFERMERIA",bdata$CARRERA_UNIV3),
                            "ENFERMERIA",bdata$CARRERA_UNIV3)

bdata$CARRERA_UNIV3<-ifelse(grepl("DERECHO",bdata$CARRERA_UNIV3),
                            "DERECHO",bdata$CARRERA_UNIV3)

bdata$CARRERA_UNIV3<-ifelse(grepl("PSICOLOGIA",bdata$CARRERA_UNIV3),
                            "PSICOLOGIA",bdata$CARRERA_UNIV3)

bdata<-left_join(bdata, enaho, by=c("CARRERA_UNIV3"="CARRERA"))

sum(table(bdata$CODIGO))

bdata[!is.na(bdata$CARRERA_UNIV3) & is.na(bdata$CODIGO),][,c("CARRERA_UNIV3","CODIGO")]

names(bdata)[length(bdata)]<-"CODIGO_UNIV3"

#------------------------------------
# Cuartas carreras universitarias.
#------------------------------------

bdata$CARRERA_UNIV4_ST<-bdata$CARRERA_UNIV4

bdata$CARRERA_UNIV4<-ifelse(grepl("EDUCACION",bdata$CARRERA_UNIV4),
                            "EDUCACION",bdata$CARRERA_UNIV4)

bdata<-left_join(bdata, enaho, by=c("CARRERA_UNIV4"="CARRERA"))

sum(table(bdata$CODIGO))

bdata[!is.na(bdata$CARRERA_UNIV4) & is.na(bdata$CODIGO),][,c("CARRERA_UNIV4","CODIGO")]

names(bdata)[length(bdata)]<-"CODIGO_UNIV4"
#------------------------------------
# Quintas carreras universitarias.
#------------------------------------

bdata$CARRERA_UNIV5_ST<-bdata$CARRERA_UNIV5

bdata$CARRERA_UNIV5<-ifelse(grepl("POLICIALES",bdata$CARRERA_UNIV5),
                            "ADMINISTRACION Y CIENCIAS POLICIALES (OFICIALES)",bdata$CARRERA_UNIV5)

bdata<-left_join(bdata, enaho, by=c("CARRERA_UNIV5"="CARRERA"))

sum(table(bdata$CODIGO))

bdata[!is.na(bdata$CARRERA_UNIV5) & is.na(bdata$CODIGO),][,c("CARRERA_UNIV5","CODIGO")]

names(bdata)[length(bdata)]<-"CODIGO_UNIV5"

#----------------------
# Educación postgrado.
#----------------------
bdata$NUM_EDUC_POSTGRADO<-ifelse(!is.na(bdata$CENTRO_DE_ESTUDIOS_POSTGRADO1), 1, 0)

bdata$NUM_EDUC_POSTGRADO<-as.numeric(bdata$NUM_EDUC_POSTGRADO)

#-------------------------------------------------
# Experiencia laboral.
#-------------------------------------------------

# Para el número de carreras.
for (i in 1:5) {
  bdata[,paste0("num_trabajo",i)]<-ifelse(!is.na(bdata[,paste0("CENTRO_DE_TRABAJO",i)]), 1, 0)
}

bdata$NUM_TRABAJOS<-rowSums(bdata[,138:142])

for (i in 1:5) {
  bdata[,paste0("num_trabajo",i)]<-NULL
}

# Periodo de trabajo.

for (i in 1:5) {
  bdata[,paste0("PERIODO_TRA",i)]<-gsub("HASTA LA ACTUALIDAD", "2021", bdata[,paste0("PERIODO",i)])
}

for (i in 1:5) {
  bdata[,paste0("PERIODO_TRA_INICIO",i)]<-as.numeric(gsub("-.*", "", bdata[,paste0("PERIODO_TRA",i)]))
}

for (i in 1:5) {
  bdata[,paste0("PERIODO_TRA_FINAL",i)]<-as.numeric(gsub(".*-", "", bdata[,paste0("PERIODO_TRA",i)]))
}
#----------------------
# Años de experiencia.
#----------------------
for (i in 1:5) {
  bdata[,paste0("AÑOS_EXPERIENCIA",i)]<-ifelse(bdata[,paste0("PERIODO_TRA_INICIO",i)]==bdata[,paste0("PERIODO_TRA_FINAL",i)],
                                               bdata[,paste0("PERIODO_TRA_FINAL",i)]-bdata[,paste0("PERIODO_TRA_INICIO",i)]+1,
                                               bdata[,paste0("PERIODO_TRA_FINAL",i)]-bdata[,paste0("PERIODO_TRA_INICIO",i)])
}

# Consolidar años es un poco complicado.
# Algunos truquillos.
library(matrixStats)
bdata$MIN_AÑO_EXPERIENCIA<-rowMins(as.matrix(bdata[,144:148]), na.rm = T)
bdata$MAX_AÑO_EXPERIENCIA<-rowMaxs(as.matrix(bdata[,149:153]), na.rm = T)

bdata$MIN_AÑO_EXPERIENCIA<-ifelse(bdata$MIN_AÑO_EXPERIENCIA==Inf, NA,
                                  bdata$MIN_AÑO_EXPERIENCIA)
bdata$MAX_AÑO_EXPERIENCIA<-ifelse(bdata$MAX_AÑO_EXPERIENCIA==-Inf, NA,
                                  bdata$MAX_AÑO_EXPERIENCIA)

# Máximo menos mínimo.
bdata$AÑO_EXPERIENCIA_A<-bdata$MAX_AÑO_EXPERIENCIA-bdata$MIN_AÑO_EXPERIENCIA

# Suma de años.
bdata$AÑO_EXPERIENCIA_B<-rowSums(bdata[,154:158], na.rm = T)

bdata$AÑOS_DE_EXPERIENCIA<-ifelse(bdata$NUM_TRABAJOS==1, bdata$AÑO_EXPERIENCIA_B,
                                  ifelse(bdata$AÑO_EXPERIENCIA_B!=0 & bdata$AÑO_EXPERIENCIA_B==bdata$AÑO_EXPERIENCIA_A,
                                         bdata$AÑO_EXPERIENCIA_B,
                                         ifelse(bdata$AÑO_EXPERIENCIA_B!=0 & bdata$AÑO_EXPERIENCIA_B!=bdata$AÑO_EXPERIENCIA_A,
                                                rowMins(as.matrix(bdata[,161:162]), na.rm = T),bdata$AÑO_EXPERIENCIA_B)))

bdata$AÑOS_DE_EXPERIENCIA<-ifelse(grepl("70440561|40890823|41604502|07472882|42816821", bdata$DNI), 3,
                                  ifelse(grepl("76684792", bdata$DNI), 4,
                                         ifelse(grepl("44817057|20709981|20117146",bdata$DNI),5,bdata$AÑOS_DE_EXPERIENCIA)))

bdata$AÑOS_DE_EXPERIENCIA<-as.numeric(bdata$AÑOS_DE_EXPERIENCIA)

# write.csv(bdata[,c(1,2,138,139:143,161:163)],"F:/experiencia.csv")

#-------------------
# Ingresos.

# Los montos a numérico.
# Conociendo que columnas son monedas.
t<-c()
for (i in 1:dim(bdata)[2]) {
  if(sum(grepl("S/ \\d+",bdata[,i]))!=0){
    sip<-i
  } else{
    sip<-NULL
  }
  t<-c(t,sip)
}

# Eliminando el S/
bdata[,1:length(bdata)]<-apply(bdata[,1:length(bdata)],2, function(x){
  x<-gsub("S/ ","", x)
  x<-limpiecito(x)
})

# Convirtiendo a numérico.
for (i in t) {
  bdata[,i]<-as.numeric(gsub(",","",bdata[,i]))
}

#---------------------------------------
# TOTAL DE INGRESOS DEL SECTOR PÚBLICO.
#---------------------------------------
bdata$TOTAL_ING_SECT_PUBLICO<-rowSums(bdata[c("REMUNERACIÓN_BRUTA_ANUAL_SECT_PUBL1",
                                              "RENTA_BRUTA_ANUAL_POR_EJERCICIO_INDIVIDUAL_SECT_PUBL2",
                                              "OTROS_INGRESOS_ANUALES_SECT_PUBL3")], na.rm = T)

#---------------------------------------
# TOTAL DE INGRESOS DEL SECTOR PRIVADO.
#---------------------------------------
bdata$TOTAL_ING_SECT_PRIVADO<-rowSums(bdata[c("REMUNERACIÓN_BRUTA_ANUAL_SECT_PRIV2",
                                              "RENTA_BRUTA_ANUAL_POR_EJERCICIO_INDIVIDUAL_SECT_PRIV3",
                                              "OTROS_INGRESOS_ANUALES_SECT_PRIV1")], na.rm = T)

#---------------------------------------
# INGRESOS TOTALES.
#---------------------------------------
bdata$TOTAL_ING<-bdata$TOTAL_ING_SECT_PRIVADO+bdata$TOTAL_ING_SECT_PUBLICO

#--------------------
# Bienes inmuebles.
#--------------------
# El número de bienes inmuebles.
nombres<-names(bdata[,grepl("BN_INM_",names(bdata))])

for (i in 1:length(nombres)) {
  bdata[,paste0("num_inmuebl",i)]<-ifelse(!is.na(bdata[,nombres[i]]), 1, 0)
}

bdata$NUM_BN_INMUEBLES<-rowSums(bdata[,paste0("num_inmuebl",1:length(nombres))])

for (i in 1:length(nombres)) {
  bdata[,paste0("num_inmuebl",i)]<-NULL
}

bdata$VALOR_BN_INMUEBLE_TOTAL<-rowSums(bdata[,nombres], na.rm = T)

#--------------------
# Bienes muebles.
#--------------------
# El número de bienes muebles.
nombres<-names(bdata[,grepl("BN_MUEB\\d",names(bdata))])

for (i in 1:length(nombres)) {
  bdata[,paste0("num_muebl",i)]<-ifelse(!is.na(bdata[,nombres[i]]), 1, 0)
}

bdata$NUM_BN_MUEBLES<-rowSums(bdata[,paste0("num_muebl",1:length(nombres))])

for (i in 1:length(nombres)) {
  bdata[,paste0("num_muebl",i)]<-NULL
}

bdata$VALOR_BN_MUEBLES_TOTAL<-rowSums(bdata[,nombres], na.rm = T)

#--------------------
# Bienes otros.
#--------------------
# El número de bienes muebles.
nombres<-names(bdata[,grepl("OTROS_",names(bdata))])

# No hay bienes otros.


####################################################################################
####################################################################################
####################################################################################
####################################################################################

#-----------------------------------------------------------------------------------
#                               GRÁFICOS Y TABLAS.
#-----------------------------------------------------------------------------------
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(extrafont)

#------------------------
# PRIMARIA.
#------------------------
table(bdata$PRIMARIA)

#------------------------
# SECUNDARIA.
#------------------------

bdata$SECUNDARIA<-ifelse(is.na(bdata$SECUNDARIA), "NO","SI")


bdata %>% group_by(PARTIDO) %>% summarise(n=n()) %>%
  ggplot(aes(x=reorder(PARTIDO,-n),y=n))+
  geom_col(col="dodgerblue4", fill="dodgerblue4")+
  labs(title = "NÚMERO DE CANDIDATOS POR PARTIDO POLÍTICO",
       subtitle = "Candidatos con información en el portal Voto informado.",
       y="Número de candidatos",
       caption = "Los partidos Patria Segura y Contigo no presentan candidatos\nELABORADO POR:R para economistas.")+
  geom_text(aes(label = n),
            position=position_stack(vjust=1.06), hjust = 0.5,
            size=4, col="black")+
  geom_text(aes(x=14,y=6.5,label = "Número total de candidatos\n95"),
            size=5, col="red")+
  scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1))+
  theme_economist()+
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1, size = 8),
        plot.caption = element_text(hjust = 0, size=7, face="bold"))
#---------------------------------------------
circulos<-function(var1,var2,name1,name2){
  t<- seq(0, 2*pi, length.out = 1000)
  radio <-1.5
  a<- 2
  b<- 0
  a1<- -2
  b1<- 0
  x <- a + cos(t)*radio
  y <- b + sin(t)*radio

  x1<- a1 + cos(t)*radio
  y1<- b1 + sin(t)*radio
  plot(0, 0, asp = 1, type = "n",
       xlim = c(-2, 2), ylim = c(-2, 2),
       ann = F, bty="n", xaxt="n", yaxt="n")

  polygon(x, y, col = "deepskyblue2")
  polygon(x1, y1, col = "dodgerblue4")

  text(a1,b1, var1, cex=5, col = "white")
  text(a1,b1-radio-0.3, name1, cex=0.8)
  text(a,b, var2, cex=5, col = "white")
  text(a,b-radio-0.3, name2, cex=0.8)
}

circulos(paste0(table(bdata$PRIMARIA)),
         paste0(table(bdata$SECUNDARIA, exclude = "NO")),
         "CANDIDATOS CON\nEDUCACIÓN PRIMARIA",
         "CANDIDATOS CON\nEDUCACIÓN SECUNDARIA")

bdata[bdata$SECUNDARIA=="NO",][,c("NOMBRE","PARTIDO")]
#-------------------------------
# NÚMERO DE ESTUDIOS TÉCNICOS.
#-------------------------------
table(bdata$NUM_EDUC_TEC, exclude = 0)

#----------------------------------------
# NÚMERO DE ESTUDIOS NO UNIVERSITARIOS.
#----------------------------------------
table(bdata$NUM_EDUC_NOUNIV, exclude = 0)

circulos(table(bdata$NUM_EDUC_TEC, exclude = 0),
         table(bdata$NUM_EDUC_NOUNIV, exclude = 0),
         "CANDIDATOS CON\nESTUDIOS TÉCNICOS",
         "CANDIDATOS CON\nESTUDIOS NO UNIVERSITARIOS")

bdata[bdata$NUM_EDUC_TEC==1,][,c("NOMBRE","PARTIDO","CARRERA_TECN1_ST")]
bdata[bdata$NUM_EDUC_NOUNIV==1,][,c("NOMBRE","PARTIDO","CARRERA_NOUNIV1_ST")]

bdata %>% group_by(PARTIDO) %>%
  filter(NUM_EDUC_TEC==1) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(PARTIDO,-n),y=n))+
  geom_col(col="dodgerblue4", fill="dodgerblue4")+
  labs(title = "NÚMERO DE CANDIDATOS CON ESTUDIOS TÉCNICOS\nPOR PARTIDO POLÍTICO",
       subtitle = "Carreras concluidas o sin concluir.",
       y="Número de candidatos",
       caption = "ELABORADO POR:R para economistas.")+
  geom_text(aes(label = n),
            position=position_stack(vjust=1.1), hjust = 0.5,
            size=4, col="black")+
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3,1))+
  theme_economist()+
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1, size = 8),
        plot.caption = element_text(hjust = 0, size=7, face="bold"))

bdata %>% group_by(PARTIDO) %>%
  filter(NUM_EDUC_NOUNIV==1) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(PARTIDO,-n),y=n))+
  geom_col(col="dodgerblue4", fill="dodgerblue4")+
  labs(title = "NÚMERO DE CANDIDATOS CON ESTUDIOS NO UNIVERSITARIOS\nPOR PARTIDO POLÍTICO",
       subtitle = "Carreras concluidas o sin concluir.",
       y="Número de candidatos",
       caption = "ELABORADO POR:R para economistas.")+
  geom_text(aes(label = n),
            position=position_stack(vjust=1.1), hjust = 0.5,
            size=4, col="black")+
  scale_y_continuous(limits = c(0,3), breaks = seq(0,3,1))+
  theme_economist()+
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1, size = 8),
        plot.caption = element_text(hjust = 0, size=7, face="bold"))

#----------------------------------------
# NÚMERO DE CARRERAS UNIVERSITARIAS.
#----------------------------------------
table(bdata$NUM_EDUC_UNIV)

c(table(bdata$NUM_EDUC_UNIV)[1],sum(table(bdata$NUM_EDUC_UNIV)[2:5]))

#----------------------------------------
# NÚMERO DE ESTUDIANTES DE POSTGRADO.
#----------------------------------------
table(bdata$NUM_EDUC_POSTGRADO, exclude = 0)

circulos(sum(table(bdata$NUM_EDUC_UNIV)[2:5]),
         table(bdata$NUM_EDUC_POSTGRADO, exclude = 0),
         "CANDIDATOS CON\nESTUDIOS UNIVERSITARIOS",
         "CANDIDATOS CON\nESTUDIOS DE POSTGRADO")

bdata[bdata$NUM_EDUC_POSTGRADO==1,][,c("NOMBRE","PARTIDO","ESPECIALIZACIÓN_POSTGRADO1")]

bdata %>% group_by(PARTIDO) %>%
  filter(NUM_EDUC_UNIV>=1) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(PARTIDO,-n),y=n))+
  geom_col(col="dodgerblue4", fill="dodgerblue4")+
  labs(title = "NÚMERO DE CANDIDATOS CON ESTUDIOS UNIVERSITARIOS\nPOR PARTIDO POLÍTICO",
       subtitle = "Carreras concluidas o sin concluir.",
       y="Número de candidatos",
       caption = "ELABORADO POR:R para economistas.")+
  geom_text(aes(label = n),
            position=position_stack(vjust=1.11),
            size=4, col="black")+
  scale_y_continuous(limits = c(0,7), breaks = seq(0,7,1))+
  theme_economist()+
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1, size = 8),
        plot.caption = element_text(hjust = 0, size=7, face="bold"))

bdata %>% group_by(PARTIDO) %>%
  filter(NUM_EDUC_POSTGRADO==1) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(PARTIDO,-n),y=n))+
  geom_col(col="dodgerblue4", fill="dodgerblue4")+
  labs(title = "NÚMERO DE CANDIDATOS CON ESTUDIOS DE POSTGRADO\nPOR PARTIDO POLÍTICO",
       subtitle = "Carreras concluidas o sin concluir.",
       y="Número de candidatos",
       caption = "ELABORADO POR:R para economistas.")+
  geom_text(aes(label = n),
            position=position_stack(vjust=1.1),
            size=4, col="black")+
  scale_y_continuous(breaks = seq(0,5,1))+
  theme_economist()+
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1, size = 8),
        plot.caption = element_text(hjust = 0, size=7, face="bold"))

#----------------------------------------
# NÚMERO DE TRABAJOS.
#----------------------------------------
table(bdata$NUM_TRABAJOS)

va<-data.frame(Freq=c(table(bdata$NUM_TRABAJOS)[1],
                      sum(table(bdata$NUM_TRABAJOS)[2:6])),
               Var1=c("NO","SI"))
va$Freq<-va$Freq/dim(bdata)[1]
row.names(va)<-NULL

ggplot(va,aes(x=2,y=Freq, fill=Var1))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=scales::percent(Freq)),
            position=position_stack(vjust=0.5), color="black",
            size=5, fontface="bold")+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("deepskyblue2", "dodgerblue4"),
                    name="")+
  theme_economist()+
  labs(title="CANDIDATOS QUE HAN TRABAJADO",
       subtitle = "Porcentaje del total de candidatos.")+
  xlim(0.5,2.5)+
  theme(legend.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, face="bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid  = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

bdata[bdata$NUM_TRABAJOS==0,][,c("NOMBRE","PARTIDO")]

#----------------------------------------
# AÑOS DE EXPERIENCIA.
#----------------------------------------
va<-as.data.frame(table(bdata$AÑOS_DE_EXPERIENCIA))

ggplot(va,aes(x=reorder(Var1,-Freq),y=Freq))+
  geom_col(col="dodgerblue4", fill="dodgerblue4")+
  labs(title = "AÑOS DE EXPERIENCIA DE LOS CANDIDATOS",
       subtitle = "Experiencia en el sector público y/o privado.",
       x="Años de experiencia", y="Candidatos",
       caption = "ELABORADO POR:R para economistas.")+
  scale_y_continuous(breaks = seq(0,15,2))+
  theme_economist()+
  theme(plot.caption = element_text(hjust = 0, size=7, face="bold"))

#----------------------------------------
# INGRESOS POR SECTOR PÚBLICO.
#----------------------------------------
table(bdata$TOTAL_ING_SECT_PUBLICO)

bdata %>% filter(TOTAL_ING_SECT_PUBLICO!=0) %>%
  group_by(PARTIDO) %>% summarise(n=n()) %>%
  ggplot(aes(x=reorder(PARTIDO,-n),y=n))+
  geom_col(col="dodgerblue4", fill="dodgerblue4")+
  labs(title = "CANDIDATOS QUE OBTIENEN INGRESOS POR LABORAR\nEN EL SECTOR PÚBLICO POR PARTIDO POLÍTICO",
       subtitle = "Ingresos anuales.",
       y="Número de candidatos", x="",
       caption = "ELABORADO POR:R para economistas.")+
  scale_y_continuous(breaks = seq(0,6,1))+
  theme_economist()+
  theme(plot.caption = element_text(hjust = 0, size=7, face="bold"),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1, size = 7.5))

#----------------------------------------
# INGRESOS POR SECTOR PRIVADO.
#----------------------------------------
table(bdata$TOTAL_ING_SECT_PRIVADO)

bdata %>% filter(TOTAL_ING_SECT_PRIVADO!=0) %>%
  group_by(PARTIDO) %>% summarise(n=n()) %>%
  ggplot(aes(x=reorder(PARTIDO,-n),y=n))+
  geom_col(col="dodgerblue4", fill="dodgerblue4")+
  labs(title = "CANDIDATOS QUE OBTIENEN INGRESOS POR LABORAR\nEN EL SECTOR PRIVADO POR PARTIDO POLÍTICO",
       subtitle = "Ingresos anuales.",
       y="Número de candidatos", x="",
       caption = "ELABORADO POR:R para economistas.")+
  scale_y_continuous(breaks = seq(0,6,1))+
  theme_economist()+
  theme(plot.caption = element_text(hjust = 0, size=7, face="bold"),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1, size = 7.5))

#----------------------------------------
# INGRESOS TOTALES ANUALES.
#----------------------------------------
table(bdata$TOTAL_ING)

bdata %>% filter(TOTAL_ING!=0) %>%
  group_by(PARTIDO) %>% summarise(n=n()) %>%
  ggplot(aes(x=reorder(PARTIDO,-n),y=n))+
  geom_col(col="dodgerblue4", fill="dodgerblue4")+
  labs(title = "CANDIDATOS QUE OBTIENEN INGRESOS LABORALES\nPOR PARTIDO POLÍTICO**",
       subtitle = "Ingresos anuales.",
       y="Número de candidatos", x="",
       caption = "**Hay Candidatos que no indicaron experiencia laboral pero si ingresos.\nELABORADO POR:R para economistas.")+
  scale_y_continuous(breaks = seq(0,6,1))+
  theme_economist()+
  theme(plot.caption = element_text(hjust = 0, size=7, face="bold"),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1, size = 7.5))

bdata[bdata$TOTAL_ING!=0 & bdata$NUM_TRABAJOS==0,][,c("NOMBRE","PARTIDO","TOTAL_ING")]

#----------------------------------------
# NÚMERO DE BIENES INMUEBLES.
#----------------------------------------
table(bdata$NUM_BN_INMUEBLES)

c(table(bdata$NUM_BN_INMUEBLES)[1],sum(table(bdata$NUM_BN_INMUEBLES)[2:6]))

bdata %>% filter(NUM_BN_INMUEBLES!=0) %>%
  group_by(PARTIDO) %>% summarise(n=n()) %>%
  ggplot(aes(x=reorder(PARTIDO,-n),y=n))+
  geom_col(col="dodgerblue4", fill="dodgerblue4")+
  labs(title = "CANDIDATOS QUE DECLARAN TENER BIENES INMUEBLES\nPOR PARTIDO POLÍTICO**",
       subtitle = "Número de candidatos.",
       y="Número de candidatos", x="",
       caption = "**Todos los candidatos del partido democracia directa indican que no tienen bienes inmuebles.\nELABORADO POR:R para economistas.")+
  scale_y_continuous(breaks = seq(0,6,1))+
  theme_economist()+
  theme(plot.caption = element_text(hjust = 0, size=7, face="bold"),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1, size = 7.5))


#----------------------------------------
# VALOR TOTAL DE BIENES INMUEBLES.
#----------------------------------------
table(bdata$VALOR_BN_INMUEBLE_TOTAL)

#----------------------------------------
# NÚMERO DE BIENES MUEBLES.
#----------------------------------------
table(bdata$NUM_BN_MUEBLES)

c(table(bdata$NUM_BN_MUEBLES)[1],sum(table(bdata$NUM_BN_MUEBLES)[2:7]))

bdata %>% filter(NUM_BN_MUEBLES!=0) %>%
  group_by(PARTIDO) %>% summarise(n=n()) %>%
  ggplot(aes(x=reorder(PARTIDO,-n),y=n))+
  geom_col(col="dodgerblue4", fill="dodgerblue4")+
  labs(title = "CANDIDATOS QUE DECLARAN TENER BIENES MUEBLES\nPOR PARTIDO POLÍTICO",
       subtitle = "Número de candidatos.",
       y="Número de candidatos", x="",
       caption = "ELABORADO POR:R para economistas.")+
  scale_y_continuous(breaks = seq(0,6,1))+
  theme_economist()+
  theme(plot.caption = element_text(hjust = 0, size=7, face="bold"),
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, hjust=1, size = 7.5))

#----------------------------------------
# VALOR TOTAL DE BIENES MUEBLES.
#----------------------------------------
table(bdata$VALOR_BN_MUEBLES_TOTAL)

# Tiene 2 bienes muebles pero sin valor.
bdata[bdata$VALOR_BN_MUEBLES_TOTAL==0 & bdata$NUM_BN_MUEBLES!=0,]

#-------------------------------------------------------------------------------
#                                    RELACIONES
#-------------------------------------------------------------------------------

#-------------------------------
# TOTAL DE CARRERAS ESTUDIADAS.
#-------------------------------
bdata$NUM_EDUC_TEC<-as.numeric(bdata$NUM_EDUC_TEC)
bdata$NUM_EDUC_NOUNIV<-as.numeric(bdata$NUM_EDUC_NOUNIV)
bdata$NUM_EDUC_UNIV<-as.numeric(bdata$NUM_EDUC_UNIV)
bdata$NUM_EDUC_POSTGRADO<-as.numeric(bdata$NUM_EDUC_POSTGRADO)

bdata$TOTAL_CARRERAS<-rowSums(bdata[,c("NUM_EDUC_TEC","NUM_EDUC_NOUNIV",
                                       "NUM_EDUC_UNIV","NUM_EDUC_POSTGRADO")],
                              na.rm = T)

# Relación número de estudios e ingresos.

options(scipen=999)
# Número total de estudios superiores.

ggplot(bdata, aes(x=1:dim(bdata)[1], y=TOTAL_CARRERAS))+
  geom_point(size=4, col="deepskyblue2", fill="dodgerblue4", shape=21, stroke = 2)+
  labs(title = "NÚMERO TOTAL DE ESTUDIOS SUPERIORES POR CANDIDATO",
       subtitle = "Estudio en carreras universitarias, no universitarias, técnicas y de postgrado",
       x="",y="Número total de estudios superiores",
       caption = "Cada circulo representa a un candidato.\nELABORADO POR:R para Economistas.")+
  geom_text_repel(aes(label=ifelse(TOTAL_CARRERAS >=4, paste(NOMBRE,"\n",PARTIDO),"")),
                  col="dimgray", size=3)+
  scale_y_continuous(breaks = seq(0,6,1))+
  theme_economist() +
  theme(plot.caption = element_text(hjust = 0, size=7, face="bold"))

# Años de experiencia.

ggplot(bdata, aes(x=1:dim(bdata)[1], y=AÑOS_DE_EXPERIENCIA))+
  geom_point(size=4, col="deepskyblue2", fill="dodgerblue4", shape=21, stroke = 2)+
  labs(title = "TOTAL DE AÑOS DE EXPERIENCIA LABORAL POR CANDIDATO",
       subtitle = "Experiencia como independiente y/o dependiente.",
       x="",y="Años de experiencia laboral",
       caption = "Cada circulo representa a un candidato.\nELABORADO POR:R para Economistas.")+
  geom_text_repel(aes(label=ifelse(AÑOS_DE_EXPERIENCIA >=30, paste(NOMBRE,"\n",PARTIDO),"")),
                  col="dimgray", size=3)+
  scale_y_continuous(limits = c(0,38), breaks = seq(0,38,4))+
  theme_economist() +
  theme(plot.caption = element_text(hjust = 0, size = 7, face="bold"))

# Ingresos.
ggplot(bdata, aes(x=1:dim(bdata)[1], y=TOTAL_ING))+
  geom_point(size=4, col="deepskyblue2", fill="dodgerblue4", shape=21, stroke = 2)+
  labs(title = "TOTAL DE INGRESOS ANUALES POR CANDIDATO",
       subtitle = "Ingresos obtenidos en el sector privado y/o público. (S/.)",
       x="",y="Ingreso anuales totales.",
       caption = "Cada circulo representa a un candidato.\nELABORADO POR:R para Economistas.")+
  geom_text_repel(aes(label=ifelse(TOTAL_ING >=155000, paste(NOMBRE,"\n",PARTIDO),"")),
                  col="dimgray", size=3)+
  scale_y_continuous(limits = c(0,600000), breaks = seq(0,600000,50000))+
  theme_economist() +
  theme(plot.caption = element_text(hjust = 0, size = 8, face="bold"))

# Ingresos obtenidos por el sector público.

ggplot(bdata, aes(x=1:dim(bdata)[1], y=TOTAL_ING_SECT_PUBLICO))+
  geom_point(size=4, col="deepskyblue2", fill="dodgerblue4", shape=21, stroke = 2)+
  labs(title = "TOTAL DE INGRESOS ANUALES OBTENIDOS POR LABORES\nEN EL SECTOR PÚBLICO",
       subtitle = "Ingresos obtenidos en el sector público. (S/.)",
       x="",y="Ingreso anuales totales",
       caption = "Cada circulo representa a un candidato.\nELABORADO POR:R para Economistas")+
  geom_text_repel(aes(label=ifelse(TOTAL_ING_SECT_PUBLICO >=101500,
                                   paste(NOMBRE,"\n",PARTIDO,
                                         "\n",CENTRO_DE_TRABAJO1,"\n",
                                         OCUPACION1,"\nS/.",TOTAL_ING_SECT_PUBLICO),"")),
                  col="dimgray", size=3)+
  scale_y_continuous(limits = c(0,270000), breaks = seq(0,250000,20000))+
  theme_economist() +
  theme(plot.caption = element_text(hjust = 0, size = 8, face="bold"))

# Ingresos obtenidos por el sector privado.

ggplot(bdata, aes(x=1:dim(bdata)[1], y=TOTAL_ING_SECT_PRIVADO))+
  geom_point(size=4, col="deepskyblue2", fill="dodgerblue4", shape=21, stroke = 2)+
  labs(title = "TOTAL DE INGRESOS ANUALES OBTENIDOS POR LABORES\nEN EL SECTOR PRIVADO",
       subtitle = "Ingresos obtenidos en el sector privado. (S/.)",
       x="",y="Ingreso anuales totales",
       caption = "Cada circulo representa a un candidato\nELABORADO POR:R para Economistas")+
  geom_text_repel(aes(label=ifelse(TOTAL_ING_SECT_PRIVADO >=126000,
                                   paste(NOMBRE,"\n",PARTIDO,"\nS/.",
                                         TOTAL_ING_SECT_PRIVADO),"")),
                  col="dimgray", size=3)+
  scale_y_continuous(limits = c(0,600000), breaks = seq(0,600000,50000))+
  theme_economist() +
  theme(plot.caption = element_text(hjust = 0, size = 8, face="bold"))

# Número total de bienes inmuebles.
ggplot(bdata, aes(x=1:dim(bdata)[1], y=NUM_BN_INMUEBLES))+
  geom_point(size=4, col="deepskyblue2", fill="dodgerblue4", shape=21, stroke = 2)+
  labs(title = "NÚMERO TOTAL DE BIENES INMUEBLES POR CANDIDATO",
       subtitle = "Bienes inmuebles declarados.",
       x="",y="Número de bienes inmuebles",
       caption = "Cada circulo representa un candidato\nELABORADO POR:R para Economistas")+
  geom_text_repel(aes(label=ifelse(NUM_BN_INMUEBLES >=4,
                                   paste(NOMBRE,"\n",PARTIDO),"")),
                  col="dimgray", size=3)+
  theme_economist() +
  theme(plot.caption = element_text(hjust = 0, size = 8, face="bold"))

# Valor total de bienes inmuebles.
ggplot(bdata, aes(x=1:dim(bdata)[1], y=VALOR_BN_INMUEBLE_TOTAL))+
  geom_point(size=4, col="deepskyblue2", fill="dodgerblue4", shape=21, stroke = 2)+
  labs(title = "VALOR TOTAL DE BIENES INMUEBLES POR CANDIDATO",
       subtitle = "Del total de bienes inmuebles declarados. (S/.)",
       x="",y="Valor total de bienes inmuebles",
       caption = "Cada circulo representa a un candidato\nELABORADO POR:R para Economistas")+
  geom_text_repel(aes(label=ifelse(VALOR_BN_INMUEBLE_TOTAL >=1000000,
                                   paste(NOMBRE,"\n",PARTIDO,
                                         "\ns/.",VALOR_BN_INMUEBLE_TOTAL,
                                         "\nNúmero de bienes inmuebles=",
                                         NUM_BN_INMUEBLES),"")),
                  col="dimgray", size=3.5)+
  scale_y_continuous(limits = c(0,35000000), breaks = seq(0,35000000,3000000))+
  theme_economist() +
  theme(plot.caption = element_text(hjust = 0, size = 8, face="bold"))

# Número total de bienes muebles.
ggplot(bdata, aes(x=1:dim(bdata)[1], y=NUM_BN_MUEBLES))+
  geom_point(size=4, col="deepskyblue2", fill="dodgerblue4", shape=21, stroke = 2)+
  labs(title = "NÚMERO TOTAL DE BIENES MUEBLES POR CANDIDATO",
       subtitle = "Bienes muebles declarados.",
       x="",y="Número de bienes muebles",
       caption = "Cada circulo representa a un candidato.\nELABORADO POR:R para Economistas")+
  geom_text_repel(aes(label=ifelse(NUM_BN_MUEBLES >=3,
                                   paste(NOMBRE,"\n",PARTIDO),"")),
                  col="dimgray", size=3)+
  scale_y_continuous(limits = c(0,13), breaks = seq(0,13,1))+
  theme_economist() +
  theme(plot.caption = element_text(hjust = 0, size = 8, face="bold"))

# Valor total de bienes muebles.
ggplot(bdata, aes(x=1:dim(bdata)[1], y=VALOR_BN_MUEBLES_TOTAL))+
  geom_point(size=4, col="deepskyblue2", fill="dodgerblue4", shape=21, stroke = 2)+
  labs(title = "VALOR TOTAL DE BIENES MUEBLES POR CANDIDATO",
       subtitle = "Del total de bienes muebles declarados. (S/.)",
       x="",y="Valor total de bienes muebles",
       caption = "Cada circulo representa a un candidato\nELABORADO POR:R para Economistas")+
  geom_text_repel(aes(label=ifelse(VALOR_BN_MUEBLES_TOTAL >=110000,
                                   paste(NOMBRE,"\n",PARTIDO,
                                         "\ns/.",VALOR_BN_MUEBLES_TOTAL,
                                         "\nNúmero de bienes muebles=",
                                         NUM_BN_MUEBLES),"")),
                  col="dimgray", size=3.5)+
  scale_y_continuous(limits = c(0,350000), breaks = seq(0,350000,50000))+
  theme_economist() +
  theme(plot.caption = element_text(hjust = 0, size = 8, face="bold"))

# Total de carreras e ingreso laboral.

ggplot(bdata, aes(x=TOTAL_CARRERAS,y=TOTAL_ING))+
  geom_point(size=5, col="deepskyblue2", fill="dodgerblue4", shape=21,stroke = 2)+
  scale_y_continuous(limits = c(0,600000), breaks = seq(0,600000, 100000))+
  scale_x_continuous(limits = c(0,6), breaks = seq(0,6,1))+
  labs(title = "NÚMERO DE ESTUDIOS SUPERIORES E INGRESOS ANUALES\nTOTALES POR CANDIDATOS",
       subtitle = "Ingresos totales anuales. (S/.)",
       x="Número total de estudios superiores",
       y="Ingresos anuales totales",
       caption = "Cada circulo representa a un candidato\nSe considera la mediana ya que se observa mucha dispersión entre candidatos (CV=136.3%)\nELABORADO POR:R para Economistas")+
  geom_text_repel(aes(label=ifelse(TOTAL_CARRERAS ==0 & TOTAL_ING>=50000,
                                   paste(NOMBRE,"\n",PARTIDO,"\ns/.",TOTAL_ING),"")),
                  col="dimgray", size=3.5)+
  geom_vline(aes(xintercept = mean(TOTAL_CARRERAS),
                 col = "Número de carreras estudiadas promedio"), size=1,
             linetype="longdash")+
  geom_hline(aes(yintercept = median(TOTAL_ING),
                 col = "Mediana de los ingresos anuales"), size=1,
             linetype="longdash")+
  scale_color_manual(name = "",
                     values = c("Número de carreras estudiadas promedio"= "red",
                                "Mediana de los ingresos anuales"="black"))+
  theme_economist() +
  theme(plot.caption = element_text(hjust = 0, size = 8, face="bold"),
        legend.position = "bottom",legend.key.size = unit(0.5,"line"))

# Ratio entre los ingresos y el número de carreras estudiadas.
bdata$RATIO_ING_ESTUDIOS<-ifelse(bdata$TOTAL_CARRERAS==0,0,
                                 bdata$TOTAL_ING/bdata$TOTAL_CARRERAS)

ggplot(bdata, aes(x=1:dim(bdata)[1], y=RATIO_ING_ESTUDIOS))+
  geom_point(size=4, col="deepskyblue2", fill="dodgerblue4", shape=21, stroke = 2)+
  labs(title = "RATIO ENTRE EL TOTAL DE INGRESOS ANUALES Y\nEL NÚMERO DE ESTUDIOS SUPERIORES",
       subtitle = "En soles por carrera",
       x="",y="ratio de ingresos entre el número de estudios superiores",
       caption = "Cada circulo representa a un candidato\nEl ratio es el cociente entre el ingreso total anual y el número de total de carreras superiores estuadiadas.\nSe considera la mediana ya que se observa mucha dispersión entre candidatos (CV=121.4%)\nELABORADO POR:R para Economistas")+
  geom_text_repel(aes(label=ifelse(RATIO_ING_ESTUDIOS >=80000,
                                   paste(NOMBRE,"\n",PARTIDO,
                                         "\nNúmero de estudios Superiores:",TOTAL_CARRERAS),"")),
                  col="dimgray", size=3.5)+
  geom_hline(aes(yintercept = median(RATIO_ING_ESTUDIOS),
                 col = "Mediana del ratio ingresos y estudios superiores"), size=1,
             linetype="longdash")+
  scale_color_manual(name = "",
                     values = c("Mediana del ratio ingresos y estudios superiores"= "red"))+

  scale_y_continuous(limits = c(0,200000), breaks = seq(0,250000,30000))+
  theme_economist() +
  theme(plot.caption = element_text(hjust = 0, size = 7, face="bold"),
        legend.position = "bottom",legend.key.size = unit(0.5,"line"))

# Número de carreras por partido político

ggplot(bdata, aes(x=TOTAL_CARRERAS,y=TOTAL_ING))+
  geom_point(size=5, col="deepskyblue2", fill="dodgerblue4", shape=21, stroke = 2)+
  scale_y_continuous(limits = c(0,600000), breaks = seq(0,600000, 100000))+
  scale_x_continuous(limits = c(0,6), breaks = seq(0,6,1))+
  labs(title = "NÚMERO DE ESTUDIOS SUPERIORES E INGRESOS ANUALES TOTALES\nPOR PARTIDO POLÍTICO",
       subtitle = "En soles. (S/.)",
       x="Número total de estudios superiores",
       y="Ingresos anuales totales en soles",
       caption = "Cada circulo representa a un candidato.\nELABORADO POR: R para economistas")+
  geom_vline(aes(xintercept = mean(TOTAL_CARRERAS),
                 col = "Número de estudios superiores promedio"), size=1,
             linetype="longdash")+
  geom_hline(aes(yintercept = mean(TOTAL_ING),
                 col = "Mediana de los ingresos anuales"), size=1,
             linetype="longdash")+
  scale_color_manual(name = "",
                     values = c("Número de estudios superiores promedio"= "red",
                                "Mediana de los ingresos anuales"="black"))+
  theme_bw()+
  facet_wrap(PARTIDO~.,ncol=3)+
  theme(plot.caption = element_text(hjust = 0, size = 8, face="bold"),
        legend.position = "bottom",legend.key.size = unit(0.5,"line"),
        plot.title = element_text(face = "bold"))

# Años de experiencia e ingresos.

bdata$AÑOS_DE_EXPERIENCIA<-as.numeric(bdata$AÑOS_DE_EXPERIENCIA)

ggplot(bdata, aes(x=AÑOS_DE_EXPERIENCIA,y=TOTAL_ING))+
  geom_point(size=5, col="deepskyblue2", fill="dodgerblue4", shape=21,stroke=2)+
  scale_y_continuous(limits = c(0,600000),
                     breaks = seq(0,600000, 100000))+
  scale_x_continuous(limits = c(0,38), breaks = seq(0,38,4))+
  labs(title = "AÑOS DE EXPERIENCIA E INGRESOS ANUALES TOTALES - POR PARTIDOS",
       x="Años de experiencia", subtitle = "En soles. (S/.)",
       y="Ingresos anuales totales en soles",
       caption = "Cada circulo representa a un candidato.\nELABORADO POR: R para economistas")+
  geom_vline(aes(xintercept = mean(AÑOS_DE_EXPERIENCIA),
                 col = "Años de experiencia promedio"), size=1,
             linetype="longdash")+
  geom_hline(aes(yintercept = median(TOTAL_ING),
                 col = "Mediana de los ingresos anuales"), size=1,
             linetype="longdash")+
  scale_color_manual(name = "",
                     values = c("Años de experiencia promedio"= "red",
                                "Mediana de los ingresos anuales"="black"))+
  theme_bw()+
  facet_wrap(PARTIDO~.,ncol=3)+
  theme(plot.caption = element_text(hjust = 0, size = 8, face="bold"),
        legend.position = "bottom",legend.key.size = unit(0.5,"line"),
        plot.title = element_text(face = "bold"))

#-----

ggplot(bdata, aes(x=AÑOS_DE_EXPERIENCIA,y=TOTAL_ING))+
  geom_point(size=5, col="deepskyblue2", fill="dodgerblue4", shape=21, stroke=2)+
  scale_y_continuous(limits = c(0,600000),
                     breaks = seq(0,600000, 50000))+
  scale_x_continuous(limits = c(0,38), breaks = seq(0,38,2))+
  labs(title = "AÑOS DE EXPERIENCIA E INGRESOS ANUALES TOTALES POR PARTIDOS",
       x="Años de experiencia", subtitle = "En soles. (S/.)",
       y="Ingresos anuales totales",
       caption = "Cada circulo representa a un candidato.\nELABORADO POR: R para economistas")+
  geom_vline(aes(xintercept = mean(AÑOS_DE_EXPERIENCIA),
                 col = "Años de experiencia promedio"), size=1,
             linetype="longdash")+
  geom_hline(aes(yintercept = median(TOTAL_ING),
                 col = "Mediana de los ingresos anuales"), size=1,
             linetype="longdash")+
  scale_color_manual(name = "",
                     values = c("Años de experiencia promedio"= "red",
                                "Mediana de los ingresos anuales"="black"))+
  geom_text_repel(aes(label=ifelse(TOTAL_ING > 160000,
                                   paste(NOMBRE,"\n",PARTIDO,"\nS/.",
                                         TOTAL_ING),"")),
                  col="dimgray", size=3)+
  theme_economist() +
  theme(plot.caption = element_text(hjust = 0, size = 8, face="bold"),
        legend.position = "bottom",legend.key.size = unit(0.5,"line"))

#--------------------------------------------------------------------------

# Candidatos que no nacieron en la región Junín.

library(rgdal)
library(tmap)

reg<-as.data.frame(table(bdata$DEPARTAMENTO))
reg$Var1<-as.character(reg$Var1)

mapa<-readOGR("F:/ML/DEPARTAMENTOS.shp")

# Para saber a que identificador pertenece cada departamento.
mapa@data

# Uniendo en el data frame.
mapa@data<-left_join(mapa@data, reg, by=c("DEPARTAMEN"="Var1"))

mapa@data$Freq2<-ifelse(is.na(mapa@data$Freq),mapa@data$Freq,
                        paste0(mapa@data$DEPARTAMEN,"\n",mapa@data$Freq))

qtm(shp = mapa, fill = c("Freq"), borders ="gray75",
    scale = 0.7, title.font=1, fill.title="",col = "Median_income", palette = "BuGn",
    title = "REGIONES DE\nNACIMIENTO\nDE LOS CANDIDATOS",
    title.fontface=2,
    fill.style="fixed", fill.breaks=seq(0, 15, by=2))+
  tm_text("Freq2", size = 0.8, col = "black")+
  tm_legend(show=FALSE)+
  tm_layout(frame = F)


bdata[bdata$DEPARTAMENTO!="JUNIN",][,c("NOMBRE","PARTIDO","DEPARTAMENTO")]





