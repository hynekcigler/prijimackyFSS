library(shiny)
library(psych)

shinyServer(function(input, output, session) {
  
  ######################## VÝPOČTY ############################################
  
  reactive(set.seed(input$seed))
  
  seOSP <- reactive({sqrt(1-input$rOSP)/sqrt(input$rOSP)}) ## standardní chyba měření OSP
  seTSP <- reactive({sqrt(1-input$rTSP)/sqrt(input$rTSP)}) ## standardní chyba měření TSP
  seZSV <- reactive({sqrt(1-input$rZSV)/sqrt(input$rZSV)}) ## standardní chyba měření ZSV
  kor <- reactive({
    matrix(c( 1, input$OSPTSP, input$OSPZSV, input$OSPTSP, 1, input$ZSVTSP, input$OSPZSV, input$ZSVTSP, 1), 
           byrow=T, ncol=3, dimnames = list(c("OSP","TSP","ZSV"),c("OSP","TSP","ZSV"))) ## korelační matice pozorovaných skórů
  })
  kortrue <- reactive({kor() / sqrt(matrix(c(1, input$rOSP*input$rTSP, input$rOSP*input$rZSV, input$rTSP*input$rOSP, 1, input$rTSP*input$rZSV,
                                             input$rZSV*input$rOSP, input$rZSV*input$rTSP, 1), 
                                           byrow=T, ncol=3,nrow=3))}) ## odhad korelace pravých skórů
  vahy <- reactive({c(SP=input$vahy1, ZSV=1-input$vahy1)}) ## váhy testů v pořadí SP, ZSV
  truesc <- reactive(sim.correlation(R=kortrue(),n=input$N,data=T)) ## simulace truescorů
  
  # Simulace pozorovaných skórů ---------------------------------------------
  
  # simulace výsledků OSP
  OSPobs <- reactive({
    OSPobs <- matrix(nrow=input$N, ncol=input$nOSP+1)
    colnames(OSPobs) <- c(1:input$nOSP, "max") 
    for (i in c(1:input$nOSP)) {
      OSPobs[,i] <- truesc()[,"OSP"] + rnorm(n=input$N, sd=seOSP())
      } ## jednotlivé skóry v OSP
    OSPobs[,input$nOSP+1] <- apply(OSPobs[,c(1:input$nOSP)], 1, max) ## maximální výsledek ze tří pokusů
    OSPobs
  })
  
  # simulace výsledků ZSV
  ZSVobs <- reactive({
    ZSVobs <- matrix(nrow=input$N, ncol=input$nZSV+1)
    colnames(ZSVobs) <- c(1:input$nZSV, "max") 
    for (i in c(1:input$nZSV)) {
      ZSVobs[,i] <- truesc()[,"ZSV"] + rnorm(n=input$N, sd=seZSV())
    } ## jednotlivé skóry v ZSV
    ZSVobs[,input$nZSV+1] <- apply(ZSVobs[,c(1:input$nZSV)], 1, max) ## maximální výsledek ze tří pokusů
    ZSVobs
  })
  
  # simulace výsledků TSP
  TSPobs <- reactive({
    r <- input$cohenD/sqrt(input$cohenD^2 + 4) * sqrt(input$rTSP) ## převod na r a korekce proti nereliabilitě
    d <- (2*r)/(1-r**2) ## převod zpět na "true" Cohenovo d.
    truesc()[,"TSP"] - d + rnorm(n=input$N, sd=seTSP()) ## včetně Cohenova d
    }) ## skóre v TSP
  
  
  # Převod na percentily ----------------------------------------------------
  
  truepc <- reactive(cbind(OSP=pnorm(truesc()[,"OSP"]),TSP=pnorm(truesc()[,"TSP"]),ZSV=pnorm(truesc()[,"ZSV"]))) ## pravé percentily
  
  # OSP
  OSPpc <- reactive({
    OSPpc <- OSPobs()
    for (i in c(1:input$nOSP)) {
      x <- ecdf(OSPpc[,i])
      OSPpc[,i] <- x(OSPpc[,i])
      }
    OSPpc[,input$nOSP+1] <- apply(OSPpc[,c(1:input$nOSP)], 1, max)
    OSPpc
    })
  
  # ZSV
  ZSVpc <- reactive({
    ZSVpc <- ZSVobs()
    for (i in c(1:input$nZSV)) {
      x <- ecdf(ZSVpc[,i])
      ZSVpc[,i] <- x(ZSVpc[,i])
    }
    ZSVpc[,input$nZSV+1] <- apply(ZSVpc[,c(1:input$nZSV)], 1, max)
    ZSVpc
  })
  
  # TSP
  TSPpc <- reactive({
    x <- ecdf(TSPobs()-input$cohenD) ## včetně korekce na základě velikosti rozdílu (Cohenovo d)
    TSPpc <- x(TSPobs())
    TSPpc
  })
  
  
  # Součtové pravé skóry ----------------------------------------------------
  
  truesc_FIN <- reactive({cbind(OSP=(vahy()[1]*truesc()[,"OSP"]+vahy()[2]*truesc()[,"ZSV"])*2 ,
                      TSP=(vahy()[1]*truesc()[,"TSP"]+vahy()[2]*truesc()[,"ZSV"])*2)}) ## finální součtový pravý skór 
  truepc_FIN <- reactive({cbind(OSP=(vahy()[1]*truepc()[,"OSP"]+vahy()[2]*truepc()[,"ZSV"])*2,
                      TSP=(vahy()[1]*truepc()[,"TSP"]+vahy()[2]*truepc()[,"ZSV"])*2)}) ## finální součtový pravý percentil

  # Součtové pozorované skóry -----------------------------------------------
  
  # skóry
  FINobs <- reactive({
    cbind(
      OSP1 = (vahy()[1]*OSPobs()[,1]+vahy()[2]*ZSVobs()[,1])*2 , # jeden pokus s výběrem OSP
      OSPn = (vahy()[1]*OSPobs()[,input$nOSP+1]+vahy()[2]*ZSVobs()[,input$nZSV+1])*2 , # více pokusů s výběrem OSP
      TSP1 = (vahy()[1]*TSPobs()+vahy()[2]*ZSVobs()[,1])*2 , # jeden pokus s výběrem TSP
      TSPn = (vahy()[1]*TSPobs()+vahy()[2]*ZSVobs()[,input$nZSV+1])*2 # více pokusů s výběrem TSP
      )
    })
  
  # percentily
  FINpc <- reactive({
    cbind(
      OSP1 = (vahy()[1]*OSPpc()[,1]+vahy()[2]*ZSVpc()[,1])*2 , # jeden pokus s výběrem OSP
      OSPn = (vahy()[1]*OSPpc()[,input$nOSP+1]+vahy()[2]*ZSVpc()[,input$nZSV+1])*2 , # více pokusů s výběrem OSP
      TSP1 = (vahy()[1]*TSPpc()+vahy()[2]*ZSVpc()[,1])*2 , # jeden pokus s výběrem TSP
      TSPn = (vahy()[1]*TSPpc()+vahy()[2]*ZSVpc()[,input$nZSV+1])*2 # více pokusů s výběrem TSP
      )
    })
  
  
  # Pravděpodobost přijetí -----------------------------------------------------------------
  
  prijatTRUE <- reactive({ ## měl být přijat na základě true-percentilu?
    cbind(    
      OSP=truepc_FIN()[,"OSP"] > input$cut/100 , # v případě výběru OSP
      TSP=truepc_FIN()[,"TSP"] > input$cut/100   # v případě výběru TSP
      )
    })
  
  prijat <- reactive({ ## je přijat?
    cbind( 
      OSP1=FINpc()[,"OSP1"] > input$cut/100 , # v případě výběru OSP s 1 pokusem u SCIO
      TSP1=FINpc()[,"TSP1"] > input$cut/100 , # v případě výběru TSP s 1 pokusem u SCIO
      OSPn=FINpc()[,"OSPn"] > input$cut/100 , # v případě výběru OSP s n pokusy u SCIO
      TSPn=FINpc()[,"TSPn"] > input$cut/100   # v případě výběru TSP s n pokusy u SCIO
      )
    })
  
  reliabilitaFIN <- reactive({
    matrix(c(
    OSP1=cor(FINpc()[,"OSP1"], truepc_FIN()[,"OSP"]),
    OSPn=cor(FINpc()[,"OSPn"], truepc_FIN()[,"OSP"]),
    TSP1=cor(FINpc()[,"TSP1"], truepc_FIN()[,"TSP"]),
    TSPn=cor(FINpc()[,"TSPn"], truepc_FIN()[,"TSP"]),
    OSP1=cor(FINobs()[,"OSP1"], truesc_FIN()[,"OSP"]),
    OSPn=cor(FINobs()[,"OSPn"], truesc_FIN()[,"OSP"]),
    TSP1=cor(FINobs()[,"TSP1"], truesc_FIN()[,"TSP"]),
    TSPn=cor(FINobs()[,"TSPn"], truesc_FIN()[,"TSP"])
    )^2, nrow=2, ncol=4, byrow=T, dimnames=list(c("pc","obs"),c("OSP1","OSPn","TSP1","TSPn")))
    })
  
  

  
  
  
  
  ######################## GRAFICKÝ OUTPUT ############################################
  
  # Graf pravděpodobnosti přijetí podle volby testu a úrovně schopnosti -----------------------------------------------------------------
  
  output$pprijeti <- renderPlot({
    
    if (input$prepinac1 == 1) {
      x <- seq(0,2-.05,by=.05) ## rozdělení osy x podle cut-skóre
    } else if (input$prepinac1 == 2) {
      x <- quantile(truepc_FIN()[,"OSP"], probs=seq(0,1-.025,by=.025))
    } else {
      x <- quantile(truepc_FIN()[,"TSP"], probs=seq(0,1-.025,by=.025))
    }
    
    pravdepodobnost <- matrix(nrow=4, ncol=length(x-1), dimnames = list(c("OSP1","OSPn","TSP1","TSPn")))
    
    for (i in c(1:length(x))) {
      pravdepodobnost[1,i] <- sum(prijat()[truepc_FIN()[,"OSP"] > x[i] & truepc_FIN()[,"OSP"] <= x[i+1],"OSP1"])/length(prijat()[truepc_FIN()[,"OSP"] > x[i] & truepc_FIN()[,"OSP"] <= x[i+1],"OSP1"])
      pravdepodobnost[2,i] <- sum(prijat()[truepc_FIN()[,"OSP"] > x[i] & truepc_FIN()[,"OSP"] <= x[i+1],"OSPn"])/length(prijat()[truepc_FIN()[,"OSP"] > x[i] & truepc_FIN()[,"OSP"] <= x[i+1],"OSPn"])
      pravdepodobnost[3,i] <- sum(prijat()[truepc_FIN()[,"TSP"] > x[i] & truepc_FIN()[,"TSP"] <= x[i+1],"TSP1"])/length(prijat()[truepc_FIN()[,"TSP"] > x[i] & truepc_FIN()[,"TSP"] <= x[i+1],"TSP1"])
      pravdepodobnost[4,i] <- sum(prijat()[truepc_FIN()[,"TSP"] > x[i] & truepc_FIN()[,"TSP"] <= x[i+1],"TSPn"])/length(prijat()[truepc_FIN()[,"TSP"] > x[i] & truepc_FIN()[,"TSP"] <= x[i+1],"TSPn"])
    }
    
    barvy <- c("red","red","green","green")
    typy <- c(1,2,1,2)
    plot(spline(x,pravdepodobnost[1,]), type="l", xlab="pravý součtový percentil", ylab="pravděpodobnost přijetí", 
         main="Pravděpodobnost přijetí podle volby testu a úrovně schopnosti", lwd=2,col=barvy[1], ylim=c(0,1),
         sub="Sám o sobě nelze snadno interpretovat, slouží ke 'srovnání' křivek na ose x.")
    for (i in c(2:4)) {
      lines(spline(x,(pravdepodobnost[i,])),col=barvy[i],lty=typy[i], lwd=2)
    }
    abline(v=input$cut/100,lty=3, col="gray", lwd=2)
    abline(h=.5, col="gray", lty=3)
    legend("topleft", c("OSP, 1 opakování","OSP, n opakování","TSP, 1 opakování","TSP, n opakování","hranice přijetí"), 
           lwd=2,lty=c(typy,4),col=c(barvy,"gray"), inset= .05)
    
  })
  
  # Scatter ploty součtového percentilu -------------------------------------------------------------------
  
  output$scatter1 <- renderPlot({
    koef2 <- lm(FINobs()[,"TSP1"] ~ FINobs()[,"OSP1"])$coefficients[2]
    koef1 <- 1-koef2
    plot(FINpc()[,"OSP1"], FINpc()[,"TSP1"], xlab="1x OSP + 1x ZSV", ylab="1x TSP + 1x ZSV", 
         main="Srovnání osob, kteří si vyberou \nOSP nebo TSP a všechny testy absolvují jedenkrát", 
         sub="Na obou osách je dosažený součtový percentil.", pch=".")
    abline(koef1, koef2, col="red", lwd=2)
    abline(h=input$cut/100,v=input$cut/100,col="gray",lty=2)
    lines(smooth.spline(FINpc()[,"OSP1"], FINpc()[,"TSP1"], keep.data = F), col="green", lwd=2)
    legend("bottomright", c("očekávný vztah", "pozorovaný vztah (spline)","hranice pro přijetí"), 
           lwd=c(2,2,1), lty=c(1,1,2), col=c("red","green","gray"), inset= .05, 
           title=paste("Zvýhodnění Md =", round(abs(median(FINpc()[,"OSP1"]-FINpc()[,"TSP1"])), 2)))
  }, width=400, height=400)
  

  output$scatter2 <- renderPlot({
    koef2 <- lm(FINobs()[,"OSPn"] ~ FINobs()[,"TSPn"])$coefficients[2]
    koef1 <- 1-koef2
    plot(FINpc()[,"TSPn"], FINpc()[,"OSPn"], ylab="Nx OSP + Nx ZSV", xlab="1x TSP + Nx ZSV", 
         main="Zvýhodnění lidí, využívajících všech možností \nSCIO oproti těm, kteří absolvují TSP a ZSV \nzkusí, kolikrát to jen jde", 
         sub="Na obou osách je dosažený součtový percentil.", pch=".")
    abline(koef1, koef2, col="red", lwd=2)
    abline(h=input$cut/100,v=input$cut/100,col="gray",lty=2)
    lines(smooth.spline(FINpc()[,"TSPn"], FINpc()[,"OSPn"], keep.data = F), col="green", lwd=2)
    legend("bottomright", c("očekávný vztah", "pozorovaný vztah (spline)","hranice pro přijetí"), 
           lwd=c(2,2,1), lty=c(1,1,2), col=c("red","green","gray"), inset= .05, 
           title=paste("Zvýhodnění Md =", round(abs(median(FINpc()[,"TSPn"]-FINpc()[,"OSPn"])), 2)))
  }, width=400, height=400)
  
  output$scatter3 <- renderPlot({
    koef2 <- (cor(truesc_FIN()[,"OSP"], FINobs()[,"OSP1"]))**2
    koef1 <- 1-koef2
    plot(FINpc()[,"OSP1"], FINpc()[,"OSPn"], xlab="1x OSP + 1x ZSV", ylab="Nx OSP + Nx ZSV", 
         main="Zvýhodnění lidí, využívajících všech \nmožností SCIO oproti těm, \nkteří absolvují oba SCIO testy jen jednou", 
         sub="Na obou osách je dosažený součtový percentil.", pch=".")
    abline(koef1, koef2, col="red", lwd=2)
    abline(h=input$cut/100,v=input$cut/100,col="gray",lty=2)
    lines(smooth.spline(FINpc()[,"OSP1"], FINpc()[,"OSPn"], keep.data = F), col="green", lwd=2)
    legend("bottomright", c("očekávný vztah", "pozorovaný vztah (spline)","hranice pro přijetí"), 
           lwd=c(2,2,1), lty=c(1,1,2), col=c("red","green","gray"), inset= .05, 
           title=paste("Zvýhodnění Md =", round(abs(median(FINpc()[,"OSP1"]-FINpc()[,"OSPn"])), 2)))
  }, width=400, height=400)
  
  output$scatter4 <- renderPlot({
    koef2 <- (cor(truesc_FIN()[,"TSP"], FINobs()[,"TSP1"]))**2
    koef1 <- 1-koef2
    plot(FINpc()[,"TSP1"], FINpc()[,"TSPn"], xlab="1x TSP + 1xZSV", ylab="1x TSP + Nx ZSV", 
         main="Zvýhodnění lidí, kteří si vybrali TSP \na využili všechni možnosti na ZSV \noproti těm, kteří na ZSV šli jen jednou", sub="Na obou osách je dosažený součtový percentil.", pch=".")
    abline(koef1, koef2, col="red", lwd=2)
    abline(h=input$cut/100,v=input$cut/100,col="gray",lty=2)
    lines(smooth.spline(FINpc()[,"TSP1"], FINpc()[,"TSPn"], keep.data = F), col="green", lwd=2)
    legend("bottomright", c("očekávný vztah", "pozorovaný vztah (spline)","hranice pro přijetí"), 
           lwd=c(2,2,1), lty=c(1,1,2), col=c("red","green","gray"), inset= .05, 
           title=paste("Zvýhodnění Md =", round(abs(median(FINpc()[,"TSPn"]-FINpc()[,"TSP1"])), 2)))
  }, width=400, height=400)
  
  output$scatter5 <- renderPlot({
    koef2 <- lm(FINobs()[,"OSPn"] ~ FINobs()[,"TSP1"])$coefficients[2]
    koef1 <- 1-koef2
    plot(FINpc()[,"TSP1"], FINpc()[,"OSPn"], xlab="1x TSP + 1x ZSV", ylab="Nx OSP + Nx ZSV", 
         main="Zvýhodnění lidí, využívajících všech \nmožností SCIO oproti těm, kteří absolvují \nTSP a neuvědomí si, že ZSV mohou vícekrát", 
         sub="Na obou osách je dosažený součtový percentil.", pch=".")
    abline(koef1, koef2, col="red", lwd=2)
    abline(h=input$cut/100,v=input$cut/100,col="gray",lty=2)
    lines(smooth.spline(FINpc()[,"TSP1"], FINpc()[,"OSPn"], keep.data = F), col="green", lwd=2)
    legend("bottomright", c("očekávný vztah", "pozorovaný vztah (spline)","hranice pro přijetí"), 
           lwd=c(2,2,1), lty=c(1,1,2), col=c("red","green","gray"), inset= .05, 
           title=paste("Zvýhodnění Md =", round(abs(median(FINpc()[,"TSP1"]-FINpc()[,"OSPn"])), 2)))
  }, width=400, height=400)
  

  # Scatter ploty pozorovaných a pravých skórů percentilů -------------------------------------------------------------------
  
  output$scatter6 <- renderPlot({
    plot(truepc_FIN()[,"OSP"],FINpc()[,"OSP1"], xlab="pravý percentil", ylab="pozorovaný percetil", main="1krát OSP a 1krát ZSV", 
         sub="Na obou osách je součtový percentil.", pch=".")
    abline(a=1-reliabilitaFIN()[1,"OSP1"],b=reliabilitaFIN()[1,"OSP1"], col="red", lwd=2)
    lines(smooth.spline(truepc_FIN()[,"OSP"],FINpc()[,"OSP1"], keep.data=F), col="green", lwd=2)
    legend("bottomright", c("předpokládaný vztah", "pozorovaný vztah (spline)"), lwd=2, col=c("red","green"), inset= .05)
  }, width=400, height=400)
  output$scatter7 <- renderPlot({
    plot(truepc_FIN()[,"TSP"],FINpc()[,"TSP1"], xlab="pravý percentil", ylab="pozorovaný percetil", main="1krát TSP a 1krát ZSV", 
         sub="Na obou osách je součtový percentil.", pch=".")
    abline(a=1-reliabilitaFIN()[1,"TSP1"],b=reliabilitaFIN()[1,"TSP1"], col="red", lwd=2)
    lines(smooth.spline(truepc_FIN()[,"TSP"],FINpc()[,"TSP1"], keep.data=F), col="green", lwd=2)
    legend("bottomright", c("předpokládaný vztah", "pozorovaný vztah (spline)"), lwd=2, col=c("red","green"), inset= .05)
  }, width=400, height=400)
  output$scatter8 <- renderPlot({
    plot(truepc_FIN()[,"OSP"],FINpc()[,"OSPn"], xlab="pravý percentil", ylab="pozorovaný percetil", main="n-krát OSP a n-krát ZSV", 
         sub="Na obou osách je součtový percentil.", pch=".")
    abline(a=1-reliabilitaFIN()[1,"OSPn"],b=reliabilitaFIN()[1,"OSPn"], col="red", lwd=2)
    lines(smooth.spline(truepc_FIN()[,"OSP"],FINpc()[,"OSPn"], keep.data=F), col="green", lwd=2)
    legend("bottomright", c("očekávaný vztah", "pozorovaný vztah (spline)"), lwd=2, col=c("red","green"), inset= .05)
  }, width=400, height=400)
  output$scatter9 <- renderPlot({
    plot(truepc_FIN()[,"TSP"],FINpc()[,"TSPn"], xlab="pravý percentil", ylab="pozorovaný percetil", main="1krát TSP a n-krát ZSV", 
         sub="Na obou osách je součtový percentil.", pch=".")
    abline(a=1-reliabilitaFIN()[1,"TSPn"],b=reliabilitaFIN()[1,"TSPn"], col="red", lwd=2)
    lines(smooth.spline(truepc_FIN()[,"TSP"],FINpc()[,"TSPn"], keep.data=F), col="green", lwd=2)
    legend("bottomright", c("očekávaný vztah", "pozorovaný vztah (spline)"), lwd=2, col=c("red","green"), inset= .05)
  }, width=400, height=400)
  
  
  
  # Srovnání percentilů a pravých skórů -------------------------------------
  
  output$srovnaniTSP1 <- renderPlot({
    plot(truesc_FIN()[,"TSP"], FINpc()[,"TSP1"], xlab="pravé skóre", ylab="pozorovaný percentil", main="TSP a ZSV, vše jednou", pch=".")
    lines(smooth.spline(truesc_FIN()[,"TSP"],FINpc()[,"TSP1"], keep.data=F), col="green", lwd=2)
    lines(seq(-5,5,by=.1),2*pnorm(seq(-5,5,by=.1), sd=sd(truesc_FIN()[,"TSP"])), col="red", lwd=2)
    abline(h=1,v=0, col="gray")
    abline(h=input$cut/100, col="black")
    legend("bottomright",c("předpokládaný vztah","pozorovaný vztah"),col=c("red","green"),lwd=2, inset=.05)
  })
  output$srovnaniOSP1 <- renderPlot({
    plot(truesc_FIN()[,"OSP"], FINpc()[,"OSP1"], xlab="pravé skóre", ylab="pozorovaný percentil", main="OSP a ZSV, vše jednou", pch=".")
    lines(smooth.spline(truesc_FIN()[,"OSP"],FINpc()[,"OSP1"], keep.data=F), col="green", lwd=2)
    lines(seq(-5,5,by=.1),2*pnorm(seq(-5,5,by=.1), sd=sd(truesc_FIN()[,"OSP"])), col="red", lwd=2)
    abline(h=1,v=0, col="gray")
    abline(h=input$cut/100, col="black")
    legend("bottomright",c("předpokládaný vztah","pozorovaný vztah"),col=c("red","green"),lwd=2, inset=.05)
  })
  
  output$srovnaniTSPn <- renderPlot({
    plot(truesc_FIN()[,"TSP"], FINpc()[,"TSPn"], xlab="pravé skóre", ylab="pozorovaný percentil", main="TSP a ZSV, ZSV n-krát", pch=".")
    lines(smooth.spline(truesc_FIN()[,"TSP"],FINpc()[,"TSPn"], keep.data=F), col="green", lwd=2)
    lines(seq(-5,5,by=.1),2*pnorm(seq(-5,5,by=.1), sd=sd(truesc_FIN()[,"TSP"])), col="red", lwd=2)
    abline(h=1,v=0, col="gray")
    abline(h=input$cut/100, col="black")
    legend("bottomright",c("předpokládaný vztah","pozorovaný vztah"),col=c("red","green"),lwd=2, inset=.05)
  })
  output$srovnaniOSPn <- renderPlot({
    plot(truesc_FIN()[,"OSP"], FINpc()[,"OSPn"], xlab="pravé skóre", ylab="pozorovaný percentil", main="OSP a ZSV, vše n-krát", pch=".")
    lines(smooth.spline(truesc_FIN()[,"OSP"],FINpc()[,"OSPn"], keep.data=F), col="green", lwd=2)
    lines(seq(-5,5,by=.1),2*pnorm(seq(-5,5,by=.1), sd=sd(truesc_FIN()[,"OSP"])), col="red", lwd=2)
    abline(h=1,v=0, col="gray")
    abline(h=input$cut/100, col="black")
    legend("bottomright",c("předpokládaný vztah","pozorovaný vztah"),col=c("red","green"),lwd=2, inset=.05)
  })
  
  output$srovnanitrueOSP <- renderPlot({
    plot(truesc_FIN()[,"OSP"], truepc_FIN()[,"OSP"], xlab="pravý skór", ylab="pravý percentil", 
         main="Srovnání pravého skóre a pravého percentilu (OSP)", pch=".")
    lines(smooth.spline(truesc_FIN()[,"OSP"],truepc_FIN()[,"OSP"], keep.data=F), col="green", lwd=2)
    lines(seq(-5,5,by=.1),2*pnorm(seq(-5,5,by=.1), sd=sd(truesc_FIN()[,"OSP"])), col="red", lwd=2)
    abline(h=1,v=0, col="gray")
    abline(h=input$cut/100, col="black")
    legend("bottomright",c("předpokládaný vztah","pozorovaný vztah"),col=c("red","green"),lwd=2, inset=.05)
  })
  output$srovnanitrueTSP <- renderPlot({
    plot(truesc_FIN()[,"TSP"], truepc_FIN()[,"TSP"], xlab="pravý skór", ylab="pravý percentil", 
         main="Srovnání pravého skóre a pravého percentilu (TSP)", pch=".")
    lines(smooth.spline(truesc_FIN()[,"TSP"],truepc_FIN()[,"TSP"], keep.data=F), col="green", lwd=2)
    lines(seq(-5,5,by=.1),2*pnorm(seq(-5,5,by=.1), sd=sd(truesc_FIN()[,"TSP"])), col="red", lwd=2)
    abline(h=1,v=0, col="gray")
    abline(h=input$cut/100, col="black")
    legend("bottomright",c("předpokládaný vztah","pozorovaný vztah"),col=c("red","green"),lwd=2, inset=.05)
  })
  
  
  
  
  ######################## TEXTOVÝ OUTPUT ############################################
    
  output$text1 <- renderTable({
    matrix(c(colSums(prijatTRUE())/input$N, 1-input$cut/200), dimnames = list("pravděpodobnost přijetí",c("ZSV+OSP","ZSV+TSP","předpoklad")), nrow=1, ncol=3)
  })
  
  output$text2 <- renderTable({
    soucty <- colSums(prijat())
    matrix(c(soucty/input$N, soucty), nrow = 2, byrow = T,
           dimnames = list(c("pravděpodobnost přijetí", "počet přijatých (když by všichni volili danou možnost)"), 
                           c("1x OSP + 1x ZSV","1x TSP + 1x ZSV","Nx OSP + Nx ZSV","1x TSP + Nx ZSV")))
  })
  
  output$spravnostOSP1 <- renderTable({
    spravnostOSP1 <- xtabs(~prijatTRUE()[,"OSP"]+prijat()[,"OSP1"])
    dimnames(spravnostOSP1) <- list(pravý=c("měl být přijat: ne","měl být přijat: ano"),pozorovaný=c("byl přijat: ne","byl přijat: ano"))
    spravnostOSP1
  })
  output$spravnostOSPn <- renderTable({
    spravnostOSPn <- xtabs(~prijatTRUE()[,"OSP"]+prijat()[,"OSPn"])
    dimnames(spravnostOSPn) <- list(pravý=c("měl být přijat: ne","měl být přijat: ano"),pozorovaný=c("byl přijat: ne","byl přijat: ano"))
    spravnostOSPn
  })
  
  output$spravnostTSP1 <- renderTable({
    spravnostTSP1 <- xtabs(~prijatTRUE()[,"TSP"]+prijat()[,"TSP1"])
    dimnames(spravnostTSP1) <- list(pravý=c("měl být přijat: ne","měl být přijat: ano"),pozorovaný=c("byl přijat: ne","byl přijat: ano"))
    spravnostTSP1
  })
  output$spravnostTSPn <- renderTable({
    spravnostTSPn <- xtabs(~prijatTRUE()[,"TSP"]+prijat()[,"TSPn"])
    dimnames(spravnostTSPn) <- list(pravý=c("měl být přijat: ne","měl být přijat: ano"),pozorovaný=c("byl přijat: ne","byl přijat: ano"))
    spravnostTSPn
  })

# Histogramy rozložení přijatých ------------------------------------------
  
  output$hist1 <- renderPlot({
    barvy <- c(rgb(0,0,204,100,maxColorValue=255), rgb(0,204,0,100,maxColorValue=255))
    hist(truepc_FIN()[prijat()[,"OSP1"] == 0,"OSP"], xlim=c(0,2), main="1x ZSV + 1x OSP", xlab="pravý percentil", ylab="frekvence", 
         col=barvy[1])
    hist(truepc_FIN()[prijat()[,"OSP1"] == 1,"OSP"], xlim=c(0,2),
         col=barvy[2], add=T)
    abline(v=input$cut/100, lwd=2)
    legend("bottomleft", inset=.05, c("nepřijat","přijat"), col=barvy, pch=15)
  })
  
  output$hist2 <- renderPlot({
    barvy <- c(rgb(0,0,204,100,maxColorValue=255), rgb(0,204,0,100,maxColorValue=255))
    hist(truepc_FIN()[prijat()[,"OSPn"] == 0,"OSP"], xlim=c(0,2), main="Nx ZSV + Nx OSP", xlab="pravý percentil", ylab="frekvence", 
         col=barvy[1])
    hist(truepc_FIN()[prijat()[,"OSPn"] == 1,"OSP"], xlim=c(0,2),
         col=barvy[2], add=T)
    abline(v=input$cut/100, lwd=2)
    legend("bottomleft", inset=.05, c("nepřijat","přijat"), col=barvy, pch=15)
  })
  
  output$hist3 <- renderPlot({
    barvy <- c(rgb(0,0,204,100,maxColorValue=255), rgb(0,204,0,100,maxColorValue=255))
    hist(truepc_FIN()[prijat()[,"TSP1"] == 0,"TSP"], xlim=c(0,2), main="1x ZSV + 1x TSP", xlab="pravý percentil", ylab="frekvence", 
         col=barvy[1])
    hist(truepc_FIN()[prijat()[,"TSP1"] == 1,"TSP"], xlim=c(0,2),
         col=barvy[2], add=T)
    abline(v=input$cut/100, lwd=2)
    legend("bottomleft", inset=.05, c("nepřijat","přijat"), col=barvy, pch=15)
  })
  
  output$hist4 <- renderPlot({
    barvy <- c(rgb(0,0,204,100,maxColorValue=255), rgb(0,204,0,100,maxColorValue=255))
    hist(truepc_FIN()[prijat()[,"TSPn"] == 0,"TSP"], xlim=c(0,2), main="Nx ZSV + 1x TSP", xlab="pravý percentil", ylab="frekvence", 
         col=barvy[1])
    hist(truepc_FIN()[prijat()[,"TSPn"] == 1,"TSP"], xlim=c(0,2),
         col=barvy[2], add=T)
    abline(v=input$cut/100, lwd=2)
    legend("bottomleft", inset=.05, c("nepřijat","přijat"), col=barvy, pch=15)
  })
  


  
  
  
  output$srovnaniOSP1TSP1 <- renderTable({
    srovnaniOSP1TSP1 <- xtabs(~prijat()[,"OSP1"]+prijat()[,"TSP1"])
    srovnaniOSP1TSP1 <- cbind(srovnaniOSP1TSP1,rowSums(srovnaniOSP1TSP1)/input$N)
    srovnaniOSP1TSP1 <- rbind(srovnaniOSP1TSP1,colSums(srovnaniOSP1TSP1)/input$N)
    srovnaniOSP1TSP1[3,3] <- (srovnaniOSP1TSP1[2,3]/srovnaniOSP1TSP1[1,3])/(srovnaniOSP1TSP1[3,2]/srovnaniOSP1TSP1[3,1])
    dimnames(srovnaniOSP1TSP1) <- list(c("OSP: nepřijat","OSP: přijat","celkem %"),c("TSP: nepřijat","TSP: přijat","celkem %"))
    srovnaniOSP1TSP1
  })
  
  output$srovnaniOSPnTSPn <- renderTable({
    srovnaniOSPnTSPn <- xtabs(~prijat()[,"OSPn"]+prijat()[,"TSPn"])
    srovnaniOSPnTSPn <- cbind(srovnaniOSPnTSPn,rowSums(srovnaniOSPnTSPn)/input$N)
    srovnaniOSPnTSPn <- rbind(srovnaniOSPnTSPn,colSums(srovnaniOSPnTSPn)/input$N)
    dimnames(srovnaniOSPnTSPn) <- list(c("OSP: nepřijat","OSP: přijat","celkem %"),c("TSP: nepřijat","TSP: přijat","celkem %"))
    srovnaniOSPnTSPn[3,3] <- (srovnaniOSPnTSPn[2,3]/srovnaniOSPnTSPn[1,3])/(srovnaniOSPnTSPn[3,2]/srovnaniOSPnTSPn[3,1])
    srovnaniOSPnTSPn
  })
  
  output$srovnaniOSPnOSP1 <- renderTable({
    srovnaniOSPnOSP1 <- xtabs(~prijat()[,"OSPn"]+prijat()[,"OSP1"])
    srovnaniOSPnOSP1 <- cbind(srovnaniOSPnOSP1,rowSums(srovnaniOSPnOSP1)/input$N)
    srovnaniOSPnOSP1 <- rbind(srovnaniOSPnOSP1,colSums(srovnaniOSPnOSP1)/input$N)
    dimnames(srovnaniOSPnOSP1) <- list(c("mnohokrát: nepřijat","mnohokrát: přijat","celkem %"),c("jednou: nepřijat","jednou: přijat","celkem %"))
    srovnaniOSPnOSP1[3,3] <- (srovnaniOSPnOSP1[2,3]/srovnaniOSPnOSP1[1,3])/(srovnaniOSPnOSP1[3,2]/srovnaniOSPnOSP1[3,1])
    srovnaniOSPnOSP1
  })
  
  output$srovnaniTSPnTSP1 <- renderTable({
    srovnaniTSPnTSP1 <- xtabs(~prijat()[,"TSPn"]+prijat()[,"TSP1"])
    srovnaniTSPnTSP1 <- cbind(srovnaniTSPnTSP1,rowSums(srovnaniTSPnTSP1)/input$N)
    srovnaniTSPnTSP1 <- rbind(srovnaniTSPnTSP1,colSums(srovnaniTSPnTSP1)/input$N)
    dimnames(srovnaniTSPnTSP1) <- list(c("mnohokrát: nepřijat","mnohokrát: přijat","celkem %"),c("jednou: nepřijat","jednou: přijat","celkem %"))
    srovnaniTSPnTSP1[3,3] <- (srovnaniTSPnTSP1[2,3]/srovnaniTSPnTSP1[1,3])/(srovnaniTSPnTSP1[3,2]/srovnaniTSPnTSP1[3,1])
    srovnaniTSPnTSP1
  })
  
  output$srovnaniTSP1OSPn <- renderTable({
    srovnaniTSP1OSPn <- xtabs(~prijat()[,"OSPn"]+prijat()[,"TSP1"])
    srovnaniTSP1OSPn <- cbind(srovnaniTSP1OSPn,rowSums(srovnaniTSP1OSPn)/input$N)
    srovnaniTSP1OSPn <- rbind(srovnaniTSP1OSPn,colSums(srovnaniTSP1OSPn)/input$N)
    dimnames(srovnaniTSP1OSPn) <- list(c("mnohokrát: nepřijat","mnohokrát: přijat","celkem %"),c("jednou: nepřijat","jednou: přijat","celkem %"))
    srovnaniTSP1OSPn[3,3] <- (srovnaniTSP1OSPn[2,3]/srovnaniTSP1OSPn[1,3])/(srovnaniTSP1OSPn[3,2]/srovnaniTSP1OSPn[3,1])
    srovnaniTSP1OSPn
  })
  
  
  
  output$reliabilita <- renderTable({
    reliabilita <- reliabilitaFIN()
    dimnames(reliabilita) <- list(c("percentily", "hrubé skóry"), c("1x OSP + 1x ZSV", "Nx OSP + Nx ZSV","1x TSP + 1x ZSV", "1x TSP + Nx ZSV"))
    reliabilita
  })
  
  
  
  
})



