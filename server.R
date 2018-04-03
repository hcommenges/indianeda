##############################
# Shiny App: indianeda
# User interface
##############################


shinyServer(function(input, output, session) {
  
  baseData <- reactiveValues(censusdata = NULL)
  
  # DYNAMIC UI ----
  
  observe({
    if(input$delna == TRUE){
      oneState <- sfIndia[sfIndia$L1_NAME == input$choosestate, ]
      idNa <- apply(oneState, 1, anyNA)
      baseData$censusdata <- oneState[!idNa, ]
    } else {
      oneState <- sfIndia[sfIndia$L1_NAME == input$choosestate, ]
      baseData$censusdata <- oneState
    }
  })
  
  observe({
    updateSelectInput(session = session,
                      inputId = "choosestate",
                      choices = sort(unique(sfIndia$L1_NAME)))
  })
  
  observe({
    colNoGeom <- !grepl(pattern = "geom", x = colnames(sfIndia))
    columnList <- c("", colnames(sfIndia)[colNoGeom])
    allInputs <- c("univar", "qualidep", "qualiindep", 
                   "quantidep", "quantiindep", "quanlidep", "quanliindep",
                   "aovdep", "aovindep", "ancovdep", "ancovindep", "quanti2dep", "quanti2indep",
                   "regmultdep", "regmultindep", "factovar", "cahvar", "cartovar", "expcartovar")
    
    sapply(X = allInputs, 
           FUN = function(x) updateSelectInput(session = session, inputId = x, choices = columnList))
  })
  
  
  # ADD COLUMNS ----
  
  # Add regression residuals (simple)
  
  observeEvent(input$addreg1resid, {
    if (input$reg1prefix != ""){
      absName <- paste(input$reg1prefix, "AbsResid", sep = "_")
      relName <- paste(input$reg1prefix, "RelResid", sep = "_")
    } else {
      absName <- paste("lm", "AbsResid", sep = "_")
      relName <- paste("lm", "RelResid", sep = "_")
    }
    
    baseData$censusdata[[absName]] <- regMod()$TABRESID[, 1]
    baseData$censusdata[[relName]] <- regMod()$TABRESID[, 2]
  })
  
  # Add regression residuals (anova)
  
  observeEvent(input$addaov1resid, {
    if (input$aov1prefix != ""){
      absName <- paste(input$aov1prefix, "AbsResid", sep = "_")
      relName <- paste(input$aov1prefix, "RelResid", sep = "_")
    } else {
      absName <- paste("anova1", "AbsResid", sep = "_")
      relName <- paste("anova1", "RelResid", sep = "_")
    }
    
    baseData$censusdata[[absName]] <- aovMod()$TABRESID[, 1]
    baseData$censusdata[[relName]] <- aovMod()$TABRESID[, 2]
  })
  
  # Add regression residuals (anova 2)
  
  observeEvent(input$addaov2resid, {
    if (input$aov2prefix != ""){
      absName <- paste(input$aov2prefix, "AbsResid", sep = "_")
      relName <- paste(input$aov2prefix, "RelResid", sep = "_")
    } else {
      absName <- paste("anova2", "AbsResid", sep = "_")
      relName <- paste("anova2", "RelResid", sep = "_")
    }
    
    baseData$censusdata[[absName]] <- aov2Mod()$TABRESID[, 1]
    baseData$censusdata[[relName]] <- aov2Mod()$TABRESID[, 2]
  })
  
  # Add regression residuals (ancova)
  
  observeEvent(input$addreg2resid, {
    if (input$reg2prefix != ""){
      absName <- paste(input$reg2prefix, "AbsResid", sep = "_")
      relName <- paste(input$reg2prefix, "RelResid", sep = "_")
    } else {
      absName <- paste("lm", "AbsResid", sep = "_")
      relName <- paste("lm", "RelResid", sep = "_")
    }
    
    baseData$censusdata[[absName]] <- ancovMod()$TABRESID[, 1]
    baseData$censusdata[[relName]] <- ancovMod()$TABRESID[, 2]
  })
  
  # Add regression residuals (2 quanti)
  
  observeEvent(input$addreg3resid, {
    if (input$reg3prefix != ""){
      absName <- paste(input$reg3prefix, "AbsResid", sep = "_")
      relName <- paste(input$reg3prefix, "RelResid", sep = "_")
    } else {
      absName <- paste("lm", "AbsResid", sep = "_")
      relName <- paste("lm", "RelResid", sep = "_")
    }
    
    baseData$censusdata[[absName]] <- reg2Mod()$TABRESID[, 1]
    baseData$censusdata[[relName]] <- reg2Mod()$TABRESID[, 2]
  })
  
  # Add regression residuals (multiple)
  
  observeEvent(input$addreg4resid, {
    if (input$reg1prefix != ""){
      absName <- paste(input$reg1prefix, "AbsResid", sep = "_")
      relName <- paste(input$reg1prefix, "RelResid", sep = "_")
    } else {
      absName <- paste("lm", "AbsResid", sep = "_")
      relName <- paste("lm", "RelResid", sep = "_")
    }
    
    baseData$censusdata[[absName]] <- regmultMod()$TABRESID[, 1]
    baseData$censusdata[[relName]] <- regmultMod()$TABRESID[, 2]
  })
  
  
  # Add factorial coordinates
  
  observeEvent(input$addfaccoord, {
    if (input$facprefix != ""){
      compNames <- paste(input$facprefix, c("C1", "C2", "C3", "C4"), sep = "_")
    } else {
      compNames <- c("C1", "C2", "C3", "C4")
    }
    
    baseData$censusdata[[compNames[1]]] <- principalComp()$li[, 1]
    baseData$censusdata[[compNames[2]]] <- principalComp()$li[, 2]
    baseData$censusdata[[compNames[3]]] <- principalComp()$li[, 3]
    baseData$censusdata[[compNames[4]]] <- principalComp()$li[, 4]
  })
  
  # Add classif clusters
  
  observeEvent(input$addcahclass, {
    if (input$cahprefix != ""){
      className <- paste(input$cahprefix, "CLASSES", sep = "_")
    } else {
      className <- "CLASSES"
    }
    
    baseData$censusdata[[className]] <- PlotProfile(classifobj = clusterComp(), nbclus = input$cahnclass)$CLUSID
  })
  
  
  # GUIDE  ----
  
  output$citation <- renderText("<strong>Once upon a time, statisticians only explored</strong> (John Tukey)")
  
  
  # DONNEES ----
  
  # show table
  output$contentstable <- renderDataTable(options = list(pageLength = 10), expr = {
    baseData$censusdata %>% st_set_geometry(NULL)
  })
  
  output$description <- renderText("<strong>Projet ANR Cartelec</strong> (cf. Guide d'utilisation).")
  
  # download data
  output$downloaddata <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      if (input$csvtype == "anglo"){
        write.csv(x = baseData$censusdata %>% st_set_geometry(NULL), file = file, row.names = FALSE)
      } else {
        write.csv2(x = baseData$censusdata %>% st_set_geometry(NULL), file = file, row.names = FALSE)
      }
    })
  
  
  # UNIVARIE ----
  
  # summary
  output$unisummary <- renderText({
    req(input$univar)
    if (is.numeric(baseData$censusdata[[input$univar]]) & input$unitype == FALSE){
      textResult <- paste("Nb. obs. = ", nrow(baseData$censusdata), "<br/>",
                          "Valeurs manquantes = ", anyNA(baseData$censusdata[[input$univar]]), "<br/>",
                          "Moyenne = ", round(mean(baseData$censusdata[[input$univar]], na.rm = TRUE), digits = 2), "<br/>",
                          "Médiane = ", round(median(baseData$censusdata[[input$univar]], na.rm = TRUE), digits = 2), "<br/>",
                          "Variance = ", round(var(baseData$censusdata[[input$univar]], na.rm = TRUE), digits = 2), "<br/>",
                          "Coef. de variation = ", round(sd(baseData$censusdata[[input$univar]], na.rm = TRUE) / mean(baseData$censusdata[[input$univar]], na.rm = TRUE), digits = 2),
                          sep = "")
      return(textResult)
      
    } else if (!is.numeric(baseData$censusdata[[input$univar]]) | input$unitype == TRUE) {
      textResult <- paste("Nb. obs. = ", nrow(baseData$censusdata), "<br/>",
                          "Valeurs manquantes = ", anyNA(baseData$censusdata[[input$univar]]),
                          sep = "")
      return(textResult)
    } else {
      return()
    }
  })
  
  # plot univariate
  output$unitab <- renderTable(include.rownames = FALSE, expr = {
    req(input$univar)
    if (is.numeric(baseData$censusdata[[input$univar]]) & input$unitype == FALSE){
      return()
    } else if (!is.numeric(baseData$censusdata[[input$univar]]) | input$unitype == TRUE) {
      if (length(unique(baseData$censusdata[[input$univar]])) > 40){
        stop("La variable sélectionnée n'est probablement pas qualitative")
      } else {
        tabFreq <- as.data.frame(table(baseData$censusdata[[input$univar]]))
        tabFreq$Perc <- round(100 * tabFreq$Freq / sum(tabFreq$Freq), digits = 2)
        colnames(tabFreq) <- c("Modalité", "Freq. absolue", "Freq. relative")
        return(tabFreq)
      }} else {
        return()
      }
  })
  
  # plot univariate
  output$uniplot <- renderPlot({
    req(input$univar)
    if (is.numeric(baseData$censusdata[[input$univar]]) & input$unitype == FALSE){
      Histogram(df = baseData$censusdata, varquanti = input$univar, nbins = input$nbins, drawsummary = input$drawsummary)
    } else if (!is.numeric(baseData$censusdata[[input$univar]]) | input$unitype == TRUE) {
      if (length(unique(baseData$censusdata[[input$univar]])) > 40){
        stop("La variable sélectionnée n'est probablement pas qualitative")
      } else {
        Barplot(df = baseData$censusdata, varquali = input$univar)
      }
    } else {
      return()
    }
  })
  
  # download plot
  output$downloaduniplot <- downloadHandler(
    filename = "uniplot.svg",
    content = function(file) {
      svg(file, width = input$widthuni / 2.54, height = input$heightuni / 2.54, pointsize = 8)
      if (is.numeric(baseData$censusdata[[input$univar]]) & input$unitype == FALSE){
        print(Histogram(df = baseData$censusdata, varquanti = input$univar, nbins = input$nbins, drawsummary = input$drawsummary))
      } else if (!is.numeric(baseData$censusdata[[input$univar]]) | input$unitype == TRUE) {
        print(Barplot(df = baseData$censusdata, varquali = input$univar))
      } else {
        return()
      }
      dev.off()
    })
  
  
  
  # BIVARIE : QUALI-QUALI ----
  
  # Print mosaic plot
  output$mosaic <- renderPlot({
    req(input$qualiindep, input$qualidep)
    if((length(unique(baseData$censusdata[[input$qualidep]])) - 1) * (length(unique(baseData$censusdata[[input$qualiindep]])) - 1) > 150){
      stop("La ou les variables sélectionnées ne sont probablement pas qualitatives (ddl > 150)")
    } else {
      Mosaicplot(df = baseData$censusdata, varx = input$qualidep, vary = input$qualiindep)
    }
  })
  
  # contingency table
  output$contingtab <- renderTable(rownames = TRUE, expr = {
    req(input$qualiindep, input$qualidep)
    levelsRow <- sort(unique(baseData$censusdata[[input$qualiindep]]))
    levelsCol <- sort(unique(baseData$censusdata[[input$qualidep]]))
    
    chiResults <- chisq.test(baseData$censusdata[[input$qualiindep]], baseData$censusdata[[input$qualidep]])
    
    if(input$contcont == "obsfreq"){
      matRes <- matrix(chiResults$observed, nrow = length(levelsRow))
      row.names(matRes) <- levelsRow
      colnames(matRes) <- levelsCol
    }
    else if(input$contcont == "rowpct"){
      matRes <- matrix(data = 100 * prop.table(table(baseData$censusdata[[input$qualiindep]], baseData$censusdata[[input$qualidep]]), margin = 1), 
                       nrow = length(levelsRow),
                       ncol = length(levelsCol))
      row.names(matRes) <- levelsRow
      colnames(matRes) <- levelsCol
    }
    else if(input$contcont == "expfreq"){
      matRes <- matrix(round(chiResults$expected, digits = 0), nrow = length(levelsRow))
      mode(matRes) <- "integer"
      row.names(matRes) <- levelsRow
      colnames(matRes) <- levelsCol
    }
    else if(input$contcont == "rawresid"){
      matRes <- matrix(chiResults$observed - chiResults$expected, nrow = length(levelsRow))
      row.names(matRes) <- levelsRow
      colnames(matRes) <- levelsCol
    }
    else if(input$contcont == "stdresid"){
      matRes <- matrix(chiResults$residuals, nrow = length(levelsRow))
      row.names(matRes) <- levelsRow
      colnames(matRes) <- levelsCol
    }
    return(matRes)
  })
  
  # contingency text
  output$contingtext <- renderText({
    req(input$qualiindep, input$qualidep)
    chiResults <- chisq.test(baseData$censusdata[[input$qualidep]], baseData$censusdata[[input$qualiindep]])
    textResult <- paste("Chi2 = ", round(chiResults$statistic, 2), "<br/>",
                        "Phi = ", round(sqrt(chiResults$statistic / nrow(baseData$censusdata)), 2), "<br/>",
                        "V de Cramer = ", round(sqrt(chiResults$statistic / (nrow(baseData$censusdata) * min(dim(chiResults$observed)))), 2), 
                        sep = "")
    return(textResult)
  })
  
  # download plot
  output$downloadmosaicplot <- downloadHandler(
    filename = "mosaicplot.svg",
    content = function(file) {
      svg(file, width = input$widthmosaic / 2.54, height = input$heightmosaic / 2.54, pointsize = 8)
      req(input$qualiindep, input$qualidep)
      print(Mosaicplot(df = baseData$censusdata, varx = input$qualiindep, vary = input$qualidep))
      dev.off()
    })
  
  
  # BIVARIE : QUANTI-QUANTI ----
  
  # print scatter plot
  output$scatterplot <- renderPlot({
    req(input$quantiindep, input$quantidep)
    ScatterPlot(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$quantiindep, vary = input$quantidep)
  })
  
  # compute linear regression
  regMod <- reactive({
    req(input$quantiindep, input$quantidep)
    ComputeRegression(df = baseData$censusdata %>% st_set_geometry(NULL), vardep = input$quantidep, varindep = input$quantiindep)
  })
  
  # print coefficients
  output$coefreg <- renderTable(include.rownames = FALSE, expr = {
    req(input$quantiindep, input$quantidep)
    regMod()$TABCOEF
  })
  
  # download plot
  output$downloadreg1 <- downloadHandler(
    filename = "regressionplot.svg",
    content = function(file) {
      svg(file, width = input$widthreg1 / 2.54, height = input$heightreg1 / 2.54, pointsize = 8)
      req(input$quantiindep, input$quantidep)
      print(ScatterPlot(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$quantiindep, vary = input$quantidep))
      dev.off()
    })
  
  
  # BIVARIE : QUALI-QUANTI ----
  
  # print boxplot
  output$boxes <- renderPlot({
    req(input$quanliindep, input$quanlidep)
    Boxplot(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$quanliindep, vary = input$quanlidep, jit = input$bpjitter)
  })
  
  # print aovplot
  output$aovplot <- renderPlot({
    req(input$quanliindep, input$quanlidep)
    AnovaPlot(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$quanliindep, vary = input$quanlidep)
  })
  
  # compute linear regression
  aovMod <- reactive({
    req(input$quanliindep, input$quanlidep)
    ComputeRegression(df = baseData$censusdata %>% st_set_geometry(NULL), vardep = input$quanlidep, varindep = input$quanliindep)
  })
  
  # print coefficients
  output$coefanova <- renderTable(include.rownames = FALSE, expr = {
    req(input$quanliindep, input$quanlidep)
    rbind(aovMod()$TABVAR, aovMod()$TABCOEF)
  })
  
  # print mean and variance
  output$tabanova <- renderTable(include.rownames = FALSE, expr = {
    req(input$quanliindep, input$quanlidep)
    AnovaTab(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$quanliindep, vary = input$quanlidep)
  })
  
  # download plot
  output$downloadanova <- downloadHandler(
    filename = "anovaplot.svg",
    content = function(file) {
      svg(file, width = input$widthanova1 / 2.54, height = input$heightanova1 / 2.54, pointsize = 8)
      req(input$quanliindep, input$quanlidep)
      print(grid.arrange(Boxplot(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$quanliindep, vary = input$quanlidep, jit = input$bpjitter),
                         AnovaPlot(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$quanliindep, vary = input$quanlidep),
                         nrow = 2))
      dev.off()
    })
  
  
  # TRIVARIE : ANOVA 2 FACTORS ----
  
  # print boxplot
  output$boxes2 <- renderPlot({
    req(input$aovindep, input$aovdep)
    Boxplot2(df = baseData$censusdata %>% st_set_geometry(NULL), 
             varx = input$aovindep[1], 
             vary = input$aovdep, 
             groupx = input$aovindep[2])
  })
  
  # print anova plot 
  output$anovaplot2 <- renderPlot({
    req(input$aovdep, input$aovindep)
    AnovaPlot2(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$aovindep[1], vary = input$aovdep, groupx = input$aovindep[2], interact = input$interactaov2)
  })
  
  # compute linear regression
  aov2Mod <- reactive({
    req(input$aovdep, input$aovindep)
    ComputeRegression(df = baseData$censusdata %>% st_set_geometry(NULL), vardep = input$aovdep, varindep = input$aovindep, interact = input$interactaov2)
  })
  
  # print coefficients
  output$coefanova2 <- renderTable(include.rownames = FALSE, expr = {
    req(input$aovdep, input$aovindep)
    aov2Mod()$TABCOEF
  })
  
  # print mean and variance 
  output$tabanova2 <- renderTable(include.rownames = FALSE, expr = {
    req(input$aovdep, input$aovindep)
    AnovaTab2(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$aovindep[1], vary = input$aovdep, groupx = input$aovindep[2])
  })
  
  # print mean and variance 
  output$tabanova2interact <- renderTable(include.rownames = FALSE, expr = {
    req(input$aovdep, input$aovindep)
    AnovaTab2Interact(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$aovindep[1], vary = input$aovdep, groupx = input$aovindep[2])
  })
  
  # download plot
  output$downloadanova2 <- downloadHandler(
    filename = "anovaplot.svg",
    content = function(file) {
      svg(file, width = input$widthanova2 / 2.54, height = input$heightanova2 / 2.54, pointsize = 8)
      req(input$aovindep, input$aovdep)
      print(grid.arrange(Boxplot2(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$aovindep[1], vary = input$aovdep, groupx = input$aovindep[2]),
                         AnovaPlot2(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$aovindep[1], vary = input$aovdep, groupx = input$aovindep[2], interact = input$interactaov2),
                         nrow = 2))
      dev.off()
    })
  
  
  # TRIVARIE : ANCOVA ----
  
  # print scatter
  output$scatterancov <- renderPlot({
    req(input$ancovdep, input$ancovindep)
    if(input$interactancov == FALSE){
      DrawRegressionLines(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$ancovindep[1], vary = input$ancovdep, groupx = input$ancovindep[2])
    } else {
      ScatterPlotAncov(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$ancovindep[1], vary = input$ancovdep, groupx = input$ancovindep[2])
    }
  })
  
  # compute linear regression
  ancovMod <- reactive({
    req(input$ancovdep, input$ancovindep)
    ComputeRegression(df = baseData$censusdata %>% st_set_geometry(NULL), vardep = input$ancovdep, varindep = input$ancovindep, interact = input$interactancov)
  })
  
  # print coefficients
  output$coefancov <- renderTable(include.rownames = FALSE, expr = {
    req(input$ancovdep, input$ancovindep)
    ancovMod()$TABCOEF
  })
  
  # download plot
  output$downloadancova <- downloadHandler(
    filename = "ancovaplot.svg",
    content = function(file) {
      svg(file, width = input$widthancova / 2.54, height = input$heightancova / 2.54, pointsize = 8)
      req(input$ancovdep, input$ancovindep)
      print(ScatterPlotAncov(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$ancovindep[1], vary = input$ancovdep, groupx = input$ancovindep[2]))
      dev.off()
    })
  
  
  # TRIVARIE : REGRESSION ----
  
  # print scatter
  output$scatterreg2 <- renderPlot({
    if (input$quanti2dep != "" & !is.null(input$quanti2indep)){
      ScatterPlot3D(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$quanti2indep[1], vary = input$quanti2indep[2], varz = input$quanti2dep)
    } else {
      return()
    }
  })
  
  # compute linear regression
  reg2Mod <- reactive({
    if (input$quanti2dep != "" & !is.null(input$quanti2indep)){
      ComputeRegression(df = baseData$censusdata %>% st_set_geometry(NULL), vardep = input$quanti2dep, varindep = input$quanti2indep, interact = input$interactreg)
    } else {
      return()
    }
  })
  
  # print coefficients
  output$coefreg2 <- renderTable(include.rownames = FALSE, expr = {
    if (input$quanti2dep != "" & !is.null(input$quanti2indep)){
      reg2Mod()$TABCOEF
    } else {
      return()
    }
  })
  
  # download plot
  output$downloadreg2 <- downloadHandler(
    filename = "regressionplot.svg",
    content = function(file) {
      svg(file, width = input$widthreg2 / 2.54, height = input$heightreg2 / 2.54, pointsize = 8)
      if (input$quanti2dep != "" & !is.null(input$quanti2indep)){
        print(ScatterPlot3D(df = baseData$censusdata %>% st_set_geometry(NULL), varx = input$quanti2indep[1], vary = input$quanti2indep[2], varz = input$quanti2dep))
      } else {
        return()
      }
      dev.off()
    })
  
  
  
  # MULTIVARIE : REGRESSION ----
  
  # Compute linear regression
  
  regmultMod <- reactive({
    if (input$regmultdep != "" & !is.null(input$regmultindep)){
      ComputeRegressionMult(df = baseData$censusdata %>% st_set_geometry(NULL), vardep = input$regmultdep, varindep = input$regmultindep)
    } else {
      return()
    }
  })
  
  # Print matrix
  output$matcor <- renderTable(include.rownames = TRUE, expr = {
    if (input$regmultdep != "" & !is.null(input$regmultindep)){
      regmultMod()$MATCOR
    } else {
      return()
    }
  })
  
  # Print coefficients
  
  output$coefregmult <- renderTable(include.rownames = FALSE, expr = {
    if (input$regmultdep != "" & !is.null(input$regmultindep)){
      regmultMod()$TABCOEF
    } else {
      return()
    }
  })
  
  
  # MULTIVARIE : ANALYSE FACTORIELLE ----
  
  # Compute factorial analysis
  
  
  principalComp <- eventReactive(input$buttonpca, {
    ComputePrincipalComp(df = baseData$censusdata %>% st_set_geometry(NULL), varquanti = input$factovar, ident = input$idtab)
  })
  
  
  
  # Correlation matrix
  
  output$facmatcor <- renderTable({
    req(principalComp)
    CorCompMat(dudiobj = principalComp(), xaxis = input$xaxis, yaxis = input$yaxis)
  })
  
  
  # Plot components
  
  output$compinert <- renderPlot({
    req(input$factovar)
    DecompInertia(dudiobj = principalComp())
  })
  
  # Plot individuals
  
  output$indivpca <- renderPlot({
    req(input$factovar)
    PlotIndiv(dudiobj = principalComp(), xaxis = as.integer(input$xaxis), yaxis = as.integer(input$yaxis), printlabel = input$labelindiv)
  })
  
  # Plot circle of correlations
  
  output$corcircle <- renderPlot({
    req(input$factovar)
    CorCircle(dudiobj = principalComp(), xaxis = as.integer(input$xaxis), yaxis = as.integer(input$yaxis))
  })
  
  # Table of contributions (variables and observations)
  
  output$contribvar <- renderTable(digits = 0, expr = {
    if (!is.null(input$factovar)){
      ContribVarIndiv(dudiobj = principalComp())$CTRVAR
    } else {
      return()
    }
  })
  
  output$contribind <- renderDataTable(options = list(pageLength = 10), expr = {
    if (!is.null(input$factovar)){
      ContribVarIndiv(dudiobj = principalComp())$CTRIND
    } else {
      return()
    }
  })
  
  # download plots
  output$downloadpca <- downloadHandler(
    filename = "pcaplot.svg",
    content = function(file) {
      svg(file, width = input$widthpca / 2.54, height = input$heightpca / 2.54, pointsize = 8)
      req(input$factovar)
      print(grid.arrange(DecompInertia(dudiobj = principalComp()),
                         CorCircle(dudiobj = principalComp(), xaxis = as.integer(input$xaxis), yaxis = as.integer(input$yaxis)),
                         PlotIndiv(dudiobj = principalComp(), xaxis = as.integer(input$xaxis), yaxis = as.integer(input$yaxis), printlabel = input$labelindiv),
                         nrow = 3))
      dev.off()
    })
  
  
  # MULTIVARIE : CLASSIFICATION ----
  
  # Compute hierarchical clustering
  
  clusterComp <- eventReactive(input$buttoncah, {
    ComputeClassif(df = baseData$censusdata %>% st_set_geometry(NULL), varquanti = input$cahvar, stand = input$cahstandardize, method = input$cahmethod)
  })
  
  # Plot dendrogram
  
  output$cahdendro <- renderPlot({
    req(clusterComp)
    withProgress(min = 0, message = "Calcul en cours", expr = {
      PlotDendro(classifobj = clusterComp())})
  })
  
  # Plot heigth
  
  output$cahheight <- renderPlot({
    req(clusterComp)
    PlotHeight(classifobj = clusterComp())
  })
  
  # Plot profile
  
  output$cahprofile <- renderPlot({
    req(clusterComp)
    PlotProfile(classifobj = clusterComp(), nbclus = input$cahnclass)$PROFILE
  })
  
  
  # download plots
  output$downloadclus <- downloadHandler(
    filename = "clusplot.svg",
    content = function(file) {
      svg(file, width = input$widthclus / 2.54, height = input$heightclus / 2.54, pointsize = 8)
      req(clusterComp)
      print(grid.arrange(PlotDendro(classifobj = clusterComp()),
                         PlotHeight(classifobj = clusterComp()),
                         PlotProfile(classifobj = clusterComp(), nbclus = input$cahnclass)$PROFILE,
                         nrow = 3))
      dev.off()
    })
  
  reactive(print(input$idtab))
  reactive(print(input$idshape))
  
  # CARTOGRAPHIE ----
  
  output$carto <- renderPlot({
    req(input$cartovar)
    CartoVar(spdf = baseData$censusdata, varquanti = input$cartovar, paltype = input$colpal, colcol = input$colcol, discret = input$cartomethod, nbcl = input$cartoclass)
  })
  
  # download plots
  output$downloadcarto <- downloadHandler(
    filename = "cartoplot.svg",
    content = function(file) {
      svg(file, width = input$widthpca / 2.54, height = input$heightpca / 2.54, pointsize = 8)
      req(input$cartovar)
      CartoVar(spdf = baseData$censusdata, varquanti = input$cartovar, paltype = input$colpal, colcol = input$colcol, discret = input$cartomethod, nbcl = input$cartoclass)
      dev.off()
    })
  
  
  # EXPLORATION ----
  
  output$expcarto <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
      fitBounds(lng1 = 68.4, lat1 = 8.2, lng2 = 95.9, lat2 = 28.5)
  })
  
  
  observe({
    req(input$expcartovar)
    if(input$expcolpal == "quanti"){
      myPal <- colorQuantile(palette = input$expcolcol, domain = baseData$censusdata[[input$expcartovar]], n = input$expcartoclass)
    } else if (input$expcolpal == "quali"){
      myPal <- colorFactor(palette = input$expcolcol, domain = baseData$censusdata[[input$expcartovar]], n = input$expcartoclass)
    }
    leafletProxy("expcarto") %>%
      clearShapes() %>% 
      addPolygons(data = baseData$censusdata, 
                  fillColor = ~myPal(eval(parse(text = input$expcartovar))), 
                  stroke = TRUE,
                  color = "grey",
                  label = paste0(baseData$censusdata$L4_NAME, " : ", baseData$censusdata[[input$expcartovar]]),
                  weight = 1,
                  fillOpacity = input$expopac)
  })
  
})
