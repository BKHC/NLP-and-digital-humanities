#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(showtext)
showtext_auto()
library(shiny)
library(shiny.i18n)
library(deSolve) # package for solving diferential equation

######################################################## 
# Global options - define the predator - prey function #
######################################################## 
######################################################## 

i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("en")
lvpp <- function(pp.time,pp.init,pp.params) {
  with (as.list(c(pp.time,pp.init,pp.params)), {
    # Parameters
    # N = prey population size; P = predator population size
    # r = intrinsic growth rate of prey
    # a = predation efficiency
    # b = conversion efficiency of prey into predator
    # d = intrinsic dseath rate of predator
    # prey_k = carrying capacity for prey; only used if user-defined
    
    # if no carrying capacity for prey, use this equation
    if (pp.params["prey_k"] == -9999 ){
      dNdt <- (r*N) - (a*N*P)
    }
    
    # else use the defined prey carrying capacity to compute the values.
    else {
      dNdt <- ((r*N)*(1-(N/prey_k))) - (a*N*P)
    }
    
    dPdt <- (b*a*N*P) - (d*P)
    
    # return the vectors as a list
    return(list(c(dNdt,dPdt)))
  })
}

### Reactive code

pp.init<- c()
pp.params <- c()
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
  })
  
  # Generate carrying capacity option if user selects 'set carrying capacity' option
  output$UIpreyk <- renderUI({
    if (input$Prey_K == 2){
      numericInput("K",label=h6(i18n$t("Select a value for K (prey's carrying capacity).")),value=500,min=1)}
    else {""}      
  })
  
  # Global setup ----------
  pp.params <- reactive({
    input$goButton
    isolate(c("r" = input$r, "a" = input$a, "d" = input$d, "b" = input$b, "prey_k" = ifelse(input$Prey_K == 2, input$K, -9999)))
  })
  
  pp.init <- reactive({
    input$goButton
    isolate(c("N" = input$N, "P" = input$P))
  })
  
  pp.time <- reactive({
    input$goButton
    isolate(seq(0,input$time,by=1))
  })
  
  param_text <- reactive({
    pp.params <- pp.params()
    pp.init <- pp.init()
    
    paste("N = ", pp.init["N"], ", P = ", pp.init["P"], ", r = ", pp.params["r"], ", a = ", pp.params["a"], ", d = ", pp.params["d"], ", b = ", pp.params["b"], ", Prey K = ", ifelse(input$Prey_K == 2, pp.params["prey_k"], "None"), sep = "")
  })
  
  full_table<- reactive({
    pp.time <- pp.time()
    pp.params <- pp.params()
    pp.init <- pp.init()
    
    # Return the table!
    floor(as.data.frame(ode(func=lvpp,y=pp.init,parms=pp.params,times=pp.time)))
    
  })
  
  # Render outputs -----------    
  
  # Table tab
  output$table1 <- renderTable({      
    generated_df <- full_table()
    generated_df
  })
  
  # Plots
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  plot1 <- reactive({
    pp.time <- pp.time()
    pp.params <- pp.params()
    pp.init <- pp.init()
    lvout <- full_table()
    # Begin plotting!
    
    plot(lvout$P~lvout$N,ylim=c(0,max(lvout$P)*1.25),xlim=c(0,max(lvout$N)*1.25),type="l",lwd=1.5,
         xlab=i18n$t("Prey population size"),ylab=i18n$t("Predator population size"), cex.lab = 1.25)
    points(x=pp.init["N"],y=pp.init["P"],col="red",pch=18,cex=1.75)
    
    abline(v=pp.params["d"]/(pp.params["b"]*pp.params["a"]))
    if (pp.params["prey_k"] == -9999) {
      abline(h=pp.params["r"]/pp.params["a"])
    }
    if (pp.params["prey_k"] != -9999) {
      abline (b=-(pp.params["r"]/(pp.params["prey_k"]*pp.params["a"])), a = pp.params["r"]/pp.params["a"])
    }
    
  })
  plot2 <- reactive({      
    pp.time <- pp.time()
    pp.params <- pp.params()
    pp.init <- pp.init()
    lvout <- full_table()
    plot(lvout$N~pp.time,type="l",xlab=i18n$t("Time"),lwd=1.5,
         ylab=i18n$t("Population Size"),ylim=c(0,max(max(lvout$N),max(lvout$P))*1.25), cex.lab = 1.25)
    points(lvout$P~pp.time,col="red",type="l",lwd=1.5)
    legend(x="topright",col=c("black","red"),lty=1,legend=c(i18n$t("Prey"),i18n$t("Predator")),bty="n",lwd=2)
    mtext(side = 3, line = 0, text = i18n$t("Brush an area over me to change axes of bottom graph"))
  })
  plot3 <- reactive({      
    pp.time <- pp.time()
    pp.params <- pp.params()
    pp.init <- pp.init()
    lvout <- full_table()
    plot(lvout$N~pp.time,type="l",xlab=i18n$t("Time"),lwd=1.5,
         ylab=i18n$t("Population Size"),ylim=ranges$y, cex.lab = 1.25, xlim = ranges$x)
    points(lvout$P~pp.time,col="red",type="l",lwd=1.5)
    legend(x="topright",col=c("black","red"),lty=1,legend=c(i18n$t("Prey"),i18n$t("Predator")),bty="n",lwd=2)
  })
  
  plot2print <- function() {
    pp.time <- pp.time()
    pp.params <- pp.params()
    pp.init <- pp.init()
    lvout <- full_table()
    par(mfrow = c(1,2))
    
    # Plot 1
    plot(lvout$P~lvout$N,ylim=c(0,max(lvout$P)*1.25),xlim=c(0,max(lvout$N)*1.25),type="l",lwd=1.5,
         xlab=i18n$t("Prey population size"),ylab=i18n$t("Predator population size"), cex.lab = 1.25)
    points(x=pp.init["N"],y=pp.init["P"],col="red",pch=18,cex=1.75)
    
    abline(v=pp.params["d"]/(pp.params["b"]*pp.params["a"]))
    if (pp.params["prey_k"] == -9999) {
      abline(h=pp.params["r"]/pp.params["a"])
    }
    if (pp.params["prey_k"] != -9999) {
      abline (b=-(pp.params["r"]/(pp.params["prey_k"]*pp.params["a"])), a = pp.params["r"]/pp.params["a"])
    }
    
    # Plot 2
    plot(lvout$N~pp.time,type="l",xlab="pp.time",lwd=1.5,
         ylab="Population Size",ylim=c(0,max(max(lvout$N),max(lvout$P))*1.25), cex.lab = 1.25)
    points(lvout$P~pp.time,col="red",type="l",lwd=1.5)
    legend(x="topright",col=c("black","red"),lty=1,legend=c("Prey","Predator"),bty="n",lwd=2)
    
  }
  
  output$plot1<- renderPlot({plot1()})
  output$plot2 <- renderPlot({plot2()})
  output$plot3 <- renderPlot({plot3()})
  
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$downloadPlot <- downloadHandler(
    filename = 
      function() {
        paste('plot-', Sys.Date(), '.png', sep='')
      },
    content = function(filename) {
      png(filename, height = 500, width = 750)
      par(oma = c(2,0,0,0))
      plot2print()
      mtext(param_text(), side = 1, outer = TRUE)
      dev.off()
    }
  )
  
})
