library(rethinking)
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-9.0.1')
library(xlsx)
data <- data.frame(read.xlsx(file='data.xlsx',sheetIndex=1,startRow = 1,endRow=172,header = T))
#load('./Data/a.RData')
#d = read.xlsx('data.xlsx',sheetIndex=2,startRow = 1,endRow=172,header = T)
#data <- read.csv('data/mydata1.csv',header = T)
d <- data
d$e.s <- (data$education-mean(data$education))/sd(data$education)
d$b.s <- (data$burn.r-mean(data$burn.r))/sd(data$burn.r)
d$ec.s <- (data$economic-mean(data$economic))/sd(data$economic)


shinyServer(function(input, output) { 
  output$distPlot <- renderPlot({
    
    t6<- map(
      alist(
        crime.r ~ dnorm( mu , sigma ) ,
        mu <- a + b1*b.s+b2*ec.s+b3*ec.s*b.s ,
        a ~ dnorm( 4.89784 , 1) ,
        b1 ~ dnorm( -0.29673 , 1 ) ,
        b2 ~ dnorm( 0.02945 , 1 ) ,
        b3 ~dnorm( 0.40648, 1 ) ,
        sigma ~ dunif( 0 , 2)
      ) ,
      data=d )
    
    
   data.s <- subset(d, input$burnrate[1]<burn.r & burn.r<input$burnrate[2])
   data.s1 <- subset(d, ec.s< -0.6912 & input$burnrate[1]<burn.r & burn.r<input$burnrate[2])
   data.s2 <- subset(d, ec.s<0.2636 & ec.s>-0.6912 & input$burnrate[1]<burn.r & burn.r<input$burnrate[2])
   data.s3 <- subset(d, ec.s>0.2636 & input$burnrate[1]<burn.r & burn.r<input$burnrate[2])
       plot( crime.r ~ burn.r ,data=data.s , col="white"
          , xlab="fertility rate" ,
          ylab = "crime rate",
          xlim=c(input$burnrate[1],input$burnrate[2]))

    if(any(input$rawdata==1 & input$economic==1)){
      par(new=TRUE)
      points( crime.r ~ burn.r , data=data.s1 ,
            col= "gray")
    }
    if(any(input$rawdata==1 & input$economic==2)){
      par(new=TRUE)
     points( crime.r ~ burn.r , data=data.s2 , 
            col= "blue")
    }
    if(any(input$rawdata==1 & input$economic==3)){
      par(new=TRUE)
      points( crime.r ~ burn.r , data=data.s3 ,
            col= "green")
    }

    burn.seq <- seq(from=(input$burnrate[1]-mean(data$burn.r))/sd(data$burn.r), 
                    to= (input$burnrate[2]-mean(data$burn.r))/sd(data$burn.r), length.out=input$observation)
    if(any(input$economic==1)){
      d1 <- data.frame(ec.s=-1.5553, b.s=burn.seq) 
      p1 <- link(t6, data = d1)
      p1mu <- apply(p1, 2, mean)
      p1pi <- apply(p1, 2, PI)
      lines( burn.seq*sd(data$burn.r)+mean(data$burn.r) , p1mu )
      shade( p1pi ,  burn.seq*sd(data$burn.r)+mean(data$burn.r))
    }
    if(any(input$economic==2)){
      d2 <- data.frame(ec.s=-0.3, b.s=burn.seq) 
      p2 <- link(t6, data = d2)
      p2mu <- apply(p2, 2, mean)
      p2pi <- apply(p2, 2, PI)
      lines( burn.seq*sd(data$burn.r)+mean(data$burn.r) , p2mu )
      shade( p2pi , burn.seq*sd(data$burn.r)+mean(data$burn.r) )
    }
    if(any(input$economic==3)){
      d3 <- data.frame(ec.s=0.2636, b.s=burn.seq) 
      p3 <- link(t6, data = d3)
      p3mu <- apply(p3, 2, mean)
      p3pi <- apply(p3, 2, PI)
      lines( burn.seq*sd(data$burn.r)+mean(data$burn.r) , p3mu )
      shade( p3pi , burn.seq*sd(data$burn.r)+mean(data$burn.r) )
    }
    
  })
  output$summary <- renderPrint({
    summary(data)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(data)
  })
  })

