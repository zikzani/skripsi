library(shiny)
library(ggplot2)
library(arules)
library(arulesViz)
library(grid)
library(Matrix)
library(shinycssloaders)
library(rsconnect)



shinyServer(function(input,output){
  datasetInput<-reactive({
    switch (as.numeric(input$var1),
            "triwulan1"=cobacoba,"triwulan2"=cobacoba2,"triwulan3"=cobacoba3,"triwulan4"=cobacoba4
    )
  })
  
  datasetInput2<-reactive({
    switch (input$var3,
            "2"=tes1minlen2,"3"=tes1minlen3,"4"=tes1minlen4
    )
  })
  
  datasetInput3<-reactive({
    switch (input$var3,
            "2"=tes2minlen2,"3"=tes2minlen3,"4"=tes2minlen4
    )
  })
  
  datasetInput4<-reactive({
    switch (input$var3,
            "2"=tes3minlen2,"3"=tes3minlen3,"4"=tes3minlen4
    )
  })
  datasetInput6<-reactive({
    switch (input$var3,
            "2"=tes4minlen2barusangat,"3"=tes4minlen3barusangat,"4"=tes4minlen4barusangat
    )
  })
  
  datasetInput5<-reactive({
    switch (as.numeric(input$var1),
            "1"=golongan3triwulan1,"2"=golongan3triwulan2,"3"=golongan3triwulan3,"4"=golongan3triwulan4
    )
  })
  grosirtriwulan1=as.matrix(triwulan1)
  grosir2triwulan1=as(grosirtriwulan1,"transactions")
  
  grosirtriwulan2=as.matrix(hasilTriwulan2)
  grosir2triwulan2=as(grosirtriwulan2,"transactions")
  
  grosirtriwulan3=as.matrix(hasilTriwulan3)
  grosir2triwulan3=as(grosirtriwulan3,"transactions")

  grosirtriwulan4=as.matrix(hasiltelahsaring)
  grosir2triwulan4=as(grosirtriwulan4,"transactions")
  
  
  output$myhist<-renderPlot({
    if(input$var1==1){
      itemFrequencyPlot(grosir2triwulan1,type="absolute", topN=input$var2,col="darkblue", main="Jumlah Frekuensi Item")
   # hist(jumlahtriwulan1[,],breaks = seq(0,max(jumlahtriwulan1[,]),l=input$var2+1),col = rainbow(6),
       #  main = "histogram of Triwulan1",,xlab = names(jumlahtriwulan1[]))
    }
    else if(input$var1==2){
      itemFrequencyPlot(grosir2triwulan2,type="absolute", topN=input$var2,col="darkblue", main="Jumlah Frekuensi Item")
     # hist(jumlahtriwulan2[,],breaks = seq(0,max(jumlahtriwulan2[,]),l=input$var2+1),col = rainbow(6),
           #main = "histogram of Triwulan2",,xlab = names(jumlahtriwulan2[]))
    }
    else if(input$var1==3){
      itemFrequencyPlot(grosir2triwulan3,type="absolute", topN=input$var2,col="darkblue", main="Jumlah Frekuensi Item")
     # hist(jumlahtriwulan3[,],breaks = seq(0,max(jumlahtriwulan3[,]),l=input$var2+1),col = rainbow(6),
          # main = "histogram of Triwulan3",,xlab = names(jumlahtriwulan3[]))
    }
    else if(input$var1==4){
      itemFrequencyPlot(grosir2triwulan4,type="absolute", topN=input$var2,col="darkblue", main="Jumlah Frekuensi Item")
      # hist(jumlahtriwulan3[,],breaks = seq(0,max(jumlahtriwulan3[,]),l=input$var2+1),col = rainbow(6),
      # main = "histogram of Triwulan3",,xlab = names(jumlahtriwulan3[]))
    }
    
    
  })
  
   output$table1<-renderTable({
    head(datasetInput(),input$var2)
  })
  
 # output$plot1<-renderPlot({
  #    colm<-as.numeric(input$var1)
   #   dt1<-table(jumlahtriwulan1GOLONGAN$colm)
   #   barplot(dt1,xlab = "golongan",col = rainbow(6),ylab = "frequency",beside = TRUE,ylim = c(0,18000),xlim = c(0,5))
  #})
  fileext<-reactive({
    switch (input$type,
            "excel(CSV)" = "csv",
            "Text(TSV)"="txt",
            "Text"="txt",
            "Docx"="doc"
    )
  })
  output$down1<-downloadHandler(
    filename = function(){
      paste(input$var1,fileext(),sep = ".")
    },
    content = function(file){
      sep<-switch (input$type,
                   "excel(CSV)" = ",",
                   "Text(TSV)"="\t",
                   "Text"=" ",
                   "Docx"=" "
      )
      write.table(datasetInput(),file,sep = sep,
                  row.names = FALSE)  
      
    }
  )
  
  output$table2<-renderTable({
     if(input$var1==1){
      head(datasetInput2(),input$var2)
    }
    else if(input$var1==2){
      head(datasetInput3(),input$var2)
    }
    else if(input$var1==3){
      head(datasetInput4(),input$var2)
    }
    else if(input$var1==4){
      head(datasetInput6(),input$var2)
    }
  })
  
  output$gol<-renderPlot({
    if(input$var1==1){
    barplot(height=golongan2$TRIWULAN1,names=golongan2$TRIWULAN1,legend.text =c("NONFOOD","FOOD","DRINK","FROZEN") ,col=rainbow(6))
    }
    else if(input$var1==2){
      barplot(height=golongan2$TRIWULAN2,names=golongan2$TRIWULAN2,legend.text =c("NONFOOD","FOOD","DRINK","FROZEN"),col=rainbow(6))
    }
    else if(input$var1==3){
      barplot(height=golongan2$TRIWULAN3,names=golongan2$TRIWULAN3,legend.text =c("NONFOOD","FOOD","DRINK","FROZEN"), col = rainbow(6))
    }
    else if(input$var1==4){
      barplot(height=golongan2$TRIWULAN4,names=golongan2$TRIWULAN4,legend.text =c("NONFOOD","FOOD","DRINK","FROZEN"), col = rainbow(6))
    }
  })
    
    output$pdp<-renderPlot({
      if(input$var1==1){
        
        # Create test data.
        data <- data.frame(
          category=c("Uang Masuk", "Uang Keluar"),
          count=c( 789675108,195020191)
        )
        
        # Compute percentages
        data$fraction <- data$count / sum(data$count)
        
        # Compute the cumulative percentages (top of each rectangle)
        data$ymax <- cumsum(data$fraction)
        
        # Compute the bottom of each rectangle
        data$ymin <- c(0, head(data$ymax, n=-1))
        
        # Compute label position
        data$labelPosition <- (data$ymax + data$ymin) / 2
        
        # Compute a good label
        data$label <- paste0(data$category, "\n value: ", data$count)
        
        # Make the plot
        ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
          geom_rect() +
          geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
          scale_fill_brewer(palette=5) +
          coord_polar(theta="y") +
          xlim(c(2, 4)) +
          theme_void() +
          theme(legend.position = "none")
       
         
        
      }
      else if(input$var1==2){
        # Create test data.
        data <- data.frame(
          category=c("Uang Masuk", "Uang Keluar"),
          count=c(676008722,160882278)
        )
        
        # Compute percentages
        data$fraction <- data$count / sum(data$count)
        
        # Compute the cumulative percentages (top of each rectangle)
        data$ymax <- cumsum(data$fraction)
        
        # Compute the bottom of each rectangle
        data$ymin <- c(0, head(data$ymax, n=-1))
        
        # Compute label position
        data$labelPosition <- (data$ymax + data$ymin) / 2
        
        # Compute a good label
        data$label <- paste0(data$category, "\n value: ", data$count)
        
        # Make the plot
        ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
          geom_rect() +
          geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
          scale_fill_brewer(palette=5) +
          coord_polar(theta="y") +
          xlim(c(2, 4)) +
          theme_void() +
          theme(legend.position = "none")
      }
      else if(input$var1==3){
        # Create test data.
        data <- data.frame(
          category=c("Uang Masuk", "Uang Keluar"),
          count=c( 631772900,242224300)
        )
        
        # Compute percentages
        data$fraction <- data$count / sum(data$count)
        
        # Compute the cumulative percentages (top of each rectangle)
        data$ymax <- cumsum(data$fraction)
        
        # Compute the bottom of each rectangle
        data$ymin <- c(0, head(data$ymax, n=-1))
        
        # Compute label position
        data$labelPosition <- (data$ymax + data$ymin) / 2
        
        # Compute a good label
        data$label <- paste0(data$category, "\n value: ", data$count)
        
        # Make the plot
        ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
          geom_rect() +
          geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
          scale_fill_brewer(palette=5) +
          coord_polar(theta="y") +
          xlim(c(2, 4)) +
          theme_void() +
          theme(legend.position = "none")
      }
      else if(input$var1==4){
        # Create test data.
        data <- data.frame(
          category=c("Uang Masuk", "Uang Keluar"),
          count=c(679824550,186573250)
        )
        
        # Compute percentages
        data$fraction <- data$count / sum(data$count)
        
        # Compute the cumulative percentages (top of each rectangle)
        data$ymax <- cumsum(data$fraction)
        
        # Compute the bottom of each rectangle
        data$ymin <- c(0, head(data$ymax, n=-1))
        
        # Compute label position
        data$labelPosition <- (data$ymax + data$ymin) / 2
        
        # Compute a good label
        data$label <- paste0(data$category, "\n value: ", data$count)
        
        # Make the plot
        ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
          geom_rect() +
          geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
          scale_fill_brewer(palette=5) +
          coord_polar(theta="y") +
          xlim(c(2, 4)) +
          theme_void() +
          theme(legend.position = "none")
      }
      
    })
    output$table4<-renderTable({
      colm<-as.numeric(input$var1)
      rangkumanakhir[colm,]
    })
    
   
    output$plot1 <- renderPlot({
      Sys.sleep(1)
      
     if(input$var1==1 && input$var3==2){
       grosir2triwulan1<-eclat(grosirtriwulan1, parameter = list(supp=0.005,minlen=2))
       plot(grosir2triwulan1, method = "graph",main = " Plot Asscoiation Rule Kombinasi 2 itemset")
     }
      else if(input$var1==1 && input$var3==3){
        grosir2triwulan1<-eclat(grosirtriwulan1, parameter = list(supp=0.0006,minlen=3))
        plot(grosir2triwulan1, method = "graph",main = " Plot Asscoiation Rule Kombinasi 3 itemset")   
      }
      else if(input$var1==1 && input$var3==4){
     grosir2triwulan1<-eclat(grosirtriwulan1, parameter = list(supp=0.0003,minlen=4))
     plot(grosir2triwulan1, method = "graph",main = " Plot Asscoiation Rule Kombinasi 4 itemset")
     }
     else if(input$var1==2 && input$var3==2){
       grosir2triwulan2<-eclat(grosirtriwulan2, parameter = list(supp=0.005,minlen=2))
       plot(grosir2triwulan2, method = "graph",main = " Plot Asscoiation Rule Kombinasi 2 itemset")
     }
      else if(input$var1==2 && input$var3==3){
        grosir2triwulan2<-eclat(grosirtriwulan2, parameter = list(supp=0.0005,minlen=3))
        plot(grosir2triwulan2, method = "graph",main = " Plot Asscoiation Rule Kombinasi 3 itemset")
      }
      else if(input$var1==2 && input$var3==4){
        grosir2triwulan2<-eclat(grosirtriwulan2, parameter = list(supp=0.0003,minlen=4))
        plot(grosir2triwulan2, method = "graph",main = " Plot Asscoiation Rule Kombinasi 4 itemset")
      }
      
      else if(input$var1==3 && input$var3==2){
        grosir2triwulan3<-eclat(grosirtriwulan3, parameter = list(supp=0.005,minlen=2))
        plot(grosir2triwulan3, method = "graph",main = " Plot Asscoiation Rule Kombinasi 2 itemset")
      }
      else if(input$var1==3 && input$var3==3){
        grosir2triwulan3<-eclat(grosirtriwulan3, parameter = list(supp=0.001,minlen=3))
        plot(grosir2triwulan3, method = "graph",main = " Plot Asscoiation Rule Kombinasi 3 itemset")
      }
      else if(input$var1==3 && input$var3==4){
        grosir2triwulan3<-eclat(grosirtriwulan3, parameter = list(supp=0.0003,minlen=4))
        plot(grosir2triwulan3, method = "graph",main = " Plot Asscoiation Rule Kombinasi 4 itemset")
      }
      
      else if(input$var1==4 && input$var3==2){
        grosir2triwulan4<-eclat(grosirtriwulan4, parameter = list(supp=0.001,minlen=2))
        plot(grosir2triwulan4, method = "graph",main = " Plot Asscoiation Rule Kombinasi 2 itemset")
        
      }
      else if(input$var1==4 && input$var3==3){
        grosir2triwulan4<-eclat(grosirtriwulan4, parameter = list(supp=0.001,minlen=3))
        plot(grosir2triwulan4, method = "graph",main = " Plot Asscoiation Rule Kombinasi 3 itemset")
      }
      else if(input$var1==4 && input$var3==4){
        grosir2triwulan4<-eclat(grosirtriwulan4, parameter = list(supp=0.0003,minlen=4))
        plot(grosir2triwulan4, method = "graph",main = " Plot Asscoiation Rule Kombinasi 4 itemset")
      }
    })
    
   
    output$Kunjungan<-renderText({
      if(!file.exists(("kunjungan.Rdata")))
        Kunjungan<-0
      else
        load(file="kunjungan.Rdata")
      Kunjungan<-Kunjungan+1
      save(Kunjungan,file = "kunjungan.Rdata")
      paste0("Kunjungan: ",Kunjungan)
    })
     
})