#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#library(colorspace)
#library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

# Pre process the latency trend data
lun_data = readRDS("anonymized.rds")
gp <- filter(lun_data, Value < 10000, complete.cases(lun_data))
if(sum(gp$DiskType == "RawPhysical")){
  gp[gp$DiskType == "RawPhysical",]$Lun = 
    paste(gp[gp$DiskType == "RawPhysical",]$Lun, "Raw")
}
gp <- group_by(gp, Lun, Timestamp)
sm <- summarize(gp, Latency = median(Value, na.rm = T))
gp_lun <- group_by(lun_data, Lun)
sm_lun <- summarise(gp_lun, Latency = median(Value, na.rm = T))
top20 <- head(sm_lun[order(sm_lun$Latency, decreasing = T),], 20)
sm <- filter(sm, Lun %in% top20$Lun)
sm$Lun <- factor(sm$Lun, levels=top20$Lun)
shinyServer(
  function(input, output) {
    gen_plot <- reactive({
      print(as.numeric(input$bins))
      sm <- filter(sm, Timestamp > input$bins[1] & Timestamp < input$bins[2] & grepl(input$filter, Lun))
      print(length(sm$Latency))
      l <- length(sm$Lun)
      if(l < 4000) {
        m = "auto"
        s = 0.3
        f = "y ~ x"
      } else {
        m = "glm"
        f = "y ~ poly(x, 15)"
        s = 0.2
      }
      if(l > 3500) {
        gg <- ggplot(data = sm, aes(x=Timestamp, y = Latency, group=Lun, color=Lun)) +
          geom_smooth(se = F, method="loess", formula = "y ~ x", span=0.3, na.rm = T) +
          #geom_smooth(se = F, method = m, formula = f, span = s, na.rm = T) + 
          ggtitle(paste0("Latency trend per LUN from", min(sm$Timestamp)+hours(8), " to ", 
                         max(sm$Timestamp)+hours(8))) +
          ylab("Mean Latency (ms)") + 
          scale_color_manual(drop=FALSE, values=
                               colorRampPalette(c(rep("red4",1),
                                                  rep("red3",1),
                                                  rep("orange2",4),
                                                  rep("yellow3",3),
                                                  rep("green4",1)))(20),
                             na.value="#EEEEEE", name="Times")
      } else {
        gg <- ggplot(data = sm, aes(x=Timestamp, y = Latency, group=Lun, color=Lun)) +
          geom_line(na.rm = T) + 
          ggtitle(paste0("Latency trend per LUN from ", min(sm$Timestamp)+hours(8), " to ", 
                         max(sm$Timestamp)+hours(8))) +
          ylab("Mean Latency (ms)") +
          scale_color_manual(drop=FALSE, values=
                               colorRampPalette(c(rep("red4",1),
                                                  rep("red3",1),
                                                  rep("orange2",4),
                                                  rep("yellow3",3),
                                                  rep("green4",1)))(20),
                             na.value="#EEEEEE", name="Times")
      }
      pp2 <- ggplotly(gg)
      
      
      for(i in 1:length(pp2$x$data)){
        pp2$x$data[[i]]$text = unlist(lapply(pp2$x$data[[i]]$text, function(x) fixLabels(x)))
        for(x in 1:length(pp2$x$data[[i]]$text)) {
          pp2$x$data[[i]]$text[x] = 
            sub("NA<br .*>",paste0(round(pp2$x$data[[i]]$y[x],2),"<br / >"),
                pp2$x$data[[i]]$text[x])
        }
      }
      pp2
    })
    output$plot <- renderPlotly({
      gen_plot()
    })
    
    # Distribution
    gen_plot2 <- reactive({
      ld <- lun_data[complete.cases(lun_data),]
      ld <- filter(ld, Timestamp > input$bins[1] & 
                     Timestamp < input$bins[2] & 
                     Value < 10000 & 
                     grepl(input$filter, Lun))
      if(sum(ld$DiskType == "RawPhysical")) {
        ld[ld$DiskType == "RawPhysical",]$Lun = 
          paste(ld[ld$DiskType == "RawPhysical",]$Lun, "Raw")
      }
      ld <- mutate(ld, Lun = paste0(Lun, " Cap: ",
                                    round(as.numeric(Lun_Free_GB,1)), 
                                    "/", round(as.numeric(Lun_Cap_GB,1)),
                                    " GB"))
      grp <- group_by(ld, Lun, Server, Disk, CapacityGB)
      smr <- summarize(grp, Latency = mean(Value, na.rm = T))
      tbl <- mutate(as.data.frame(smr), 
                    Server = paste0(Server,"\nDisk: ",Disk, " Cap:",
                                  round(as.numeric(CapacityGB),1)," GB"))
      tbl <- tbl[order(tbl$Latency, decreasing = T),]
      
      lvls <- sapply(tbl$Latency, function(x) runif(1, min = 0.000001, max = 0.001)+x)
      tbl$Server <- factor(lvls, levels=lvls, labels = tbl$Server)
      tbl$Latency <- round(tbl$Latency, 2)
      p1 <- ggplot(data=tbl, aes(x=Lun, y=Latency, fill=Server)) +
        geom_col(show.legend = F, colour="white") + 
        ggtitle(paste("Mean Latency per LUN from ",
                      min(lun_data$Timestamp)+hours(8), " to ",
                      max(lun_data$Timestamp)+hours(8))) +
        ylab("Mean Latency (ms)") + 
        coord_flip() + 
        scale_fill_manual(drop=FALSE, values=
                            colorRampPalette(c(rep("purple4",1),
                                               rep("red3",5),
                                               rep("orange2",6),
                                               rep("yellow2",10),
                                               rep("green4",80)))(length(tbl$Server)+1),
                          na.value="#EEEEEE", name="Times") +
        scale_x_discrete() 
      p2 <- ggplotly(p1)
      # Manually hide legends from the plotly object:
      for(i in 1:length(p2$x$data)) {
        p2$x$data[[i]]$showlegend = F
      }
      p2
    })
    output$plot2 <- renderPlotly({
      gen_plot2()
    })
  }
)
