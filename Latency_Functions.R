# Translates from scsi ID to disk number
require(dplyr)
require(lubridate)
require(ggplot2)
require(plotly)

scsi2int <- function(id) {
  id <- sub("scsi","",id)
  a <- as.numeric(sub(":[0-9]*","",id))
  b <- as.numeric(sub("[0-9]:","",id))
  return(a*100+b)
}

# Filename to LUN
file2lun <- function(file) {
  file <- sub("\\[","",file)
  lun <- sub("\\].*","",file)
  return(lun)
}

# Converts the epoch timestamp to readable
fixLabels <- function(s) {
  ts <- sub("<br","",unlist(strsplit(s," "))[2])
  
  tx <- as.POSIXct(as.numeric(ts)-3600*8, 
                   origin = "1970-01-01", tz = "Asia/Shanghai")
  r <- paste0("<br ",paste0(unlist(strsplit(s," "))[10:13], collapse = " "))
  tm <- sub(ts,tx,s)
  sub(r,"",tm)
}



fixLabels <- function(s) {
  for(n in 1:length(s)) {
    t <- sub("  |   "," ",s[n])
    ts <- sub("<br","",unlist(strsplit(s[n]," "))[2])
    tx <- as.POSIXct(as.numeric(ts)-3600*8, 
                     origin = "1970-01-01", tz = "Asia/Shanghai")
    r <- paste0("<br />Disk: ",paste0(unlist(strsplit(t," "))[10:12], collapse = " "))
    tm <- sub(ts,tx,s[n])
    s[n] <- sub(r,"",tm)
  }
  return(s)
}




##############################
# Read Data
read.stats <- function(stt, dsk, dts, pth) {
  lun_data <- data.frame(read.csv(paste0(pth, stt), stringsAsFactors = F))
  disk_data <- data.frame(read.csv(paste0(pth, dsk), stringsAsFactors = F))
  dt_data <- data.frame(read.csv(paste0(pth, dts), stringsAsFactors = F))
  
  # Remove unnecessary variables
  lun_data <- select(lun_data, Value, Timestamp, MetricId, Entity, Instance)
  lun_data$Timestamp <- mdy_hms(lun_data$Timestamp)
  dt_data <- select(dt_data, Name, CapacityGB, FreeSpaceGB)
  
  # Translate from filename to LUN:
  disk_data <- mutate(disk_data, Lun = file2lun(Filename))
  disk_data <- select(disk_data, StorageFormat, DiskType, 
                      CapacityGB, Name, Lun, Parent)
  
  # List of unique disks
  disks <- select(lun_data, Entity, Instance)
  disks <- unique(disks)
  disks <- mutate(disks, Disk_num = scsi2int(disks$Instance))
  
  # Gets the LUN where the disk belongs to
  id2disk <- function(vm, dnum) {
    dsks <- disk_data[disk_data$Parent == vm,]$Name
    ids <- sort(disks[disks$Entity == vm,]$Disk_num)
    return(as.character(dsks[which(ids == dnum)]))
  }
  
  # Add disk names:
  disks <- mutate(disks, Name = mapply(id2disk, Entity, Disk_num))
  
  # Join disk_data and disks 
  disk_data <- full_join(disks,disk_data, by = c("Entity"="Parent","Name"="Name"))
  
  # Join disk_data with dt_data
  names(dt_data) <- c("Name", "Lun_Cap_GB", "Lun_Free_GB")
  disk_data <- left_join(disk_data,dt_data, by = c("Lun"="Name"))
  
  # Add disk data to lun_data
  lun_data <- left_join(lun_data,disk_data, by = c("Entity"="Entity","Instance"="Instance"))
  names(lun_data)[4] = "Server"
  names(lun_data)[7] = "Disk"
  return(lun_data)
}

###################

dist_graph <- function(lun_data, path, file) {
  lun_data <- lun_data[complete.cases(lun_data),]
  lun_data <- filter(lun_data, Value < 20000)
  if(sum(lun_data$DiskType == "RawPhysical")) {
    lun_data[lun_data$DiskType == "RawPhysical",]$Lun = 
      paste(lun_data[lun_data$DiskType == "RawPhysical",]$Lun, "Raw")
  }
  lun_data <- mutate(lun_data, Lun = paste0(Lun, "\nCapacity: ", 
                                            round(as.numeric(Lun_Free_GB,1)), 
                                            "/", round(as.numeric(Lun_Cap_GB,1)), 
                                            " GB"))
  grp <- group_by(lun_data, Lun, Server, Disk, CapacityGB)
  smr <- summarize(grp, Latency = mean(Value))
  tbl <- mutate(as.data.frame(smr), Server = paste0(Server,"\nDisk: ",Disk, " Cap:", 
                                                  round(as.numeric(CapacityGB),1)," GB"))
  
  
  tbl <- tbl[order(tbl$Latency, decreasing = T),]
  lvls <- sapply(tbl$Latency, function(x) runif(1, min = 0.00001, max = 0.0001)+x)
  tbl$Server <- factor(lvls, levels=lvls, labels = tbl$Server)
  tbl$Latency <- round(tbl$Latency, 2)
  p1 <- ggplot(data=tbl, aes(x=Lun, y=Latency, fill=Server)) +
    geom_col(show.legend = F, colour="white") + ggtitle(paste("Mean Latency per LUN\n", 
                                              min(lun_data$Timestamp)+hours(8), " to ",
                                              max(lun_data$Timestamp)+hours(8))) +
    ylab("Mean Latency (ms)") + 
    coord_flip() + 
    scale_fill_manual(drop=FALSE, values=
                        colorRampPalette(c(rep("purple4",1),
                                           rep("red3",6),
                                           rep("orange2",6),
                                           rep("yellow2",7),
                                           rep("green4",40)))(length(tbl$Server)),
                      na.value="#EEEEEE", name="Times") +
    scale_x_discrete() 
  
  pp1 <- ggplotly(p1)
  # Manually hide legends from the plotly object:
  for(i in 1:length(pp1$x$data)) {
    pp1$x$data[[i]]$showlegend = F
  }
  htmlwidgets::saveWidget(as_widget(pp1), paste0(path,file), selfcontained = T)
}

##############################################################

trend_graph <- function(lun_data, path, file) {
  gp <- mutate(lun_data, Disk=paste0(Server,"-",Disk)) %>% group_by(Disk, Timestamp)
  gp <- filter(gp, Value < 5000)
  sm <- summarize(gp, Latency = mean(Value))
  gp_disk <- group_by(sm, Disk)
  sm_disk <- summarise(gp_disk, Latency = mean(Latency))
  top20 <- head(sm_disk[order(sm_disk$Latency, decreasing = T),], 20)
  sm <- filter(sm, Disk %in% top20$Disk)
  sm <- sm[order(sm$Latency, decreasing = T),]
  sm$Disk <- factor(sm$Disk, levels=top20$Disk)
  gg <- ggplot(data = sm[,], aes(x=Timestamp, y = Latency, group=Disk, color=Disk)) +
    geom_smooth(se = F, method = "loess", formula = "y ~ x", span = 0.2) + #geom_line() +
    ggtitle(paste0("Latency trend per LUN from", min(sm$Timestamp)+hours(8), " to ", 
                   max(sm$Timestamp)+hours(8))) +
    ylab("Mean Latency (ms)") + scale_color_manual(drop=FALSE, values=
                                                     colorRampPalette(c(rep("red4",1),
                                                                        rep("red3",1),
                                                                        rep("orange2",4),
                                                                        rep("yellow3",3),
                                                                        rep("green4",1)))(length(unique(sm$Disk))),
                                                   na.value="#EEEEEE", name="Times")
  pp2 <- ggplotly(gg)
  for(i in 1:length(pp2$x$data)){
    pp2$x$data[[i]]$text = unlist(lapply(pp2$x$data[[i]]$text, function(x) fixLabels(x)))
  }
  htmlwidgets::saveWidget(as_widget(pp2), paste0(path, file), selfcontained = T)
}

##############################################################

lun_trend_graph <- function(lun_data, path, file) {
  lun_data <- filter(lun_data, !DiskType == "RawPhysical")
  gp <- group_by(lun_data, Lun, Timestamp)
  gp <- filter(gp, Value < 5000)
  sm <- summarize(gp, Latency = mean(Value))
  gp_lun <- group_by(sm, Lun)
  sm_lun <- summarise(gp_lun, Latency = mean(Latency))
  top20 <- head(sm_lun[order(sm_lun$Latency, decreasing = T),], 20)
  sm <- filter(sm, Lun %in% top20$Lun)
  sm$Lun <- factor(sm$Lun, levels=top20$Lun)
  gg <- ggplot(data = sm, aes(x=Timestamp, y = Latency, group=Lun, color=Lun)) + 
    geom_smooth(se = F, method = "loess", formula = "y ~ x", span=0.2) + 
    ggtitle(paste0("Latency trend per Lun\n", 
                   min(lun_data$Timestamp)," to ",
                   max(lun_data$Timestamp))) + 
    ylab("Mean Latency (ms)") + scale_color_manual(drop=FALSE, values=
                                                     colorRampPalette(c(rep("red4",1),
                                                                        rep("red3",1),
                                                                        rep("orange2",4),
                                                                        rep("yellow3",3),
                                                                        rep("green4",1)))(length(unique(sm$Lun))),
                                                   na.value="#EEEEEE", name="Times")
  pp2 <- ggplotly(gg)
  for(i in 1:length(pp2$x$data)){
    pp2$x$data[[i]]$text = unlist(lapply(pp2$x$data[[i]]$text, function(x) fixLabels(x)))
    for(x in 1:length(pp2$x$data[[i]]$text)) {
      pp2$x$data[[i]]$text[x] = 
        sub("Latency:.*Lun:",paste0("Latency: ",round(pp2$x$data[[i]]$y[x],2),"<br / >Lun: "),
            pp2$x$data[[i]]$text[x])
    }
  }
  htmlwidgets::saveWidget(as_widget(pp2), paste0(path, file), selfcontained = T)
}

