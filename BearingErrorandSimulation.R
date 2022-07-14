rm(list=ls())
library(ggplot2)
library(MASS)
library(scales)
library(mgcv)
library(mgcViz)
library(caret)
library(boot) # inv.logit
library(viridis)
require(magick)
require(rgl)
require(gifski)
require(rlang)

library(ragg)

transition_values <- function(from, to, steps = 10, 
                              one_way = FALSE, type = "cos") {
  if (!(type %in% c("cos", "lin")))
    stop("type must be one of: 'cos', 'lin'")
  
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range)/2
  
  # define scaling vector starting at 1 (between 1 to -1)
  if (type == "cos") {
    scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))
  } else if (type == "lin") {
    if (one_way) {
      xout <- seq(1, -1, length.out = steps)
    } else {
      xout <- c(seq(1, -1, length.out = floor(steps/2)), 
                seq(-1, 1, length.out = ceiling(steps/2)))
    }
    scaling <- approx(x = c(-1, 1), y = c(-1, 1), xout = xout)$y 
  }
  
  middle - half_width * scaling
}

# Load the functions
source("C:\\BitBucketRepositories\\CABOWProcessing\\R\\simFunctions.R")

# Load the actual data
SLtable = read.csv(
  'C:\\BitBucketRepositories\\CABOWProcessing\\ProcessedCSVfiles\\MDTrials.csv')

# Organize north to south
SLtable$CABOWid=as.factor(SLtable$CABOWid)
SLtable$CABOWid <- factor(SLtable$CABOWid,
                          levels = c("324", "325", "321", "327","319"))

# Model the probability of detection vs signal excess
pDetModel <- gam(data = SLtable,
                 detected~te(range, SLNL,k = 5),
                 family = binomial)

pDetModel.glm <- glm(data = SLtable,
                 detected~range+SLNL+Truebearing,
                 family = binomial())
# save the model
saveRDS(pDetModel, "C:\\BitBucketRepositories\\CABOWProcessing\\R\\SLNLgam.rds")


# gam prediction plot
b <- getViz(pDetModel)

gamPlot<-plot(sm(b, 1), trans = inv.logit) +
  l_fitRaster() + l_fitContour() + 
  ggtitle('')+
  xlab('Range (m)')+ 
  ylab(expression("SLNR (dB"["rms"]*" )"))+
  scale_fill_viridis(name = expression("P"["det"]*""),
                     begin = .1)



# Create the figure file
jpeg(paste(figFilesloc, "GamModel.png", sep=""),
     width = 12, 
     height = 10, 
     units = "cm", res = 300)
print(gamPlot)
dev.off()



####################################################
## Create the plot for signal excess##
###################################################
figFilesloc ='C:\\BitBucketRepositories\\CABOWProcessing\\R\\Publication Figures'
DistBreak = seq(0, 10000, by=500) 
SLNLbreaks = seq(20, 75,by = 5)

## Bin the results
SLtable$SLNLbin = cut(as.numeric(SLtable$SLNL), 
                           breaks=SLNLbreaks, 
                           include.lowest=TRUE, 
                           right=FALSE)
SLtable$DistBin = cut(SLtable$range, 
                           breaks=DistBreak, 
                           include.lowest=TRUE, 
                           right=FALSE)

SLNRden = aggregate(data = SLtable, 
                    detected~SLNLbin+DistBin, FUN =mean)
SLNRden$Nobs = aggregate(data = SLtable, 
                         detected~SLNLbin+DistBin, FUN =length)[3]
SLNRden$Dist =DistBreak[as.numeric(SLNRden$DistBin)]
SLNRden$SLNLBinNumeric =SLNLbreaks[as.numeric(SLNRden$SLNLbin)]

SLNRden =SLNRden[SLNRden$Nobs>5,]
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
# Transmission loss
df = data.frame(x = seq(0,max(DistBreak)), 
                y = 16.1*log10(seq(0,max(DistBreak))))
My_Theme = theme(
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 12),
  legend.title=element_text(size=10))

SEplot<- 
  ggplot() +
  geom_tile(data = SLNRden, mapping = aes(x = Dist,
                                          y = SLNLBinNumeric,
                                          fill = detected))+
  scale_fill_gradientn(name  ="g(SLNR|r)", colours =  jet.colors(7))+
  geom_text(data = SLNRden, aes(x = Dist,
                                y = SLNLBinNumeric,
                                label=round(detected,2))) +
  theme_bw()+
  xlab('Range (m)') + 
  ylab(expression("SLNL (dB"["rms"]*" )"))+
  geom_line(data = df, aes(x = x,  y = y), size=1)+
  ylim(c(20,75))+
  My_Theme

ragg::agg_jpeg(paste(figFilesloc,"\\Figure 4 SLNL.jpg", sep=""), 
              width = 35, 
              height = 16, 
              units = "cm", res = 300)
SEplot
dev.off()

# Create the figure file
png(paste(figFilesloc, "/Fig 4 sigExcess.png", sep=""),
     width = 12, 
     height = 10, 
     units = "cm", res = 300)
print(SEplot)
dev.off()





#########################################
# 
# library(rayshader)
# library(av)
# 
# # make a 3d map
# 
# c= plot_gg(SEplot, width = 4, height = 4,
#         scale = 300, multicore = TRUE)
# 
# 
# #generate mp4
# n_frames <- 1080 
# #video transition variables
# theta <- transition_values(from = -15, 
#                            to = 180, 
#                            steps = n_frames, 
#                            one_way = FALSE, type = "lin") 
# phi <- transition_values(from = 40,
#                          to = 15, 
#                          steps = n_frames,
#                          one_way = FALSE, type = "cos")
# zoom <- transition_values(from = 1,
#                           to = 0.8, 
#                           steps = n_frames, 
#                           one_way = FALSE, type = "cos") 
# 
# 
# library(av)
# 
# 
# 
# 
# for(i in 1:1080) {
#  
#   render_camera(theta=theta[i], phi=phi[i], zoom=zoom[i])
#   
#   render_snapshot(filename = sprintf("welli%i.png", i), 
#                   title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
# }
# rgl::rgl.close()
# 
# 
# av::av_encode_video(sprintf("welli%d.png",seq(1,1080,by=1)), framerate = 30,
#                     output = "wellington.mp4")
# 



#######################3


####################################################
## Create the plot for SNR##
###################################################
SLtable$SNR =SLtable$RecRMSsignal-SLtable$RecRMSnoise 
DistBreak = seq(0, 10000, by=500) 
SNRbreaks = seq(floor(min(SLtable$SNR, na.rm = TRUE)), 
                ceiling(max(SLtable$SNR, na.rm = TRUE)),by = 1)


## Bin the results
SLtable$SNRbin = cut(as.numeric(SLtable$SNR), 
                      breaks=SNRbreaks, 
                      include.lowest=TRUE, 
                      right=FALSE)
SNRden = aggregate(data = SLtable, 
                    detected~SNRbin, FUN =mean)
SNRden$Nobs = aggregate(data = SLtable, 
                         detected~SNRbin, FUN =length)[3]
SNRden$SNRBinNumeric =SNRbreaks[as.numeric(SNRden$SNRbin)]

## Revised SNR
# Load the actual data
SLtableRev = read.csv(
  'C:\\BitBucketRepositories\\CABOWProcessing\\ProcessedCSVfiles\\MDtrialsRevision321.csv')
SLtableRev$SNR =SLtableRev$RecRMSsignal-SLtableRev$RecRMSnoise 
SNRbreaksrev = seq(floor(min(SLtableRev$SNR, na.rm = TRUE)), 
                ceiling(max(SLtableRev$SNR, na.rm = TRUE)),
                by = 1)
SLtableRev$SNRbin = cut(as.numeric(SLtableRev$SNR), 
                     breaks=SNRbreaks, 
                     include.lowest=TRUE, 
                     right=FALSE)
SNRdenrev = aggregate(data = SLtableRev, 
                   detected~SNRbin, FUN =mean)
SNRdenrev$SNRBinNumeric =SNRbreaks[as.numeric(SNRdenrev$SNRbin)]

SNRdenrev$run ='Revised'
SNRden$run ='Origional'

SNRplotdata = rbind(SNRden, SNRdenrev)



  SNRplot<-
    ggplot() +
  geom_point(data = SNRplotdata, 
             aes(x = SNRBinNumeric,
                 y = detected,color=run, 
                 shape=run), size = 2)+
    theme_bw()+
    scale_color_grey(start = 0,
                     end = .6,)+
    
    xlab(
      expression(
        "SNR (dB"[rms]^{}*"50-225 Hz)"))+
    ylab('Proportion Detected')+
    My_Theme+
    theme(legend.title=element_blank())
    
  # ensure consistant sizeing
  ragg::agg_jpeg(paste(figFilesloc,"\\ Figure 5 SNR.jpg",sep=""), 
                width = 25, 
                height = 16, 
                units = "cm", res = 300)
  SNRplot
  dev.off()
  
  

  
  # # Create the figure file
  # png(paste(figFilesloc, "SNR.png", sep=""),
  #     width = 25, 
  #     height = 15, 
  #     units = "cm", res = 300)
  # print(SNRplot)
  # dev.off()
  # 

  ####################################################
  ## Create the plot for SNR##
  ###################################################
  SLtable$SNR =SLtable$RecRMSsignal-SLtable$RecRMSnoise 
  DistBreak = seq(0, 10000, by=500) 
  SNRbreaks = seq(floor(min(SLtable$SNR, na.rm = TRUE)), 
                  ceiling(max(SLtable$SNR, na.rm = TRUE)),by = 1)
  
  
  ## Bin the results
  SLtable$SNRbin = cut(as.numeric(SLtable$SNR), 
                       breaks=SNRbreaks, 
                       include.lowest=TRUE, 
                       right=FALSE)
  
  SNRden = aggregate(data = SLtable, 
                     detected~SNRbin+DistBin, FUN =mean)
  SNRden$Nobs = aggregate(data = SLtable, 
                          detected~SNRbin+DistBin, FUN =length)[3]
  SNRden$Dist =DistBreak[as.numeric(SNRden$DistBin)]
  SNRden$SNRBinNumeric =SNRbreaks[as.numeric(SNRden$SNRbin)]
  
    SNRden =SNRden[SNRden$Nobs>5,]
  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  # Transmission loss
  df = data.frame(x = seq(0,max(DistBreak)), 
                  y = 16*log10(seq(0,max(DistBreak))))
  
  SNRplot<- 
    ggplot() +
    geom_tile(data = SNRden, mapping = aes(y = Dist,
                                            x = SNRBinNumeric,
                                            fill = detected))+
    scale_fill_gradientn(name  ="g(SNR,r)", colours =  jet.colors(7))+
    #scale_fill_viridis_c(option="plasma")+
    theme_bw()+
    xlab('Range (m)') + 
    #theme(text=element_text(family="serif"))+
    ylab(expression("SLNR (dB"["rms"]*" )"))
  #geom_line(data = df, aes(x = x,  y = y), size=2)+
  #ylim(c(20,75))
  
  
  

  ####################################################
  ##  Display Noise levels ##
  ###################################################  
  
  # this requires more investigation. Likely relationship between 
  # Noise level at the boat and range
  
  
  ggplot(SLtable,aes(x=as.factor(CABOWid), y=RecRMSnoise))+
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
    theme_bw()+
    labs(y = 
           expression("Ambient Noise Level (dB"[rms]^{}*"50-225Hz)"),
         x= 'CABOW ID')
  ## Noise level plot 
  library(ggridges)
  
  NLPlot<- ggplot(SLtable,aes(y=as.factor(CABOWid), x=RecRMSnoise))+
    stat_density_ridges(
      quantile_lines = TRUE, 
      quantiles = c(0.25,.5, 0.75), alpha = 0.7)+
    theme_bw()+
    labs(x = 
           expression("Ambient Noise Level (dB"[rms]^{}*" 50-225Hz)"),
         y= 'CABOW ID')+
    My_Theme
  
  ragg::agg_jpeg(paste(figFilesloc,"\\ Figure 3 NL.jpg",sep=""), 
                width = 25, 
                height = 16, 
                units = "cm", res = 300)
  NLPlot
  dev.off()
  # 
  # # Create the figure file
  # png(paste(figFilesloc, "Figure 3 NL.png", sep=""),
  #     width = 25, 
  #     height = 15, 
  #     units = "cm", res = 300)
  # print(NLPlot)
  # dev.off()
  # 

  
  
  
  
  ggplot(SLtableRev$SLNL)+
    geom_point(aes(x=log10(range), y=RecRMSnoise,
                   col = as.factor(CABOWid)))+
    geom_smooth(method = "lm", alpha = .15, 
                aes(x=log10(range), y=RecRMSnoise,
                    color = as.factor(CABOWid)))+
    scale_color_brewer(palette ='Accent')
  
  #### Get the detection Performance#################
  
 
  ####################################################
  ##  Display Bearing Error ##
  ###################################################  
   
  ggplot(SLtable,aes(x=CABOWid, y=BearingError))+
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
    theme_bw()+
    ylim(c(-10,10))
  
  xlab <- "Bearing Error (°Degrees)"
  
  bearingPlot<- 
    ggplot(SLtable,
           aes(y=CABOWid, x=BearingError, color = SNR))+
    
  
    stat_density_ridges(
      quantile_lines = TRUE, 
      quantiles = c(0.25,.5, 0.75), alpha = 0.8)+
    xlim(-10,10)+
    theme_bw()+
    labs(x=xlab, y= 'CABOW ID')+
    My_Theme
    
  
  # # Create the figure file
  # png(paste(figFilesloc, "BearingError.png", sep=""),
  #     width = 18, 
  #     height = 16, 
  #     units = "cm", res = 300)
  # print(bearingPlot)
  # dev.off()
  # 

  # ensure consistant text size
  ragg::agg_jpeg(paste(figFilesloc,"\\ Figure 6 BearingError.jpg",sep=""),
                width = 18, 
                height = 16, 
                units = "cm", res = 300)
  bearingPlot
  dev.off()
  
  ggplot(SLtable,aes(x=log10(range), y=BearingError))+
    facet_grid(~CABOWid)+
    geom_point()+
    theme_bw()+
    ylim(c(-6,14))
  
  
  xlab <- "Bearing Error(°)"
  fullHist<-ggplot(SLtable,aes(BearingError))+
  geom_histogram(aes(y = ..density..),
                 breaks=c(seq(-180,-20,40),
                          seq(-19,19,1), 
                          seq(20,180,40)) )+
    geom_line(aes(y = ..density..), 
              colour=26, stat = 'density',
              size = .1, alpha = .6)+
    xlab('Bearing Error (Degrees)')+
    ylab('Histogram Density')+
    theme_bw()
  
  # Create the figure file
  jpeg(paste(figFilesloc, "FullHistogram.jpg", sep=""),
       width = 20, 
       height = 10, 
       units = "cm", res = 200)
  print(fullHist)
  dev.off()
  
  # Create the figure file
  jpeg(paste(figFilesloc, "croppedHistogram.jpg", sep=""),
       width = 20, 
       height = 10, 
       units = "cm", res = 200)
  print(fullHist+xlim(c(-20,20)))
  dev.off()

#### Run a simulation moving the sensor#################


p2=list()

kmFROMExclusion = seq(0,4,.3)
Pilexlocs = 10000+1000*(kmFROMExclusion)
pileY = 0
buoyX =0; buoyY =0


outputMetrics=data.frame()
for (ii in 1:length(kmFROMExclusion)){
  
  pileX=Pilexlocs[ii]
  exclusion = circleFun(center = c(pileX, pileY),
                        diameter = 20000,
                        npoints = 100)

  ## Set up the simulation
  df =createGrid( gridxMin =-20000, gridxMax = 80000,
                    gridyMin =-80000, gridyMax = 80000,
                    pileX = pileX, pileY = 0, 
                  buoyX = buoyX,
                  buoyY = buoyY, gridRes =200)
  
  ## Add an SNR column
  df <- createSNRgrid(df,SL = 145,NL=98,tlCoef=16)
  df$SLNL <-  median(SLtable$SLdB)-median(SLtable$RecRMSnoise)
  
  ## Update detections and bearing error
  df <- detectionsBearingErr(df, pDetModel)

  out <-exclusionCalcs(df,
                       pileY = pileY, pileX = pileX,
                       buoyX = buoyX, buoyY = buoyY)
  
  df=out[[1]]
  calcs=out[[2]]
  
  df$plotting <- factor(df$plotting, 
                          levels = c( "Missed" , "False Alarm", "Correct","unk"))
  df$BearingPlot <- factor(df$BearingPlot, 
                           levels = c( "Missed" , "False Alarm", "Correct","unk"))
  
  
  cbp1 <- c("#D55E00",  "#F4EDCA", "#52854C")
  custom.col <- c("#D16103","#FFDB6D", "#52854C", "#C4961A", "#F4EDCA", 
                   "#C3D7A4", "#4E84C4", "#293352")
  
  p2[[ii]]<- ggplot(data=df[df$BearingPlot != 'unk',])+
    geom_point(aes(buoyX,buoyY, color =BearingPlot))+
    #scale_color_manual(values  = custom.col )+
    scale_color_viridis_d()+
    geom_path(data = exclusion,aes(x,y))+
    guides(color=guide_legend(title="Action"))+
    theme_bw()+ xlab('meters')+ylab('meters')
    
  ggplot(data=df[df$plotting != 'unk',])+
    geom_point(aes(buoyX,buoyY, color =plotting))+
    #scale_color_manual(values  = custom.col )+
    scale_color_viridis_d()+
    geom_path(data = exclusion,aes(x,y))+
    guides(color=guide_legend(title="Action"))+
    theme_bw() +xlab('meters')+ylab('meters')

  outputMetrics=rbind(outputMetrics, out[[2]])
  
}


outputMetrics$SepDist= kmFROMExclusion
# Combine data for the f1 plot
temp = outputMetrics[, c('ModF1','SepDist', 'modPrec', 'modRecall','propCovered')]
temp$Bearing = 'Bearing'

temp1 = outputMetrics[, c('ModF1noBearing','SepDist', 'modPrecNoBearing',
                          'modRecallNoBearing','propCovered')]
temp1$Bearing = 'No Bearing'
colnames(temp1)<-colnames(temp)


temp = rbind(temp, temp1 )
temp$Bearing<-as.factor(temp$Bearing)

ggplot(temp)+
  geom_point(aes(SepDist, ModF1, color =Bearing))+
  geom_line(aes(SepDist, ModF1, color =Bearing))+
  scale_color_viridis_d()+ theme_bw()+
  ylab('Modified F1 Score')+
  xlab('Distance to Exclusion Zone Radius (km)') 
                                                                labels = trans_format("log10", math_format(10^.x))) 


ggplot(temp)+
  geom_point(aes(SepDist, ModF1, color =Bearing))+
  geom_line(aes(SepDist, ModF1, color =Bearing))+
  scale_color_viridis_d()+ theme_bw()+
  ylab('Modified F1 Score')+
  xlab('Distance to Exclusion Zone Radius (km)')+
  


ggplot(temp)+
  geom_point(aes(SepDist, falseAlarmRate, color =Bearing))+
  xlab('Distance to Exclusion Zone Radius (km)')




# Estimate SNR as a function of range

# p <- ggplot(df, aes(x = buoyX, y = buoyY))+ 
#   geom_tile(aes(fill = SNR))+
#   scale_fill_distiller(palette = "YlGnBu")+
#   xlab('meters')+ylab('meters')

# 
# # Add a circle for the pile driving apparatus
# 
# p + geom_path(data = exclusion,aes(x,y))
# 
# 
# 
# 
# 
# 
# p1 <- ggplot(df, aes(x = buoyX, y = buoyY))+ 
#   geom_tile(aes(fill = bearingError))+
#   scale_fill_distiller(palette = "YlGnBu")+
#   xlab('meters')+ylab('meters')
# 
# 
# # Add a circle for the pile driving apparatus
# p1 + geom_path(data = exclusion,aes(x,y))
# 
# 
# 
# 



  

