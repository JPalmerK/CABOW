## Functions for simulation

library(sp) # point.in.polygon()
library(mgcv) # gam model predictions
#library(boot) # inverse logit
#library(ggplot2)
# library(raster) #atan2
# pDetR <- function(r) {
#   # Probability of detection as a function of range (r)
#   funs <- 1-exp(-(r/14000)^(-30))
# }
# 
# pDetSNR <- function(SNR) {
#   # Probability of detection as a function of (SNR)
#   b=-2; m=1.3;
#   funs <- 1./(1+exp(-(b+m*SNR)))
#          
# }
# 
# 
# createGrid <-function (gridxMin =-40000, gridxMax = 40000,
#                        gridyMin =-40000, gridyMax = 40000,
#                        gridRes =400, buoyX =0, buoyY =0,
#                        pileX = 0, pileY = 10000){
# 
#   xRes = seq(gridxMin, gridxMax, by=gridRes)
#   yRes =seq(gridyMin, gridyMax, by=gridRes)
#   
#   # Create the grid referenced to the buoy
#   buoyXgrid = rep.row(xRes, length(yRes)) -buoyX
#   buoyYgrid = rep.col(yRes, length(xRes)) -buoyY
#   buoyRange = sqrt(buoyXgrid^2+ buoyYgrid^2)
#   buoyBearing =  atan2(buoyYgrid, buoyXgrid)*180/pi
#   
#   ## Same grid referenced to a pile driving instrument at 
# 
#   pileXgrid = rep.row(xRes, length(yRes)) -pileX
#   pileYgrid = rep.col(yRes, length(xRes)) -pileY
#   pileRange = sqrt(pileXgrid^2+ pileYgrid^2)
#   
#   
#   # Dataframe for plotting and simulations
#   df= data.frame(buoyX = as.vector(buoyXgrid),
#                  buoyY = as.vector(buoyYgrid),
#                  buoyR = as.vector(buoyRange),
#                  buoyBearing = as.vector(buoyBearing),
#                  pileX = as.vector(pileXgrid),
#                  pileY = as.vector(pileYgrid),
#                  pileR = as.vector(pileRange))
#   
#   # clean out zeros
#   df=df[df$buoyR>0,]
#   
#   return(df)
#   
# }
# 
# 
# createSNRgrid<- function(df, SL = 165,NL=130,tlCoef=20){
#   # Create an SNR column
#   df$SNR = SL-NL-tlCoef*log10(df$buoyR)
#   return(df)
# }
# 
# createSNRgrid<- function(df, SL = 165,NL=130,tlCoef=20){
#   # Create an SNR column
#   df$SNR = SL-NL-tlCoef*log10(df$buoyR)
#   return(df)
# }
# 
# 
# bearingErrorSTD <- function(SNR, slope=-.3, actSNR=40, meanBearErr=1.25) {
#   # Standard deviation in bearing error (in degrees) as a function of SNR, 
#   # also looks like an activiation function
#   funs <- as.numeric(SNR<actSNR)*(slope*SNR+68)+(SNR>=actSNR)*meanBearErr
# }
# 
# rep.row<-function(x,n){
#   matrix(rep(x,each=n),nrow=n)
# }
# rep.col<-function(x,n){
#   matrix(rep(x,each=n), ncol=n, byrow=TRUE)
# }
# 
circleFun <- function(center = c(0,0),
                      diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
# 
# detectionsBearingErr<-function(df, pDetModel){
#   # Update which calls were detected (binom) and get a bearing error
#   
#   #1 determine which calls are detected
#   #df$detected = rbinom(nrow(df),1,pDetR(df$buoyR)) 
#   if (is.null(pDetModel)){
#     df$detected = rbinom(nrow(df),1,pDetSNR(df$SNR))
#     
#     
#   }else{
#     # use SLNL
#     tempdf = df[c('buoyR', 'SLNL')]
#     colnames(tempdf)<-c('range', 'SLNL')
#     tempdf$pred = predict(pDetModel, tempdf,type = 'response')
#     df$detected = rbinom(nrow(df),1, tempdf$pred)
#     rm(tempdf)
#   } 
#   
#   
#   #2 Assign a bearing error
#   #df$bearingError = bearingErrorSNR(df$SNR)
#   # Bearing error appears to be described by a beta distribution
#   df$bearingError = abs(rnorm(nrow(df), 1, 0.23))
#   df$bearingError = abs(rnorm(nrow(df),.8, 15))
#   
#   
#   # For the detected ones get the bearing error
#   df$newBearing = df$buoyBearing + rnorm(nrow(df),
#                                          mean = df$bearingError,
#                                          sd = .1)*(rbinom(nrow(df),1,.5) * 2 - 1)
#   # 
#   # df$newBearing[df$newBearing>360]= -(df$newBearing[df$newBearing>360])
#   # df$newBearing[df$newBearing<=-360]=-(360 +df$newBearing[df$newBearing<=-360])
#   # 
#    df$newBearing[df$newBearing>180]= -180+(df$newBearing[df$newBearing>180]-180)
#    df$newBearing[df$newBearing<=-180]=180 +(df$newBearing[df$newBearing<=-180]+180)
#   
#   return(df)
#   
#   
#   
# }
# 
# exclusionCalcs <- function(df,
#                            exclusionAngOverride=c(-5000, 5000),
#                            pileX, pileY, buoyX, buoyY){
#   # Get the bearing angles that point inside the exclusion zone and
#   # would trigger a shutdown
#   
#   ## Calculate Metrics for the exclusion zone
#   exclusion = circleFun(center = c(pileX, pileY),
#                         diameter = 20000,
#                         npoints = 100)
#   
#     df$InsideExcllusion  <-
#     point.in.polygon(df$buoyX, df$buoyY,
#                      exclusion$x, exclusion$y)      
#     
#     # If user entered override angles, then use them
#     exclusionAngles = (range(df$buoyBearing[df$InsideExcllusion==1],
#                                     na.rm = TRUE))
#     
#     
#   # get the angles- positive and net
#     theta = atan2(20000, buoyX+pileX)*180/pi
#     
#     P1= c(10000^2/(buoyX+pileX), 
#           (10000/(buoyX+pileX)) * sqrt((buoyX+pileX)^2-10000^2))
#     
#     theta = 90-asin(P1[2]/P1[1])*180/pi
#     exclusionAngles=c(theta, -theta)
# 
#   # Bearing alarm, if a call is detected from within the set bearings, 
#   # Step 1- bearing ok
#     df$BearingAlarm=0;
#     df$BearingAlarm[df$newBearing<exclusionAngles[1] & 
#                       df$newBearing>exclusionAngles[2]]=1
#     
#   
#   # Step 2- Call detected
#   df$AlarmTriggered <- df$BearingAlarm*df$detected
#   
#   
#   # Compare with no bearing information
#   df$TPnobearing <- df$detected*df$InsideExcllusion
#   df$FPnobearing <- df$detected*as.numeric(!(df$InsideExcllusion))
#   df$FNnobearing <- as.numeric(!df$detected)*as.numeric(df$InsideExcllusion)
#   
#   # Correct and Incorrect Actions
#   # Correct Action (Inside Exclusion zone Alarm, Outside Exclusion Zone No alarm)
#   
#   df$CorrectAction = as.numeric((df$AlarmTriggered & df$InsideExcllusion) ||  
#     (!df$AlarmTriggered &!as.numeric(df$InsideExcllusion)))
#   df$MissedAlarm = as.numeric(!df$AlarmTriggered & df$InsideExcllusion)
#   df$FalseAlarm = as.numeric(df$AlarmTriggered & !df$InsideExcllusion)
#   
#   # Proportion of Exclusion zone covered
#   
#   df$plotting = 'unk'
#   df$plotting[df$InsideExcllusion==1 & df$detected==0]= 'Missed' # False Negative
#   df$plotting[df$InsideExcllusion==1 & df$detected==1]= 'Correct' # True Positive (no bearing)
#   df$plotting[df$InsideExcllusion==0 & df$detected==1]= 'False Alarm' # False Positive
#   df$plotting<-as.factor(df$plotting)
#   
#   df$BearingPlot = 'unk'
#   df$BearingPlot[df$InsideExcllusion==1 & df$AlarmTriggered==0]= 'Missed' # False Negative
#   df$BearingPlot[df$InsideExcllusion==1 & df$AlarmTriggered==1]= 'Correct' # True Positive (no bearing)
#   df$BearingPlot[df$InsideExcllusion==0 & df$AlarmTriggered==0 & df$detected==1]= 'Correct' # True Positive (no bearing)
#   df$BearingPlot[df$InsideExcllusion==0 & df$AlarmTriggered==1 & df$BearingAlarm==1]= 'False Alarm' # False Positive
#   df$BearingPlot<-as.factor(df$BearingPlot)
#   df$ndet <- sum(df$detected)
#   
#   calcs = data.frame(modPrec = 
#                        sum(df$BearingPlot=='Correct')/
#                        (sum(df$BearingPlot=='Correct')+sum(df$BearingPlot=='False Alarm')),
#                      modRecall = 
#                        sum(df$BearingPlot=='Correct')/
#                        (sum(df$BearingPlot=='Correct')+sum(df$BearingPlot=='Missed')),
#                      modPrecNoBearing= 
#                        sum(df$plotting=='Correct')/
#                        (sum(df$plotting=='Correct')+ sum(df$plotting=='False Alarm')),
#                      modRecallNoBearing= 
#                        sum(df$plotting=='Correct')/
#                        (sum(df$plotting=='Correct')+ sum(df$plotting=='Missed')),
#                      propCovered = sum(df$detected &df$InsideExcllusion)/
#                        sum(df$InsideExcllusion))
#   
#   calcs$ModF1noBearing = (calcs$modPrecNoBearing*calcs$modRecallNoBearing)/
#     (calcs$modPrecNoBearing+calcs$modRecallNoBearing)
#   calcs$ModF1 = (calcs$modPrec*calcs$modRecall)/
#     (calcs$modPrec+calcs$modRecall)
#   
#                       
# 
#   
#   out =list()
#   out[[1]]<-df
#   out[[2]]<-calcs
#   return(out)
# }


# New function for bearing errors
detectionDF <- function(buoyLocs, buoyAngles, gridSpace, SLNL,
                        pileX=0,pileY=0, excDia,
                        exclusion, slnlModel) {
  
  # Radius of the exclusion zone
  #excR= excDia/2
  d = sqrt((buoyLocs$x-pileX)^2+(buoyLocs$y-pileY)^2)
  
  # This is with respect to the buoy
  theta= acos(excDia/2/d)*180/pi # when you forget https://www.youtube.com/watch?v=Cx37V981gCo
  thetaNeg = acos(-excDia/2/d)*180/pi
  
  
  
  theta= theta%% 360
  thetaNeg = thetaNeg%% 360
  
  
  
  bearingMat =  180+atan2((gridSpace$x-buoyLocs$x),
                          (gridSpace$y-buoyLocs$y))*180/pi
  
  
  # ## Determine which calls were detected based on bearing ##
  # distMat = sqrt((gridSpace$x-buoyLocs$x)^2+
  #                  (gridSpace$y-buoyLocs$y)^2)
  
  Pdets=t(t(predict(slnlModel, 
                    data.frame(range=sqrt((gridSpace$x-buoyLocs$x)^2+
                                            (gridSpace$y-buoyLocs$y)^2), 
                               SLNL = SLNL$NL), 
                    type = 'response' )))
  dfOut=data.frame(Detected =rbinom(nrow(Pdets),1, Pdets))
  
  rm(Pdets)
  
  ## Rotate the bearing matrix based on the orientation with respect to the 
  ## center of the exclusion zone
  bearingMat = bearingMat+(buoyAngles*180/pi)
  
  # add bearing error
  bearingMat= bearingMat+  rnorm(nrow(SLNL),mean = 0,sd =15)
  bearingMat= bearingMat%% 360
  
  # bearing alarm
  dfOut$bearingAlarm=matrix(0,nrow(dfOut),1)
  dfOut$bearingAlarm[bearingMat>= theta & 
                       bearingMat<= thetaNeg] = 1
  dfOut$DetandBearing = dfOut$Detected*dfOut$bearingAlarm
  dfOut$x = gridSpace$x
  dfOut$y = gridSpace$y
  # 
  # # Plot to check
  # ggplot()+geom_point(data=dfOut[dfOut$Detected==1,], 
  #                     aes(x, y, color=bearingAlarm))+
  #   scale_color_viridis_c()+
  #   geom_path(data = exclusion,aes(x,y), color ='red')
  
  return(dfOut)
  
}

