library(shiny)
library(httr) 

library('RColorBrewer')
Color = brewer.pal(11,'RdBu')


Weight= {}

Weight = read.table('weightlog.txt', sep=',', header=T)
accessToken <- "Bearer eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE0ODk3OTE4NjUsInNjb3BlcyI6Indwcm8gd2xvYyB3bnV0IHdzbGUgd3NldCB3aHIgd3dlaSB3YWN0IHdzb2MiLCJzdWIiOiIyWktONlIiLCJhdWQiOiIyMjlHOFoiLCJpc3MiOiJGaXRiaXQiLCJ0eXAiOiJhY2Nlc3NfdG9rZW4iLCJpYXQiOjE0NTgyNTU4NjV9.tN1x_hjOyqRYAPoJQ-dabCsiPu9VjNWQsbO5gOCd-8o"
getString <- paste("https://api.fitbit.com/1/user/2ZKN6R/body/log/weight/date/",Sys.Date(),"/1m.json",sep="")
request <- GET(getString, add_headers("Authorization"= accessToken))
Data = content(request)$weight

Data[lapply(Data,function(x){length(x)})==6] = lapply(Data[lapply(Data,function(x){length(x)})==6], function(x){c(x[1:2],NA,x[3:6])})
NewWeight = matrix(unlist(Data),ncol=7, byrow=T, dimnames=list(rep(NULL,length(Data)),names(Data[[1]])))
NewWeight[,7] = as.numeric(NewWeight[,7])*2.20462

Weight = rbind(Weight, NewWeight)

 

Weight$date = as.POSIXct(paste(Weight$date,Weight$time),format="%Y-%m-%d %H:%M:%S")
Weight$weight = as.numeric(Weight$weight)
Weight = Weight[!duplicated(Weight),]
write.table(Weight,'weightlog.txt', row.names=F, col.names=T, sep=',')

function(input, output) {

    output$plot = renderPlot({
      
      
      a <- Weight$date >= as.POSIXct(input$Dates[1]) & Weight$date <= as.POSIXct(input$Dates[2])

     
      x = Weight$date[a]
      y = Weight$weight[a]
      
      AM = format(Weight$date[a],"%H") < 12
      
      
      ma = function(x){
        mean(Weight[Weight$date > ( x-2.5*24*60*60) & Weight$date < (x+2.5*24*60*60),]$weight)
      }
      
        plot(x,y, pch=20, ylab='lbs', xlab='', main=format(Sys.Date(),"%b %d, %Y"), col=NULL, xaxt="n")
        grid(nx=0, ny=NULL)
        abline(h=seq(1,200,by=1), col='lightgray',lty=3)
        axis.POSIXct(1, at=seq(round(range(x),"days")[1],round(range(x),"days")[2], by="days"), labels=NA, tcl=-0.25)
        axis.POSIXct(1, at=seq(round(range(x),"days")[1],round(range(x),"days")[2], by="7 days"), format='%b %d')

        abline(v=seq(round(range(x),"days")[1],round(range(x),"days")[2], by="7 days"), col='lightgray', lty=3)
        
        box()
        
        points(x[!AM],y[!AM], pch=20, col=Color[9])
        points(x[AM],y[AM], pch=20, col=Color[4])
        
        if(input$Poly){
          fit=lm(y~poly(x,4))
          lines(sort(x), fitted(fit)[order(x)], col=Color[11], lty=2, lwd=2) 
        }
        
        if(input$MovingAverage){
          withProgress(message="Rendering Data", value=0,{
            lines(x, lapply(Weight[a,2],ma), col=Color[2], lwd=1.5, lty=2,type='o', pch=19, cex=0.5)
          })
        }
        
      
        # legend(x=0.01*(par("usr")[2]-par("usr")[1])+par("usr")[2],y=0.5*(par("usr")[4]-par("usr")[3])+par("usr")[3], c('AM weight','PM weight','polynomial fit','moving average'), inset=c(-1,0),col=c(Color[c(5,8,11,2)]), lty=c(NA,NA,2,2), pch=c(20,20,NA,19), lwd=c(NA,NA,2,1), bg='white', cex=0.8, yjust=0.5)
        if(input$Legend){
          b = c((length(x)*2/3) : length(x))
          if (lm(y[b]~x[b])$coef[2]>0){
            legend("bottomright", c('AM weight','PM weight','polynomial fit','moving average'), col=c(Color[c(5,8,11,2)]), lty=c(NA,NA,2,2), pch=c(20,20,NA,19), lwd=c(NA,NA,2,1), bg='white', cex=0.8)
          }else{
            legend("topright", c('AM weight','PM weight','polynomial fit','moving average'), col=c(Color[c(5,8,11,2)]), lty=c(NA,NA,2,2), pch=c(20,20,NA,19), lwd=c(NA,NA,2,1), bg='white', cex=0.8)
          }
        }
        
        
      
      
    })
  
}