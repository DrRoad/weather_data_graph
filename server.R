# pkgTest <- function(x) {
#     if (!require(x,character.only = TRUE)) {
#         install.packages(x,dep=TRUE)
#         if(!require(x,character.only = TRUE)) {
#             stop("Package not found")
#         }
#     }
# }
# pkgTest("shiny")
# pkgTest("ggplot2")
# pkgTest("gridExtra")
require("shiny")
require("ggplot2")
require("gridExtra")

AbBkSCol <- c("springgreen4", "cornflowerblue", "mediumvioletred", "darkorange", "red4", "plum3", "wheat3", "orangered2", "blue4")
Col <- c('greenyellow', 'limegreen', 'mediumaquamarine', 'dodgerblue3', 'midnightblue', 'darkgreen')
NonBlk <- c(452.40000, 1448.67460, 1261.23571,   16.36111,   66.46528,   11.67857,    0.00000,    0.00000,    0.00000)/14.5
FilAbr <- c(48.58333,   26.66667,  982.76720, 1028.98511,  651.25534,   49.91183,   13.02083,    0.00000,    0.00000)/14.5
NonAbr <- c(1051.93056,   15.16667,   32.00000,    0.00000,   17.00000,    0.00000,    0.00000,    0.00000,    0.00000)/14.5
FilBlk <- c(114.555556, 1103.083333,  249.947917,  251.305556,   33.000000,    1.083333,    4.616667,    0.150000,   0.000000)/14.5


shinyServer(function(input, output) {
    
    data.set <- reactive({
        
        HTemp.dat <- read.csv("dater.csv", stringsAsFactors=F)
        HTemp.dat$Rtime <- strptime(as.character(HTemp.dat$Rtime),"%Y-%m-%d %H:%M:%S",tz="GMT")
        
        if(input$Data.place == 'Location 1') {
            HTemp.dat <- HTemp.dat[,c(2,4,3,5,6,11,12)]
        }
        
        if(input$Data.place == 'Location 2') {
            HTemp.dat <- HTemp.dat[,c(2,7,8,9,10,11,12)]
        }
        
        
        if(input$Data.FNF == 'Treatment') {
            HTemp.dat <- HTemp.dat[,c(1,2,4,6,7)]
        }
        
        if(input$Data.FNF == 'No Treatment') {
            HTemp.dat <- HTemp.dat[,c(1,3,5,6,7)]
        }
        
        
        
        
        
        colnames(HTemp.dat) <- c('time', 'Temp', 'Wat', 'day', 'X')
        print(HTemp.dat[1:14,])
        HTemp.dat <- na.omit(HTemp.dat)
        
        if(input$Meanpoints != 'a') {
            switch(input$Meanpoints,
                   b= {num <- 2},
                   c= {num <- 3},
                   d= {num <- 4},
                   e= {num <- 6},
                   f= {num <- 12},
                   g= {num <- 24},
                   print("Error..."))
            
            for(a in seq(1, nrow(HTemp.dat), 1)) {
                HTemp.dat$TMean[a] <- mean(na.omit(HTemp.dat$Temp[c(a:(a+num))]))
                HTemp.dat$WMean[a] <- mean(na.omit(HTemp.dat$Wat[c(a:(a+num))]))
            }            
        } else {
            HTemp.dat$TMean <- HTemp.dat$Temp
            HTemp.dat$WMean <- HTemp.dat$Wat
        }
        
        data <- HTemp.dat
        data <- data
    })
    
    
    
    
    
    output$plot <- renderPlot({
        
        
        if(input$Data.place == 'Location 1') {
            if(input$Data.FNF == 'Treatment') {
                SeedTill <- FilAbr
            }
            
            if(input$Data.FNF == 'No Treatment') {
                SeedTill <- NonAbr
            }
        }
        
        if(input$Data.place == 'Location 2') {
            if(input$Data.FNF == 'Treatment') {
                SeedTill <- FilBlk
            }
            
            if(input$Data.FNF == 'No Treatment') {
                SeedTill <- NonBlk
            }
        }
        
        p <-  ggplot(data.set(), aes(X, TMean)) + ylim(-5, 35) + xlab('Date') + ylab('Temperature (C)')
        q <-  ggplot(data.set(), aes(X, WMean)) + ylim(0, 0.35) + ylab('Moisture (W/V')
        
        if(input$Poly) {
            p <- p + stat_smooth(method = "lm", formula = y ~ poly(x, 5), size = 1, fill="red", colour="darkred")
            q <- q + stat_smooth(method = "lm", formula = y ~ poly(x, 5), size = 1)
        }
        
        
        if(input$SowingDates) {
            p <- p + annotate("pointrange", x = as.POSIXct("2013-05-08 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[1], colour = AbBkSCol[1], size = 1.5)
            p <- p + annotate("pointrange", x = as.POSIXct("2013-05-18 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[2], colour = AbBkSCol[2], size = 1.5)
            p <- p + annotate("pointrange", x = as.POSIXct("2013-05-28 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[3], colour = AbBkSCol[3], size = 1.5)
            p <- p + annotate("pointrange", x = as.POSIXct("2013-06-08 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[4], colour = AbBkSCol[4], size = 1.5)
            p <- p + annotate("pointrange", x = as.POSIXct("2013-06-18 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[5], colour = AbBkSCol[5], size = 1.5)
            p <- p + annotate("pointrange", x = as.POSIXct("2013-06-28 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[6], colour = AbBkSCol[6], size = 1.5)
            p <- p + annotate("pointrange", x = as.POSIXct("2013-07-08 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[7], colour = AbBkSCol[7], size = 1.5)
            p <- p + annotate("pointrange", x = as.POSIXct("2013-07-18 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[8], colour = AbBkSCol[8], size = 1.5)
            p <- p + annotate("pointrange", x = as.POSIXct("2013-07-28 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[9], colour = AbBkSCol[9], size = 1.5)
            p <- p + annotate("text", x = as.POSIXct("2013-05-08 12:00"), y = 0, label = "1", colour = 'black', size = 4)
            p <- p + annotate("text", x = as.POSIXct("2013-05-18 12:00"), y = 0, label = "2", colour = 'black', size = 4)
            p <- p + annotate("text", x = as.POSIXct("2013-05-28 12:00"), y = 0, label = "3", colour = 'black', size = 4)
            p <- p + annotate("text", x = as.POSIXct("2013-06-08 12:00"), y = 0, label = "4", colour = 'black', size = 4)
            p <- p + annotate("text", x = as.POSIXct("2013-06-18 12:00"), y = 0, label = "5", colour = 'black', size = 4)
            p <- p + annotate("text", x = as.POSIXct("2013-06-28 12:00"), y = 0, label = "6", colour = 'black', size = 4)
            p <- p + annotate("text", x = as.POSIXct("2013-07-08 12:00"), y = 0, label = "7", colour = 'black', size = 4)
            p <- p + annotate("text", x = as.POSIXct("2013-07-18 12:00"), y = 0, label = "8", colour = 'black', size = 4)
            p <- p + annotate("text", x = as.POSIXct("2013-07-28 12:00"), y = 0, label = "9", colour = 'black', size = 4)
            
        }
        
        
        if(input$Points){
            p <- p + geom_point(size = .1)
            q <- q + geom_point(size = .1, colour = "darkblue")
        }
        if(input$Color) {
            p <- p + geom_point(aes(color=time)) + scale_colour_manual(values=Col)
            q <- q + geom_point(aes(color=time)) + scale_colour_manual(values=Col)
        }
        
        
        p <- p + xlim(as.POSIXct(input$Dates[1]), as.POSIXct(input$Dates[2]))
        q <- q + xlim(as.POSIXct(input$Dates[1]), as.POSIXct(input$Dates[2])) + ggtitle(paste(input$Data.place, input$Data.FNF))
        
        p <- p + theme(plot.margin = unit(c(0,0,0,0),units="points"))
        q <- q + theme(plot.margin = unit(c(0,0,-37,0),units="points"))
        
        
        grid.arrange(q,p, heights = c(2/8, 6/8))

        
    }  ) #, height=100
    
    
    output$plot2 <- renderPlot({
        input$makeG2
        isolate({    
            
            if(input$Data.place == 'Location 1') {
                if(input$Data.FNF == 'Treatment') {
                    SeedTill <- FilAbr
                }
                
                if(input$Data.FNF == 'No Treatment') {
                    SeedTill <- NonAbr
                }
            }
            
            if(input$Data.place == 'Location 2') {
                if(input$Data.FNF == 'Treatment') {
                    SeedTill <- FilBlk
                }
                
                if(input$Data.FNF == 'No Treatment') {
                    SeedTill <- NonBlk
                }
            }
            
            p2 <-  ggplot(data.set(), aes(X, TMean)) + ylim(-5, 35) + xlab('Date') + ylab('Temperature (C)')
            q2 <-  ggplot(data.set(), aes(X, WMean)) + ylim(0, 0.35) + ylab('Moisture (W/V')
            
            if(input$Poly) {
                p2 <- p2 + stat_smooth(method = "lm", formula = y ~ poly(x, 5), size = 1, fill="red", colour="darkred")
                q2 <- q2 + stat_smooth(method = "lm", formula = y ~ poly(x, 5), size = 1)
            }
            
            
            if(input$SowingDates) {
                p2 <- p2 + annotate("pointrange", x = as.POSIXct("2013-05-08 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[1], colour = AbBkSCol[1], size = 1.5)
                p2 <- p2 + annotate("pointrange", x = as.POSIXct("2013-05-18 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[2], colour = AbBkSCol[2], size = 1.5)
                p2 <- p2 + annotate("pointrange", x = as.POSIXct("2013-05-28 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[3], colour = AbBkSCol[3], size = 1.5)
                p2 <- p2 + annotate("pointrange", x = as.POSIXct("2013-06-08 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[4], colour = AbBkSCol[4], size = 1.5)
                p2 <- p2 + annotate("pointrange", x = as.POSIXct("2013-06-18 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[5], colour = AbBkSCol[5], size = 1.5)
                p2 <- p2 + annotate("pointrange", x = as.POSIXct("2013-06-28 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[6], colour = AbBkSCol[6], size = 1.5)
                p2 <- p2 + annotate("pointrange", x = as.POSIXct("2013-07-08 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[7], colour = AbBkSCol[7], size = 1.5)
                p2 <- p2 + annotate("pointrange", x = as.POSIXct("2013-07-18 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[8], colour = AbBkSCol[8], size = 1.5)
                p2 <- p2 + annotate("pointrange", x = as.POSIXct("2013-07-28 12:00"), y = 0, ymin = 0, ymax = (35/100)*SeedTill[9], colour = AbBkSCol[9], size = 1.5)
                p2 <- p2 + annotate("text", x = as.POSIXct("2013-05-08 12:00"), y = 0, label = "1", colour = 'black', size = 4)
                p2 <- p2 + annotate("text", x = as.POSIXct("2013-05-18 12:00"), y = 0, label = "2", colour = 'black', size = 4)
                p2 <- p2 + annotate("text", x = as.POSIXct("2013-05-28 12:00"), y = 0, label = "3", colour = 'black', size = 4)
                p2 <- p2 + annotate("text", x = as.POSIXct("2013-06-08 12:00"), y = 0, label = "4", colour = 'black', size = 4)
                p2 <- p2 + annotate("text", x = as.POSIXct("2013-06-18 12:00"), y = 0, label = "5", colour = 'black', size = 4)
                p2 <- p2 + annotate("text", x = as.POSIXct("2013-06-28 12:00"), y = 0, label = "6", colour = 'black', size = 4)
                p2 <- p2 + annotate("text", x = as.POSIXct("2013-07-08 12:00"), y = 0, label = "7", colour = 'black', size = 4)
                p2 <- p2 + annotate("text", x = as.POSIXct("2013-07-18 12:00"), y = 0, label = "8", colour = 'black', size = 4)
                p2 <- p2 + annotate("text", x = as.POSIXct("2013-07-28 12:00"), y = 0, label = "9", colour = 'black', size = 4)
                
            }
            
            
            if(input$Points){
                p2 <- p2 + geom_point(size = .1)
                q2 <- q2 + geom_point(size = .1, colour = "darkblue")
            }
            if(input$Color) {
                p2 <- p2 + geom_point(aes(color=time)) + scale_colour_manual(values=Col)
                q2 <- q2 + geom_point(aes(color=time)) + scale_colour_manual(values=Col)
            }
            
            
            p2 <- p2 + xlim(as.POSIXct(input$Dates[1]), as.POSIXct(input$Dates[2]))
            q2 <- q2 + xlim(as.POSIXct(input$Dates[1]), as.POSIXct(input$Dates[2])) + ggtitle(paste(input$Data.place, input$Data.FNF))
            
            p2 <- p2 + theme(plot.margin = unit(c(0,0,0,0),units="points"))
            q2 <- q2 + theme(plot.margin = unit(c(0,0,-37,0),units="points"))
            
            
            grid.arrange(q2,p2, heights = c(2/8, 6/8))

            
        })        
    }  ) #, height=100
    
#     observe({
#         if (input$save == 0)
#             return()
#         
#         isolate({
#             ggsave(input$name, p, dpi = 500)
#         })
#     })
    
    #############May be save when ploting the top plot based on 2 word boxis
})
