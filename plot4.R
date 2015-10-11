loaddata <- function(fn, s, e)
{
    s <- as.POSIXlt(s)
    e <- as.POSIXlt(e)

    d <- read.csv(fn, sep = ';', na.strings = '?')
    d$DateTime <- as.POSIXlt(paste(d$Date, d$Time), 
                             format = '%d/%m/%Y %H:%M:%S')
    d <- d[, c(-1, -2)]
    d <- d[which(d$DateTime>=s & d$DateTime<e),]

}

plot1 <- function(d)
{
    hist(d$Global_active_power, main = 'Global Active Power', xlab = 'Global Active Power (kilowatts)', col = 'red', ylim = c(0, 1200))
}

plot2 <- function(d)
{
    plot(d$Global_active_power, type = 'l', 
         ylab = 'Global Active Power (kilowatts)', 
        xlab = '',
        xaxt = 'n'
        )
    axis(1, at = c(1, 24*60, 2*24*60), labels = c("Thu", 'Fri', 'Sat'))
}

plot3 <- function(d, bty = 'o')
{
    plot(d$Sub_metering_1, type = 'l',
         ylab = 'Energy sub metering',
         xlab = '',
         xaxt = 'n'
         )
    lines(d$Sub_metering_2, col = "red")
    lines(d$Sub_metering_3, col = "blue")
    legend("topright", col = c('black', 'red', 'blue'), 
           legend = paste0('Sub_metering_', 1:3), 
           lty = 1,
           bty = bty)
    axis(1, at = c(1, 24*60, 2*24*60), labels = c("Thu", 'Fri', 'Sat'))
    
}

plot4 <- function(d) 
{
    par(mfrow = c(2, 2))
    plot(d$Global_active_power, type = 'l', 
         ylab = 'Global Active Power', 
         xlab = '',
         xaxt = 'n'
    )
    axis(1, at = c(1, 24*60, 2*24*60), labels = c("Thu", 'Fri', 'Sat'))
    
    plot(d$Voltage, type = 'l', 
         ylab = 'Voltage', 
         xlab = 'datetime',
         xaxt = 'n'
    )
    axis(1, at = c(1, 24*60, 2*24*60), labels = c("Thu", 'Fri', 'Sat'))
    plot3(d, bty = 'n')
    
    plot(d$Global_reactive_power, type = 'l', 
         ylab = 'Global_reactive_power', 
         xlab = 'datetime',
         xaxt = 'n'
    )
    axis(1, at = c(1, 24*60, 2*24*60), labels = c("Thu", 'Fri', 'Sat'))
    
}

d <- loaddata('household_power_consumption.txt', '2007-02-01', '2007-02-03')
par(mfrow = c(1,1))

png('plot4.png')
plot4(d)
dev.off()
