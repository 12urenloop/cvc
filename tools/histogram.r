# Script to produce a histogram of lap times
lap_times <- read.csv('lap-times.csv', header = F)
lap_times <- round(lap_times)

print(lap_times)

pdf(file = 'lap-times.pdf')

hist(lap_times$V1, main = 'Lap time histogram',
        xlab = 'Time (in s)', ylab = 'Frequency', col = 'lightblue')

dev.off()
