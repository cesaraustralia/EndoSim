input.Data <- c()

temp <- read.csv('st_r_Data_Precipitation.csv')
temp <- list(Precipitation = temp)
input.Data <- c(input.Data, temp)

temp <- read.csv('st_r_Data_Temperature.csv')
temp <- list(Temperature = temp)
input.Data <- c(input.Data, temp)

temp <- read.csv('st_r_Data_Prop_day.csv')
temp <- list(Prop_day = temp)
input.Data <- c(input.Data, temp)

temp <- read.csv('st_r_Data_julday.csv')
temp <- list(julday = temp)
input.Data <- c(input.Data, temp)

rm(temp)
#inputData: function to call climate/input data from 
#file, integrated within the deSolve function for Aphid
#population dynamics model
#x = timestep
#name = data required ("Temperature or Precipitation)
#input.Data = the list of data that have just been called
inputData <- function(x, name, datalist=input.Data) {
	df=datalist[[name]]
	minT <- min(df[,1],na.rm=T)
	maxT <- max(df[,1],na.rm=T)
	if (x < minT | x > maxT) {
		l <- lm(get(colnames(df)[2])~poly(get(colnames(df)[1]),3),data=df)
		do <- data.frame(x); colnames(do) <- colnames(df)[1]
		o <- predict(l,newdata=do)[[1]]	} else {
	t1 <- max(df[which(df[,1] <= x),1])
	t2 <- min(df[which(df[,1] >= x),1])
	if (t1 == t2) {
		o <- df[t1,2]}
	else {
		w1=1/abs(x-t1);w2=1/abs(x-t2)
	o <- ((df[which( df[,1] == t1),2]*w1)+(df[which( df[,1] == t2),2]*w2)) / (w1+w2) } }
  o }

