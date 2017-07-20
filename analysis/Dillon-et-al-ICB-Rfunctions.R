# DillonetalICBfunctions.R
#
#  Dillon, ME, Woods, HA, Wang, G, Fey, SB, Vasseur, DA, Telemeco, RS, Marshall, K, and S Pincebourde. 2016
#    Life in the frequency domain: the biological impacts of changes in climate variability at multiple time
#    scales. Int. Compar. Biol.
#  
# Supplementary Code for manipulating power in time series
# Written by Sam Fey, David Vasseur, and Art Woods 7/2015 - 3/2016

## manip.power() increases the power in a specified set of frequencies (fqs) in a time series.
## The power change is distributed evenly across all specified frequencies.

manip.power = function(series, fqs, power.change){

	L <- length(series)
	
	# Fourier transform series and capture phases and amplitudes of series
	dft <-  fft(series)/L
	mean <- sqrt(Im(dft[1])^2 + Re(dft[1])^2)		# first term in series
	amp <- sqrt(Im(dft[-1])^2 + Re(dft[-1])^2)	# omit that first term
	amp <- amp[1:((L/2) - 1)]				# snag just first half of amplitudes
	phase <- atan2(Im(dft),Re(dft))
	lastamp <- sqrt(Im(dft[(L/2) + 1])^2 + Re(dft[(L/2) + 1])^2)

	# extract amplitudes according to frequency window given by fqs
	vals <- amp[fqs]
	
	# squares of each
	valst <- (2*vals)^2/2

	# SS to add to each frequency
	power.increment <- power.change/length(vals)
	valst <- valst + power.increment
	amp[fqs] <- sqrt(2*valst)/2

	# construct vectors 
	dftshift <- c(mean, amp, lastamp, rev(amp))
	dftshift3 <- numeric(length = L)
	for (i in 1:L) {
 		dftshift3[i]<- complex(real = dftshift[i]*cos(phase[i]), imaginary = dftshift[i]*sin(phase[i]))
	}

	# reconstruct modified time series
	newseries <- Re(fft(c(dftshift3),inverse = TRUE))	 
	return(newseries)
}

# in the function above, the argument power.change indicates how much power to add to the specified
# frequencies. To determine this, it helps to be able to calculate the total power in a series, which
# can be done wtih get.total.SS:

get.total.SS = function(series){
	L = length(series)
	# Fourier transform series and capture phases and amplitudes of series
	dft<-  fft(series)/L
	amp<-sqrt(Im(dft[-1])^2+Re(dft[-1])^2)	# omit this first term
	amp<-amp[1:((L/2)-1)]				# snag just first half of amplitudes
	# squares of each
	ampt = (2*amp)^2/2
	X = sum(ampt)
	return(X)
}



######### This function manipulates ONLY mean and total variance (no power shifting among frequencies)

warpSeriesBasic = function(series, u, v){

	# Parameters to change series
	# u is the mean temperature adjustment, moves temperature up or down
	# v controls total variance adjustment: 4= no change, 8= 2 fold increase in total variance
	
	L <- length(series)
	dft <- fft(series)/L
	mean <- sqrt(Im(dft[1])^2 + Re(dft[1])^2)	# first term in series
	amp <- sqrt(Im(dft[-1])^2 + Re(dft[-1])^2)	# omit this first term
	amp <- amp[1:((L/2)-1)]				# snag just first half of amplitudes
	phase <- atan2(Im(dft),Re(dft))

	lastamp <- sqrt(Im(dft[(L/2) + 1])^2 + Re(dft[(L/2) + 1])^2)

	newmean <- mean + u					# alter mean
	newamp <- amp/2*sqrt(v)
	lastamp <- lastamp*sqrt(v)

  	dftshift <- c(newmean, newamp, lastamp, rev(newamp))

	dftshift3 <- numeric(length = L)
	for (i in 1:L) {
 		dftshift3[i] <- complex(real = dftshift[i]*cos(phase[i]), imaginary = dftshift[i]*sin(phase[i]))
	}

	newseries <- Re(fft(c(dftshift3), inverse = TRUE))
	return(newseries)
}


######### This function provides the most flexibility. It provides ways of manipulating mean and total variance
######### of a series. In addition, the parameters s, ds, and ys allow one to red- or blue-shift total variance
######### centered around seasonal, diel, and annual frequencies respectively. 
######### At present the function is designed to handle data with 3-hourly time steps (GLDAS). For other sampling
######### frequencies, the code will have to be modified.

warpSeries = function(u, v, s, ds, ys, series){

	# u = mean temperature adjustment, moves temperature up or down
	# v = total variance adjustment: 4 = no change, 8 = 2 fold increase in total variance
	# s = variance shift for seasonal signal -1 <= s <= 1, numbers near -1 shift more variance to
	# 	low frequencies (leading up to annual signal), values near 1 shift more variance to high frequecnies
	# 	no shift is s = 0. Note this value is constrained by "slimit" to perserve the periodogram continuity
	# ds = variance shift between the diel signal, and the other frquencies lower than the 
	# 	annual signal; -1 <= ds <= 1, values near -1 increase variance at the daily scale thus decreasing 
	# 	seasonal variance, values near 1 increase seasonal variance while decreasing daily, no shift is ds = 0
	# ys = variance shift between the annual signal, and all other frequncies lower than annual


	L <- length(series)

	#Calculate which amps correspond to annual and daily frequencies, based on 8 measurments per day (as in GLDAS)
	annualfq <- L/(8*365)
	dailyfq <- L/8
	
	# Fourier transform series and capture phases and amplitudes of series
	dft <-  fft(series)/L
	mean <- sqrt(Im(dft[1])^2 + Re(dft[1])^2)	# first term in series
	amp <- sqrt(Im(dft[-1])^2 + Re(dft[-1])^2)	# omit this first term
	amp <- amp[1:((L/2) - 1)]				# snag just first half of amplitudes
	phase <- atan2(Im(dft),Re(dft))

	lastamp <- sqrt(Im(dft[(L/2) + 1])^2 + Re(dft[(L/2) + 1])^2)

	newmean <- mean + u					# alter mean

	# Begin 3-part series that moves variance between daily, seasonal, and annual frequencies
	
	#
	# Part 1 of 3: move variance between low (longer than daily) and high frequencies (shorter than daily) 
	#
	
	diel <- dailyfq
	dielvalue <- amp[dailyfq]

	annual <- annualfq
	annualvalue <- amp[annualfq]

	# Split the amplitude series into low frequency less than annual but greater
	# than daily "red" and high frequency sub-daily "blue" bins
	red <- amp[(annualfq + 1):(dailyfq - 1)]
	blue <- amp[(dailyfq + 1):length(amp)]
	

	# Calculate the total contribution to red and blue amplitude squared sums
	redt <- (2*red)^2/2
	bluet <- (2*blue)^2/2
	tr2 <- sum(redt)
	tb2 <- sum(bluet)


		# Calculate the maximum contribution of each series amplitudes to inflating the other series

		maxred2 <- (tr2 + tb2)/tr2
		maxblue2 <- (tr2 + tb2)/tb2

		# Adjust the contributions to red and blue amplitudes based on the the "s" term
		# that appears at the top of the notebook, such that when s = 0, no shift takes place, 
		# negative values red shift the series, and high values blue shift the series

		noshift <- 1/maxblue2;

		# constrain the amount of variance that can be moved, such that the continuity 
		# of the periodogram is preserved. slimit=.5 is a 
		# conservative value for preserving continuity.

		slimit <- .5

		# define red distance (rl), blue distance (bl), and ratio of two 
		# (r1bl) from noshift to maximum s value

		rl <- noshift;
		bl <- 1 - noshift;

		# add or subtract the amount of variance moved depending on "s" *)
		acts <- ifelse(s < 0, noshift + (rl*s*slimit), 
      		 ifelse(s > 0, noshift + (bl*slimit*s), noshift)
      	 	)
		
		# redefine redt and bluet based on above; otherwise they are passed through unchanged to next block
		redt <- redt*maxred2*(1 - acts);
		bluet <- bluet*maxblue2*acts;


		#
		# Part 2 of 3: move variance between daily and all frequncies below annual frequency
		#
		
		

		td3 <- (2*dielvalue)^2/2

		# Determine amplitude totals of all other components (both red and blue series)
		to3 <- tr2 + tb2

		# Calulate the maximum contribution of diel and non-diel amplitudes to inflating the other series
		maxdiel3 <- (td3 + to3)/td3
		maxo3 <- (to3 + td3)/to3

		# Calculate value no shift in the daily amplitudes
		nodshift <- 1/maxo3

		# define "other variance" distance (ol), and diel distance (dl) from nodshift to maximum ds value
		dl <- nodshift
		ol <- 1 - nodshift

		# add or subtract the amount of variance moved depending on "ds"
		actds <- ifelse(ds < 0, nodshift + (dl*ds), 
            	  	 ifelse(ds > 0, nodshift + (ol*ds), nodshift)
			)

		diel2 <- td3*maxdiel3*(1 - actds)
		dielvalue2 <- sqrt(2*diel2)/2
		bluet <- bluet*(maxo3*actds)
		redt <- redt*(maxo3*actds)

		#
		# Part 3 of 3: move variance between daily and all frequncies below annual frequency
		#
		
		ty4 <- (2*annualvalue)^2/2
		
		# Determine amplitude totals of all other components (both red and blue series, and daily signal)
		to4 <- tr2 + tb2 + td3
		
		# Calulate the maximum contribution of diel and non-diel amplitudes to inflating the other series
		maxy4 <- (ty4 + to4)/ ty4
		maxo4 <- (to4 + ty4)/ to4
		
		# Calculate value no shift in the daily amplitudes
		noyshift <- 1/maxo4
		
		# define "other variance" distance (ol), and diel distance (dl) from nodshift to maximum ds value
		yl <- noyshift
		ol2 <- 1 - noyshift
		
		#add or subtract the amount of variance moved depending on "ds"
		actys <- ifelse(ys < 0, noyshift + (yl*ys), 
		                ifelse(ys > 0, noyshift + (ol2*ys), noyshift)
		)
		
		annual2 <- ty4*maxy4*(1 - actys)
		annualvalue2 <- sqrt(2*annual2)/2
		bluet <- bluet*(maxo4*actys)
		redt  <- redt*(maxo4*actys)
		diel3 <- diel2*(maxo4*actys)
		
		
	# Now, calculate the new amplitudes by back transforming the squared sums into the new amplitudes
	redbackt <- sqrt(2*redt)/2
	bluebackt <- sqrt(2*bluet)/2
	dielbackt <- sqrt(2*diel3)/2

	newamp <- c(amp[1:(annualfq - 1)], annualvalue2, redbackt, dielbackt, bluebackt)

	# Change variance in series, v=4 is no change

		newamp <- newamp / 2 * sqrt(v)
		lastamp <- lastamp*sqrt(v)


  	dftshift <- c(newmean, newamp, lastamp, rev(newamp))

	dftshift3<-numeric(length = L)
	for (i in 1:L) {
 		dftshift3[i]<- complex(real = dftshift[i]*cos(phase[i]), imaginary = dftshift[i]*sin(phase[i]))
	}

	newseries <- Re(fft(c(dftshift3), inverse = TRUE))
	return(newseries)
	
} # end of function



############## testing the above functions with sample data ######

# This requires first loading the "SampleData.RData" file

load("SampleData.RData")
sseries <- mat.10.series[,2] 	# defines a specific temperature time series. The matrix has 10 time series
					# (columns) corresponding to 10 different locations in North America.
					# The data are air temperatures at 2-m height for the periods
					# 2001 - 2006. See http://disc.sci.gsfc.nasa.gov/datareleases/gldas-version-2.0-data-sets


## Test 1: Testing manip.power function

# First, calculate some relevant frequencies for the gldas data

L <- length(sseries)
dailyfqs <- (L/4):(L/24)						# from half a day to 3 days
weeklyfqs <- (L/24 - 1):round((L/(14*8)))				# from three days to two weeks
monthlyfqs <- round((L/(14*8)-1)):round((L/(8*30*2)))		# from two weeks to two months
seasonalfqs <- (round((L/(8*30*2)))-1):round((L/(8*30*6)))	# from two months to six months
annualfqs <- 11:3								# from six months to two years


# Calculate total power in sseries
get.total.SS(sseries)
86.27993

mseries1 <- manip.power(sseries, monthlyfqs, 8.6) 		# Perform manipulation, here were are adding about 10% of the total 
										# power to the monthly frequencies

# Graphically examine the effects of the manipulation over the entire series 
plot(sseries,type="l") #plot old series over the entire range
lines(mseries1,col="red") #over plot new series over the entire range in red

r <- 1000:1200 # Define a small range over which to visual changes to data

plot(sseries[r], type="l")
lines(mseries1[r], type="l", col="red", lwd=2)


## Test 2: Testing warpSeriesBasic function

newseries2 <- warpSeriesBasic(series = sseries, u = 3, v = 1) 
    # defines "newseries2" as a new manipulated time series, by increasing mean (u) and reducing variance (v) 

# Graphically examine the effects of the manipulation over the entire series 
plot(sseries,type="l") #plot old series over the entire range
lines(newseries2,col="green") #over plots new series over entire range in green

# Graphically examine the effects of the manipulation over a subset of series 
plot(sseries[r], type="l") # plot subset of the old series
lines(newseries2[r], type="l",col="green",lwd=2) # plot  subset of manipulated series in green


## Test 3: Testing warpSeries function

newseries3 <- warpSeries(u = -1, v = 6, s = 0, ds = 0, ys = -.9, series = sseries)
    # defines "newseries3" as a new manipulated time series, by decreasing mean (u),
    # incresing the total variance (v), and increasing all of the variance experienced at the yearly frequency

# Graphically examine the effects of the manipulation over the entire series 
plot(sseries, type="l")  #plot old series over the entire range
lines(newseries3, type="l", col="blue", lwd=3) #over plots new series over entire range in blue

# Graphically examine the effects of the manipulation over a subset of series 
plot(sseries[r], type="l", lwd=2) # plot subset of the old series
lines(newseries3[r], type="l", col="blue", lwd=2) # plot subset of manipulated series in blue

# Examine the temperature distributions of both series by plotting a histogram of the orgional and manipulated time series
hist(sseries,col= "dark grey", xlim=c(min(sseries,newseries3),max(sseries,newseries3)),add=F, nclass = 30, main = "")
hist(newseries3,col= rgb(0,0,1,.5),add=T, xlim=c(min(series,newseries),max(series,newseries)), nclass = 30)





