#############################################################################################################################

# ---------------------------------------------- LOADING REQUIRED LIBRARIES ----------------------------------------------- #

#############################################################################################################################

### Install Packages
# install.packages('rlang')
# install.packages('lubridate')

### Loading the required libraries
library(data.table)
library(rlang)
library(forecast)
library(tseries)
library(graphics)
require(graphics)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
require(gridExtra)
library(cowplot)


#############################################################################################################################

# ---------------------------------------------------- DATA Understanding ------------------------------------------------- #

#############################################################################################################################


#Order ID:-Unique ID of the transaction 
#Order Date:-Date on which the order was placed
#Ship Date:-Date on which the shipment was made
#Ship Mode:-The mode of shipment (category)
#Customer ID:-The unique ID of the customer
#Customer Name:-Name of the customer
#Segment:-The market segment to which the product belongs
#City:-City of the delivery address
#State:-State of the delivery address
#Country:-Country of the delivery address
#Postal Code:-Postal code of the delivery address
#Market:-Market segment to which the customer belongs
#Region:-Geographical region of the customer
#Product ID:-Unique ID of the product
#Category:-Category of the product
#Sub-Category:-Sub-category of the product
#Product Name:-Name of the product
#Sales:-Total sales value of the transaction
#Quantity:-Quantity of the product ordered
#Discount:-Discount percentage offered on the product
#Profit:-Profit made on the transaction
#Shipping Cost:-Shipping cost incured on the transaction
#Order Priority:-Priority assigned to the order


#############################################################################################################################

# -------------------------------------------------------- Load Data ------------------------------------------------------ #

#############################################################################################################################

# Loading the data file.
Global_Superstore <- read.csv("Global Superstore.csv", header = T, sep = ',')
str(Global_Superstore) 

# Checking for any duplication
nrow((unique(Global_Superstore))) == nrow(Global_Superstore) #No of unique rows is equal to total no of rows, hence duplication doesnt exists.

# using only required columns for analysis
Global_Superstore<- Global_Superstore[,c(3,8,13,19,20,22)]

# Eliminating days, and keeping only months and years
Global_Superstore$Date <- as.Date(Global_Superstore$Order.Date, "%d-%m-%Y")
Global_Superstore$Date <- format(Global_Superstore$Date,"%y-%m")

head(Global_Superstore)

# Adding a conctaenate column by merging the 7 markets with 3 segments. Hence, Total Market_Segments = 21
Global_Superstore$concatenate <- as.factor(paste(Global_Superstore$Market,Global_Superstore$Segment, sep = '_'))
Global_Superstore$concatenate


#############################################################################################################################

# ------------------------------------------ Exploratory Data Analysis ---------------------------------------------------- #

#############################################################################################################################


# Plots of Monthly Quantity for all 21 segments
ggplot(Global_Superstore, aes(x = factor(Date), y = Quantity, group = 1)) + geom_point() + 
  scale_x_discrete(breaks = Global_Superstore$Date[c(T,F,F)]) + facet_wrap(~concatenate) + 
  geom_line() + labs(x = "Month", y = "Quantity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Plots of Monthly Sales for all 21 segments
ggplot(Global_Superstore, aes(x = factor(Date), y = Sales, group = 1)) + geom_point() + 
  scale_x_discrete(breaks = Global_Superstore$Date[c(T,F,F)]) + facet_wrap(~concatenate) + 
  geom_line() + labs(x = "Month", y = "Sales") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Plots of Monthly Profit for all 21 segments
ggplot(Global_Superstore, aes(x = factor(Date), y = Profit, group = 1)) + geom_point() + 
  scale_x_discrete(breaks = Global_Superstore$Date[c(T,F,F)]) + facet_wrap(~concatenate) + 
  geom_line() + labs(x = "Month", y = "Profit") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#############################################################################################################################

# ------------------------------- Creating function for 21 market buckets to store value ---------------------------------- #

#############################################################################################################################


# Function to calculate coefficient of variance for each unique market segment
Store <- function(buckets_21){
  sub1<- subset(Global_Superstore,Global_Superstore$concatenate== buckets_21)
  sub2 <- aggregate(cbind(Sales,Profit,Quantity)~ Date,sub1,sum)
  sub2$Month <- seq(1,nrow(sub2),1)
  cov <- sd(sub2$Profit)/mean(sub2$Profit)
  return(list(nrow(sub2),cov))
}


# Empty Matrix to store coefficient of variance variables for each unique market_segment
Nil_matrix <- matrix(0,21,2)


# Calculating coefficient of variance and adding it to above matrix
for(i in 1:21){
  Nil_matrix[i,1] <- levels(Global_Superstore$concatenate)[i]
  Nil_matrix[i,2] <- Store(levels(Global_Superstore$concatenate)[i])[[2]]
}


# Converting matrix to data frame.
Nil_matrix <-data.frame(Nil_matrix)
colnames(Nil_matrix)<-c("segments","coven")


# Arranging the Nil_matrix data frame on the ascending order of coefficient of variance.
Nil_matrix <- Nil_matrix[order(Nil_matrix$coven), ]
head(Nil_matrix)

# The best segments are the once which has the lowest value of coefficient of variance.
# Taking first two best segments into consideration.
top2 <-as.character(Nil_matrix$segments[1:2])
top2

# Plotting top segments.
top_segment <- head(Nil_matrix)
ggplot(top_segment, aes(segments, coven, fill = segments)) + geom_bar(stat = "identity") + ylab("Coefficient of Variance")

# Clearly, EU_Consumer and APAC_COnsumer have the lowest coefficient of variance. Hence they are the best segments for 
# forecasting.

detach(package:dplyr)


#############################################################################################################################

# ---------------------------------Creating functions for Time Series Forecasting------------------------------------------ #

#############################################################################################################################


# Creating a function to Smoothen the time series, taking width as function argument.
smooth_series <- function(timeser, width){
  
  smoothedseries <- stats::filter(timeser, filter = rep(1/(2*width + 1), (2*width + 1)), method = "convolution", sides = 2)
  
  # Smoothing left end of the time series
  diff <- smoothedseries[width+2] - smoothedseries[width+1]
  for (i in seq(width,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  # Smoothing right end of the time series
  n <- length(timeser)
  diff <- smoothedseries[n-width] - smoothedseries[n-width-1]
  for (i in seq(n-width+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  
  smoothedseries
}

### Why ts_data_prep function?
# Below we have created a ts_data_prep_function to incorporate the concept of encapsulation and reduce code repeatence, since
# we have to do time series analysis four times on four different subsets of data, where the only difference in code will
# be a few parameters or arguments. 

### Arguments for ts_data_prep()

# segment - Segment No.
# forecast_for - 'Sales' or 'Quantity'
# width - smoothening width


### What ts_data_prep() does?
## For each time series analysis, we have to do a lot of data preparation like:

#-- Creating a subset of data from master_frame

#-- Aggregating the date w.r.t Sales or Quantity for each month, whichever is provided as function argument.

#-- Dividing the aggregated data into in_data and out_data, where in_data will be used to create a model, and out_data will be 
#  used to verify the reliability of model by calculating MAPE values.

#-- creating a total_timeseries and timeseries variable by applying ts() function

#-- Creating timevals_in and timevals_out variable i.e. timevalues input for model preparation and timevalues output for forecasting.

#-- Smoothening the time series with the width provided as function argument

#-- Creating a smoothed data frame from the smoothed series

#-- Creating a combined plot of raw data and smoothed data to visualize our smoothing accuracy

# Thus, ts_data_prep function encapsulates the above repetitive task and creates unique variables, dataframes, plots into 
# Global Environmet(w.r.t segments provided as function arguments), by appending segment name at the end of each variable 
# created. This variables, dataframes and plots can then be called to straight away begin the forecasting,


ts_data_prep <- function(segment=1, forecast_for = 'Sales', width = 1){
  
  forecast_name <- paste("forecast_for", top2[segment], forecast_for, "on", sep = "_")
  
  assign(forecast_name, forecast_for, .GlobalEnv)
  
  sub1 <- subset(Global_Superstore,Global_Superstore$concatenate==top2[segment])
  
  df_name <- paste(top2[segment], forecast_for, sep = "_")
  
  assign(df_name, aggregate(get(forecast_for)~Date,sub1,sum), .GlobalEnv)
  
  Month <- seq(1,nrow(get(df_name)),1)
  
  cbind(Month, get(df_name))
  
  assign(df_name, cbind(Month,get(df_name)), .GlobalEnv)
  
  setnames(get(df_name), old=colnames(get(df_name)), new=c("Month", "Date",forecast_for))
  
  df_name_in <- paste(df_name, 'in', sep = "_")
  
  df_name_out<- paste(df_name, 'out', sep = "_")
  
  assign(df_name_in, get(df_name)[1:42,], .GlobalEnv)
  
  assign(df_name_out, get(df_name)[43:48,], .GlobalEnv)
  
  totaltimeser <- ts(get(df_name)[, forecast_for])
  
  timeser <- ts(get(df_name_in)[,forecast_for])
  
  timevals_in <- get(df_name_in)[,"Month"]
  
  timevals_out <- get(df_name_out)[,"Month"]
  
  totaltimeser_name <- paste("totaltimeser", df_name, sep = "_")
  
  timeser_name <- paste("timeser", df_name, sep = "_")
  
  timevals_in_name <- paste("timevals_in", df_name, sep = "_")
  
  timevals_out_name <- paste("timevals_out", df_name, sep = "_")
  
  assign(totaltimeser_name, totaltimeser, .GlobalEnv)
  
  assign(timeser_name, timeser, .GlobalEnv)
  
  assign(timevals_in_name, timevals_in, .GlobalEnv)
  
  assign(timevals_out_name, timevals_out, .GlobalEnv)
  
  smoothedseries <- smooth_series(timeser, width = width) 
  
  smoothedseries_name <- paste("smoothedseries", df_name, sep = "_")
  
  assign(smoothedseries_name, smoothedseries, .GlobalEnv)
  
  smoothedplot_name <- paste("smoothedplot", df_name, sep = "_")
  
  smoothed_df <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
  
  colnames(smoothed_df) <- c("Month", paste("Smoothed", forecast_for))
  
  smoothed_df_name <- paste("smoothed_df", df_name, sep = "_")
  
  assign(smoothed_df_name, smoothed_df, .GlobalEnv)
  
  smoothed_plot <- ggplot(melt(timeser), aes(x=seq_along(value), y = value)) + 
    geom_line(aes(colour = "Raw"), size = 1) + 
    geom_line(data =melt(smoothedseries),  aes(x=seq_along(value), y = value, colour = "Smoothed"),size = 2)+
    guides(color=guide_legend(title="labels")) + xlab("Months: January 2011 to June 2014 -- 42 months") + ylab("Monthly Sales")+
    labs(title= paste("Monthly", forecast_for, "for", top2[segment]))
  
  assign(smoothedplot_name, smoothed_plot, .GlobalEnv)
  
  paste("Time series data prepared for", df_name )

}


### Creating plot_all functions to straightaway prepare ggplots for the timeseries entered as function arguments,
###   The plot aesthetics will be automatically arranged w.r.t function arguments.

### Arguments for plot_all

#-- timeser = Raw Data timeseries
#-- smoothedseries = smoothed timeseries
#-- global pred = forecasted timeseries
#-- forecast_on = 'Sales' or 'Quantity'
#-- segment = segment_name.
#-- arimamode = TRUE if model is prepared through autoarima, else it assumes model is prepared through classical decomposiion.

plot_all <- function(timeser, smoothedseries = NA, global_pred, forecast_on, segment = segment_name, arimamode = FALSE){
  
  xlab1 = NA
  
  if (length(timeser) == 42){
    xlab1 = "Months: January 2011 to June 2014 -- 42 months"
  }
  
  if (length(timeser) == 48){
    xlab1 = "Months: January 2011 to Dec 2014 -- 48 months"
  }
  
  titlex = NA
  if (arimamode){
    titlex = paste("Monthly", forecast_on, "for", segment, "(AUTO ARIMA MODEL)")
  }else{
    titlex = paste("Monthly", forecast_on, "for", segment, "(CLASSICAL DECOMPOSITION MODEL)")
    
  }
  out_plot <- ggplot(melt(timeser),(aes(x=seq_along(value), y = value))) + 
    geom_line(aes(colour = "Raw"), size = 1) +
    geom_line(data =melt(global_pred),  aes(x=seq_along(value), y = value, colour = "Model"),size = 2) +
    guides(color=guide_legend(title="labels")) + xlab(xlab1) + ylab(paste("Monthly", forecast_on)) +
    labs(title= titlex)
  
  if(is.na(smoothedseries)){
    
    out_plot
    
  }else{
    
    out_plot <- out_plot + 
      geom_line(data =melt(smoothedseries),  aes(x=seq_along(value), y = value, colour = "Smoothed"),size = 1.5)
    out_plot
  }
  
}


#############################################################################################################################

# ----------------------------- TIME SERIES ANALYSIS - 1) EU CONSUMER SALES FORECASTING ---------------------------------- #

#############################################################################################################################


# Segment Name
segment_name <- top2[1]
segment_name


# Arguments for ts_data_prep
segment_EUCS <- 1
forecast_for_EUCS <- 'Sales' 
width_EUCS <- 1


# Calling ts_data_prep function
ts_data_prep(segment = segment_EUCS, forecast_for = forecast_for_EUCS, width = width_EUCS)


# Plotting Smoothed Time Series
smoothedplot_EU_Consumer_Sales


### Model Building -- Classical Decomposition ###


# Modeling trend and seasonal components
lmfit_EU_Consumer_Sales <- lm(`Smoothed Sales` ~ sin(0.5*Month) + poly(Month, 3) , data=smoothed_df_EU_Consumer_Sales)
global_pred_EU_Consumer_Sales <- predict(lmfit_EU_Consumer_Sales, Month=timevals_in_EU_Consumer_Sales)
summary(global_pred_EU_Consumer_Sales)


# Plotting original timeseries, smoothed series along with the model line.
plot_all(timeser = timeser_EU_Consumer_Sales,
         smoothedseries = smoothedseries_EU_Consumer_Sales,
         global_pred = global_pred_EU_Consumer_Sales,
         forecast_on = forecast_for_EU_Consumer_Sales_on,
         segment = segment_name)


# Now lets look at the locally predictable series
local_pred_EU_Consumer_Sales <- timeser_EU_Consumer_Sales - global_pred_EU_Consumer_Sales
plot(local_pred_EU_Consumer_Sales, type = "l")
acf(local_pred_EU_Consumer_Sales)
acf(local_pred_EU_Consumer_Sales, type = "partial")


# We will model it as an ARMA Series through auto.arima function
armafit_EU_Consumer_Sales <- auto.arima(local_pred_EU_Consumer_Sales)
tsdiag(armafit_EU_Consumer_Sales)
armafit_EU_Consumer_Sales


# We'll check if the residual is white noise
resi_EU_Consumer_Sales <- local_pred_EU_Consumer_Sales - fitted(armafit_EU_Consumer_Sales)
plot(resi_EU_Consumer_Sales, type = "l")
adf.test(resi_EU_Consumer_Sales, alternative = "stationary")           # pvalue = 0.01 - Alternate Hypothesis - Stationary
kpss.test(resi_EU_Consumer_Sales)                                      # pvalue = 0.1  - Null Hypothesis      - Stationary
# Both the test results provide enough evidence to consider the residual as white noise


### Evaluating the Classical Decomposition model using MAPE ###


# Making a prediction for the last 6 months
global_pred_out_EU_Consumer_Sales <- predict(lmfit_EU_Consumer_Sales, data.frame(Month = timevals_out_EU_Consumer_Sales))
fcast_Eu_Consumer_Sales <- global_pred_out_EU_Consumer_Sales
fcast_Eu_Consumer_Sales


# Comparing our predictions with actual values using MAPE
MAPE_class_dec_EU_Consumer_Sales <- accuracy(fcast_Eu_Consumer_Sales, EU_Consumer_Sales_out[,3])[5]
MAPE_class_dec_EU_Consumer_Sales # MAPE = 21%, Lower the MAPE -- better the model. Hence this model appears quite justifiable


# Plotting the predictions along with the original values, to get a visual feel of the fit.
class_dec_pred_EU_Consumer_Sales <- c(ts(global_pred_EU_Consumer_Sales), ts(global_pred_out_EU_Consumer_Sales))
smoothedseries_EU_Consumer_Sales_total <- smooth_series(totaltimeser_EU_Consumer_Sales, width = width_EUCS)

plot_all(timeser = totaltimeser_EU_Consumer_Sales,
         smoothedseries = smoothedseries_EU_Consumer_Sales_total,
         global_pred = class_dec_pred_EU_Consumer_Sales,
         forecast_on = forecast_for_EU_Consumer_Sales_on,
         segment = segment_name)


# So, that was classical decomposition, now let's do an ARIMA fit


### Model Building -- Auto Arima ###


# Modelling through auto arima
autoarima_EU_Consumer_Sales <- auto.arima(timeser_EU_Consumer_Sales)
autoarima_EU_Consumer_Sales
tsdiag(autoarima_EU_Consumer_Sales)


# Plotting the model
plot(autoarima_EU_Consumer_Sales$x, col="black")
lines(fitted(autoarima_EU_Consumer_Sales), col="red")


# We'll check if the residual is white noise
resi_auto_arima_EU_Consumer_Sales<- timeser_EU_Consumer_Sales - fitted(autoarima_EU_Consumer_Sales)
adf.test(resi_auto_arima_EU_Consumer_Sales, alternative = "stationary")   # pvalue = 0.01 - Alternate Hypothesis - Stationary
kpss.test(resi_auto_arima_EU_Consumer_Sales)                              # pvalue = 0.1  - Null Hypothesis      - Stationary
# Both the test results provide enough evidence to consider the residual as white noise


### Evaluating the Auto Arima model using MAPE ###


# Making a prediction for the last 6 months
fcast_auto_arima_EU_Consumer_Sales <- predict(autoarima_EU_Consumer_Sales, n.ahead=6)


# Comparing our predictions with actual values using MAPE
MAPE_arima_EU_Consumer_Sales <- accuracy(fcast_auto_arima_EU_Consumer_Sales$pred,EU_Consumer_Sales_out[,3])[5]
MAPE_arima_EU_Consumer_Sales   # MAPE Value = 28%


# Lastly, let's plot the predictions along with original values, to
# get a visual feel of the fit
auto_arima_pred_EU_Consumer_Sales <- c(fitted(autoarima_EU_Consumer_Sales),ts(fcast_auto_arima_EU_Consumer_Sales$pred))

plot_all(timeser = totaltimeser_EU_Consumer_Sales,
         smoothedseries = NA,
         global_pred = auto_arima_pred_EU_Consumer_Sales,
         forecast_on = forecast_for_EU_Consumer_Sales_on,
         segment = segment_name,
         arimamode = TRUE)


### COMPARING CLASSICAL DECOMPOSITION MODEL AND AUTO ARIMA MODEL ### 


# Comparing MAPE Values
MAPE_class_dec_EU_Consumer_Sales    # 21% - Classical Decompostion
MAPE_arima_EU_Consumer_Sales        # 28% - AUTO ARIMA


# Comparig Plots
plot_cde_EU_Consumer_Sales <- plot_all(timeser = totaltimeser_EU_Consumer_Sales,
                                      smoothedseries = smoothedseries_EU_Consumer_Sales_total,
                                      global_pred = class_dec_pred_EU_Consumer_Sales,
                                      forecast_on = forecast_for_EU_Consumer_Sales_on,
                                      segment = segment_name)


plot_arm_EU_Consumer_Sales <- plot_all(timeser = totaltimeser_EU_Consumer_Sales,
                                       smoothedseries = NA,
                                       global_pred = auto_arima_pred_EU_Consumer_Sales,
                                       forecast_on = forecast_for_EU_Consumer_Sales_on,
                                       segment = segment_name,
                                       arimamode = TRUE)


grid.arrange(plot_cde_EU_Consumer_Sales, plot_arm_EU_Consumer_Sales, ncol=2)


# Clearly, through Graphs as well as MAPE Values, we can conclude out the the model prepared manually through classical
# decomposition will lead to better forecasts for this particular market_segment.


#############################################################################################################################

# ----------------------------- TIME SERIES ANALYSIS - 2) EU CONSUMER QUANTITY FORECASTING -------------------------------- #

#############################################################################################################################


# Segment Name
segment_name <- top2[1]
segment_name


# Arguments for ts_data_prep
segment_EUCQ <- 1
forecast_for_EUCQ <- 'Quantity' 
width_EUCQ <- 1

# Calling ts_data_prep function
ts_data_prep(segment = segment_EUCQ, forecast_for = forecast_for_EUCQ, width = width_EUCQ)

# Plotting Smoothed Time Series
smoothedplot_EU_Consumer_Quantity


### Model Building -- Classical Decomposition ###


# Modeling trend and seasonal components
lmfit_EU_Consumer_Quantity <- lm(`Smoothed Quantity` ~ sin(0.5*Month) + poly(Month, 4) , data=smoothed_df_EU_Consumer_Quantity)
global_pred_EU_Consumer_Quantity <- predict(lmfit_EU_Consumer_Quantity, Month=timevals_in_EU_Consumer_Quantity)
summary(global_pred_EU_Consumer_Quantity)


# Plotting original timeseries, smoothed series along with the model line.
plot_all(timeser = timeser_EU_Consumer_Quantity,
         smoothedseries = smoothedseries_EU_Consumer_Quantity,
         global_pred = global_pred_EU_Consumer_Quantity,
         forecast_on = forecast_for_EU_Consumer_Quantity_on,
         segment = segment_name)


# Now lets look at the locally predictable series
local_pred_EU_Consumer_Quantity <- timeser_EU_Consumer_Quantity - global_pred_EU_Consumer_Quantity
plot(local_pred_EU_Consumer_Quantity, type = "l")
acf(local_pred_EU_Consumer_Quantity)
acf(local_pred_EU_Consumer_Quantity, type = "partial")


# We will model it as an ARMA Series through auto.arima function
armafit_EU_Consumer_Quantity <- auto.arima(local_pred_EU_Consumer_Quantity)
tsdiag(armafit_EU_Consumer_Quantity)
armafit_EU_Consumer_Quantity


# We'll check if the residual is white noise
resi_EU_Consumer_Quantity <- local_pred_EU_Consumer_Quantity - fitted(armafit_EU_Consumer_Quantity)
plot(resi_EU_Consumer_Quantity, type = "l")
adf.test(resi_EU_Consumer_Quantity, alternative = "stationary")           # pvalue = 0.02 - Alternate Hypothesis - Stationary
kpss.test(resi_EU_Consumer_Quantity)                                      # pvalue = 0.1  - Null Hypothesis      - Stationary
# Both the test results provide enough evidence to consider the residual as white noise


### Evaluating the Classical Decomposition model using MAPE ###


# Making a prediction for the last 6 months
global_pred_out_EU_Consumer_Quantity <- predict(lmfit_EU_Consumer_Quantity, data.frame(Month = timevals_out_EU_Consumer_Quantity))
fcast_Eu_Consumer_Quantity <- global_pred_out_EU_Consumer_Quantity
fcast_Eu_Consumer_Quantity


# Comparing our predictions with actual values using MAPE
MAPE_class_dec_EU_Consumer_Quantity <- accuracy(fcast_Eu_Consumer_Quantity, EU_Consumer_Quantity_out[,3])[5]
MAPE_class_dec_EU_Consumer_Quantity # MAPE = 22%, Lower the MAPE -- better the model. Hence this model appears quite justifiable


# Plotting the predictions along with the original values, to get a visual feel of the fit.
class_dec_pred_EU_Consumer_Quantity <- c(ts(global_pred_EU_Consumer_Quantity), ts(global_pred_out_EU_Consumer_Quantity))
smoothedseries_EU_Consumer_Quantity_total <- smooth_series(totaltimeser_EU_Consumer_Quantity, width = width_EUCQ)

plot_all(timeser = totaltimeser_EU_Consumer_Quantity,
         smoothedseries = smoothedseries_EU_Consumer_Quantity_total,
         global_pred = class_dec_pred_EU_Consumer_Quantity,
         forecast_on = forecast_for_EU_Consumer_Quantity_on,
         segment = segment_name)

# So, that was classical decomposition, now let's do an ARIMA fit


### Model Building -- Auto Arima ###


# Modelling through auto arima
autoarima_EU_Consumer_Quantity <- auto.arima(timeser_EU_Consumer_Quantity)
autoarima_EU_Consumer_Quantity
tsdiag(autoarima_EU_Consumer_Quantity)


# Plotting the model
plot(autoarima_EU_Consumer_Quantity$x, col="black")
lines(fitted(autoarima_EU_Consumer_Quantity), col="red")


# We'll check if the residual is white noise
resi_auto_arima_EU_Consumer_Quantity<- timeser_EU_Consumer_Quantity - fitted(autoarima_EU_Consumer_Quantity)
adf.test(resi_auto_arima_EU_Consumer_Quantity, alternative = "stationary") # pvalue = 0.04 - Alternate Hypothesis -Stationary
kpss.test(resi_auto_arima_EU_Consumer_Quantity)                            # pvalue = 0.1  - Null Hypothesis      -Stationary
# Both the test results provide enough evidence to consider the residual as white noise


### Evaluating the Auto Arima model using MAPE ###


# Making a prediction for the last 6 months
fcast_auto_arima_EU_Consumer_Quantity <- predict(autoarima_EU_Consumer_Quantity, n.ahead=6)


# Comparing our predictions with actual values using MAPE
MAPE_arima_EU_Consumer_Quantity <- accuracy(fcast_auto_arima_EU_Consumer_Quantity$pred,EU_Consumer_Quantity_out[,3])[5]
MAPE_arima_EU_Consumer_Quantity     # MAPE Value = 30%


# Lastly, let's plot the predictions along with original values, to
# get a visual feel of the fit
auto_arima_pred_EU_Consumer_Quantity <- c(fitted(autoarima_EU_Consumer_Quantity),ts(fcast_auto_arima_EU_Consumer_Quantity$pred))

plot_all(timeser = totaltimeser_EU_Consumer_Quantity,
         smoothedseries = NA,
         global_pred = auto_arima_pred_EU_Consumer_Quantity,
         forecast_on = forecast_for_EU_Consumer_Quantity_on,
         segment = segment_name,
         arimamode = TRUE)


### COMPARING CLASSICAL DECOMPOSITION MODEL AND AUTO ARIMA MODEL ### 


# Comparing MAPE Values
MAPE_class_dec_EU_Consumer_Quantity    # 22% - Classical Decompostion
MAPE_arima_EU_Consumer_Quantity        # 30% - AUTO ARIMA


# Comparig Plots
plot_cde_EU_Consumer_Quantity <- plot_all(timeser = totaltimeser_EU_Consumer_Quantity,
                                          smoothedseries = smoothedseries_EU_Consumer_Quantity_total,
                                          global_pred = class_dec_pred_EU_Consumer_Quantity,
                                          forecast_on = forecast_for_EU_Consumer_Quantity_on,
                                          segment = segment_name)


plot_arm_EU_Consumer_Quantity <- plot_all(timeser = totaltimeser_EU_Consumer_Quantity,
                                          smoothedseries = NA,
                                          global_pred = auto_arima_pred_EU_Consumer_Quantity,
                                          forecast_on = forecast_for_EU_Consumer_Quantity_on,
                                          segment = segment_name,
                                          arimamode = TRUE)


grid.arrange(plot_cde_EU_Consumer_Quantity, plot_arm_EU_Consumer_Quantity, ncol=2)


# Clearly, through Graphs as well as MAPE Values, we can conclude out the the model prepared manually through classical
# decomposition will lead to better forecasts for this particular market_segment.


#############################################################################################################################

# ----------------------------- TIME SERIES ANALYSIS - 3) APAC CONSUMER SALES FORECASTING --------------------------------- #

#############################################################################################################################


# Segment Name
segment_name <- top2[2]
segment_name


# Arguments for ts_data_prep
segment_APACS <- 2
forecast_for_APACS <- 'Sales' 
width_APACS <- 1


# Calling ts_data_prep function
ts_data_prep(segment = segment_APACS, forecast_for = forecast_for_APACS, width = width_APACS)


# Plotting Smoothed Time Series
smoothedplot_APAC_Consumer_Sales


### Model Building -- Classical Decomposition ###

# Modeling trend and seasonal components
lmfit_APAC_Consumer_Sales <- lm(`Smoothed Sales` ~ cos(0.6*Month) + poly(Month, 3) * sin(0.19*Month) , data=smoothed_df_APAC_Consumer_Sales)
global_pred_APAC_Consumer_Sales <- predict(lmfit_APAC_Consumer_Sales, Month=timevals_in_APAC_Consumer_Sales)
summary(global_pred_APAC_Consumer_Sales)


# Plotting original timeseries, smoothed series along with the model line.
plot_all(timeser = timeser_APAC_Consumer_Sales,
         smoothedseries = smoothedseries_APAC_Consumer_Sales,
         global_pred = global_pred_APAC_Consumer_Sales,
         forecast_on = forecast_for_APAC_Consumer_Sales_on,
         segment = segment_name)


# Now lets look at the locally predictable series
local_pred_APAC_Consumer_Sales <- timeser_APAC_Consumer_Sales - global_pred_APAC_Consumer_Sales
plot(local_pred_APAC_Consumer_Sales, type = "l")
acf(local_pred_APAC_Consumer_Sales)
acf(local_pred_APAC_Consumer_Sales, type = "partial")


# We will model it as an ARMA Series through auto.arima function
armafit_APAC_Consumer_Sales <- auto.arima(local_pred_APAC_Consumer_Sales)
tsdiag(armafit_APAC_Consumer_Sales)
armafit_APAC_Consumer_Sales


# We'll check if the residual is white noise (Stationary)
resi_APAC_Consumer_Sales <- local_pred_APAC_Consumer_Sales - fitted(armafit_APAC_Consumer_Sales)
plot(resi_APAC_Consumer_Sales, type = "l")
adf.test(resi_APAC_Consumer_Sales, alternative = "stationary")           # pvalue = 0.01 - Alternate Hypothesis - Stationary
kpss.test(resi_APAC_Consumer_Sales)                                      # pvalue = 0.1  - Null Hypothesis      - Stationary
# Both the test results provide enough evidence to consider the residual as white noise


### Evaluating the Classical Decomposition model using MAPE ###


# Making a prediction for the last 6 months
global_pred_out_APAC_Consumer_Sales <- predict(lmfit_APAC_Consumer_Sales, data.frame(Month = timevals_out_APAC_Consumer_Sales))
fcast_APAC_Consumer_Sales <- global_pred_out_APAC_Consumer_Sales
fcast_APAC_Consumer_Sales


# Comparing our predictions with actual values using MAPE
MAPE_class_dec_APAC_Consumer_Sales <- accuracy(fcast_APAC_Consumer_Sales, APAC_Consumer_Sales_out[,3])[5]
MAPE_class_dec_APAC_Consumer_Sales # MAPE = 24%, Lower the MAPE -- better the model. Hence this model appears quite justifiable


# Plotting the predictions along with the original values, to get a visual feel of the fit.
class_dec_pred_APAC_Consumer_Sales <- c(ts(global_pred_APAC_Consumer_Sales), ts(global_pred_out_APAC_Consumer_Sales))
smoothedseries_APAC_Consumer_Sales_total <- smooth_series(totaltimeser_APAC_Consumer_Sales, width = width_APACS)

plot_all(timeser = totaltimeser_APAC_Consumer_Sales,
         smoothedseries = smoothedseries_APAC_Consumer_Sales_total,
         global_pred = class_dec_pred_APAC_Consumer_Sales,
         forecast_on = forecast_for_APAC_Consumer_Sales_on,
         segment = segment_name)

# So, that was classical decomposition, now let's do an ARIMA fit


### Model Building -- Auto Arima ###


# Modelling through auto arima
autoarima_APAC_Consumer_Sales <- auto.arima(timeser_APAC_Consumer_Sales)
autoarima_APAC_Consumer_Sales
tsdiag(autoarima_APAC_Consumer_Sales)


# Plotting the model
plot(autoarima_APAC_Consumer_Sales$x, col="black")
lines(fitted(autoarima_APAC_Consumer_Sales), col="red")


# We'll check if the residual is white noise
resi_auto_arima_APAC_Consumer_Sales <- timeser_APAC_Consumer_Sales - fitted(autoarima_APAC_Consumer_Sales)
adf.test(resi_auto_arima_APAC_Consumer_Sales, alternative = "stationary") # pvalue = 0.01 - Alternate Hypothesis - Stationary
kpss.test(resi_auto_arima_APAC_Consumer_Sales)                            # pvalue = 0.1  - Null Hypothesis      - Stationary
# Both the test results provide enough evidence to consider the residual as white noise


### Evaluating the Auto Arima model using MAPE ###


# Making a prediction for the last 6 months
fcast_auto_arima_APAC_Consumer_Sales <- predict(autoarima_APAC_Consumer_Sales, n.ahead=6)


# Comparing our predictions with actual values using MAPE
MAPE_arima_APAC_Consumer_Sales <- accuracy(fcast_auto_arima_APAC_Consumer_Sales$pred, APAC_Consumer_Sales_out[,3])[5]
MAPE_arima_APAC_Consumer_Sales  # MAPE Value = 27%


# Lastly, let's plot the predictions along with original values, to
# get a visual feel of the fit
auto_arima_pred_APAC_Consumer_Sales <- c(fitted(autoarima_APAC_Consumer_Sales),ts(fcast_auto_arima_APAC_Consumer_Sales$pred))

plot_all(timeser = totaltimeser_APAC_Consumer_Sales,
         smoothedseries = NA,
         global_pred = auto_arima_pred_APAC_Consumer_Sales,
         forecast_on = forecast_for_APAC_Consumer_Sales_on,
         segment = segment_name,
         arimamode = TRUE)


### COMPARING CLASSICAL DECOMPOSITION MODEL AND AUTO ARIMA MODEL ### 


# Comparing MAPE Values
MAPE_class_dec_APAC_Consumer_Sales  # 24% - Classical Decompostion
MAPE_arima_APAC_Consumer_Sales      # 27% - AUTO ARIMA


# Comparig Plots
plot_cde_APAC_Consumer_Sales <- plot_all(timeser = totaltimeser_APAC_Consumer_Sales,
                                         smoothedseries = smoothedseries_APAC_Consumer_Sales_total,
                                         global_pred = class_dec_pred_APAC_Consumer_Sales,
                                         forecast_on = forecast_for_APAC_Consumer_Sales_on,
                                         segment = segment_name)


plot_arm_APAC_Consumer_Sales <- plot_all(timeser = totaltimeser_APAC_Consumer_Sales,
                                         smoothedseries = NA,
                                         global_pred = auto_arima_pred_APAC_Consumer_Sales,
                                         forecast_on = forecast_for_APAC_Consumer_Sales_on,
                                         segment = segment_name,
                                         arimamode = TRUE)


grid.arrange(plot_cde_APAC_Consumer_Sales, plot_arm_APAC_Consumer_Sales, ncol=2)


# Clearly, through Graphs as well as MAPE Values, we can conclude out the the model prepared manually through classical
# decomposition will lead to better forecasts for this particular market_segment.


#############################################################################################################################

# ---------------------------- TIME SERIES ANALYSIS - 4) APAC CONSUMER QUANTITY FORECASTING ------------------------------- #

#############################################################################################################################


# Segment Name
segment_name <- top2[2]
segment_name


# Arguments for ts_data_prep
segment_APACQ <- 2
forecast_for_APACQ <- 'Quantity' 
width_APACQ <- 1


# Calling ts_data_prep function
ts_data_prep(segment = segment_APACQ, forecast_for = forecast_for_APACQ, width = width_APACQ)


# Plotting Smoothed Time Series
smoothedplot_APAC_Consumer_Quantity


### Model Building -- Classical Decomposition ###

# Modeling trend and seasonal components
lmfit_APAC_Consumer_Quantity <- lm(`Smoothed Quantity` ~ sin(0.5*Month) + poly(Month, 3.5), data=smoothed_df_APAC_Consumer_Quantity)
global_pred_APAC_Consumer_Quantity <- predict(lmfit_APAC_Consumer_Quantity, Month=timevals_in_APAC_Consumer_Quantity)
summary(global_pred_APAC_Consumer_Quantity)


# Plotting original timeseries, smoothed series along with the model line.
plot_all(timeser = timeser_APAC_Consumer_Quantity,
         smoothedseries = smoothedseries_APAC_Consumer_Quantity,
         global_pred = global_pred_APAC_Consumer_Quantity,
         forecast_on = forecast_for_APAC_Consumer_Quantity_on,
         segment = segment_name)


# Now lets look at the locally predictable series
local_pred_APAC_Consumer_Quantity <- timeser_APAC_Consumer_Quantity - global_pred_APAC_Consumer_Quantity
plot(local_pred_APAC_Consumer_Quantity, type = "l")
acf(local_pred_APAC_Consumer_Quantity)
acf(local_pred_APAC_Consumer_Quantity, type = "partial")


# We will model it as an ARMA Series through auto.arima function
armafit_APAC_Consumer_Quantity <- auto.arima(local_pred_APAC_Consumer_Quantity)
tsdiag(armafit_APAC_Consumer_Quantity)
armafit_APAC_Consumer_Quantity


# We'll check if the residual is white noise
resi_APAC_Consumer_Quantity <- local_pred_APAC_Consumer_Quantity - fitted(armafit_APAC_Consumer_Quantity)
plot(resi_APAC_Consumer_Quantity, type = "l")
adf.test(resi_APAC_Consumer_Quantity, alternative = "stationary")           # pvalue = 0.01 - Alternate Hypothesis - Stationary
kpss.test(resi_APAC_Consumer_Quantity)                                      # pvalue = 0.1  - Null Hypothesis      - Stationary
# Both the test results provide enough evidence to consider the residual as white noise


### Evaluating the Classical Decomposition model using MAPE ###


# Making a prediction for the last 6 months
global_pred_out_APAC_Consumer_Quantity <- predict(lmfit_APAC_Consumer_Quantity, data.frame(Month = timevals_out_APAC_Consumer_Quantity))
fcast_APAC_Consumer_Quantity <- global_pred_out_APAC_Consumer_Quantity
fcast_APAC_Consumer_Quantity


# Comparing our predictions with actual values using MAPE
MAPE_class_dec_APAC_Consumer_Quantity <- accuracy(fcast_APAC_Consumer_Quantity, APAC_Consumer_Quantity_out[,3])[5]
MAPE_class_dec_APAC_Consumer_Quantity # MAPE = 20%, Lower the MAPE -- better the model. Hence this model appears quite justifiable


# Plotting the predictions along with the original values, to get a visual feel of the fit.
class_dec_pred_APAC_Consumer_Quantity <- c(ts(global_pred_APAC_Consumer_Quantity), ts(global_pred_out_APAC_Consumer_Quantity))
smoothedseries_APAC_Consumer_Quantity_total <- smooth_series(totaltimeser_APAC_Consumer_Quantity, width = width_APACQ)

plot_all(timeser = totaltimeser_APAC_Consumer_Quantity,
         smoothedseries = smoothedseries_APAC_Consumer_Quantity_total,
         global_pred = class_dec_pred_APAC_Consumer_Quantity,
         forecast_on = forecast_for_APAC_Consumer_Quantity_on,
         segment = segment_name)

# So, that was classical decomposition, now let's do an ARIMA fit


### Model Building -- Auto Arima ###


# Modelling through auto arima
autoarima_APAC_Consumer_Quantity <- auto.arima(timeser_APAC_Consumer_Quantity)
autoarima_APAC_Consumer_Quantity
tsdiag(autoarima_APAC_Consumer_Quantity)


# Plotting the model
plot(autoarima_APAC_Consumer_Quantity$x, col="black")
lines(fitted(autoarima_APAC_Consumer_Quantity), col="red")


# We'll check if the residual is white noise
resi_auto_arima_APAC_Consumer_Quantity <- timeser_APAC_Consumer_Quantity - fitted(autoarima_APAC_Consumer_Quantity)
adf.test(resi_auto_arima_APAC_Consumer_Quantity, alternative = "stationary") # pvalue = 0.01 - Alternate Hypothesis - Stationary
kpss.test(resi_auto_arima_APAC_Consumer_Quantity)                            # pvalue = 0.1  - Null Hypothesis      - Stationary
# Both the test results provide enough evidence to consider the residual as white noise


### Evaluating the Auto Arima model using MAPE ###


# Making a prediction for the last 6 months
fcast_auto_arima_APAC_Consumer_Quantity <- predict(autoarima_APAC_Consumer_Quantity, n.ahead=6)


# Comparing our predictions with actual values using MAPE
MAPE_arima_APAC_Consumer_Quantity <- accuracy(fcast_auto_arima_APAC_Consumer_Quantity$pred, APAC_Consumer_Quantity_out[,3])[5]
MAPE_arima_APAC_Consumer_Quantity  # MAPE Value = 26%


# Lastly, let's plot the predictions along with original values, to
# get a visual feel of the fit
auto_arima_pred_APAC_Consumer_Quantity <- c(fitted(autoarima_APAC_Consumer_Quantity),ts(fcast_auto_arima_APAC_Consumer_Quantity$pred))

plot_all(timeser = totaltimeser_APAC_Consumer_Quantity,
         smoothedseries = NA,
         global_pred = auto_arima_pred_APAC_Consumer_Quantity,
         forecast_on = forecast_for_APAC_Consumer_Quantity_on,
         segment = segment_name,
         arimamode = TRUE)


### COMPARING CLASSICAL DECOMPOSITION MODEL AND AUTO ARIMA MODEL ### 


# Comparing MAPE Values
MAPE_class_dec_APAC_Consumer_Quantity  # 20% - Classical Decompostion
MAPE_arima_APAC_Consumer_Quantity      # 26% - AUTO ARIMA


# Comparig Plots
plot_cde_APAC_Consumer_Quantity <- plot_all(timeser = totaltimeser_APAC_Consumer_Quantity,
                                            smoothedseries = smoothedseries_APAC_Consumer_Quantity_total,
                                            global_pred = class_dec_pred_APAC_Consumer_Quantity,
                                            forecast_on = forecast_for_APAC_Consumer_Quantity_on,
                                            segment = segment_name)


plot_arm_APAC_Consumer_Quantity <- plot_all(timeser = totaltimeser_APAC_Consumer_Quantity,
                                            smoothedseries = NA,
                                            global_pred = auto_arima_pred_APAC_Consumer_Quantity,
                                            forecast_on = forecast_for_APAC_Consumer_Quantity_on,
                                            segment = segment_name,
                                            arimamode = TRUE)


grid.arrange(plot_cde_APAC_Consumer_Quantity, plot_arm_APAC_Consumer_Quantity, ncol=2)


# Clearly, through Graphs as well as MAPE Values, we can conclude out the the model prepared manually through classical
# decomposition will lead to better forecasts for this particular market_segment. Also, the autoarima model appears to be
# fitting better on the input data, but thats the case of overfitting, as it doesnot fit that well on output data.


#############################################################################################################################

## ======================================================== END ========================================================== ##

#############################################################################################################################




