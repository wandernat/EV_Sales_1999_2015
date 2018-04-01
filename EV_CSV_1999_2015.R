#---- Import the data set ----#
library(readr)
library(rstudioapi)
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
ev_csv <- read_csv(paste(path, '/table_01_19_1.csv', sep=""), col_names=FALSE)

options(scipen=999)

#---- Remove unnecessary rows ----#

ev_csv_1 <- ev_csv[-1, ]
ev_csv_2 <- ev_csv_1[-c(5:13), ]

#---- Remove the "U" strings, replace with "0" ----#

library(stringr)

for (i in 2:ncol(ev_csv_2)) {
  ev_csv_2[[i]] <- str_replace(ev_csv_2[[i]], "U", "0")
}

#-----Transpose because the data set is wide with years as columns ----#

ev_csv_3 <- as.data.frame(t(ev_csv_2), stringsAsFactors = FALSE)

#---- Set colnames and remove rownames ---#

colnames(ev_csv_3) <- as.character(unlist(ev_csv_3[1, ]))
ev_csv_3 = ev_csv_3[-1, ]

colnames(ev_csv_3) <- c("YEAR", "HEV", "PHEV", "EV")

rownames(ev_csv_3) <- NULL

#---- Convert the data in sales columns from character to numeric, removing comma along the way ----#

for (i in 2:ncol(ev_csv_3)) {
  ev_csv_3[[i]] <- as.numeric(str_replace(ev_csv_3[[i]], ",", ""))
}

#---- PLOTTING TIME ----#

library(ggplot2)

#----Plotting hybrid (HEV) sales agaainst YEAR ----#

ggplot(data = ev_csv_3, aes(x = as.numeric(YEAR), y = HEV)) + 
  geom_line() +
  ggtitle("Hybrid car sales by year from 1999 to 2015") +
  xlab("YEAR")

#----Plotting plug-in hybrid electric (PHEV) sales agaainst YEAR ----#

ggplot(data = ev_csv_3, aes(x = as.numeric(YEAR), y = PHEV)) + 
  geom_line() +
  ggtitle("Plugin hybrid electric car sales by year from 1999 to 2015") +
  xlab("YEAR")

#----Plotting battery-only hybrid electric (EV) sales agaainst YEAR ----#

ggplot(data = ev_csv_3, aes(x = as.numeric(YEAR), y = EV)) + 
  geom_line() +
  ggtitle("Battery-only electric car sales by year from 1999 to 2015") +
  xlab("YEAR")

#---- Attempting to plot all 3 on the same graph ----#

ggplot(data = ev_csv_3, aes(x = as.numeric(YEAR))) +
  geom_line(aes(y = HEV, col = "Hybrid")) +
  geom_line(aes(y = PHEV, col = "Plugin hybrid electric")) +
  geom_line(aes(y = EV, col = "Battery only electric")) +
  labs(title = "Hybrid and electric car sales 1999-2015", 
       subtitle = "Source:  U.S. Department of Energy") + 
  xlab("YEAR")