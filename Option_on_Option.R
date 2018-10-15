library(dplyr)
library(rusquant)
library(forecast)


Option_on_Option( tickers     = c("SPFB.Si", "SPFB.BR"),
                  dateFrom    = '2018-01-01',
                  volaPeriod  = 5,
                  n_for_VV    = 3,
                  forc_period = 5 )



Option_on_Option <- function(tickers, dateFrom, volaPeriod, n_for_VV, forc_period) {
  ## import ========================
  
  for (i in seq_along(tickers)){
    getSymbols(tickers[i], src="Finam", from = dateFrom, period = "day")
    Sys.sleep(5)
  }
  tickers_toupper <- lapply(tickers, function(x) toupper(x)) %>% unlist()
  
  ## functions =====================
  vola <- function(ticker, n, calc){
    volatility(get(ticker), n = n, N = 252, calc = calc)
  }
  
  for (tic in seq_along(tickers_toupper)) {
    
    ticker = tickers_toupper[tic]
    # target value
    Y <- vola(ticker = tickers_toupper, n = volaPeriod, calc = "yang.zhang")
    
    assign( paste('X', ticker, sep = '_'),
            cbind(
              Y %>% 
                arfima(lambda = BoxCox.lambda(.),biasadj = TRUE) %>% 
                forecast(h = forc_period,  robust = TRUE) %>% 
                data.frame(.) %>%
                rename(faf = names(.)[1]),
              
              Y %>%
                auto.arima() %>% 
                forecast(h = forc_period) %>% 
                data.frame(.) %>% 
                rename(fa = names(.)[1]),
              
              Y %>%
                volatility(n = n_for_VV, N = 252, calc = "close") %>% 
                auto.arima() %>% 
                forecast(h = forc_period) %>% data.frame(.) %>% 
                rename(VV = names(.)[1])
            ), # end cbind
            envir = parent.frame()
    ) # end assign
    
  } # end 'for' loop
  
} # end function   