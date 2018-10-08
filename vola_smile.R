library(dplyr)
library(rusquant)
library(TTR)
library(forecast)
library(purrr)
library(PerformanceAnalytics)
library(quantmod)
library(rugarch)
library(magrittr)

vola_funny.smile(tickers           = c("SPFB.Si", "SPFB.BR"),
                 dateFrom         = "2018-01-01",
                 periodPerYear    = 252,
                 volaPeriod       = 3,
                 volaPeriod_smile = 2,
                 forc_period      = 5,
                 estimators       = c("yang.zhang", "close"))



vola_funny.smile <- function(tickers, dateFrom, periodPerYear, volaPeriod, 
                             volaPeriod_smile, forc_period, estimators) {
  ## import ========================
  
  for (i in seq_along(tickers)){
    getSymbols(tickers[i], src="Finam", from = dateFrom, period = "day")
    Sys.sleep(5)
  }
  tickers_toupper <- lapply(tickers, function(x) toupper(x)) %>% unlist()
  
  ## functions =====================
  vola <- function(ticker, n = volaPeriod, N = periodPerYear, calc = calc){
    volatility(get(ticker), n = n, N = N, calc = calc)
  }
  
  # function to set names
  set_nms <- function(){
    conf_intervals = c("Y", "L80", "H80", "L95", "H95") 
    model_names = c("faf", "fa", "VV")
    names = paste(paste(unlist(lapply(model_names, function(x) rep(x, 5))), conf_intervals, sep = "."))
    estimators %>% lapply(., function(y) paste(names, y, sep = "_")) %>% unlist()
  }
  
  # funny_vola function
  funny_vola <- function(estimators){
    cbind(
      vola(ticker = ticker, n = volaPeriod, N = periodPerYear, calc = estimators) %>% 
        arfima(lambda = BoxCox.lambda(.),biasadj = TRUE) %>% 
        forecast(h = forc_period,  robust = TRUE) %>% data.frame(.),
      
      vola(ticker = ticker, n = volaPeriod, N = periodPerYear, calc = estimators) %>%
        auto.arima() %>% 
        forecast(h = forc_period) %>% 
        data.frame(.),
      
      vola(ticker = ticker, n = volaPeriod, N = periodPerYear, calc = estimators) %>%
        volatility(n = volaPeriod, N = periodPerYear, calc = "close") %>% 
        auto.arima() %>% 
        forecast(h = forc_period) %>% data.frame(.)
    )
  } # end funny_vola function
  
  # vola_smile function
  vola_smile <- function(tckr){
    for (k in 1:length(tckr)){
      
      ticker <- tckr[k]
      
      arma_for_spec <- vola(ticker, n = volaPeriod_smile, N = periodPerYear, calc = "yang.zhang") %>% 
        arfima(lambda = BoxCox.lambda(.), biasadj = TRUE)
      
      realizedVol <- ticker %>% vola(n = 2, N = 1, calc = "yang.zhang") %>% na.omit() %$% . * 100
      data_returns <- get(ticker) %>% Cl() %>% CalculateReturns() %$% .[as.POSIXlt(realizedVol)] * 100
      
      
      spec = ugarchspec(mean.model = list(armaOrder = c(length(arma_for_spec$ar), length(arma_for_spec$ma)), 
                                          include.mean = TRUE,  arfima = TRUE), 
                        variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)),
                        distribution.model = "norm")
      
      setbounds(spec)<-list(alpha2 = c(-1,1))
      
      ## fit & plot
      fit = ugarchfit(spec, data = data_returns, out.sample = 25, 
                      solver = 'hybrid', realizedVol = realizedVol)
      
      #set.seed(55)
      forc = ugarchforecast(fit, n.ahead = 400, n.sim = 5000)
      plot(sigma(forc)*periodPerYear^0.5, type = 'l', main = paste('realGARCH long-run forecast', ticker))
      abline(h = sqrt(uncvariance(fit))*periodPerYear^0.5, col = 2)
      legend('topright', c('long-run forecast', 'unconditional value'), col = 1:2, lty = c(1, 1), bty = 'n')
      un <- (uncvariance(fit)*periodPerYear)^0.5
      
      #set.seed(55)
      forc1 = ugarchforecast(fit, n.ahead = 25, n.sim = 10000)
      
      sigmaDF = forc1@forecast$sigmaDF
      meansig = sqrt(exp(rowMeans(log(sigmaDF[, , 1]^2))))
      boxplot(t(sigmaDF[, , 1])*periodPerYear^0.5, 
              main = paste('25-ahead volatility forecast (realGARCH)', ticker), col = 'orange')
      points(as.numeric(meansig)*periodPerYear^0.5, col = 'green')
      
      # note that for the 1-ahead there is no uncertainty (unless we were doing this Bayes-style 
      # so that parameter uncertainty would have an impact).
      # describe(abs(returns)*252^0.5*100)
      sigmaDF_summary <- summary(t(sigmaDF[, , 1])*periodPerYear^0.5)
      
      ## fit2 & plot
      fit2 = ugarchfit(spec, data = data_returns, solver = 'hybrid', realizedVol = realizedVol)
      ni = newsimpact(fit2, z = seq(-3, 3, length.out = 100))
      plot(ni$zx, ni$zy, ylab = ni$yexpr, xlab = ni$xexpr, type = 'l', 
           main = paste('News Impact realGARCH', ticker))
      abline(v = 0)
      abline(h = 0)
      grid()
      
      return(  data.frame("Predict + 25" = meansig[25]*periodPerYear^0.5, 
                          "Quantile"     = sigmaDF_summary[c(2,5),25],
                          
                          "Mean"         = vola(ticker, n = volaPeriod_smile, 
                                                N = periodPerYear, 
                                                calc = "yang.zhang") %>% 
                                                na.omit() %>% mean() * 100,
                          "Uncov"        = un,
                          "Kurtosis"     = kurtosis(c(ni$zy,ni$zx)),
                          "Skewness"     = skewness(c(ni$zy,ni$zx) )
                          
      )) # end return
    } # end 'for' loop
    
  } #end vola_smile function
  #===== END FUNCTIONS ==  
  
  #### run functions in 'for' loop by tickers =======
  
  for (tic in seq_along(tickers_toupper)){
    ticker = tickers_toupper[tic]
    
    # creating funny_vola tables 
    assign(paste(ticker, "vola", sep = "_"),
           estimators %>% map(~ funny_vola(.x)) %>% do.call(cbind, .) %>% setNames(., set_nms()), 
           envir = parent.frame())
    
    #### creating vola_smile tables & plots
    assign(paste(ticker, "smile", sep = "_"),vola_smile(tckr = ticker), envir = parent.frame())
    
  } # end 'for' loop
  
} # end vola_funny.smile function
