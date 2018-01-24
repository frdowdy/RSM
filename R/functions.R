packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

apply_transform<-function(x){
  require(tidyverse)
  require(caret)
  trans = preProcess(data.frame(x), c("BoxCox", "center", "scale"))
  data_trans = data.frame(trans = predict(trans, data.frame(x)))
  colnames(data_trans) <- str_replace_all(colnames(data_trans),"[.]","_")
  return(data_trans)
}

alternativeFunction <- function(){
  newdat = data.frame(temp = seq(min(rsm_hist$temp),max(rsm_hist$temp),length.out = 100))
  newdat$pred = predict(model, newdata = newdat)
  plot(unlist(rsm_hist[dfVariable$variable[i]]) ~ temp, data = rsm_hist, main = paste(dfVariable$variable[i],"excluding validation points (AIC model)"),xlab="Temperature",ylab=dfVariable$variable[i])
  with(newdat, lines(x = temp, y = pred))
}

rsm_and_visualize <- function(data = "data",response_cols = "response_cols"){
  require(tidyverse)
  require(rsm)
  # without validation points, then stepped with AIC
  dfVariable <- data %>% select(mfc,type,temp,acetate,response_cols) %>% 
    filter(type!="validation") %>% 
    gather("variable","value",c(5:ncol(.))) %>% 
    group_by(variable) %>% 
    do(fitVariable = step(trace=0,lm(value ~ temp+acetate+I(temp^2)+I(acetate^2)+temp:acetate, data = .)))
    # get the coefficients by group in a tidy data_frame
  dfVariableCoef = tidy(dfVariable, fitVariable)
  dfVariableCoef
    # # get the predictions by group in a tidy data_frame 
  dfVariablePred = augment(dfVariable,fitVariable)
  dfVariablePred
    # get the summary statistics by group in a tidy data_frame
  dfVariableSumm = glance(dfVariable, fitVariable)
  dfVariableSumm
    # get all significant models
  dfVariableSumm %>% filter(p.value<=.05) %>% select(variable) %>% unique() %>% as.matrix() %>% match(.,dfVariable$variable) -> sig_models
  dfVariable$variable[sig_models]
     
  
  # loop it and provide titles - looks like it doesn't know what to do when acetate coefficients are undefined
   for(i in sig_models){
    dfVariable$fitVariable[i] %>% unlist(recursive = FALSE) %>% structure(class = "lm") -> model
    t <- try(
      persp(model, ~ temp + acetate, col = rainbow(50), contours = "colors", main = paste(dfVariable$variable[i],"excluding validation points (AIC model)")))
    if("try-error" %in% class(t)) 
      alternativeFunction()
   }
  list(dfVariable = dfVariable,dfVariableCoef = dfVariableCoef,dfVariablePred = dfVariablePred,dfVariableSumm = dfVariableSumm)
}