# predict.BC.SLR : BC using SLR

predict.BC.SLR<-function(rfobj,train.data,test.data=train.data){
  
  #this function predict RF regression with bias correction
  #the idea of bias correction is simple.
  #we fit SLR for predicted y vs observed y
  #bias_corrected_yhat=a+b*yhat
  #so when new data (test data) comes, get the fitted value first and 
  #get the fitted value using SLR
  
  #first, fit yhat on y using SLR and get the coeffs
  rf1.pred<-predict(rfobj,train.data)
  rf1.orgy<-rf1$y
  lm1<-lm(rf1.orgy~rf1.pred)$coeff
  #print(lm1)
  
  #next, get the fitted value of RF for test data
  rf1testdata.pred<-predict(rfobj,test.data)
  rf1.testdata.bcpred<-lm1[1]+lm1[2]*rf1testdata.pred
  return(rf1.testdata.bcpred)#first col is original RF, second col is BCRF estimates
}