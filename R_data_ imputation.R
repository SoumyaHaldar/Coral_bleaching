oxy_ts<-ts(Oxygen, start=c(2002,1), end=c(2017, 12), frequency=12)
oxy_imp<-na_kalman(oxy_ts)
talk_ts<-ts(talk, start=c(2002,1), end=c(2017, 12), frequency=12)
talk_imp<-na_kalman(talk_ts)
ggplot_na_imputations(oxy_ts,oxy_imp)#### plot imputed data and original data