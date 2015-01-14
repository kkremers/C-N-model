############SENSITIVITY ANALYSIS USING LME PACKAGE###############

sensvars = c("Biomass_C", 
             "Biomass_N", 
             "Litter_C", 
             "Litter_N", 
             "SOM_C", 
             "SOM_N",
             "Available_N",
             "GPP",
             "NEE",
             "Re",
             "LAI")


#local sensitivity analysis
s.local <- sensFun(func=solvemodel, parms=params, senspar = names(params), 
                   sensvar = sensvars, varscale = 1)

head(s.local)
s.local.summ = summary(s.local, var=T)
s.loc.summ.ordered = s.local.summ[order(s.local.summ$var, abs(s.local.summ$Mean)),] 
write.csv(s.loc.summ.ordered, "c:/Users/Rocha Lab/Desktop/Kelsey/LocalSensitivityAnalysis.csv") #univariate sensitivity
param.cor = data.frame(cor(s.local[,c(-1,-2)]))#table of parameter correlations
param.cor
write.csv(param.cor, "c:/Users/Rocha Lab/Desktop/Kelsey/ParamCorr.csv") #bivariate sensitivity
pairs(s.local)


#global sensitivity analysis

#alter all params by 20%
parms = as.vector(unlist(params))
paramsperc =parms*0.5
params.min =  parms - paramsperc
params.max = parms + paramsperc
parRanges = data.frame(min = params.min,  max = params.max)
rownames(parRanges) = names(params)
parRanges

s.global <- sensRange(func=solvemodel, parms=params, dist ="unif", 
                      sensvar = sensvars, parRange=parRanges, num=50)

s.global.summ = summary(s.global)
head(s.global.summ)
#plots 
par(mfrow=c(3,2)) 
plot(s.global.summ, xlab = "Time (days)", mfrow = NULL,
     quant = TRUE, col = c("lightblue", "darkblue"), legpos = "topright")
