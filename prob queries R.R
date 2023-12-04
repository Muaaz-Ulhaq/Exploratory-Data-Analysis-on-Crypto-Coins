ethereum <- c(2841.39,2829.07,2859.94,2784.92,2943.68,2751.91,2697.75,2639.72,2523.54,2239.57,2344.88,2076.23,1958.58,2012.03,2058.87,2147.95,2031.38,2107.88,1929.12,2039.07,1981.41,1995.85,2061.15,1991.82,1992.11,1955.96,1809.49,1733.17,1814.63,1821.12)
bitcoin<-c(29468,29031,28606,29195,29555,29655,29096,30296,29445,29205,30320,28697,30446,29833,31293,30077,29290,29012,29044,31001,30106,34063,35479,36020,36572,39690,37724,38510,38466,37636)
doge<-c(0.082674,0.081897,0.081410,0.077908,0.082907,0.083591,0.083254,0.085958,0.084483,0.083877,0.086705,0.083287,0.090201,0.087769,0.093033,0.089475,0.088141,0.082477,0.084586,0.108048,0.102331,0.124201,0.127471,0.127813,0.128165,0.135930,0.129468,0.130800,0.132732,0.127466)

summary(ethereum)
summary(bitcoin)
summary(doge)


sdE = sd(ethereum)
sdB = sd(bitcoin)
sdD = sd(doge)


hist(ethereum)
hist(bitcoin)
hist(doge)

eMean = mean(ethereum)
bMean = mean(bitcoin)
dMean = mean(doge)

eLast= 1821.12
bLast = 37636
dLast = 0.127466

pLessE =  (eLast - eMean) / sdE
print(pLessE)

pLessB =  (bLast - bMean) / sdB
print(pLessB)

pLessD =  (dLast - dMean) / sdD
print(pLessD)

v =pdf_text("https://www.math.arizona.edu/~rsims/ma464/standardnormaltable.pdf")
print(v)

mCap_e<-c(339.66,335.41,342.23,340.95,344.55,345.57,327.57,323.44,308.67,289.67,284.99,275.65,235.93,250.62,244.10,249.74,247.09,250.4,244.86,240.14,243.87,240.32,244.25,248.71,240.13,240.24,228.50,214.59,215.76,218.96)
mCap_b<-c(583.25,555.23,550.40,550.55,561.11,566.96,558.38,573.31,567.57,558.93,567.83,562.87,563.94,576.73,570.95,573.62,559.87,574.59,543.92,582.28,596.37,621.85,657.63,682.60,688.13,731.32,738.71,728.00,736.16,723.84)
mCap_d<-c(11.29,10.84,10.86,10.50,10.60,11.03,11.04,11.43,11.31,11.17,11.34,11.28,11.60,11.88,11.70,11.82,11.71,11.99,10.60,12.95,14.75,15.48,16.67,17.02,16.89,17.59,17.45,17.27,17.42,17.41)

eCmean= mean(mCap_e)
bCmean= mean(mCap_b)
dCmean= mean(mCap_d)
xC = c(eCmean, bCmean, dCmean)
xC
labels= c("Ethereum", "Bitcoin","Doge")
pie(xC, labels)

mCapEBill <- mCap_e * 1000000000
mCapBBill <- mCap_b * 1000000000
mCapDBill <- mCap_d * 1000000000

eCoins = mCapEBill/mCap_e
eCoins
bCoins = mCapBBill/mCap_b
bCoins
dCoins = mCapDBill/mCap_d
dCoins


cor(ethereum,mCapEBill)
cor(bitcoin,mCapBBill)
cor(doge,mCapDBill)

cov(ethereum,mCapEBill)
cov(bitcoin,mCapBBill)
cov(doge,mCapDBill)

plot(ethereum,mCap_e)
plot(bitcoin, mCap_b)
plot(doge, mCap_b)

linearRegE<-lm(eCoins~ethereum)
print(linearRegE)


linearRegB<-lm(bCoins~bitcoin)
print(linearRegB)


linearRegD<-lm(dCoins~doge)
print(linearRegD)


