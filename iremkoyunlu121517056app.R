#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(olsrr)
library(lmtest)
library(readr)
library(corrplot)
library(car)
library(ggplot2) 
library(faraway)
library(MASS)
NSTS=read.csv("C:/Users/CASPER/Desktop/infy_stock.csv", header=T)
set.seed(2)
index<-sample(1:nrow(NSTS),round(nrow(NSTS)*0.85)) 
veritrain<-NSTS[index,] 
veritest<-NSTS[-index,] 
lmod<-lm(VWAP~Volume+High+Low+Close+Last,data=veritrain)
wmod<-lm(residuals(lmod)^2~fitted(lmod)+fitted(lmod)^2,veritrain)
library(ggplot2)
veritrain$artik<-(residuals(lmod)) #EKK modelinden elde edilen artiklar (residuals)
veritrain$tahmin<-predict(lmod) #EKK modelinden elde edilen tahminler (prediction)
model1<-lm(abs(veritrain$artik)~Volume,data=veritrain)
weights1<-1/(predict(model1))^2 
veritrain<-veritrain[, -c(16,17)]
weightedleastsquaremod1<-lm(VWAP~Volume+High+Low+Close+Last, data= veritrain, weights = weights1)
kareresid<-((residuals(lmod))^2)
model3<-lm(kareresid~High+Low+Close+Last,data=veritrain)
weights3<-1/(predict(model3))^2 
weightedleastsquaremod3<-lm(VWAP~Volume+High+Low+Close+Last, data= veritrain, weights = weights3)
library(dplyr)
X<-model.matrix(lmod) 
y<-veritrain$VWAP #verimizdeki yanit degiskeni
W<-diag(weights3) #kosegenlerinde agirliklar olan matris
Z<-sqrt(W)%*%X #w matrisinin karekoku ile x in carpimi
yyildiz<-sqrt(W) %*% y
Betawls<-solve(t(Z)%*%Z,t(Z)%*%yyildiz)
donart<-yyildiz-Z%*%Betawls #donusturulmus model artiklari
bpmod<-lm(donart^2~ Volume+High+Low+Close+Last,data=veritrain) 
nsts <- NSTS%>%select(c("VWAP","Volume","High","Low","Close","Last" ))
set.seed(2)
index<-sample(1:nrow(nsts),round(nrow(nsts)*0.85)) 
veritrain1<-nsts[index,] 
veritest1<-nsts[-index,] 
lmod1<-lm(VWAP~Volume+High+Low+Close+Last,data=veritrain1)
x <- model.matrix(lmod1)[,-1]
e <- eigen(t(x)%*%x)$values
k <- sqrt(max(e)/min(e))
lambda<-10^seq(-3, 5, length.out = 100) #lamda icin dizi olusturma
x1<-as.matrix(veritrain1[,-1]) 
library(glmnet) 
ridgemodel<-glmnet(x1,veritrain1$VWAP,alpha = 0,lambda = lambda) 
cv.fitridge<-cv.glmnet(x1,veritrain1$VWAP,alpha=0,lambda = lambda) #yukarida olusturulan lambda dizisi icin
optimumlambda<-cv.fitridge$lambda.min
lambda_1SE<-cv.fitridge$lambda.1se
ridgemodel1<-glmnet(x1,veritrain1$VWAP,alpha=0,lambda=optimumlambda)
rmse<-function(true, predicted,n) {sqrt(sum((predicted - true)^2)/n)}

ypredictedridge <- predict(ridgemodel, s = optimumlambda, newx = as.matrix(veritest1[,-1]))# kurulan model uzerinden elde edilen tahmin degerleri

rsquare <- function(true, predicted) { 
    sse <- sum((predicted - true)^2) 
    sst <- sum((true - mean(true))^2) 
    rsq <- 1 - sse / sst 
    rsq }
ridgerkare<-rsquare(veritest1$VWAP,ypredictedridge) 
ridgermse<-rmse(veritest1$VWAP,ypredictedridge,length(veritest1$VWAP)) 
ridgeartik<-veritest1$VWAP-(ypredictedridge)
ridgeaic<-nrow(nsts)*(log(2*pi)+1+log((sum((ridgeartik)^2)/nrow(nsts))))+((length(ridgemodel$VWAP)+1)*2) 
ridgebic<-nrow(nsts)*(log(2*pi)+1+log((sum((ridgeartik)^2)/nrow(nsts))))+((length(ridgemodel$VWAP)+1)*log(nrow(nsts)))
cv.fitlasso<-cv.glmnet(x1,veritrain1$VWAP,alpha=1,lambda = lambda)
optimallambda<-cv.fitlasso$lambda.min
lambda_1SE1<-cv.fitlasso$lambda.1se
lassomodel<-glmnet(x1,veritrain1$VWAP,alpha=1,lambda=optimallambda) 
ypredictedlasso <- predict(lassomodel, s = optimallambda, newx = as.matrix(veritest1[,-1]))
lassorkare<-rsquare(veritest1$VWAP,ypredictedlasso) 
lassormse<-rmse(veritest1$VWAP,ypredictedlasso,length(veritest1$VWAP))
lassoartik<-veritest1$VWAP-(ypredictedlasso)
lassoaic<-nrow(nsts)*(log(2*pi)+1+log((sum((lassoartik)^2)/nrow(nsts))))+((length(lassomodel$VWAP)+1)*2) 
lassobic<-nrow(nsts)*(log(2*pi)+1+log((sum((lassoartik)^2)/nrow(nsts))))+((length(lassomodel$VWAP)+1)*log(nrow(nsts)))
cv.fitelasticnet<-cv.glmnet(x1,veritrain1$VWAP,alpha=0.5,lambda = lambda)
optlambda<-cv.fitelasticnet$lambda.min
lambda_1SE2<-cv.fitelasticnet$lambda.1se
elasticmodel<-glmnet(x1,veritrain1$VWAP,alpha=0.5,lambda=optlambda) 
ypredictedelasticnet <- predict(elasticmodel, s = optlambda, newx = as.matrix(veritest1[,-1]))
elasticrkare<-rsquare(veritest1$VWAP,ypredictedelasticnet)
elasticrmse<-rmse(veritest1$VWAP,ypredictedelasticnet,length(veritest1$VWAP)) 
elasticartik<-veritest1$VWAP-(ypredictedelasticnet)
elasticaic<-nrow(nsts)*(log(2*pi)+1+log((sum((elasticartik)^2)/nrow(nsts))))+((length(elasticmodel$VWAP)+1)*2)
elasticbic<-nrow(nsts)*(log(2*pi)+1+log((sum((elasticartik)^2)/nrow(nsts))))+((length(elasticmodel$VWAP)+1)*log(nrow(nsts)))
tablo<-matrix(c(ridgeaic,ridgebic,ridgermse,ridgerkare,
                lassoaic,lassobic,lassormse,lassorkare,
                elasticaic,elasticbic,elasticrmse,elasticrkare),3,4,byrow = TRUE)

row.names(tablo)<-c("Ridge","Lasso","Elasticnet") 

colnames(tablo)<-c("AIC","BIC","RMSE","Rkare") 
set.seed(3)
index<-sample(1:nrow(nsts),round(nrow(nsts)*0.95)) 
veritrain2<-nsts[index,] 
veritest2<-nsts[-index,] 
lmod2<-lm(VWAP~Volume+High+Low+Close+Last,data=veritrain2)
rmse1 <- function(x,y) sqrt(mean((x-y)^2))
library(pls)
pcrmodel <- pcr(VWAP~Volume+ High+ Low+Close+Last,data=veritrain2,scale=T) 
pcrmse <- RMSEP(pcrmodel) 
pcrmse1 <- RMSEP(pcrmodel,newdata=veritest2)
set.seed(3) 
pcrmodel1 <- pcr(VWAP~Volume+High+Low+Close+Last,data=veritrain2,scale=T,validation="CV") 
pcrCV <- RMSEP(pcrmodel1, estimate="CV")
model1<-lm(VWAP~Volume+High+Low+Close+Last,data=NSTS)
fit<- fitted(model1)
resid <-residuals(model1)
stud <- rstudent(model1)

hubermod <- rlm(VWAP~Volume+High+Low+Close+Last,data=NSTS)
stud1<- rstudent(hubermod)
bisquaremod <- rlm(VWAP~Volume+High+Low+Close+Last ,data=NSTS,psi=psi.bisquare)

biweights <- data.frame(state= NSTS$Date, resid = bisquaremod$resid, weight = bisquaremod$w)
biweights2 <- biweights[order(bisquaremod$w), ] 



ui<-dashboardPage(
    dashboardHeader(title ="ILERI REGRESYON"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Veri Tanitilmasi", tabName = "first",icon = icon("th")),
            
            menuItem("Model Kurulumu", tabName = "second", icon = icon(("th"))),
            
            menuItem("Varsayimlarin Sinanmasi", tabName = "third", icon = icon("th"),
                     
                     menuSubItem(
                         "Sabit Varyanslilik", tabName = "homojenvaryanslilik", icon = icon("th")),
                     
                     menuSubItem(
                         "Degisken Varyanslilik", tabName = "degiskenvaryanslilik", icon = icon("th"))),
            
            menuItem("Agirlikli EKK", tabName = "EKK", icon = icon("th")),
            
            menuItem("Collinearity", tabName = "Collinearity", icon = icon("th"),
                     
                     menuSubItem(
                         "collinearity", tabName = "collinearity", icon = icon("th")),
                     menuSubItem(
                         "Ridge Regresyon", tabName = "ridge", icon = icon("th")),
                     menuSubItem(
                         "Lasso Regresyon", tabName = "lasso", icon = icon("th")),
                     menuSubItem(
                         "Elastic Net Regresyon", tabName = "elastic", icon = icon("th")),
                     menuSubItem(
                         "Temel Bilesenler Regresyonu", tabName = "pcr", icon = icon("th"))),
            
            menuItem("Outlier", tabName = "outlier", icon = icon("th"),
                     menuSubItem(
                         "Normallik", tabName = "normallik",icon = icon("th")),
                     
                     menuSubItem(
                         "Robust Regresyon", tabName = "robust", icon = icon("th"))
            ))
        
        
    ), 
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "first",
                    h1("Irem Koyunlu 121517056"),
                    h1("Veri Tanitilmasi"),
                    h2("Veri Seti Aciklamasi"),
                    p("Ulusal Borsa:Hint bilisim sirketlerinin ulusal borsa veri setidir.
                        Hindistan Ulusal Borsasi (NSE) Mumbai, Maharashtra, Hindistan'da bulunan bir Hint borsasidir.
                        Ulusal Menkul Kiymetler Borsasi (NSE) 1992 yilinda yetkisiz bir elektronik borsa olarak kuruldu.
                        Hindistan Hukumeti'nin talebi uzerine onde gelen finans kuruluslari tarafindan tesvik edildi.
                        Hindistan'in ciro ile yaptigi en buyuk borsa. 1994 yilinda elektronik ekran tabanli ticareti baslatti.
                        Daha sonra, 2000 yilinda ulkede turunun ilk ornegi olan endeks futures ve internet ticareti baslatti.
                        248 gozlem 15 sutun bulunmakta."),
                    h2("Degiskenler:"),
                    p("Date: Verilerin kaydedildigi tarih"),
                    p("Symbol: Stokun NSE (Hindistan Ulusal Borsasi ) sembolu"),
                    p("Series: Bu hisse senedinin serisi.(EQ,BE,BL,BT,GC,IL)"),
                    p("Prev Close: Son gun kapanis noktasi"),
                    p("Open: Mevcut gun acilis noktasi"),
                    p("High: Mevcut gunun en yuksek noktasi"),
                    p("Low: Mevcut gunun en dusuk noktasi"),
                    p("Last: Son islem gununde belirli bir hisse senedi veya borsa endeksi icin son teklif edilen islem fiyati"),
                    p("Close: Gecerli gun icin kapanis noktasi"),
                    p("VWAP: Hacim agirlikli ortalama fiyat anlamina gelir, 
                        bir varligin belirli bir zaman araligi icin hacme gore agirligi alinmis ortalama fiyatidir"),
                    p("Volume: Belirli bir zaman diliminde islem goren menkul kiymet tutari.
                        Her alici icin bir satici var ve her islem toplam hacim sayisina katkida bulunuyor"),
                    p("Turnover: Hisse senedinin o gune kadar toplam cirosu"),
                    p("Deliverable: Bir grup insandan (bugunden once demat hesabida bu hisseleri olan ve bugun satis yapan) 
                        baska bir grup insana (bu hisseleri satin almis olan ve bu hisseleri T+2 gunlerine kadar alacak olan) 
                        hareket eden hisse miktari. Demat hesabi(Hindistan Internet Borsasi)"),
                    p("%Deliverble: Bu hisse senedinin teslimat yuzdesi"),
                    p("VWAP'yi ozellikle guclu bir gosterge haline getiren ortalama fiyat hesaplamasinda hacmi kullanma seklidir.
                        VWAP, hacmin gucuyle fiyat hareketini birlestirerek pratik ve kullanimi kolay bir gosterge yaratir. 
                        Alim satim yapanlar VWAP'yi bir trend onaylama ya da giris ve cikis noktalarini belirleme araci olarak kullanabilir.
                       VWAP her gunun basinda sifirlanir. Alim satim yapmak istedigimizde, VWAP'in altinda almak ve ustunde satmak karlidir.
                        Fiyatin bugunku degerinin ustunde mi alim yaptik yoksa altinda mi alim yaptik bunu belirlememizi saglar.
                        Fiyat VWAP uzerinde ise, satmak icin iyi bir gun ici fiyattir. Fiyat VWAP'in altindaysa, satin almak icin iyi bir gun ici fiyatidir")),
            
            tabItem(tabName = "second",
                    h1("Model Kurulumu"),
                    p("Regresyon modelimizi kuralim"),
                    p("lmod<-lm(VWAP~Volume+ High+ Low+Close+Last,data=veritrain)"),
                    p("VWAP, hacmin gucuyle fiyat hareketini birlestirerek pratik ve kullanimi kolay bir gosterge yaratir."),
                    p("Bagimli degiskenimiz VWAP tir.(Yanit degiskeni) Aciklayici degisken olarak da Volume,High,Low,Close,Last kullanarak regresyon modeli kuralim."),
                    box(verbatimTextOutput("summarymodel"), width = 12),
                    p("Kurulan regresyon modelinin anlamliligina baktigimizda p value=yaklasik 0 < 0.05 oldugundan kurulan model anlamlidir.")
            ),
            tabItem(tabName = "third",
                    h1("Varsayimlarin Sinanmasi"),
                    p("yorumlar buraya yazılacak")),
        
            
            tabItem(tabName = "homojenvaryanslilik",
                    h2("Sabit Varyanslilik Testleri"),
                    p("Sabit varyansliligin en kullanisli teshis yontemi artiklara (residuals(lmod)) karsilik tahmin (fitted(lmod)) degerlerinin  plotlanmasidir."),
                    box(plotOutput("homojenvaryanslilik1"),width = 12),
                    p("Cizdirdigimiz grafikte sifir etrafinda nasil dagildigini gormek icin h=0 ile yataya cizgi ekledik."),
                    p("Grafigimiz bize duzgun bir sekil vermedigi icin sabit varyansli mi diye emin olamiyoruz."),
                    p("Guvenilir olmasi icin degisken varyanslilik testlerine de bakmaliyiz.")),
            
            tabItem(tabName ="degiskenvaryanslilik",
                    h2("Degisken varyans testleri"),
                    h2("BREUSCH-PAGAN TESTI"),
                    p("H0:Heterosce Dosticity (Degisken Varyanslilik) problemi yok."),
                    p("H1:Heterosce Dosticity (Degisken Varyanslilik) problemi vardir."),
                    box(verbatimTextOutput("degiskenvaryanslilik"),width = 12),
                    p("Breusch pagan testimizin sonucuna gore p-value=2.2e-16 yaklasik 0 <0.05 oldugundan H0 hipotezi reddedilir yani heterocedosticity (degisken varyanslilik) problemi vardir deriz."),
                    h2("WHITE TEST"),
                    p("wmod<-lm(residuals(lmod)^2~fitted(lmod)+fitted(lmod)^2,veritrain)"),
                    box(verbatimTextOutput("degiskenvaryanslilik1"),width = 12),
                    p("White testimizin sonucuna gore p-value=0.004915 <0.05 oldugundan H0 hipotezi reddedilir yani heterocedosticity (degisken varyanslilik) problemi vardir.")),
            
            tabItem(tabName = "EKK",
                    h2("Agirlikli En Kucuk Kareler"),
                    p("Varyanslarin homojenligi saglanmamasi durumunda Yanit degiskeni uzerinde donusum yapmak veya Agirlikli En Kucuk Kareler yontemlerine basvururuz."),
                    p("Siradan en kucuk kareler yontemi, hata varyanslarinin sabit oldugunu varsayar(homoscedasticity). Agirlikli en kucuk kareler yontemi bu varsayim saglanmadigi durumlarda kullanilir."),
                    p("Varyansi buyuk olan degiskenin model uzerinde etkisi fazla olur. 
                       EKK nin en iyi calisabilmesi icin hata varyansi  Sigma2i   i=1,2,....n birbirine esit olmasi gerekir. Eger Sigma2i ler esit degil ise agirlikli en kucuk kareler yontemine gecmeliyiz."),
                    p("Her bir  Sigma2i varyansina karsilik wi= 1/Sigma2i agirligini tanimlariz.(Agirliklari 1/Sigma2i almamizin sebebi hepsinin varsayansini 1 e ve birbirlerine esitlemeye calismamizdir. Sigma2i*(1/Sigma2i))"),
                    p("Bu sekilde agirligi buyuk olanin agirligini alip, agirligi kucuk olana agirlik yukluyoruz.
                       Buradaki zorluk Sigma2i parametresinin bilinmemesinden kaynaklanir ve w matrisi kolay belirlenemez."),
                    p("Agirliklari belirlemek icin ilk olarak EKK regresyon modeli kurulur artiklar elde edilir.Agirliklar belirlendikten sonra agirliklandirilmis EKK kullanilir.Regresyon modeli uzerinden artiklar hesaplanip agirlik incelemesi yapilir. Gercekten kullanilan agirlikla varyans homojen hale gelmis mi diye bakilir.Eger varyans homojenligi saglanmamissa tekrar agirliklandirma yapilir buna iteratif agirliklandirma denir."),
                    h2("Bazi olasi varyans ve standart sapma fonksiyonu tahminleri:"),
                    p("1) Aciklayici degiskenlere karsilik artiklarin grafigini cizdirip megafon sekli varmi diye bakilir.Eger megafon sekli var ise aciklayici degiskenler ile mutlak artiklarin arasinda regresyon modeli kurulur.Bu regresyon modeli uzerinden tahmin degeri elde edilir.Bu elde elilen tahmin degerlerini Sigmai yerine kullaniriz.Agirliklar da 1/Sigma2i diye olusturulur."),
                    p("2) Ilk basta kurulan EKK modelindeki tahmin edilen yanitlara (y sapkalara) karsilik ei lerin grafigi megafon seklinde ise artiklarin mutlak degerine karsilik y sapkalarin regresyon modeli kurulur.Bu kurulan modelden elde edilen tahmin degerlerini Sigmai yerine kullaniriz.Agirliklari da wi= 1/Sigma2i seklinde olustururuz."),
                    p("3) Aciklayici degiskene karsi ei kare grafigi artan seklindeyse ei karelere karsilik o aciklayici degiskenin regresyon modeli kurulur.Bu modelin tahminlerini Sigma2i yerine kullaniriz.Agirliklari da  wi= 1/Sigma2i seklinde olustururuz."),
                    p("4) Tahmin edilen yanitlara (y sapka) karsi ei kare grafigi artan seklindeyse ei karelere karsilik tahmin edilen yanitlara regresyon modeli kurulur. Bu modelden elde edilen tahminler Sigma2i tahminleri olarak kullanilir. Agirliklari da  wi= 1/Sigma2i seklinde olustururuz."),
                    p("Bunlardan hangisi daha uygun gorulurse agirlik o sekilde belirlenmelidir."),
                    p("Simdi verimiz uzerinde bu anlatilanlari yapmaya baslayalim"),
                    p("EKK modelimizdeki elde edilen artiklar ve tahmin degerlerini verimize degisken olarak ekledik."),
                    p("Simdi Bazi olasi varyans ve standart sapma fonksiyonu tahminlerinden 1. inceleyelim."),
                    box(plotOutput("pairs1"), width = 12),
                    p("Artiklar ile Volume sacinim grafigi megafon seklindedir.Bu bagimsiz degisken ile artiklarin mutlak degeri arasinda regresyon modeli kuralim."),
                    p("weightedleastsquaremod1<-lm(VWAP~Volume+High+Low+Close+Last, data= veritrain, weights = weights1)"),
                    box(verbatimTextOutput("weightedleastsquaremod1"), width = 12),
                    p("Summary kodumuza baktigimizda Residual standard error: 1.458 ve Adjusted R-squared: 0.9999 cikmistir."),
                    p("Simdi Bazi olasi varyans ve standart sapma fonksiyonu tahminlerinden 2. inceleyelim."),
                    box(plotOutput("pairs2"), width = 12),
                    p("Artiklar ile tahminlerin  sacinim grafigi megafon seklinde degildir sonuc olarak bu yontemi verimizde kullanamiyoruz."),
                    p("Simdi Bazi olasi varyans ve standart sapma fonkiyonu tahminlerinden 3. inceleyelim"),
                    box(plotOutput("pairs3"), width = 12),
                    p("Aciklayici degiskene karsi ei kare grafigi artan seklinde oldugu icin ei karelere karsilik o aciklayici degiskenin Regresyon Modeli kurulur."),
                    box(verbatimTextOutput("weightedleastsquaremod3"), width = 12),
                    p("Summary kodumuza baktigimizda Residual standard error:  0.6213 ve Adjusted R-squared: 1 cikmistir."),
                    p("Simdi Bazi olasi varyans ve standart sapma fonkiyonu tahminlernden 4. inceleyelim;"),
                    box(plotOutput("pairs4"), width = 12),
                    p("Tahmin edilen yanitlara karsilik hatalarin karelerinin grafigi artan seklinde olmadigi icin bu yontemi kullanamayiz."),
                    p("EKK modelimizin Residual standard error: 4.431 ve Adjusted R-squared:  0.9999 dir."),
                    p("1. yontem ile agirliklandirma yaptigimizda Residual standard error: 1.458 ve Adjusted R-squared: 0.9999 dir."),
                    p("3. yontem ile agirliklandirma yaptigimizda Residual standard error: 0.6213 ve Adjusted R-squared: 1 dir."),
                    p("En dusuk stanard error a sahip ve en yuksek adjusted r-squared degerine sahip model 3. yontem ile agirliklandirma yaptigimizdaki modeldir."),
                    p("Ayrica agirliklandirma yaptigimda kullanilan bagimsiz degiskenlerimizin katsayilarinda degisimler meydana gelmistir."),
                    p("Simdi degisken varyanslilik probleminin giderildigini kontrol edelim."),
                    box(plotOutput("kontrol"), width = 12),
                    p("EKK modeli Basit EKK modeline donusturelim"),
                    box(verbatimTextOutput("out1"), width = 12),
                    p("Birinci sutun donusum ile cikan Beta katsayilarini,Ikinci sutun lm kodu ile elde edilen Beta katsayilarini gosterir.Agirliklandirilmis EKK modelindeki Beta katsayilari ile donusum yapildiginda cikan Beta katsayilari birebir ayni cikti"),
                    box(verbatimTextOutput("out2"), width = 12),
                    p("Birinci sutun donusturulmus artiklari ,ikinci sutun Agirliklandirilmis EKK modelinin artiklarini gostermektedir.Modelin artiklarina bakildiginda birbirinden ayri cikmistir."),
                    p("lm modeli ile kurulan Agirliklandirilmis EKK  modelinin artiklarini kok icinde w ile carparak donusturulmus modelin artiklari elde edilir."),
                    box(verbatimTextOutput("out3"), width = 12),
                    p("Birinci sutun donusturulmus artiklari,Ikinci sutun Agirliklandirilmis EKK modelinin artiklarini gostermekte.Modelin artiklarina bakildiginda birbirleriyle ayni cikmistir."),
                    p("Eger Agirliklandirilmis EKK uzerinden residuals standart error hesaplarsak"),
                    box(verbatimTextOutput("out4"), width = 12),
                    p("Agirliklandirilmis EKK modelinin residuals standat erroru ile ayni cikmamistir."),
                    p("Simdi Donusturulmus artiklarin standart errorunu hesaplayalim;"),
                    box(verbatimTextOutput("out5"), width = 12),
                    p("Simdi Agirliklandirilmis modelimiz icin BREUSCH-PAGAN TESTI yapalim;"),
                    h2("BREUSCH-PAGAN TESTI"),
                    p("H0:Heterosce Dosticity (Degisken Varyanslilik) problemi yok."),
                    p("H1:Heterosce Dosticity (Degisken Varyanslilik) problemi vardir."),
                    p("bpmod<-lm(donart^2~ Volume+ High+ Low+Close+Last,data=veritrain)"),
                    box(verbatimTextOutput("bpt"), width = 12),
                    p("BREUSCH-PAGAN Testimizin sonucuna gore p-value: 0.176  > 0.05 oldugundan H0 hipotezi kabul edilir yani degisken varyanslilik problemi yoktur deriz.Goruldugu uzere Agirliklandirma yaparak degisken varyanslilik problemini ortadan kaldirmis olduk.")),
            
            tabItem(tabName = "Collinearity",
                    h2("Aciklayici Degiskenlerle Ilgili Problemler")),
            
            tabItem(tabName = "collinearity",
                    h2("IC ILISKI (COLLINEARITY)"),
                    p("Iki degisken arasi lineer iliskiyi gosterir.Eger bir aciklayici degisken ve bir diger aciklayici degiskenin veya degiskenlerin lineer bir kombinasyonlari ise bu durumda x transpoz x matrisi (X'X) singuler olur ve tersi alinamaz. Bu durumdan ilgili degiskenlerden biri modelden cikartilarak kurtulunur.
                       Gozlem sayisi arttikca ic iliski durumu azalir."),
                    p("lmod1<-lm(VWAP~Volume+High+Low+Close+Last,data=veritrain)"),
                    box(verbatimTextOutput("lmod1"), width = 12),
                    p("Kurulan regresyon modelinin anlamliligina baktigimizda p value yaklasik 0 < 0.05 oldugundan kurulan model anlamlidir deriz."),
                    p("Simdi Collinearity teshisi icin korelasyon matrisi, kosul indeksi ve vif e bakacagiz"),
                    h2("KORELASYON MATRISI"),
                    p("Simdi x in korelasyon matrisine bakalim. Bunun icin yanit degiskenini (VWAP) veriden cikartmaliyiz.Geri kalanlarin korelasyon matrisine bakmaliyiz."),
                    box(verbatimTextOutput("cor"), width = 12),
                    p("Korelasyon matrisine baktigimizda ornegin en yuksek Close ile Last (aralarindaki korelasyon 0.9999782) bagimsiz degiskenlerinin iliskili oldugu gorulmektedir. Diger bagimsiz degiskenler arasinda da korelasyon oldukca yuksektir."),
                    p("Bu korelasyonlara simdi korelasyon plotu ile bakalim."),
                    box(plotOutput("cor1"), width = 12),
                    p("Korelasyon plotunda Pozitif korelasyonlar mavi, negatif korelasyonlar kirmizi renkte gosterilir."),
                    p("Korelasyon plotu ile korelasyon matrisimizin sonuclari ayni cikmistir.Bagimsiz degiskenlerin arasinda pozitif yonlu iliskinin yuksek oldugu gorulmektedir. (mavinin tonuna gore en yuksek iliskiden en dusuk iliskiye gore koyu maviden aciga dogru gidiyor.)"),
                    h2("KOSUL INDEKSI"),
                    p("Kappa degeri > 30 ise orta derece collinearity , kappa degeri > 100 ise guclu collinearity oldugunu gosterir."),
                    p("Kurulan regresyon modelimizi matrix haline donusturelim."),
                    box(verbatimTextOutput("matrix"), width = 12),
                    p("Verimizden yanit degiskenini cikartip sadece aciklayici degiskenlerden olusan matrix formuna donusturuyoruz."),
                    p("Simdi x transpoz x in eigen valuelerini hesaplayalim."),
                    box(verbatimTextOutput("eigen"), width = 12),
                    p("Kappa degeri : sqrt(En buyuk ozdeger / En kucuk ozdeger) "),
                    box(verbatimTextOutput("kappa"), width = 12),
                    p("Sonucumuzda Kappa=1517373 > 30 oldugundan sonucumuza gore collinearity (ic iliski) problemi vardir."),
                    h2("VIF"),
                    p("Xi degiskenlerinin diger bagimsiz degiskenler ile regresyonundan elde edilen R kare degerlerinin yuksekligi collinearity (ic iliski)nin varligini gosterir. Buna bagli gelistirilmis olcut VIF dir."),
                    p("VIF degerinin 10 dan buyuk olmasi collinearity (ic iliski) probleminin oldugunu soyler."),
                    p("Hazir kod ile vif degerlerine bakmak icin car paketi kullanilir."),
                    box(verbatimTextOutput("vif"), width = 12),
                    p("Bagimsiz degiskenlerimiz icin hesaplatilan vif degerlerimiz 10 dan buyuk oldugundan bagimsiz degiskenler arasinda collinearity(ic iliski) problemi vardir deriz."),
                    p("Bu uc tanimlama yontemi de bize bu veride collinearity problemi oldugunu isaret etmektedir. ")),
            
            tabItem(tabName = "ridge",
                    h2("Ridge Regresyon"),
                    p("Ridge regresyon EKK optimizasyon yontemine bir kisit getirir. Bu kisit Beta katsayilarinin karelerinin toplaminin uzerinedir."),
                    p("Lambda buyudukce Beta parametreleri 0 a dogru yaklasir.Parametrelerin buyumesi parametre tahminlerinin varyansini dusurur. Negatif etkisi modele yanlilik katmasidir. "),
                    p("Var(Betasapka ridge)= Sigma^2 / (x'x + LambdaI) burada Lambda yi buyuk tutarsak varyans kuculur ayni zamanda yanlilik artar. Ikisi arasinda denge kuracak sekilde Lambda belirlenmelidir. Multicollinearity problemini halledebilecek en kucuk Lambda yi belirlemeliyiz."),
                    p("EKK tahmin edicisi yansiz bir tahmin edici iken Ridge Regresyonunun tahmin edicisi yanli bir tahmin ediciye donusur. Ic iliski durumlarinda varyanslar buyukken Ridge Regresyonunda varyanslar daha kucuktur."),
                    p("Ridge Regresyon da aciklayici degiskenlerin tamamini modele dahil eder ve kompleks model olusmasini saglar.Modele degisken ekledikce hata duser fakat kompleks hale de gelebilir."),
                    p("lambda<-10^seq(-3, 5, length.out = 100) #lamda icin dizi olusturma"),
                    p("Ridge de lambda parametresinin secimi icin en iyi yontem Cross Validationdur. Bu sebeple lambdalar icin oncelikle bir dizi olusturduk."),
                    box(verbatimTextOutput("x1"), width = 12),
                    p("Train verimizi matrix haline donusturuyoruz (yanit degiskenini VWAP i cikardik)"),
                    p("Lambdanin farkli degerleri icin degiskenlerin aldigi farkli degerlerin grafigini cizdirelim;Ridge Regresyonda alpha = 0 degerini alir."),
                    box(plotOutput("ridgemodel"), width = 12),
                    p("Bu grafik lambdanin farkli degerleri icin degiskenlerin aldigi farkli degerlerin grafigidir."),
                    p("Grafikte kirmizi cizgiye baktigimizda stabil olmayan durum var. Bu degiskenin degeri digerlerine gore cok az bir degisim gosteriyor. Bu multicollinearity nin bir etkisidir."),
                    p("Grafigimizin ust bolumunde gozuken 5 degerleri Ridge Regresyonun hicbir bagimsiz degiskeni atmayip tamamini kullanmasindandir."),
                    p("Grafikte gorulen her bir cizgi bir degiskene karsilik gelmektedir."),
                    p("Bu grafik ile model katsayilarinin lambdaya gore nasil degistigini gosterdik. Simdi Cross Validation Yontemi ile optimal lambdayi belirleyelim."),
                    p("Cross Validationda 9 fold ile model kurulur 10 uncu fold da bu modelin performansi incelenir MSE' ye bakilir. Herbir lambda degeri icin Cross Validation yapilir. Tum bulunan MSE ortalamalari alinir ve minimum RMSE degerini veren lambda degeri secilir."),
                    box(plotOutput("ridgemodel1"), width = 12),
                    p("Grafikte gozuken kirmizi noktalar her lambda degeri icin 10 folddan gelen MSE lerin otalamasidir."),
                    p("Grafikteki ilk dogru minimum MSE degerini veren lambda degerinin logaritmasini, ikinci dogru ise foldlardan elde edilen MSE degerlerinin standart sapmasinin 1 oldugu lambda degerinin logaritmasini gostermektedir."),
                    p("Grafigimizin ust bolumunde gozuken 5 degerleri Ridge Regresyonunun tum degiskenleri kullanmasidir."),
                    p("Simdi optimal lambda degerini kullanarak Ridge Regresyon modelimizi kuralim ve bu modelin test verisi uzerinde RMSE ve R kare degerlerini hesaplayalim."),
                    p("Performans kiyaslamasi her zaman test veri seti uzerinden yapilir. Cunku multicollinearity nin train'e bir etkisi yoktur ama test'e etkisi vardir."),
                    box(verbatimTextOutput("optimumlambda"), width = 12),
                    box(verbatimTextOutput("lambda1se"), width = 12),
                    p("Grafigimizde cikan cizgilerimizin yerine bakarsak;"),
                    p("Ilk Dogru: log(Lambdamin)=log(0.001)= -6.907755"),
                    p("Ikinci Dogru : log(Lambda1SE)=log(9.111628)= 2.209551"),
                    p("Ridge Regresyon modeli;"),
                    p("ridgemodel1<-glmnet(x1,veritrain$VWAP,alpha=0,lambda=optimumlambda)"),
                    p("RMSE ve R kare hesaplayan fonksiyonlar;"),
                    p("rmse<-function(true, predicted,n) {sqrt(sum((predicted - true)^2)/n)}"),
                    p("ypredictedridge <- predict(ridgemodel, s = optimumlambda, newx = as.matrix(veritest[,-1]))# kurulan model uzerinden elde edilen tahmin degerleri"),
                    p("rsquare <- function(true, predicted) {sse <- sum((predicted - true)^2)"),
                    p("sst <- sum((true - mean(true))^2)"),
                    p("rsq <- 1 - sse / sst rsq }"),
                    p("Test verisi uzerinden Ridge modelinin R karesini hesaplatalim;"),
                    box(verbatimTextOutput("ridgerkare"), width = 12),
                    p("Ridge regresyon icin test verisi uzerinden R kare 0.9997101 cikmistir."),
                    p("Test verisi uzerinden Ridge Modelinin RMSE sini hesaplatalim;"),
                    box(verbatimTextOutput("ridgermse"), width = 12),
                    p("Ridge Regresyon icin test verisi uzerinden RMSE 9.104282 cikmistir."),
                    p("AIC ve BIC performans degerlendirme kriterleridir. Modeller arasinda kiyas yaparken kullanilir. Genel olarak AIC ve BIC degerleri daha kucuk olan model diger modellere gore daha iyidir. 
AIC ve BICde artik degerler kullanilir."),
                    p("Simdi Test verisi icin artiklar;"),
                    box(verbatimTextOutput("ridgeartik"), width = 12),
                    h2("AIC"),
                    box(verbatimTextOutput("ridgeaic"), width = 12),
                    p("Ridge Regresyon icin AIC degeri  1329.508 cikmistir."),
                    h2("BIC "),
                    p("Ridge Regresyon icin BIC degeri 1333.022 cikmistir."),
                    box(verbatimTextOutput("ridgebic"), width = 12)),
            
            tabItem(tabName = "lasso",
                    h2("Lasso Regresyon"),
                    p("Degisken secimi icin kullanilir. Lassoda katsayilarin bazilari Ridgeden farkli olarak sifirlanmaktadir. "),
                    p("Lasso multicollinearity problemini cozerken ayni zamanda degisken secimi de yapabilme yetenegine sahiptir. Lassoda da Lambda ceza parametresi vardir. Lambda buyudukce modelden atilan (disarida birakilan) degisken sayimiz artiyor."),
                    p("Bazen Lasso cok fazla degiskeni disarida birakmaktadir. Bu modelin tahmin performansini dusurur. Modelde gormek istedigimiz degiskeni goremeyebiliriz."),
                    p("Ilk olarak Cross Validation ile Optimal Lambda degerini belirleyelim.Lasso Regresyonunda alpha = 1 degerini alir."),
                    box(plotOutput("lassomodel"), width = 12),
                    p("Grafigimizin ust bolumunde gozuken degerler Lasso Regresyonunun bagimsiz degiskenlerinin tamamini kullanmayarak modelden atmasindan dolayi kaynaklanir."),
                    p("Grafikte gozuken kirmizi noktalar her lambda degeri icin 10 folddan gelen MSE lerin ortalamasidir."),
                    p("Grafikteki ilk dogru minimum MSE degerini veren lambda degerinin logaritmasini, ikinci dogru ise foldlardan elde edilen MSE degerlerinin standart sapmasinin 1 oldugu lambda degerinin logaritmasini gostermektedir."),
                    p("Optimal Lambda degeri;"),
                    box(verbatimTextOutput("optimallambda"), width = 12),
                    box(verbatimTextOutput("lambda1se1"), width = 12),
                    p("Grafigimizde cikan cizgilerimizin yerine bakarsak ;"),
                    p("Ilk Dogru: log(Lambdamin)=log(0.001)= -6.907755 "),
                    p("Ikinci Dogru : log(LAmbda1SE)=log(1.417474)= 0.3488764"),
                    p("Simdi Optimal Lambda degerini kullanarak Lasso Regresyon modelimizi kuralim ve bu modelin test verisi uzerinde RMSE ve R kare degerlerini hesaplayalim."),
                    p("Lasso regresyon modeli;"),
                    box(verbatimTextOutput("lassomodel1"), width = 12),
                    p("Burada hicbir degerimiz atilmamistir."),
                    p("Simdi test verisi uzerinden Lasso Regresyon modelimizin performansina bakalim."),
                    p("Kurulan model uzerinden elde edilen tahmin degerleri;"),
                    box(verbatimTextOutput("ypredictedlasso"), width = 12),
                    p("Test verisi icin Lasso Regresyon modelinin R karesi;"),
                    box(verbatimTextOutput("lassorkare"), width = 12),
                    p("Test verisi icin Lasso Regresyon Modelinin R karesi 0.999713 cikmistir."),
                    p("Test verisi icin Lasso Regresyon Modelinin RMSE si;"),
                    box(verbatimTextOutput("lassormse"), width = 12),
                    p("Test verisi icin Lasso Regresyon Modelinin RMSE si 9.058652 cikmistir."),
                    p("Lasso Regresyon Modeli icin artiklar;"),
                    box(verbatimTextOutput("lassoartik"), width = 12),
                    p("Lassonun artiklari uzerinden AIC;"),
                    box(verbatimTextOutput("lassoaic"), width = 12),
                    p("Lasso Regresyon icin AIC degeri 1327.016 cikmistir."),
                    p("Lassonun artiklari uzerinden BIC;"),
                    box(verbatimTextOutput("lassobic"), width = 12),
                    p("Lasso Regresyon icin BIC degeri 1330.53 cikmistir.")),
            
            
            tabItem(tabName = "elastic",
                    h2("Elastic Net Regresyon"),
                    p("Elatic net Ridge Regresyon Modeli ile Lasso Regresyon Modelinin bir kombinasyonudur.Ridge tarzi cezalandirma ve Lasso tarzi degisken secimi yapar. Lasso ve Ridge'de bulunan  Lambda parametresi haricinde ikinci bir parametre olan Alfa parametresi de vardir. Ozellikle yuksek korelasyonlu degisken gruplari oldugunda onerilir."),
                    p("Ilk olarak Cross Validation ile Optimal Lambda degerini belirleyelim.Lambda= 0.5 alindiginda Elastic Net Regresyon Modeli elde edilir."),
                    box(plotOutput("elasticmodel"), width = 12),
                    p("Grafigimizin ust bolumunde gozuken degerler Elastic Net Regresyonun bagimsiz degiskenlerinin tamamini kullanmayarak modelden atmasindan dolayi kaynaklanir."),
                    p("Grafikte gozuken kirmizi noktalar her lambda degeri icin 10 folddan gelen MSE'lerin ortalamasidir."),
                    p("Gra???kteki ilk dogru minimum MSE degerini veren lambda degerinin logaritmasini, ikinci dogru ise foldlardan elde edilen MSE degerlerinin standart sapmasinin 1 oldugu lambda degerinin logaritmasini gostermektedir."),
                    p("Optimal Lambda degeri;"),
                    box(verbatimTextOutput("optlambda"), width = 12),
                    box(verbatimTextOutput("lambda_1SE"), width = 12),
                    p("Grafigimizde cikan cizgilerimizin yerine bakarsak ;"),
                    p("Ilk Dogru: log(Lambdamin)=log(1.176812)= 0.1628091 "),
                    p("Ikinci Dogru : log(Lambda1SE)=log(4.328761)= 1.465281"),
                    p("Simdi Optimal Lambda degerini kullanarak Elastic Net Regresyon Modelimizi kuralim ve bu modelin test verisi uzerinde RMSE ve R kare degerlerini hesaplayalim."),
                    box(verbatimTextOutput("elasticmodel1"), width = 12),
                    p("5 degiskenli modelimizde Elastic Net Regresyonu 1 degiskeni (Volume ) atilmistir.Bu degiskene iliskin katsayilar sifirlanmistir."),
                    p("Simdi test verisi uzerinden Elastic Net Regresyon Modelimizin performansina bakalim."),
                    p("Kurulan model uzerinden elde edilen tahmin degerleri;"),
                    box(verbatimTextOutput("ypredictedelasticnet"), width = 12),
                    p("Test verisi icin Elastic Net Regresyon modelinin R karesi;"),
                    box(verbatimTextOutput("elasticrkare"), width = 12),
                    p("Test verisi icin Elastic Net Regresyon Modelinin R karesi 0.9999421 cikmistir."),
                    p("Test verisi icin Elastic Net Regresyon Modelinin RMSE si;"),
                    box(verbatimTextOutput("elasticrmse"), width = 12),
                    p("Test verisi icin Elastic Net Regresyon Modelinin RMSE si 4.069581 cikmistir."),
                    p("Elastic Net Regresyon Modeli icin artiklar;"),
                    box(verbatimTextOutput("elasticartik"), width = 12),
                    p("Elastic Net in artiklari uzerinden AIC;"),
                    box(verbatimTextOutput("elasticaic"), width = 12),
                    p("Elastic Net Regresyon icin AIC degeri 930.1267 cikmistir."),
                    p("Elastic Net in artiklari uzerinden BIC;"),
                    box(verbatimTextOutput("elasticbic"), width = 12),
                    p("Elastic Net Regresyon icin BIC degeri  933.6402 cikmistir."),
                    p("Simdi kurulan modellerde AIC, BIC, RMSE ve R kare degerlerini karsilastirip model secimi yapalim."),
                    box(verbatimTextOutput("tablo"), width = 12),
                    p("Secim yaparken AIC BIC ve RMSE degerleri kucuk , R kare degeri buyuk olan model secilmelidir."),
                    p("Modellerin R2 degerlerini, RMSE degerlerini, AIC, BIC degerlerini karsilastirdigimizda;"),
                    p("En kucuk rmse degeri Elasticnet Regresyon Modelinde (RMSE: 4.069581)"),
                    p("En kucuk AIC degeri Elasticnet Regresyon Modelinde (AIC: 930.1267)"),
                    p("En kucuk BIC degeri Elasticnet Regresyon Modelinde (BIC: 930.1267)"),
                    p("En buyuk R kare degeri Elasticnet regresyon modelinde (R Kare: 930.1267)."),
                    p("Bu sebeple calisabilecegimiz en iyi regresyon modeli Elastic Net Regresyon Modelidir. Bu veriyi modellemede Elastic Net Regresyon Modeli daha uygundur."),
                    p("Multicollinearity nin cozumunde Ridge, Lasso, Elastic Net haricinde Temel Bilesenler Regresyonu da kullanilir."),
                    p("Simdi multicollinearity probleminden Temel Bilesenler Yolu ile kurtulmayi deneyelim.")),
            
            
            tabItem(tabName = "pcr",
                    h2("Temel Bilesenler Regresyonu(PCR)"),
                    p("Veri setindeki tum degiskenler vektoru ifade eder. Degiskenler arasinda iliski olmadiginda bu vektorler birbirlerine diktir.Temel bilesenler analizinin amaci, X matrisine bir donusum uygulayarak birbirlerine dik vektorlerden olusan bir sistem elde etmektir. Yuksek boyutlu verilerde dusuk boyutlu dogrusal yapi elde etmek icin kullanilan bir yontemdir. Vektor boyutlari kisa olanlar goz ardi edilir."),
                    p(" Oncelikle Train veri seti uzerinde kurdugumuz EKK Regresyon Modelini kullanarak, hem train hem de test veri seti uzerinde RMSE degerlerini hesaplayalim."),
                    p("EKK Regresyon Modelimiz;"),
                    box(verbatimTextOutput("lmod2"),width = 12),
                    p("Kurulan Regresyon Modelinin anlamliligina baktigimizda p value yaklasik=0< 0.05 oldugundan H0 red edilir yani kurulan model anlamlidir deriz."),
                    p("Simdi train ve test veri seti uzerinde RMSE degerlerini hesaplayalim."),
                    box(verbatimTextOutput("rmse1"),width = 12),
                    p("Train veri seti uzerinden kurulan regresyon modelinin RMSE degeri 4.379819 dir."),
                    box(verbatimTextOutput("rmse2"),width = 12),
                    p("Test veri seti uzerinden kurulan regresyon modelinin RMSE degeri 3.084308 dir."),
                    p("Iki RMSE degeri arasinda fark olmasinin sebebi multicollinearity nin varliginin gostergesidir."),
                    box(plotOutput("ucluplot"), width = 12),
                    p("Bazi aciklayici degiskenler arasinda sacinim grafigi cizdirdik. Bunlar arasinda lineer iliski goruluyor. Bu da multicollinearity'nin varliginin bir gostergesidir."),
                    p("Simdi tum componentleri kullanarak bir Temel Bilesenler Regresyonu kuralim."),
                    box(verbatimTextOutput("pcrmodel"),width = 12),
                    p("Ciktinin ilk satiri componentlerin X matrisindeki aciklayicilik oranlarini, ikinci satir ise yanit degiskenindeki aciklayicilik miktarlarini gostermektedir."),
                    p("Verimizde 5 adet vektor vardi. Besinci component ile yanittaki degiskenligin yaklasik %99.9 aciklanabilmektedir."),
                    p("Train ne kadar cok componentle calisirsa o kadar iyi sonuc alinir. Cunku multicollinearity nin train uzerinde etkisi yoktur fakat test verisi uzerine etkisi vardir."),
                    box(plotOutput("validationplot"), width = 12),
                    p("Bu grafik bize her componente karsilik gelen RMSE degerlerini gosterir. Train veri seti uzerinden cizilen grafikte component sayisi arttikca RMSE degerleri dusmektedir."),
                    p("En buyuk degisim intercepten birinci componente geciste yasanmistir. Ikinci componentten sonra bir degisim olmamistir. Toplamda intercept ile birlikte 6 component var."),
                    p("Bu grafige iliskin RMSE degerleri;"),
                    box(verbatimTextOutput("pcrmse"),width = 12),
                    p("RMSE degeri component sayisi arttikca azalmaktadir. En dusuk RMSE degeri ilk olarak birinci componentte gorulmektedir."),
                    p("Simdi RMSE icin yaptiklarimizi R kare icinde yapalim."),
                    box(plotOutput("validationplot1"), width = 12),
                    p("Bu grafik bize her componente karsilik gelen R kare degerlerini gosterir. Train veri seti uzerinden cizilen grafikte component sayisi arttikca R kare degerleri artmaktadir."),
                    p("En buyuk degisim intercepten birinci componente geciste yasanmis.Ikinci componentten sonra bir degisim olmamistir. Toplamda intercept ile birlikte 6 component var."),
                    p("Multicollinearity icin train veri setine bakmak uygun olmadigindan test veri setine bakmak daha uygun olacaktir."),
                    p("Test seti uzerinde component sayilarina gore RMSE degerlerini de RMSEP fonksiyonunu kullanarak bulabiliriz."),
                    box(verbatimTextOutput("pcrmse1"),width = 12),
                    p("Test veri seti uzerinden baktigimizda performans acisindan en kucuk RMSE degeri 3.084 ile besinci componenttedir."),
                    p("Simdi 5 component ile kurulan modelin katsayilarina bakalim."),
                    box(verbatimTextOutput("coef1"), width = 12),
                    p("Bu cikti bize pcr 5 component icin modelin yanit degiskeni tahminlerini verir."),
                    p("Simdi bu modeli kullanarak train ve test veri seti uzerinden RMSE degerlerini hesaplayalim."),
                    box(verbatimTextOutput("rmse3"),width = 12),
                    p("Train veri seti uzerinden RMSE degeri 4.379819 dir."),
                    box(verbatimTextOutput("rmse4"),width = 12),
                    p("Test veri seti uzerinden RMSE degeri 3.084308 dir."),
                    p("Componentlerimizin tamamini kullandigimiz icin sonucumuz Ekk modelimizle ayni cikmistir.Optimal component sayisini belirlerken test verisi uzerindeki performansa baktik. Bu is icin Cross Validation Yonteminin kullanilmasi aslinda daha dogru bir yaklasim olacaktir."),
                    h2("CROSS-VALIDATION ILE TEMEL BILESEN ANALIZI"),
                    p("Kac component olmasi gerektigini daha iyi verecek yontemdir. Diger yontemlerde her yapista farkliliklar olmaktadir."),
                    p("Train veri seti uzerinden Cross Validation hesaplatalim;"),
                    box(verbatimTextOutput("pcrCV"),width = 12),
                    p("RMSE degeri en dusuk besinci componentte (5.116) cikmistir."),
                    box(plotOutput("plotcv"), width = 12),
                    p("Cross Validation icin componentlere karsilik RMSE degerlerinin grafigi yukarida gosterilmektedir."),
                    p("Pcrcv icin en kucuk RMSE degerini alan componentin hangisi olduguna bakmak icin;"),
                    box(verbatimTextOutput("min1"),width = 12),
                    p("Grafikte Cross Validated RMSE degerleri vardir. 10 fold Cross Validation ile pcrCV degerlerinin altincisi yani besinci componente karsilik gelen RMSE degeri minimum cikti."),
                    box(verbatimTextOutput("coefcv"),width = 12),
                    p("Bu cikti bize pcrCV 5 component icin modelin yanit degiskeni tahminlerini verir."),
                    p("Yani tahmin performansimiz besinci componentte en iyi olmaktadir."),
                    p("Datayi NSTS alarak modelimizi kuralim; "),
                    p("model1<-lm(VWAP~Volume+High+Low+Close+Last,data=NSTS)"),
                    box(verbatimTextOutput("model1"),width = 12),
                    p("Kurulan Regresyon Modelinin anlamliligina baktigimizda p value yaklasik=0< 0.05 oldugundan H0 red edilir yani kurulan model anlamlidir deriz."),
                    p("Tahmin degerlerini gosterelim"),
                    box(verbatimTextOutput("fit"),width = 12),
                    p("Artik degerlerini gosterelim"),
                    box(verbatimTextOutput("resid"),width = 12)),
            
            
            tabItem(tabName = "outlier",
                    h2("Outlier")),
            
            tabItem(tabName= "normallik",
                    h2("HATA ILE ILGILI VARSAYIMLAR"),
                    h2("NORMALLIK VARSAYIMININ KONTROLU TESTI"),
                    p("Tum normallik testlerini (Shapiro-Wilk,Kolmogorov-Smirnov,Cramer-von Mises,Anderson-Darling) bir arada gormek icin asagidaki kodu kullanmaliyiz;"),
                    box(verbatimTextOutput("normalliksinamasi"),width = 12),
                    p("Normallik testlerimizin p-valuelerine baktigimizda 0.05 ten kucuk oldugunu goruyoruz yani artiklarimizin dagilimi normal degildir deriz."),
                    box(plotOutput("plotnormallik"), width = 12),
                    p("Histogram Grafigimize baktigimizda Kirmizi cizgi bize Normal Dagilim Egrisini verir , Mavi olan cizgi ise sinif orta noktalarindan gecen diyagramdir.Mavi ve Kirmizi cizgilerimiz birbirine benzemedigi icin verinin Normal dagilmadigini soyleyebiliriz."),
                    p("Q-Q Plot Grafigimize baktigimizda verimiz Q-Q Plot cizgisi uzerinde olmadigindan normal dagilim varsayiminin saglanmadigi gorulmektedir."),
                    p("Yogunluk Grafigimize baktigimizda sag kuyruk daha uzun oldugu icin grafigimiz sola carpiktir deriz."),
                    h2("SABIT VARYANS"),
                    p("Sabit varyansliligin en kullanisli teshis yontemi artiklara  karsilik tahmin degerlerinin  plotlanmasidir."),
                    box(plotOutput("ggplot"), width = 12),
                    p("Cizdirdigimiz grafigimiz bize duzdun bir sekil vermedigi icin sabit varyansli olup olmadigina emin olamiyoruz.Bu yuzden degisken varyanslilik testlerine basvuruyoruz."),
                    h2("DEGISKEN VARYANSLILIK TESTLERI"),
                    h2("BREUSCH-PAGAN TESTI"),
                    p("H0:Heterosce dosticity (Degisken Varyanslilik) problemi yok."),
                    p("H1:Heterosce dosticity (Degisken Varyanslilik) problemi vardir."), 
                    box(verbatimTextOutput("bp1"),width = 12),
                    p("BREUSCH-PAGAN testimizin sonucuna gore p-value yaklasik=0 <0.05 oldugundan H0 hipotezi reddedilir yani Heteroscedosticity  (degisken varyanslilik) problemi vardir."),
                    h2("ILISKILI HATALAR (OTOKORELASYON)"),
                    p("H0: Otokorelasyon yoktur."),
                    p("H1: Otokorelasyon vardir."),
                    p("dwtest(VWAP~Volume+High+Low+Close+Last ,data=NSTS)"),
                    box(verbatimTextOutput("dw"),width = 12),
                    p("Sonucumuza gore p-value degerimiz 0.2024  cikmistir.P-value degerimiz 0.05'ten buyuk oldugu icin H0 kabul edilir.Bu nedenle Otokorelasyon yoktur deriz."),
                    h2("OLAGAN DISI GOZLEMLER(AYKIRI GOZLEMLERIN BELIRLENMESI)"),
                    h2("OUTLIER"),
                    p("Model tarafindan iyi tahmin edilemeyen gozlemlere denir.Hatalari buyuk olan gozlemlere denir."),
                    p("Simdi kurulan regresyon modelimizin grafiklerini inceleyelim;"),
                    p("Ilk oncelikle modelimiz icin supheli outlier varsayimlarimiza bakalim; "),
                    box(plotOutput("plotm1"), width = 12),
                    p("Cizdirdigimiz grafikte kirmizi olan gozlemler supheli outlier degerlerimizdir.Bu grafik sadece varsayim yapmaktadir bu nedenle oncellikle library(faraway) ile plot cizdirerek modelimizde outlier suphesi olan gozlemlerine bakalim;"),
                    box(plotOutput("plotm"), width = 12),
                    p("1.Grafik icin Artiklara karsi cizdirdigimiz grafige baktigimizda varyanslarin homojen olmadigi gorulmektedir.Outlier suphesi olan gozlemlerimiz 18,77 ve 155. gozlem cikmistir."),
                    p("2.Grafik icin Normal Q-Q Plot Grafigimize baktigimizda verimiz Q-Q Plot cizgisi uzerinde olmadigindan normal dagilim varsayiminin saglanmadigi gorulmektedir.Outlier suphesi olan gozlemlerimiz 18,77 ve 155. gozlem cikmistir."),
                    p("3.Grafik icin Standartlastirilmis artiklarin karekokune karsi cizdirdigimiz grafige baktigimizda varyanslarin homojen olmadigi gorulmektedir.Outlier suphesi olan gozlemlerimiz 18,77 ve 155. gozlem cikmistir"),
                    p("4.Grafik icin Cook's Distance a gore grafikte cikan degerler yani 18,77 ve 155. gozlemlere outlier suphesiyle yaklasilir."),
                    p("Bu grafiklere baktigimizda 18,77 ve 155. gozlemleri muhtemelen sorunlu olarak tanimlayabiliriz"),
                    p("Hangi durumlari temsil ettigini gormek icin bu gozlemlere bakmaliyiz"),
                    box(verbatimTextOutput("gozlem"),width = 12),
                    p("Modelimiz icin standartlastirilmis artiklarimiza bakacak olursak;"),
                    box(verbatimTextOutput("stud"),width = 12),
                    p("Standartlastirilmis artiklarimizin mutlakca en buyugune bakmaliyiz."),
                    box(verbatimTextOutput("stud1"),width = 12),
                    p("En buyuk standartlastirilmis artik degerim 77. gozlem olup degeri mutlakca 7.088158  dir. Fakat aykiri gozlem midir bakalim "),
                    p("Benferonni duzleltmesi yaparsak;"),
                    box(verbatimTextOutput("qt"),width = 12),
                    p("Burada en buyuk standartlastirilmis mutlak artik degerini 7.088158   olarak bulmustuk."),
                    p("(|7.088158| > |-3.774908|) oldugundan  77  inci gozlem bizim icin outlierdir."),
                    p("Bunu hazir kod ile yaptigimizda;"),
                    box(verbatimTextOutput("outliertest"),width = 12),
                    p("En buyuk aykiri deger 77. gozleme ait olmakla beraber diger outlier degerler ise 155,18 gozlemlere aittir. Veri setimizde aykiri gozlemler mevcuttur."),
                    p("Varsayimlar gerceklesmediginde ve aykiri gozlemler oldugunda ROBUST REGRESYON yontemi kullanilabilir.")),
            
            tabItem(tabName = "robust",
                    h2("ROBUST REGRESYON"),
                    p("Tum regresyon varsayimlari gecerli oldugunda , dogrusal regresyon icin normal EKK tahminleri en uygunudur.Bu varsayimlardan bazilari gecersiz oldugunda , EKK regresyonu kotu performans gosterebilir."),
                    p("Aykiri gozlemler regresyon dogrusunu kendine gore kendine dogru ceker.Bu sekilde parametre tahminlerini olmasi gerektigi yerden cok uzaga tasiyabilir.Model uzerinde etkileri diger gozlemlerden daha fazla olur.Bu durum bizim model tahmin performansimizi dusurur.Bu durumda robust regresyon kullanilir."),
                    p("Bu gozlemlerin model uzerinde etkisini dusunerek agirliklandirma yapiyoruz."),
                    p("Birden cok aykiri gozlem varsa modele bundan etkilenebiliyor(maskeleme-swamping).Bu sekilde kurulan modelde yanlis olmus oluyor ve bu modelin artiklari uzerinden yorum yapmak da cok dogru olmuyor."),
                    p("Aykiri gozlem calismasi yapilacaksa Robust Regresyon Modeli kurup bu regresyon modelinin artiklari uzerinden konusmak daha dogru olacaktir."),
                    p("Robust Regresyon iteratif yeniden agirlikli En Kucuk Kareler (IRLS) ile yapilir.Robust Regresyon calistirma komutu MASS paketinde rlm'dir.IRLS icin kullanilabilecek cesitli agirlik fonksiyonlari vardir."),
                    p("Siradan EKK regresyonu ve robust regresyonun sonuclarini karsilastirirken sonuclar cok farkliysa Robust Regresyondan gelen sonuclar kullanilir.Buyuk farkliliklar, model parametrelerinin aykiri degerlerden buyuk oranda etkilendigini gostermektedir."),
                    p("Farkli agirliklandirmalarin avantajlari ve dezavantajlari vardir.Huber agirliklari siddetli aykiri degerlerde zorluklar yasayabilir ve bisquare agirliklar yakinsamada zorluk yasayabilir veya birden fazla cozum verebilir."),
                    p("Robust Regresyon modelimizi kuralim."),
                    p("hubermod <- rlm(VWAP~Volume+High+Low+Close+Last,data=NSTS)"),
                    box(verbatimTextOutput("hubermod"),width = 12),
                    p("Huber agirliklari kullanarak kurdugumuz Robust Regresyon Modelimizin Residual standard error u 2.932 cikmistir."),
                    p("EKK modelimizde Residual standard error u 4.374 cikmisti."),
                    p("Iki Regresyon Modelinin katsayilarini yan yana gosterelim;"),
                    box(verbatimTextOutput("hubermod1"),width = 12),
                    p("Iki modelin ciktilari arasinda gozle gorulur degisimler vardir."),
                    p("Simdi Robust Regresyon icin standart artiklari inceleyelim;"),
                    box(plotOutput("halfnorm"), width = 12),
                    p("Robust resgresyonun ham artiklari 18,155 ve 77. gozlemler olarak cikmistir."),
                    box(verbatimTextOutput("studhubermod"),width = 12),
                    p("En buyuk standartlastirilmis artik degerim 77. gozlem olup degeri mutlakca 7.088158  dir.Fakat aykiri gozlem midir bakmaliyiz."),
                    p("Bonferonni duzeltmesi yaparsak;"),
                    box(verbatimTextOutput("qt1"),width = 12),
                    p("Burada en buyuk standartlastirilmis mutlak artik degerini 7.088158 olarak bulmustuk."),
                    p("(|7.088158| > |-3.774908|) oldugundan  77  inci gozlem bizim icin outlierdir."),
                    p("Diger outlier degerler;"),
                    box(verbatimTextOutput("huberoutlier"),width = 12),
                    p("En buyuk aykiri deger 77. gozleme ait olmakla beraber diger outlier deger ise 155. gozleme aittir. 18. gozlemimiz outlier olmaktan cikmistir Swamping olmustur."),
                    p("Swamping Outlier yuzunden aslinda outlier olmayan bir gozlemin outliermis gibi gozukmesidir."),
                    p("Simdi de Bisquare agirliklandirmasini kullanarak Regresyon Modelimizi kuralim;"),
                    p("bisquaremod <- rlm(VWAP~Volume+High+Low+Close+Last ,data=NSTS,psi=psi.bisquare)"),
                    box(verbatimTextOutput("bisquaremod"),width = 12),
                    p("Bisquare agirliklari kullanarak kurdugumuz Robust Regresyon Modelimizin Residual standard error u 3 cikmistir."),
                    p("Huber agirliklari kullanarak kurdugumuz Robust Regresyon Modelimizin Residual standard error u 2.932 cikmistir."),
                    p("EKK modelimizde Residual standard error u 4.374 cikmistir."),
                    p("Huber agirliklari kullanarak kurdugumuz Robust Regresyon Modelimizin Residual standard erroru daha iyi cikmistir."),
                    p("Simdi tekrardan agirliklara bakalim;"),
                    box(verbatimTextOutput("biweights2"),width = 12),
                    p("Ilk sutun Robust Regresyon Modelinin artiklarini gosterir."),
                    p("Resid ler  ne kadar buyukse ona karsilik gelen weight degeri de o kadar kucuk olmaktadir."),
                    p("Iki modelin residual standart errorlerine bakildigi zaman Huber yontemi daha kucuk residual standart error (2.932) degerine sahiptir."))
            
            
            
        )))



server<- function(input, output){
    
    output$degiskenvaryanslilik<- renderPrint(bptest(lmod,data=veritrain))
    
    output$degiskenvaryanslilik1<- renderPrint(summary(wmod))
    
    output$summarymodel<- renderPrint({summary(lmod)})
    
    output$pairs1<-renderPlot(pairs(~residuals(lmod)+Volume+High+Low+Close+Last+predict(lmod),data=veritrain, main="Temel Dagilim Grafigi Matrisi"))
    
    output$pairs2<-renderPlot(pairs(residuals(lmod)~predict(lmod),data=veritrain, main="Temel Dagilim Grafigi Matrisi"))
    
    output$pairs3<-renderPlot(pairs(kareresid~Volume+High+Low+Close+Last,data=veritrain, main="Temel Dagilim Grafigi Matrisi"))
    
    output$pairs4<-renderPlot(pairs(kareresid~predict(lmod), main="Temel Dagilim Grafigi Matrisi"))
    
    output$weightedleastsquaremod1<-renderPrint(summary(weightedleastsquaremod1))
    
    output$weightedleastsquaremod3<-renderPrint(summary(weightedleastsquaremod3))
    
    output$kontrol<-renderPlot({par(mfrow=c(1,2))
        plot(predict(lmod),residuals(lmod))
        plot(predict(weightedleastsquaremod3),residuals(weightedleastsquaremod3))})
    
    output$out1<-renderPrint(cbind(Betawls,coef(weightedleastsquaremod3)))
    
    output$out2<-renderPrint(head(cbind(donart,residuals(weightedleastsquaremod3)),15))
    
    output$out3<-renderPrint(head(cbind(sqrt(W)%*%residuals(weightedleastsquaremod3), donart),10))
    
    output$out4<-renderPrint(sqrt(sum(residuals(weightedleastsquaremod3)^2)/205))
    
    output$out5<-renderPrint(sqrt(sum(donart^2)/205))
    
    output$bpt<-renderPrint(summary(bpmod))
    
    output$lmod1<-renderPrint(summary(lmod1))
    
    output$cor<-renderPrint(cor(veritrain1[,-c(1)]))
    
    output$cor1<-renderPlot({corrplot(cor(veritrain1[,-c(1)]),method = "pie", order="hclust")})
    
    output$matrix<-renderPrint(head(x,5))
    
    output$eigen<-renderPrint(e)
    
    output$kappa<-renderPrint(k)
    
    output$vif<-renderPrint(vif(lmod1))
    
    output$x1<-renderPrint(head(x1,10))
    
    output$ridgemodel<-renderPlot({plot(ridgemodel,xvar = "lambda")})
    
    output$ridgemodel1<-renderPlot({plot(cv.fitridge)})
    
    output$optimumlambda<-renderPrint(optimumlambda)
    
    output$lambda1se<-renderPrint(lambda_1SE)
    
    output$ridgerkare<-renderPrint(ridgerkare)
    
    output$ridgermse<-renderPrint(ridgermse)
    
    output$ridgeartik<-renderPrint(head(ridgeartik,10))
    
    output$ridgeaic<-renderPrint(ridgeaic)
    
    output$ridgebic<-renderPrint(ridgebic)
    
    output$lassomodel<-renderPlot(plot(cv.fitlasso))
    
    output$optimallambda<-renderPrint(optimallambda)
    
    output$lambda1se1<-renderPrint(lambda_1SE1)
    
    output$lassomodel1<-renderPrint(coef(lassomodel))
    
    output$ypredictedlasso<-renderPrint(head(ypredictedlasso,10))
    
    output$lassorkare<-renderPrint(lassorkare)
    
    output$lassormse<-renderPrint(lassormse)
    
    output$lassoartik<-renderPrint(head(lassoartik,10))
    
    output$lassoaic<-renderPrint(lassoaic)
    
    output$lassobic<-renderPrint(lassobic)
    
    output$elasticmodel<-renderPlot(plot(cv.fitelasticnet))
    
    output$optlambda<-renderPrint(optlambda)
    
    output$lambda_1SE<-renderPrint(lambda_1SE)
    
    output$elasticmodel1<-renderPrint(coef(elasticmodel))
    
    output$ypredictedelasticnet<-renderPrint(head(ypredictedelasticnet,10))
    
    output$elasticrkare<-renderPrint(elasticrkare)
    
    output$elasticrmse<-renderPrint(elasticrmse)
    
    output$elasticartik<-renderPrint(head(elasticartik,10))
    
    output$elasticaic<-renderPrint(elasticaic)
    
    output$elasticbic<-renderPrint(elasticbic)
    
    output$tablo<-renderPrint(tablo)
    
    output$lmod2<-renderPrint(summary(lmod2))
    
    output$rmse1<-renderPrint(rmse1(predict(lmod2), veritrain2$VWAP))
    
    output$rmse2<-renderPrint(rmse1(predict(lmod2, veritest2), veritest2$VWAP))
    
    output$ucluplot<-renderPlot({par(mfrow=c(1,3))
        plot(Last~High,veritrain2)
        plot(High~Low,veritrain2)
        plot(Close~Last,veritrain2)})
    
    output$pcrmodel<-renderPrint(summary(pcrmodel))
    
    output$validationplot<-renderPlot(validationplot(pcrmodel,val.type = "RMSE",col="green"))
    
    output$pcrmse<-renderPrint(pcrmse)
    
    output$validationplot1<-renderPlot(validationplot(pcrmodel,val.type = "R2",col="orangered3"))
    
    output$pcrmse1<-renderPrint(pcrmse1)
    
    output$coef1<-renderPrint(coef(pcrmodel,ncomp=5))
    
    output$rmse3<-renderPrint(rmse1(predict(pcrmodel, ncomp=5), veritrain2$VWAP))
    
    output$rmse4<-renderPrint(rmse1(predict(pcrmodel, veritest2, ncomp=5), veritest2$VWAP))
    
    output$pcrCV<-renderPrint(pcrCV)
    
    output$plotcv<-renderPlot(plot(pcrCV,main="",col="red"))
    
    output$min1<-renderPrint(which.min(pcrCV$val))
    
    output$coefcv<-renderPrint(coef(pcrmodel1,ncomp=5))
    
    output$model1<-renderPrint(summary(model1))
    
    output$fit<-renderPrint(head(fit,10))
    
    output$resid<-renderPrint(head(resid,10))
    
    output$normalliksinamasi<- renderPrint(ols_test_normality(model1))
    
    output$plotnormallik<-renderPlot({par(mfrow=c(1,3)) 
        x2 <-residuals(model1)
        histogram <-hist(x2, breaks=10, density=10,col="darkgrey",xlab="Residuals", main="Histogram")
        abline(v=mean(x2), col="darkgreen", lwd=2)
        multiplier <- histogram$counts / histogram$density 
        mydensity <- density(x2) 
        mydensity$y <- mydensity$y * multiplier[1] 
        lines(mydensity,col="blue", lwd=2)
        xfit <- seq (min(x2), max(x2), length=40) 
        yfit <- dnorm(xfit, mean =mean(x2), sd = sd(x2))
        yfit <- yfit *diff(histogram$mids[1:2]) *length(x2) 
        lines(xfit, yfit, col="red", lwd=2)
        qqnorm(residuals(model1),ylab="residuals",main="QQPLOT",col="orange") 
        qqline(residuals(model1),col="darkblue")
        d <- density(x2) 
        plot(d,main = "Yogunluk Grafigi") 
        polygon(d, col="violet", border="blue")})
    
    output$ggplot<-renderPlot({ggplot(data=NSTS,mapping=aes(x=fit,y=resid))+ 
                                  geom_jitter(color="green")+ 
                                  geom_hline(yintercept=0,color="red")+ggtitle("RESID & FITTED ")+xlab(" FITTED ")+ylab("RESID")})
    
    output$bp1<-renderPrint(bptest(model1,data=NSTS))
    
    output$dw<-renderPrint(dwtest(VWAP~Volume+High+Low+Close+Last ,data=NSTS))
    
    output$plotm1<-renderPlot(ols_plot_resid_stud_fit(model1))
    
    output$plotm<-renderPlot({par(mfrow=c(1,4))
        plot(model1)})
    
    output$gozlem<-renderPrint(NSTS[c(8, 77,155), ])
    
    output$stud<-renderPrint(head(stud,10))
    
    output$stud1<-renderPrint(stud[which.max(abs(stud))])
    
    output$qt<-renderPrint({qt(0.05/(length(stud)*2) , (length(NSTS$VWAP)-6-1))})
    
    output$outliertest<-renderPrint(outlierTest(model1))
    
    output$hubermod<-renderPrint(summary(hubermod))
    
    output$hubermod1<-renderPrint(cbind(coef(model1),coef(hubermod)))
    
    output$halfnorm<-renderPlot(halfnorm(residuals(hubermod),3,ylab = "hubermod residuals"))
    
    output$studhubermod<-renderPrint(stud1[which.max(abs(stud1))])
    
    output$qt1<-renderPrint({qt(.05/(length(stud)*2),length(NSTS$VWAP)-6-1)})
    
    output$huberoutlier<-renderPrint(outlierTest(hubermod))
    
    output$bisquaremod<-renderPrint(summary(bisquaremod))
    
    output$biweights2<-renderPrint(biweights2[1:10, ])
    
    output$homojenvaryanslilik1<- renderPlot({plot(fitted(lmod),residuals(lmod), xlab = "fitted y" ,ylab = "residuals",col="purple",main="Artiklar-Tahmin Grafigi")
        abline(h=0,col="orange")}
        
    )}

shinyApp(ui, server)