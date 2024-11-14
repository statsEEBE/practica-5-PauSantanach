#resum previ a la classe:

#per al Q2

#Tindrem Distribucions poblacionals:

#1r pas a fer--> identificar el model.

#De models tenim:
#Per a Variable Discreta:
#--> Binomial: pensem en una ENQUESTA (repetim varis ensajos de bernoulli)
#--> Binomial Negativa: pensem en un ensaig de "RESISTÈNCIA" (Y=nº de fallos abans de n encerts)
#--> Hipergeomètrica
#--> Poisson
#
#Per a Variable Continua
#-->Uniforme
#-->Exp
#-->Normal

#2 Identifiquem els paràmetres i intentem solucionar el problema

#Parlem una mica de la distribució normal.

#per això alhora farem la pregunta 1 de la pràctica 5

#ens diuen que tenim una normal per tant ja hem identificat la distribució poblacional (NORMAL)
#els paràmetres tb ens els donen:

mu<-95.3 #mitjana
sigma<-5.7 #desviació típica(si ens donessin la variança ens hauriem de quedar amb l'arrel quadrada)

#dibuixem la distribució per veure coses:
curve(dnorm(x,mean=mu,sd=sigma),xlim=c(80,120),col="red")

#fem ara alguna simulació per veure +coses:

rnorm(4,mean=mu,sd=sigma) #aquí hem fet una simulació de que extraiem 4 cables.

#un estadístic és una funció de la mostra que prenem, en la pregunta 1 a) ens demanen coses sobre el següent estadístic: suma de resistències de la mostra (i tenim que la mostra és de 4)
#definint Y com a aquesta nova variable aleatòria (la suma de les resistències de lamostra), ens estan demanant l'esperança(la mitjana)
Y<-function(i){sum(rnorm(4,mean=mu,sd=sigma))} #Fiquem el sum perquè com hem dit la nostra nova variable aleatòria és la SUMA de les resitències de la mostra 
Y(1) #si executem això ens dona un valor--> si repetim això molts cops podem veure cap a on tendeix


sapply(1:5, Y) #sapply fa un bucle --> obtenim 5 resultats --> podem fer la mitjana d'això
mean(sapply(1:5, Y)) #en aquest cas tindriem que la mitjana de 5 cops és això

#anem-ho a fer 100000 cops ara:
y100000<-sapply(1:100000,Y)
mean(y100000) #si anem executant aquestes últimes 2 linies varis cops veiem que ara només varia uns decimals--> però volem ser més exactes, necessitem fer-ho infinits cops per obtenir l'esperança exacte
hist(y100000) #fem això per veure com s'assembla a la distribució norma

#però no podem fer l'infinit: tenim un teorema
#si X-->N(mu,sigma^2)
#i tenim que sum(Xi) = Y (suma mostral de n elements de X)
#llavors Y-->(n*mu,n*sigma^2) és a dir E(Y) = n*mu (sent mu la mitjana de X) i V(Y)= n*sigma^2

hist(y100000, freq=FALSE)
curve(dnorm(x,4*mu,2*sigma), col = "red", add=TRUE)

#ara en vermell veiem la distribució normal bona

#així doncs la resposta a la nostra pregunta és 
resposta<-4*mu
resposta
#veiem que el que feiem abans tendia cap a aquest resultat si haguessim seguit augmentant el número

#pregunta b
#ara enlloc de 4 cables de mostra tenim 100, però ens tornen a demanar coses sobre la SUMA de resistències de la mostra
#així doncs la nostra resposta, seguint el teorema serà


100*sigma^2
#podríem fer el que hem fet abans si no recordem al teorema però millor ficar això i fora

#pregunta c
#ens diuen que seleccionant un cable quina és la probabilitat de que la seva resistència NO sigui menor a 103 ( és a dir, que sigui major o igual a 103)
#això ho calculem fent:
1-pnorm(103,mu,sigma) #i això directament ja és la resposta

#pregunta d

#ara ens demanen que havent agafat 4 cables otravez, quina és la probabilitat de que la MITJANA de les resistències de la mostra sigui menor a 98

#Ara tenim que la nova variable que li direm Xbar ha de ser la mitjana de la mostra:
Xbar<- function(i){mean(rnorm(4,mean=mu,sd = sigma))}
Xbar100000<-sapply(1:100000,Xbar)
hist(Xbar100000)

mean(Xbar100000<98)

hist(Xbar100000,freq =FALSE)
curve(dnorm(x,mu,sigma/sqrt(4)),add= TRUE,col="red")

#el teorema diu : Si X-->N(mu,sigma^2) llavors Xbar-->N(mu,sigma^2/n)
#per R haurem de posar sigma/sqrt(n)


#per tant podem calcular el què ens demanen com:
pnorm(98,mu,sigma/sqrt(4))#veiem que ja és un valor força semblant al que haviem calculcat sense fer servir el teorema.




#pregunta e)--> error a l'enunciat, enlloc de 98, hem de ficar 32
#ara ens demanen la probabilitat de que la variança de les resisències de la mostra (variança mostral) sigui > que 32 (ara tornem a tenir 100 mostres, osigui 100 cables)

Ssq<-function(i){var(rnorm(100,mean=mu,sd=sigma))}
Ssq100000<-sapply(1:1000000,Ssq)

hist(Ssq100000,freq = FALSE)
mean(Ssq100000>32)
#això és la demostració sense el teorema,
#amb el teorema: 
#el teorema diu: si X-->N(mu,sigma^2) W->chisq(n-1)-->en R dchisq(W,n-1) i W = (s^2*(n-1))/sigma^2  (chi al quadrat)

hist(Ssq100000 * (100-1)/sigma^2,freq=FALSE)
curve(dchisq(x,100-1),add=TRUE,col = "red") #grafiquem i tal per veure coses

#en l'ex ens demanen: P(s^2>32) --> P(s^2(n-1)/sigma^2 > 32*(n-1)/sigma^2) -->apliquem el teorema 
#llavors fixem-nos que tenim P(W>32*(n-1)/sigma^2), que això a r es calcula com:
W = 32*(100-1)/sigma^2 #definim W
1-pchisq(W,100-1) #això ja és la resposta al problema. restem el comando de dchisq a 1 perquè la probabilitat és de que la variança sigui +GRAN que 32



