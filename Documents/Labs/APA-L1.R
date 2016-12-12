#####################################
## APA Laboratori 1 (TEMA 1)       ##
## Ridge and standard regression   ##
## version of September, 2016      ## 
#####################################


## Començarem fent bàsicament l'exemple introductori del TEMA 1 (ajust polinòmic)

set.seed (7) # per igualar els resultats de tothom (esperem!)

# Variables de control

N <- 20
a <- 0
b <- 1
sigma.quadrat <- 0.3^2

# Noteu que si poseu una expressió entre parèntesis, R l'avalúa i mostra el resultat (per això poso tants parèntesis). Proveu:

pep <- 3

(pep <- 3)

# Generació de la mostra TR (de training) de mida N, inputs x_n uniformes en (0,1)
# El sort() és per claredat, no té cap importància
# Noteu que a classe era N=10 per pura simplicitat del dibuix a la pissarra!

x <- sort(runif(N, a,b))
t <- sin(2*pi*x) + rnorm(N, mean=0, sd=sqrt(sigma.quadrat))
(sample <- data.frame(input=x,target=t))

dim(sample)
attach(sample)

# Dibuixem les dades i la funció "target" (la part regular, que voldríem detectar)
plot(input,target, lwd=3, ylim = c(-1.1, 1.1))
curve (sin(2*pi*x), a, b, add=TRUE, ylim = c(-1.1, 1.1))

# Generació de la mostra de validacio (VA) de mida N.valid, inputs x equiespaiats en (a,b)
# la usarem per fer prediccions

N.valid <- 1000

x.valid <- sort(runif(N.valid, a,b))
t.valid <- sin(2*pi*x.valid) + rnorm(N.valid, mean=0, sd=sqrt(sigma.quadrat))
valid.sample <- data.frame(input=x.valid,target=t.valid)

##################
# Regressió lineal amb grau M=1, per començar, model lineal

model <- glm (target ~ input, data = sample, family = gaussian)

model # ens diu que el model és y(x) = -1.606·x + 1.133

# fem-lo predir les dades TR (les usades per trobar el model) i calculem l'error quadràtic mitjà

(prediccio <- predict(model, data=sample))
abline(model,col="red", lwd=2)
(mean.square.error <- sum((target - prediccio)^2)/N)

# alternativament, glm() ens el calcula
(mean.square.error <- model$deviance/N)

# si mireu el help de glm() fent ?glm --que és una rutina molt potent i general, i serveix per molts tipus de regressions-- veureu que diu:

# "deviance": up to a constant, minus twice the maximized log-likelihood

# això ho hem vist a classe, en el cas de regressió lineal (el cas que ens ocupa), hem
# obtingut la suma dels error quadràtics (la deviance, en argot estadístic), per maximització del "minus log-likelihood".

# la frase "Where sensible, the constant is chosen so that a saturated model has deviance zero" vol dir simplement que un model interpolador (que prediu exactament els targets) tindrà deviance zero.

# com a curiositat, 'null.deviance' és l'error d'un model "null" (que només te el terme
# independent), i correspón al model M=0; l'AIC el veurem més endavant (o no)

model$null.deviance/N

# Dit tot això, reprenem el fil i tornem a l'error quadràtic mitjà. Resulta que és convenient treballar amb la seva arrel, per obtenir així la "llargada" del vector d'errors

(root.mse <- model$deviance/N)

# i és encara millor normalitzar l'error, dividint per la variança dels targets, obtenint el que s'anomena NRMSE (normalized root mean squared error)

(norm.mse <- model$deviance/((N-1)*var(target)))


# Interpretació:
#   0) Òbviament NMSE >= 0, però no té cota superior definida
#   1) un model constant que predigui la mitjana dels targets (de fet, el millor model constant), tindria un NMSE = 1.
#   2) models amb NRMSE > 1 són per tant horribles; un model es comença a considerar acceptable a partir de NMSE < 0.2
#   3) observeu que, notant que l'error quadràtic no és més que l'estimació de la variança dels targets, el NMSE es pot veure com la fracció de la variança dels targets no explicada (capturada) per les prediccions del model. Per exemple, un NMSE = 0.13 correspon a un model capaç d'explicar el 87% de la variabilitat del target.

# El nostre model M=1 explica per tant només el 58% de la variabilitat del target.

#######################
# Regressió cúbica (polinomi de grau M=3, continua sent un model lineal, penseu-lo)

model <- glm (target ~ poly(input, 3, raw=TRUE), data = sample, family = gaussian)

# veiem-ne el model: summary() dóna més informació

summary(model)

# Deviance Residuals són les diferències al quadrat punt a punt. El que se'ns mostra són els estadístics (min, median, etc) de les 20 que tenim.

# els coeficients del polinomi (del model) són:

model$coefficients

# o sigui el polinomi és y(x) = 17.5885350·x^3 -26.5898537·x^2 -8.6171010·x + 0.3264768

# Std. Error és la incertesa de cada coeficient, relativa al valor del coeficient; en aquest cas són molt altes, cosa que és deguda a que només tenim 20 punts 

# La darrera columna és un test sobre la probabilitat de que cada coeficient sigui en realitat zero (i per tant, l'entrada x corresponent no té relació amb el target). Per interpretar-ho, mireu el codi inferior:

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Entrades x amb '***' són imprescindibles, després '**', etc. Tot això, suposant, és clar, que el model és correcte (que no és el cas, com sabem, perquè l'hem dissenyat nosaltres)


# Ara ho dibuixem tot de nou

plot(input,target, lwd=3, ylim = c(-1.1, 1.1))                # dades de TR
curve (sin(2*pi*x), a, b, add=TRUE, ylim = c(-1.1, 1.1))      # part regular a modelar
points(input, predict(model), type="l", col="red", lwd=2)     # el model obtingut

# calculem l'error normalitzat NRMSE a la mostra de TR
(norm.mse <- model$deviance/((N-1)*var(target)))

# gairebé 84%, ha millorat força ...

# Ara veurem com calcular l'error normalitzat a la mostra de validacio
# hem de fer-ho explícitament, ja que predict() només dóna les prediccions, no els errors

# fem un cop d'ull primer a les dades de VA
plot(valid.sample$input, valid.sample$target)

# i calculem l'error
prediccions <- predict (model, newdata=valid.sample)
(norm.mse.valid <- sum((valid.sample$target - prediccions)^2)/((N.valid-1)*var(valid.sample$target)))

# Quan un model és incorrecte, el seu error de predicció és alt. En cas de models sobreajustats, sol ser molt superior al de TR. En cas de models infraajustats (com és el cas ara), tots dos són elevats i similars. Aquest és un model raonable i per tant tos dos errors són baixos i similars.

######################################
# Ara farem la mateixa simulació que a classe, fent regressió polinòmica, de graus M des de p a q (poden ser arbitraris, els fixarem en 1 i N-1)

p <- 1
q <- N-1

coef <- list()
model <- list()
norm.mse.train <- NULL
norm.mse.valid <- NULL

for (i in p:q)
{
  model[[i]] <- glm(target ~ poly(input, i, raw=TRUE), data = sample, family = gaussian)

  # desem coeficients del polinomi (del model) i els error de training i validacio

  coef[[i]] <- model[[i]]$coefficients
  norm.mse.train[i] <- model[[i]]$deviance/N
  
  prediccions <- predict (model[[i]], newdata=valid.sample)  
  norm.mse.valid[i] <- sum((valid.sample$target - prediccions)^2)/((N.valid-1)*var(valid.sample$target))
}

# ho arrepleguem tot en una matriu

resultats <- cbind (Grau=p:q, Coeficients=coef, Error.train=norm.mse.train, Error.valid=norm.mse.valid)

# dibuixem 6 dels models (graus 1,2,3,4,9,19) contra les dades de training

par(mfrow=c(2, 3))                # això crea una graella (grid) de 2x3

graus <- c(1,2,3,4,9,19)

for (i in graus)
{
  plot(input,target, lwd=3)
  curve (sin(2*pi*x), a, b, add=TRUE)
  abline(0,0)
  points(input, predict(model[[i]]), type="l", col=25+i, lwd=2)
  title (main=paste('Grau',i))
}

# Ara dibuixem les prediccions dels mateixos models contra les dades de validacio
# la funció a modelar està sempre en groc, per referència

for (i in graus)
{
  plot(valid.sample$input, valid.sample$target)
  curve (sin(2*pi*x), a, b, add=TRUE, col='yellow',lwd=2)
  points(valid.sample$input, predict(model[[i]], newdata=valid.sample), type="l", col=25+i, lwd=2)
  title (main=paste('Grau',i))
}

# Ara farem una gràfica similar a la de classe: error de TR i error de VA junts, en funció del grau M; observarem els fenòmens de sobreajust i infraajust molt clarament

# Ometem els graus a partir del 17 perquè els valors es disparen i caurien fora del dibuix; però podeu mirar els resultats numèrics a la matriu:

(r <- data.frame(resultats[,-2]))

par(mfrow=c(1,1))

# preparar un plot buit
plot(1:20, 1:20, xlim=c(1,16), ylim=c(0,1.5), type = "n", xlab="Grau", ylab="", xaxt="n")  

# omplir-lo
axis(1, at=1:16,labels=1:16, col.axis="red", las=2)
points (x=r$Grau[1:16], y=r$Error.train[1:16], type='b', pch=0)
points (x=r$Grau[1:16], y=r$Error.valid[1:16], type='b', pch=3, lwd=3)

legend(x="topleft", legend=c("Error.TR", "Error.VA"), pch=c(0, 3), lwd=c(1, 3))

# Per un pèl, el menor error de VA és per M=3, per tant el métode vist a classe seleccionaria el model correcte (se'n diu "model selection"). Noteu que el "model selection" és bàsicament la detecció de la complexitat adequada al problema (la millor en aquest cas, --és a dir, dins els polinomis-- correspón a un de cúbic). Més endavant tractarem el tema de la "complexitat" d'un model des d'un de vista més general.


# Ara investigarem els propis coeficients: veurem que tot coeficient del mateix grau va fent-se gran (en valor absolut) a mesura que puja el grau màxim (tret del coeficient de grau 0 o Intercept, que més o menys es manté constant, donat que intenta seguir la mitjana dels targets, que no varia)

# Estudiem només fins al grau 10, per fer la taula manegable

coefs.table <- matrix (nrow=10, ncol=9)

for (i in 1:10)
  for (j in 1:9)
    coefs.table[i,j] <- coef[[j]][i]

coefs.table

# si cap a la finestra, movem-ne els marges: és important veure la taula d'una sola peça

# Interpretació:
#   0) Les files són els coeficients dels diferents graus 1..M i les columnes els graus M; per exemple, [3,4] és el coeficient de grau 3 del polinomi M=4
#   1) tot coeficient del mateix grau va fent-se gran (en valor absolut) a mesura que puja el grau màxim; per exemple, el coeficient de grau 2 passa de 1 a 8-9, 22-32, 69, 74, etc.
#   2) Això implica que a mesura que creix M, *tots* els coeficients es fan molt grans. I ens suggereix que, tot i sobreestimar el grau M, podríem controlar l'ajust si poguéssim limitar aquest creixement: això porta a la tècnica de regularització (que en estadística es coneix com a "ridge regression")
 
##########################################################################
# Ara comprovarem que el sobreajust disminueix amb N !!!
##########################################################################

# generació de la mostra de training de mida N, inputs x equiespaiats en (a,b)

N.big <- 100                       # abans eren 20, la resta és exactament igual
x <- seq(a,b,length.out=N.big)
t <- sin(2*pi*x) + rnorm(N.big, mean=0, sd=sqrt(sigma.quadrat))
big.sample <- data.frame(input=x,target=t)
attach(big.sample)

par(mfrow=c(1, 1))
plot(big.sample$input,big.sample$target, lwd=3)

model <- glm(target ~ poly(input, 9, raw=TRUE), data = big.sample, family = gaussian)

root.mse.train <- sqrt(model$deviance/N.big)

prediccions <- predict (model, newdata=valid.sample)  
root.mse.valid <- sqrt(sum((valid.sample$target - prediccions)^2)/((N.big-1)*var(valid.sample$target)))

curve (sin(2*pi*x), a, b, add=TRUE)
points(big.sample$input, predict(model), type="l", col="red", lwd=2)

# Sí: estem modelant el TR amb M=9 i no sobreajusta tant (compareu-lo amb la gràfica que hem generat abans)

# Conclusió: a mesura que N es fa gran el problema del sobreajust s'alleuja força. En
# general, però, el concepte "N gran" depèn de quantes variables x usem per modelar i, a més, no és usual tenir control sobre N; per tant, en la pràctica caldrà restringir la mida dels coeficients.

##########################################################################
# Ara passem a regressió ridge per re-ajustar el TR més gran (N=100)
##########################################################################

library(MASS)

par(mfcol=c(1,1))

# Aquesta és la idea de la regularització; partim d'una especificació de model sobradament complexe (M=12) i limitem explícitament la mida (en valor absolut) dels coeficients, via el paràmetre lambda (constant de regularització)

# La gràcia ara és estimar un bon valor per lambda: veurem més endavant que es pot fer de manera molt eficient quan el model és lineal. Provem doncs dins una seqüència de valors molt llarga:

lambdes <- seq(0.001,0.5,0.001)

length(lambdes)

# aquest seria el model "estàndar" (sense regularitzar)
model <- glm (target ~ poly(input, 12, raw=TRUE), data = sample, family = gaussian)

# aquest seria el model regularitzat
model.ridge <- lm.ridge (model, lambda = lambdes)
plot(model.ridge, lty=1:3)

# El que veiem a la gràfica (que és molt xula) és com tots els coeficients (dels graus 1 al 12) se'n van a zero a mesura que regularitzem més (lambdes més altes)

# Igual que la crida anterior, però ara seleccionem la millor lambda (que és la que té un error quadràtic regularitzat menor); podríem haver fet només aquesta crida directament:

select( lm.ridge(model, lambda = lambdes) )

# hi ha varis criteris per seleccionar la millor lambda de manera eficient; a classe (quan toqui) veurem el GCV

# dóna lambda = 0.008, per tant ajustem un nou model (que serà el definitiu) amb aquest valor:

model.final <- lm.ridge (model,lambda=0.008)

# Compareu els dos grups de coeficients del nostre model M=12 (estàndar i regularitzats)

coef(model)         # M=12 (estàndar)

coef(model.final)   # M=12 (regularitzat)

sqrt(sum(coef(model)^2))

sqrt(sum(coef(model.final)^2))

# per tenir una idea millor, calcularem el quocient entre uns i altres. Com sabreu de classe, el coeficient de grau 0 (Intercept) no es regularitza; per això model.final$coef no ens el dóna, però coef() sí. En general és millor usar els mètodes que permeten accedir als camps d'un objecte que accedir-hi "directament" via $

# Millor fem un plot logarítmic, per veure els ordres de magnitud
plot (log10(abs(coef(model) / coef(model.final))), xlim=c(1,13), xlab="Grau", ylab="", main="Log10 quocient", type="b")

# Queda per veure que el model regularitzat no és pitjor que l'inicial (no sigui que per penalitzar-li els coeficients ara sigui un model pitjor)

# primer calculem l'error pel mètode estàndar (sense regularitzar) com fins ara

prediccions.classic <- predict (model, newdata=valid.sample)
(NMSE.VA.classic <- sum((valid.sample$target - prediccions.classic)^2)/((N.valid-1)*var(valid.sample$target)))

# dóna terrible, com ja imaginavem

# Malauradament la funció predict() no accepta un objecte lm.ridge() --això és "culpa" dels programadors de lm.ridge(), que no van fer-ho--, no de predict(); total, que ho hem de fer nosaltres:

# primer posem els coeficients en format "xulo", amb els noms de classe

(c <- setNames(coef(model.final), paste0("c_", 0:12)))

# ara calculem les potències (els graus) per les dades x

pots <- outer (X=valid.sample$input, Y=0:12, FUN="^")

# ara calculem el propi polinomi, multiplicant coeficients per potències i sumant-ho tot (és a dir, calculem el producte escalar entre coeficients i potències)

prediccions.regul <- pots %*% c

# finalment ja podem calcular l'error d'aquestes prediccions

(NMSE.VA.regul <- sum((valid.sample$target - prediccions.regul)^2)/((N.valid-1)*var(valid.sample$target)))

# dóna 0.2368707, mentre que el model anterior, trobat per prova i error de M donava 0.2335588; és a dir, són dos models igualment bons, però el regularitzat s'ha trobat mitjançant un mecanisme de control de complexitat que és independent del métode (només afecta la funció d'error) i el paràmetre lambda és més fàcil d'ajustar i menys sensible a fluctuacions