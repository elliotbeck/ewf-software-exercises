######################################################
#' EWF:Uebung 6, Aufgabe 3
#'
######################################################

# Arbeitsverzeichnis (individuell)
setwd("/path/tp/working/directory")


######################################################
#' Packages
######################################################

library(lmtest)
library(sandwich)
library(car)


######################################################
#' 3. a) Datenimport und deskriptive Statistik
######################################################
Data <- read.table("data/growth.csv", header = T, sep = ",")
head(Data)
summary(Data)

#' Die meisten Werte scheinen auf den ersten Blick vernünftig.
#' Nur die Variable oil nimmt ausschliesslich den Wert 0 an,
#' was der Tatsache geschuldet ist, dass es sich bei den vorliegenden Daten
#' um einen Teil eines grösseren Datensatzes handelt.
#' Die Variablen rev_coups und assasinations sind rechtsschief.
#' Der Handelsanteil ist eigentlich kein Anteil,
#' weil hier Exporte und Importe zusammengezählt werden.


######################################################
#' 3. b) Einfache lineare Regression
######################################################

Regression1 <- lm(growth ~ tradeshare, data = Data)
summary(Regression1)
plot(Regression1, 1)
plot(Regression1, 2)
plot(Regression1, 3)
plot(Regression1, 4)

plot(tradeshare, growth)
points(tradeshare[65], growth[65], pch = 16, col = "blue")
abline(Regression1, col = "red")


#' Das geschätzte Modell impliziert erst einmal einen positiven Zusammenhang
#' zwischen dem durchschnittlichen jährlichen Wachstum und der Offenheit der Wirtschaft
#' - gemessen als durchschnittlicher Handelsanteil. Ein um einen Prozentpunkt höherer Handelsanteil
#' führt zu einer um 0.01 * 2.3064 = 0.02 Prozentpunkte höheren Wachstumrate
#' Das Residuendiagramm zeigt einen deutlichen Zusammenhang zwischen der Lage der Residuen
#' und den vorhergesagten Werten. Die vorangegangene Modellinterpretation ist also so nicht mehr haltbar.
#' Hierbei handelt es sich um den Kleinstaat Malta.
#' Der extrem hohe Handelsanteil von 200 % fällt sofort auf. Ein Grund ist darin zu sehen,
#' dass Exporte und Importe für Malta als "Durchgangshafen" eine andere Rolle spielen:
#' Viele Importe sind für die sofortige Ausschiffung in andere Länder bestimmt.
#' Aufgrund dieser Besonderheit könnte man sagen, dass Malta nicht in die Regression gehört,
#' wenn man an dem "gewöhnlichen" Zusammenhang zwischen Offenheit und Wachstum interessiert ist.


######################################################
#' 3. c) Bereinigte einfache lineare Regression
######################################################

Regression1.2 <- lm(growth[-c(65)] ~ tradeshare[-c(65)], data = Data)
summary(Regression1.2)
plot(Regression1.2, 1)
plot(Regression1.2, 2)
plot(Regression1.2, 3)
plot(Regression1.2, 4)

plot(tradeshare[-65], growth[-65])
abline(Regression1.2, col = "red")



#' Der Effekt des Handelsanteils ist nun stark reduziert, da Malta auch ein Staat ist,
#' der eine überdurchschnittliche Wachstumsrate im Beobachtungszeitraum aufweist.
#' Das Streuungsdiagramm hat sich nun stark verbessert. Auch wenn die rote Linie keine perfekte Gerade ist,
#' so scheint doch die Beziehung annäherend linear.
#' Damit ist das Modell interpretierbar und für Vorhersagen nutzbar.


######################################################
#' 3. d) Multiple lineare Regression
######################################################

Regression2 <- lm(growth ~ tradeshare + yearsschool + rev_coups + assasinations + rgdp60, data = Data)
summary(Regression2)
plot(Regression2, 1)
plot(Regression2, 2)
plot(Regression2, 3)
plot(Regression2, 4)

#' In der erweiterten Regression ändert sich der geschätzte Effekt von tradeshare kaum
#' relativ zu den Resultaten des einfachen Modells ohne Malta.
#' Die Beziehung ist näherungsweise linear, selbst im urspünglichen Datensatz.
#' Dies ist ein Hinweis auf den in der Vorlesung angesprochenen Sachverhalt,
#' dass Ausreisser in einer bivariaten Regression, keine Ausreisser in einer erweiterten multiplen Regression sein müssen.


######################################################
#' 3. e) Korrelation
######################################################

vcov.Regression2 <- vcovHC(Regression2, "HC3")
vcov.Regression2

correlation <- vcov.Regression2[2, 3] / (sqrt(vcov.Regression2[2, 2] * vcov.Regression2[3, 3]))
correlation
