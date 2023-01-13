powiadomienia = read.table("data_set.txt", header = TRUE)$Powiadomienia

#ZADANIE 1 - Miary tendencji centralnej:
#Średnia arytmetyczna
mean(powiadomienia)

#Dominanta
values <- unique(powiadomienia)
values[which.max(tabulate(match(powiadomienia, values)))]

#Mediana
median(powiadomienia)

#Kwartyl pierwszy
quantile(powiadomienia, probs = 0.25)

#Kwartyl trzeci
quantile(powiadomienia, probs = 0.75)

#ZADANIE 2 - MIARY ZRÓŻNICOWANIA
#Rozstęp
max(powiadomienia) - min(powiadomienia)

#Wariancja
var(powiadomienia)

#Odchylenie standardowe
sd(powiadomienia)

#Odchylenie ćwiartkowe
IQR(powiadomienia)/2

#Rozstęp międzykwartylowy
IQR(powiadomienia)

#ZADANIE 3 - współczynnik zmienności
wspolczynnik_zmiennosci <- sd(powiadomienia) / mean(powiadomienia)
wspolczynnik_zmiennosci

#ZADANIE 4 - wykres boxplot
boxplot(powiadomienia, 
        main = "Wykres boxplot liczby powiadomień", 
        ylab = "Liczba powiadomień")


#ZADANIE 5 - szereg rozdzielczy
#Liczba przypadków
n <- length(powiadomienia)

#Liczba klas
k <- round(sqrt(n), digits = 0)

#Szerokość przedziału klasowego
szer_klasy <- ceiling((max(powiadomienia) - min(powiadomienia)) / k)

min_vals <- seq(min(powiadomienia)-0.5, by = szer_klasy, length.out = k)
max_vals <- seq(min(powiadomienia)-0.5 + szer_klasy, by = szer_klasy, length.out = k)
srodek_klasy <- c()
licznosc_klasy <- c()
licznosc_skumulowana <- c()

i <- 1
while (i <= k) {
  srodek_klasy <- append(srodek_klasy, ((min_vals[i] + max_vals[i]) / 2))
  licznosc_klasy <- append(licznosc_klasy, sum(powiadomienia > min_vals[i] & powiadomienia < max_vals[i]))
  if(i == 1){
    licznosc_skumulowana <- append(licznosc_skumulowana, licznosc_klasy[i])
  } else {
    licznosc_skumulowana <- append(licznosc_skumulowana, licznosc_skumulowana[i-1]+licznosc_klasy[i])
  }
  i <- i + 1
}

szereg_rozdzielczy <- data.frame(NumerKlasy = c(1, 2, 3, 4, 5, 6))
szereg_rozdzielczy$PoczatekKlasy <- min_vals
szereg_rozdzielczy$KoniecKlasy <- max_vals
szereg_rozdzielczy$SrodekKlasy <- srodek_klasy
szereg_rozdzielczy$Licznosc <- licznosc_klasy
szereg_rozdzielczy$LiczSkumul <- licznosc_skumulowana
szereg_rozdzielczy

#ZADANIE 6 - Histogram
hist(powiadomienia, seq(min(powiadomienia)-0.5, by = szer_klasy, length.out = k+1),
     xlim = c(50, 350), 
     ylim = c(0, 14), 
     main = "Histogram powiadomień", 
     xlab = "Liczba powiadomień w ciągu dnia",
     ylab = "Liczba dni")

#ZADANIE 7 - miary rozstępu i zróżnicowania dla szeregu rozdzielczego
szereg_wartosci <- c()

i <- 1
while(i <= k){
  szereg_wartosci <- append(szereg_wartosci, 
                            rep(szereg_rozdzielczy$SrodekKlasy[i], szereg_rozdzielczy$Licznosc[i]))
  i <- i + 1
}

#Średnia arytmetyczna
mean(szereg_wartosci)

#Mediana
#Klasa w której znajduje sie mediana
klasa <- 2
mediana <- szereg_rozdzielczy$PoczatekKlasy[klasa] + 
  ((szer_klasy * (n/2 - szereg_rozdzielczy$LiczSkumul[klasa-1])) / szereg_rozdzielczy$Licznosc[klasa])
mediana

#Dominanta
#Klasa z największą licznością to klasa 1, a więc wyznaczenie dominanty algebraicznie nie jest możliwe
#klasa <- 1
#dominanta <- szereg_rozdzielczy$PoczatekKlasy[klasa] +
#  ((szereg_rozdzielczy$Licznosc[klasa] - szereg_rozdzielczy$Licznosc[klasa-1]) / 
#  ((szereg_rozdzielczy$Licznosc[klasa] - szereg_rozdzielczy$Licznosc[klasa-1]) + 
#     (szereg_rozdzielczy$Licznosc[klasa] + szereg_rozdzielczy$Licznosc[klasa+1])))
#dominanta

#Kwartyl pierwszy
#Klasa zawierająca kwartyl pierwszy
klasa <- 1
q1 <- szereg_rozdzielczy$PoczatekKlasy[klasa] + 
  ((szer_klasy*((n/4)-0))/szereg_rozdzielczy$Licznosc[klasa])
q1

#Kwartyl trzeci
#Klasa zawierająca kwartyl trzeci
klasa <- 3
q3 <- szereg_rozdzielczy$PoczatekKlasy[klasa] + 
  ((szer_klasy*((3*n/4)-szereg_rozdzielczy$LiczSkumul[klasa-1]))/szereg_rozdzielczy$Licznosc[klasa])
q3

#Wariancja
var(szereg_wartosci)

#Rozstęp
max(powiadomienia) - min(powiadomienia)

#Odchylenie standardowe
sd(szereg_wartosci)

#Rozstęp międzykwartylowy
q3 - q1

#Odchylenie ćwiartkowe
(q3-q1)/2

#INTERPRETACJA
#
# Średnia liczba powiadomień dziennie wyniosła 149.4 powiadomienia
# 
# Najczęściej pojawiającą się liczbą powiadomień jest 133
# Mediana jest równa 133, a więc 50% dni miało mniej powiadomień, a 50% więcej.
# Kwartyl pierwszy wynosi 113, czyli 25% najniższych obserwacji jest mniejsza niż 113
# Kwartyl trzeci wynosi 172.5, czyli 25% największych obserwacji jest większa niż 172.5
# Rozstęp danych to 213 - różnica między najniższą liczbą powiadomień, a najwyższą
# Rozstęp międzykwartylowy wynosi 59.5, czyli 50% najbardziej centralnych mieści się w zakresie 59.5
# Współczynnik zmienności wynosi ok. 35% czyli zmienność jest na przeciętnym poziomie
# 
# Szereg rozdzielczy wskazuje na silnie asymetryczny rozkład danych


