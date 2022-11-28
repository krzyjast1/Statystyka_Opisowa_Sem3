powiadomienia = read.table("data_set.txt", header = TRUE)$Powiadomienia

powiadomienia

#ZADANIE 1 - Miary tendencji centralnej:
#Średnia arytmetyczna
print("Średnia arytmetyczna: ")
mean(powiadomienia)

#Dominanta
print("Dominanta: ")
values <- unique(powiadomienia)
values[which.max(tabulate(match(powiadomienia, values)))]

#Mediana
print("Mediana:")
median(powiadomienia)

#Kwartyl pierwszy
print("Kwartyl pierwszy:")
quantile(powiadomienia, probs = 0.25)

#Kwartyl trzeci
print("Kwartyl trzeci:")
quantile(powiadomienia, probs = 0.75)

#ZADANIE 2 - MIARY ZRÓŻNICOWANIA
#Rozstęp
print("Rozstęp:")
max(powiadomienia) - min(powiadomienia)

#Wariancja
print("Wariancja:")
var(powiadomienia)

#Odchylenie standardowe
print("Odchylenie standardowe:")
sd(powiadomienia)

#Odchylenie ćwiartkowe
IQR(powiadomienia)/2

#Rozstęp międzykwartylowy
IQR(powiadomienia)

#ZADANIE 3 - współczynnik zmienności
wspolczynnik_zmiennosci <- sd(powiadomienia) / mean(powiadomienia)

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
szer_klasy <- (max(powiadomienia) - min(powiadomienia)) / k

szereg <- cut(powiadomienia, seq(min(powiadomienia), max(powiadomienia), by = szer_klasy), include.lowest = TRUE)
table(szereg)

#ZADANIE 6 - Histogram
hist(powiadomienia, seq(min(powiadomienia), max(powiadomienia), by = szer_klasy), 
     include.lowest = TRUE)


