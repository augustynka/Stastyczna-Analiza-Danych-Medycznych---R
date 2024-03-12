# Stastyczna-Analiza-Danych-Medycznych---R
Analiza Statystyczna Danych Medycznych - R


**Wymagane pakiety, aby uruchomić program**
→ car, Hmics, dunn.test, FSA, ggpubr, ggplot2, dplyr


Dane do programu powinny znajdować się w formacie csv, a pierwsza kolumna powinna zawierać nazwę grupy, do której należą wiersze. Realizowany program jest stworzony do analizy 2 lub więcej grup badawczych.

**Tryb wsadowy**
“ścieżka do R” CMD BATCH  - -vanilla “--args ścieżka do pliku z danymi.csv” “ścieżka do programu”


**Opis i działanie programu wraz z przykładami**
Program generuje raport w formacie txt o nazwie “projekt_raport.txt”
Program po załadowaniu danych, przechodzi do podziału na grupy oraz uzupełnieniu brakujących danych średnią dla określonych kolumn w określonych grupach. Zbierane to są informacje o brakach i umieszczane w raporcie:


Usuwanie brakujących danych nie jest dobrym rozwiązanie,  dlatego dane uzupełnione są  dopasowując je do modelu regresji na zbiorze kompletnych danych (dzięki impute).
Następnie dla każdej kolumny numerycznej program wypisuje wartości odstające oraz generuje wykresy do pliku pdf (WARTOSCI_ODSTAJACE.pdf) dołączonym do sprawozdania. Dzięki temu zastosowaniu można łatwo odczytać ile takich wartości występuje, ocenić różnice.



Następnie program przechodzi do statystyk dla każdej badanej grupy i wypisuje je do raportu.

Spełnienie założenia o zgodności z rozkładem normalnym jest często wymogiem do zastosowania odpowiednich testów statystycznych. Dlatego przechodzimy do testu rozkładu normalnego. Program oblicza p$value przy pomocy Shapiro-Wilk normality test. Dodatkowo oprócz adnotacji do raportu o otrzymanych wynikach generowane są wykresy (pozwalające na sprawdzenie zgodności z rozkładem normalnym wykresy gęstości).

W przypadku niektórych testów wymagana jest zarówno zgodność z rozkładem normalnym jak i wymagane jest spełnienie założenia o homogeniczności (jednorodności) wariancji. Dlatego zastosowano test Levene’a. Jeśli wartość p-value > 0.05 oznacza to, że dane są zgodne z założeniem o jednorodności wariancji.
Następnie dochodzi do rozróżnienia dalszego przebiegu ze względu na przeprowadzane testy dla 2 lub więcej grup niezależnych.
Testy statystyczne dla 2 grup niezależnych
Jeśli dane nie spełniają założenia o zgodności z rozkładem normalnym (p-value < 0.05) do analizy porównawczej wykonuje się testy nieparametryczne. Wykonano test Wilcoxona. Jeśli wartość p-value jest mniejsza niż poziom istotności 0.05 możemy stwierdzić, że istnieją znaczące różnice między grupami. 
Jeśli dane spełniają założenie o zgodności z rozkładem normalnym (p-value > 0.05) oraz spełniają założenie o jednorodności wariancji (p-value > 0.05) zastosowano test t-Studenta.
Jeśli dane spełniają założenie o zgodności z rozkładem normalnym (p-value> 0.05), ale nie spełniają założenia o jednorodności wariancji (p-value < 0.05) zastosowano test Welcha. 
Testy statystyczne dla >2 grup niezależnych
Jeśli dane nie spełniają założenia o zgodności z rozkładem normalnym (p-value < 0.05) do analizy porównawczej wykorzystuje się testy nieparametryczne. Wykonano test Kruskala-Wallisa (tak samo w przypadku, gdy dane są zgodne z rozkładem normalnym, ale nie spełniają założenia o jednorodności wariancji)
Gdy wartość p-value jest mniejsza niż poziom istotności 0.05 możemy stwierdzić, że istnieją znaczące różnice między grupami. Jeśli występują istotne statystycznie różnice pomiędzy grupami, należy dokładnie określić pomiędzy którymi grupami występują te różnice. Dlatego zastosowano Dunn test.
Jeśli dane spełniają założenie o zgodności z rozkładem normalnym (p-value > 0.05) oraz spełniają założenie o jednorodności wariancji (p-value > 0.05) zastosowano  parametryczny test ANOVA. Gdy wartość p-value jest mniejsza niż poziom istotności 0.05 możemy stwierdzić, że istnieją znaczące różnice między grupami. Dlatego zastosowano test Tukeya.


**Ocena zależności pomiędzy parametrami**
Ocena istnienia i siły korelacji pomiędzy wybranymi parametrami, a także określenie kształtu i kierunku tej zależności stanowią ważną część analizy statystycznej. Do przeprowadzenia testu korelacji służy funkcja cor.test(), użyto metody spearman (współczynnik korelacji rangowej Spearmana jest nieparametrycznym odpowiednikiem współczynnika korelacji Pearsona)
Siła korelacji została zinterpretowana zgodnie z poniższymi przedziałami:
- −1 < r ≤ −0.7 bardzo silna korelacja ujemna
- −0.7 < r ≤ −0.5 silna korelacja ujemna
- −0.5 < r ≤ −0.3 korelacja ujemna o średnim natężeniu
- −0.3 < r ≤ −0.2 słaba korelacja ujemna
- −0.2 < r < 0.2 brak korelacji
- 0.2 ≤ r < 0.3 słaba korelacja dodatnia
- 0.3 ≤ r < 0.5 korelacja dodatnia o średnim natężeniu
- 0.5 ≤ r < 0.7 silna korelacja dodatnia
- 0.7 ≤ r < 1 bardzo silna korelacja dodatni


