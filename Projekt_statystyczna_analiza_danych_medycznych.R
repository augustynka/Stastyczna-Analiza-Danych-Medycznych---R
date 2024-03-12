#!/usr/bin/env Rscript
#PROJEKT ZE STATYCZTYNEJ ANALIZY DANYCH - ALICJA AUGUSTYNIAK
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Należy podać jednej argument wejściowy")
}
dane <- read.csv2(file = args[1], header=TRUE, sep=",")
sink ("projekt_raport.txt")
library(Hmisc)
library(dplyr)
library(ggplot2)
library(dunn.test)
library(FSA)


#BRAKUJCE DANE
brakujace <- colSums(is.na(dane))
cat("\nDanych brakuje w kolumnach:\n")
brakujace

sumabrakujacych <- sum(is.na(dane))
cat("\nSuma brakujących danych:")
sumabrakujacych

dokladnebrakujace <- which(is.na(dane), arr.ind = TRUE)
cat("\nDokładnie brakuje danych:\n")
dokladnebrakujace

grupy <-split(dane, dane[[1]])
cat("Dane są uzupełnianie średnią dla danej kolumny w grupie badawczej.")
#UZUPELNIENIE BRAKÓW
for (i in 1:length(grupy)) {
  for (j in 2:ncol(grupy[[i]])) {
    if(is.numeric(grupy[[i]][[j]])){
      grupy[[i]][[j]] <- impute(grupy[[i]][[j]], fun=mean)
    }
  }
}
dane <- NULL
for (i in seq_along(grupy)) {
  dane <- rbind(dane, grupy[[i]])
}

cat("\n\n------------------------WARTOŚCI ODSTAJĄCE------------------------\n\n")

#BOXPLOTY/WARTOŚCI ODSTAJĄCE
pdf("WARTOSCI_ODSTAJACE.pdf")
for (i in 1:length(grupy)) {
  dane_grupa <- grupy[[i]]
  
  boxplot(dane_grupa[, sapply(dane_grupa, is.numeric)],
         main = paste("Grupa", names(grupy)[i]),
          ylab = "Wartości",
          xlab = "Zmienne")
  
  for (j in 1:ncol(dane_grupa)) {
    if (is.numeric(dane_grupa[[j]])) {
      w_odstepne <- boxplot.stats(dane_grupa[[j]])$out
      nazwy_kolumn <- names(dane_grupa)[which(dane_grupa[[j]] %in% w_odstepne)]
      if (length(nazwy_kolumn) > 0) {
        cat("Grupa", i, ", zmienna", names(dane_grupa)[j], ", wartości odstające:", w_odstepne, "dla kolumny:", names(dane_grupa)[j], "\n")
      }
    }
  }
}
dev.off()


########################################################################################################

#STATYSTYKI
cat("\n\n------------------------STATYSTYKI------------------------\n\n")
statysty <- lapply(grupy, function(grupa){
  sapply(grupa[,sapply(grupa, is.numeric)], function(x) c("1st Qu" = quantile(x, 0.25), "3rd Qu" = quantile(x, 0.75), "Mean" = mean(x), "Median" = median(x), "Sd" = sd(x), "Var" = var(x), "Min." = min(x), "Max." = max(x)))
})
statysty

##########################################################################################################

library(car)

wyniki_shapiro <- lapply(grupy, function(grupa) {
  lapply(grupa[, sapply(grupa, is.numeric)], shapiro.test)
})
cat("\n\n------------------------TEST SHAPIRO------------------------\n\n")

wyniki_shapiro
wyniki_levene <- list()
wynik_kw <- list ()

library(ggplot2)

pdf("ROZKLAD_NORMALNY.pdf")
for (i in 1:length(grupy)) {
  for (j in 2:ncol(grupy[[i]])) {
    if(is.numeric(grupy[[i]][[j]])){
      wykres <- ggplot(grupy[[i]], aes(x = grupy[[i]][[j]])) +
        geom_density(fill = "lightblue", alpha = 0.5) +
        stat_function(fun = dnorm, args = list(mean = mean(grupy[[i]][[j]], na.rm = TRUE), 
                                               sd = sd(grupy[[i]][[j]], na.rm = TRUE)),
                      color = "red", size = 1) +
        ggtitle(paste("Zgodność z rozkładem normalnym dla zmiennej", names(grupy[[i]])[j], "dla grupy", names(grupy)[i])) +
        xlab("Wartość") +
        ylab("Gęstość") +
        theme_bw()
      print(wykres)
    }
  }
}
dev.off()

cat("\n\n------------------------ANALIZA PORÓWNAWCZA POMIĘDZY GRUPAMI------------------------\n\n")

if (length(grupy) == 2) {
  #for (i in seq_along(grupy)) {
    for (kolumna in names(grupy[[i]][, sapply(grupy[[i]], is.numeric)])) {
      #for (kolumna in names(grupy[, sapply(grupy, is.numeric)])) {
      czy_normalne <- TRUE
      dane_kolumny <- numeric()
      for (i in seq_along(grupy)) {
        p_value <- wyniki_shapiro[[i]][[kolumna]]$p.value
        if (p_value < 0.05) {
          czy_normalne <- FALSE
          dane_kolumny <- c(dane_kolumny, grupy[[i]][, kolumna])
          break
        } else {
          dane_kolumny <- c(dane_kolumny, grupy[[i]][, kolumna])
        }
      }
     # if (length(dane_kolumny) > 1) {
        if (czy_normalne) {
          cat(kolumna, "ma rozkład normalny\n")
          wynik_levene <- leveneTest(dane_kolumny, group = rep(names(grupy), sapply(grupy, nrow)))
          wyniki_levene[[kolumna]] <- wynik_levene
          # Zapisanie wartości liczbowych z kolumny Pr(>F)
          if ("Pr(>F)" %in% names(wynik_levene)) {
            wyniki_levene_prf <- as.numeric(wynik_levene[["Pr(>F)"]])
            wyniki_levene_prf_na <- na.omit(wyniki_levene_prf)
            # Wyświetlenie wartości liczbowych
            cat("p-wartość testu Levene'a:")
            print(wyniki_levene_prf_na[1])
          }
          if (wyniki_levene_prf_na[1] < 0.05) {
            cat("Nie ma jednorodności wariancji, przeprowadzam test Welch t-test\n")
            wynik_t <- t.test(dane_kolumny ~ rep(names(grupy), sapply(grupy, nrow)), var.equal = FALSE)
            cat("Wartość statystyki t:", wynik_t$statistic, "\n")
            cat("p-wartość testu t:", wynik_t$p.value, "\n")
          } else {
            cat("Jest jednorodność wariancji, przeprowadzam test t-Studenta\n")
            wynik_t <- t.test(dane_kolumny ~ rep(names(grupy), sapply(grupy, nrow)), var.equal = TRUE)
            cat("Wartość statystyki t:", wynik_t$statistic, "\n")
            cat("p-wartość testu t:", wynik_t$p.value, "\n")
          }
        } 
      if (!czy_normalne) {
        cat(kolumna, "nie ma rozkładu normalnego, przeprowadzam test Wilcoxona\n")
        dane_kolumny <- dane_kolumny[complete.cases(dane_kolumny)]
        if (length(dane_kolumny) > 1) {
          wynik_wilcox <- wilcox.test(dane_kolumny, group = rep(names(grupy), sapply(grupy, nrow)))
          cat("Wartość statystyki U:", wynik_wilcox$statistic, "\n")
          cat("p-wartość testu U:", wynik_wilcox$p.value, "\n")
        } else {
          cat("Nie można przeprowadzić testu Wilcoxona, ponieważ brakuje wystarczającej liczby niebrakujących obserwacji\n")
        }
      }
      #} 
    } 
  } 

if (length(grupy) > 2) {
  for (kolumna in names(grupy[[1]][, sapply(grupy[[1]], is.numeric)])) {
    czy_normalne <- TRUE
    dane_kolumny <- numeric()
    for (i in seq_along(grupy)) {
      p_value <- wyniki_shapiro[[i]][[kolumna]]$p.value
      if (p_value < 0.05) {
        czy_normalne <- FALSE
        dane_kolumny <- c(dane_kolumny, grupy[[i]][, kolumna])
      } else {
        dane_kolumny <- c(dane_kolumny, grupy[[i]][, kolumna])
      }
    }
    if (!czy_normalne) {
      cat(kolumna, "nie ma rozkładu normalnego, przeprowadzam test Kruskala-Wallisa\n")
      pvalueKW <- kruskal.test(dane_kolumny ~ rep(names(grupy), sapply(grupy, nrow)))$p.value
      if (pvalueKW < 0.05) {
        cat(pvalueKW, "< 0.05 - są różnice pomiędzy grupami\n")
        wyniki_Dunn <- dunnTest(dane_kolumny, rep(names(grupy), sapply(grupy, nrow)))
        print(wyniki_Dunn)
      } else {
        cat(pvalueKW, "> 0.05 - brak różnic pomiędzy grupami\n")
      }
    } else {
      cat(kolumna, "ma rozkład normalny, przeprowadzam test ANOVA\n")
      wynik_levene <- leveneTest(dane_kolumny, group = rep(names(grupy), sapply(grupy, nrow)))
      wyniki_levene[[kolumna]] <- wynik_levene
      if ("Pr(>F)" %in% names(wynik_levene)) {
        wyniki_levene_prf <- as.numeric(wynik_levene[["Pr(>F)"]])
        wyniki_levene_prf_na <- na.omit(wyniki_levene_prf)
        cat("p-wartość testu Levene'a:")
        print(wyniki_levene_prf_na[1])
      }
      if (wyniki_levene_prf_na[1] >= 0.05) {
        cat("Przeprowadzam test ANOVA\n")
        wynik_anova <- aov(dane_kolumny ~ rep(names(grupy), sapply(grupy, nrow)))
        wyniczki <- summary(wynik_anova)
        print(wyniczki)
        pvalueAOV <- wyniczki[[1]][["Pr(>F)"]][[1]]
        if (pvalueAOV < 0.05) {
          cat(pvalueAOV, "< 0.05 - są różnice pomiędzy grupami, przeprowadzam test TukeyHSD\n\n")
          wyniki_Tukey <- TukeyHSD(wynik_anova)
          print(wyniki_Tukey)
        } else {
          cat(pvalueAOV, "> 0.05 - brak różnic pomiędzy grupami\n")
        }
      }
    }
  }
}

#cat("------TEST KORELACJI SPEARMAN-----\n")
library(ggpubr)
pdf("KORELACJE.pdf")
for (i in 1:length(grupy)) {
  dane_grupa <- grupy[[i]]
  cat("\n################################################################################")
  cat("\n------------------------------------Grupa", names(grupy)[i], "------------------------------------------------------\n")
  cat("\n################################################################################\n\n")
  num_kolumny <- which(sapply(dane_grupa, is.numeric))
  for (j in num_kolumny) {
    for (k in 1:ncol(dane_grupa)) {
      if (is.numeric(dane_grupa[[k]]) && k != j) {
        wynik_spearman <- cor.test(dane_grupa[[j]], dane_grupa[[k]], method = "spearman")
        corr_coef <- round(wynik_spearman$estimate, 2)
        if (corr_coef > -1 && corr_coef <= -0.7) {
          cat("bardzo silna korelacja ujemna\n")
          cat("Korelacja między zmienną", names(dane_grupa)[j], "a zmienną", names(dane_grupa)[k], ", współczynnik korelacji:", round(wynik_spearman$estimate, 2), "p-wartość:", round(wynik_spearman$p.value, 4), "\n\n")
          plot <- ggscatter(data = dane_grupa, x = names(dane_grupa)[j], y = names(dane_grupa)[k], 
                            xlab = names(dane_grupa)[j], ylab = names(dane_grupa)[k], 
                            title = paste("Correlation plot for", names(dane_grupa)[j], "and", names(dane_grupa)[k], "GROUP:", names(grupy)[i]), 
                            add = "reg.line", conf.int = TRUE)
          
          plot <- plot + geom_text(x = max(dane_grupa[[j]]) - 0.2*max(dane_grupa[[j]]), 
                                   y = max(dane_grupa[[k]]) - 0.2*max(dane_grupa[[k]]), 
                                   label = paste("r = ", round(wynik_spearman$estimate, 2), "\n", "p = ", 
                                                 round(wynik_spearman$p.value, 4)), 
                                   size = 6, color = "black")
          
          print(plot)
        } else if (corr_coef > -0.7 && corr_coef <= -0.5) {
          cat("silna korelacja ujemna\n")
          cat("Korelacja między zmienną", names(dane_grupa)[j], "a zmienną", names(dane_grupa)[k], ", współczynnik korelacji:", round(wynik_spearman$estimate, 2), "p-wartość:", round(wynik_spearman$p.value, 4), "\n\n")
          plot <- ggscatter(data = dane_grupa, x = names(dane_grupa)[j], y = names(dane_grupa)[k], 
                            xlab = names(dane_grupa)[j], ylab = names(dane_grupa)[k], 
                            title = paste("Correlation plot for", names(dane_grupa)[j], "and", names(dane_grupa)[k], "GROUP:", names(grupy)[i]), 
                            add = "reg.line", conf.int = TRUE)
          
          plot <- plot + geom_text(x = max(dane_grupa[[j]]) - 0.2*max(dane_grupa[[j]]), 
                                   y = max(dane_grupa[[k]]) - 0.2*max(dane_grupa[[k]]), 
                                   label = paste("r = ", round(wynik_spearman$estimate, 2), "\n", "p = ", 
                                                 round(wynik_spearman$p.value, 4)), 
                                   size = 6, color = "black")
          
          print(plot)
        } else if (corr_coef > -0.5 && corr_coef <= -0.3) {
          cat("korelacja ujemna o średnim natężeniu\n")
          cat("Korelacja między zmienną", names(dane_grupa)[j], "a zmienną", names(dane_grupa)[k], ", współczynnik korelacji:", round(wynik_spearman$estimate, 2), "p-wartość:", round(wynik_spearman$p.value, 4), "\n\n")
          plot <- ggscatter(data = dane_grupa, x = names(dane_grupa)[j], y = names(dane_grupa)[k], 
                            xlab = names(dane_grupa)[j], ylab = names(dane_grupa)[k], 
                            title = paste("Correlation plot for", names(dane_grupa)[j], "and", names(dane_grupa)[k], "GROUP:", names(grupy)[i]), 
                            add = "reg.line", conf.int = TRUE)
          
          plot <- plot + geom_text(x = max(dane_grupa[[j]]) - 0.2*max(dane_grupa[[j]]), 
                                   y = max(dane_grupa[[k]]) - 0.2*max(dane_grupa[[k]]), 
                                   label = paste("r = ", round(wynik_spearman$estimate, 2), "\n", "p = ", 
                                                 round(wynik_spearman$p.value, 4)), 
                                   size = 6, color = "black")
          
          print(plot)
        } else if (corr_coef > -0.3 && corr_coef <= -0.2) {
          cat("słaba korelacja ujemna\n")
          cat("Korelacja między zmienną", names(dane_grupa)[j], "a zmienną", names(dane_grupa)[k], ", współczynnik korelacji:", round(wynik_spearman$estimate, 2), "p-wartość:", round(wynik_spearman$p.value, 4), "\n\n")
          plot <- ggscatter(data = dane_grupa, x = names(dane_grupa)[j], y = names(dane_grupa)[k], 
                            xlab = names(dane_grupa)[j], ylab = names(dane_grupa)[k], 
                            title = paste("Correlation plot for", names(dane_grupa)[j], "and", names(dane_grupa)[k], "GROUP:", names(grupy)[i]), 
                            add = "reg.line", conf.int = TRUE)
          
          plot <- plot + geom_text(x = max(dane_grupa[[j]]) - 0.2*max(dane_grupa[[j]]), 
                                   y = max(dane_grupa[[k]]) - 0.2*max(dane_grupa[[k]]), 
                                   label = paste("r = ", round(wynik_spearman$estimate, 2), "\n", "p = ", 
                                                 round(wynik_spearman$p.value, 4)), 
                                   size = 6, color = "black")
          
          print(plot)
        } else if (corr_coef > -0.2 && corr_coef < 0.2) {
          cat("BRAK KORELACJI MIĘDZY ZMIENNĄ", names(dane_grupa)[j], "A ZMIENNĄ", names(dane_grupa)[k], ", współczynnik korelacji:", round(wynik_spearman$estimate, 2), "p-wartość:", round(wynik_spearman$p.value, 4), "\n\n")
          
        } else if (corr_coef > 0.2 && corr_coef < 0.3) {
          cat("słaba korelacja dodatnia\n")
          cat("Korelacja między zmienną", names(dane_grupa)[j], "a zmienną", names(dane_grupa)[k], ", współczynnik korelacji:", round(wynik_spearman$estimate, 2), "p-wartość:", round(wynik_spearman$p.value, 4), "\n\n")
          plot <- ggscatter(data = dane_grupa, x = names(dane_grupa)[j], y = names(dane_grupa)[k], 
                            xlab = names(dane_grupa)[j], ylab = names(dane_grupa)[k], 
                            title = paste("Correlation plot for", names(dane_grupa)[j], "and", names(dane_grupa)[k], "GROUP:", names(grupy)[i]), 
                            add = "reg.line", conf.int = TRUE)
          
          plot <- plot + geom_text(x = max(dane_grupa[[j]]) - 0.2*max(dane_grupa[[j]]), 
                                   y = max(dane_grupa[[k]]) - 0.2*max(dane_grupa[[k]]), 
                                   label = paste("r = ", round(wynik_spearman$estimate, 2), "\n", "p = ", 
                                                 round(wynik_spearman$p.value, 4)), 
                                   size = 6, color = "black")
          
         
          print(plot)
          
        } else if (corr_coef > 0.3 && corr_coef < 0.5) {
          cat("korelacja dodatnia o średnim natężeniu\n")
          cat("Korelacja między zmienną", names(dane_grupa)[j], "a zmienną", names(dane_grupa)[k], ", współczynnik korelacji:", round(wynik_spearman$estimate, 2), "p-wartość:", round(wynik_spearman$p.value, 4), "\n\n")
          plot <- ggscatter(data = dane_grupa, x = names(dane_grupa)[j], y = names(dane_grupa)[k], 
                            xlab = names(dane_grupa)[j], ylab = names(dane_grupa)[k], 
                            title = paste("Correlation plot for", names(dane_grupa)[j], "and", names(dane_grupa)[k],"GROUP:", names(grupy)[i]), 
                            add = "reg.line", conf.int = TRUE)
          
          plot <- plot + geom_text(x = max(dane_grupa[[j]]) - 0.2*max(dane_grupa[[j]]), 
                                   y = max(dane_grupa[[k]]) - 0.2*max(dane_grupa[[k]]), 
                                   label = paste("r = ", round(wynik_spearman$estimate, 2), "\n", "p = ", 
                                                 round(wynik_spearman$p.value, 4)), 
                                   size = 6, color = "black")
          
          
          print(plot)
        } else if (corr_coef > 0.5 && corr_coef < 0.7) {
          cat("silna korelacja dodatnia\n")
          cat("Korelacja między zmienną", names(dane_grupa)[j], "a zmienną", names(dane_grupa)[k], ", współczynnik korelacji:", round(wynik_spearman$estimate, 2), "p-wartość:", round(wynik_spearman$p.value, 4), "\n\n")
          plot <- ggscatter(data = dane_grupa, x = names(dane_grupa)[j], y = names(dane_grupa)[k], 
                            xlab = names(dane_grupa)[j], ylab = names(dane_grupa)[k], 
                            title = paste("Correlation plot for", names(dane_grupa)[j], "and", names(dane_grupa)[k], "GROUP:", names(grupy)[i]), 
                            add = "reg.line", conf.int = TRUE)
          
          plot <- plot + geom_text(x = max(dane_grupa[[j]]) - 0.2*max(dane_grupa[[j]]), 
                                   y = max(dane_grupa[[k]]) - 0.2*max(dane_grupa[[k]]), 
                                   label = paste("r = ", round(wynik_spearman$estimate, 2), "\n", "p = ", 
                                                 round(wynik_spearman$p.value, 4)), 
                                   size = 6, color = "black")
          
          print(plot)
        } else if (corr_coef >= 0.7 && corr_coef < 1) {
          cat("bardzo silna korelacja dodatnia\n")
          cat("Korelacja między zmienną", names(dane_grupa)[j], "a zmienną", names(dane_grupa)[k], ", współczynnik korelacji:", round(wynik_spearman$estimate, 2), "p-wartość:", round(wynik_spearman$p.value, 4), "\n\n")
          plot <- ggscatter(data = dane_grupa, x = names(dane_grupa)[j], y = names(dane_grupa)[k], 
                            xlab = names(dane_grupa)[j], ylab = names(dane_grupa)[k], 
                            title = paste("Correlation plot for", names(dane_grupa)[j], "and", names(dane_grupa)[k], "GROUP:", names(grupy)[i]), 
                            add = "reg.line", conf.int = TRUE)
          
          plot <- plot + geom_text(x = max(dane_grupa[[j]]) - 0.2*max(dane_grupa[[j]]), 
                                   y = max(dane_grupa[[k]]) - 0.2*max(dane_grupa[[k]]), 
                                   label = paste("r = ", round(wynik_spearman$estimate, 2), "\n", "p = ", 
                                                 round(wynik_spearman$p.value, 4)), 
                                   size = 6, color = "black")
          
          print(plot)
        }
        cat("----------------------------------------------------------------------------------------------------------------------\n\n")
      }
    }
  }
}
dev.off()

sink()

