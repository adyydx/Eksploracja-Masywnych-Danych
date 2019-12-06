---
title: "Raport śledziowy"
author: "Adrian Kotarski, Tomasz Pecyna"
date: '`r format(Sys.Date(), "%d.%m.%Y")`'
output: 
  html_document:
    toc: true
  md_document:
    toc: true
    
---



```{r setup, include=FALSE}
options(warn=-1)
knitr::opts_chunk$set(echo = TRUE)
warning = FALSE
message = FALSE
warn.conflicts = FALSE
suppressPackageStartupMessages = TRUE
```

## 0. Wstęp i ogólna analiza
Projekt miał na celu określenie przyczyny zmniejszającego się rozmiaru śledzia ocenicznego wyławianego na terytorium Europy. Zebrano dane z ostatnich 60 lat na temat różnych statystyk zawierających podstawowe informacje związane z aktualnym stanem morza. Analiza opierała się o rozwiązanie problemu brakujących wartości (zastosowano użycie wartości NA), wizualizację odpowiednich atrybutów i sprawdzenie ich korelacji. Największą korelację wykazały parametry: nao i sst, tj. oscylacja północnoatlantycka i temperatura przy powierzchni wody. Najistotniejszym atrybutem w predykcji rozmiaru śledzia okazało się być łączne roczne natężenie połowów w regionie.

## 1. Wykorzystane biblioteki
```{r, results='hide'}
suppressPackageStartupMessages({
  library(xgboost)
  library(ggplot2)
  library(reshape2)
  library(tidyr)
  library(corrplot)
  library(shiny)
  library(plotly)
})

```

## 2. Powtarzalność wyników
```{r}
set.seed(476)
```

## 3+4. Czytanie danych z pliku i przetwarzanie brakujących danych (które zdecydowano się zastąpić wartościami NA)
```{r}
data <- read.table("sledzie.csv", header = TRUE, sep = ",", colClasses = c("integer","double", "double", "double", "double", "double", "double", "double", "double", "integer", "double", "double", "double", "double", "integer", "double"), na.strings="?")
```

## 5. Rozmiar zbioru i podstawowe statystyki

Liczba rekordów w zbiorze:
```{r}
nrow(data)
```

Liczba atrybutów w zbiorze:
```{r}
ncol(data)
```

Rozkład typów danych przedstawia się w następujący sposób:
```{r}
sapply(data, typeof)
```
gdzie nazwy kolumn znaczą kolejno:

* **length**: długość złowionego śledzia [cm]

* **cfin1**: dostępność planktonu [zagęszczenie Calanus finmarchicus gat. 1]

* **cfin2**: dostępność planktonu [zagęszczenie Calanus finmarchicus gat. 2]

* **chel1**: dostępność planktonu [zagęszczenie Calanus helgolandicus gat. 1]

* **chel2**: dostępność planktonu [zagęszczenie Calanus helgolandicus gat. 2]

* **lcop1**: dostępność planktonu [zagęszczenie widłonogów gat. 1]

* **lcop2**: dostępność planktonu [zagęszczenie widłonogów gat. 2]

* **fbar**: natężenie połowów w regionie [ułamek pozostawionego narybku]

* **recr**: roczny narybek [liczba śledzi]

* **cumf**: łączne roczne natężenie połowów w regionie [ułamek pozostawionego narybku]

* **totaln**: łączna liczba ryb złowionych w ramach połowu [liczba śledzi]

* **sst**: temperatura przy powierzchni wody [°C]

* **sal**: poziom zasolenia wody [Knudsen ppt]

* **xmonth**: miesiąc połowu [numer miesiąca]

* **nao**: oscylacja północnoatlantycka [mb]

Natomiast ich średnia, mediana, odchylenie standardowe, wartości maksymalne i minimalne wyglądają tak:
```{r}
sapply(subset(data, select=length:nao), mean, na.rm = TRUE)
sapply(subset(data, select=length:nao), median, na.rm = TRUE)
sapply(subset(data, select=length:nao), sd, na.rm = TRUE)
sapply(subset(data, select=length:nao), min, na.rm = TRUE)
sapply(subset(data, select=length:nao), max, na.rm = TRUE)
```


## 6. Szczegółowa analiza wartości atrybutów

Histogramy dla poszczególnych kolumn pokazane są poniżej:
```{r, results='hide', echo=FALSE}
subset(subset(data, select=length:chel2), select=length:chel2) %>% gather() %>% head()
p1 <- ggplot(gather(subset(data, select=length:chel2)), aes(value, na.rm = TRUE)) + 
    geom_histogram(bins = 10) + 
    facet_wrap(~key, scales = 'free', ncol=2)

subset(subset(data, select=lcop1:cumf), select=lcop1:cumf) %>% gather() %>% head()
p2 <- ggplot(gather(subset(data, select=lcop1:cumf)), aes(value, na.rm = TRUE)) + 
    geom_histogram(bins = 10) + 
    facet_wrap(~key, scales = 'free', ncol=2)

subset(subset(data, select=totaln:nao), select=totaln:nao) %>% gather() %>% head()
p3 <- ggplot(gather(subset(data, select=totaln:nao)), aes(value, na.rm = TRUE)) + 
    geom_histogram(bins = 10) + 
    facet_wrap(~key, scales = 'free', ncol=2)

p1
p2
p3

```

## 7. Korelacja między zmiennymi

Korelację między zmiennymi pokazuje macierz poniżej:
```{r, results='hide'}
mydata.cor = cor(na.omit(subset(data, select=length:nao)))
corrplot(mydata.cor)
```

## 8. Interaktywny wykres zależności długości śledzia od czasu

```{r}

xx <- aggregate(length ~ xmonth, data=data, FUN=mean)

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "Miesiąc",
  titlefont = f
)
y <- list(
  title = "Długość śledzia [cm]",
  titlefont = f
)

p <- xx %>%
  plot_ly(
    x = ~xmonth,
    y = ~length,
    frame = ~xmonth,
    type = 'scatter',
    mode = 'markers',
    showlegend = F
  ) %>%
  layout(xaxis = x, yaxis = y)
p

```

## 9. Regresor przewidujący rozmiar śledzia

Zbiór danych został podzielony w proporcji 60:20:20. Zbiór walidacyjny był używany do optymalizowania parametrów, a zbiór testowy użyty jest teraz do podania ostatecznego wyniku. Do regresji wykorzystano bibliotekę XGBoost.
```{r}
train_index <- sample(1:nrow(data), 0.6 * nrow(data))
rest_index <- setdiff(1:nrow(data), train_index)
rest <- data[rest_index,]
val_index <- sample(1:nrow(rest), 0.5 * nrow(rest))
test_index <- setdiff(1:nrow(rest), val_index)

X_train <- data[train_index, 3:16]
y_train <- data.frame(data[train_index, 2])

X_val <- data[val_index, 3:16]
y_val <- data.frame(data[val_index, 2])

X_test <- data[test_index, 3:16]
y_test <- data.frame(data[test_index, 2])

dtrain <- xgb.DMatrix(data = as.matrix(X_train),label = as.matrix(y_train))
dval <- xgb.DMatrix(data = as.matrix(X_val),label = as.matrix(y_val))
dtest <- xgb.DMatrix(data = as.matrix(X_test),label = as.matrix(y_test))

params <- list(booster = "gbtree",
               objective = "reg:linear",
               eta=0.2,
               gamma=0.01,
               max_depth=12,
               min_child_weight=0.8,
               subsample=1,
               colsample_bytree=1)

res <- xgb.train( params = params, data = dtrain, nrounds = 50, nfold = 5, showsd = T, stratified = T, maximize = F)
```
Wartości R^2 oraz RMSE przy takich ustawieniach wyglądają następująco
```{r}
xgbpred <- predict (res,dtest)
rsq <- function (x, y) cor(x, y) ^ 2
r_squared <- rsq(y_test, xgbpred)
print(r_squared[1])
RMSE <-  function(m, o){sqrt(mean((m - o)^2))}
test_numeric <- data[test_index, 2]
rmse_res <- RMSE(test_numeric, xgbpred)
print(rmse_res)
```
## 10. Analiza ważności atrybutów najlepszego znalezionego modelu regresji
```{r}

mat <- xgb.importance (feature_names = colnames(data),model = res)
xgb.plot.importance (importance_matrix = mat) 
```
Z analizy wynika, iż najistotniejszym atrybutem w predykcji rozmiaru śledzia jest łączne roczne natężenie połowów w regionie (ułamek pozostawionego narybku). Drugim co do istotności atrybutem jest dostępność planktonu -- zagęszczenie widłonogów gat. 2.
