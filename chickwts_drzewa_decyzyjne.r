# Załaduj dane
data(chickwts)

# Cel klasyfikacji - "feed"
cel_klasyfikacji <- "feed"

# Ustawienie ziarna dla powtarzalności wyników
set.seed(434)

# Podział danych na dane trenujące i testowe (2/3 - 1/3)
ile <- nrow(chickwts)
idxTren <- sample(1:ile, 2 * ile / 3)
idxTest <- setdiff(1:ile, idxTren)

# Tworzenie drzewa decyzyjnego
library(rpart)
library(rpart.plot)

# Tworzymy formułę dla drzewa
formula <- as.formula(paste(cel_klasyfikacji, "~ ."))

# Tworzymy drzewo decyzyjne
drzewo <- rpart(formula, data = chickwts[idxTren, ], method = "class", control = rpart.control(cp = 0))
prp(drzewo, type = 3, extra = 1)

# Określenie błędu klasyfikacji na danych testowych
y_pred_test <- predict(drzewo, newdata = chickwts[idxTest,], type = "class")
blad_test <- 1 - sum(y_pred_test == factor(chickwts[idxTest, cel_klasyfikacji])) / length(chickwts[idxTest, cel_klasyfikacji])
print(blad_test)
