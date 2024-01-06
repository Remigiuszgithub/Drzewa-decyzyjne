# Załaduj dane
data(mtcars)

# Podział danych na dane trenujące i testowe (2/3 - 1/3)
set.seed(434)
ile <- nrow(mtcars)
idxTren <- sample(1:ile, 2 * ile / 3)
idxTest <- setdiff(1:ile, idxTren)

# Tworzenie drzewa decyzyjnego
library(rpart)
library(rpart.plot)

drzewo <- rpart(factor(cyl) ~ mpg, data = mtcars[idxTren,], method = "class", control = rpart.control(cp = 0))
prp(drzewo, type = 3, extra = 1)

# Stosowanie drzewa do danych testowych
y_pred_test <- predict(drzewo, newdata = mtcars[idxTest,], type = "class")

# Określenie błędu klasyfikacji na danych testowych
blad_test <- 1 - sum(y_pred_test == factor(mtcars[idxTest, "cyl"])) / length(mtcars[idxTest, "cyl"])
print(blad_test)
