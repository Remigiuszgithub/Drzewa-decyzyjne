# Instaluj i załaduj pakiet rpart
# install.packages("rpart")
library(rpart)

# Załaduj dane
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
heart_data <- read.csv(url, header = FALSE)

# Przypisz nazwy kolumn
colnames(heart_data) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")

# Zamień brakujące wartości na NA
heart_data[heart_data == "?"] <- NA

# Przekształć kolumny na numeryczne
heart_data[] <- lapply(heart_data, as.numeric)

# Usuń brakujące wartości
heart_data <- na.omit(heart_data)

# Podział danych na dane trenujące i testowe (2/3 - 1/3)
set.seed(123)
idxTren <- sample(1:nrow(heart_data), 2 * nrow(heart_data) / 3)
idxTest <- setdiff(1:nrow(heart_data), idxTren)

# Tworzymy formułę dla drzewa
formula_heart <- as.formula(paste("target ~ ."))

# Tworzymy drzewo decyzyjne
drzewo_heart <- rpart(formula_heart, data = heart_data[idxTren, ], method = "class", control = rpart.control(cp = 0.01))

# Wyświetlamy drzewo decyzyjne
library(rpart.plot)
prp(drzewo_heart, type = 3, extra = 1)
