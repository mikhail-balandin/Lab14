# renv::init() # инициализация виртуального окружения
# renv::install("devtools") # установка библиотеки из CRAN
# renv::snapshot() # делаем снимок версий библиотек в нашем виртуальном окружении
# фиксируем этот список в .lock-файле для возможности восстановления
# renv::restore() # команда отктиться к предыдушему удачному обновления библиотек

# ------------------- 
# Лабораторная работа №14:
# Деревья решений.

library(tree)
library(ggplot2)
setwd("C:/Users/misha/Desktop/информационные технологии/lab14")

iris <- read.csv("iris.csv")
str(iris)
head(iris)
tail(iris)
summary(iris)

na_check <- function(x){
  
  if (missing(x))
    print("Необходимо ввести название таблицы для проверки..")
  
  else
    if (sum(is.na(x)) > 0) 
      print(paste(c("Обнаружено"), round(sum(is.na(x)), digits = 2), c("пропущенных значений")
      )
      )
  else
    print("Пропущенных данных не обнаружено")
  
}

na_check(iris)

y1 <- rep(0, nrow(iris))
y1[iris[ , 5]=="Setosa"] <-1

y2 <- rep(0, nrow(iris))
y2[iris[ , 5]=="Versicolor"] <-1

y3 <- rep(0, nrow(iris))
y3[iris[ , 5]=="Virginica"] <-1

z<- as.data.frame(cbind(iris, y1, y2, y3))
z <-z[,-5]

set.seed(1)
tree.z = tree(z$y3 ~ .  - z$y3, z)
summary(tree.z)

plot(tree.z)
text(tree.z, pretty = 0)

tree.z

set.seed(2) 
train = sample(1:nrow(z), 100)
iris.test = z[ -train, ]


length(z[train, ]) # на всякий случай, проверяем, что длины совпадают
length(z[-train, ])

Class.test = z$y3[-train] # тестовая часть по колонке Y
tree.z = tree(z$y3 ~ . - z$y3,  z, subset = train) # обучаем модель
tree.pred = predict(tree.z, iris.test) # делаем прогноз по тестовым данным
table(tree.pred, Class.test) # строим confusion matrix

length(Class.test)
length(tree.z)
length(tree.pred)

set.seed(3) # пробуем повысить точность при помощи подрезки ветвей
cv.iris = cv.tree(tree.z, FUN = prune.tree) # к-кратная кросс-валидация для подсчета разброса ошибочно-классифицированных наблюдений как ф-ия от величина к (число блоков кросс-валидации)
names(cv.iris)
cv.iris

par(mfrow = c(1, 2)) # строим график
plot(cv.iris$size, cv.iris$dev, type = "b")
plot(cv.iris$k, cv.iris$dev, type = "b")

prune.iris = prune.tree(tree.z, best = 2) # подрезаем ветви
plot(prune.iris) 
text(prune.iris, pretty = 0)

tree.pred = predict(prune.iris, iris.test)
report_tree_table <- table(tree.pred, Class.test)

report_tree_table

print("Ошибка классификации и точность прогноза")

1 - sum(diag(report_tree_table))/sum(report_tree_table)
sum(diag(report_tree_table))/sum(report_tree_table)
