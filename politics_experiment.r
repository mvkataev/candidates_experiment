library(readxl)
library(dplyr)
library(ggplot2)
library(data.table)
library(DescTools)
library(vcd)
library(ggcorrplot)
library(margins)
library(mfx)
library(descr)
library(caret)
library(stargazer)
library(flexmix)
library(ggpubr)
library(DescTools)
library(ggpattern)
library(corrplot)
library(MASS)
library(parameters)
library(BSDA)


df <- read_excel("/Users/maksimkataev/Downloads/politics_experiment.xlsx",sheet = 1)

df_names <- names(df)

df_nominal <- df
df_graoh <- df_nominal
df_graoh_names <- names(df_graoh)

df_graoh_names

str(df_graoh)
df_graoh <- mutate(df_graoh, gender = as.factor(gender), 
                   marital_status = as.factor(marital_status), 
                   political_orientation = as.factor(political_orientation),
                   candidate_gender = as.factor(candidate_gender), 
                   candidate_text = as.factor(candidate_text), 
                   candidate_score = as.factor(candidate_score))


df_nominal <- subset(df_nominal, select = -c(age))
df_nominal_names <- names(df_nominal)

actual <- c('Возраст респондента', 'Пол респондента', 'Семейное положение', 
            'Политическая принадлежность', 
            'Пол кандидата', 'Наличие в опросе абзаца про гендерную повестку', 
            'Рейтинг кандидата')

actual_nominal <- c('Пол респондента', 'Семейное положение', 
                    'Политическая принадлежность', 
                    'Пол кандидата', 'Наличие в опросе абзаца про гендерную повестку', 
                    'Рейтинг кандидата')

glimpse(df)
str(df_nominal)

df_true_nominal <- df_graoh
df_true_nominal_names <- names(df_true_nominal)
df_true_nominal_names


df_true_nominal <- df_true_nominal %>%
  mutate("political_orientation" = ifelse(political_orientation == 1, "Правые", "Левые"))
  
df_true_nominal <- df_true_nominal %>%
  mutate("marital_status" = ifelse(marital_status == 1, "В браке", "Не в браке"))

df_true_nominal <- df_true_nominal %>%
  mutate("gender" = ifelse(gender == 1, "Мужской", "Женский"))

df_true_nominal <- df_true_nominal %>%
  mutate("candidate_gender" = ifelse(candidate_gender == 1, "Мужской", "Женский"))

df_true_nominal <- df_true_nominal %>%
  mutate("candidate_text" = ifelse(candidate_text == 1, "Есть", "Нет"))


# Графики violin
qplot(data = df_true_nominal, x = gender, y = age, geom = "violin") +
  labs(x = "Пол респондента", 
       y = "Возраст респондента") + 
  theme_minimal()

qplot(data = df_true_nominal, x = political_orientation, y = age, geom = "violin") +
  labs(x = "Политическая принадлежность", 
       y = "Возраст респондента") + 
  theme_minimal()


for (i in 1:length(df_true_nominal_names)){
  names(df_true_nominal)[names(df_true_nominal)==df_true_nominal_names[i]] <- actual[i]
}



# Корреляционный анализ

cor_matrix <- cor(df_nominal, method = "spearman")
testRes = cor.mtest(df_nominal, conf.level = 0.95)

corrplot(cor_matrix, p.mat = testRes$p, 
         method = 'circle', type = 'lower', insig='pch',
         pch.col = 'grey',
         pch.cex = 5,
         addCoef.col ='black', number.cex = 0.8, 
         tl.col = "black",
         tl.srt = 30, 
         title = "Корреляция Спирмена для порядковых переменных (p-value < 0.05)", 
         order = 'AOE', diag=FALSE, mar=c(0,0,2,0))



# Построение графиков и проведение тестов на пропорцию

df_true_nominal$`Рейтинг кандидата` = as.numeric(df_true_nominal$`Рейтинг кандидата`)
decimal2 <- function(x) trimws(format(round(x, 2), nsmall=2))


df_candidate_male <- df_true_nominal %>% 
  filter(`Пол кандидата` == "Мужской")
df_candidate_female <- df_true_nominal %>% 
  filter(`Пол кандидата` == "Женский")

# В целом дискриминации не наблюдалось
z.test(x=df_candidate_male$`Рейтинг кандидата`, 
       y=df_candidate_female$`Рейтинг кандидата`, 
       sigma.x = sd(df_candidate_male$`Рейтинг кандидата`), 
       sigma.y = sd(df_candidate_female$`Рейтинг кандидата`))



# Политическая принадлежность
df_by_politics0 <- df_true_nominal %>% 
  filter(`Политическая принадлежность` == "Левые")
df_by_politics1 <- df_true_nominal %>% 
  filter(`Политическая принадлежность` == "Правые")

z.test(x=df_by_politics0$`Рейтинг кандидата`, 
       y=df_by_politics1$`Рейтинг кандидата`, 
       sigma.x = sd(df_by_politics0$`Рейтинг кандидата`), 
       sigma.y = sd(df_by_politics1$`Рейтинг кандидата`))

df_by_politics0_m <- df_by_politics0 %>% 
  filter(`Пол кандидата` == "Мужской")
df_by_politics0_f <- df_by_politics0 %>% 
  filter(`Пол кандидата` == "Женский")
df_by_politics1_m <- df_by_politics1 %>% 
  filter(`Пол кандидата` == "Мужской")
df_by_politics1_f <- df_by_politics1 %>% 
  filter(`Пол кандидата` == "Женский")

# Дискриминации не наблюдается
z.test(x=df_by_politics0_m$`Рейтинг кандидата`, 
       y=df_by_politics0_f$`Рейтинг кандидата`, 
       sigma.x = sd(df_by_politics0_m$`Рейтинг кандидата`), 
       sigma.y = sd(df_by_politics0_f$`Рейтинг кандидата`))
z.test(x=df_by_politics1_m$`Рейтинг кандидата`, 
       y=df_by_politics1_f$`Рейтинг кандидата`, 
       sigma.x = sd(df_by_politics1_m$`Рейтинг кандидата`), 
       sigma.y = sd(df_by_politics1_f$`Рейтинг кандидата`))


bp_m = c(decimal2(mean(df_by_politics0_m$`Рейтинг кандидата`)), 
         decimal2(mean(df_by_politics1_m$`Рейтинг кандидата`)))
bp_f = c(decimal2(mean(df_by_politics0_f$`Рейтинг кандидата`)), 
         decimal2(mean(df_by_politics1_f$`Рейтинг кандидата`)))

type_bp = c("Мужчина", "Мужчина",
            "Женщина", "Женщина")
cnt_bp = c(bp_m, 
           bp_f)
inf_bp = c("Левые", "Правые",
           "Левые", "Правые")
inf_bp <- factor(inf_bp, ordered = TRUE, 
                 levels = c("Левые", "Правые"))
df_bp = data.frame(type_bp, cnt_bp, inf_bp)

ggplot(df_bp, aes(x = inf_bp, y = cnt_bp, fill = type_bp)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("#D33844", "#172C65")) +
  labs(title = "Средний рейтинг кандидатов 
и политическая принадлежность респондентов", x = "Политическая принадлежность", 
       y = "Средний рейтинг", fill = "Кандидат") +
  geom_text(aes(label = cnt_bp),
            position = position_dodge(0.9), vjust =-1) +
  theme_minimal()



# Наличие в опросе абзаца про гендерную повестку
df_by_gp0 <- df_true_nominal %>% 
  filter(`Наличие в опросе абзаца про гендерную повестку` == "Нет")
df_by_gp1 <- df_true_nominal %>% 
  filter(`Наличие в опросе абзаца про гендерную повестку` == "Есть")

z.test(x=df_by_gp0$`Рейтинг кандидата`, 
       y=df_by_gp1$`Рейтинг кандидата`, 
       sigma.x = sd(df_by_gp0$`Рейтинг кандидата`), 
       sigma.y = sd(df_by_gp1$`Рейтинг кандидата`))

df_by_gp0_m <- df_by_gp0 %>% 
  filter(`Пол кандидата` == "Мужской")
df_by_gp0_f <- df_by_gp0 %>% 
  filter(`Пол кандидата` == "Женский")
df_by_gp1_m <- df_by_gp1 %>% 
  filter(`Пол кандидата` == "Мужской")
df_by_gp1_f <- df_by_gp1 %>% 
  filter(`Пол кандидата` == "Женский")

# Дискриминации не наблюдается
z.test(x=df_by_gp0_m$`Рейтинг кандидата`, 
       y=df_by_gp0_f$`Рейтинг кандидата`, 
       sigma.x = sd(df_by_gp0_m$`Рейтинг кандидата`), 
       sigma.y = sd(df_by_gp0_f$`Рейтинг кандидата`))
z.test(x=df_by_gp1_m$`Рейтинг кандидата`, 
       y=df_by_gp1_f$`Рейтинг кандидата`, 
       sigma.x = sd(df_by_gp1_m$`Рейтинг кандидата`), 
       sigma.y = sd(df_by_gp1_f$`Рейтинг кандидата`))


bgp_m = c(decimal2(mean(df_by_gp0_m$`Рейтинг кандидата`)), 
          decimal2(mean(df_by_gp1_m$`Рейтинг кандидата`)))
bgp_f = c(decimal2(mean(df_by_gp0_f$`Рейтинг кандидата`)), 
          decimal2(mean(df_by_gp1_f$`Рейтинг кандидата`)))

type_gp = c("Мужчина", "Мужчина",
            "Женщина", "Женщина")
cnt_gp = c(bgp_m, 
           bgp_f)
inf_gp = c("Нет", "Есть",
           "Нет", "Есть")
inf_gp <- factor(inf_gp, ordered = TRUE, 
                 levels = c("Нет", "Есть"))
df_gp = data.frame(type_gp, cnt_gp, inf_gp)

ggplot(df_gp, aes(x = inf_gp, y = cnt_gp, fill = type_gp)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("#D33844", "#172C65")) +
  labs(title = "Средний рейтинг кандидатов 
и наличие текста про гендерную повестку в опросе", x = "Текст про гендерную повестку в опросе", 
       y = "Средний рейтинг", fill = "Кандидат") +
  geom_text(aes(label = cnt_gp),
            position = position_dodge(0.9), vjust =-1) +
  theme_minimal()



# Пол респондента
df_by_gp0 <- df_true_nominal %>% 
  filter(`Пол респондента` == "Женский")
df_by_gp1 <- df_true_nominal %>% 
  filter(`Пол респондента` == "Мужской")

# Значимое различие (p-value = 0.000358)
z.test(x=df_by_gp0$`Рейтинг кандидата`, 
       y=df_by_gp1$`Рейтинг кандидата`, 
       sigma.x = sd(df_by_gp0$`Рейтинг кандидата`), 
       sigma.y = sd(df_by_gp1$`Рейтинг кандидата`))

df_by_gp0_m <- df_by_gp0 %>% 
  filter(`Пол кандидата` == "Мужской")
df_by_gp0_f <- df_by_gp0 %>% 
  filter(`Пол кандидата` == "Женский")
df_by_gp1_m <- df_by_gp1 %>% 
  filter(`Пол кандидата` == "Мужской")
df_by_gp1_f <- df_by_gp1 %>% 
  filter(`Пол кандидата` == "Женский")

# Дискриминации не наблюдается
z.test(x=df_by_gp0_m$`Рейтинг кандидата`, 
       y=df_by_gp0_f$`Рейтинг кандидата`, 
       sigma.x = sd(df_by_gp0_m$`Рейтинг кандидата`), 
       sigma.y = sd(df_by_gp0_f$`Рейтинг кандидата`))
z.test(x=df_by_gp1_m$`Рейтинг кандидата`, 
       y=df_by_gp1_f$`Рейтинг кандидата`, 
       sigma.x = sd(df_by_gp1_m$`Рейтинг кандидата`), 
       sigma.y = sd(df_by_gp1_f$`Рейтинг кандидата`))


bgp_m = c(decimal2(mean(df_by_gp0_m$`Рейтинг кандидата`)), 
          decimal2(mean(df_by_gp1_m$`Рейтинг кандидата`)))
bgp_f = c(decimal2(mean(df_by_gp0_f$`Рейтинг кандидата`)), 
          decimal2(mean(df_by_gp1_f$`Рейтинг кандидата`)))

type_gp = c("Мужчина", "Мужчина",
            "Женщина", "Женщина")
cnt_gp = c(bgp_m, 
           bgp_f)
inf_gp = c("Женский", "Мужской",
           "Женский", "Мужской")
inf_gp <- factor(inf_gp, ordered = TRUE, 
                 levels = c("Женский", "Мужской"))
df_gp = data.frame(type_gp, cnt_gp, inf_gp)

ggplot(df_gp, aes(x = inf_gp, y = cnt_gp, fill = type_gp)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("#D33844", "#172C65")) +
  labs(title = "Средний рейтинг кандидатов 
и пол респондента", x = "Пол респондента", 
       y = "Средний рейтинг", fill = "Кандидат") +
  geom_text(aes(label = cnt_gp),
            position = position_dodge(0.9), vjust =-1) +
  theme_minimal()



# Семейное положение респондента
df_by_gp0 <- df_true_nominal %>% 
  filter(`Семейное положение` == "Не в браке")
df_by_gp1 <- df_true_nominal %>% 
  filter(`Семейное положение` == "В браке")

z.test(x=df_by_gp0$`Рейтинг кандидата`, 
       y=df_by_gp1$`Рейтинг кандидата`, 
       sigma.x = sd(df_by_gp0$`Рейтинг кандидата`), 
       sigma.y = sd(df_by_gp1$`Рейтинг кандидата`))

df_by_gp0_m <- df_by_gp0 %>% 
  filter(`Пол кандидата` == "Мужской")
df_by_gp0_f <- df_by_gp0 %>% 
  filter(`Пол кандидата` == "Женский")
df_by_gp1_m <- df_by_gp1 %>% 
  filter(`Пол кандидата` == "Мужской")
df_by_gp1_f <- df_by_gp1 %>% 
  filter(`Пол кандидата` == "Женский")

# Дискриминации не наблюдается
z.test(x=df_by_gp0_m$`Рейтинг кандидата`, 
       y=df_by_gp0_f$`Рейтинг кандидата`, 
       sigma.x = sd(df_by_gp0_m$`Рейтинг кандидата`), 
       sigma.y = sd(df_by_gp0_f$`Рейтинг кандидата`))
z.test(x=df_by_gp1_m$`Рейтинг кандидата`, 
       y=df_by_gp1_f$`Рейтинг кандидата`, 
       sigma.x = sd(df_by_gp1_m$`Рейтинг кандидата`), 
       sigma.y = sd(df_by_gp1_f$`Рейтинг кандидата`))


bgp_m = c(decimal2(mean(df_by_gp0_m$`Рейтинг кандидата`)), 
          decimal2(mean(df_by_gp1_m$`Рейтинг кандидата`)))
bgp_f = c(decimal2(mean(df_by_gp0_f$`Рейтинг кандидата`)), 
          decimal2(mean(df_by_gp1_f$`Рейтинг кандидата`)))

type_gp = c("Мужчина", "Мужчина",
            "Женщина", "Женщина")
cnt_gp = c(bgp_m, 
           bgp_f)
inf_gp = c("Не в браке", "В браке",
           "Не в браке", "В браке")
inf_gp <- factor(inf_gp, ordered = TRUE, 
                 levels = c("Не в браке", "В браке"))
df_gp = data.frame(type_gp, cnt_gp, inf_gp)

ggplot(df_gp, aes(x = inf_gp, y = cnt_gp, fill = type_gp)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("#D33844", "#172C65")) +
  labs(title = "Средний рейтинг кандидатов 
и семейное положение респондента", x = "Семейное положение респондента", 
       y = "Средний рейтинг", fill = "Кандидат") +
  geom_text(aes(label = cnt_gp),
            position = position_dodge(0.9), vjust =-1) +
  theme_minimal()


# Регрессионный анализ

summary(df)

# Базовая модель
df1 <- df
df1$candidate_score <- as.factor(df1$candidate_score)
model <- polr(candidate_score ~ age + gender + marital_status + 
                political_orientation + candidate_gender + candidate_text,
              data = df1)
summary(model)
# AIC = 1063.281

model1 <- polr(candidate_score ~ age + gender + marital_status + 
                political_orientation + candidate_gender + candidate_text +
                political_orientation*candidate_gender,
              data = df1)
summary(model1)
# AIC = 1065.264

model2 <- polr(candidate_score ~ age + gender + marital_status + 
                 political_orientation + candidate_gender + candidate_text +
                 gender*candidate_gender,
               data = df1)
summary(model2)
# AIC = 1065.219 

model3 <- polr(candidate_score ~ age + gender + marital_status + 
                 political_orientation + candidate_gender + candidate_text +
                 political_orientation*candidate_gender + gender*candidate_gender,
               data = df1)
summary(model3)
# AIC = 1067.198


stargazer(model, model1, model2, model3, out = "politics_experiment_models.html")
