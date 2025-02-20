chess <- read.csv("/Users/matviionyshchenko/Library/CloudStorage/OneDrive-KyivSchoolofEconomics/Statistics/games.csv")
diamonds <- read.csv("/Users/matviionyshchenko/Library/CloudStorage/OneDrive-KyivSchoolofEconomics/Statistics/diamonds.csv")

summary(chess)
summary(diamonds)


# Chess
rated_distribution <- chess$rated
normalizing <- function(x) {
  if (x == "TRUE" | x == "True") {
    return(T)
  } else {
    return(F)
  }
}
chess$rated <- sapply(chess$rated, normalizing)
rated_distribution <- chess$rated
rated_distribution <- table(rated_distribution)
summary(rated_distribution)
barplot(rated_distribution, main = "Розподіл значень 'rated'", 
        col = c("lightblue", "lightgreen"), 
        ylab = "Частота", xlab = "'Rated'")

vic_status_distribution <- chess$victory_status
vic_status_distribution <- table(vic_status_distribution)
summary(vic_status_distribution)
barplot(vic_status_distribution, main = "Розподіл значень 'victory_status'", 
        col = c("lightblue", "lightgreen", "lightcoral", "lightyellow"), 
        ylab = "Частота", xlab = "'Victory status'")

# Кореляція між рейтинговою грою та статусом перемоги
table_data <- table(chess$victory_status, chess$rated)
chisq_test <- chisq.test(table_data)
chisq_test

# Виникла ідея перевірити, чи є залежність між рейтингом гравця та його статусом перемоги. Але я не знав як це зробити адже це не числові значення тому взяв запропоноване рішення ChatGPT
# В результаті виявилось, що нульова гіпотеза про незалежність відхиляється, тобто є залежність між рейтинговою грою(чи ні) та статусом перемоги.

winner <- chess$winner
winner <- table(winner)
summary(winner)
barplot(winner, main = "Розподіл значень 'winner'", 
        col = c("lightblue", "lightgreen", "lightcoral"), 
        ylab = "Частота", xlab = "Winner")
total <- sum(winner) 
percentages <- round(100 * winner / total, 1) 
text(x = seq_along(winner), 
     y = winner - 0.1 * max(winner)
     labels = paste0(percentages, "%"), 
     pos = 3, 
     col = "black")



winner <- chess$winner
victory_status <- chess$victory_status
combined_table <- table(winner, victory_status)
combined_table_filtered <- combined_table[!(rownames(combined_table) == "draw"), ]
combined_table_filtered <- combined_table_filtered[, !(colnames(combined_table_filtered) == "draw")]

combined_table_filtered


barplot(as.matrix(combined_table), 
        beside = FALSE, 
        col = c("black", "lightgreen", "white"),
        main = "Розподіл перемоги по статусу",
        xlab = "Переможець", 
        ylab = "Частота",
        args.legend = list(title = "Статус перемоги"))
legend("topleft", 
       legend = rownames(combined_table),
       fill = c("black", "lightgreen", "white"),
       title = "Статус перемоги", 
       xjust = 0, yjust = 1)

# Кореляція між переможцем та рейтингом гравця

white_rating <- chess$white_rating
black_rating <- chess$black_rating
winner <- chess$winner



winner_numeric <- ifelse(winner == "white", 1, 0)
cor_white <- cor(white_rating, winner_numeric)
cor_black <- cor(black_rating, winner_numeric)

cor_white
cor_black

# Рейтингʼ
total_rating <- c(chess$white_rating, chess$black_rating)
summary(total_rating)
hist(total_rating, main = "Розподіл сумарного рейтингу", 
     col = "lightblue", xlab = "Рейтинг", ylab = "Частота")
boxplot(total_rating, main = "Розподіл сумарного рейтингу", 
        col = "lightblue", xlab = "Рейтинг", ylab = "Частота")

# Гравці
players <- c(chess$white_id, chess$black_id)
table_players <- table(players)
players_df <- data.frame(
  Гравець = names(table_players),
  Кількість_ігор = as.integer(table_players)
)
print(players_df)
players_df <- players_df[order(-players_df$Кількість_ігор), ]
head(players_df)


# Додаємо це в таблицю для кожного гравця 
colnames(players_df) <- c("player_id", "games_played")

chess <- merge(chess, players_df, by.x = "white_id", by.y = "player_id", all.x = TRUE)
colnames(chess)[ncol(chess)] <- "white_games_played"
chess <- merge(chess, players_df, by.x = "black_id", by.y = "player_id", all.x = TRUE)
colnames(chess)[ncol(chess)] <- "black_games_played"

cor(chess$white_games_played, as.numeric(chess$winner == "white"))
cor(chess$black_games_played, as.numeric(chess$winner == "black"))

mean(chess$white_games_played[chess$winner == "white"])
mean(chess$black_games_played[chess$winner == "black"])


chess_white <- chess[chess$white_games_played < 50, ]
chess_black <- chess[chess$black_games_played < 50, ]

# Bar plot
ggplot(chess_white, aes(x = white_games_played, fill = winner)) +
  geom_bar(position = "dodge") +
  labs(title = "Розподіл кількості ігор для білих",
       x = "Кількість ігор",
       y = "Частота") +
  theme_minimal()

ggplot(chess_black, aes(x = black_games_played, fill = winner)) +
  geom_bar(position = "dodge") +
  labs(title = "Розподіл кількості ігор для чорних",
       x = "Кількість ігор",
       y = "Частота") +
  theme_minimal()


# Scatetr plot
ggplot(chess, aes(x = white_games_played, y = black_games_played, color = winner)) +
  geom_point() +  
  
  labs(title = "Залежність кількості ігор для білих та чорних",
       x = "Кількість ігор для білих",
       y = "Кількість ігор для чорних") +
  theme_minimal()


opening_moves <- chess$opening_ply
summary(opening_moves)
hist(opening_moves, main = "Розподіл кількості ходів в початковій позиції", 
     col = "lightblue", xlab = "Кількість ходів", ylab = "Частота")
boxplot(opening_moves, main = "Розподіл кількості ходів в початковій позиції", 
        col = "lightblue", xlab = "Кількість ходів", ylab = "Частота")


cor(chess$turns, chess$opening_ply)



top_10_types <- chess %>%
  count(increment_code, name = "Frequency") %>%
  arrange(desc(Frequency)) %>%
  slice_head(n = 10)


ggplot(top_10_types, aes(x = reorder(increment_code, Frequency), y = Frequency, fill = increment_code)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  scale_fill_viridis_d(option = "Pastel1") +  
  labs(title = "Топ-10 найпопулярніших Типів Інкременту",
       x = "Increment Code",
       y = "Частота") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


library(ggplot2)
library(dplyr)
library(tidyr)


top_10_types <- chess %>%
  count(increment_code, name = "Frequency") %>%
  arrange(desc(Frequency)) %>%
  slice_head(n = 10) %>%
  pull(increment_code)

filtered_chess <- chess %>%
  filter(increment_code %in% top_10_types)

filtered_chess$increment_code <- factor(filtered_chess$increment_code, levels = top_10_types)
long_chess <- filtered_chess %>%
  select(increment_code, white_rating, black_rating) %>%
  pivot_longer(cols = c(white_rating, black_rating), names_to = "player", values_to = "rating")

long_chess$player <- recode(long_chess$player, "white_rating" = "White", "black_rating" = "Black")
ggplot(long_chess, aes(x = increment_code, y = rating, color = player)) +
  geom_jitter(alpha = 0.6, width = 0.2, size = 2) +  
  geom_smooth(method = "lm", se = FALSE, aes(group = player), linetype = "solid", color = "black") + # Лінія тренду
  scale_color_manual(values = c("White" = "blue", "Black" = "red")) + 
  labs(title = "Залежність рейтингу гравців від Increment Code",
       x = "Increment Code", 
       y = "Рейтинг гравця",
       color = "Гравець") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 



library(ggplot2)
library(dplyr)
library(tidyr)
top_10_types <- chess %>%
  count(increment_code, name = "Frequency") %>%
  arrange(desc(Frequency)) %>%
  slice_head(n = 10) %>%
  pull(increment_code)

filtered_chess <- chess %>%
  filter(increment_code %in% top_10_types)
filtered_chess$increment_code <- factor(filtered_chess$increment_code, levels = top_10_types)
long_chess <- filtered_chess %>%
  select(increment_code, white_rating, black_rating, winner) %>%
  pivot_longer(cols = c(white_rating, black_rating), names_to = "player", values_to = "rating")
long_chess$player <- recode(long_chess$player, "white_rating" = "White", "black_rating" = "Black")
long_chess <- long_chess %>%
  filter((player == "White" & winner == "white") | (player == "Black" & winner == "black"))
ggplot(long_chess, aes(x = increment_code, y = rating, color = player)) +
  geom_jitter(alpha = 0.6, width = 0.2, size = 2) + 
  geom_smooth(method = "lm", se = FALSE, aes(group = player), linetype = "solid", color = "black") + # Лінія тренду
  scale_color_manual(values = c("White" = "blue", "Black" = "red")) + 
  labs(title = "Залежність рейтингу переможців від Increment Code",
       x = "Increment Code", 
       y = "Рейтинг гравця",
       color = "Гравець") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Взаємозвязок між кількістю зіграних ігор та переможцем
chess$difference_in_games <- chess$white_games_played - chess$black_games_played
chess$winner_numeric <- ifelse(chess$winner == "white", 1, 0)

library(ggplot2)
ggplot(chess, aes(x = factor(winner_numeric), y = difference_in_games, fill = factor(winner_numeric))) +
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = 0.1, fill = "white", color = "black") + 
  labs(title = "Розподіл різниці в кількості ігор для білих і чорних",
       x = "Переможець",
       y = "Різниця в кількості ігор (білі - чорні)") +
  scale_x_discrete(labels = c("Чорні", "Білі")) + 
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() +
  theme(legend.position = "none")  


# Тепер переходимо до аналізу даних про діаманти

hist(diamonds$price, main = "Розподіл ціни діамантів", 
     col = "lightblue", xlab = "Ціна", ylab = "Частота")
hist(diamonds$depth, main = "Розподіл глибини діамантів", 
     col = "lightblue", xlab = "Глибина", ylab = "Частота")
summary(diamonds$depth)

library(ggplot2)
heatmap_data <- data.frame(
  price = diamonds$price,
  depth = diamonds$depth
)

ggplot(heatmap_data, aes(x = price, y = depth)) +
  stat_bin2d(bins = 50, aes(fill = ..count..)) + 
  scale_fill_viridis_c() +  
  labs(title = "Heatmap ціни та глибини діамантів",
       x = "Ціна",
       y = "Глибина") +
  theme_minimal()

hist(diamonds$carat, main = "Розподіл каратів діамантів", 
     col = "lightblue", xlab = "Карати", ylab = "Частота")
summary(diamonds$carat)

# Побудова скаттерплоту для каратів та глибини діамантів з лінією тренду
ggplot(diamonds, aes(x = carat, y = depth)) +
  geom_point(alpha = 0.5, color = "lightgreen") +  
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(title = "Скаттерплот каратів та глибини діамантів з лінією тренду",
       x = "Карати",
       y = "Глибина") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Побудова скаттерплоту для каратів та ціни діамантів з лінією тренду
correlation <- cor(diamonds$carat, diamonds$price)

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point(alpha = 0.5, color = "blue") + 
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  
  labs(title = paste("Скаттерплот каратів та ціни діамантів\nКореляція: ", round(correlation, 2)),
       x = "Карати",
       y = "Ціна") +
  theme_minimal()

# Класифікація діаманту
ggplot(diamonds, aes(x = cut, fill = cut)) +
  geom_bar() + 
  labs(title = "Розподіл класифікації діамантів",
       x = "Класифікація",
       y = "Частота") +
  theme_minimal()





# Скаттерплот для класу діамантів і ціни
diamonds$cut <- factor(diamonds$cut, levels = c("Fair", "Good", "Very Good", "Premium", "Ideal"))
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_jitter(alpha = 0.5, color = "blue") +
  labs(title = "Залежність класу діаманта та ціни",
       x = "Клас діаманта",
       y = "Ціна") +
  theme_minimal()



library(plotly)

diamonds$color <- factor(diamonds$color, levels = c("J", "I", "H", "G", "F", "E", "D"))



plot_ly(data = diamonds, 
        x = ~cut, 
        y = ~price, 
        z = ~carat, 
        type = "scatter3d", 
        mode = "markers", 
        marker = list(size = 3, color = ~price, colorscale = "Viridis", showscale = TRUE)) %>%
  layout(
    title = list(text = "Клас діаманта, ціна та вага в каратах"),
    scene = list(
      xaxis = list(title = "Клас діаманта", categoryorder = "array", categoryarray = c("J", "I", "H", "G", "F", "E", "D")),
      yaxis = list(title = "Ціна"),
      zaxis = list(title = "Вага в каратах")
    )
  )








