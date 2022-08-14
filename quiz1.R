df <- read.csv("hw1_data.csv")
col <- names(df)
print(col)
df[1:2, ]
n <- nrow(df)
print(n)
last_row <- tail(df, n=2)
print(last_row)
df[47, 1]
missing <- is.na(df$Ozone)
sum(missing)
o <- complete.cases(df$Ozone)
y <- df$Ozone[o]
mean(y)
x <- df$Ozone>31
y <- df$Temp>90
df_new <- subset(df, x & y)
mean(df_new$Solar.R)
u <- df$Month == 6
df_month <- subset(df, u)
mean(df_month$Temp)
v <- df$Month == 5
df_may <- subset(df, v)
comp <- complete.cases(df_may)
df_max <- subset(df_may, comp)
max(df_max$Ozone)
g <- list(2, "a", "b", TRUE)