# '''
# Main target :
# Exploration data, analyze and find out the trend of house price.
# Define the cost–performance ratio under the conditions of selling price, EMI, acquired area, etc. to obtain the best investment object.
# '''
# =================================================
# Phase 1 : Import data
# =================================================
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)

setwd("D:\\Github_version_file_R\\data_set\\marchine_learning_data\\Nashik_apartment_price_prediction")

nashik_house <- read.csv("final_data.csv")

# =================================================
# Phase 2 : Data view
# =================================================
# check data set
str(nashik_house)
dim(nashik_house) # 5496 12
colnames(nashik_house)

# check fixed values
table(nashik_house$housetype) # apart 4323, inde 1173
table(nashik_house$house_condition) # new 1846, old 3650
table(nashik_house$BHK) # 1 ~ 10(2.5 2, 3.5 1)

any(is.na(nashik_house))

# =================================================
# Phase 3 : Clearn data
# =================================================
house_df <- nashik_house

# create col，1 Lakh = 100,000 INR(Rs)
house_df <- house_df %>%
    mutate(
        price_INR = price * 100000,
        month_emi = per_month_emi * 1000
    )

# change coltype 1 USD = 0.013INR
house_df <- house_df %>%
    mutate(
        price_USD = price_INR * 0.013,
        price_INR_sqft = price_INR / total_sqft
    )

# change BHK type to character
house_df$BHK <- as.character(house_df$BHK)

# remove incorrect number of rooms
house_df <- house_df[!(house_df$BHK == 2.5 | house_df$BHK == 3.5), ]

# remove null data
house_df <- house_df[complete.cases(house_df), ]

# remove col
house_df <- house_df %>%
    select(-c("X", "latitude", "longitude"))

# ----------------------------------------------------
# check NA
any(is.na(house_df))

# check data set
dim(house_df) # 3871 13

# check new fixed values
table(house_df$housetype) # apart 2814, inde 1057
table(house_df$house_condition) # new 1769, old 2102
table(house_df$BHK) # 1 ~ 10(no BHK 9)

# check values range
range(house_df$total_sqft) # 150 ~ 40000
range(house_df$price) # 1 ~ 700 Lakh
range(house_df$price_USD) # 100,000 ~ 70,000,000
range(house_df$per_month_emi) # 1.05 ~ 529
range(house_df$month_emi) # 1050 ~ 529,000
str(house_df)

# ============================================
# Phase Four : Data consolidation
# ============================================
# house avg price and sd price
summary(house_df$price)
summary(house_df$price_INR)
summary(house_df$price_USD)
summary(house_df$month_emi)
sd(house_df$price)
sd(house_df$price_INR)
sd(house_df$price_USD)

# ----------------------------------------------------
# summary data set
house_df_v2 <- house_df %>%
    group_by(BHK) %>%
    summarise(
        total = n(),
        avg_price_INR = mean(price_INR),
        sd_price_INR = sd(price_INR),
        avg_sqft = round(mean(total_sqft), digits = 2),
        avg_emi_INR = round(mean(month_emi), digits = 2)
    ) %>%
    mutate(
        price_percentage = paste(round(avg_price_INR / sum(avg_price_INR) * 100, digits = 2), "%"),
        total_percentage = paste(round(total / sum(total) * 100, digits = 2), "%"),
        sqft_percentage = paste(round((avg_sqft / sum(avg_sqft)) * 100, digits = 2), "%"),
        emi_percentage = paste(round((avg_emi_INR / sum(avg_emi_INR)) * 100, digits = 2), "%")
    )

# remove percentage symbol spaces
house_df_v2$price_percentage <- gsub(" ", "", house_df_v2$price_percentage)
house_df_v2$total_percentage <- gsub(" ", "", house_df_v2$total_percentage)
house_df_v2$sqft_percentage <- gsub(" ", "", house_df_v2$sqft_percentage)
house_df_v2$emi_percentage <- gsub(" ", "", house_df_v2$emi_percentage)

# reorder rooms type
house_df_v2$BHK <- factor(house_df_v2$BHK, level = c("1", "2", "3", "4", "5", "6", "7", "8", "10"))

house_df_v2

# ----------------------------------------------------
# house type summary
type_price <- house_df %>%
    group_by(housetype) %>%
    summarise(
        total = n(),
        price_INR_sum = sum(price_INR)
    ) %>%
    mutate(
        price_percentage = paste(round(price_INR_sum / sum(price_INR_sum) * 100, digits = 2), "%"),
        total_percentage = paste(round(total / sum(total) * 100, digits = 2), "%")
    )

# remove percentage symbol spaces
type_price$price_percentage <- gsub(" ", "", type_price$price_percentage)
type_price$total_percentage <- gsub(" ", "", type_price$total_percentage)

type_price

# ----------------------------------------------------
# old & new condition summary
condition_df <- house_df %>%
    group_by(house_condition) %>%
    summarise(
        total = n(),
        price_INR_sum = sum(price_INR)
    ) %>%
    mutate(
        price_percentage = paste(round(price_INR_sum / sum(price_INR_sum) * 100, digits = 2), "%"),
        total_percentage = paste(round(total / sum(total) * 100, digits = 2), "%")
    )

# remove percentage symbol spacess
condition_df$price_percentage <- gsub(" ", "", condition_df$price_percentage)
condition_df$total_percentage <- gsub(" ", "", condition_df$total_percentage)

condition_df

# ----------------------------------------------------
# Adjust the EMI、area ratio, create EMI and Sqft level in the object
level_df <- house_df

level_df <- level_df %>%
    mutate(
        price_INR_ratio = price_INR,
        sqft_level = total_sqft,
        emi_level = month_emi
    )

# classification sqft
level_df$sqft_level <- recode( # car package
    level_df$sqft_level, "lo:710 = 'small'; 711:1600 = 'median'; 1601:2300 = 'large'; 2301:40000 = 'huge'",
    as.factor = TRUE,
    levels = c("small", "median", "large", "huge")
)

# classification emi
level_df$emi_level <- recode(
    level_df$emi_level,
    "lo:1400 = 'expected'; 1401:5600 = 'affordable'; 5601:11200 = 'Investable'; 11201:100000 = 'costly'; 100001:529000 = 'inflated'",
    as.factor = TRUE,
    levels = c("expected", "affordable", "Investable", "costly", "inflated")
)

# ============================================
# Phase FIve : Analytic and Graph
# ============================================
# Rooms type & Price relation
price_max_min <- ggplot(
    data = house_df,
    mapping = aes(x = reorder(BHK, price), y = price, fill = BHK)
) +
    geom_boxplot() +
    scale_y_continuous(
        name = "Price Unit : Lakh",
        breaks = c(0, max(house_df$price), 100)
    ) +
    scale_x_discrete(name = "Rooms Type") +
    labs(
        title = "各房數的價格範圍比較",
        caption = "1 Lakh = 100,000 INR"
    ) + # add note
    guides(fill = guide_legend(title = "Rooms Type"))

price_max_min

# ----------------------------------------------------
# Rooms type & Total number
price_bhk <- ggplot(
    data = house_df_v2,
    mapping = aes(x = reorder(BHK, total), y = total, fill = BHK)
) +
    geom_col() +
    scale_fill_brewer(palette = "Set1") +
    geom_text(
        aes(label = total_percentage),
        vjust = 0.5, # adjust position
        hjust = 0.5
    ) +
    labs(
        title = "市場上流通的房型數比較",
        subtitle = "不同房數總數與所占比例"
    ) +
    xlab("Rooms Type") +
    scale_y_continuous(
        name = "Total Number",
        breaks = c(0, max(house_df_v2$total), 150)
    ) +
    coord_flip() + # graph flip
    guides(fill = guide_legend(title = "Rooms Type"))

price_bhk

# ----------------------------------------------------
# EMI & BHK
bhk_emi <- ggplot(
    data = house_df_v2,
    mapping = aes(x = reorder(BHK, -avg_emi_INR), y = avg_emi_INR, fill = BHK)
) +
    geom_col() +
    scale_fill_brewer(palette = "Set3") +
    geom_text(
        aes(label = emi_percentage),
        vjust = -0.3
    ) +
    labs(
        title = "支付越多是否與房間數成比例?",
        subtitle = "房間數與每月支付EMI比較",
        caption = "Price Unit : INR"
    ) +
    xlab("Rooms type") +
    ylab("AVG EMI") +
    guides(fill = guide_legend(titile = "Rooms Type"))

bhk_emi

# ----------------------------------------------------
# SQFT & BHK
bhk_sqft <- ggplot(
    data = house_df_v2,
    mapping = aes(x = reorder(BHK, -avg_sqft), y = avg_sqft, fill = BHK)
) +
    geom_col() +
    scale_fill_brewer(palette = "Paired") +
    geom_text(
        aes(label = sqft_percentage),
        vjust = -0.3
    ) +
    labs(
        title = "房間數越多是否取得面積越大",
        subtitle = "房間數與面積比較",
        caption = "Area Unit : Square Feet"
    ) +
    xlab("Rooms Type") +
    ylab("AVG Sqft") +
    guides(fill = guide_legend(title = "Rooms Type"))

bhk_sqft

# ----------------------------------------------------
# Apartment & Independent percentage
type_pie <- pie(
    type_price$price_INR_sum,
    labels = type_price$total_percentage,
    col = c("slateblue3", "tan2"),
    main = "不同房屋型態所占比例 | Different House Type Percentage"
)
# add notes
legend(
    "topright",
    legend = c("Apartment", "Independent"),
    title = "House Type",
    fill = c("slateblue3", "tan2"),
    cex = 1.2
)

# ----------------------------------------------------
# New & Old house percentage
pie(
    condition_df$price_INR_sum,
    labels = condition_df$price_percentage,
    col = c("#d425ae", "#5bb91d"),
    main = "新、房屋所佔比例 | New、Old house percentage"
)

legend(
    "topright",
    legend = c("New House", "Old House"),
    title = "House Condition",
    fill = c("#d425ae", "#5bb91d"),
    cex = 1.2 # 字符大小
)

# ----------------------------------------------------
# EMI vs SQFT
emi_sqft <- ggplot(
    data = house_df_v2,
    mapping = aes(x = avg_emi_INR, y = avg_sqft)
) +
    geom_point(
        alpha = 0.6, # transparency透明度
        size = 2.0
    ) +
    geom_smooth(
        method = "loess",
        aes(x = avg_emi_INR, y = avg_sqft)
    ) +
    labs(
        title = "每月EMI越高，所得的面積是否越大?",
        subtitle = "平均EMI vs 平均面積",
        caption = "EMI Unit = INR"
    ) +
    xlab("AVG EMI") +
    scale_y_continuous(
        name = "AVG Square Feet",
        breaks = c(0, max(house_df_v2$avg_sqft))
    )

emi_sqft

# ----------------------------------------------------
# Sqft and EMI Level distribution
emi_sqft_level <- ggplot(
    data = level_df,
    mapping = aes(
        x = emi_level,
        fill = sqft_level
    )
) +
    geom_bar() +
    guides(fill = guide_legend(title = "Sqft Level")) +
    xlab("EMI Level") +
    ylab("Total") +
    labs(title = "經分類後的分布情形", subtitle = "EMI & Sqft")

emi_sqft_level

# ----------------------------------------------------
# Sqft Level and BHK distribution
sqft_bhk_level <- ggplot(
    data = level_df,
    mapping = aes(
        x = sqft_level,
        fill = BHK
    )
) +
    geom_bar() +
    guides(fill = guide_legend(title = "Rooms Type")) +
    xlab("Sqft Level") +
    ylab("Total") +
    labs(title = "各房數對應的面積分布", subtitle = "BHK & Sqft Level")

sqft_bhk_level

# ===================================================
# According to per capita income find an object that people can burden
suitable_object <- level_df %>%
    select(address, owners, housetype, house_condition, BHK, month_emi, price_INR, emi_level, sqft_level) %>%
    filter(emi_level == "affordable", sqft_level == "median", price_INR < 1000000) %>%
    view()

suitable_object