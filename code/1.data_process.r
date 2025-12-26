install.packages("languageserver")
install.packages("dplyr")
install.packages("igraph")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("purrr")
install.packages("ggraph")
install.packages("tidygraph")

#####  数据和包的准备 #####
library(languageserver)
library(dplyr)
library(igraph)
library(tidyverse)
library(lubridate)
library(purrr)
library(data.table)
library(ggraph)
library(tidygraph)

##### Step 0: 读取数据#####
global_business_network <- readRDS("/Users/jane/Documents/1.Global_business_network/Data/global_business_network.rds")
str(global_business_network)

# 读取网络数据（在之前已经得到并已经储存在文件夹）
graph_list <- readRDS("/Users/jane/Documents/Global_business_network_Data/Data/graph_list.rds")
str(graph_list[1])
# 基本五数 + 平均数 + 标准差
with(global_business_network, {
  stats <- c(
    Min   = min(cos_sim, na.rm = TRUE),
    Q1    = quantile(cos_sim, 0.25, na.rm = TRUE),
    Median= median(cos_sim, na.rm = TRUE),
    Mean  = mean(cos_sim, na.rm = TRUE),
    Q3    = quantile(cos_sim, 0.75, na.rm = TRUE),
    Max   = max(cos_sim, na.rm = TRUE),
    SD    = sd(cos_sim,  na.rm = TRUE)
  )
  print(round(stats, 4))
})
# Min Q1.25% Median   Mean Q3.75%    Max     SD 
# 0.8574 0.8697 0.8754 0.8791 0.8848 1.0000 0.0132 

# 选取0.1%绘制相似度分布
set.seed(42)
idx   <- sample.int(nrow(global_business_network), size = 0.001 * nrow(global_business_network))
samp  <- global_business_network$cos_sim[idx]

hist(samp, breaks = 100,  main = "cos_sim (0.1% sample)", xlab = "cos_sim")
lines(density(samp), lwd = 2)
#从此时我就应该认知到相似度是取到0.85至1.0的值，不是一个0-1全分布的！

##### Step 1: 构建单个企业的年度丰度 #####
# 从每年的网络图中提取了企业的活跃度，即边权总和（Abundance）
firm_abundance_index <- lapply(names(graph_list), function(y) {
  g <- graph_list[[y]]
  abun <- strength(g, mode = "all", weights = E(g)$weight)
  data.frame(
    Year = as.integer(y),
    Firm = names(abun),
    Abundance = as.numeric(abun)
  )
})
str(firm_abundance_index)
firm_abundance_df <- do.call(rbind, firm_abundance_index)

# 此时得到一个包含企业、年份和它们年度活跃度的数据框。
str(firm_abundance_index)
saveRDS(firm_abundance_index, file = "firm_abundance_index.rds")
str(firm_abundance_df)
saveRDS(firm_abundance_df, file = "firm_abundance_df.rds")
firm_abundance_index <- readRDS("/Users/jane/Documents/Global_business_network_Data/Analysis daily results/202404data_process/firm_abundance_index.rds")
firm_abundance_df <- readRDS("/Users/jane/Documents/Global_business_network_Data/Data/firm_abundance_df.rds")

##### Step 2: 构建年度系统指标 #####
# 计算所有企业在该年的边权总和为每年系统的总活跃度。
habitat_index <- sapply(graph_list, function(g) {
  sum(E(g)$weight)
})
str(habitat_index)
habitat_index_year <- data.frame(
  Year = as.integer(names(habitat_index)),
  HabitatIndex = as.numeric(habitat_index)
)
str(habitat_index_year)
saveRDS(habitat_index_year, file = "habitat_index_year.rds")
habitat_index_year <- readRDS("/Users/jane/Documents/Global_business_network_Data/Data/habitat_index_year.rds")

##### Step 3: 合并 firm-year 和 H^t 数据 #####
habitat_index_year <- as.data.table(habitat_index_year)
# 遍历 firm_abundance_index 中每一年数据，加上对应 HabitatIndex
firm_abundance_list <- lapply(firm_abundance_index, function(df) {
  df <- as.data.table(df)
  # 左连接 habitat_index 到每个 year's data
  df <- merge(df, habitat_index_year, by = "Year", all.x = TRUE)
  return(df)
})
firm_scaling_data <- rbindlist(firm_abundance_list)
str(firm_scaling_data)
saveRDS(firm_scaling_data, file = "firm_scaling_data.rds")
firm_scaling_data <- readRDS("/Users/jane/Documents/Global_business_network_Data/Data/firm_scaling_data.rds")
str(firm_scaling_data)
unique(firm_scaling_data$Firm) # 由此可知在2000-2021年间共61416家企业曾经存在！

##### Step 4: 对每家企业拟合幂律（log–log 回归） #####
# 对每年活跃的企业进行幂律拟合 找到符合幂律分布的企业
beta_results <- firm_scaling_data %>%
  filter(!is.na(Abundance),
         !is.na(HabitatIndex),
         Abundance > 0,
         HabitatIndex > 0) %>%  # 清洗数据：只保留合法值
  group_by(Firm) %>% 
  filter(n() >= 22) %>%  # 企业至少存活22年
  do({
    tryCatch({
      model <- lm(log(Abundance) ~ log(HabitatIndex), data = .)
      data.frame(
        beta = coef(model)[2],
        alpha = exp(coef(model)[1]),
        r2 = summary(model)$r.squared,
        n = nrow(.)
      )
    }, error = function(e) {
      # 出错时返回 NA 作为空行
      data.frame(beta = NA, alpha = NA, r2 = NA, n = nrow(.))
    })
  }) %>%
  ungroup()
str(beta_results)
saveRDS(beta_results, file = "beta_results.rds")

##### Step 5: 对拟合的数据进行分组 #####
# 在拟合过后，其实有很多企业的拟合优度<0.7,到底占多少比例？
beta_results <- readRDS("/Users/jane/Documents/Global_business_network_Data/Data/beta_results.rds")
# 假设你的数据框是 beta_results
# 查看基本结构
str(beta_results)
unique(beta_results$Firm) #由此可知这里的存活了22年的老企业一共有6503家。

# 设置保存路径，可根据需要修改
out_dir <- "/Users/jane/Documents/Global_business_network_Data/Data"  # 当前目录
# 1. β 直方图
png(filename = paste0("beta_histogram.png"), width = 1500, height = 1200, res = 300)
hist(beta_results$beta,
     breaks = 50,
     col = "lightblue",
     main = "Histogram of Beta (β)",
     xlab = "Beta (β)",
     ylab = "Frequency")
abline(v = 1, col = "red", lty = 2)  # β = 1 的分界线
dev.off()

# 高拟合组的直方图
png(filename = paste0("Distribution of Beta for High Fit Group.png"), width = 1500, height = 1200, res = 300)
high_beta_results <- subset(beta_results, fit_class == "High Fit")
hist(high_beta_results$beta,
     breaks = 50,
     col = "lightblue",
     main = "Distribution of Beta for High Fit Group (β)",
     xlab = "Beta (β)",
     ylab = "Frequency")
abline(v = 1, col = "red", lty = 2)  # β = 1 的分界线
dev.off()

# 2. R² 直方图
png(filename = paste0("r2_histogram.png"), width = 1500, height = 1200, res = 300)

# 绘制直方图
hist(beta_results$r2,
     breaks = 40,
     col = "lightgreen",
     main = "Histogram of R²",
     xlab = "R² (Goodness of Fit)",
     ylab = "Frequency")

# 添加 R² 阈值线
abline(v = 0.3, col = "orange", lwd = 2, lty = 2)  # 中度拟合线
abline(v = 0.7, col = "red", lwd = 2, lty = 2)     # 高度拟合线

# 添加图例
legend("topright", legend = c("R² = 0.3", "R² = 0.7"),
       col = c("orange", "red"), lwd = 2, lty = 2, box.lwd = 0)

dev.off()
# 这幅图描绘了所有的6503家企业中，有多少企业是高拟合有多少企业是低拟合 

# 3. β vs R² 散点图 + 平滑线
png(filename = paste0(out_dir, "beta_vs_r2_scatter.png"), width = 1500, height = 1500, res = 300)
plot(beta_results$beta, beta_results$r2,
     pch = 16, col = rgb(0, 0, 1, 0.4),
     main = "Scatter Plot of Beta vs R²",
     xlab = "Beta (β)",
     ylab = "R²")
lines(lowess(beta_results$beta, beta_results$r2), col = "red", lwd = 2)
dev.off()

# 4. R² 分类柱状图
fit_class <- ifelse(beta_results$r2 >= 0.7, "High Fit",
              ifelse(beta_results$r2 >= 0.3, "Medium Fit", "Low Fit"))
        
beta_results_fit <- beta_results %>%
  mutate(
    r2 = as.numeric(r2),
    fit_class = case_when(
      is.na(r2) ~ "Missing",
      r2 >= 0.7 ~ "High Fit",
      r2 >= 0.3 ~ "Medium Fit",
      TRUE      ~ "Low Fit"
    )
  )
# beta_results$fit_class <- fit_class
fit_table <- table(beta_results$fit_class)
str(beta_results)
str(beta_results_fit)
saveRDS(beta_results_fit, file = "beta_results_fit.rds")

png(filename = paste0(out_dir, "r2_fit_class_barplot.png"), width = 1500, height = 1200, res = 300)
barplot(fit_table,
        col = c("salmon", "orange", "lightgray"),
        main = "Fit Quality Categories (Based on R²)",
        ylab = "Number of Firms")
dev.off()

# 5. 每类的平均 Beta 和 R²
high_idx   <- beta_results$fit_class == "High Fit"
medium_idx <- beta_results$fit_class == "Medium Fit"
low_idx    <- beta_results$fit_class == "Low Fit"

summary_stats <- data.frame(
  Class = c("High Fit", "Medium Fit", "Low Fit"),
  Count = c(sum(high_idx), sum(medium_idx), sum(low_idx)),
  Mean_Beta = c(mean(beta_results$beta[high_idx]),
                mean(beta_results$beta[medium_idx]),
                mean(beta_results$beta[low_idx])),
  SD_Beta = c(sd(beta_results$beta[high_idx]),
              sd(beta_results$beta[medium_idx]),
              sd(beta_results$beta[low_idx])),
  Mean_R2 = c(mean(beta_results$r2[high_idx]),
              mean(beta_results$r2[medium_idx]),
              mean(beta_results$r2[low_idx]))
)

print(summary_stats)

#        Class Count Mean_Beta   SD_Beta    Mean_R2
# 1   High Fit   571 1.8324809 0.9687369 0.78394127
# 2 Medium Fit  2159 1.1081272 0.9611160 0.49262181
# 3    Low Fit  3773 0.1987953 0.5091648 0.09743292

# 设置图像输出
png("fit_class_counts.png", width = 1500, height = 1200, res = 300)

barplot(summary_stats$Count,
        names.arg = summary_stats$Class,
        col = c("tomato", "gold", "skyblue"),
        main = "Number of Firms in Each R² Fit Class",
        ylab = "Firm Count")

dev.off()
png("mean_beta_by_fit_class.png", width = 1500, height = 1200, res = 300)

bar_centers <- barplot(summary_stats$Mean_Beta,
                       names.arg = summary_stats$Class,
                       col = c("tomato", "gold", "skyblue"),
                       main = "Mean Beta (β) by R² Fit Class",
                       ylab = "Mean Beta")

# 添加误差条（±1 SD）
arrows(x0 = bar_centers,
       y0 = summary_stats$Mean_Beta - summary_stats$SD_Beta,
       x1 = bar_centers,
       y1 = summary_stats$Mean_Beta + summary_stats$SD_Beta,
       angle = 90, code = 3, length = 0.1)

dev.off()
png("mean_r2_by_fit_class.png", width = 1500, height = 1200, res = 300)

barplot(summary_stats$Mean_R2,
        names.arg = summary_stats$Class,
        col = c("tomato", "gold", "skyblue"),
        main = "Mean R² by Fit Class",
        ylab = "Mean R²")

dev.off()


##### Step 5.1 : 按照拟合优度将高拟合企业进行重点分析，进入模块聚类 #####
saveRDS(beta_results, file = "/home/newuser/Documents/Global_business_network_Data/Business network/beta_results.rds")
str(global_business_network)
beta_results_fit <- readRDS("/Users/jane/Documents/Global_business_network_Data/Data/beta_results_fit.rds")
str(beta_results_fit)
unique(beta_results_fit$fit_class)

##### Step 5.2: 可视化企业的幂律指数 #####
png("beta_scaling.png", width = 1500, height = 2000, res = 300)
hist(beta_results$beta, breaks = 50,
    main = "Distribution of Firm-level β",
    xlab = expression(beta[i]), col = "skyblue")
  # 标记不同响应类型
    abline(v = 1, col = "red", lty = 2)  # β = 1 的分界线
# 关闭图像设备（非常重要！）
dev.off()

# Conclusion：
# 绝大多数企业的\beta在0-2之间，说明它们的响应是比例增长或者温和放大的。 少部分企业\beta<0,这些是反向响应，即系统增长它们反而减少。
# \beta是强放大响应者，对系统波动非常敏感。
##### Step 5.3 基于高拟合结果的beta分布结果 #####

high_fit_firms <- subset(beta_results_fit, fit_class == "High Fit")
str(high_fit_firms)
unique(high_fit_firms$Firm)

dt <- as.data.table(global_business_network)
dt_sub_2000 <- dt[identifier1 %in% high_ids & identifier2 %in% high_ids & year == 2000]
bone_dt_sub_2000 <- dt_sub_2000[cos_sim >= 0.85]
str(bone_dt_sub_2000)  

# 2000年高拟合企业骨架网络
# 加载必要包
library(igraph)
library(data.table)

# Step 1: 准备边数据（已是 data.table）
edges <- bone_dt_sub_2000[, .(from = identifier1, to = identifier2, weight = cos_sim)]

# Step 2: 构建 igraph 对象（无向图）
g_bone <- graph_from_data_frame(edges, directed = FALSE)

# Step 3: 基本网络可视化
png("Skeleton Network of High-Fit Firms in 2000.png", width = 2000, height = 1500, res = 300)
plot(g_bone,
     vertex.label = NA,
     vertex.size = 4,
     edge.width = E(g_bone)$weight * 5,
     layout = layout_with_fr,  # 力导布局，适合观察结构
     main = "Skeleton Network of High-Fit Firms in 2000")
dev.off()
# 网络分析
# 网络密度（越高越集中）
cat("Network density:", edge_density(g_bone), "\n")

# 顶点度分布（找出中心企业）
deg <- degree(g_bone)
cat("Top 5 most connected firms:\n")
print(sort(deg, decreasing = TRUE)[1:5])

# 连通组件
comp <- components(g_bone)
cat("Number of connected components:", comp$no, "\n")

##### Step 5.4 高拟合企业的beta值都是怎么样的？ #####
high_fit_firms <- readRDS("/home/newuser/Documents/Global_business_network_Data/Business network/202404data_process/high_fit_firms.rds")
str(high_fit_firms)
summary(high_fit_firms$beta)
sd(high_fit_firms$beta)

png("Distribution of β among High-Fit Firms.png", width = 2000, height = 1500, res = 300)
hist(high_fit_firms$beta,
     breaks = 40,
     col = "skyblue",
     main = "Distribution of β among High-Fit Firms",
     xlab = "β (Allometric Exponent)",
     ylab = "Number of Firms")
abline(v = 1, col = "red", lty = 5)  # 加一条红线表示 β=1（系统一致响应点）
dev.off()
# 分布偏右，整体上这群高拟合企业呈现“系统响应放大效应”。在1-2之间呈现的局部聚集的企业是典型地“随系统共振”可能是供应链上游（受影响）或下游（快速扩张者）。
# 整体的结果说明供应链中的关键企业并非稳定器而是系统趋势的“倍增器”。

plot(density(high_fit_firms$beta),
     main = "Kernel Density of β",
     xlab = "β", col = "darkblue", lwd = 2)
abline(v = 1, col = "red", lty = 2)  # 加一条红线表示 β=1（系统一致响应点）
high_fit_firms$response_type <- cut(
  high_fit_firms$beta,
  breaks = c(-Inf, 0.95, 1.05, Inf),
  labels = c("Stabilizer", "Neutral", "Amplifier")
)
table(high_fit_firms$response_type)
barplot(table(high_fit_firms$response_type),
        col = c("lightgreen", "gold", "tomato"),
        main = "Firm Type by β Value",
        ylab = "Number of Firms")

# 
##### Step 6: 拟合多家企业的点和loess曲线 #####
firm_scaling_data <- readRDS("Global_business_network_Data/Business network/202404data_process/firm_scaling_data.rds")
str(firm_scaling_data)

high_fit_firms <- readRDS("/Users/jane/Documents/Global_business_network_Data/Analysis daily results/202404data_process/high_fit_firms.rds")
str(high_fit_firms)
unique(high_fit_firms$Firm)

# 确保必要包加载
library(data.table)
library(ggplot2)

# 保证Firm列一致
firm_scaling_data[, Firm := as.character(Firm)]
high_fit_firms$Firm <- as.character(high_fit_firms$Firm)

# 筛选出高拟合企业数据
high_fit_firm_scaling_data <- firm_scaling_data[Firm %in% high_fit_firms$Firm]

# 随机挑选40家企业
set.seed(123)
selected_firms <- sample(unique(high_fit_firm_scaling_data$Firm), 40)
selected_data <- high_fit_firm_scaling_data[Firm %in% selected_firms]

# 加上log变换
selected_data[, log_Abundance := log10(Abundance + 1)]
selected_data[, log_HabitatIndex := log10(HabitatIndex + 1)]

# 合并beta和r2
selected_data <- merge(selected_data, high_beta_results[, c("Firm", "beta", "r2")], by = "Firm", all.x = TRUE)
str(selected_data)
# 创建beta分类
selected_data[, beta_group := fifelse(beta < 0, "beta < 0",
                              fifelse(beta >= 0 & beta < 1, "0 <= beta < 1", 
                                      "beta >= 1"))]
str(selected_data)
# 创建label文本
selected_data[, label_text := paste0("β = ", round(beta, 2), "\nR² = ", round(r2, 2))]

# 绘图
p <- ggplot(selected_data, aes(x = log_HabitatIndex, y = log_Abundance, color = beta_group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
  facet_wrap(~ Firm, nrow = 10, ncol = 4, scales = "free") +
  theme_bw() +
  geom_text(data = unique(selected_data[, .(Firm, log_HabitatIndex, log_Abundance, label_text)]), 
            aes(x = -Inf, y = Inf, label = label_text), 
            inherit.aes = FALSE, hjust = -0.1, vjust = 1.1, size = 2.5) +
  scale_color_manual(values = c("beta < 0" = "red",
                                "0 <= beta < 1" = "blue",
                                "beta >= 1" = "green")) +
  labs(x = "log10(Habitat Index)", y = "log10(Firm Abundance)",
       title = "Power Law Fitting for 40 Randomly Selected Firms (Colored by Beta Group)") +
  theme(strip.text = element_text(size = 7))

# 保存
ggsave(filename = "powerlaw_fitting_40firms_with_labels.png", plot = p, 
       width = 16, height = 20, units = "in", dpi = 600)
# 这张图验证了“企业响应系统变化存在异质性”的理论假设。有的企业是系统变化的放大器，有的是缓冲器，甚至还有反向响应者。

##### Step  根据拟合优度和beta进行企业特征进行统计 #####
firm_scaling_data <- readRDS("/Users/jane/Documents/Global_business_network_Data/Data/firm_scaling_data.rds")
str(firm_scaling_data)
beta_results <- readRDS("/Users/jane/Documents/Global_business_network_Data/Data/beta_results.rds")
str(beta_results)

library(data.table)
library(dplyr)
library(ggplot2)

# Step 1: 保证 Firm 字段一致
firm_scaling_data[, Firm := as.character(Firm)]
beta_results_fit$Firm <- as.character(beta_results_fit$Firm)
str(firm_scaling_data)
str(beta_results_fit)
# Step 2: 合并 beta 信息
merged_data <- merge(firm_scaling_data, beta_results_fit[, c("Firm", "beta", "fit_class")], 
                     by = "Firm", all.x = TRUE)
str(merged_data)

# Step 3: 创建分组标签
merged_data <- merged_data %>%
  mutate(group = case_when(
    fit_class == "Low Fit" ~ "Low Fit",
    fit_class == "High Fit" & beta < 0 ~ "High Fit: beta < 0",
    fit_class == "High Fit" & beta >= 0 & beta < 1 ~ "High Fit: 0 <= beta < 1",
    fit_class == "High Fit" & beta >= 1 ~ "High Fit: beta >= 1",
    TRUE ~ NA_character_
  ))

# Step 4: 过滤掉没有分组的信息（只分析符合分类的企业）
merged_data_filtered <- merged_data %>%
  filter(!is.na(group))
str(merged_data_filtered)
unique(merged_data_filtered$group)
saveRDS(merged_data_filtered, file = "/home/newuser/Documents/Global_business_network_Data/Business network/merged_data_filtered.rds")

# Step 5: 按年份和分组求 Abundance 的均值
abundance_summary <- merged_data_filtered %>%
  group_by(Year, group) %>%
  summarise(mean_abundance = mean(Abundance, na.rm = TRUE), .groups = "drop")

# 查看结果
head(abundance_summary)
# > head(abundance_summary)
# # A tibble: 6 × 3
#    Year group                   mean_abundance
#   <int> <chr>                            <dbl>
# 1  2000 High Fit: 0 <= beta < 1          64.5 
# 2  2000 High Fit: beta < 0              111.  
# 3  2000 High Fit: beta >= 1               9.11
# 4  2000 Low Fit                          53.7 
# 5  2001 High Fit: 0 <= beta < 1          73.3 
# 6  2001 High Fit: beta < 0              130.  

# Step 6: 绘制分组时间变化曲线
install.packages("viridis")
library(viridis)  # 安装一次就好
 abundance_summary_group_plot <- ggplot(abundance_summary, aes(x = Year, y = mean_abundance, color = group)) +
  geom_line(size = 1.8) +  # 线条加粗
  geom_point(size = 1.2, alpha = 0.7) +  # 点变小且半透明
  scale_color_viridis_d(option = "D", end = 0.8) +  # 色盲友好调色
  geom_vline(xintercept = 2017.5, linetype = "dashed", color = "black", linewidth = 0.8) +  # 贸易战标线
  annotate("text", x = 2018, y = max(abundance_summary$mean_abundance) * 0.95, 
          label = "Trade War", hjust = 0, size = 4, fontface = "italic") +
  labs(
    x = "Year", 
    y = "Mean Firm Abundance", 
    color = NULL,  # 图例不要标题
    title = "Firm Activity Dynamics by Functional Group (2000–2021)"
  ) +
  theme_minimal(base_family = "Arial", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),   # 删除竖线
    panel.grid.minor = element_blank(),      # 删除小网格
    panel.grid.major.y = element_line(color = "grey80"),  # 保留横线
    axis.line = element_line(size = 1, color = "black"),  # 坐标轴加粗
    axis.ticks = element_line(size = 1),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    plot.margin = margin(10, 10, 10, 10)
  ) 
ggsave(filename = "abundance_summary_group_plot.png", plot = abundance_summary_group_plot, 
       width = 20, height = 16, units = "in", dpi = 600)
#  现象：韧性分层现象（Resilience Stratification）
#  高拟合beta>1组从2000年开始就保持了稳健持续增长，贸易战之后依旧维持在高位。这些企业极有可能是供应链中的主导放大器，他们的规模或活跃度受系统总体变化强烈放大，且具有强大的适应性和韧性。
#  高拟合 0<beta<1,也在持续增长，但是增长速度较慢，这些系统对系统变化呈现比例响应，表现为一种稳定随动的行为，类似于生态系统中的“跟随者”物种。
#  高拟合 beta<0组，整体缓慢下降或波动下降，在2005年和2015年附近有明显的波动。 这批企业对系统变化呈反向响应，在系统扩张时缩小，可能是一些易受挤压、退场或边缘化的供应链角色。
#  低拟合始终保持在较低的abundance水平，没有显著增长。
#  贸易战后的结构性变化：几乎所有组别在2018年附近都有趋势变化，系统性的外部冲击会加剧企业间的异质性，强者愈强，弱者更弱，放大了供应链系统的极化现象（polarization）。
# Results：整个商业网络并非均匀扩张，而是由一小部分强烈放大响应的企业（高beta）主导驱动，其他企业则随动、滞后或者消亡。
# Theory：支持了复杂系统中“幂律分布+核心-边缘结构”的经典假设。系统冲击放大了企业之间原有的功能性差异，韧性和脆弱性在不同群体中呈现出清晰的分层。
# Application：在供应链管理、政策干预中，应该识别并优化保护或引导这些高beta企业，因为它们是系统活性的主要贡献者和风险扩散源。

# Figure 2 ：Mean ± 95% CI Ribbon Line Plot
# Step 1: 保证 Firm 字段一致
firm_scaling_data[, Firm := as.character(Firm)]
beta_results$Firm <- as.character(beta_results$Firm)

# Step 2: 合并 beta 信息
merged_data <- merge(firm_scaling_data, beta_results_fit[, c("Firm", "beta", "fit_class")], 
                     by = "Firm", all.x = TRUE)

# Step 3: 创建分组标签
merged_data <- merged_data %>%
  mutate(group = case_when(
    fit_class == "Low Fit" ~ "Low Fit",
    fit_class == "High Fit" & beta < 0 ~ "High Fit: beta < 0",
    fit_class == "High Fit" & beta >= 0 & beta < 1 ~ "High Fit: 0 <= beta < 1",
    fit_class == "High Fit" & beta >= 1 ~ "High Fit: beta >= 1",
    TRUE ~ NA_character_
  ))

# Step 4: 过滤掉没有分组的信息（只分析符合分类的企业）
merged_data_filtered <- merged_data %>%
  filter(!is.na(group))

# Step 5: 按年份和分组求均值 + 标准误
abundance_summary_ci <- merged_data_filtered %>%
  group_by(Year, group) %>%
  summarise(
    mean_abundance = mean(Abundance, na.rm = TRUE),
    se = sd(Abundance, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_abundance - 1.96 * se,  # 95% CI 下界
    upper = mean_abundance + 1.96 * se   # 95% CI 上界
  )

# Step 6: 绘图
library(ggplot2)
library(viridis)

abundance_summary_group_plot_ci <- ggplot(abundance_summary_ci, aes(x = Year, y = mean_abundance, color = group, fill = group)) +
  geom_line(size = 1.8) +  # 均值曲线
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, color = NA) +  # 阴影区域（置信区间）
  geom_point(size = 1.2, alpha = 0.7) +
  scale_color_viridis_d(option = "D", end = 0.8) +
  scale_fill_viridis_d(option = "D", end = 0.8) +
  geom_vline(xintercept = 2017.5, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = 2018, y = max(abundance_summary_ci$upper, na.rm = TRUE) * 0.95, 
         label = "Trade War", hjust = 0, size = 4, fontface = "italic") +
  labs(
    x = "Year", 
    y = "Mean Firm Abundance ± 95% CI", 
    color = NULL,  # 图例去掉标题
    fill = NULL,
    title = "Firm Activity Dynamics by Functional Group (2000–2021)"
  ) +
  theme_minimal(base_family = "Arial", base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    axis.line = element_line(size = 1, color = "black"),
    axis.ticks = element_line(size = 1),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    plot.margin = margin(10, 10, 10, 10)
  )

# Step 7: 保存高清图片
ggsave(filename = "abundance_summary_group_plot_CI.png", plot = abundance_summary_group_plot_ci, 
       width = 20, height = 16, units = "in", dpi = 600)

# Figure 3 ： 带误差条的折线图
# Step 1: 保证数据一致
firm_scaling_data[, Firm := as.character(Firm)]
beta_results$Firm <- as.character(beta_results$Firm)

# Step 2: 合并 beta 信息
merged_data <- merge(firm_scaling_data, beta_results[, c("Firm", "beta", "fit_class")], 
                     by = "Firm", all.x = TRUE)

# Step 3: 创建分组标签
merged_data <- merged_data %>%
  mutate(group = case_when(
    fit_class == "Low Fit" ~ "Low Fit",
    fit_class == "High Fit" & beta < 0 ~ "High Fit: beta < 0",
    fit_class == "High Fit" & beta >= 0 & beta < 1 ~ "High Fit: 0 <= beta < 1",
    fit_class == "High Fit" & beta >= 1 ~ "High Fit: beta >= 1",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(group))
str(merged_data)
unique(merged_data$group)

# Step 4: 计算每年每组的均值、标准差、样本量
abundance_summary <- merged_data %>%
  group_by(Year, group) %>%
  summarise(
    mean_abundance = mean(Abundance, na.rm = TRUE),
    sd_abundance = sd(Abundance, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_abundance / sqrt(n),  # 标准误
    upper = mean_abundance + 1.96 * se,  # 上置信区间
    lower = mean_abundance - 1.96 * se   # 下置信区间
  )

# Step 5: 绘制
p <- ggplot(abundance_summary, aes(x = Year, y = mean_abundance, color = group)) +
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5, linewidth = 0.8) +  # 加误差条
  geom_vline(xintercept = 2017.5, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = 2018, y = max(abundance_summary$mean_abundance, na.rm = TRUE) * 0.95, 
           label = "Trade War", hjust = 0, size = 4, fontface = "italic") +
  scale_color_viridis_d(option = "D", end = 0.85) +
  labs(
    title = "Firm Activity Dynamics by Functional Group (2000–2021)",
    x = "Year",
    y = "Mean Firm Abundance ± 95% CI",
    color = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# 保存
ggsave(filename = "abundance_with_errorbars.png", plot = p, 
       width = 14, height = 10, units = "in", dpi = 600)

# Figure 4: 局部平滑（Loess曲线+点）
library(data.table)
library(dplyr)
library(ggplot2)

# Step 1: 保证数据一致
firm_scaling_data[, Firm := as.character(Firm)]
str(firm_scaling_data)
unique(firm_scaling_data$Firm)

beta_results$Firm <- as.character(beta_results$Firm)
str(beta_results)
unique(beta_results$Firm)

# Step 2: 合并 beta 信息
merged_data <- merge(firm_scaling_data, beta_results[, c("Firm", "beta", "fit_class")], 
                     by = "Firm", all.x = TRUE)

str(merged_data)
unique(merged_data$Firm)

# Step 3: 创建分组标签
merged_data <- merged_data %>%
  mutate(group = case_when(
    fit_class == "Low Fit" ~ "Low Fit",
    fit_class == "High Fit" & beta < 0 ~ "High Fit: beta < 0",
    fit_class == "High Fit" & beta >= 0 & beta < 1 ~ "High Fit: 0 <= beta < 1",
    fit_class == "High Fit" & beta >= 1 ~ "High Fit: beta >= 1",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(group))
str(merged_data) # 将beta拟合后的企业的数据筛选出来 
unique(merged_data$Firm) 

# Step 4: 计算每年每组的均值
abundance_summary <- merged_data %>%
  group_by(Year, group) %>%
  summarise(
    mean_abundance = mean(Abundance, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Step 5: 绘制 Loess 平滑图
p <- ggplot(abundance_summary, aes(x = Year, y = mean_abundance, color = group)) +
  geom_point(size = 2, alpha = 0.7) +   # 小点，标注每年均值
  geom_smooth(method = "loess", se = TRUE, span = 0.5, linewidth = 1.5, alpha = 0.2) +  # Loess曲线
  geom_vline(xintercept = 2017.5, linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = 2018, y = max(abundance_summary$mean_abundance, na.rm = TRUE) * 0.95, 
           label = "Trade War", hjust = 0, size = 4, fontface = "italic") +
  scale_color_viridis_d(option = "D", end = 0.85) +
  labs(
    title = "Firm Activity Dynamics by Functional Group (2000–2021)",
    x = "Year",
    y = "Mean Firm Abundance",
    color = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# 保存
ggsave(filename = "abundance_loess_trend.png", plot = p, 
       width = 14, height = 10, units = "in", dpi = 600)
# 我觉得loess平滑图是最好看的！


##### Step 6.3 定量化组间差异 #####
# 比较增长率（Growth Rate）& 年均增长率（CAGR）
firm_scaling_data <- readRDS("/Users/jane/Documents/1.Global_business_network/Data/firm_scaling_data.rds")
beta_results_fit <- readRDS("/Users/jane/Documents/1.Global_business_network/Data/beta_results_fit.rds")
merged_data_filtered <- readRDS("/Users/jane/Documents/1.Global_business_network/Data/merged_data_filtered.rds")

library(tidyr)
str(firm_scaling_data)
str(beta_results_fit)
str(merged_data_filtered) 

# Step 1: 按 group 和 Year 聚合，计算每组每年的 mean(Abundance)
abundance_by_group_year <- merged_data_filtered %>%
  group_by(group, Year) %>%
  summarise(mean_abundance = mean(Abundance, na.rm = TRUE), .groups = "drop")

# Step 2: 只提取 2000年 和 2021年 的数据
abundance_start_end <- abundance_by_group_year %>%
  filter(Year %in% c(2000, 2021)) %>%
  pivot_wider(names_from = Year, values_from = mean_abundance, names_prefix = "Year_")

# Step 3: 计算每组的总增长率和 CAGR
abundance_growth <- abundance_start_end %>%
  mutate(
    Total_Growth = (Year_2021 - Year_2000) / Year_2000,
    CAGR = (Year_2021 / Year_2000)^(1 / (2021 - 2000)) - 1
  ) %>%
  select(group, Year_2000, Year_2021, Total_Growth, CAGR) %>%
  arrange(desc(CAGR))

# Step 4: 查看结果
print(abundance_growth)
#   group                   Year_2000 Year_2021 Total_Growth    CAGR
#   <chr>                       <dbl>     <dbl>        <dbl>   <dbl>
# 1 High Fit: beta >= 1          9.11     152.        15.7    0.143 
# 2 High Fit: 0 <= beta < 1     64.5      221.         2.43   0.0604
# 3 Low Fit                     53.7       95.0        0.769  0.0275
# 4 High Fit: beta < 0         111.        21.2       -0.809 -0.0758

# 拟合线性回归模型：\text{mean\_abundance} = a + b \times \text{Year}

library(dplyr)
library(broom)  # 用来提取回归系数
library(ggplot2)

# Step 1: 先按 group 和 year 求均值
mean_abundance_per_year <- merged_data_filtered %>%
  group_by(group, Year) %>%
  summarise(mean_abundance = mean(Abundance, na.rm = TRUE), .groups = "drop")

# Step 2: 对每个组分别做线性回归
library(dplyr)
library(tidyr)
library(purrr)
# Step 1: 按 group 和 year 计算 mean_abundance
mean_abundance_by_group <- merged_data_filtered %>%
  group_by(group, Year) %>%
  summarise(mean_abundance = mean(Abundance, na.rm = TRUE), .groups = "drop")

# Step 2: 按 group 分组拟合线性回归 (mean_abundance ~ Year)
# 使用 nest + mutate + map 自动化回归
nested_data <- mean_abundance_by_group %>%
  group_by(group) %>%
  nest()

# 对每个组拟合线性模型
nested_data <- nested_data %>%
  mutate(model = map(data, ~ lm(mean_abundance ~ Year, data = .x)))

# 提取模型系数（截距、斜率）
model_coefficients <- nested_data %>%
  mutate(tidy_model = map(model, broom::tidy)) %>%
  unnest(tidy_model) %>%
  select(group, term, estimate)

# 还可以提取模型R²、F值等总结指标
model_summaries <- nested_data %>%
  mutate(glance_model = map(model, broom::glance)) %>%
  unnest(glance_model) %>%
  select(group, r.squared, adj.r.squared, p.value, statistic)

# 最后输出结果
print(model_coefficients)
#   group                   term         estimate
#   <chr>                   <chr>           <dbl>
# 1 High Fit: 0 <= beta < 1 (Intercept) -15687.  
# 2 High Fit: 0 <= beta < 1 Year             7.88
# 3 High Fit: beta < 0      (Intercept)   8817.  
# 4 High Fit: beta < 0      Year            -4.36
# 5 High Fit: beta >= 1     (Intercept) -16014.  
# 6 High Fit: beta >= 1     Year             8.01
# 7 Low Fit                 (Intercept)  -2863.  
# 8 Low Fit                 Year             1.47
# 企业的系统响应能力显著影响了其长期活跃度的演变轨迹，高响应企业增长最稳定，反向响应和低拟合企业则表现出更大的波动性和异质性。
print(model_summaries)
# group                   r.squared adj.r.squared  p.value statistic
#   <chr>                       <dbl>         <dbl>    <dbl>     <dbl>
# 1 High Fit: 0 <= beta < 1     0.936         0.933 1.98e-13     294. 
# 2 High Fit: beta < 0          0.590         0.569 3.01e- 5      28.8
# 3 High Fit: beta >= 1         0.952         0.950 1.17e-14     397. 
# 4 Low Fit                     0.676         0.660 2.65e- 6      41.8
# 结果明确揭示了企业韧性在系统演化过程中的分层结构：具有良好拟合特征（High Fit）且响应指数适中的企业，
# 展现出更高的增长一致性与系统适应能力；而响应反向或低拟合企业则面临更高的不确定性与波动风险。这一发现对理解供应链网络中企业抗冲击与适应性机制提供了实证支持。

# Step 3 回归诊断表
library(dplyr)
library(purrr)
library(broom)        # tidy() / glance()
library(car)          # vif()
library(lmtest)       # bptest()
library(performance)  # check_normality(), check_heteroscedasticity()…
library(kableExtra)   # 表格排版，可换成 writexl::write_xlsx

install.packages("performance")
install.packages("lmtest")

# 1. 定义一个函数：对单个 lm 输出诊断
diag_one <- function(model) {
  g       <- glance(model)           # R²、AIC、BIC、F 统计量…
  coefs   <- tidy(model) %>%         # 系数、SE、t、p
               filter(term == "Year")
  n       <- nobs(model)

  # VIF 只有一个自变量时是 1，这里留接口便于多元回归
  vif_val <- tryCatch(
    sqrt(car::vif(model)["Year"]),   # √VIF = √(1⁄(1-R²_j))
    error = function(e) NA_real_
  )

  # 残差检验
  bp_p    <- lmtest::bptest(model)$p.value   # Breusch-Pagan
  dw      <- lmtest::dwtest(model)$statistic # Durbin-Watson

  tibble(
    Intercept  = coef(model)[1],
    Slope      = coefs$estimate,
    SE_Slope   = coefs$std.error,
    t_Slope    = coefs$statistic,
    p_Slope    = coefs$p.value,
    R2         = g$r.squared,
    Adj_R2     = g$adj.r.squared,
    AIC        = g$AIC,
    BIC        = g$BIC,
    F_stat     = g$statistic,
    RMSE       = sqrt(mean(residuals(model)^2)),
    `√VIF`     = vif_val,
    BP_p       = bp_p,
    DW         = dw,
    n          = n
  )
}

# 2. 批量应用到每个组
diagnostics_tbl <- nested_data %>%
  mutate(diag = map(model, diag_one)) %>%
  select(group, diag) %>%
  unnest(diag) %>%
  arrange(desc(R2))

# 3. 打印 / 导出
# 3a. 终端快速查看
print(diagnostics_tbl, n = Inf, width = Inf)
# # A tibble: 4 × 16
# # Groups:   group [4]
#   group                   Intercept Slope SE_Slope t_Slope  p_Slope    R2 Adj_R2
#   <chr>                       <dbl> <dbl>    <dbl>   <dbl>    <dbl> <dbl>  <dbl>
# 1 High Fit: beta >= 1       -16014.  8.01    0.402   19.9  1.17e-14 0.952  0.950
# 2 High Fit: 0 <= beta < 1   -15687.  7.88    0.460   17.2  1.98e-13 0.936  0.933
# 3 Low Fit                    -2863.  1.47    0.227    6.46 2.65e- 6 0.676  0.660
# 4 High Fit: beta < 0          8817. -4.36    0.813   -5.36 3.01e- 5 0.590  0.569
#     AIC   BIC F_stat  RMSE `√VIF`   BP_p    DW     n
#   <dbl> <dbl>  <dbl> <dbl>  <dbl>  <dbl> <dbl> <int>
# 1  176.  179.  397.  11.4      NA 0.120  0.821    22
# 2  181.  185.  294.  13.0      NA 0.193  0.658    22
# 3  150.  154.   41.8  6.43     NA 0.0184 0.263    22

# 3b. 论文排版（LaTeX 或 Word）
kable(diagnostics_tbl, format = "latex", booktabs = TRUE,
      digits = 3, caption = "回归诊断表（2000–2021）") %>%
  kable_styling(position = "center", font_size = 9)

# 3c. 导出 Excel 供后期美化
# writexl::write_xlsx(diagnostics_tbl, "diagnostics.xlsx")

# ========== 下面是更高级的一步，直接做 ANCOVA 检验 ========= #

# Step 5: ANCOVA，检验各组之间的斜率是否显著不同
# 首先：要在一张表里回归，所以不能先算均值，直接用原始 merged_data_filtered
# 回归模型：Abundance ~ Year * group
ancova_model <- lm(Abundance ~ Year * group, data = merged_data_filtered)

# 查看交互项结果
summary(ancova_model)
# Call:
# lm(formula = Abundance ~ Year * group, data = merged_data_filtered)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -239.22  -61.87  -26.65   35.32  832.47 

# Coefficients:
#                                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   -1.569e+04  5.201e+02 -30.160  < 2e-16 ***
# Year                           7.883e+00  2.587e-01  30.469  < 2e-16 ***
# groupHigh Fit: beta < 0        2.450e+04  6.132e+03   3.996 6.45e-05 ***
# groupHigh Fit: beta >= 1      -3.273e+02  5.974e+02  -0.548    0.584    
# groupLow Fit                   1.282e+04  5.295e+02  24.216  < 2e-16 ***
# Year:groupHigh Fit: beta < 0  -1.224e+01  3.050e+00  -4.014 5.96e-05 ***
# Year:groupHigh Fit: beta >= 1  1.277e-01  2.972e-01   0.430    0.667    
# Year:groupLow Fit             -6.417e+00  2.634e-01 -24.363  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 90.43 on 95560 degrees of freedom
# Multiple R-squared:  0.06828,   Adjusted R-squared:  0.06821 
# F-statistic:  1000 on 7 and 95560 DF,  p-value: < 2.2e-16
# 特别关注：Year:group 交互项的 p 值
# 如果 Year:group 的交互显著，说明不同组的增长趋势（斜率）不同！
# 韧性分层现象得到支持，不同类型企业在增长斜率上存在显著差异。
# ========== 可选，绘制回归趋势图 ========== #

# 基于均值画趋势
Mean_abundance_trends_by_group <- ggplot(mean_abundance_per_year, aes(x = Year, y = mean_abundance, color = group)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  theme_minimal() +
  labs(title = "Mean Abundance Trends by Group", x = "Year", y = "Mean Abundance")

ggsave(filename = "Mean Abundance Trends by Group.png", plot = Mean_abundance_trends_by_group, 
       width = 14, height = 10, units = "in", dpi = 600)


##### Step 7.1: 绘制高拟合的企业网络 ###### 
str(merged_data_filtered)
str(global_business_network)

# Step 1: 提取高拟合企业的 Firm 列表
high_fit_firms <- merged_data_filtered %>%
  filter(fit_class == "High Fit") %>%
  distinct(Firm) %>%
  pull(Firm) %>%
  as.integer()  # 注意你的 global_business_network 是整数型ID
str(high_fit_firms)
# 检查数量
length(high_fit_firms)

# Step 2: 筛选global_business_network中的高拟合企业对
high_fit_network <- global_business_network %>%
  filter(
    identifier1 %in% high_fit_firms & 
    identifier2 %in% high_fit_firms
  )
# 检查筛选后网络规模
nrow(high_fit_network)
head(high_fit_network)
str(high_fit_network)
saveRDS(high_fit_network, file = "/home/newuser/Documents/Global_business_network_Data/Business network/high_fit_network.rds")

# 加载必要的包
library(igraph)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(scales)
library(RColorBrewer)
library(dplyr)

# 1. 提取某一年年数据
network_2021 <- high_fit_network[high_fit_network$year == 2021, ]

# 2. 计算每个节点的总cos_sim
node_strength <- network_2021 %>%
  group_by(identifier1) %>%
  summarise(total_cos = sum(cos_sim)) %>%
  rename(node = identifier1)

# 3. 创建igraph对象
g <- graph_from_data_frame(
  d = network_2021[, c("identifier1", "identifier2", "cos_sim")],
  directed = FALSE
)
str(g)

# 注意，cos_sim 是已有的属性了！正确从 g 里取出来
E(g)$distance <- 1 - E(g)$cos_sim + 1e-6 # 一定要保证distance严格大于0

node_size <- setNames(node_strength$total_cos, node_strength$node)
V(g)$strength <- node_size[as.character(V(g)$name)]
V(g)$strength[is.na(V(g)$strength)] <- min(node_size, na.rm = TRUE)
V(g)$size <- sqrt(V(g)$strength)

# 5. 网络布局
set.seed(42)
layout <- layout_with_fr(g, weights = E(g)$distance)
str(E(g)$distance)
str(E(g))
# 6. 绘制网络图
nature_colors <- brewer.pal(8, "Set2")

p <- ggraph(g, layout = layout) +
  geom_edge_link(aes(alpha = 1 - distance), 
                 color = "grey70", width = 0.3) +
  geom_node_point(aes(size = size), 
                  color = nature_colors[1], alpha = 0.8) +
  scale_size_continuous(
    name = "Node strength\n(sum of cosine similarity)",
    range = c(1, 10),
    breaks = pretty_breaks(n = 5)
  ) +
  theme_graph(base_family = "Arial") +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(title = "Corporate Network in 2021",
       subtitle = "Node distance based on cosine similarity, size by total similarity")

# 7. 保存图
ggsave("corporate_network_2021.png", plot = p,
       width = 8, height = 6, dpi = 600, bg = "white")
# Results：高拟合企业之间不是随机连接的，而是有明显组织化、自组织特征。 网络随时间不断演化、凝聚，
# 出现了越来越明显中心节点与核心网络结构。这部分高拟合企业确实可以很好地代表整个商业生态系统的骨架与动态演变。


##### Step 7.2 Louvain方法社区检测聚类 ##### 
high_fit_network <- readRDS("/Users/jane/Documents/Global_business_network_Data/Analysis daily results/202404data_process/high_fit_network.rds")
str(high_fit_network)
# 加载必要包
library(igraph)
library(ggraph)
library(ggplot2)
library(dplyr)
library(scales)
library(ggrepel)
library(RColorBrewer)

# 1. 筛选2000年的数据
network_2000 <- high_fit_network %>% 
  filter(year == 2000)

# 2. 创建igraph对象（边的权重是cos_sim）
g_2000 <- graph_from_data_frame(
  d = network_2000[, c("identifier1", "identifier2", "cos_sim")],
  directed = FALSE
)

# 3. 设置布局：用1 - cos_sim作为距离（cos_sim越大，距离越小）
E(g_2000)$distance <- 1 - E(g_2000)$cos_sim + 1e-6  # 保证distance>0

# 4. 网络布局（力导向布局，考虑"distance"）
set.seed(42)
layout_2000 <- layout_with_fr(g_2000, weights = E(g_2000)$distance)

# 5. 社区检测（Louvain方法）
community <- cluster_louvain(g_2000, weights = E(g_2000)$cos_sim)
str(community)
V(g_2000)$community <- membership(community)  # 给节点打标签
str(g_2000)

# 6. 绘制
p <- ggraph(g_2000, layout = layout_2000) +
  geom_edge_link(color = "grey80", alpha = 0.5) +
  geom_node_point(aes(color = factor(community)), size = 2, alpha = 0.9) +
  theme_void() +
  labs(title = "High-Fit Firms Network (2000)",
       subtitle = "Colored by Community (Louvain Clustering)",
       color = "Community") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "right"
  )
# 7. 保存图
ggsave("high_fit_network_2000_louvain_clusters.png", plot = p,
       width = 10, height = 8, dpi = 600, bg = "white")

# 提取节点数据
node_data <- data.frame(
  node = V(g_2000)$name,
  community = V(g_2000)$community
)
# 查看本年社区统计结果
# 1.计算每个节点的强度(如果图是加权的)
if("weight" %in% edge_attr_names(g_2000)) {
  node_data$strength <- strength(g_2000, weights = E(g_2000)$weight)
} else {
  node_data$degree <- degree(g_2000)
}

# 2.计算每个社区的统计特性
community_stats <- node_data %>%
  group_by(community) %>%
  summarise(
    size = n(),
    avg_degree_or_strength = if("strength" %in% names(node_data)) {
      mean(strength)
    } else {
      mean(degree)
    },
    sd_degree_or_strength = if("strength" %in% names(node_data)) {
      sd(strength)
    } else {
      sd(degree)
    },
    .groups = 'drop'
  )
print(community_stats)

# 3. 计算整个网络的模块度
if(!is.null(V(g_2000)$community)) {
  modularity_score <- modularity(g_2000, membership = V(g_2000)$community)
  cat("\nNetwork modularity:", modularity_score, "\n")
}
# Network modularity: 0.7026613 
# 4. 绘制社区大小分布

p <- ggplot(community_stats, aes(x = size)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(title = "Community Size Distribution", x = "Community Size", y = "Count")
ggsave("community_size_distribution.png", plot = p, width = 8, height = 6, dpi = 300)

###### Step 7.3  coarse-graining 超网络建模  ######
str(g_2000)
# 1. 把节点所属社区信息加回到边数据
edge_data <- data.frame(
  from = as.character(ends(g_2000, es = E(g_2000), names = TRUE)[,1]),
  to   = as.character(ends(g_2000, es = E(g_2000), names = TRUE)[,2]),
  cos_sim = E(g_2000)$cos_sim
)
node_info <- data.frame(
  node = V(g_2000)$name,
  community = V(g_2000)$community
)
edge_data <- edge_data %>%
  left_join(node_info, by = c("from" = "node")) %>%
  rename(community_from = community) %>%
  left_join(node_info, by = c("to" = "node")) %>%
  rename(community_to = community)

# 2. 过滤掉社区内部连接，只保留跨社区的边
edge_cross_community <- edge_data %>%
  filter(community_from != community_to)

# 3. 统计社区-社区之间的总连接强度
community_edges <- edge_cross_community %>%
  group_by(community_from, community_to) %>%
  summarise(
    total_cos_sim = sum(cos_sim),
    n_edges = n(),
    avg_cos_sim = mean(cos_sim)
  ) %>%
  ungroup()

# 4. 创建新的社区级别图
g_community <- graph_from_data_frame(community_edges, directed = FALSE)
str(g_community)

# 保证 g_community 已经存在，并且含有 total_cos_sim
# 1. 设置节点属性
V(g_community)$degree <- degree(g_community)  # 每个模块的度
V(g_community)$size <- sqrt(V(g_community)$degree + 1)  # 尺寸放缩
E(g_community)$interaction_type <- ifelse(E(g_community)$avg_cos_sim > 0.8, "Promotion", "Inhibition")
# 为社区之间的边定义类型：
# 	•	avg_cos_sim > 0.8：认为是“促进型”连接（合作强）
# 	•	否则：视为“抑制型”连接（协同弱、甚至可能竞争）

# 2. 绘制
set.seed(42)  # 保证布局可复现
layout <- layout_with_fr(g_community, weights = E(g_community)$total_cos_sim)

# 3. 绘图
coarse_graining_2000 <- ggraph(g_community, layout = layout) +
  geom_edge_link(
    aes(width = total_cos_sim),
    color = "grey70",
    alpha = 0.8
  ) +
  geom_node_point(
    aes(size = size),
    color = "orange",
    fill = "orange",
    shape = 21,
    stroke = 1.2
  ) +
  geom_node_text(
  aes(label = name),
  size = 4,
  fontface = "bold",
  repel = TRUE  # 注意ggraph的geom_node_text()里可以设置repel
  ) +
  scale_size_continuous(range = c(4, 10)) +
  scale_edge_width(range = c(0.5, 3)) +
  theme_graph(base_family = "Arial") +
  labs(
    title = "Module-Level Corporate Interaction Network (2000)",
    subtitle = "Node = Enterprise cluster; Edge width = Inter-cluster cosine similarity"
  ) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  )+
  geom_edge_link(
   aes(width = total_cos_sim, color = interaction_type),
    alpha = 0.8
  ) +
  scale_edge_color_manual(values = c("Promotion" = "red", "Inhibition" = "blue"))

# 保存为PNG格式（默认）
ggsave("coarse_graining(2000).png", plot = coarse_graining_2000, width = 8, height = 6, dpi = 300)

##### Step 7.4 社区持续同调分析（2000） #####
install.packages("TDAstats")
library(TDAstats)

# 1. 提取节点与边信息
# 假设 g_community 是你之前构建的模块-模块网络，边权为 total_cos_sim
str(g_community)
# 获取邻接矩阵，使用边属性 total_cos_sim
adj <- as_adjacency_matrix(g_community, attr = "total_cos_sim", sparse = FALSE)
str(adj)

# Step 1: 将余弦相似度转为距离矩阵
dist_mat <- max(adj) + 1 - adj

# Step 2: 转换为 dist 对象（TDAstats 需要）
dist_obj <- as.dist(dist_mat)
str(dist_obj)

# Step 3: 计算 Rips filtration 的 barcode
ph_result <- calculate_homology(dist_obj, dim = 2, threshold = max(adj), format = "distmat")
str(ph_result)
print(ph_result)

# Step 4: 绘制 barcode 图
# 将矩阵转换为数据框（如果尚未转换）
ph_df <- as.data.frame(ph_result)
colnames(ph_df) <- c("dimension", "birth", "death")

# 添加一列表示特征ID（用于绘图）
ph_df$feature_id <- 1:nrow(ph_df)

library(ggplot2)

# 按维度分组颜色（0维和1维）
barcode_2000 <- ggplot(ph_df, aes(x = birth, xend = death, 
                 y = feature_id, yend = feature_id, 
                 color = as.factor(dimension))) +
  geom_segment(linewidth = 1.5) +  # 绘制水平线段
  labs(
    title = "Persistent Homology Barcode Diagram",
    x = "Filtration Parameter (ε)",
    y = "Topological Feature",
    color = "Dimension"
  ) +
  scale_color_manual(
    values = c("0" = "blue", "1" = "red"),  # 0维蓝色，1维红色
    labels = c("0" = "H0 (Components)", "1" = "H1 (Loops)")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
# 保存为PNG格式（默认）
ggsave("barcode_2000.png", plot = barcode_2000, width = 8, height = 6, dpi = 300)

######### 2000-2021循环代码 #####
# 加载必要包
library(igraph)
library(ggraph)
library(ggplot2)
library(dplyr)
library(scales)
library(ggrepel)
library(RColorBrewer)
library(TDAstats)

# 读取数据
high_fit_network <- readRDS("/Users/jane/Documents/Global_business_network_Data/Data/high_fit_network.rds")

# 创建文件夹保存结果
dir.create("network_results", showWarnings = FALSE)

# 循环处理2000-2021年
for (yr in 2000:2021) {
  cat("Processing year:", yr, "\n")
  
  # Step 1: 筛选每年数据
  network_year <- high_fit_network %>% filter(year == yr)
  
  if (nrow(network_year) < 10) next  # 数据太少，跳过
  
  # Step 2: 创建igraph对象
  g_year <- graph_from_data_frame(
    d = network_year[, c("identifier1", "identifier2", "cos_sim")],
    directed = FALSE
  )
  
  # Step 3: 设置距离属性
  E(g_year)$distance <- 1 - E(g_year)$cos_sim + 1e-6
  
  # Step 4: 网络布局
  set.seed(42)
  layout_year <- layout_with_fr(g_year, weights = E(g_year)$distance)
  
  # Step 5: 社区检测
  community <- cluster_louvain(g_year, weights = E(g_year)$cos_sim)
  V(g_year)$community <- membership(community)
  
  # Step 6: 绘制高拟合网络 + 社区上色
  p_net <- ggraph(g_year, layout = layout_year) +
    geom_edge_link(color = "grey80", alpha = 0.5) +
    geom_node_point(aes(color = factor(community)), size = 2, alpha = 0.9) +
    theme_void() +
    labs(title = paste("High-Fit Firms Network", yr),
         subtitle = "Colored by Community (Louvain Clustering)",
         color = "Community") +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0.5),
          legend.position = "right")
  
  ggsave(filename = paste0("network_results/network_", yr, ".png"), plot = p_net,
         width = 10, height = 8, dpi = 300, bg = "white")
  
  # Step 7: Coarse-Graining 超网络
  edge_data <- data.frame(
    from = as.character(ends(g_year, es = E(g_year), names = TRUE)[,1]),
    to   = as.character(ends(g_year, es = E(g_year), names = TRUE)[,2]),
    cos_sim = E(g_year)$cos_sim
  )
  
  node_info <- data.frame(
    node = V(g_year)$name,
    community = V(g_year)$community
  )
  
  edge_data <- edge_data %>%
    left_join(node_info, by = c("from" = "node")) %>%
    rename(community_from = community) %>%
    left_join(node_info, by = c("to" = "node")) %>%
    rename(community_to = community)
  
  edge_cross_community <- edge_data %>%
    filter(community_from != community_to)
  
  community_edges <- edge_cross_community %>%
    group_by(community_from, community_to) %>%
    summarise(
      total_cos_sim = sum(cos_sim),
      n_edges = n(),
      avg_cos_sim = mean(cos_sim),
      .groups = "drop"
    )
  
  g_community <- graph_from_data_frame(community_edges, directed = FALSE)
  
  if (gorder(g_community) < 2) next  # 如果社区数太少就跳过
  
  V(g_community)$degree <- degree(g_community)
  V(g_community)$size <- sqrt(V(g_community)$degree + 1)
  E(g_community)$interaction_type <- ifelse(E(g_community)$avg_cos_sim > 0.8, "Promotion", "Inhibition")
  
  layout_community <- layout_with_fr(g_community, weights = E(g_community)$total_cos_sim)
  
  p_community <- ggraph(g_community, layout = layout_community) +
    geom_edge_link(aes(width = total_cos_sim, color = interaction_type), alpha = 0.8) +
    geom_node_point(aes(size = size), color = "orange", fill = "orange", shape = 21, stroke = 1.2) +
    geom_node_text(aes(label = name), size = 4, fontface = "bold", repel = TRUE) +
    scale_size_continuous(range = c(4, 10)) +
    scale_edge_width(range = c(0.5, 3)) +
    scale_edge_color_manual(values = c("Promotion" = "red", "Inhibition" = "blue")) +
    theme_graph(base_family = "Arial") +
    labs(
      title = paste("Module-Level Network", yr),
      subtitle = "Node = Cluster; Edge width = Inter-cluster cosine similarity"
    )
  
  ggsave(filename = paste0("network_results/coarse_graining_", yr, ".png"), plot = p_community,
         width = 10, height = 8, dpi = 300, bg = "white")
  
  # Step 8: Persistent Homology 分析
  adj <- as_adjacency_matrix(g_community, attr = "total_cos_sim", sparse = FALSE)
  
  if (nrow(adj) > 2) {  # 至少要有几个模块才能计算同调
    dist_mat <- max(adj) + 1 - adj
    dist_obj <- as.dist(dist_mat)
    ph_result <- calculate_homology(dist_obj, dim = 2, threshold = max(dist_mat), format = "distmat")
    
    ph_df <- as.data.frame(ph_result)
    colnames(ph_df) <- c("dimension", "birth", "death")
    ph_df$feature_id <- 1:nrow(ph_df)
    
    barcode_plot <- ggplot(ph_df, aes(x = birth, xend = death, 
                                      y = feature_id, yend = feature_id, 
                                      color = as.factor(dimension))) +
      geom_segment(linewidth = 1.5) +
      labs(
        title = paste("Persistent Homology Barcode (", yr, ")", sep = ""),
        x = "Filtration Parameter (ε)",
        y = "Topological Feature",
        color = "Dimension"
      ) +
      scale_color_manual(values = c("0" = "blue", "1" = "red")) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
    ggsave(filename = paste0("network_results/barcode_", yr, ".png"), plot = barcode_plot,
           width = 8, height = 6, dpi = 300, bg = "white")
  }
}
# 以上数据非常成功，但是以上的聚类结果是基于距离进行的，和我们论文的设计不符！  

##### Step 8: 引入国家属性解释异质性形成机制 #####
static_info <- read.csv("/Users/jane/Documents/Global_business_network_Data/Data/static_info.csv")
str(static_info)
# 读取全球商业网络数据
global_business_network <- read.csv("/Users/jane/Documents/Global_business_network_Data/Data/global_business_network.csv")
str(global_business_network)

merged_data_filtered <- readRDS("Global_business_network_Data/Business network/20240427/merged_data_filtered.rds")
str(merged_data_filtered)
unique(merged_data_filtered$Firm)

# 先统一格式（都转为字符型，避免匹配失败）
merged_data_filtered$Firm <- as.character(merged_data_filtered$Firm)
static_info$identifier <- as.character(static_info$identifier)

# 合并，匹配字段是：merged_data_filtered$Firm == static_info$identifier
merged_data_filtered_full <- merged_data_filtered %>%
  left_join(static_info[, c("identifier", "region")], by = c("Firm" = "identifier"))
str(merged_data_filtered_full)
unique(merged_data_filtered_full$region)
table(is.na(merged_data_filtered_full$region))    # 看看还有多少没有匹配到
# 已经完全匹配结束

library(ggplot2)
library(dplyr)
library(patchwork)

# 假设你的数据是 merged_data_filtered_full
# 保持 Region 是因子，控制显示顺序
merged_data_filtered_full$region <- factor(
  ifelse(merged_data_filtered_full$region == "usa", "USA", "Non-USA"),
  levels = c("Non-USA", "USA")
)

# 1. 准备p值
pval_df <- merged_data_filtered_full %>%
  group_by(group) %>%
  summarise(
    p_value = ifelse(
      length(unique(region)) > 1,
      t.test(beta ~ region)$p.value,
      NA_real_
    )
  )

# 2. 准备均值
mean_df <- merged_data_filtered_full %>%
  group_by(group, region) %>%
  summarise(mean_beta = mean(beta, na.rm = TRUE),
            n = n(), .groups = "drop")

# 3. 绘图
p <- ggplot(merged_data_filtered_full, aes(x = region, y = beta, fill = region)) +
  geom_violin(trim = FALSE, width = 0.8, alpha = 0.7) +
  geom_boxplot(width = 0.2, outlier.size = 0.5, position = position_dodge(width = 0.8)) +
  geom_point(data = mean_df, aes(x = region, y = mean_beta),
             shape = 23, size = 3, fill = "white", stroke = 1, inherit.aes = FALSE) +
  facet_wrap(~ group, scales = "free", ncol = 2) +
  labs(
    title = "Beta Distribution by Region within Groups",
    x = "Region",
    y = "Beta Value"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"))  # 绿色/橙色

# 4. 添加P值和样本量标注
# 获取每组Y轴最大值，方便放置标签
ymax_df <- merged_data_filtered_full %>%
  group_by(group) %>%
  summarise(ymax = max(beta, na.rm = TRUE) + 0.5)

# 合并信息
annotation_df <- left_join(pval_df, ymax_df, by = "group") %>%
  mutate(label = paste0("p = ", signif(p_value, 3)))

# 加p值
p <- p + geom_text(
  data = annotation_df,
  aes(x = 1.5, y = ymax, label = label),
  inherit.aes = FALSE,
  size = 4, fontface = "italic"
)

# 加N数量标注
mean_df <- mean_df %>%
  mutate(label_n = paste0("N=", n))

p <- p + geom_text(
  data = mean_df,
  aes(x = as.numeric(region) + 0.15, y = mean_beta - 0.3, label = label_n),
  inherit.aes = FALSE,
  size = 3.5
)

# 5. 保存高质量图片
ggsave("beta_distribution_by_region_improved.png", plot = p,
       width = 10, height = 6, dpi = 600, bg = "white")

# 合并国家信息到\beta表做整体的地区间分析
# 确保格式一致
beta_results$Firm <- as.character(beta_results$Firm)
static_info$identifier <- as.character(static_info$identifier)

# 合并 region
beta_results_region <- left_join(beta_results, static_info[, c("identifier", "region")],
                                  by = c("Firm" = "identifier"))
# 创建“美国vs非美国”标签
beta_results_region <- beta_results_region %>%
  mutate(us_flag = ifelse(region == "usa", "USA", "Non-USA"))

# 统计分析（两组是否显著不同）？
t.test(beta ~ us_flag, data = beta_results_region)

#      Welch Two Sample t-test

# data:  beta by us_flag
# t = 22.056, df = 2405.3, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group Non-USA and group USA is not equal to 0
# 95 percent confidence interval:
#  0.4762717 0.5692234
# sample estimates:
# mean in group Non-USA     mean in group USA 
#             0.7434171             0.2206695 
# 美国企业和非美国企业在\beta均值差异非常显著；非美国企业的响应指数\beta显著高于美国企业；
# 这表明非美国企业更“系统响应驱动”，可能更暴露于系统风险或机会。
# 美国企业更稳定、或响应受更多结构限制（比如法规、规模等）。

# 更稳健的非参数检验（无需分布假设）
wilcox.test(beta ~ us_flag, data = beta_results_region)
# Wilcoxon rank sum test with continuity correction

# data:  beta by us_flag
# W = 4497209, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
# 美国 vs 非美国企业的 β 分布在位置（中位数）上存在显著差异。

#小提琴图 + 中位数标记
# 计算中位数用于标注
beta_medians <- beta_results_region %>%
  group_by(us_flag) %>%
  summarise(med_beta = median(beta, na.rm = TRUE))

# 绘图
ggplot(beta_results_region, aes(x = us_flag, y = beta, fill = us_flag)) +
  geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
  geom_boxplot(width = 0.1, outlier.size = 0.5, fill = "white") +
  geom_point(data = beta_medians, aes(x = us_flag, y = med_beta),
             color = "red", size = 3, shape = 18) +  # 中位数红色菱形点
  geom_text(data = beta_medians, aes(x = us_flag, y = med_beta, 
            label = paste0("Median = ", round(med_beta, 2))), 
            vjust = -1, size = 3.5, color = "red") +
  scale_fill_manual(values = c("USA" = "#1f77b4", "Non-USA" = "#2ca02c")) +
  labs(
    title = "Firm-Level β Distribution: USA vs Non-USA",
    x = "",
    y = expression(beta[i])
  ) +
  theme_minimal(base_size = 13)

ggsave("beta_results_region.png", 
       plot = last_plot(),     # 或者你可以指定 plot = your_ggplot_object
       width = 15,             # 单位为英寸
       height = 20,
       dpi = 300,              # 分辨率，300dpi 是出版级别
       bg = "white")

##### Step 9 :Region × TradePhase 双因素分析 #####
# Goal: 展示来自不同 Group（如 T0_intl、T0_usa 等）下具有不同响应特征的企业（高/中/低 β）如何随着系统变化 (HabitatIndex) 调整其业务活动 (Abundance)。
# 拟合模型: log(y) = α + β * log(H)，其中 β 衡量响应强度。
unique(df$Year)
# 构造TradePhase标签（此时的df_full来自于国家单因素分析过程）
df_full$TradePhase <- with(df_full, case_when(
  Year >= 2000 & Year <= 2017 ~ "T0",  # 正常
  Year >= 2018 & Year <= 2021 ~ "T1",  # 贸易战
))
str(df_full)

# 构造组合标签：Region × TradePhase 
df_full$Group <- paste0(df_full$TradePhase, "_", df_full$region)
str(df_full)
saveRDS(df_full, file = "df_full.rds")

df_beta <- df_full %>%
  filter(Abundance > 0, HabitatIndex > 0) %>%
  mutate(
    log_y = log(Abundance),
    log_H = log(HabitatIndex)
  )
str(df_beta)
saveRDS(df_beta, file = "df_beta.rds")
df_full <- readRDS("/Users/jane/Documents/Global_business_network_Data/Data/df_full.rds")
df_beta <- readRDS("/Users/jane/Documents/Global_business_network_Data/Data/df_beta.rds")

beta_results_RT <- df_beta %>%
  group_by(Group, Firm) %>%
  filter(n() >= 3) %>%  # 每个组合至少有3年观测值才能拟合
  do({
    model <- lm(log(Abundance) ~ log(HabitatIndex), data = .)
    data.frame(
      beta = coef(model)[2],
      alpha = exp(coef(model)[1]),
      r2 = summary(model)$r.squared,
      n = nrow(.)
    )
  }) %>%
  ungroup()
table(beta_results_RT$Group)
str(beta_results_RT)
saveRDS(beta_results_RT, file = "beta_results_RT.rds")

# Represent 每组每层随机选两家 
beta_labeled <- beta_results_RT %>%
  group_by(Group) %>%
  mutate(
    beta_rank = percent_rank(beta),  # β 在本组内的分位
    beta_label = case_when(
      beta_rank >= 0.8 ~ "High",
      beta_rank >= 0.4 ~ "Mid",
      TRUE ~ "Low"
    )
  )

set.seed(123)  # 保证可重复
top_sampled_firms <- beta_labeled %>%
  group_by(Group, beta_label) %>%
  slice_sample(n = 2) %>%  # 每层2家，共6家
  ungroup()

df_plot <- df_beta %>%
  filter(Abundance > 0, HabitatIndex > 0) %>%
  mutate(
    log_y = log(Abundance),
    log_H = log(HabitatIndex)
  ) %>%
  semi_join(top_sampled_firms, by = c("Group", "Firm"))

label_data <- top_sampled_firms %>%
  mutate(
    label = paste0("β = ", round(beta, 2), "\nR² = ", round(r2, 2)),
    beta_color = case_when(
      beta > 1 ~ "red",
      beta > 0 & beta <= 1 ~ "green",
      TRUE ~ "blue"
    )
  )
df_plot <- df_plot %>%
  left_join(label_data %>% select(Group, Firm, beta_color), by = c("Group", "Firm"))

# 绘图部分
ggplot(df_plot, aes(x = log_H, y = log_y)) +
  geom_point(alpha = 0.6, size = 1.2, color = "grey40") +
  geom_smooth(aes(color = beta_color), method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ Group + Firm, scales = "free", ncol = 6) +  # 每列一个Group
  geom_text(data = label_data, 
            aes(x = Inf, y = Inf, label = label),
            inherit.aes = FALSE, hjust = 1.1, vjust = 1.1, size = 3.2) +
  scale_color_identity() +  # 使用设定的颜色，不用图例
  labs(
    title = "Firm Response Curves by Group (β Colored)",
    x = "log(Habitat Index)",
    y = "log(Firm Abundance)"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(hjust = 0.5)
  )
ggsave("beta_results_group.png", 
       plot = last_plot(),     # 或者你可以指定 plot = your_ggplot_object
       width = 15,             # 单位为英寸
       height = 20,
       dpi = 300,              # 分辨率，300dpi 是出版级别
       bg = "white")


###### Step 10: 观察Group和beta的影响 #####
# 1.小提琴图
# 如果未添加样本量信息，请添加：
str(beta_results_RT)
str(beta_results_fit)

beta_plot_data <- beta_results_RT %>%
  filter(!is.na(beta), abs(beta) < 5) %>%  # 控制极端值
  group_by(Group) %>%
  mutate(n = n(),
         GroupLabel = paste0(Group, "\n(n=", n, ")")) %>%
  ungroup()

# 计算中位数
medians <- beta_plot_data %>%
  group_by(GroupLabel) %>%
  summarise(med = median(beta))

# 绘图
ggplot(beta_plot_data, aes(x = GroupLabel, y = beta, fill = GroupLabel)) +
  geom_violin(trim = FALSE, alpha = 0.8, color = NA) +
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
  geom_point(data = medians, aes(x = GroupLabel, y = med),
             color = "red", size = 2) +
  geom_text(data = medians, aes(x = GroupLabel, y = med, 
                                label = paste0("Median = ", round(med, 2))),
            color = "red", vjust = -1, size = 3.5) +
  labs(
    title = expression("Distribution of Firm-level "*beta[i]*" across Trade × Region Groups"),
    x = "Trade Phase × Region (with sample size)", y = expression(beta[i])
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(-2.5, 4))  # 根据你数据调节合适范围
ggsave("beta_violin_for_paper.png", width = 10, height = 6, dpi = 300, bg = white)

# 箱线图
beta_box_data <- beta_results_RT %>%
  filter(!is.na(beta), abs(beta) < 5) %>%
  group_by(Group) %>%
  mutate(n = n(),
         GroupLabel = paste0(Group, "\n(n=", n, ")")) %>%
  ungroup()

# 计算中位数
medians <- beta_box_data %>%
  group_by(GroupLabel) %>%
  summarise(med = median(beta))

# 绘制箱线图
ggplot(beta_box_data, aes(x = GroupLabel, y = beta, fill = GroupLabel)) +
  geom_boxplot(width = 0.5, outlier.shape = 1, outlier.size = 0.8) +
  geom_point(data = medians, aes(x = GroupLabel, y = med),
             color = "red", size = 2) +
  geom_text(data = medians, aes(x = GroupLabel, y = med, 
                                label = paste0("Median = ", round(med, 2))),
            color = "red", vjust = -1, size = 3.5) +
  labs(
    title = expression("Firm-level Responsiveness "*beta[i]*" across Trade × Region Groups"),
    x = "Trade Phase × Region (with sample size)", y = expression(beta[i])
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(-2.5, 4))  # 根据你的数据调节
  ggsave("beta_boxplot.png", width = 10, height = 6, dpi = 300)

# 结果解读
# 贸易战前，非美企业对系统变化响应相对积极；
# 同期美国企业响应较低，表现出更稳定行为；
# 贸易战期间，非美企业整体响应变为负向，或许为去全球化、断链反应；
# 美国企业即使在贸易战期也能维持稳定响应模式。
# Conclusion：	
  # 1.	非美国企业更敏感：T0 和 T1 阶段中，非美企业的响应度（β₁）中位数都高于/低于美国企业，波动性更大，说明它们对系统性冲击（如贸易战）更敏感。
	# 2.	美国企业更稳定：T0 和 T1 阶段中，美国企业的 β₁ 中位数都维持在 0.19，呈现出较强的制度惯性或政策缓冲机制。
	# 3.	贸易战带来剧烈调整：非美企业在 T1 期间出现显著负响应（中位数为 -1.05），可能反映出全球供应链调整、区域转移、策略性收缩等现象。
	# 4.	政策与制度嵌入效应显著：政策阶段（T0 vs. T1）与地理属性（USA vs. Intl）对企业行为的复合影响显著。
  
##### Step 11: 分组聚类 Kmeans-聚类 #####
# 基于拟合的beta和R^2对企业进行聚类。
str(beta_results_RT)
# beta相当于在生态系统中描述物种“对环境扰动响应”的指标，非常适合做功能性聚类。
# input：
# beta: 响应强度（主变量）
# r2: 拟合优度（辅助判断beta是否可信）
# out: 每个企业的Cluster标签
library(dplyr)
library(purrr)

# Step 1: 准备数据（清除异常值）
beta_input <- beta_results_RT %>%
  filter(!is.na(beta), is.finite(beta), !is.na(r2), is.finite(r2)) %>%
  select(Group, Firm, beta, r2)
str(beta_input)

# Step 2: 对每个 Group 分别聚类（以 beta 和 r2 为变量）
set.seed(123)
clustered_list <- beta_input %>%
  group_split(Group) %>%
  map_df(function(df) {
    if (nrow(df) >= 10) {  # 只对样本数足够的组进行聚类
      mat <- scale(df[, c("beta", "r2")])
      km <- kmeans(mat, centers = 3)
      df$Cluster <- as.factor(km$cluster)
    } else {
      df$Cluster <- NA  # 样本太少不聚类
    }
    return(df)
  })

# Step 3: 查看结果结构
table(clustered_list$Group, clustered_list$Cluster)

library(ggplot2)

ggplot(clustered_list, aes(x = beta, y = r2, color = Cluster)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Group) +
  labs(title = "Firm Clustering by Group (based on β and R²)")

cluster_summary <- clustered_list %>%
  group_by(Group, Cluster) %>%
  summarise(
    mean_beta = mean(beta),
    mean_r2 = mean(r2),
    n = n(),
    .groups = "drop"
  )
print(cluster_summary)


##### Step 12: GMM-聚类 #####
install.packages("mclust")
library(mclust)
beta_results_RT <- readRDS("/Users/jane/Documents/1.Global_business_network/Data/beta_results_RT.rds")

str(beta_results_RT)

# Step 1：转换为宽格式 β 向量表
# 宽表：每家企业一行，每个 Group（如 T0_intl）一列
beta_wide <- beta_results_RT %>%
  select(Firm, Group, beta) %>%
  pivot_wider(names_from = Group, values_from = beta)
head(beta_wide)

# 获取所有 Group 名称（去掉 Firm 列）
group_names <- colnames(beta_wide)[-1]
str(group_names)
# 初始化列表存放结果
gmm_results_list <- list()
# 遍历每一个 Group，分别进行聚类
for (grp in group_names) {
  cat("Clustering for group:", grp, "\n")
  # 选出这一列不为 NA 的企业
  df_grp <- beta_wide %>%
    select(Firm, all_of(grp)) %>%
    filter(!is.na(.data[[grp]]))
  # 如果企业太少，跳过
  if (nrow(df_grp) < 10) next
  # 准备聚类输入
  mat <- df_grp %>%
    column_to_rownames("Firm") %>%
    as.matrix()
  # 执行 GMM 聚类
  gmm <- Mclust(mat)
  # 保存聚类结果
  gmm_results_list[[grp]] <- list(
    model = gmm,
    classified = tibble(
      Firm = rownames(mat),
      Cluster = gmm$classification
    )
  )
}

# 查看 T1_intl 聚类结果
gmm_results_list[["T1_intl"]]$model
gmm_results_list[["T1_intl"]]$classified

unique(gmm_results_list[["T1_usa"]]$classified$Cluster)


# 修改代码
str(gmm_results_list)
cluster_all <- bind_rows(
  lapply(names(gmm_results_list), function(grp) {
    gmm_results_list[[grp]]$classified %>%
      mutate(
        Group = grp,
        Cluster = paste0(grp, "_", Cluster)  # 👈 全局唯一标签
      )
  })
)
head(cluster_all)
print(gmm$BIC)

# 在T0_intl组内提取并打印出 GMM 模型中每个聚类簇（Cluster）对应的均值（即均值 β），以便了解每一类企业的大致特征。
means <- gmm_results_list[["T0_intl"]]$model$parameters$mean 
print(means)

# 可视化基于beta的聚类结果 
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(purrr)
library(glue)

# Step 1: 整合所有分类结果并提取 Group + Firm + Cluster
# cluster_all <- map_df(names(gmm_results_list), function(grp) {
#   gmm_results_list[[grp]]$classified %>%
#     mutate(Group = grp)
# })

# Step 2: 合并对应 β 值
cluster_beta <- cluster_all %>%
  left_join(beta_wide, by = "Firm") %>%
  pivot_longer(cols = starts_with("T"), names_to = "Group_beta", values_to = "Beta") %>%
  filter(Group == Group_beta)  # 确保匹配 Group 与 Beta 值
str(cluster_beta)

# Step 3: 计算每个 Group-Cluster 的均值与企业数
summary_stats <- cluster_beta %>%
  group_by(Group, Cluster) %>%
  summarise(
    mean_beta = round(mean(Beta, na.rm = TRUE), 2),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(label = glue("μ = {mean_beta}\nn = {n}"))
str(summary_stats)

# Step 4: 提取每个组的聚类模型指标（如 BIC 与聚类数）
bic_summary <- map_df(names(gmm_results_list), function(grp) {
  model_info <- gmm_results_list[[grp]]$model
  tibble(
    Group = grp,
    G = model_info$G,
    BIC = round(model_info$bic, 1)
  )
})
print(bic_summary)
# 合并 summary 到绘图数据
cluster_beta <- cluster_beta %>%
  mutate(
    Cluster = as.factor(Cluster),
    Group = factor(Group, levels = unique(cluster_all$Group))  # 可排序
  )

# Step 5: 绘图：小提琴 + 注释
install.packages("ggsci")
library(ggplot2)
library(dplyr)
library(glue)
library(forcats)
library(ggsci)

# 重新绘制
# 将 Cluster 设置为因子并排序，便于颜色和排列控制
cluster_beta <- cluster_beta %>%
  mutate(Cluster = fct_inorder(Cluster))

max_y <- 10  # 设置合理的 y 上限

# 假设已有 cluster_beta 和 summary_stats 数据框
# 1. 按 beta 均值排序 Cluster，使 X 轴更逻辑清晰
summary_stats <- summary_stats %>%
  mutate(Cluster = as.character(Cluster))  # 确保是字符型

cluster_order <- summary_stats %>%
  arrange(mean_beta) %>%
  pull(Cluster)

cluster_beta <- cluster_beta %>%
  mutate(Cluster = factor(Cluster, levels = cluster_order))

summary_stats <- summary_stats %>%
  mutate(Cluster = factor(Cluster, levels = cluster_order))

# 2. 优化注释内容（μ 和 n 换行，避免长行）
summary_stats <- summary_stats %>%
  mutate(label = glue("μ = {round(mean_beta, 2)}\nn = {n}"))

# 3. 绘图
p <- ggplot(cluster_beta, aes(x = Cluster, y = Beta, fill = Cluster)) +
  geom_violin(trim = TRUE, alpha = 0.7, color = "grey40") +
  geom_boxplot(width = 0.12, fill = "white", outlier.size = 0.3, color = "black") +
  geom_text(
    data = summary_stats,
    aes(x = Cluster, y = max_y * 0.9, label = label),
    inherit.aes = FALSE,
    size = 3.2,
    color = "black"
  ) +
  coord_cartesian(ylim = c(-10, 10)) +
  facet_wrap(~ Group, scales = "free_x", ncol = 2,
             labeller = as_labeller(function(grp) {
               bic_row <- bic_summary %>% filter(Group == grp)
               glue("{grp} | G = {bic_row$G}, BIC = {round(bic_row$BIC)}")
             })) +
  scale_fill_viridis_d(option = "D") +  # 更美观的颜色方案
  labs(
    title = expression("Firm Scaling Coefficients (" * beta * ") by GMM Cluster"),
    x = "Cluster (Sorted by Mean β)",
    y = expression(beta),
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 11),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none",
    panel.spacing = unit(1.2, "lines")
  )
# 4. 导出高分辨率版本
ggsave("gmm_beta_violin_summary.png", width = 14, height = 12, dpi = 600, bg = "white")

# 用合并的方式绘图
library(ggplot2)
library(dplyr)
library(glue)
library(forcats)
library(patchwork)  # 合并子图
library(viridis)
install.packages("viridis")
# 假设已有 cluster_beta, summary_stats, bic_summary 数据框

# 1. 重新排序 Cluster（便于美观展示）
summary_stats <- summary_stats %>%
  mutate(Cluster = as.character(Cluster))

cluster_order <- summary_stats %>%
  arrange(mean_beta) %>%
  pull(Cluster)

cluster_beta <- cluster_beta %>%
  mutate(Cluster = factor(Cluster, levels = cluster_order))

summary_stats <- summary_stats %>%
  mutate(
    Cluster = factor(Cluster, levels = cluster_order),
    label = glue("μ = {round(mean_beta, 2)}\nn = {n}"),
    label_y = max_y + 1.5  # 稍微往上提
  )

# 2. 分组绘图
plot_by_group <- function(group_name) {
  df_cluster <- cluster_beta %>% filter(Group == group_name)
  df_stats <- summary_stats %>% filter(Group == group_name)

  bic_info <- bic_summary %>% filter(Group == group_name)

  ggplot(df_cluster, aes(x = Cluster, y = Beta, fill = Cluster)) +
    geom_violin(trim = TRUE, alpha = 0.7, color = "grey40") +
    geom_boxplot(width = 0.12, fill = "white", outlier.size = 0.3, color = "black") +
    geom_text(
      data = df_stats,
      aes(x = Cluster, y = label_y, label = label),
      inherit.aes = FALSE,
      size = 3.2,
      color = "black"
    ) +
    coord_cartesian(ylim = c(-10, max(df_stats$label_y) + 1.5)) +
    scale_fill_viridis_d(option = "D") +
    labs(
      title = glue("{group_name}  |  G = {bic_info$G},  BIC = {round(bic_info$BIC)}"),
      x = "Cluster", y = expression(beta)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      legend.position = "none",
      plot.margin = margin(5, 5, 5, 5)
    )
}

# 3. 分别画四个图
groups <- unique(cluster_beta$Group)
plots <- lapply(groups, plot_by_group)

# 4. 用 patchwork 合并，并加外框
library(ggplot2)
library(patchwork)

combined <- (plots[[1]] | plots[[2]]) / (plots[[3]] | plots[[4]]) +
  plot_annotation(
    title = "Firm β Distribution Across GMM Clusters by Group",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.margin = margin(10, 10, 10, 10)
    )
  ) & theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))

# 5. 保存图像
ggsave("cluster_beta_violin_combined.png", combined, width = 14, height = 10, dpi = 600)

# Result: 同一组别的企业beta明显分为多个响应模式；正常期T0更集中，贸易战期间T1更极端；
# 国际企业分别更加稳定，美国企业波动性更大；小比例企业拥有远超平均度响应强度，是潜在系统放大器。

# headmap展示
library(dplyr)
library(tibble)
install.packages("pheatmap")
library(pheatmap)
library(ggplot2)
library(tidyr)
library(mclust)

# 指定需要拟合的 4 个组
target_groups <- c("T0_intl", "T0_usa", "T1_intl", "T1_usa")

# Step 0: 如果有缺失的 group，就补做 GMM 拟合
for (grp in target_groups) {
  if (!grp %in% names(gmm_results_list)) {
    message("Fitting GMM for group: ", grp)
    df_grp <- beta_results %>% filter(Group == grp) %>% drop_na(beta)
    if (nrow(df_grp) > 10) {  # 简单判断数据量足够才拟合
      gmm <- Mclust(df_grp$beta)
      classified <- tibble(Firm = df_grp$Firm,
                           Cluster = gmm$classification)
      gmm_results_list[[grp]] <- list(model = gmm, classified = classified)
    } else {
      message("Skipped ", grp, ": not enough data.")
    }
  }
}

# Step 1: 提取聚类标签
extract_cluster_data <- function(result_list) {
  all_data <- lapply(names(result_list), function(group_name) {
    df <- result_list[[group_name]]$classified
    df$Group <- group_name
    colnames(df) <- c("Firm", "Cluster", "Group")
    return(df)
  })
  bind_rows(all_data)
}

cluster_info <- extract_cluster_data(gmm_results_list)

# Step 2: 合并 beta 信息
beta_long <- beta_results_RT %>%
  select(Firm, Group, beta)

beta_clustered <- beta_long %>%
  inner_join(cluster_info, by = c("Firm", "Group"))

# Step 3: 画图
heatmap_list <- split(beta_clustered, beta_clustered$Group)

for (grp in names(heatmap_list)) {
  df <- heatmap_list[[grp]]
  
  mat <- df %>%
    select(Firm, beta) %>%
    column_to_rownames("Firm") %>%
    as.matrix()
  
  mat_scaled <- scale(mat)
  
  ann_row <- df %>%
    select(Firm, Cluster) %>%
    column_to_rownames("Firm")
  
  png(paste0("heatmap_", grp, ".png"), width = 1200, height = 1200, res = 150)
  pheatmap(mat_scaled,
           cluster_rows = TRUE,
           cluster_cols = FALSE,
           show_rownames = FALSE,
           annotation_row = ann_row,
           main = paste("Firm β Distribution -", grp),
           color = colorRampPalette(c("blue", "white", "red"))(100),
           fontsize = 10,
           border_color = NA)
  dev.off()
}

# 图的合并
install.packages("patchwork")
install.packages("ggplotify")   # 将 grid 对象转换为 ggplot
install.packages("ggplot2")     # 必备

library(patchwork)
library(ggplotify)
library(pheatmap)
library(dplyr)
library(tibble)
# 拆分数据
heatmap_list <- split(beta_clustered, beta_clustered$Group)

heatmap_plots <- list()

for (grp in names(heatmap_list)) {
  df <- heatmap_list[[grp]]
  
  # 获取统计信息
  mean_beta <- round(mean(df$beta, na.rm = TRUE), 2)
  G <- gmm_results_list[[grp]]$model$G
  BIC <- round(gmm_results_list[[grp]]$model$bic, 1)
  
  mat <- df %>%
    select(Firm, beta) %>%
    column_to_rownames("Firm") %>%
    as.matrix()
  mat_scaled <- scale(mat)
  
  ann_row <- df %>%
    select(Firm, Cluster) %>%
    column_to_rownames("Firm")
  
  # 使用 grid::grabExpr + as.ggplot() 把 pheatmap 转成 ggplot 对象
  p <- as.ggplot(grid::grid.grabExpr({
    pheatmap(mat_scaled,
             cluster_rows = TRUE,
             cluster_cols = FALSE,
             show_rownames = FALSE,
             annotation_row = ann_row,
             main = paste0(grp, " | G=", G, " | β̄=", mean_beta, " | BIC=", BIC),
             color = colorRampPalette(c("blue", "white", "red"))(100),
             fontsize = 10,
             border_color = NA)
  }))
  
  heatmap_plots[[grp]] <- p
}

# 按顺序拼接成 2x2 布局
combined_plot <- (
  heatmap_plots[["T0_intl"]] + heatmap_plots[["T0_usa"]] +
  heatmap_plots[["T1_intl"]] + heatmap_plots[["T1_usa"]]
) + plot_layout(ncol = 2) +
  plot_annotation(
    title = "Firm β Distribution across GMM Clusters by Group",
    theme = theme(plot.title = element_text(size = 16, hjust = 0.5))
  )

# 保存
ggsave("combined_heatmap_patchwork.png", combined_plot,
       width = 16, height = 12, dpi = 300, bg = "white")

# Result: 合并的热力图从视觉上非常清晰地揭示了不同贸易阶段与区域背景下，企业响应系数beta的聚类结构差异：
# 在贸易战前，非美企业更敏感地随系统变化而扩张
# 非美企业在贸易冲击中呈现整体性收缩特征，但是异质性仍然存在
# T0_usa 聚类数量略多，说明内部结构更加复杂。
# 均值远低于其他组别，表明美企在贸易战期间系统性收缩最严重。

##### Step 13: GMM模块之间的关系 #####
# Module-Module interaction
# 利用企业之间的余弦相似度，把公司所属模块做汇总，构建“模块×模块”的相似性网络。
gmm_results_list <- readRDS("/Users/jane/Documents/1.Global_business_network/Data/gmm_results_list.rds")
str(gmm_results_list)
global_bussiness_network <- read.csv("/Users/jane/Documents/1.Global_business_network/Data/global_business_network.csv")
str(global_bussiness_network)

# 1.提取所有企业的聚类信息
# 提取每个 Group 的 Cluster 信息，并打上标签
library(dplyr)
library(purrr)
# 合并所有组别的分类信息，并生成全局唯一的 ClusterID
cluster_info <- map_dfr(names(gmm_results_list), function(group_name) {
  df <- gmm_results_list[[group_name]]$classified
  df <- df %>%
    mutate(
      Group = group_name,
      Cluster = as.character(Cluster),                         # 保留为字符型
      ClusterID = paste0(group_name, "_", Cluster)             # 唯一模块标签
    )
  return(df)
})

# 查看结果结构
str(cluster_info)
head(cluster_info)


##### Step 14:持续同调分析 #####
install.packages("TDAstats")
library(TDAstats)

str(high_fit_firms)
str(global_business_network)

# 数据预处理，确保 ID 类型统一
high_fit_firms$Firm <- as.integer(high_fit_firms$Firm)
# 从全网中筛选出高拟合企业之间的边
# 转换为 data.table
gnet <- as.data.table(global_business_network)

# 提取高拟合企业ID列表
high_ids <- unique(high_fit_firms$Firm)

# 筛选 identifier1 和 identifier2 都是高拟合企业的边
high_fit_edges <- gnet[
  identifier1 %in% high_ids & identifier2 %in% high_ids
]

# 可选阈值过滤，构建骨架网络。只保留相似度较高的边（如 ≥ 0.85）
high_fit_edges <- high_fit_edges[cos_sim >= 0.85]
# 只取某一年
high_fit_edges_2000 <- high_fit_edges[year == 2000]
# 构建节点表并合并beta参数
nodes <- data.frame(id = unique(c(high_fit_edges_2000$identifier1, high_fit_edges_2000$identifier2)))
nodes <- merge(nodes, high_fit_firms, by.x = "id", by.y = "Firm", all.x = TRUE)

library(igraph)

g <- graph_from_data_frame(
  high_fit_edges_2000[, .(from = identifier1, to = identifier2, weight = cos_sim)],
  directed = TRUE
)
# 写出为 csv，用于 Python flagser 输入
write.csv(high_fit_edges_2000[, .(source = identifier1, target = identifier2, weight = cos_sim)],
          "high_fit_edges_2000.csv", row.names = FALSE)

# 把高拟合企业分类后附加属性
high_fit_firms$response_type <- cut(
  high_fit_firms$beta,
  breaks = c(-Inf, 0.95, 1.05, Inf),
  labels = c("Stabilizer", "Neutral", "Amplifier")
)
str(high_fit_edges_2000)
# 1.构造相似度矩阵
install.packages("reshape2")
library(reshape2)

# 提取边并构造相似度矩阵
edge_mat <- dcast(high_fit_edges_2000, identifier1 ~ identifier2, value.var = "cos_sim", fill = 0)
rownames(edge_mat) <- edge_mat$identifier1
edge_mat$identifier1 <- NULL

# 转为相似度矩阵
sim_mat <- as.matrix(edge_mat)

# 对称化（因为 dcast 只填了一半）
sim_mat <- pmax(sim_mat, t(sim_mat))  # 保证对称
# 转为距离矩阵
dist_mat <- 1 - sim_mat  # 越小越相似，符合 TDA 要求

# 计算同调群（默认 max dimension = 1，推荐设到 2）
ph <- calculate_homology(dist_mat, dim = 2, threshold = 1, format = "distmat")
str(ph)

library(ggplot2)
ph_df <- as.data.frame(ph)

# 只画 H0（0维连通分支）
h0 <- subset(ph_df, dimension == 0)
png("Barcode Plot(H0)_2000.png", width = 2000, height = 1500, res = 300)
ggplot(h0, aes(y = seq_along(birth), x = birth, xend = death)) +
  geom_segment(aes(yend = seq_along(birth)), color = "steelblue") +
  labs(title = "Barcode Plot (H0: Connected components)",
       x = "Filtration Scale", y = "Feature Index") +
  theme_minimal()
dev.off( )

# 只画 H1（1维环）
h1 <- subset(ph_df, dimension == 1)
png("Barcode Plot(H1)_2000.png", width = 2000, height = 1500, res = 300)
ggplot(h1, aes(y = seq_along(birth), x = birth, xend = death)) +
  geom_segment(aes(yend = seq_along(birth)), color = "steelblue") +
  labs(title = "Barcode Plot (H1: Loops)",
       x = "Filtration Scale", y = "Feature Index") +
  theme_minimal()
dev.off( )

# 只画 H2（空腔）
h2 <- subset(ph_df, dimension == 2)
png("Barcode Plot(H2)_2000.png", width = 2000, height = 1500, res = 300)
ggplot(h2, aes(y = seq_along(birth), x = birth, xend = death)) +
  geom_segment(aes(yend = seq_along(birth)), color = "steelblue") +
  labs(title = "Barcode Plot (H2: Cavity)",
       x = "Filtration Scale", y = "Feature Index") +
  theme_minimal()
dev.off( )

png("Persistence Diagram_2000.png", width = 2000, height = 1500, res = 300)
ggplot(ph_df, aes(x = birth, y = death, color = as.factor(dimension))) +
  geom_point(size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Persistence Diagram",
       x = "Birth", y = "Death", color = "Dimension") +
  theme_minimal()
dev.off( )

# 通过对2000年的骨干网络（con_sim>0.85）进行分析发现，大多数连通分支在非常小的时候就合并了，
# 即网络在非常稀疏时连通性就极强。网络具有天然紧密的聚集结构，几乎没有孤立的节点。
# 系统达到一定的相似度密度时，涌现出稳定的三角协同结构和反馈闭环。但是持久性大多中等，说明环路是存在的，但多数是局部的、非结构性强的。
# 有部分高拟合企业之间形成了潜在协同群体，但是缺少“中介闭合”。 这是典型的“结构洞”现象，可用于识别调控枢纽或合作机会。
# Conclusion：H0连通块非常快速地连通，网络结构紧凑，无孤立群体。H1环路结构存在中等持久性，有一定的协同闭环机制，但多数反馈环不稳定。
# H2 空腔结构，极少但是清晰，存在少数高阶合作群体(结构洞附近)

library(data.table)
library(TDAstats)
library(ggplot2)
library(reshape2)

# 封装函数
analyze_homology_by_year <- function(edge_data, year, min_sim = 0.85, max_dim = 2, max_thresh = 1) {
  message("▶ Year: ", year)

  # 1. 筛选年份 + 相似度阈值
  edges_year <- edge_data[year == !!year & cos_sim >= min_sim]

  if (nrow(edges_year) == 0) {
    warning("⚠ No data for year ", year)
    return(NULL)
  }

  # 2. 相似度矩阵
  edge_mat <- dcast(edges_year, identifier1 ~ identifier2, value.var = "cos_sim", fill = 0)
  rownames(edge_mat) <- edge_mat$identifier1
  edge_mat$identifier1 <- NULL

  sim_mat <- as.matrix(edge_mat)
  sim_mat <- pmax(sim_mat, t(sim_mat))  # 保证对称
  dist_mat <- 1 - sim_mat

  # 3. 计算同调
  ph <- calculate_homology(dist_mat, dim = max_dim, threshold = max_thresh, format = "distmat")
  ph_df <- as.data.frame(ph)

  # 4. 按维度画图
  for (d in 0:max_dim) {
    df_d <- subset(ph_df, dimension == d)
    if (nrow(df_d) > 0) {
      png(sprintf("Barcode_Plot(H%d)_%d.png", d, year), width = 2000, height = 1500, res = 300)
      print(ggplot(df_d, aes(y = seq_along(birth), x = birth, xend = death)) +
              geom_segment(aes(yend = seq_along(birth)), color = "steelblue") +
              labs(title = sprintf("Barcode Plot (H%d) - Year %d", d, year),
                   x = "Filtration Scale", y = "Feature Index") +
              theme_minimal())
      dev.off()
    }
  }

  # 5. 持续图
  png(sprintf("Persistence_Diagram_%d.png", year), width = 2000, height = 1500, res = 300)
  print(ggplot(ph_df, aes(x = birth, y = death, color = as.factor(dimension))) +
          geom_point(size = 2, alpha = 0.7) +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
          labs(title = sprintf("Persistence Diagram - Year %d", year),
               x = "Birth", y = "Death", color = "Dimension") +
          theme_minimal())
  dev.off()
  return(ph_df)
}
