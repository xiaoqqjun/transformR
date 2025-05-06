# transformR

## 概述

transformR是一个R包，用于寻找数据变量的最佳转换方法，以使其更接近正态分布。当您需要对偏斜或异常分布的数据进行转换以满足统计分析中正态性假设时，该包特别有用。

transformR实现了多种常见的变量转换方法，自动评估每种方法的效果，并推荐最佳选择。它简化了变量转换的工作流程，使您可以专注于数据分析而不是变换技术。

## 安装

### 从GitHub安装

```r
# 安装devtools（如果尚未安装）
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# 从GitHub安装transformR
devtools::install_github("您的GitHub用户名/transformR")
```

## 功能特点

* 自动评估并比较多种转换方法
* 为变量推荐最优的转换方法
* 提供详细的评估指标（偏度、峰度、综合正态性得分）
* 可视化不同转换效果的比较
* 支持批量处理多个变量

## 支持的转换方法

transformR包含以下转换方法：

1. **平方根转换** (transform_sqrt)：适合计数数据和右偏分布
2. **Box-Cox转换** (transform_boxcox)：一系列幂转换，包括对数和幂转换
3. **Yeo-Johnson转换** (transform_yeojohnson)：Box-Cox的扩展，可处理负值
4. **倒数转换** (transform_inverse)：对强右偏数据特别有效
5. **基于排序的归一化** (transform_rank)：使用正态分位数函数的非参数方法
6. **双曲反正弦转换** (transform_asinh)：可同时处理不同尺度的正负值

## 基本用法

```r
library(transformR)

# 生成示例右偏数据
set.seed(123)
skewed_data <- rexp(100, 0.2)

# 检查原始数据的正态性
check_normality(skewed_data)

# 找出最佳转换并生成对比图
result <- best_transform(skewed_data, all_transforms = TRUE, plot = TRUE)

# 查看评估指标
print(result$evaluation)

# 应用最佳转换
transformed_data <- best_transform(skewed_data)
```

## 高级用法示例

### 转换多个变量

```r
# 创建包含多个变量的数据框
data <- data.frame(
  var1 = rexp(100, 0.5),       # 右偏
  var2 = -rexp(100, 0.5) + 10, # 左偏
  var3 = rt(100, df = 3)       # 重尾
)

# 处理每个变量
vars <- c("var1", "var2", "var3")
transformed_data <- data

for (var in vars) {
  cat("处理变量:", var, "\n")
  transformed_data[[paste0(var, "_transformed")]] <- best_transform(data[[var]])
}
```

### 在统计模型中使用

```r
# 创建回归数据
x <- runif(100, 0, 10)
y <- 2 + 3*x + rexp(100, 0.5)  # 非正态误差

# 拟合原始模型
original_model <- lm(y ~ x)

# 转换因变量
y_trans <- best_transform(y)

# 拟合转换后的模型
trans_model <- lm(y_trans ~ x)

# 比较结果
summary(original_model)
summary(trans_model)
```

## 引用

如果您在研究中使用了transformR，请按以下格式引用：

```
您的姓名 (2025). transformR: 查找最优数据转换. R package version 0.1.0.
https://github.com/您的用户名/transformR
```

## 贡献

欢迎提交问题报告和拉取请求！如需贡献，请遵循以下步骤：

1. Fork仓库
2. 创建您的功能分支 (`git checkout -b feature/amazing-feature`)
3. 提交您的更改 (`git commit -m 'Add some amazing feature'`)
4. 推送到分支 (`git push origin feature/amazing-feature`)
5. 打开拉取请求

## 许可证

本项目采用MIT许可证 - 详见 [LICENSE](LICENSE) 文件
