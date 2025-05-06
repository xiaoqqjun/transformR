library(testthat)
library(transformR)

test_that("transform_sqrt works", {
  # 测试正值
  x <- c(1, 4, 9, 16)
  expect_equal(transform_sqrt(x), c(1, 2, 3, 4))
  
  # 测试零值
  x <- c(0, 1, 4)
  expect_equal(transform_sqrt(x), c(sqrt(0.01), 1, 2))
  
  # 测试负值
  x <- c(-4, 0, 4)
  shift <- abs(min(x, na.rm = TRUE)) + 0.01
  expect_equal(transform_sqrt(x), sqrt(x + shift))
})

test_that("transform_boxcox works", {
  # 测试正值
  x <- c(1, 2, 3, 4, 5)
  result <- transform_boxcox(x)
  expect_true(is.list(result))
  expect_true("data" %in% names(result))
  expect_true("lambda" %in% names(result))
  
  # 验证特定lambda值
  lambda <- 0.5  # 平方根
  manual_result <- (x^lambda - 1)/lambda
  expect_equal(length(result$data), length(manual_result))
})

test_that("transform_rank works", {
  # 测试一般排序
  x <- c(5, 3, 1, 4, 2)
  result <- transform_rank(x)
  expect_equal(length(result), length(x))
  
  # 测试有相同值的情况
  x <- c(5, 3, 3, 4, 2)
  result <- transform_rank(x)
  expect_equal(length(result), length(x))
  
  # 测试顺序保持
  x <- c(5, 3, 1, 4, 2)
  result <- transform_rank(x)
  expect_true(all(order(x) == order(result, decreasing = TRUE)))
})

test_that("transform_asinh works", {
  # 测试正值
  x <- c(1, 2, 3, 4)
  expect_equal(transform_asinh(x), asinh(x))
  
  # 测试负值
  x <- c(-2, -1, 0, 1, 2)
  expect_equal(transform_asinh(x), asinh(x))
})

test_that("evaluate_transformations works", {
  # 生成测试数据
  set.seed(123)
  x <- rexp(50, 0.5)
  
  # 创建转换后的数据集
  transformed <- list(
    original = x,
    sqrt = transform_sqrt(x),
    boxcox = transform_boxcox(x),
    rank = transform_rank(x)
  )
  
  # 测试评估函数
  result <- evaluate_transformations(transformed)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), length(transformed))
  expect_true("Skewness" %in% names(result))
  expect_true("ExcessKurtosis" %in% names(result))
  expect_true("NormalityScore" %in% names(result))
})

test_that("best_transform works", {
  # 生成测试数据
  set.seed(123)
  x <- rexp(50, 0.5)
  
  # 测试基本功能
  result <- best_transform(x, all_transforms = FALSE)
  expect_true(is.numeric(result))
  expect_equal(length(result), length(x))
  
  # 测试完整输出
  full_result <- best_transform(x, all_transforms = TRUE)
  expect_true(is.list(full_result))
  expect_true("evaluation" %in% names(full_result))
  expect_true("transformed" %in% names(full_result))
  expect_true("best" %in% names(full_result))
  
  # 测试方法选择
  skew_result <- best_transform(x, method = "skewness", all_transforms = TRUE)
  kurt_result <- best_transform(x, method = "kurtosis", all_transforms = TRUE)
  auto_result <- best_transform(x, method = "auto", all_transforms = TRUE)
  
  expect_true(is.character(skew_result$best))
  expect_true(is.character(kurt_result$best))
  expect_true(is.character(auto_result$best))
})