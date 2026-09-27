# =============================================================================
# 大学生压力建构、抗压取向与心理—学业适应
# MASTER ANALYSIS SCRIPT
# =============================================================================
# 适配数据：
#   问卷星导出的 103 列 Excel 文件（当前文件为 665 份答卷）
#
# 核心理论模型：
#   1) 风险路径：
#      压力情境强度 -> 压力评价 -> 被压垮体验 -> 情绪耗竭 / 学业退缩
#
#   2) 适应路径：
#      适应型抗压 -> 资源转换 -> 心理恢复 / 学业适应
#
#   3) 硬撑路径：
#      硬撑型抗压 -> 求助抑制 -> 情绪耗竭 / 求助意愿
#
#   4) 增量效度：
#      在控制总体压力、压力评价和适应型抗压后，
#      检验硬撑型抗压是否仍能解释求助抑制和情绪耗竭。
#
#   5) 完整测量质量：
#      alpha、omega、CR、AVE、标准化载荷、HTMT、
#      Fornell-Larcker、潜变量相关、项目—总分相关。
#
#   6) 诊断：
#      VIF/Tolerance 共线性；
#      Harman 单因子 + 理论多因子CFA vs 单因子CFA。
#
# 数据质量策略：
#   主分析：
#      通过注意力题（AC1 == 4）且不是 80 个量表题完全同值作答。
#   敏感性分析 A：
#      只要求通过注意力题。
#   敏感性分析 B（严格）：
#      主分析标准 + 作答时间 >= 90 秒 + 80题作答SD >= .20。
#
# 说明：
#   1. 作答时间和低变异仅作为严格敏感性分析条件，不作为主分析硬删除条件。
#   2. 重复 IP 只标记，不删除，因为校园网/移动网络可能共享出口 IP。
#   3. PS1-PS9 更接近不同压力情境的形成性指标，不强制要求高 alpha。
#   4. HI1-HI6 是不同求助来源，不预设必须形成单一因子。
#   5. 所有横断面“中介”均应在论文中表述为统计间接关联，而非时间因果。
# =============================================================================


# =============================================================================
# 00. 安装并加载包
# =============================================================================

required_pkgs <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "psych",
  "lavaan",
  "semTools",
  "openxlsx",
  "ggplot2",
  "broom",
  "sandwich",
  "lmtest",
  "readr"
)

installed <- rownames(installed.packages())
to_install <- setdiff(required_pkgs, installed)

if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

invisible(lapply(
  required_pkgs,
  function(x) suppressPackageStartupMessages(library(x, character.only = TRUE))
))

options(stringsAsFactors = FALSE)
options(scipen = 999)


# =============================================================================
# 01. 读取数据
# =============================================================================

cat("\n请选择问卷星导出的 Excel 文件...\n")
INPUT_FILE <- file.choose()

OUT_DIR <- file.path(
  dirname(INPUT_FILE),
  "Stress_Resilience_Analysis_Output"
)

if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR, recursive = TRUE)
}

raw <- readxl::read_excel(
  INPUT_FILE,
  sheet = 1,
  col_names = TRUE,
  .name_repair = "minimal"
)

original_names <- names(raw)

cat("\n========================================\n")
cat("Raw data loaded\n")
cat("========================================\n")
cat("File: ", INPUT_FILE, "\n", sep = "")
cat("Rows: ", nrow(raw), "\n", sep = "")
cat("Columns: ", ncol(raw), "\n\n", sep = "")


# =============================================================================
# 02. 按问卷真实顺序重命名变量
# =============================================================================

meta_names <- c(
  "id",
  "submit_time",
  "duration_raw",
  "source",
  "source_detail",
  "ip",
  "platform_total_score",
  "consent_raw"
)

demo_names <- c(
  "gender",
  "grade",
  "major",
  "only_child",
  "hometown",
  "econ_stress",
  "exp_postgrad",
  "exp_civil_service",
  "exp_job",
  "exp_exam_competition",
  "exp_interpersonal",
  "exp_family_expectation",
  "exp_none",
  "stress_global"
)

PS <- paste0("PS", 1:9)
PN <- paste0("PN", 1:12)
CF <- paste0("CF", 1:6)
GR <- paste0("GR", 1:5)
HR <- paste0("HR", 1:6)
RC <- paste0("RC", 1:6)
HS <- paste0("HS", 1:5)
HI <- paste0("HI", 1:6)
EE <- paste0("EE", 1:5)
BR <- paste0("BR", 1:5)
AA <- paste0("AA", 1:5)
AW <- paste0("AW", 1:4)
TN <- paste0("TN", 1:6)

item_names <- c(
  PS, PN, CF, GR, HR, RC, HS,
  HI, EE, BR, AA, AW, TN,
  "AC1"
)

new_names <- c(
  meta_names,
  demo_names,
  item_names
)

if (ncol(raw) != length(new_names)) {
  stop(
    paste0(
      "列数与预期不一致：当前数据有 ",
      ncol(raw),
      " 列，但代码预期 ",
      length(new_names),
      " 列。\n",
      "请先确认是否仍然是原始问卷星导出文件。"
    )
  )
}

header_map <- data.frame(
  column_number = seq_along(original_names),
  original_name = original_names,
  analysis_name = new_names,
  stringsAsFactors = FALSE
)

names(raw) <- new_names
dat0 <- as.data.frame(raw)

numeric_cols <- c(
  demo_names,
  PS, PN, CF, GR, HR, RC, HS,
  HI, EE, BR, AA, AW, TN,
  "AC1"
)

dat0[numeric_cols] <- lapply(
  dat0[numeric_cols],
  function(x) suppressWarnings(as.numeric(x))
)

dat0$duration_sec <- suppressWarnings(
  as.numeric(stringr::str_extract(as.character(dat0$duration_raw), "\\d+"))
)


# =============================================================================
# 03. 数据质量检查
# =============================================================================

core_80 <- c(
  PS, PN, CF, GR, HR, RC, HS,
  HI, EE, BR, AA, AW, TN
)

core_mat <- as.matrix(dat0[, core_80, drop = FALSE])
storage.mode(core_mat) <- "numeric"

safe_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA_real_)
  sd(x)
}

longest_run <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_integer_)
  max(rle(x)$lengths)
}

dat0$response_sd <- apply(core_mat, 1, safe_sd)

dat0$n_unique_core <- apply(
  core_mat,
  1,
  function(x) length(unique(x[!is.na(x)]))
)

dat0$longest_run_core <- apply(
  core_mat,
  1,
  longest_run
)

dat0$attention_pass <- !is.na(dat0$AC1) & dat0$AC1 == 4
dat0$straightline_all <- dat0$n_unique_core == 1
dat0$too_fast_flag <- !is.na(dat0$duration_sec) & dat0$duration_sec < 90
dat0$low_variance_flag <- !is.na(dat0$response_sd) & dat0$response_sd < 0.20

dat0$ip_duplicate_flag <- duplicated(dat0$ip) |
  duplicated(dat0$ip, fromLast = TRUE)

# 主分析标准：注意力题通过 + 不是80题完全同值作答
dat0$keep_primary <- dat0$attention_pass & !dat0$straightline_all

# 敏感性A：只要求注意力题通过
dat0$keep_attention_only <- dat0$attention_pass

# 敏感性B：更严格，但不作为主分析
dat0$keep_strict <- dat0$attention_pass &
  !dat0$straightline_all &
  !dat0$too_fast_flag &
  !dat0$low_variance_flag

quality_summary <- data.frame(
  indicator = c(
    "Raw N",
    "Attention failures",
    "Invariant responders (all 80 items identical)",
    "Attention failure AND invariant",
    "Duration < 90 sec",
    "Response SD < .20",
    "Duplicate IP flag (not excluded)",
    "Primary analysis N",
    "Attention-only sensitivity N",
    "Strict sensitivity N"
  ),
  n = c(
    nrow(dat0),
    sum(!dat0$attention_pass, na.rm = TRUE),
    sum(dat0$straightline_all, na.rm = TRUE),
    sum(!dat0$attention_pass & dat0$straightline_all, na.rm = TRUE),
    sum(dat0$too_fast_flag, na.rm = TRUE),
    sum(dat0$low_variance_flag, na.rm = TRUE),
    sum(dat0$ip_duplicate_flag, na.rm = TRUE),
    sum(dat0$keep_primary, na.rm = TRUE),
    sum(dat0$keep_attention_only, na.rm = TRUE),
    sum(dat0$keep_strict, na.rm = TRUE)
  )
)

quality_summary$percent_raw <- round(
  100 * quality_summary$n / nrow(dat0),
  2
)

print(quality_summary)

quality_person <- dat0[, c(
  "id",
  "duration_sec",
  "AC1",
  "attention_pass",
  "response_sd",
  "n_unique_core",
  "longest_run_core",
  "straightline_all",
  "too_fast_flag",
  "low_variance_flag",
  "ip_duplicate_flag",
  "keep_primary",
  "keep_attention_only",
  "keep_strict"
)]


# =============================================================================
# 04. 计分函数
# =============================================================================

row_mean_min <- function(df, items, min_prop = 0.80) {

  m <- as.matrix(df[, items, drop = FALSE])
  storage.mode(m) <- "numeric"

  n_valid <- rowSums(!is.na(m))
  min_valid <- ceiling(length(items) * min_prop)

  out <- rowMeans(m, na.rm = TRUE)
  out[n_valid < min_valid] <- NA_real_

  return(out)
}

add_scores <- function(df) {

  # 压力情境强度指数：形成性描述指标
  df$PS_index <- row_mean_min(df, PS)

  # 压力评价四个维度
  df$PN_consequence <- row_mean_min(df, PN[1:3])
  df$PN_control_loss <- row_mean_min(df, PN[4:6])
  df$PN_self_threat <- row_mean_min(df, PN[7:9])
  df$PN_vulnerability <- row_mean_min(df, PN[10:12])

  # 仅作为观察变量控制/描述使用
  df$appraisal_mean <- rowMeans(
    cbind(
      df$PN_consequence,
      df$PN_control_loss,
      df$PN_self_threat,
      df$PN_vulnerability
    ),
    na.rm = TRUE
  )

  df$CF_mean <- row_mean_min(df, CF)
  df$GR_mean <- row_mean_min(df, GR)
  df$HR_mean <- row_mean_min(df, HR)
  df$RC_mean <- row_mean_min(df, RC)
  df$HS_mean <- row_mean_min(df, HS)

  # 求助意愿不预设为单一量表
  df$HI_informal <- rowMeans(
    df[, c("HI1", "HI2"), drop = FALSE],
    na.rm = TRUE
  )

  df$HI_institutional <- rowMeans(
    df[, c("HI3", "HI4", "HI6"), drop = FALSE],
    na.rm = TRUE
  )

  df$HI_digital <- df$HI5

  df$EE_mean <- row_mean_min(df, EE)
  df$BR_mean <- row_mean_min(df, BR)
  df$AA_mean <- row_mean_min(df, AA)
  df$AW_mean <- row_mean_min(df, AW)
  df$TN_mean <- row_mean_min(df, TN)

  return(df)
}

dat_primary <- add_scores(
  dat0[dat0$keep_primary, , drop = FALSE]
)

dat_attention <- add_scores(
  dat0[dat0$keep_attention_only, , drop = FALSE]
)

dat_strict <- add_scores(
  dat0[dat0$keep_strict, , drop = FALSE]
)

cat("\n========================================\n")
cat("Analysis samples\n")
cat("========================================\n")
cat("Raw: ", nrow(dat0), "\n", sep = "")
cat("Primary: ", nrow(dat_primary), "\n", sep = "")
cat("Attention-only: ", nrow(dat_attention), "\n", sep = "")
cat("Strict: ", nrow(dat_strict), "\n\n", sep = "")


# =============================================================================
# 05. 人口学变量标签
# =============================================================================

dat_primary$gender_f <- factor(
  dat_primary$gender,
  levels = 1:4,
  labels = c("男", "女", "其他", "不愿说明")
)

dat_primary$grade_f <- factor(
  dat_primary$grade,
  levels = 1:6,
  labels = c("大一", "大二", "大三", "大四", "研究生", "其他")
)

dat_primary$major_f <- factor(
  dat_primary$major,
  levels = 1:7,
  labels = c(
    "文史哲类", "教育类", "经管法类",
    "理工类", "医学类", "艺术体育类", "其他"
  )
)

dat_primary$only_child_f <- factor(
  dat_primary$only_child,
  levels = 1:2,
  labels = c("是", "否")
)

dat_primary$hometown_f <- factor(
  dat_primary$hometown,
  levels = 1:4,
  labels = c("城市", "县城", "乡镇", "农村")
)


# =============================================================================
# 06. 描述统计
# =============================================================================

score_vars <- c(
  "PS_index",
  "PN_consequence",
  "PN_control_loss",
  "PN_self_threat",
  "PN_vulnerability",
  "appraisal_mean",
  "CF_mean",
  "GR_mean",
  "HR_mean",
  "RC_mean",
  "HS_mean",
  "HI_informal",
  "HI_institutional",
  "HI_digital",
  "EE_mean",
  "BR_mean",
  "AA_mean",
  "AW_mean",
  "TN_mean",
  "stress_global",
  "econ_stress"
)

desc_scales <- psych::describe(
  dat_primary[, score_vars, drop = FALSE]
)

desc_scales <- data.frame(
  variable = rownames(desc_scales),
  desc_scales,
  row.names = NULL,
  check.names = FALSE
)

demographic_tables <- list(
  gender = as.data.frame(table(dat_primary$gender_f, useNA = "ifany")),
  grade = as.data.frame(table(dat_primary$grade_f, useNA = "ifany")),
  major = as.data.frame(table(dat_primary$major_f, useNA = "ifany")),
  only_child = as.data.frame(table(dat_primary$only_child_f, useNA = "ifany")),
  hometown = as.data.frame(table(dat_primary$hometown_f, useNA = "ifany"))
)


# =============================================================================
# 07. 各压力源、训练需求、求助来源的排序
# =============================================================================

PS_labels <- c(
  "课程/作业/考试/论文负荷",
  "成绩、绩点或排名",
  "考研考公实习就业竞争",
  "未来方向或职业出路不确定",
  "同伴比较",
  "家庭期待",
  "室友/同学/恋爱/朋友关系",
  "课堂会议或公开表现",
  "学校/班级事务与形式性任务"
)

TN_labels <- c(
  "识别不同类型的压力",
  "把大压力拆解成可处理任务",
  "判断什么时候需要求助",
  "学习求助渠道与求助方法",
  "获得具体压力应对方法",
  "愿意参加抗压能力训练课程"
)

HI_labels <- c(
  "亲密朋友",
  "父母或家人",
  "教师/导师/辅导员",
  "学校心理咨询中心",
  "网络平台/AI/匿名渠道",
  "专业医生或心理咨询师"
)

item_summary <- function(df, items, labels) {

  stopifnot(length(items) == length(labels))

  out <- lapply(
    seq_along(items),
    function(i) {
      x <- df[[items[i]]]
      data.frame(
        item = items[i],
        label = labels[i],
        n = sum(!is.na(x)),
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  )

  out <- dplyr::bind_rows(out)
  out$rank_desc <- rank(-out$mean, ties.method = "min")
  out <- out[order(out$rank_desc, out$item), ]

  return(out)
}

pressure_source_rank <- item_summary(
  dat_primary,
  PS,
  PS_labels
)

training_need_rank <- item_summary(
  dat_primary,
  TN,
  TN_labels
)

help_source_rank <- item_summary(
  dat_primary,
  HI,
  HI_labels
)


# =============================================================================
# 08. 信度
# =============================================================================

reliability_scales <- list(
  PN_consequence = PN[1:3],
  PN_control_loss = PN[4:6],
  PN_self_threat = PN[7:9],
  PN_vulnerability = PN[10:12],
  Crush_experience = CF,
  Adaptive_orientation = GR,
  Endurance_orientation = HR,
  Resource_conversion = RC,
  Help_suppression = HS,
  Emotional_exhaustion = EE,
  Psychological_recovery = BR,
  Academic_adaptation = AA,
  Academic_withdrawal = AW
)

reliability_one <- function(df, items, scale_name) {

  x <- df[, items, drop = FALSE]

  a <- tryCatch(
    suppressWarnings(
      psych::alpha(
        x,
        check.keys = FALSE,
        warnings = FALSE
      )
    ),
    error = function(e) NULL
  )

  alpha_raw <- if (!is.null(a)) a$total$raw_alpha else NA_real_
  alpha_std <- if (!is.null(a)) a$total$std.alpha else NA_real_

  o <- tryCatch(
    suppressMessages(
      suppressWarnings(
        psych::omega(
          x,
          nfactors = 1,
          plot = FALSE
        )
      )
    ),
    error = function(e) NULL
  )

  omega_total <- if (!is.null(o)) o$omega.tot else NA_real_

  data.frame(
    scale = scale_name,
    n_items = length(items),
    alpha_raw = alpha_raw,
    alpha_standardized = alpha_std,
    omega_total = omega_total,
    stringsAsFactors = FALSE
  )
}

reliability_table <- dplyr::bind_rows(
  lapply(
    names(reliability_scales),
    function(nm) {
      reliability_one(
        dat_primary,
        reliability_scales[[nm]],
        nm
      )
    }
  )
)


# =============================================================================
# 09. 量表分数相关矩阵
# =============================================================================

corr_obj <- psych::corr.test(
  dat_primary[, score_vars, drop = FALSE],
  use = "pairwise",
  method = "pearson",
  adjust = "BH"
)

corr_r <- data.frame(
  variable = rownames(corr_obj$r),
  corr_obj$r,
  row.names = NULL,
  check.names = FALSE
)

corr_p <- data.frame(
  variable = rownames(corr_obj$p),
  corr_obj$p,
  row.names = NULL,
  check.names = FALSE
)


# =============================================================================
# 10. 探索性因子分析（分半样本）
# =============================================================================
# 对研究中新建的两个关键结构做探索性检查：
#   A. GR1-GR5 + HR1-HR6：理论上2因子
#   B. PN1-PN12：理论上4因子
#
# 随机一半做EFA，另一半做CFA，减少完全在同一样本中
# “探索后再确认”的循环论证。
# =============================================================================

set.seed(20260924)

efa_index <- sample(
  seq_len(nrow(dat_primary)),
  size = floor(nrow(dat_primary) / 2),
  replace = FALSE
)

dat_efa <- dat_primary[efa_index, , drop = FALSE]
dat_cfa <- dat_primary[-efa_index, , drop = FALSE]

grhr_items <- c(GR, HR)

rho_grhr <- psych::polychoric(
  dat_efa[, grhr_items, drop = FALSE]
)$rho

efa_grhr <- psych::fa(
  rho_grhr,
  nfactors = 2,
  n.obs = nrow(dat_efa),
  fm = "minres",
  rotate = "oblimin"
)

efa_grhr_loadings <- as.data.frame(
  unclass(efa_grhr$loadings)
)

efa_grhr_loadings$item <- rownames(efa_grhr_loadings)
rownames(efa_grhr_loadings) <- NULL
efa_grhr_loadings <- efa_grhr_loadings[, c(
  "item",
  setdiff(names(efa_grhr_loadings), "item")
)]

grhr_kmo <- psych::KMO(rho_grhr)
grhr_bartlett <- psych::cortest.bartlett(
  rho_grhr,
  n = nrow(dat_efa)
)

efa_grhr_diagnostics <- data.frame(
  indicator = c("KMO_MSA", "Bartlett_chisq", "Bartlett_df", "Bartlett_p"),
  value = c(
    grhr_kmo$MSA,
    unname(grhr_bartlett$chisq),
    unname(grhr_bartlett$df),
    unname(grhr_bartlett$p.value)
  )
)

rho_pn <- psych::polychoric(
  dat_efa[, PN, drop = FALSE]
)$rho

efa_pn <- psych::fa(
  rho_pn,
  nfactors = 4,
  n.obs = nrow(dat_efa),
  fm = "minres",
  rotate = "oblimin"
)

efa_pn_loadings <- as.data.frame(
  unclass(efa_pn$loadings)
)

efa_pn_loadings$item <- rownames(efa_pn_loadings)
rownames(efa_pn_loadings) <- NULL
efa_pn_loadings <- efa_pn_loadings[, c(
  "item",
  setdiff(names(efa_pn_loadings), "item")
)]

pn_kmo <- psych::KMO(rho_pn)
pn_bartlett <- psych::cortest.bartlett(
  rho_pn,
  n = nrow(dat_efa)
)

efa_pn_diagnostics <- data.frame(
  indicator = c("KMO_MSA", "Bartlett_chisq", "Bartlett_df", "Bartlett_p"),
  value = c(
    pn_kmo$MSA,
    unname(pn_bartlett$chisq),
    unname(pn_bartlett$df),
    unname(pn_bartlett$p.value)
  )
)


# =============================================================================
# 11. CFA 辅助函数
# =============================================================================

fit_table <- function(fit, model_name) {

  if (is.null(fit)) {
    return(
      data.frame(
        model = model_name,
        converged = FALSE
      )
    )
  }

  fm <- lavaan::fitMeasures(fit)

  wanted <- c(
    "chisq.scaled",
    "df.scaled",
    "pvalue.scaled",
    "cfi.scaled",
    "tli.scaled",
    "rmsea.scaled",
    "rmsea.ci.lower.scaled",
    "rmsea.ci.upper.scaled",
    "cfi.robust",
    "tli.robust",
    "rmsea.robust",
    "srmr"
  )

  vals <- sapply(
    wanted,
    function(k) {
      if (k %in% names(fm)) {
        unname(fm[k])
      } else {
        NA_real_
      }
    }
  )

  out <- as.data.frame(as.list(vals))
  out <- cbind(
    model = model_name,
    converged = lavaan::lavInspect(fit, "converged"),
    out
  )

  return(out)
}

parameter_table <- function(fit) {

  if (is.null(fit)) return(data.frame())

  lavaan::parameterEstimates(
    fit,
    standardized = TRUE,
    ci = TRUE
  )
}

safe_cfa <- function(model, data, ordered_items) {

  tryCatch(
    lavaan::cfa(
      model = model,
      data = data,
      estimator = "WLSMV",
      ordered = ordered_items,
      std.lv = TRUE,
      parameterization = "theta"
    ),
    error = function(e) {
      message("CFA failed: ", conditionMessage(e))
      NULL
    }
  )
}

safe_sem <- function(model, data, ordered_items) {

  tryCatch(
    lavaan::sem(
      model = model,
      data = data,
      estimator = "WLSMV",
      ordered = ordered_items,
      std.lv = TRUE,
      parameterization = "theta"
    ),
    error = function(e) {
      message("SEM failed: ", conditionMessage(e))
      NULL
    }
  )
}


# =============================================================================
# 12. H4a：适应型抗压 vs 硬撑型抗压的一因子/二因子比较
# =============================================================================

model_grhr_one <- paste0(
  "GeneralOrientation =~ ",
  paste(grhr_items, collapse = " + ")
)

model_grhr_two <- paste0(
  "Adaptive =~ ",
  paste(GR, collapse = " + "),
  "\nEndurance =~ ",
  paste(HR, collapse = " + ")
)

fit_grhr_one <- safe_cfa(
  model_grhr_one,
  dat_cfa,
  grhr_items
)

fit_grhr_two <- safe_cfa(
  model_grhr_two,
  dat_cfa,
  grhr_items
)

grhr_cfa_fit <- dplyr::bind_rows(
  fit_table(fit_grhr_one, "GR_HR_one_factor"),
  fit_table(fit_grhr_two, "GR_HR_two_factor")
)

grhr_two_params <- parameter_table(fit_grhr_two)

grhr_latent_cor <- tryCatch(
  {
    x <- lavaan::lavInspect(fit_grhr_two, "cor.lv")
    data.frame(
      variable = rownames(x),
      x,
      row.names = NULL,
      check.names = FALSE
    )
  },
  error = function(e) data.frame()
)


# =============================================================================
# 13. 压力评价 PN：一因子 vs 四因子 vs 二阶模型
# =============================================================================

model_pn_one <- paste0(
  "AppraisalGeneral =~ ",
  paste(PN, collapse = " + ")
)

model_pn_four <- paste(
  "Consequence =~ PN1 + PN2 + PN3",
  "ControlLoss =~ PN4 + PN5 + PN6",
  "SelfThreat =~ PN7 + PN8 + PN9",
  "Vulnerability =~ PN10 + PN11 + PN12",
  sep = "\n"
)

model_pn_second <- paste(
  model_pn_four,
  "AppraisalRisk =~ Consequence + ControlLoss + SelfThreat + Vulnerability",
  sep = "\n"
)

fit_pn_one <- safe_cfa(
  model_pn_one,
  dat_cfa,
  PN
)

fit_pn_four <- safe_cfa(
  model_pn_four,
  dat_cfa,
  PN
)

fit_pn_second <- safe_cfa(
  model_pn_second,
  dat_cfa,
  PN
)

pn_cfa_fit <- dplyr::bind_rows(
  fit_table(fit_pn_one, "PN_one_factor"),
  fit_table(fit_pn_four, "PN_four_correlated"),
  fit_table(fit_pn_second, "PN_second_order")
)

pn_four_params <- parameter_table(fit_pn_four)
pn_second_params <- parameter_table(fit_pn_second)


# =============================================================================
# 14. 完整测量模型（推荐）
# =============================================================================

RUN_FULL_MEASUREMENT <- TRUE

fit_measurement <- NULL

if (RUN_FULL_MEASUREMENT) {

  model_measurement <- paste(
    "Consequence =~ PN1 + PN2 + PN3",
    "ControlLoss =~ PN4 + PN5 + PN6",
    "SelfThreat =~ PN7 + PN8 + PN9",
    "Vulnerability =~ PN10 + PN11 + PN12",
    "Crush =~ CF1 + CF2 + CF3 + CF4 + CF5 + CF6",
    "Adaptive =~ GR1 + GR2 + GR3 + GR4 + GR5",
    "Endurance =~ HR1 + HR2 + HR3 + HR4 + HR5 + HR6",
    "Resource =~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6",
    "HelpSuppress =~ HS1 + HS2 + HS3 + HS4 + HS5",
    "Exhaust =~ EE1 + EE2 + EE3 + EE4 + EE5",
    "Recovery =~ BR1 + BR2 + BR3 + BR4 + BR5",
    "AcadAdapt =~ AA1 + AA2 + AA3 + AA4 + AA5",
    "Withdraw =~ AW1 + AW2 + AW3 + AW4",
    sep = "\n"
  )

  measurement_items <- c(
    PN, CF, GR, HR, RC, HS,
    EE, BR, AA, AW
  )

  fit_measurement <- safe_cfa(
    model_measurement,
    dat_primary,
    measurement_items
  )
}

measurement_fit <- fit_table(
  fit_measurement,
  "Full_measurement_model"
)

measurement_params <- parameter_table(
  fit_measurement
)

# =============================================================================
# 聚合效度：CR 与 AVE
# =============================================================================
# 基于 CFA 的标准化因子载荷 std.all 计算
#
# CR  = (Σλ)^2 / [(Σλ)^2 + Σ(1 - λ^2)]
# AVE = Σλ^2 / [Σλ^2 + Σ(1 - λ^2)]
#
# 对标准化解而言，AVE 等价于标准化载荷平方的平均值。
# =============================================================================

measurement_cr <- data.frame()
measurement_ave <- data.frame()
convergent_validity <- data.frame()

if (!is.null(fit_measurement)) {
  
  # 提取标准化因子载荷
  loading_dat <- lavaan::parameterEstimates(
    fit_measurement,
    standardized = TRUE
  ) |>
    dplyr::filter(op == "=~") |>
    dplyr::select(
      construct = lhs,
      item = rhs,
      std_loading = std.all
    ) |>
    dplyr::filter(
      !is.na(std_loading)
    )
  
  # 按潜变量分别计算 CR 和 AVE
  convergent_validity <- loading_dat |>
    dplyr::group_by(construct) |>
    dplyr::summarise(
      
      n_items = dplyr::n(),
      
      # 标准化载荷平方
      sum_lambda_sq = sum(std_loading^2),
      
      # 标准化残差方差
      sum_error = sum(1 - std_loading^2),
      
      # Composite Reliability
      CR = (sum(std_loading)^2) /
        (
          (sum(std_loading)^2) +
            sum(1 - std_loading^2)
        ),
      
      # Average Variance Extracted
      AVE = sum(std_loading^2) /
        (
          sum(std_loading^2) +
            sum(1 - std_loading^2)
        ),
      
      .groups = "drop"
    )
  
  # 增加判断列
  convergent_validity <- convergent_validity |>
    dplyr::mutate(
      
      CR_ge_70 = CR >= .70,
      
      AVE_ge_50 = AVE >= .50,
      
      validity_flag = dplyr::case_when(
        
        CR >= .70 & AVE >= .50 ~
          "good",
        
        CR >= .70 & AVE < .50 ~
          "CR_good_AVE_review",
        
        TRUE ~
          "review"
      )
    )
  
  # 为了兼容后面已有代码，分别保留两个对象
  measurement_cr <- convergent_validity |>
    dplyr::select(
      construct,
      composite_reliability = CR
    )
  
  measurement_ave <- convergent_validity |>
    dplyr::select(
      construct,
      AVE
    )
}

cat("\n========================================\n")
cat("Convergent validity: CR and AVE\n")
cat("========================================\n")

print(convergent_validity)

# =============================================================================
# 15. 风险路径 SEM
# H1:
# 压力情境强度 -> 压力评价 -> 被压垮体验 -> 情绪耗竭/学业退缩
# =============================================================================

model_risk <- '
  Consequence =~ PN1 + PN2 + PN3
  ControlLoss =~ PN4 + PN5 + PN6
  SelfThreat =~ PN7 + PN8 + PN9
  Vulnerability =~ PN10 + PN11 + PN12

  AppraisalRisk =~ Consequence + ControlLoss + SelfThreat + Vulnerability

  Crush =~ CF1 + CF2 + CF3 + CF4 + CF5 + CF6
  Exhaust =~ EE1 + EE2 + EE3 + EE4 + EE5
  Withdraw =~ AW1 + AW2 + AW3 + AW4

  AppraisalRisk ~ p*PS_index

  Crush ~ a*AppraisalRisk + d*PS_index

  Exhaust ~ b1*Crush + c1*AppraisalRisk + e1*PS_index
  Withdraw ~ b2*Crush + c2*AppraisalRisk + e2*PS_index

  ind_app_EE := a*b1
  ind_app_AW := a*b2

  ind_PS_via_app_crush_EE := p*a*b1
  ind_PS_via_app_crush_AW := p*a*b2
'

fit_risk <- safe_sem(
  model_risk,
  dat_primary,
  ordered_items = c(PN, CF, EE, AW)
)

risk_fit <- fit_table(
  fit_risk,
  "Risk_path_SEM"
)

risk_params <- parameter_table(
  fit_risk
)


# =============================================================================
# 16. 适应路径 SEM
# H2:
# 适应型抗压 -> 资源转换 -> 心理恢复/学业适应
# =============================================================================

model_adaptive <- '
  Adaptive =~ GR1 + GR2 + GR3 + GR4 + GR5
  Resource =~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6
  Recovery =~ BR1 + BR2 + BR3 + BR4 + BR5
  AcadAdapt =~ AA1 + AA2 + AA3 + AA4 + AA5
  Withdraw =~ AW1 + AW2 + AW3 + AW4

  Resource ~ a*Adaptive

  Recovery ~ b1*Resource + c1*Adaptive + s1*PS_index
  AcadAdapt ~ b2*Resource + c2*Adaptive + s2*PS_index
  Withdraw ~ b3*Resource + c3*Adaptive + s3*PS_index

  ind_recovery := a*b1
  ind_acad := a*b2
  ind_withdraw := a*b3
'

fit_adaptive <- safe_sem(
  model_adaptive,
  dat_primary,
  ordered_items = c(GR, RC, BR, AA, AW)
)

adaptive_fit <- fit_table(
  fit_adaptive,
  "Adaptive_path_SEM"
)

adaptive_params <- parameter_table(
  fit_adaptive
)


# =============================================================================
# 17. 硬撑路径 SEM
# H3:
# 硬撑型抗压 -> 求助抑制 -> 情绪耗竭/求助意愿
#
# 同时控制：
#   适应型抗压
#   压力情境强度
#   总体压力
#   压力评价均值
# =============================================================================

model_endurance <- '
  Adaptive =~ GR1 + GR2 + GR3 + GR4 + GR5
  Endurance =~ HR1 + HR2 + HR3 + HR4 + HR5 + HR6
  HelpSuppress =~ HS1 + HS2 + HS3 + HS4 + HS5
  Exhaust =~ EE1 + EE2 + EE3 + EE4 + EE5

  HelpSuppress ~ a*Endurance + g*Adaptive +
                 q1*PS_index + q2*stress_global + q3*appraisal_mean

  Exhaust ~ b*HelpSuppress + c*Endurance + h*Adaptive +
            r1*PS_index + r2*stress_global + r3*appraisal_mean

  HI_institutional ~ bh*HelpSuppress + ch*Endurance + ah*Adaptive +
                     u1*PS_index + u2*stress_global + u3*appraisal_mean

  ind_exhaust := a*b
  ind_help_intention := a*bh
'

fit_endurance <- safe_sem(
  model_endurance,
  dat_primary,
  ordered_items = c(GR, HR, HS, EE)
)

endurance_fit <- fit_table(
  fit_endurance,
  "Endurance_path_SEM"
)

endurance_params <- parameter_table(
  fit_endurance
)


# =============================================================================
# 18. H4b 增量效度：分层回归 + HC3稳健标准误
# =============================================================================

z_vars <- c(
  "PS_index",
  "stress_global",
  "appraisal_mean",
  "GR_mean",
  "HR_mean",
  "HS_mean",
  "EE_mean",
  "RC_mean",
  "BR_mean",
  "AA_mean",
  "AW_mean",
  "HI_institutional"
)

for (v in z_vars) {
  dat_primary[[paste0("z_", v)]] <- as.numeric(
    scale(dat_primary[[v]])
  )
}

robust_tidy <- function(model, model_name) {

  ct <- lmtest::coeftest(
    model,
    vcov. = sandwich::vcovHC(
      model,
      type = "HC3"
    )
  )

  out <- data.frame(
    term = rownames(ct),
    estimate = ct[, 1],
    std_error_HC3 = ct[, 2],
    statistic = ct[, 3],
    p_value = ct[, 4],
    row.names = NULL
  )

  out$model <- model_name

  out[, c(
    "model",
    "term",
    "estimate",
    "std_error_HC3",
    "statistic",
    "p_value"
  )]
}

# Outcome 1: 求助抑制
m_hs_1 <- lm(
  z_HS_mean ~
    z_PS_index +
    z_stress_global +
    z_appraisal_mean +
    z_GR_mean,
  data = dat_primary
)

m_hs_2 <- lm(
  z_HS_mean ~
    z_PS_index +
    z_stress_global +
    z_appraisal_mean +
    z_GR_mean +
    z_HR_mean,
  data = dat_primary
)

# Outcome 2: 情绪耗竭
m_ee_1 <- lm(
  z_EE_mean ~
    z_PS_index +
    z_stress_global +
    z_appraisal_mean +
    z_GR_mean,
  data = dat_primary
)

m_ee_2 <- lm(
  z_EE_mean ~
    z_PS_index +
    z_stress_global +
    z_appraisal_mean +
    z_GR_mean +
    z_HR_mean,
  data = dat_primary
)

incremental_coefficients <- dplyr::bind_rows(
  robust_tidy(m_hs_1, "HS_step1_without_HR"),
  robust_tidy(m_hs_2, "HS_step2_with_HR"),
  robust_tidy(m_ee_1, "EE_step1_without_HR"),
  robust_tidy(m_ee_2, "EE_step2_with_HR")
)

incremental_r2 <- data.frame(
  outcome = c("Help_suppression", "Emotional_exhaustion"),
  R2_step1 = c(
    summary(m_hs_1)$r.squared,
    summary(m_ee_1)$r.squared
  ),
  R2_step2 = c(
    summary(m_hs_2)$r.squared,
    summary(m_ee_2)$r.squared
  )
)

incremental_r2$delta_R2 <- with(
  incremental_r2,
  R2_step2 - R2_step1
)

incremental_r2$F_change_p <- c(
  anova(m_hs_1, m_hs_2)$`Pr(>F)`[2],
  anova(m_ee_1, m_ee_2)$`Pr(>F)`[2]
)


# =============================================================================
# 19. 不同求助来源的补充分析
# =============================================================================

help_source_regressions <- list()

for (h in HI) {

  dat_primary[[paste0("z_", h)]] <- as.numeric(
    scale(dat_primary[[h]])
  )

  f <- as.formula(
    paste0(
      "z_", h,
      " ~ z_HR_mean + z_HS_mean + z_GR_mean + ",
      "z_PS_index + z_stress_global + z_appraisal_mean"
    )
  )

  fit <- lm(
    f,
    data = dat_primary
  )

  help_source_regressions[[h]] <- robust_tidy(
    fit,
    paste0("Outcome_", h)
  )
}

help_source_regression_table <- dplyr::bind_rows(
  help_source_regressions
)


# =============================================================================
# 20. 敏感性分析
# =============================================================================
# 不重复跑全部大型SEM。
# 用核心观察分数路径检查数据清理规则改变后方向是否稳定。
# =============================================================================

run_sensitivity <- function(df, sample_name) {

  needed <- c(
    "PS_index",
    "stress_global",
    "appraisal_mean",
    "GR_mean",
    "HR_mean",
    "RC_mean",
    "HS_mean",
    "EE_mean",
    "BR_mean",
    "AA_mean"
  )

  for (v in needed) {
    df[[paste0("z_", v)]] <- as.numeric(
      scale(df[[v]])
    )
  }

  fits <- list(

    RC = lm(
      z_RC_mean ~ z_GR_mean + z_PS_index + z_stress_global,
      data = df
    ),

    HS = lm(
      z_HS_mean ~
        z_HR_mean +
        z_GR_mean +
        z_appraisal_mean +
        z_PS_index +
        z_stress_global,
      data = df
    ),

    EE = lm(
      z_EE_mean ~
        z_HS_mean +
        z_HR_mean +
        z_GR_mean +
        z_appraisal_mean +
        z_PS_index +
        z_stress_global,
      data = df
    ),

    BR = lm(
      z_BR_mean ~
        z_RC_mean +
        z_GR_mean +
        z_PS_index +
        z_stress_global,
      data = df
    ),

    AA = lm(
      z_AA_mean ~
        z_RC_mean +
        z_GR_mean +
        z_PS_index +
        z_stress_global,
      data = df
    )
  )

  out <- dplyr::bind_rows(
    lapply(
      names(fits),
      function(nm) {
        x <- robust_tidy(
          fits[[nm]],
          paste0(sample_name, "_", nm)
        )
        x$sample <- sample_name
        x$outcome <- nm
        x$n <- nrow(df)
        x
      }
    )
  )

  return(out)
}

sensitivity_table <- dplyr::bind_rows(
  run_sensitivity(
    dat_primary,
    "Primary"
  ),
  run_sensitivity(
    dat_attention,
    "Attention_only"
  ),
  run_sensitivity(
    dat_strict,
    "Strict"
  )
)


# =============================================================================
# 21. 图形
# =============================================================================

p_pressure <- ggplot(
  pressure_source_rank,
  aes(
    x = reorder(label, mean),
    y = mean
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "平均得分（1–5）",
    title = "大学生主要压力情境"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(
    OUT_DIR,
    "01_pressure_sources.png"
  ),
  plot = p_pressure,
  width = 9,
  height = 6,
  dpi = 300
)

p_training <- ggplot(
  training_need_rank,
  aes(
    x = reorder(label, mean),
    y = mean
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "平均得分（1–5）",
    title = "大学生抗压能力训练需求"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(
    OUT_DIR,
    "02_training_needs.png"
  ),
  plot = p_training,
  width = 9,
  height = 6,
  dpi = 300
)

p_help <- ggplot(
  help_source_rank,
  aes(
    x = reorder(label, mean),
    y = mean
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "平均得分（1–5）",
    title = "不同求助来源的使用意愿"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(
    OUT_DIR,
    "03_help_seeking_sources.png"
  ),
  plot = p_help,
  width = 9,
  height = 6,
  dpi = 300
)


# =============================================================================
# 22. 可选：SEM 路径图
# =============================================================================

if (requireNamespace("semPlot", quietly = TRUE)) {

  plot_sem <- function(fit, filename) {

    if (is.null(fit)) return(invisible(NULL))

    png(
      filename = filename,
      width = 2200,
      height = 1500,
      res = 220
    )

    semPlot::semPaths(
      fit,
      what = "std",
      whatLabels = "std",
      layout = "tree2",
      residuals = FALSE,
      intercepts = FALSE,
      edge.label.cex = 0.75,
      sizeLat = 8,
      sizeMan = 4,
      nCharNodes = 0,
      mar = c(6, 6, 6, 6)
    )

    dev.off()
  }

  plot_sem(
    fit_risk,
    file.path(
      OUT_DIR,
      "SEM_Risk_Path.png"
    )
  )

  plot_sem(
    fit_adaptive,
    file.path(
      OUT_DIR,
      "SEM_Adaptive_Path.png"
    )
  )

  plot_sem(
    fit_endurance,
    file.path(
      OUT_DIR,
      "SEM_Endurance_Path.png"
    )
  )
}



# =============================================================================
# 22A. 补充测量质量诊断：标准化载荷、聚合效度、区分效度
# =============================================================================

measurement_loadings <- data.frame()
if (!is.null(fit_measurement)) {
  measurement_loadings <- lavaan::parameterEstimates(
    fit_measurement, standardized = TRUE, ci = TRUE
  ) |>
    dplyr::filter(op == "=~") |>
    dplyr::select(
      construct = lhs, item = rhs, estimate = est, se, pvalue,
      ci.lower, ci.upper, std_loading = std.all
    )
}
if (nrow(measurement_loadings) > 0) {
  measurement_loadings$loading_flag <- dplyr::case_when(
    abs(measurement_loadings$std_loading) >= .70 ~ "strong",
    abs(measurement_loadings$std_loading) >= .50 ~ "acceptable",
    abs(measurement_loadings$std_loading) >= .40 ~ "borderline_review",
    TRUE ~ "weak_review"
  )
}

convergent_validity <- dplyr::full_join(
  measurement_cr, measurement_ave, by = "construct"
)
if (nrow(convergent_validity) > 0) {
  convergent_validity$CR_ge_70 <- convergent_validity$composite_reliability >= .70
  convergent_validity$AVE_ge_50 <- convergent_validity$AVE >= .50
}

latent_cor_matrix <- data.frame()
cor_lv <- NULL
if (!is.null(fit_measurement)) {
  cor_lv <- tryCatch(
    lavaan::lavInspect(fit_measurement, "cor.lv"),
    error = function(e) NULL
  )
  if (!is.null(cor_lv)) {
    latent_cor_matrix <- data.frame(
      construct = rownames(cor_lv), cor_lv,
      row.names = NULL, check.names = FALSE
    )
  }
}

htmt_matrix <- data.frame()
if (!is.null(fit_measurement)) {
  htmt_raw <- tryCatch(
    semTools::htmt(model = model_measurement, data = dat_primary),
    error = function(e) {
      message("HTMT calculation failed: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(htmt_raw)) {
    htmt_matrix <- data.frame(
      construct = rownames(htmt_raw), htmt_raw,
      row.names = NULL, check.names = FALSE
    )
  }
}

fornell_larcker <- data.frame()
if (!is.null(cor_lv) && nrow(measurement_ave) > 0) {
  common_constructs <- intersect(rownames(cor_lv), measurement_ave$construct)
  if (length(common_constructs) > 1) {
    fl <- cor_lv[common_constructs, common_constructs, drop = FALSE]
    ave_lookup <- setNames(measurement_ave$AVE, measurement_ave$construct)
    diag(fl) <- sqrt(ave_lookup[common_constructs])
    fornell_larcker <- data.frame(
      construct = rownames(fl), fl,
      row.names = NULL, check.names = FALSE
    )
  }
}


# =============================================================================
# 22B. 共线性诊断：VIF / Tolerance
# =============================================================================

manual_vif <- function(data, predictors, model_name) {
  x <- data[, predictors, drop = FALSE]
  x <- x[stats::complete.cases(x), , drop = FALSE]
  if (ncol(x) < 2) return(data.frame())

  dplyr::bind_rows(lapply(predictors, function(target) {
    others <- setdiff(predictors, target)
    fit <- lm(reformulate(others, response = target), data = x)
    r2 <- summary(fit)$r.squared
    vif <- 1 / (1 - r2)
    data.frame(
      model = model_name,
      predictor = target,
      R2_from_other_predictors = r2,
      VIF = vif,
      Tolerance = 1 / vif
    )
  }))
}

multicollinearity_table <- dplyr::bind_rows(
  manual_vif(
    dat_primary,
    c("PS_index","stress_global","appraisal_mean","GR_mean","HR_mean"),
    "Predicting_help_suppression"
  ),
  manual_vif(
    dat_primary,
    c("PS_index","stress_global","appraisal_mean","GR_mean","HR_mean","HS_mean"),
    "Predicting_emotional_exhaustion"
  ),
  manual_vif(
    dat_primary,
    c("PS_index","stress_global","GR_mean","RC_mean"),
    "Adaptive_path_observed_predictors"
  )
)

if (nrow(multicollinearity_table) > 0) {
  multicollinearity_table$VIF_flag <- dplyr::case_when(
    multicollinearity_table$VIF < 3 ~ "low",
    multicollinearity_table$VIF < 5 ~ "review",
    TRUE ~ "potential_problem"
  )
}


# =============================================================================
# 22C. 共同方法偏差诊断
# =============================================================================
# Harman 单因子只能作为粗略诊断，不能证明“没有共同方法偏差”。
# 更关键的是比较：理论多因子 CFA vs 全部核心题项单因子 CFA。

cmb_items <- c(PN, CF, GR, HR, RC, HS, EE, BR, AA, AW)

harman_result <- data.frame()
harman_cor <- tryCatch(
  psych::polychoric(dat_primary[, cmb_items, drop = FALSE])$rho,
  error = function(e) NULL
)

if (!is.null(harman_cor)) {
  harman_one <- tryCatch(
    psych::fa(
      harman_cor, nfactors = 1, n.obs = nrow(dat_primary),
      fm = "minres", rotate = "none"
    ),
    error = function(e) NULL
  )
  if (!is.null(harman_one)) {
    proportion_var <- tryCatch(
      as.numeric(harman_one$Vaccounted["Proportion Var", 1]),
      error = function(e) NA_real_
    )
    harman_result <- data.frame(
      indicator = c(
        "First_factor_proportion_variance",
        "Heuristic_50_percent_flag"
      ),
      value = c(
        proportion_var,
        as.numeric(!is.na(proportion_var) && proportion_var >= .50)
      )
    )
  }
}

model_cmb_onefactor <- paste0(
  "CommonFactor =~ ", paste(cmb_items, collapse = " + ")
)

fit_cmb_onefactor <- safe_cfa(
  model_cmb_onefactor, dat_primary, cmb_items
)

cmb_cfa_fit <- dplyr::bind_rows(
  fit_table(fit_cmb_onefactor, "CMB_all_items_one_factor"),
  fit_table(fit_measurement, "Theory_multifactor_measurement")
)


# =============================================================================
# 22D. 项目—总分相关
# =============================================================================

item_total_one <- function(df, items, scale_name) {
  a <- tryCatch(
    psych::alpha(
      df[, items, drop = FALSE],
      check.keys = FALSE, warnings = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(a)) return(data.frame())

  stats <- a$item.stats
  data.frame(
    scale = scale_name,
    item = rownames(stats),
    item_total_r = stats$r.drop,
    item_mean = stats$mean,
    item_sd = stats$sd,
    row.names = NULL
  )
}

item_total_table <- dplyr::bind_rows(
  lapply(names(reliability_scales), function(nm) {
    item_total_one(dat_primary, reliability_scales[[nm]], nm)
  })
)

if (nrow(item_total_table) > 0) {
  item_total_table$item_total_flag <- dplyr::case_when(
    item_total_table$item_total_r >= .40 ~ "good",
    item_total_table$item_total_r >= .30 ~ "acceptable",
    TRUE ~ "review"
  )
}


# =============================================================================
# 22E. 测量质量摘要
# =============================================================================

max_lat_cor <- NA_real_
if (!is.null(cor_lv)) {
  tmp <- abs(cor_lv)
  diag(tmp) <- NA_real_
  max_lat_cor <- max(tmp, na.rm = TRUE)
}

max_htmt <- NA_real_
if (nrow(htmt_matrix) > 0) {
  tmp <- as.matrix(htmt_matrix[, -1, drop = FALSE])
  storage.mode(tmp) <- "numeric"
  diag(tmp) <- NA_real_
  max_htmt <- max(tmp, na.rm = TRUE)
}

measurement_quality_summary <- data.frame(
  indicator = c(
    "Reflective constructs with CR >= .70",
    "Reflective constructs with AVE >= .50",
    "Maximum absolute latent correlation",
    "Maximum HTMT",
    "Maximum VIF",
    "Harman first-factor variance proportion"
  ),
  value = c(
    if (nrow(convergent_validity) > 0) sum(convergent_validity$CR_ge_70, na.rm = TRUE) else NA,
    if (nrow(convergent_validity) > 0) sum(convergent_validity$AVE_ge_50, na.rm = TRUE) else NA,
    max_lat_cor,
    max_htmt,
    if (nrow(multicollinearity_table) > 0) max(multicollinearity_table$VIF, na.rm = TRUE) else NA,
    if (nrow(harman_result) > 0)
      harman_result$value[harman_result$indicator == "First_factor_proportion_variance"][1]
    else NA
  )
)


# =============================================================================
# 23. 保存分析数据
# =============================================================================

# 为保护隐私，不把 IP 写入分析数据文件。
analysis_export <- dat_primary

analysis_export$ip <- NULL
analysis_export$source_detail <- NULL

readr::write_excel_csv(
  analysis_export,
  file.path(
    OUT_DIR,
    "clean_data_primary.csv"
  )
)

readr::write_excel_csv(
  header_map,
  file.path(
    OUT_DIR,
    "variable_header_map.csv"
  )
)

readr::write_excel_csv(
  quality_person,
  file.path(
    OUT_DIR,
    "data_quality_flags.csv"
  )
)


# =============================================================================
# 24. 保存 lavaan 模型对象
# =============================================================================

saveRDS(
  list(
    fit_grhr_one = fit_grhr_one,
    fit_grhr_two = fit_grhr_two,
    fit_pn_one = fit_pn_one,
    fit_pn_four = fit_pn_four,
    fit_pn_second = fit_pn_second,
    fit_measurement = fit_measurement,
    fit_risk = fit_risk,
    fit_adaptive = fit_adaptive,
    fit_endurance = fit_endurance
  ),
  file.path(
    OUT_DIR,
    "lavaan_model_objects.rds"
  )
)


# =============================================================================
# 25. 输出 Excel 结果总表
# =============================================================================

wb_out <- openxlsx::createWorkbook()

add_sheet <- function(
  wb,
  sheet_name,
  object
) {

  if (nchar(sheet_name) > 31) {
    sheet_name <- substr(sheet_name, 1, 31)
  }

  openxlsx::addWorksheet(
    wb,
    sheet_name
  )

  if (is.null(object) || nrow(as.data.frame(object)) == 0) {
    openxlsx::writeData(
      wb,
      sheet_name,
      data.frame(
        note = "No result available / model may not have converged."
      )
    )
  } else {
    openxlsx::writeData(
      wb,
      sheet_name,
      as.data.frame(object)
    )
    openxlsx::freezePane(
      wb,
      sheet_name,
      firstRow = TRUE
    )
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = 1:ncol(as.data.frame(object)),
      widths = "auto"
    )
  }
}

add_sheet(wb_out, "Quality_summary", quality_summary)
add_sheet(wb_out, "Header_map", header_map)
add_sheet(wb_out, "Scale_descriptives", desc_scales)
add_sheet(wb_out, "Reliability", reliability_table)
add_sheet(wb_out, "Correlations_r", corr_r)
add_sheet(wb_out, "Correlations_p", corr_p)

add_sheet(wb_out, "Stress_sources", pressure_source_rank)
add_sheet(wb_out, "Training_needs", training_need_rank)
add_sheet(wb_out, "Help_sources", help_source_rank)

add_sheet(wb_out, "EFA_GRHR_loadings", efa_grhr_loadings)
add_sheet(wb_out, "EFA_GRHR_diag", efa_grhr_diagnostics)
add_sheet(wb_out, "EFA_PN_loadings", efa_pn_loadings)
add_sheet(wb_out, "EFA_PN_diag", efa_pn_diagnostics)

add_sheet(wb_out, "CFA_GRHR_fit", grhr_cfa_fit)
add_sheet(wb_out, "CFA_GRHR_params", grhr_two_params)
add_sheet(wb_out, "CFA_GRHR_latcor", grhr_latent_cor)

add_sheet(wb_out, "CFA_PN_fit", pn_cfa_fit)
add_sheet(wb_out, "CFA_PN4_params", pn_four_params)
add_sheet(wb_out, "CFA_PN2nd_params", pn_second_params)

add_sheet(wb_out, "Measurement_fit", measurement_fit)
add_sheet(wb_out, "Measurement_params", measurement_params)
add_sheet(wb_out, "Measurement_AVE", measurement_ave)
add_sheet(wb_out, "Measurement_CR", measurement_cr)

add_sheet(wb_out, "SEM_Risk_fit", risk_fit)
add_sheet(wb_out, "SEM_Risk_params", risk_params)

add_sheet(wb_out, "SEM_Adaptive_fit", adaptive_fit)
add_sheet(wb_out, "SEM_Adaptive_params", adaptive_params)

add_sheet(wb_out, "SEM_Endurance_fit", endurance_fit)
add_sheet(wb_out, "SEM_Endurance_params", endurance_params)

add_sheet(wb_out, "Incremental_R2", incremental_r2)
add_sheet(wb_out, "Incremental_coef", incremental_coefficients)
add_sheet(wb_out, "Help_source_regs", help_source_regression_table)
add_sheet(wb_out, "Sensitivity", sensitivity_table)

# 人口学分布
for (nm in names(demographic_tables)) {
  add_sheet(
    wb_out,
    paste0("Demo_", nm),
    demographic_tables[[nm]]
  )
}


# 补充：信效度、区分效度、共线性与共同方法偏差
add_sheet(wb_out, "Std_loadings", measurement_loadings)
add_sheet(wb_out, "Convergent_validity", convergent_validity)
add_sheet(wb_out, "Latent_correlations", latent_cor_matrix)
add_sheet(wb_out, "HTMT", htmt_matrix)
add_sheet(wb_out, "Fornell_Larcker", fornell_larcker)
add_sheet(wb_out, "Multicollinearity_VIF", multicollinearity_table)
add_sheet(wb_out, "CMB_Harman", harman_result)
add_sheet(wb_out, "CMB_CFA_compare", cmb_cfa_fit)
add_sheet(wb_out, "Item_total", item_total_table)
add_sheet(wb_out, "Measurement_quality", measurement_quality_summary)

RESULT_XLSX <- file.path(
  OUT_DIR,
  "Stress_Resilience_Analysis_Results.xlsx"
)

openxlsx::saveWorkbook(
  wb_out,
  RESULT_XLSX,
  overwrite = TRUE
)


# =============================================================================
# 26. 输出运行摘要
# =============================================================================

summary_lines <- c(
  "University Student Stress / Resilience Analysis",
  "================================================",
  paste0("Input file: ", INPUT_FILE),
  paste0("Raw N: ", nrow(dat0)),
  paste0("Attention failures: ", sum(!dat0$attention_pass, na.rm = TRUE)),
  paste0(
    "Invariant responders across 80 core items: ",
    sum(dat0$straightline_all, na.rm = TRUE)
  ),
  paste0("Primary N: ", nrow(dat_primary)),
  paste0("Attention-only N: ", nrow(dat_attention)),
  paste0("Strict sensitivity N: ", nrow(dat_strict)),
  "",
  "Primary exclusion rule:",
  "  AC1 must equal 4 AND the 80 substantive Likert items cannot all be identical.",
  "",
  "Duration <90 sec and low response variance are FLAGS only in the main analysis.",
  "They are used as exclusions only in the strict sensitivity analysis.",
  "",
  paste0("Main result workbook: ", RESULT_XLSX),
  paste0("Output folder: ", OUT_DIR)
)

writeLines(
  summary_lines,
  con = file.path(
    OUT_DIR,
    "RUN_SUMMARY.txt"
  ),
  useBytes = TRUE
)
# =============================================================================
# SEM PATH FIGURES — STABLE / CLEAR VERSION
# =============================================================================
# Main changes:
# 1) REMOVE edgeLabels = TRUE (this causes the current semPlot/qgraph error).
# 2) Keep whatLabels = "name" after writing the desired standardized coefficients
#    into the semPlotModel label column.
# 3) By default, hide numerical labels for item loadings but KEEP the item arrows.
#    Structural regression coefficients and second-order latent loadings remain shown.
# 4) Use a wider left-to-right tree layout, larger ellipses, and greater level spacing.
# 5) English labels only; no figure title.
#
# Required objects already fitted in your main script:
#   fit_risk
#   fit_adaptive
#   fit_endurance
#   OUT_DIR
# =============================================================================

if (requireNamespace("semPlot", quietly = TRUE)) {
  
  # ===========================================================================
  # User switches
  # ===========================================================================
  
  # FALSE is recommended for the manuscript:
  # item loading arrows remain, but their numerical values are hidden.
  SHOW_ITEM_LOADING_VALUES <- FALSE
  
  # 600 dpi for publication-quality PNG files.
  FIGURE_DPI <- 600
  
  
  # ===========================================================================
  # 1. Short English node labels
  # ===========================================================================
  
  pretty_sem_label <- function(x) {
    
    label_dict <- c(
      "AppraisalRisk"     = "Appraisal\nRisk",
      "ControlLoss"       = "Control\nLoss",
      "SelfThreat"        = "Self\nThreat",
      "HelpSuppress"      = "Help\nSuppression",
      "stress_global"     = "Overall\nStress",
      "PS_index"          = "Pressure\nSituation",
      "appraisal_mean"    = "Mean\nAppraisal",
      "HI_institutional"  = "Institutional\nHelp Intention",
      "AcadAdapt"         = "Academic\nAdaptation",
      "Adaptive"          = "Adaptive\nResilience",
      "Endurance"         = "Endurance\nResilience",
      "Resource"          = "Resource\nReconfiguration",
      "Recovery"          = "Recovery",
      "Consequence"       = "Consequence\nMagnification",
      "Vulnerability"     = "Vulnerability\nExpression",
      "Crush"             = "Crushed\nFeeling",
      "Exhaust"           = "Emotional\nExhaustion",
      "Withdraw"          = "Academic\nWithdrawal"
    )
    
    out <- x
    hit <- out %in% names(label_dict)
    
    out[hit] <- unname(label_dict[out[hit]])
    
    # Unlisted long labels: split underscores only.
    out[!hit] <- gsub("_", "\n", out[!hit])
    
    out
  }
  
  
  # ===========================================================================
  # 2. Prepare a plotting copy of the semPlotModel
  # ===========================================================================
  
  prepare_sem_plot_model <- function(fit, show_item_loadings = FALSE) {
    
    spm <- semPlot::semPlotModel(fit)
    
    # English display labels in exact semPlot node order
    node_labels <- pretty_sem_label(spm@Vars$name)
    
    if (!show_item_loadings) {
      
      # semPlot edge types:
      # "->" = factor loading
      # "~>" = regression
      #
      # We keep numerical labels for:
      #   A. all structural regression paths (~>)
      #   B. factor loadings whose RHS is itself latent
      #      (e.g., second-order AppraisalRisk -> first-order appraisal factors)
      #
      # We suppress only numerical item-loading labels.
      # The loading arrows themselves remain in the graph.
      
      latent_lookup <- setNames(
        !spm@Vars$manifest,
        spm@Vars$name
      )
      
      rhs_is_latent <- rep(FALSE, nrow(spm@Pars))
      
      ok_rhs <- spm@Pars$rhs %in% names(latent_lookup)
      
      rhs_is_latent[ok_rhs] <- latent_lookup[
        spm@Pars$rhs[ok_rhs]
      ]
      
      keep_numeric_label <-
        spm@Pars$edge == "~>" |
        (
          spm@Pars$edge == "->" &
            rhs_is_latent
        )
      
      new_edge_labels <- rep(
        "",
        nrow(spm@Pars)
      )
      
      good_std <-
        keep_numeric_label &
        !is.na(spm@Pars$std) &
        is.finite(spm@Pars$std)
      
      new_edge_labels[good_std] <- sprintf(
        "%.2f",
        spm@Pars$std[good_std]
      )
      
      # whatLabels = "name" will read this column.
      spm@Pars$label <- new_edge_labels
    }
    
    list(
      model = spm,
      node_labels = node_labels
    )
  }
  
  
  # ===========================================================================
  # 3. Main plotting function
  # ===========================================================================
  
  plot_sem_clean <- function(
    fit,
    filename,
    model_type = c(
      "risk",
      "adaptive",
      "endurance"
    )
  ) {
    
    model_type <- match.arg(model_type)
    
    if (is.null(fit)) {
      message(
        "Skipping ",
        model_type,
        ": fit object is NULL."
      )
      return(invisible(NULL))
    }
    
    prepared <- prepare_sem_plot_model(
      fit,
      show_item_loadings =
        SHOW_ITEM_LOADING_VALUES
    )
    
    spm <- prepared$model
    node_labels <- prepared$node_labels
    
    
    # -------------------------------------------------------------------------
    # Model-specific canvas and spacing
    # -------------------------------------------------------------------------
    
    cfg <- switch(
      
      model_type,
      
      risk = list(
        width = 7600,
        height = 5000,
        sizeLat = 14,
        sizeLat2 = 8.5,
        sizeMan = 4.5,
        sizeMan2 = 2.5,
        label_cex = 0.78,
        edge_cex = 0.60,
        levels = c(1, 4.5, 8.5, 13),
        curve = 1.25,
        curvature = 1.35,
        mar = c(10, 12, 10, 12)
      ),
      
      adaptive = list(
        width = 7000,
        height = 4400,
        sizeLat = 14,
        sizeLat2 = 8.5,
        sizeMan = 4.7,
        sizeMan2 = 2.5,
        label_cex = 0.80,
        edge_cex = 0.64,
        levels = c(1, 4.5, 8.5, 13),
        curve = 1.15,
        curvature = 1.25,
        mar = c(10, 12, 10, 12)
      ),
      
      endurance = list(
        width = 7400,
        height = 4700,
        sizeLat = 14,
        sizeLat2 = 8.5,
        sizeMan = 5.0,
        sizeMan2 = 2.6,
        label_cex = 0.76,
        edge_cex = 0.58,
        levels = c(1, 5, 9, 14),
        curve = 1.45,
        curvature = 1.45,
        mar = c(11, 13, 11, 13)
      )
    )
    
    
    # -------------------------------------------------------------------------
    # Open high-resolution device
    # -------------------------------------------------------------------------
    
    png(
      filename = filename,
      width = cfg$width,
      height = cfg$height,
      res = FIGURE_DPI,
      bg = "white"
    )
    
    
    # -------------------------------------------------------------------------
    # Draw model
    # -------------------------------------------------------------------------
    #
    # IMPORTANT:
    # There is intentionally NO edgeLabels = TRUE here.
    #
    # whatLabels already controls the edge labels:
    #   - "std"  if item loading values should be shown
    #   - "name" if we use the custom label column made above
    # -------------------------------------------------------------------------
    
    semPlot::semPaths(
      
      object = spm,
      
      what = "std",
      
      whatLabels = if (
        SHOW_ITEM_LOADING_VALUES
      ) {
        "std"
      } else {
        "name"
      },
      
      
      # -----------------------------------------------------------------------
      # Layout: left -> right, with more space between levels
      # -----------------------------------------------------------------------
      
      layout = "tree",
      
      rotation = 2,
      
      reorder = TRUE,
      
      centerLevels = TRUE,
      
      levels = cfg$levels,
      
      springLevels = FALSE,
      
      
      # -----------------------------------------------------------------------
      # Remove unnecessary clutter
      # -----------------------------------------------------------------------
      
      residuals = FALSE,
      
      intercepts = FALSE,
      
      thresholds = FALSE,
      
      exoCov = FALSE,
      
      exoVar = FALSE,
      
      
      # -----------------------------------------------------------------------
      # Node labels
      # -----------------------------------------------------------------------
      
      nodeLabels = node_labels,
      
      nCharNodes = 0,
      
      nCharEdges = 0,
      
      
      # -----------------------------------------------------------------------
      # Make latent ellipses wide enough for text
      # -----------------------------------------------------------------------
      
      shapeLat = "ellipse",
      
      sizeLat = cfg$sizeLat,
      
      sizeLat2 = cfg$sizeLat2,
      
      
      # -----------------------------------------------------------------------
      # Manifest nodes: wider rectangles, low height
      # -----------------------------------------------------------------------
      
      shapeMan = "rectangle",
      
      sizeMan = cfg$sizeMan,
      
      sizeMan2 = cfg$sizeMan2,
      
      equalizeManifests = TRUE,
      
      
      # -----------------------------------------------------------------------
      # Text inside nodes
      # -----------------------------------------------------------------------
      
      label.cex = cfg$label_cex,
      
      label.scale = FALSE,
      
      label.prop = 0.82,
      
      
      # -----------------------------------------------------------------------
      # Path coefficients
      # -----------------------------------------------------------------------
      
      edge.label.cex = cfg$edge_cex,
      
      nDigits = 2,
      
      
      # -----------------------------------------------------------------------
      # Arrows / edges
      # -----------------------------------------------------------------------
      
      edge.color = "black",
      
      asize = 2.0,
      
      esize = 0.75,
      
      curve = cfg$curve,
      
      curvature = cfg$curvature,
      
      curvePivot = TRUE,
      
      # Connect arrows to node borders rather than node centres.
      cardinal = "all",
      
      # Curve adjacent regressions when needed.
      curveAdjacent = c(
        "<->",
        "reg"
      ),
      
      optimizeLatRes = TRUE,
      
      
      # -----------------------------------------------------------------------
      # Black-and-white journal style
      # -----------------------------------------------------------------------
      
      color = list(
        lat = "white",
        man = "white"
      ),
      
      border.color = "black",
      
      fade = FALSE,
      
      inheritColor = FALSE,
      
      
      # -----------------------------------------------------------------------
      # No title inside the figure
      # -----------------------------------------------------------------------
      
      title = FALSE,
      
      
      # -----------------------------------------------------------------------
      # Extra margin
      # -----------------------------------------------------------------------
      
      mar = cfg$mar
    )
    
    
    dev.off()
    
    message(
      "Saved: ",
      filename
    )
    
    invisible(NULL)
  }
  
  
  # ===========================================================================
  # 4. Generate the three figures
  # ===========================================================================
  
  plot_sem_clean(
    fit = fit_risk,
    filename = file.path(
      OUT_DIR,
      "SEM_Risk_Path_clean_v2.png"
    ),
    model_type = "risk"
  )
  
  plot_sem_clean(
    fit = fit_adaptive,
    filename = file.path(
      OUT_DIR,
      "SEM_Adaptive_Path_clean_v2.png"
    ),
    model_type = "adaptive"
  )
  
  plot_sem_clean(
    fit = fit_endurance,
    filename = file.path(
      OUT_DIR,
      "SEM_Endurance_Path_clean_v2.png"
    ),
    model_type = "endurance"
  )
  
  cat(
    "\n========================================\n",
    "SEM figures generated successfully\n",
    "========================================\n",
    "Output folder: ",
    OUT_DIR,
    "\n\n",
    sep = ""
  )
}

cat("\n========================================\n")
cat("PIPELINE COMPLETED\n")
cat("========================================\n")
cat("Primary N: ", nrow(dat_primary), "\n", sep = "")
cat("Results workbook:\n", RESULT_XLSX, "\n\n", sep = "")
cat("Output folder:\n", OUT_DIR, "\n\n", sep = "")

