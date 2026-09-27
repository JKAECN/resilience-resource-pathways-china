# =============================================================================
# SEM FIGURES — FINAL MERGED SCRIPT V5.1
# =============================================================================
# This script merges:
#   (1) the V5 main plotting script
#   (2) the item-label centering patch
#
# Run this file ONCE from top to bottom.
#
# It will:
#   - load fit_adaptive / fit_endurance / fit_risk if needed
#   - create the three manual layouts
#   - draw the three SEM figures
#   - center GR/HR/RC/BR/AA/AW/HS/EE/PN/CF item labels
#   - resize only item boxes
#   - keep long observed-variable boxes larger
#   - save both PNG and PDF
#
# Output folder:
#   E:/论文/学生压力/SEM_Figures_FINAL_v5_1
# =============================================================================


# =============================================================================
# 0. Packages
# =============================================================================

need_pkgs <- c("lavaan", "semPlot", "semptools")

for (p in need_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(lavaan)
library(semPlot)
library(semptools)


# =============================================================================
# 1. Output folder
# =============================================================================

OUT_DIR <- "E:/论文/学生压力/SEM_Figures_FINAL_v5_1"

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

cat(
  "\nOutput folder:\n",
  normalizePath(
    OUT_DIR,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n\n",
  sep = ""
)


# =============================================================================
# 2. Load saved lavaan models if needed
# =============================================================================

load_saved_models <- function() {

  already_loaded <- all(
    c(
      "fit_adaptive",
      "fit_endurance",
      "fit_risk"
    ) %in% ls(.GlobalEnv)
  )

  if (already_loaded) {

    cat(
      "fit_adaptive, fit_endurance, and fit_risk ",
      "already exist in memory.\n\n",
      sep = ""
    )

    return(invisible(TRUE))
  }

  cat(
    "Please select either:\n",
    "  1) lavaan_model_objects.rds\n",
    "  OR\n",
    "  2) Stress_Resilience_Analysis_Output.zip\n\n",
    sep = ""
  )

  selected_file <- file.choose()

  ext <- tolower(
    tools::file_ext(selected_file)
  )

  if (ext == "rds") {

    obj <- readRDS(
      selected_file
    )

  } else if (ext == "zip") {

    zip_listing <- unzip(
      selected_file,
      list = TRUE
    )

    rds_inside <- zip_listing$Name[
      grepl(
        "lavaan_model_objects\\.rds$",
        zip_listing$Name
      )
    ]

    if (length(rds_inside) != 1) {
      stop(
        "Could not uniquely locate ",
        "lavaan_model_objects.rds inside the ZIP file."
      )
    }

    temp_dir <- tempfile(
      "lavaan_models_"
    )

    dir.create(
      temp_dir
    )

    unzip(
      selected_file,
      files = rds_inside,
      exdir = temp_dir
    )

    obj <- readRDS(
      file.path(
        temp_dir,
        rds_inside
      )
    )

  } else {

    stop(
      "Please select an .rds file ",
      "or the analysis-output .zip file."
    )
  }

  needed <- c(
    "fit_adaptive",
    "fit_endurance",
    "fit_risk"
  )

  missing_models <- setdiff(
    needed,
    names(obj)
  )

  if (length(missing_models) > 0) {

    stop(
      "The saved object does not contain: ",
      paste(
        missing_models,
        collapse = ", "
      )
    )
  }

  assign(
    "fit_adaptive",
    obj$fit_adaptive,
    envir = .GlobalEnv
  )

  assign(
    "fit_endurance",
    obj$fit_endurance,
    envir = .GlobalEnv
  )

  assign(
    "fit_risk",
    obj$fit_risk,
    envir = .GlobalEnv
  )

  rm(obj)

  cat(
    "\nModels loaded successfully.\n",
    "fit_adaptive:  ", class(fit_adaptive)[1], "\n",
    "fit_endurance: ", class(fit_endurance)[1], "\n",
    "fit_risk:      ", class(fit_risk)[1], "\n\n",
    sep = ""
  )

  invisible(TRUE)
}


load_saved_models()


stopifnot(
  inherits(
    fit_adaptive,
    "lavaan"
  ),
  inherits(
    fit_endurance,
    "lavaan"
  ),
  inherits(
    fit_risk,
    "lavaan"
  )
)


# =============================================================================
# 3. Display labels
# =============================================================================

pretty_labels <- c(

  "AppraisalRisk" =
    "Appraisal\nRisk",

  "Consequence" =
    "Consequence",

  "ControlLoss" =
    "Control\nLoss",

  "SelfThreat" =
    "Self-Worth\nThreat",

  "Vulnerability" =
    "Vulnerability",

  "Crush" =
    "Overwhelmed",

  "Exhaust" =
    "Exhaustion",

  "Withdraw" =
    "Withdrawal",

  "Adaptive" =
    "Adaptive\nResilience",

  "Endurance" =
    "Endurance\nResilience",

  "Resource" =
    "Resource",

  "Recovery" =
    "Recovery",

  "AcadAdapt" =
    "Academic\nAdaptation",

  "HelpSuppress" =
    "Help-Seeking\nSuppression",

  "PS_index" =
    "Pressure\nSituation",

  "stress_global" =
    "Overall\nStress",

  "appraisal_mean" =
    "Mean\nAppraisal",

  "HI_institutional" =
    "Institutional /\nProfessional\nHelp Intention"
)


get_node_labels <- function(fit) {

  nodes <- semPlot::semPlotModel(
    fit
  )@Vars$name

  labels <- nodes

  hit <- nodes %in% names(
    pretty_labels
  )

  labels[hit] <- unname(
    pretty_labels[
      nodes[hit]
    ]
  )

  labels
}


# =============================================================================
# 4. Layout helper functions
# =============================================================================

make_layout <- function(fit) {

  vars <- semPlot::semPlotModel(
    fit
  )@Vars$name

  L <- matrix(
    NA_real_,
    nrow = length(vars),
    ncol = 2
  )

  rownames(L) <- vars

  colnames(L) <- c(
    "x",
    "y"
  )

  L
}


set_xy <- function(
    L,
    name,
    x,
    y
) {

  if (name %in% rownames(L)) {

    L[
      name,
    ] <- c(
      x,
      y
    )
  }

  L
}


set_row <- function(
    L,
    names,
    xmin,
    xmax,
    y
) {

  names <- names[
    names %in% rownames(L)
  ]

  if (length(names) == 0) {

    return(L)
  }

  xs <- if (length(names) == 1) {

    mean(
      c(
        xmin,
        xmax
      )
    )

  } else {

    seq(
      xmin,
      xmax,
      length.out = length(names)
    )
  }

  for (i in seq_along(names)) {

    L[
      names[i],
    ] <- c(
      xs[i],
      y
    )
  }

  L
}


check_layout <- function(L) {

  bad <- rownames(L)[
    is.na(
      L[, 1]
    ) |
      is.na(
        L[, 2]
      )
  ]

  if (length(bad) > 0) {

    stop(
      "Missing coordinates for: ",
      paste(
        bad,
        collapse = ", "
      )
    )
  }

  L
}


# =============================================================================
# 5. Adaptive-model layout
# =============================================================================

layout_adaptive_manual <- function(fit) {

  L <- make_layout(
    fit
  )

  L <- set_row(
    L,
    c(
      "GR1",
      "GR2",
      "GR3",
      "GR4",
      "GR5"
    ),
    5.4,
    10.2,
    9.7
  )

  L <- set_xy(
    L,
    "Adaptive",
    7.8,
    7.1
  )

  L <- set_xy(
    L,
    "Resource",
    2.0,
    4.7
  )

  L <- set_xy(
    L,
    "Recovery",
    7.2,
    4.7
  )

  L <- set_xy(
    L,
    "AcadAdapt",
    12.5,
    4.7
  )

  L <- set_xy(
    L,
    "Withdraw",
    17.7,
    4.7
  )

  L <- set_xy(
    L,
    "PS_index",
    20.0,
    7.5
  )

  L <- set_row(
    L,
    c(
      "RC1",
      "RC2",
      "RC3",
      "RC4",
      "RC5",
      "RC6"
    ),
    0.5,
    3.8,
    1.1
  )

  L <- set_row(
    L,
    c(
      "BR1",
      "BR2",
      "BR3",
      "BR4",
      "BR5"
    ),
    5.5,
    8.9,
    1.1
  )

  L <- set_row(
    L,
    c(
      "AA1",
      "AA2",
      "AA3",
      "AA4",
      "AA5"
    ),
    10.8,
    14.2,
    1.1
  )

  L <- set_row(
    L,
    c(
      "AW1",
      "AW2",
      "AW3",
      "AW4"
    ),
    16.3,
    18.9,
    1.1
  )

  check_layout(
    L
  )
}


# =============================================================================
# 6. Endurance-model layout
# =============================================================================

layout_endurance_manual <- function(fit) {

  L <- make_layout(
    fit
  )

  L <- set_row(
    L,
    c(
      "GR1",
      "GR2",
      "GR3",
      "GR4",
      "GR5"
    ),
    5.0,
    9.8,
    9.8
  )

  L <- set_row(
    L,
    c(
      "HR1",
      "HR2",
      "HR3",
      "HR4",
      "HR5",
      "HR6"
    ),
    13.6,
    19.8,
    9.8
  )

  L <- set_xy(
    L,
    "Adaptive",
    7.2,
    7.2
  )

  L <- set_xy(
    L,
    "Endurance",
    16.8,
    7.2
  )

  L <- set_xy(
    L,
    "PS_index",
    0.7,
    8.0
  )

  L <- set_xy(
    L,
    "stress_global",
    0.7,
    5.3
  )

  L <- set_xy(
    L,
    "appraisal_mean",
    0.7,
    2.6
  )

  L <- set_xy(
    L,
    "HI_institutional",
    4.8,
    4.7
  )

  L <- set_xy(
    L,
    "HelpSuppress",
    12.3,
    4.7
  )

  L <- set_xy(
    L,
    "Exhaust",
    20.8,
    4.7
  )

  L <- set_row(
    L,
    c(
      "HS1",
      "HS2",
      "HS3",
      "HS4",
      "HS5"
    ),
    10.2,
    14.6,
    1.1
  )

  L <- set_row(
    L,
    c(
      "EE1",
      "EE2",
      "EE3",
      "EE4",
      "EE5"
    ),
    19.0,
    23.4,
    1.1
  )

  check_layout(
    L
  )
}


# =============================================================================
# 7. Risk-model layout
# =============================================================================

layout_risk_manual <- function(fit) {

  L <- make_layout(
    fit
  )

  L <- set_xy(
    L,
    "PS_index",
    1.4,
    10.0
  )

  L <- set_xy(
    L,
    "AppraisalRisk",
    10.5,
    10.0
  )

  L <- set_xy(
    L,
    "Consequence",
    2.8,
    6.8
  )

  L <- set_xy(
    L,
    "ControlLoss",
    6.8,
    6.8
  )

  L <- set_xy(
    L,
    "SelfThreat",
    10.8,
    6.8
  )

  L <- set_xy(
    L,
    "Vulnerability",
    14.8,
    6.8
  )

  L <- set_xy(
    L,
    "Crush",
    19.8,
    6.8
  )

  L <- set_xy(
    L,
    "Exhaust",
    24.6,
    6.8
  )

  L <- set_xy(
    L,
    "Withdraw",
    29.4,
    6.8
  )

  L <- set_row(
    L,
    c(
      "PN1",
      "PN2",
      "PN3"
    ),
    1.0,
    3.2,
    1.1
  )

  L <- set_row(
    L,
    c(
      "PN4",
      "PN5",
      "PN6"
    ),
    5.1,
    7.3,
    1.1
  )

  L <- set_row(
    L,
    c(
      "PN7",
      "PN8",
      "PN9"
    ),
    9.2,
    11.4,
    1.1
  )

  L <- set_row(
    L,
    c(
      "PN10",
      "PN11",
      "PN12"
    ),
    13.3,
    15.9,
    1.1
  )

  L <- set_row(
    L,
    c(
      "CF1",
      "CF2",
      "CF3",
      "CF4",
      "CF5",
      "CF6"
    ),
    18.0,
    21.6,
    1.1
  )

  L <- set_row(
    L,
    c(
      "EE1",
      "EE2",
      "EE3",
      "EE4",
      "EE5"
    ),
    23.2,
    26.4,
    1.1
  )

  L <- set_row(
    L,
    c(
      "AW1",
      "AW2",
      "AW3",
      "AW4"
    ),
    28.2,
    30.8,
    1.1
  )

  check_layout(
    L
  )
}


# =============================================================================
# 8. Center and resize item nodes
# =============================================================================

center_item_nodes <- function(
    g,
    fit,
    item_width = 4.2,
    item_height = 3.2,
    item_label_cex = 0.98
) {

  item_nodes <- grep(
    "^(GR|HR|RC|BR|AA|AW|HS|EE|PN|CF)[0-9]+$",
    semPlot::semPlotModel(
      fit
    )@Vars$name,
    value = TRUE
  )

  # Item labels themselves remain GR1, RC1, PN1, etc.,
  # so these names match the displayed node names.

  g <- semptools::set_node_width(
    g,
    stats::setNames(
      rep(
        item_width,
        length(item_nodes)
      ),
      item_nodes
    ),
    how = "value"
  )

  g <- semptools::set_node_height(
    g,
    stats::setNames(
      rep(
        item_height,
        length(item_nodes)
      ),
      item_nodes
    ),
    how = "value"
  )

  g <- semptools::set_node_label_size(
    g,
    stats::setNames(
      rep(
        item_label_cex,
        length(item_nodes)
      ),
      item_nodes
    )
  )

  g
}


# =============================================================================
# 9. Main plotting function
# =============================================================================

plot_sem_final <- function(
    fit,
    layout_mat,
    png_file,
    pdf_file,
    png_width,
    png_height,

    sizeLat = 15,
    sizeLat2 = 8.8,

    sizeMan = 7.0,
    sizeMan2 = 5.0,

    label_cex = 0.98,
    edge_cex = 0.44,

    curve_list = NULL,
    pos_list = NULL,

    item_width = 4.2,
    item_height = 3.2,
    item_label_cex = 0.98
) {

  node_labels <- get_node_labels(
    fit
  )


  draw_one <- function() {

    g <- semPlot::semPaths(

      object = fit,

      what = "std",

      whatLabels = "std",

      nDigits = 2,

      # Cleaner appearance
      style = "ram",

      layout = layout_mat,

      reorder = FALSE,

      rescale = TRUE,

      # Remove clutter
      thresholds = FALSE,

      intercepts = FALSE,

      residuals = FALSE,

      exoCov = FALSE,

      exoVar = FALSE,

      # Display labels
      nodeLabels = node_labels,

      nCharNodes = 0,

      nCharEdges = 0,

      # Shapes
      shapeLat = "ellipse",

      shapeMan = "rectangle",

      # Default sizes for general nodes
      sizeLat = sizeLat,

      sizeLat2 = sizeLat2,

      sizeMan = sizeMan,

      sizeMan2 = sizeMan2,

      equalizeManifests = TRUE,

      # Explicitly center labels in nodes
      node.label.offset = c(
        0.5,
        0.5
      ),

      # Keep node-label size stable
      label.cex = label_cex,

      label.scale = FALSE,

      label.prop = 1.0,

      # Edge labels
      edge.label.cex = edge_cex,

      edge.label.position = 0.52,

      edge.label.bg = TRUE,

      # Edges
      curve = 0.10,

      curvature = 0.18,

      curvePivot = TRUE,

      cardinal = "none",

      asize = 1.8,

      esize = 0.75,

      # Clean monochrome journal style
      color = list(
        lat = "white",
        man = "white"
      ),

      border.color = "black",

      edge.color = "black",

      fade = FALSE,

      mar = c(
        3,
        3,
        3,
        3
      ),

      title = FALSE,

      DoNotPlot = TRUE
    )


    # -------------------------------------------------------------------------
    # Resize ONLY questionnaire-item boxes and center their labels
    # -------------------------------------------------------------------------

    g <- center_item_nodes(

      g = g,

      fit = fit,

      item_width = item_width,

      item_height = item_height,

      item_label_cex = item_label_cex
    )


    # -------------------------------------------------------------------------
    # Automatic edge-label adjustment
    # -------------------------------------------------------------------------

    g <- tryCatch(

      semptools::safe_edge_label_position(
        g,
        update_plot = TRUE
      ),

      error = function(e) {

        message(
          "safe_edge_label_position skipped: ",
          e$message
        )

        g
      }
    )


    # -------------------------------------------------------------------------
    # Optional manual path-curve refinement
    # -------------------------------------------------------------------------

    if (!is.null(curve_list)) {

      g <- tryCatch(

        semptools::set_curve(
          g,
          curve_list
        ),

        error = function(e) {

          message(
            "set_curve skipped: ",
            e$message
          )

          g
        }
      )
    }


    # -------------------------------------------------------------------------
    # Optional manual edge-label-position refinement
    # -------------------------------------------------------------------------

    if (!is.null(pos_list)) {

      g <- tryCatch(

        semptools::set_edge_label_position(
          g,
          pos_list
        ),

        error = function(e) {

          message(
            "set_edge_label_position skipped: ",
            e$message
          )

          g
        }
      )
    }


    plot(
      g
    )
  }


  # ---------------------------------------------------------------------------
  # PNG
  # ---------------------------------------------------------------------------

  grDevices::png(

    filename = png_file,

    width = png_width,

    height = png_height,

    res = 600,

    bg = "white"
  )

  draw_one()

  grDevices::dev.off()


  # ---------------------------------------------------------------------------
  # PDF
  # ---------------------------------------------------------------------------

  grDevices::pdf(

    file = pdf_file,

    width = png_width / 600,

    height = png_height / 600,

    useDingbats = FALSE,

    bg = "white"
  )

  draw_one()

  grDevices::dev.off()


  # ---------------------------------------------------------------------------
  # Check output
  # ---------------------------------------------------------------------------

  png_ok <- file.exists(
    png_file
  ) &&
    file.info(
      png_file
    )$size > 0

  pdf_ok <- file.exists(
    pdf_file
  ) &&
    file.info(
      pdf_file
    )$size > 0


  cat(
    "\n----------------------------------------\n",
    "PNG saved: ",
    png_ok,
    "\n",
    normalizePath(
      png_file,
      winslash = "/",
      mustWork = FALSE
    ),
    "\n",
    "PDF saved: ",
    pdf_ok,
    "\n",
    normalizePath(
      pdf_file,
      winslash = "/",
      mustWork = FALSE
    ),
    "\n",
    "----------------------------------------\n",
    sep = ""
  )


  invisible(
    list(
      png = png_file,
      pdf = pdf_file,
      png_ok = png_ok,
      pdf_ok = pdf_ok
    )
  )
}


# =============================================================================
# 10. Build layouts
# =============================================================================

layout_adaptive <- layout_adaptive_manual(
  fit_adaptive
)

layout_endurance <- layout_endurance_manual(
  fit_endurance
)

layout_risk <- layout_risk_manual(
  fit_risk
)


# =============================================================================
# 11. Optional V5 structural-path micro-adjustments
# =============================================================================

adaptive_pos <- c(

  "Recovery ~ Adaptive" =
    0.62,

  "AcadAdapt ~ Adaptive" =
    0.40,

  "Withdraw ~ Adaptive" =
    0.76,

  "Resource ~ Adaptive" =
    0.54
)


adaptive_curve <- c(

  "Withdraw ~ Adaptive" =
    -0.8
)


endurance_pos <- c(

  "HI_institutional ~ PS_index" =
    0.68,

  "HI_institutional ~ stress_global" =
    0.50,

  "HI_institutional ~ appraisal_mean" =
    0.34,

  "HelpSuppress ~ Adaptive" =
    0.75,

  "HelpSuppress ~ Endurance" =
    0.25,

  "HelpSuppress ~ PS_index" =
    0.20,

  "HelpSuppress ~ stress_global" =
    0.48,

  "HelpSuppress ~ appraisal_mean" =
    0.30,

  "Exhaust ~ Adaptive" =
    0.72,

  "Exhaust ~ Endurance" =
    0.28,

  "Exhaust ~ HelpSuppress" =
    0.58
)


endurance_curve <- c(

  "HelpSuppress ~ Adaptive" =
    -0.9,

  "HelpSuppress ~ Endurance" =
    0.9,

  "Exhaust ~ Adaptive" =
    -1.0,

  "Exhaust ~ Endurance" =
    1.0,

  "Exhaust ~ HelpSuppress" =
    0.5
)


risk_pos <- c(

  "Consequence ~ AppraisalRisk" =
    0.22,

  "ControlLoss ~ AppraisalRisk" =
    0.36,

  "SelfThreat ~ AppraisalRisk" =
    0.50,

  "Vulnerability ~ AppraisalRisk" =
    0.64,

  "Crush ~ AppraisalRisk" =
    0.20,

  "Exhaust ~ AppraisalRisk" =
    0.52,

  "Withdraw ~ AppraisalRisk" =
    0.84,

  "Exhaust ~ Crush" =
    0.32,

  "Withdraw ~ Crush" =
    0.68
)


risk_curve <- c(

  "Crush ~ AppraisalRisk" =
    -0.8,

  "Exhaust ~ AppraisalRisk" =
    0.8,

  "Withdraw ~ AppraisalRisk" =
    1.5,

  "Exhaust ~ Crush" =
    -0.5,

  "Withdraw ~ Crush" =
    0.5
)


# =============================================================================
# 12. Draw Adaptive figure
# =============================================================================

result_adaptive <- plot_sem_final(

  fit = fit_adaptive,

  layout_mat = layout_adaptive,

  png_file = file.path(
    OUT_DIR,
    "SEM_Adaptive_Path_FINAL_v5_1.png"
  ),

  pdf_file = file.path(
    OUT_DIR,
    "SEM_Adaptive_Path_FINAL_v5_1.pdf"
  ),

  png_width = 9800,

  png_height = 4800,

  sizeLat = 15.5,

  sizeLat2 = 9.0,

  sizeMan = 7.0,

  sizeMan2 = 5.0,

  label_cex = 0.98,

  edge_cex = 0.44,

  curve_list = adaptive_curve,

  pos_list = adaptive_pos,

  item_width = 4.2,

  item_height = 3.2,

  item_label_cex = 0.98
)


# =============================================================================
# 13. Draw Endurance figure
# =============================================================================

result_endurance <- plot_sem_final(

  fit = fit_endurance,

  layout_mat = layout_endurance,

  png_file = file.path(
    OUT_DIR,
    "SEM_Endurance_Path_FINAL_v5_1.png"
  ),

  pdf_file = file.path(
    OUT_DIR,
    "SEM_Endurance_Path_FINAL_v5_1.pdf"
  ),

  png_width = 10800,

  png_height = 5000,

  sizeLat = 15.5,

  sizeLat2 = 9.0,

  sizeMan = 7.2,

  sizeMan2 = 5.0,

  label_cex = 0.98,

  edge_cex = 0.43,

  curve_list = endurance_curve,

  pos_list = endurance_pos,

  item_width = 4.2,

  item_height = 3.2,

  item_label_cex = 0.98
)


# =============================================================================
# 14. Draw Risk figure
# =============================================================================

result_risk <- plot_sem_final(

  fit = fit_risk,

  layout_mat = layout_risk,

  png_file = file.path(
    OUT_DIR,
    "SEM_Risk_Path_FINAL_v5_1.png"
  ),

  pdf_file = file.path(
    OUT_DIR,
    "SEM_Risk_Path_FINAL_v5_1.pdf"
  ),

  png_width = 13200,

  png_height = 5600,

  sizeLat = 14.2,

  sizeLat2 = 8.2,

  sizeMan = 6.3,

  sizeMan2 = 4.6,

  label_cex = 0.84,

  edge_cex = 0.38,

  curve_list = risk_curve,

  pos_list = risk_pos,

  item_width = 3.8,

  item_height = 3.0,

  item_label_cex = 0.90
)


# =============================================================================
# 15. Final verification
# =============================================================================

all_files <- c(

  result_adaptive$png,

  result_adaptive$pdf,

  result_endurance$png,

  result_endurance$pdf,

  result_risk$png,

  result_risk$pdf
)


verification <- data.frame(

  file = basename(
    all_files
  ),

  exists = file.exists(
    all_files
  ),

  size_MB = round(
    file.info(
      all_files
    )$size / 1024^2,
    2
  )
)


print(
  verification
)


cat(
  "\n============================================\n",
  "FINISHED\n",
  "Figures saved in:\n",
  normalizePath(
    OUT_DIR,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n============================================\n",
  sep = ""
)


# =============================================================================
# 16. Open output folder automatically on Windows
# =============================================================================

if (.Platform$OS.type == "windows") {

  shell.exec(
    normalizePath(
      OUT_DIR,
      winslash = "\\",
      mustWork = FALSE
    )
  )
}
