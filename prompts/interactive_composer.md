# 交互式组图编排器 - Interactive Figure Composer

提供图形化交互界面，让用户轻松完成发表级组图的最终编排。

## 功能特点

1. **图片选择** - 勾选需要组合的子图
2. **布局模式** - n×m矩阵 或 自定义区域拖放
3. **尺寸预设** - A4、期刊规范、自定义尺寸
4. **实时预览** - 所见即所得
5. **一键导出** - PDF/PNG/TIFF多格式

## 两种使用方式

### 方式1: Shiny交互界面（推荐）
完整的图形化操作界面，支持拖放布局

### 方式2: 命令行交互
轻量级菜单式交互，适合快速操作

---

## 完整代码

```r
# ============================================================
# 交互式组图编排器 - Interactive Figure Composer
# ============================================================

# 加载必要的包
required_packages <- c("shiny", "shinydashboard", "sortable",
                       "ggplot2", "patchwork", "grid", "gridExtra")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(shiny)
library(shinydashboard)
library(sortable)
library(ggplot2)
library(patchwork)
library(grid)
library(gridExtra)

# ============================================================
# 预设尺寸配置
# ============================================================

SIZE_PRESETS <- list(
  "A4纵向(带边距)" = list(
    width = 210 - 25*2,  # mm, 左右各25mm边距
    height = 297 - 38*2, # mm, 上下各38mm边距
    desc = "160 × 221 mm (A4去边距)"
  ),
  "A4横向(带边距)" = list(
    width = 297 - 25*2,
    height = 210 - 38*2,
    desc = "247 × 134 mm"
  ),
  "Nature单栏" = list(
    width = 89,
    height = 89,
    desc = "89 × 89 mm (正方形)"
  ),
  "Nature双栏" = list(
    width = 183,
    height = 150,
    desc = "183 × 150 mm"
  ),
  "Cell双栏" = list(
    width = 178,
    height = 150,
    desc = "178 × 150 mm"
  ),
  "120×150" = list(
    width = 120,
    height = 150,
    desc = "120 × 150 mm (常用)"
  ),
  "140×180" = list(
    width = 140,
    height = 180,
    desc = "140 × 180 mm"
  ),
  "自定义" = list(
    width = NA,
    height = NA,
    desc = "手动输入尺寸"
  )
)

# ============================================================
# 方式1: Shiny交互界面
# ============================================================

launch_figure_composer <- function(plot_list = NULL) {

  # 如果没有提供图片列表，使用示例
  if (is.null(plot_list)) {
    message("未提供图片列表，将使用示例图片演示")
    plot_list <- list(
      "火山图" = ggplot(mtcars, aes(mpg, hp)) + geom_point() + ggtitle("A: Volcano"),
      "热图" = ggplot(mtcars, aes(mpg, wt)) + geom_point() + ggtitle("B: Heatmap"),
      "富集图" = ggplot(mtcars, aes(mpg, qsec)) + geom_point() + ggtitle("C: Enrichment"),
      "网络图" = ggplot(mtcars, aes(mpg, drat)) + geom_point() + ggtitle("D: Network")
    )
  }

  # 确保有名字
  if (is.null(names(plot_list))) {
    names(plot_list) <- paste0("图", seq_along(plot_list))
  }

  # ---- UI ----
  ui <- dashboardPage(
    dashboardHeader(title = "组图编排器 Figure Composer"),

    dashboardSidebar(
      width = 280,

      # 步骤1: 选择图片
      h4("步骤1: 选择子图", style = "padding-left: 15px; color: #00A087;"),
      checkboxGroupInput("selected_plots", "勾选要组合的图片:",
                         choices = names(plot_list),
                         selected = names(plot_list)),
      hr(),

      # 步骤2: 布局模式
      h4("步骤2: 布局模式", style = "padding-left: 15px; color: #E64B35;"),
      radioButtons("layout_mode", NULL,
                   choices = c("矩阵布局 (n×m)" = "matrix",
                              "自定义布局" = "custom"),
                   selected = "matrix"),

      # 矩阵布局选项
      conditionalPanel(
        condition = "input.layout_mode == 'matrix'",
        fluidRow(
          column(6, numericInput("nrow", "行数:", value = 2, min = 1, max = 6)),
          column(6, numericInput("ncol", "列数:", value = 2, min = 1, max = 6))
        )
      ),

      # 自定义布局选项
      conditionalPanel(
        condition = "input.layout_mode == 'custom'",
        textAreaInput("custom_layout", "布局代码 (字母代表图片):",
                      value = "AABB\nAABB\nCCDD\nEEEE",
                      rows = 4,
                      placeholder = "例如:\nAAB\nAAC\nDDD"),
        helpText("A=第1张图, B=第2张, 以此类推")
      ),
      hr(),

      # 步骤3: 输出尺寸
      h4("步骤3: 输出尺寸", style = "padding-left: 15px; color: #3C5488;"),
      selectInput("size_preset", "预设尺寸:",
                  choices = names(SIZE_PRESETS),
                  selected = "A4纵向(带边距)"),

      conditionalPanel(
        condition = "input.size_preset == '自定义'",
        fluidRow(
          column(6, numericInput("custom_width", "宽度(mm):", value = 150, min = 50, max = 300)),
          column(6, numericInput("custom_height", "高度(mm):", value = 180, min = 50, max = 400))
        )
      ),

      verbatimTextOutput("size_info"),
      hr(),

      # 步骤4: 导出
      h4("步骤4: 导出", style = "padding-left: 15px; color: #F39B7F;"),
      textInput("filename", "文件名:", value = "Figure_combined"),
      checkboxGroupInput("formats", "输出格式:",
                         choices = c("PDF (矢量)" = "pdf",
                                    "PNG (300dpi)" = "png",
                                    "TIFF (600dpi)" = "tiff"),
                         selected = c("pdf", "png")),
      actionButton("export", "导出图片",
                   class = "btn-primary",
                   style = "width: 100%; margin-top: 10px;")
    ),

    dashboardBody(
      tags$head(
        tags$style(HTML("
          .preview-box {
            border: 2px dashed #ccc;
            min-height: 500px;
            background: white;
            padding: 10px;
          }
          .layout-cell {
            border: 1px solid #ddd;
            padding: 5px;
            margin: 2px;
            text-align: center;
            cursor: move;
          }
        "))
      ),

      fluidRow(
        box(
          title = "预览 Preview",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          div(class = "preview-box",
              plotOutput("preview", height = "600px")
          )
        )
      ),

      # 自定义布局时的拖放区域
      conditionalPanel(
        condition = "input.layout_mode == 'custom'",
        fluidRow(
          box(
            title = "布局设计器 (拖放排列)",
            status = "info",
            width = 12,
            uiOutput("drag_drop_ui")
          )
        )
      )
    )
  )

  # ---- Server ----
  server <- function(input, output, session) {

    # 尺寸信息显示
    output$size_info <- renderText({
      preset <- SIZE_PRESETS[[input$size_preset]]
      if (input$size_preset == "自定义") {
        paste0("尺寸: ", input$custom_width, " × ", input$custom_height, " mm")
      } else {
        preset$desc
      }
    })

    # 获取当前尺寸
    get_size <- reactive({
      if (input$size_preset == "自定义") {
        list(width = input$custom_width, height = input$custom_height)
      } else {
        SIZE_PRESETS[[input$size_preset]]
      }
    })

    # 获取选中的图片
    get_selected_plots <- reactive({
      plot_list[input$selected_plots]
    })

    # 生成布局
    get_layout <- reactive({
      n_plots <- length(input$selected_plots)

      if (input$layout_mode == "matrix") {
        # 矩阵布局
        NULL  # patchwork会自动处理
      } else {
        # 自定义布局
        input$custom_layout
      }
    })

    # 预览图
    output$preview <- renderPlot({
      req(length(input$selected_plots) > 0)

      plots <- get_selected_plots()
      layout <- get_layout()

      # 组合图片
      if (input$layout_mode == "matrix") {
        combined <- wrap_plots(plots, nrow = input$nrow, ncol = input$ncol)
      } else {
        # 解析自定义布局
        combined <- wrap_plots(plots, design = layout)
      }

      combined
    }, res = 100)

    # 拖放UI
    output$drag_drop_ui <- renderUI({
      req(length(input$selected_plots) > 0)

      plot_names <- input$selected_plots
      letters_used <- LETTERS[1:length(plot_names)]

      tagList(
        p("拖动调整顺序，字母对应关系:"),
        tags$ul(
          lapply(seq_along(plot_names), function(i) {
            tags$li(paste0(letters_used[i], " = ", plot_names[i]))
          })
        ),
        rank_list(
          text = "拖动排序",
          labels = lapply(seq_along(plot_names), function(i) {
            div(class = "layout-cell",
                paste0(letters_used[i], ": ", plot_names[i]))
          }),
          input_id = "plot_order"
        )
      )
    })

    # 导出
    observeEvent(input$export, {
      req(length(input$selected_plots) > 0)

      plots <- get_selected_plots()
      layout <- get_layout()
      size <- get_size()

      # 转换mm到inch
      width_inch <- size$width / 25.4
      height_inch <- size$height / 25.4

      # 组合
      if (input$layout_mode == "matrix") {
        combined <- wrap_plots(plots, nrow = input$nrow, ncol = input$ncol)
      } else {
        combined <- wrap_plots(plots, design = layout)
      }

      # 保存文件
      saved_files <- c()

      for (fmt in input$formats) {
        filename <- paste0(input$filename, ".", fmt)

        tryCatch({
          if (fmt == "pdf") {
            ggsave(filename, combined, width = width_inch, height = height_inch,
                   device = cairo_pdf)
          } else if (fmt == "png") {
            ggsave(filename, combined, width = width_inch, height = height_inch,
                   dpi = 300)
          } else if (fmt == "tiff") {
            ggsave(filename, combined, width = width_inch, height = height_inch,
                   dpi = 600, compression = "lzw")
          }
          saved_files <- c(saved_files, filename)
        }, error = function(e) {
          showNotification(paste("保存失败:", e$message), type = "error")
        })
      }

      if (length(saved_files) > 0) {
        showNotification(
          paste("已保存:", paste(saved_files, collapse = ", ")),
          type = "message",
          duration = 5
        )
      }
    })
  }

  # 启动应用
  shinyApp(ui, server)
}


# ============================================================
# 方式2: 命令行交互界面
# ============================================================

interactive_compose <- function(plot_list) {

  if (is.null(names(plot_list))) {
    names(plot_list) <- paste0("图", seq_along(plot_list))
  }

  cat("\n")
  cat("╔══════════════════════════════════════════════════════════╗\n")
  cat("║          交互式组图编排器 Figure Composer                ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")

  # ---- 步骤1: 选择图片 ----
  cat("\n【步骤1】选择要组合的子图\n")
  cat("─────────────────────────────\n")
  for (i in seq_along(plot_list)) {
    cat(sprintf("  [%d] %s\n", i, names(plot_list)[i]))
  }
  cat(sprintf("  [A] 全选 (%d张图)\n", length(plot_list)))

  selection <- readline("请输入选择 (如: 1,2,3 或 A): ")

  if (toupper(selection) == "A") {
    selected_idx <- seq_along(plot_list)
  } else {
    selected_idx <- as.integer(strsplit(selection, ",")[[1]])
    selected_idx <- selected_idx[!is.na(selected_idx)]
  }

  selected_plots <- plot_list[selected_idx]
  cat(sprintf("已选择 %d 张图: %s\n",
              length(selected_plots),
              paste(names(selected_plots), collapse = ", ")))

  # ---- 步骤2: 布局模式 ----
  cat("\n【步骤2】选择布局模式\n")
  cat("─────────────────────────────\n")
  cat("  [1] 矩阵布局 (n×m)\n")
  cat("  [2] 自定义布局 (字母指定区域)\n")

  layout_choice <- readline("请选择 (1/2): ")

  if (layout_choice == "1") {
    # 矩阵布局
    nrow <- as.integer(readline("请输入行数: "))
    ncol <- as.integer(readline("请输入列数: "))
    layout <- NULL
    layout_params <- list(nrow = nrow, ncol = ncol)
    cat(sprintf("布局: %d行 × %d列\n", nrow, ncol))
  } else {
    # 自定义布局
    cat("\n自定义布局说明:\n")
    cat("  - 使用字母A、B、C...代表第1、2、3...张图\n")
    cat("  - 同一字母占据的区域会合并\n")
    cat("  - 用回车分隔行\n")
    cat("  - 例如:\n")
    cat("    AAB    (A占左侧2/3上半, B占右侧1/3上半)\n")
    cat("    AAC    (A继续, C占右侧1/3中部)\n")
    cat("    DDD    (D占整个下半)\n")
    cat("\n")
    cat("常用预设:\n")
    cat("  [a] 2×2标准: AB/CD\n")
    cat("  [b] 1大3小: AAB/AAC/AAD\n")
    cat("  [c] 上大下小: AAA/BCD\n")
    cat("  [d] 自己输入\n")

    preset <- readline("选择预设或自己输入 (a/b/c/d): ")

    if (preset == "a") {
      layout <- "AB\nCD"
    } else if (preset == "b") {
      layout <- "AAB\nAAC\nAAD"
    } else if (preset == "c") {
      layout <- "AAA\nBCD"
    } else {
      cat("请逐行输入布局 (输入空行结束):\n")
      lines <- c()
      repeat {
        line <- readline()
        if (line == "") break
        lines <- c(lines, line)
      }
      layout <- paste(lines, collapse = "\n")
    }
    layout_params <- list(design = layout)
    cat(sprintf("布局:\n%s\n", layout))
  }

  # ---- 步骤3: 输出尺寸 ----
  cat("\n【步骤3】选择输出尺寸\n")
  cat("─────────────────────────────\n")
  cat("  [1] A4纵向(带边距): 160×221 mm\n")
  cat("  [2] A4横向(带边距): 247×134 mm\n")
  cat("  [3] Nature单栏: 89×89 mm\n")
  cat("  [4] Nature双栏: 183×150 mm\n")
  cat("  [5] 120×150 mm (常用)\n")
  cat("  [6] 140×180 mm\n")
  cat("  [7] 自定义尺寸\n")

  size_choice <- readline("请选择 (1-7): ")

  size_map <- list(
    "1" = c(160, 221),
    "2" = c(247, 134),
    "3" = c(89, 89),
    "4" = c(183, 150),
    "5" = c(120, 150),
    "6" = c(140, 180)
  )

  if (size_choice %in% names(size_map)) {
    size_mm <- size_map[[size_choice]]
  } else {
    w <- as.numeric(readline("请输入宽度 (mm): "))
    h <- as.numeric(readline("请输入高度 (mm): "))
    size_mm <- c(w, h)
  }

  width_inch <- size_mm[1] / 25.4
  height_inch <- size_mm[2] / 25.4
  cat(sprintf("尺寸: %d × %d mm (%.2f × %.2f inch)\n",
              size_mm[1], size_mm[2], width_inch, height_inch))

  # ---- 步骤4: 文件名和格式 ----
  cat("\n【步骤4】输出设置\n")
  cat("─────────────────────────────\n")

  filename <- readline("文件名 (不含扩展名) [Figure_combined]: ")
  if (filename == "") filename <- "Figure_combined"

  cat("输出格式 (可多选, 用逗号分隔):\n")
  cat("  [1] PDF (矢量, 推荐投稿)\n")
  cat("  [2] PNG (300dpi)\n")
  cat("  [3] TIFF (600dpi, 印刷)\n")

  format_choice <- readline("请选择 (如: 1,2): ")
  format_idx <- as.integer(strsplit(format_choice, ",")[[1]])
  formats <- c("pdf", "png", "tiff")[format_idx]

  # ---- 生成并保存 ----
  cat("\n【生成组图中...】\n")
  cat("─────────────────────────────\n")

  # 组合图片
  if (is.null(layout)) {
    combined <- wrap_plots(selected_plots,
                           nrow = layout_params$nrow,
                           ncol = layout_params$ncol)
  } else {
    combined <- wrap_plots(selected_plots, design = layout)
  }

  # 保存
  saved_files <- c()
  for (fmt in formats) {
    out_file <- paste0(filename, ".", fmt)

    tryCatch({
      if (fmt == "pdf") {
        ggsave(out_file, combined, width = width_inch, height = height_inch,
               device = cairo_pdf)
      } else if (fmt == "png") {
        ggsave(out_file, combined, width = width_inch, height = height_inch,
               dpi = 300)
      } else if (fmt == "tiff") {
        ggsave(out_file, combined, width = width_inch, height = height_inch,
               dpi = 600, compression = "lzw")
      }
      saved_files <- c(saved_files, out_file)
      cat(sprintf("✓ 已保存: %s\n", out_file))
    }, error = function(e) {
      cat(sprintf("✗ 保存失败 %s: %s\n", out_file, e$message))
    })
  }

  cat("\n╔══════════════════════════════════════════════════════════╗\n")
  cat("║                      完成！                               ║\n")
  cat("╚══════════════════════════════════════════════════════════╝\n")
  cat(sprintf("输出文件: %s\n", paste(saved_files, collapse = ", ")))

  return(invisible(combined))
}


# ============================================================
# 快速启动函数
# ============================================================

# Shiny界面
# launch_figure_composer(your_plot_list)

# 命令行交互
# interactive_compose(your_plot_list)
```

## 使用方法

### Shiny图形界面（推荐）

```r
# 准备你的图片列表
plots <- list(
  "火山图" = p_volcano,
  "热图" = p_heatmap,
  "富集分析" = p_enrichment,
  "网络图" = p_network,
  "PCA" = p_pca
)

# 启动交互界面
launch_figure_composer(plots)
```

界面功能：
- ✅ 勾选需要的子图
- ✅ 选择矩阵布局或自定义布局
- ✅ 预设尺寸一键选择
- ✅ 实时预览
- ✅ 多格式导出

### 命令行交互

```r
# 简洁的菜单式交互
interactive_compose(plots)
```

会依次询问：
1. 选择哪些图？
2. 布局模式？
3. 输出尺寸？
4. 文件名和格式？

## 预设尺寸说明

| 预设 | 尺寸 (mm) | 适用场景 |
|------|-----------|----------|
| A4纵向(带边距) | 160 × 221 | 论文正文配图 |
| A4横向(带边距) | 247 × 134 | 宽幅数据展示 |
| Nature单栏 | 89 × 89 | 单栏方形图 |
| Nature双栏 | 183 × 150 | 双栏主图 |
| 120×150 | 120 × 150 | 通用尺寸 |

请提供你的图片列表，我将帮你启动交互式编排。
