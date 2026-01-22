# RomicShow

R语言组学数据分析与可视化工具包 | R Omics Data Visualization Toolkit

## 简介

这是一个用于Claude Code的自定义Skills，专注于使用R语言进行组学数据的分析与可视化。

## 功能模块

| 命令 | 说明 |
|------|------|
| `/omics-analysis` | 组学数据综合分析 |
| `/volcano` | 火山图 |
| `/heatmap` | 聚类热图 |
| `/enrichment` | GO/KEGG富集分析 |
| `/network` | 蛋白互作网络 |
| `/venn` | 韦恩图/UpSet图 |
| `/pca-umap` | 降维分析 |
| `/publication-figure` | 发表级组合图 |
| `/compose` | 交互式组图编排器 |

## 安装要求

### R包依赖

```r
# CRAN包
install.packages(c(
  "tidyverse", "ggplot2", "ggrepel", "patchwork", "cowplot",
  "circlize", "pheatmap", "ggVennDiagram", "VennDiagram",
  "igraph", "ggraph", "tidygraph", "umap", "FactoMineR", "factoextra",
  "shiny", "shinydashboard", "sortable", "gridExtra"
))

# Bioconductor包
BiocManager::install(c(
  "ComplexHeatmap", "clusterProfiler", "enrichplot", "pathview",
  "org.Hs.eg.db", "org.Mm.eg.db", "org.Rn.eg.db"
))
```

## 使用方法

1. 在Claude Code中输入对应命令
2. 根据提示提供数据
3. 获取定制化的R脚本

## 示例

```
用户: /volcano 我有一个差异表达结果，包含gene, log2FC, pvalue列

Claude: 我将帮你创建火山图...
```

## 作者

[Benson Kin]

## 版本

v1.0.0 - 2026-01
