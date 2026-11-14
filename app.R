# =====================================================================
#  BETONLENS - Dashboard Interaktif Kuat Tekan Beton (R Shiny)
#  Data   : Concrete_Data.xls  (UCI - Concrete Compressive Strength)
#           1030 observasi, 8 prediktor + 1 respon (Strength, MPa)
#  Isi    : Ringkasan | Univariat | Bivariat | Multivariat | Regresi | Data
#  File ini harus bernama app.R dan satu folder dengan Concrete_Data.xls
# =====================================================================

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(plotly)
library(DT)

# ---- Identitas pembuat (UBAH DI SINI) --------------------------------
pembuat <- "Agustinus_Karol_Sani_2310680121"

# ---- Palet "Concrete & Safety" ---------------------------------------
pal <- list(bg = "#EDEFF2", surface = "#FFFFFF", edge = "#D5D9E0",
            grid = "#E3E6EB", ink = "#1E2530", muted = "#6B7685",
            nav = "#1F2937", orange = "#F26B1D", blue = "#2F6FDE",
            teal = "#12A38A", purple = "#7C5CFA", yellow = "#F2B705")
bg <- pal$bg; surface <- pal$surface; edge <- pal$edge; grid_col <- pal$grid
ink <- pal$ink; muted <- pal$muted
orange <- pal$orange; blue <- pal$blue; teal <- pal$teal
purple <- pal$purple; yellow <- pal$yellow
cols <- c(orange, blue, teal, purple, yellow)

# ---- Data ------------------------------------------------------------
raw <- as.data.frame(read_excel("Concrete_Data.xls"))
names(raw) <- c("Cement", "Slag", "FlyAsh", "Water", "SP",
                "CoarseAgg", "FineAgg", "Age", "Strength")

df <- raw %>%
  mutate(
    Dup = duplicated(raw),
    WC  = Water / Cement,
    AgeGroup = cut(Age, breaks = c(0, 3, 7, 28, 91, Inf),
                   labels = c("<=3 hari", "4-7 hari", "8-28 hari",
                              "29-91 hari", ">91 hari")),
    StrengthClass = cut(Strength, breaks = c(0, 20, 40, 60, Inf),
                        labels = c("Rendah (<20)", "Sedang (20-40)",
                                   "Tinggi (40-60)", "Sangat tinggi (>60)")),
    MixType = factor(
      case_when(Slag == 0 & FlyAsh == 0 ~ "Semen saja",
                Slag >  0 & FlyAsh == 0 ~ "Semen + Slag",
                Slag == 0 & FlyAsh >  0 ~ "Semen + Fly Ash",
                TRUE                    ~ "Semen + Slag + Fly Ash"),
      levels = c("Semen saja", "Semen + Slag", "Semen + Fly Ash",
                 "Semen + Slag + Fly Ash")),
    SPUse = factor(ifelse(SP > 0, "Pakai superplasticizer",
                          "Tanpa superplasticizer"))
  )

pred_vars <- c("Semen (Cement)"       = "Cement",
               "Slag"                 = "Slag",
               "Fly Ash"              = "FlyAsh",
               "Air (Water)"          = "Water",
               "Superplasticizer"     = "SP",
               "Agregat Kasar"        = "CoarseAgg",
               "Agregat Halus"        = "FineAgg",
               "Umur, hari (Age)"     = "Age")
num_vars <- c(pred_vars,
              "Kuat Tekan, MPa (Strength)" = "Strength",
              "Rasio Air/Semen (WC)"       = "WC")
cat_vars <- c("Tipe Campuran"   = "MixType",
              "Kelompok Umur"   = "AgeGroup",
              "Kelas Kekuatan"  = "StrengthClass",
              "Superplasticizer" = "SPUse")
lab <- function(v) {
  all <- c(num_vars, cat_vars)
  out <- names(all)[match(v, all)]
  ifelse(is.na(out), v, out)
}

# ---- Fungsi bantu ----------------------------------------------------
fmt <- function(x, d = 0)
  format(round(x, d), big.mark = ".", decimal.mark = ",", nsmall = d, trim = TRUE)
pfmt <- function(p) ifelse(p < 0.001, "<0,001", fmt(p, 3))

group_colors <- function(lv) setNames(rep_len(cols, length(lv)), lv)

kpi <- function(label, value, color) {
  div(class = "kpi", style = paste0("--accent:", color),
      div(class = "kpi-label", label), div(class = "kpi-value", value))
}
legend_chips <- function(lv, gc) {
  div(class = "chips", lapply(lv, function(l)
    span(class = "chip", span(class = "dot", style = paste0("background:", gc[[l]])), l)))
}
base_plotly <- function(p) {
  p %>% layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               font = list(color = ink, size = 12),
               legend = list(bgcolor = "rgba(0,0,0,0)"),
               margin = list(t = 30, r = 15)) %>%
    config(displaylogo = FALSE)
}
style_plotly <- function(p) {
  base_plotly(p) %>%
    layout(xaxis = list(gridcolor = grid_col, zerolinecolor = edge),
           yaxis = list(gridcolor = grid_col, zerolinecolor = edge))
}
skew <- function(x) {
  m <- mean(x); s2 <- mean((x - m)^2)
  if (s2 == 0) return(NA_real_)
  mean((x - m)^3) / s2^1.5
}
describe <- function(x) {
  q <- quantile(x, c(.25, .5, .75))
  data.frame(N = length(x), Mean = mean(x), Median = q[[2]], SD = sd(x),
             Min = min(x), Q1 = q[[1]], Q3 = q[[3]], Max = max(x),
             IQR = q[[3]] - q[[1]], Skewness = skew(x),
             Outlier = length(boxplot.stats(x)$out))
}

# ---- Fungsi regresi (tanpa paket tambahan) ---------------------------
make_formula <- function(vars, logage) {
  terms <- ifelse(vars == "Age" & logage, "log(Age)", vars)
  reformulate(terms, response = "Strength")
}
vif_fit <- function(fit) {
  mm <- as.data.frame(model.matrix(fit)[, -1, drop = FALSE])
  nm <- colnames(mm)
  if (ncol(mm) < 2) return(setNames(NA_real_, nm))
  colnames(mm) <- paste0("V", seq_along(nm))
  v <- sapply(colnames(mm), function(j) {
    r2 <- summary(lm(reformulate(setdiff(colnames(mm), j), response = j),
                     data = mm))$r.squared
    1 / (1 - r2)
  })
  setNames(as.numeric(v), nm)
}
bp_test <- function(fit) {            # Breusch-Pagan (versi Koenker)
  mm <- model.matrix(fit)[, -1, drop = FALSE]
  e2 <- resid(fit)^2
  r2 <- summary(lm(e2 ~ mm))$r.squared
  stat <- length(e2) * r2
  c(stat = stat, p = pchisq(stat, df = ncol(mm), lower.tail = FALSE))
}
dw_stat <- function(fit) { e <- resid(fit); sum(diff(e)^2) / sum(e^2) }
eval_model <- function(fml, tr, te) {
  m <- lm(fml, data = tr); p <- predict(m, te); a <- te$Strength
  c(RMSE = sqrt(mean((a - p)^2)), MAE = mean(abs(a - p)),
    R2_test = 1 - sum((a - p)^2) / sum((a - mean(a))^2),
    R2_train = summary(m)$r.squared)
}
class_label <- function(x)
  as.character(cut(x, c(-Inf, 20, 40, 60, Inf),
                   labels = c("Rendah (<20 MPa)", "Sedang (20-40 MPa)",
                              "Tinggi (40-60 MPa)", "Sangat tinggi (>60 MPa)")))

# ---- Tema bslib + CSS ------------------------------------------------
app_theme <- bs_theme(
  version = 5, bg = bg, fg = ink,
  primary = orange, secondary = blue, success = teal,
  info = blue, warning = yellow, danger = "#D64545",
  base_font    = font_collection("Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection("Trebuchet MS", "Segoe UI", "sans-serif")
)

css <- "
body{background:@bg@;}
.navbar{background:@nav@ !important;border-bottom:3px solid @orange@;}
.navbar-brand{font-weight:800;letter-spacing:1px;color:#fff !important;}
.navbar .nav-link{color:#CBD5E1 !important;font-weight:600;}
.navbar .nav-link.active{color:#fff !important;border-bottom:3px solid @orange@;}
.bslib-sidebar-layout > .sidebar{background:@surface@ !important;border-right:1px solid @edge@;}
.card{background:@surface@ !important;border:1px solid @edge@ !important;border-radius:14px;
      box-shadow:0 1px 3px rgba(31,41,55,.06);}
.card-header{background:transparent !important;border-bottom:1px solid @edge@;
             font-weight:700;color:@ink@;}
.kpi{background:@surface@;border:1px solid @edge@;border-radius:14px;padding:14px 18px;
     position:relative;overflow:hidden;box-shadow:0 1px 3px rgba(31,41,55,.06);}
.kpi:before{content:'';position:absolute;left:0;top:0;bottom:0;width:6px;background:var(--accent);}
.kpi-label{font-size:.7rem;letter-spacing:1.3px;text-transform:uppercase;color:@muted@;}
.kpi-value{font-size:1.75rem;font-weight:800;color:var(--accent);line-height:1.25;}
.chips{display:flex;flex-wrap:wrap;gap:8px;margin:6px 0;}
.chip{background:@bg@;border:1px solid @edge@;border-radius:999px;padding:3px 12px;font-size:.8rem;}
.dot{display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:6px;}
.note{color:@muted@;font-size:.85rem;}
.section-title{font-weight:800;color:@ink@;margin:4px 0 2px;}
.insight{background:#FFF4EC;border-left:5px solid @orange@;border-radius:8px;
         padding:10px 14px;margin-top:10px;font-size:.92rem;}
.author{font-size:.72rem;letter-spacing:2px;text-transform:uppercase;color:@muted@;}
.author-name{display:inline-block;background:@orange@;color:#fff;font-weight:700;
             border-radius:999px;padding:3px 14px;margin-top:4px;font-size:.85rem;}
.footer-box{margin-top:26px;padding:14px 20px;border:1px dashed @edge@;border-radius:14px;}
.table{--bs-table-bg:transparent;}
.btn-primary{color:#fff;font-weight:700;}
"
for (k in names(pal)) css <- gsub(paste0("@", k, "@"), pal[[k]], css, fixed = TRUE)

footer_box <- function() {
  div(class = "footer-box",
      div(class = "author", "Dashboard disusun oleh"),
      span(class = "author-name", pembuat))
}

# ---- UI --------------------------------------------------------------
ui <- page_navbar(
  title = "BETONLENS", id = "nav", theme = app_theme, fillable = FALSE,
  header = tags$head(tags$style(HTML(css))),

  sidebar = sidebar(
    width = 290, open = "desktop",
    h5("FILTER DATA", class = "section-title"),
    checkboxGroupInput("f_mix", "Tipe campuran", choices = levels(df$MixType),
                       selected = levels(df$MixType)),
    checkboxGroupInput("f_age", "Kelompok umur", choices = levels(df$AgeGroup),
                       selected = levels(df$AgeGroup)),
    sliderInput("f_str", "Kuat tekan (MPa)",
                min = floor(min(df$Strength)), max = ceiling(max(df$Strength)),
                value = c(floor(min(df$Strength)), ceiling(max(df$Strength)))),
    checkboxInput("f_dup", "Hapus baris duplikat", FALSE),
    actionButton("f_reset", "Reset filter", class = "btn-primary w-100"),
    hr(),
    div(class = "note", textOutput("f_info")),
    hr(),
    div(class = "author", "Disusun oleh"),
    span(class = "author-name", pembuat)
  ),

  # ===== RINGKASAN =====
  nav_panel(
    "Ringkasan",
    h3("Ringkasan Data Kuat Tekan Beton", class = "section-title"),
    p("Komposisi campuran beton dan kuat tekannya, sesuai filter di kiri.", class = "note"),
    uiOutput("kpis"), br(),
    layout_columns(
      col_widths = c(5, 7),
      card(card_header("Distribusi Kuat Tekan"), plotlyOutput("ov_hist", height = "300px")),
      card(card_header("Kurva Umur vs Rata-rata Kuat Tekan (sumbu umur log)"),
           plotlyOutput("ov_age", height = "300px"))
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(card_header("Tipe Campuran"), plotlyOutput("ov_mix", height = "280px")),
      card(card_header("Kuat Tekan per Tipe Campuran"), plotlyOutput("ov_mixbox", height = "280px")),
      card(card_header("Kualitas Data & Outlier (IQR)"), tableOutput("ov_quality"))
    ),
    footer_box()
  ),

  # ===== UNIVARIAT =====
  nav_panel(
    "Univariat",
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("Distribusi Variabel Numerik"),
           layout_columns(
             col_widths = c(4, 4, 4),
             selectInput("uni_var", "Variabel", num_vars, selected = "Strength"),
             selectInput("uni_type", "Jenis grafik",
                         c("Histogram", "Density", "Boxplot", "Violin", "QQ Plot")),
             sliderInput("uni_bins", "Jumlah bin", 5, 40, 20)),
           plotlyOutput("uni_plot", height = "360px")),
      card(card_header("Statistik Deskriptif & Normalitas"), tableOutput("uni_stats"))
    ),
    card(card_header("Variabel Kategorik"),
         layout_columns(
           col_widths = c(3, 9),
           div(selectInput("uni_cat", "Variabel", cat_vars),
               radioButtons("uni_cat_type", "Tampilan", c("Batang", "Donat"), inline = TRUE)),
           plotlyOutput("uni_cat_plot", height = "300px")))
  ),

  # ===== BIVARIAT =====
  nav_panel(
    "Bivariat",
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("Scatterplot & Garis Tren"),
           layout_columns(
             col_widths = c(3, 3, 3, 3),
             selectInput("bi_x", "Sumbu X", num_vars, selected = "Cement"),
             selectInput("bi_y", "Sumbu Y", num_vars, selected = "Strength"),
             selectInput("bi_color", "Warna", c("Tanpa warna" = "none", cat_vars)),
             selectInput("bi_fit", "Garis tren", c("Linear", "Lowess", "Tidak ada"))),
           plotlyOutput("bi_scatter", height = "380px")),
      card(card_header("Kekuatan Hubungan"), uiOutput("bi_cor"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Numerik vs Kategorik (Boxplot)"),
           layout_columns(
             col_widths = c(6, 6),
             selectInput("bi_num", "Variabel numerik", num_vars, selected = "Strength"),
             selectInput("bi_cat", "Kelompok", cat_vars, selected = "AgeGroup")),
           plotlyOutput("bi_box", height = "320px"),
           uiOutput("bi_test")),
      card(card_header("Kategorik vs Kategorik (Proporsi %)"),
           layout_columns(
             col_widths = c(6, 6),
             selectInput("bi_c1", "Kategori utama", cat_vars, selected = "MixType"),
             selectInput("bi_c2", "Dipecah menurut", cat_vars, selected = "StrengthClass")),
           plotlyOutput("bi_stack", height = "320px"))
    )
  ),

  # ===== MULTIVARIAT =====
  nav_panel(
    "Multivariat",
    navset_card_tab(
      id = "mv_tabs",

      nav_panel("Korelasi",
        layout_columns(
          col_widths = c(3, 9),
          div(checkboxGroupInput("cor_vars", "Variabel", num_vars,
                                 selected = unname(c(pred_vars, "Strength"))),
              radioButtons("cor_method", "Metode",
                           c("Pearson" = "pearson", "Spearman" = "spearman")),
              h6("Pasangan terkuat", class = "section-title"),
              tableOutput("cor_top")),
          plotlyOutput("cor_heat", height = "480px"))),

      nav_panel("Scatter Matrix",
        layout_columns(
          col_widths = c(3, 9),
          div(checkboxGroupInput("sp_vars", "Variabel (2-5)", num_vars,
                                 selected = c("Cement", "Water", "Age", "Strength")),
              selectInput("sp_color", "Warna", c("Tanpa warna" = "none", cat_vars),
                          selected = "MixType"),
              uiOutput("sp_legend")),
          plotlyOutput("sp_plot", height = "580px"))),

      nav_panel("Plot 3D",
        layout_columns(
          col_widths = c(3, 9),
          div(selectInput("d3_x", "Sumbu X", num_vars, selected = "Cement"),
              selectInput("d3_y", "Sumbu Y", num_vars, selected = "Water"),
              selectInput("d3_z", "Sumbu Z", num_vars, selected = "Strength"),
              selectInput("d3_color", "Warna", cat_vars, selected = "MixType")),
          plotlyOutput("d3_plot", height = "580px"))),

      nav_panel("Bubble 4D",
        layout_columns(
          col_widths = c(3, 9),
          div(selectInput("bb_x", "Sumbu X", num_vars, selected = "Cement"),
              selectInput("bb_y", "Sumbu Y", num_vars, selected = "Strength"),
              selectInput("bb_s", "Ukuran gelembung", num_vars, selected = "Age"),
              selectInput("bb_color", "Warna", cat_vars, selected = "MixType")),
          plotlyOutput("bb_plot", height = "540px"))),

      nav_panel("Parallel Coordinates",
        layout_columns(
          col_widths = c(3, 9),
          div(checkboxGroupInput("pc_vars", "Variabel", num_vars,
                                 selected = c("Cement", "Water", "SP", "Age", "Strength")),
              selectInput("pc_color", "Warna garis", cat_vars, selected = "StrengthClass"),
              uiOutput("pc_legend"),
              p("Seret pada sumbu untuk menyeleksi rentang nilai.", class = "note")),
          plotlyOutput("pc_plot", height = "540px"))),

      nav_panel("PCA",
        layout_columns(
          col_widths = c(3, 9),
          div(checkboxGroupInput("pca_vars", "Variabel", num_vars,
                                 selected = unname(c(pred_vars, "Strength"))),
              selectInput("pca_color", "Warna titik", cat_vars, selected = "StrengthClass"),
              p("Data distandarisasi (skala z) sebelum PCA.", class = "note")),
          div(layout_columns(
                col_widths = c(5, 7),
                plotlyOutput("pca_scree", height = "340px"),
                plotlyOutput("pca_bi", height = "340px")),
              layout_columns(
                col_widths = c(6, 6),
                div(h6("Nilai eigen", class = "section-title"), tableOutput("pca_eig")),
                div(h6("Loading komponen", class = "section-title"), tableOutput("pca_load")))))),

      nav_panel("Klaster (K-means)",
        layout_columns(
          col_widths = c(3, 9),
          div(checkboxGroupInput("km_vars", "Variabel pembentuk klaster", num_vars,
                                 selected = unname(pred_vars)),
              sliderInput("km_k", "Jumlah klaster (k)", 2, 6, 3, step = 1),
              uiOutput("km_legend"),
              p("Klaster dibentuk dari variabel terpilih (terstandarisasi); profil juga menampilkan rata-rata kuat tekan.",
                class = "note")),
          div(layout_columns(
                col_widths = c(7, 5),
                plotlyOutput("km_plot", height = "360px"),
                plotlyOutput("km_elbow", height = "360px")),
              h6("Profil klaster (rata-rata)", class = "section-title"),
              tableOutput("km_profile"))))
    )
  ),

  # ===== REGRESI =====
  nav_panel(
    "Regresi",
    navset_card_tab(
      id = "rg_tabs",

      nav_panel("Model",
        layout_columns(
          col_widths = c(3, 9),
          div(checkboxGroupInput("rg_vars", "Prediktor", pred_vars,
                                 selected = unname(pred_vars)),
              checkboxInput("rg_log", "Gunakan log(Age) untuk umur", TRUE),
              p("Respon: kuat tekan beton (MPa). Regresi linear berganda (OLS) pada data terfilter.",
                class = "note")),
          div(uiOutput("rg_kpis"), br(),
              h6("Koefisien regresi", class = "section-title"),
              tableOutput("rg_coef"),
              uiOutput("rg_interp")))),

      nav_panel("Diagnostik Asumsi",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Residual vs Fitted (linearitas & homoskedastisitas)"),
               plotlyOutput("rg_res", height = "300px")),
          card(card_header("Normal Q-Q Residual (normalitas)"),
               plotlyOutput("rg_qq", height = "300px"))),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Scale-Location"),
               plotlyOutput("rg_scale", height = "300px")),
          card(card_header("Cook's Distance (pengamat berpengaruh)"),
               plotlyOutput("rg_cook", height = "300px"))),
        card(card_header("Uji Asumsi"), tableOutput("rg_tests"))),

      nav_panel("Prediksi",
        layout_columns(
          col_widths = c(7, 5),
          div(h6("Atur komposisi campuran (kg/m3) dan umur (hari)", class = "section-title"),
              do.call(layout_columns, c(
                list(col_widths = c(6, 6)),
                lapply(unname(pred_vars), function(v) {
                  x <- df[[v]]
                  sliderInput(paste0("p_", v), lab(v),
                              min = floor(min(x)), max = ceiling(max(x)),
                              value = round(median(x)),
                              step = if (v == "SP") 0.1 else 1)
                })))),
          div(uiOutput("rg_pred"), br(),
              plotlyOutput("rg_pred_plot", height = "300px"),
              p("Prediksi memakai model yang dipilih di tab Model.", class = "note")))),

      nav_panel("Evaluasi Model",
        layout_columns(
          col_widths = c(3, 9),
          div(numericInput("rg_seed", "Seed acak", 123, min = 1, step = 1),
              sliderInput("rg_prop", "Proporsi data latih", 0.6, 0.9, 0.8, step = 0.05),
              selectInput("rg_eval_model", "Model untuk plot",
                          c("Linear (8 prediktor)" = "A",
                            "Linear + log(Age)" = "B",
                            "Model terpilih (tab Model)" = "C"), selected = "B")),
          div(h6("Perbandingan model pada data uji", class = "section-title"),
              tableOutput("rg_eval_tbl"),
              plotlyOutput("rg_eval_plot", height = "340px"))))
    )
  ),

  # ===== DATA =====
  nav_panel(
    "Data",
    card(card_header("Tabel Data (sesuai filter)"),
         DT::DTOutput("tbl"),
         downloadButton("dl", "Unduh CSV", class = "btn-primary mt-2")),
    footer_box()
  )
)

# ---- SERVER ----------------------------------------------------------
server <- function(input, output, session) {

  dat <- reactive({
    d <- df %>%
      filter(MixType %in% input$f_mix, AgeGroup %in% input$f_age,
             Strength >= input$f_str[1], Strength <= input$f_str[2])
    if (isTRUE(input$f_dup)) d <- d %>% filter(!Dup)
    validate(need(nrow(d) > 0, "Tidak ada data pada filter ini. Longgarkan filter di kiri."))
    d
  })

  output$f_info <- renderText({
    d <- df %>% filter(MixType %in% input$f_mix, AgeGroup %in% input$f_age,
                       Strength >= input$f_str[1], Strength <= input$f_str[2])
    if (isTRUE(input$f_dup)) d <- d %>% filter(!Dup)
    paste0(nrow(d), " dari ", nrow(df), " observasi terpilih")
  })

  observeEvent(input$f_reset, {
    updateCheckboxGroupInput(session, "f_mix", selected = levels(df$MixType))
    updateCheckboxGroupInput(session, "f_age", selected = levels(df$AgeGroup))
    updateSliderInput(session, "f_str", value = c(floor(min(df$Strength)), ceiling(max(df$Strength))))
    updateCheckboxInput(session, "f_dup", value = FALSE)
  })

  count_df <- function(d, v) { t <- table(d[[v]]); data.frame(k = names(t), n = as.integer(t)) }
  grp <- function(d, v) if (v == "none") factor(rep("Semua", nrow(d))) else droplevels(d[[v]])

  # ======================= RINGKASAN =======================
  output$kpis <- renderUI({
    d <- dat()
    layout_columns(
      col_widths = c(2, 3, 3, 2, 2),
      kpi("Observasi",            fmt(nrow(d)), blue),
      kpi("Rata-rata kuat tekan", paste(fmt(mean(d$Strength), 1), "MPa"), orange),
      kpi("Kuat tekan maks.",     paste(fmt(max(d$Strength), 1), "MPa"), teal),
      kpi("Rasio air/semen",      fmt(mean(d$WC), 2), purple),
      kpi("Mutu tinggi",          paste0(fmt(mean(d$Strength >= 40) * 100, 1), "%"), yellow)
    )
  })

  output$ov_hist <- renderPlotly({
    d <- dat()
    plot_ly(x = d$Strength, type = "histogram", nbinsx = 25,
            marker = list(color = orange, line = list(color = surface, width = 1))) %>%
      style_plotly() %>%
      layout(xaxis = list(title = "Kuat tekan (MPa)"), yaxis = list(title = "Frekuensi"),
             shapes = list(list(type = "line", x0 = mean(d$Strength), x1 = mean(d$Strength),
                                y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = blue, dash = "dash", width = 2))))
  })

  output$ov_age <- renderPlotly({
    d <- dat() %>% group_by(Age) %>% summarise(m = mean(Strength), n = n(), .groups = "drop")
    plot_ly(d, x = ~Age, y = ~m, type = "scatter", mode = "lines+markers",
            line = list(color = blue, width = 3),
            marker = list(color = orange, size = 9, line = list(color = surface, width = 1)),
            text = ~paste0("n = ", n), hovertemplate = "Umur %{x} hari<br>Rata-rata %{y:.1f} MPa<br>%{text}<extra></extra>") %>%
      style_plotly() %>%
      layout(xaxis = list(title = "Umur (hari, skala log)", type = "log"),
             yaxis = list(title = "Rata-rata kuat tekan (MPa)"))
  })

  output$ov_mix <- renderPlotly({
    cd <- count_df(dat(), "MixType")
    plot_ly(cd, labels = ~k, values = ~n, type = "pie", hole = 0.55,
            marker = list(colors = cols[1:nrow(cd)], line = list(color = surface, width = 3)),
            textinfo = "percent") %>%
      base_plotly() %>% layout(legend = list(orientation = "h", y = -0.1))
  })

  output$ov_mixbox <- renderPlotly({
    d <- dat(); g <- droplevels(d$MixType); gc <- group_colors(levels(g))
    p <- plot_ly()
    for (lv in levels(g))
      p <- add_trace(p, y = d$Strength[g == lv], type = "box", name = lv,
                     marker = list(color = gc[[lv]]), line = list(color = gc[[lv]]))
    style_plotly(p) %>% layout(showlegend = FALSE, yaxis = list(title = "MPa"),
                               xaxis = list(showticklabels = FALSE))
  })

  output$ov_quality <- renderTable({
    d <- dat(); v <- unname(c(pred_vars, "Strength"))
    data.frame(Variabel = lab(v),
               Missing = sapply(v, function(z) sum(is.na(d[[z]]))),
               Outlier = sapply(v, function(z) length(boxplot.stats(d[[z]])$out)),
               row.names = NULL)
  }, width = "100%")

  # ======================= UNIVARIAT =======================
  output$uni_plot <- renderPlotly({
    d <- dat(); v <- input$uni_var; x <- d[[v]]
    validate(need(length(x) > 3, "Data terlalu sedikit."),
             need(length(unique(x)) > 1, "Nilai variabel konstan."))
    p <- switch(input$uni_type,
      "Histogram" = plot_ly(x = x, type = "histogram", nbinsx = input$uni_bins,
                            marker = list(color = orange, line = list(color = surface, width = 1))),
      "Density" = { dn <- density(x)
        plot_ly(x = dn$x, y = dn$y, type = "scatter", mode = "lines", fill = "tozeroy",
                line = list(color = orange, width = 3), fillcolor = "rgba(242,107,29,.25)") },
      "Boxplot" = plot_ly(x = x, type = "box", boxpoints = "all", jitter = 0.4, pointpos = 0,
                          name = "", marker = list(color = blue, opacity = .6),
                          line = list(color = orange)),
      "Violin" = plot_ly(x = x, type = "violin", name = "", points = "all",
                         box = list(visible = TRUE), meanline = list(visible = TRUE),
                         line = list(color = purple), fillcolor = "rgba(124,92,250,.3)",
                         marker = list(color = orange, opacity = .5)),
      "QQ Plot" = { qq <- qqnorm(x, plot.it = FALSE)
        qs <- quantile(x, c(.25, .75)); th <- qnorm(c(.25, .75))
        sl <- diff(qs) / diff(th); ic <- qs[[1]] - sl * th[1]
        plot_ly() %>%
          add_trace(x = qq$x, y = qq$y, type = "scatter", mode = "markers",
                    marker = list(color = blue, size = 7, opacity = .7)) %>%
          add_trace(x = range(qq$x), y = ic + sl * range(qq$x), type = "scatter",
                    mode = "lines", line = list(color = orange, width = 3)) }
    )
    qq_on <- input$uni_type == "QQ Plot"
    style_plotly(p) %>%
      layout(xaxis = list(title = if (qq_on) "Kuantil teoretis" else lab(v)),
             yaxis = list(title = if (qq_on) "Kuantil sampel" else if (input$uni_type == "Histogram") "Frekuensi" else ""),
             showlegend = FALSE)
  })

  output$uni_stats <- renderTable({
    x <- dat()[[input$uni_var]]; s <- describe(x)
    out <- data.frame(Statistik = names(s),
                      Nilai = sapply(seq_along(s), function(i)
                        if (names(s)[i] %in% c("N", "Outlier")) fmt(s[[i]]) else fmt(s[[i]], 2)))
    if (length(x) >= 3 && length(x) <= 5000 && length(unique(x)) > 1) {
      sw <- shapiro.test(x)
      out <- rbind(out, data.frame(Statistik = c("Shapiro-Wilk W", "Shapiro-Wilk p"),
                                   Nilai = c(fmt(sw$statistic, 3), pfmt(sw$p.value))))
    }
    out
  }, width = "100%")

  output$uni_cat_plot <- renderPlotly({
    d <- dat(); v <- input$uni_cat
    cd <- count_df(d, v); cd$k <- factor(cd$k, levels = levels(d[[v]]))
    if (input$uni_cat_type == "Donat") {
      plot_ly(cd, labels = ~k, values = ~n, type = "pie", hole = 0.55,
              marker = list(colors = cols[1:nrow(cd)], line = list(color = surface, width = 3)),
              textinfo = "label+percent+value") %>% base_plotly()
    } else {
      plot_ly(cd, x = ~k, y = ~n, type = "bar", text = ~n, textposition = "outside",
              marker = list(color = cols[1:nrow(cd)])) %>%
        style_plotly() %>% layout(xaxis = list(title = ""), yaxis = list(title = "Jumlah"))
    }
  })

  # ======================= BIVARIAT =======================
  output$bi_scatter <- renderPlotly({
    d <- dat(); x <- input$bi_x; y <- input$bi_y
    validate(need(x != y, "Pilih dua variabel yang berbeda."))
    g <- grp(d, input$bi_color); gc <- group_colors(levels(g))
    p <- plot_ly()
    for (lv in levels(g)) {
      i <- g == lv
      p <- add_trace(p, x = d[[x]][i], y = d[[y]][i], type = "scatter", mode = "markers", name = lv,
                     marker = list(color = if (input$bi_color == "none") blue else gc[[lv]],
                                   size = 8, opacity = .7, line = list(color = surface, width = 1)))
    }
    if (input$bi_fit == "Linear" && nrow(d) > 2) {
      fit <- lm(d[[y]] ~ d[[x]]); o <- order(d[[x]])
      p <- add_trace(p, x = d[[x]][o], y = fitted(fit)[o], type = "scatter", mode = "lines",
                     name = "Regresi linear", line = list(color = ink, dash = "dash", width = 3))
    } else if (input$bi_fit == "Lowess" && nrow(d) > 5) {
      lw <- lowess(d[[x]], d[[y]])
      p <- add_trace(p, x = lw$x, y = lw$y, type = "scatter", mode = "lines",
                     name = "Lowess", line = list(color = ink, width = 3))
    }
    style_plotly(p) %>% layout(xaxis = list(title = lab(x)), yaxis = list(title = lab(y)))
  })

  output$bi_cor <- renderUI({
    d <- dat(); x <- d[[input$bi_x]]; y <- d[[input$bi_y]]
    validate(need(input$bi_x != input$bi_y, "Pilih dua variabel yang berbeda."),
             need(length(x) > 3, "Data terlalu sedikit."),
             need(sd(x) > 0 && sd(y) > 0, "Salah satu variabel konstan."))
    pe <- cor.test(x, y, method = "pearson")
    sp <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
    str_lbl <- function(r) { a <- abs(r)
      if (a < .2) "sangat lemah" else if (a < .4) "lemah" else
        if (a < .6) "sedang" else if (a < .8) "kuat" else "sangat kuat" }
    tagList(
      kpi("Pearson r", fmt(pe$estimate, 3), orange), br(),
      kpi("Spearman rho", fmt(sp$estimate, 3), blue), br(),
      p(class = "note",
        sprintf("Hubungan %s (%s). p-value Pearson = %s, Spearman = %s.",
                str_lbl(pe$estimate), if (pe$estimate >= 0) "positif" else "negatif",
                pfmt(pe$p.value), pfmt(sp$p.value))))
  })

  output$bi_box <- renderPlotly({
    d <- dat(); v <- input$bi_num; g <- droplevels(d[[input$bi_cat]]); gc <- group_colors(levels(g))
    p <- plot_ly()
    for (lv in levels(g))
      p <- add_trace(p, y = d[[v]][g == lv], type = "box", name = lv, boxpoints = "all",
                     jitter = .4, pointpos = 0, marker = list(color = gc[[lv]], opacity = .55),
                     line = list(color = gc[[lv]]))
    style_plotly(p) %>% layout(showlegend = FALSE, yaxis = list(title = lab(v)))
  })

  output$bi_test <- renderUI({
    d <- dat(); g <- droplevels(d[[input$bi_cat]])
    validate(need(nlevels(g) >= 2, "Butuh minimal 2 kelompok."))
    kw <- kruskal.test(d[[input$bi_num]] ~ g)
    p(class = "insight", sprintf("Uji Kruskal-Wallis: chi-kuadrat = %s, p = %s. %s",
        fmt(kw$statistic, 2), pfmt(kw$p.value),
        if (kw$p.value < 0.05) "Ada perbedaan nyata antar kelompok (alpha 5%)."
        else "Tidak ada bukti perbedaan antar kelompok (alpha 5%)."))
  })

  output$bi_stack <- renderPlotly({
    d <- dat(); a <- input$bi_c1; b <- input$bi_c2
    validate(need(a != b, "Pilih dua variabel kategorik yang berbeda."))
    tb <- as.data.frame(table(d[[a]], d[[b]])); names(tb) <- c("A", "B", "n")
    tb$pct <- ave(tb$n, tb$A, FUN = function(z) if (sum(z) == 0) 0 else z / sum(z) * 100)
    gc <- group_colors(levels(tb$B)); p <- plot_ly()
    for (lv in levels(tb$B)) {
      s <- tb[tb$B == lv, ]
      p <- add_trace(p, x = s$A, y = s$pct, type = "bar", name = lv,
                     text = paste0(round(s$pct), "%"), marker = list(color = gc[[lv]]))
    }
    style_plotly(p) %>% layout(barmode = "stack", xaxis = list(title = ""),
                               yaxis = list(title = "Persentase (%)", range = c(0, 100)))
  })

  # ======================= MULTIVARIAT =======================
  cor_mat <- reactive({
    v <- input$cor_vars
    validate(need(length(v) >= 2, "Pilih minimal 2 variabel."))
    d <- dat(); validate(need(nrow(d) > 3, "Data terlalu sedikit."))
    X <- d[v]; validate(need(all(sapply(X, sd) > 0), "Ada variabel yang konstan."))
    cor(X, method = input$cor_method)
  })

  output$cor_heat <- renderPlotly({
    cm <- cor_mat(); nm <- lab(colnames(cm)); n <- length(nm)
    ann <- lapply(seq_len(n * n), function(k) {
      i <- (k - 1) %% n + 1; j <- (k - 1) %/% n + 1
      list(x = nm[i], y = nm[j], text = sprintf("%.2f", cm[j, i]), showarrow = FALSE,
           font = list(color = if (abs(cm[j, i]) > .55) "#fff" else ink, size = 12))
    })
    plot_ly(x = nm, y = nm, z = cm, type = "heatmap", zmin = -1, zmax = 1, xgap = 3, ygap = 3,
            colorscale = list(list(0, blue), list(0.5, "#FFFFFF"), list(1, orange)),
            colorbar = list(title = "r")) %>%
      base_plotly() %>%
      layout(annotations = ann, xaxis = list(title = "", tickangle = -35),
             yaxis = list(title = "", autorange = "reversed"),
             margin = list(l = 150, b = 130))
  })

  output$cor_top <- renderTable({
    cm <- cor_mat(); ix <- which(upper.tri(cm), arr.ind = TRUE)
    out <- data.frame(Pasangan = paste(colnames(cm)[ix[, 1]], "-", colnames(cm)[ix[, 2]]), r = cm[ix])
    out <- out[order(-abs(out$r)), ][seq_len(min(5, nrow(out))), ]
    out$r <- fmt(out$r, 2); out
  }, width = "100%")

  output$sp_legend <- renderUI({
    if (input$sp_color == "none") return(NULL)
    g <- grp(dat(), input$sp_color); legend_chips(levels(g), group_colors(levels(g)))
  })

  output$sp_plot <- renderPlotly({
    d <- dat(); v <- input$sp_vars
    validate(need(length(v) >= 2 && length(v) <= 5, "Pilih 2 sampai 5 variabel."))
    g <- grp(d, input$sp_color); gc <- group_colors(levels(g)); p <- plot_ly()
    for (lv in levels(g)) {
      dd <- d[g == lv, , drop = FALSE]; if (nrow(dd) == 0) next
      dims <- lapply(v, function(z) list(label = lab(z), values = dd[[z]]))
      p <- add_trace(p, type = "splom", dimensions = dims, name = lv,
                     diagonal = list(visible = FALSE), showupperhalf = FALSE,
                     marker = list(color = if (input$sp_color == "none") blue else gc[[lv]],
                                   size = 5, opacity = .7, line = list(color = surface, width = .5)))
    }
    n <- length(v)
    ax <- setNames(rep(list(list(gridcolor = grid_col, zerolinecolor = edge, tickfont = list(size = 9))), 2 * n),
                   c(paste0("xaxis", c("", seq_len(n)[-1])), paste0("yaxis", c("", seq_len(n)[-1]))))
    p <- do.call(layout, c(list(base_plotly(p)), ax))
    layout(p, showlegend = FALSE)
  })

  output$d3_plot <- renderPlotly({
    d <- dat(); vx <- input$d3_x; vy <- input$d3_y; vz <- input$d3_z
    validate(need(length(unique(c(vx, vy, vz))) == 3, "Pilih tiga variabel yang berbeda."))
    g <- droplevels(d[[input$d3_color]]); gc <- group_colors(levels(g)); p <- plot_ly()
    for (lv in levels(g)) {
      i <- g == lv
      p <- add_trace(p, x = d[[vx]][i], y = d[[vy]][i], z = d[[vz]][i], type = "scatter3d",
                     mode = "markers", name = lv,
                     marker = list(size = 4, color = gc[[lv]], opacity = .8,
                                   line = list(color = surface, width = 1)))
    }
    ax <- function(v) list(title = lab(v), gridcolor = edge, color = ink)
    base_plotly(p) %>%
      layout(scene = list(xaxis = ax(vx), yaxis = ax(vy), zaxis = ax(vz)),
             margin = list(l = 0, r = 0, b = 0, t = 0))
  })

  output$bb_plot <- renderPlotly({
    d <- dat(); x <- input$bb_x; y <- input$bb_y; s <- input$bb_s
    validate(need(x != y, "Sumbu X dan Y harus berbeda."))
    sv <- d[[s]]; rg <- diff(range(sv))
    size <- if (rg == 0) rep(18, length(sv)) else 6 + 34 * (sv - min(sv)) / rg
    g <- droplevels(d[[input$bb_color]]); gc <- group_colors(levels(g)); p <- plot_ly()
    for (lv in levels(g)) {
      i <- g == lv
      p <- add_trace(p, x = d[[x]][i], y = d[[y]][i], type = "scatter", mode = "markers", name = lv,
                     text = paste0(lab(s), ": ", fmt(sv[i], 2)),
                     hovertemplate = "%{x}, %{y}<br>%{text}<extra></extra>",
                     marker = list(size = size[i], color = gc[[lv]], opacity = .55,
                                   line = list(color = surface, width = 1)))
    }
    style_plotly(p) %>% layout(xaxis = list(title = lab(x)), yaxis = list(title = lab(y)))
  })

  output$pc_legend <- renderUI({
    g <- droplevels(dat()[[input$pc_color]]); legend_chips(levels(g), group_colors(levels(g)))
  })

  output$pc_plot <- renderPlotly({
    d <- dat(); v <- input$pc_vars
    validate(need(length(v) >= 2, "Pilih minimal 2 variabel."))
    g <- droplevels(d[[input$pc_color]]); k <- nlevels(g); kk <- max(k, 2); gc <- rep_len(cols, kk)
    stops <- unlist(lapply(seq_len(kk), function(i)
      list(list((i - 1) / kk, gc[i]), list(i / kk, gc[i]))), recursive = FALSE)
    dims <- lapply(v, function(z) list(label = lab(z), values = d[[z]]))
    plot_ly(type = "parcoords",
            line = list(color = as.integer(g), colorscale = stops, cmin = 1, cmax = kk, showscale = FALSE),
            dimensions = dims, labelfont = list(color = ink), tickfont = list(color = muted),
            rangefont = list(color = muted)) %>%
      base_plotly() %>% layout(margin = list(l = 60, r = 40, t = 60))
  })

  pca_res <- reactive({
    v <- input$pca_vars
    validate(need(length(v) >= 2, "Pilih minimal 2 variabel."))
    d <- dat(); validate(need(nrow(d) >= 5, "Minimal 5 observasi untuk PCA."))
    X <- d[v]; validate(need(all(sapply(X, sd) > 0), "Ada variabel yang konstan."))
    prcomp(X, scale. = TRUE)
  })

  output$pca_scree <- renderPlotly({
    pc <- pca_res(); pct <- pc$sdev^2 / sum(pc$sdev^2) * 100; nm <- paste0("PC", seq_along(pct))
    plot_ly() %>%
      add_trace(x = nm, y = pct, type = "bar", name = "% varians", marker = list(color = blue)) %>%
      add_trace(x = nm, y = cumsum(pct), type = "scatter", mode = "lines+markers", name = "Kumulatif",
                line = list(color = orange, width = 3), marker = list(color = orange, size = 8)) %>%
      style_plotly() %>%
      layout(title = list(text = "Scree plot", font = list(size = 14)),
             yaxis = list(title = "% varians", range = c(0, 105)),
             xaxis = list(categoryorder = "array", categoryarray = nm),
             legend = list(orientation = "h", y = -0.15))
  })

  output$pca_bi <- renderPlotly({
    pc <- pca_res(); d <- dat()
    sc <- pc$x[, 1:2, drop = FALSE]; ld <- pc$rotation[, 1:2, drop = FALSE]
    k <- 0.8 * max(abs(sc)) / max(abs(ld))
    g <- droplevels(d[[input$pca_color]]); gc <- group_colors(levels(g)); p <- plot_ly()
    for (lv in levels(g)) {
      i <- g == lv
      p <- add_trace(p, x = sc[i, 1], y = sc[i, 2], type = "scatter", mode = "markers", name = lv,
                     marker = list(color = gc[[lv]], size = 7, opacity = .7,
                                   line = list(color = surface, width = 1)))
    }
    ann <- lapply(seq_len(nrow(ld)), function(i)
      list(x = ld[i, 1] * k, y = ld[i, 2] * k, ax = 0, ay = 0, xref = "x", yref = "y",
           axref = "x", ayref = "y", showarrow = TRUE, arrowhead = 2, arrowcolor = ink, arrowwidth = 2,
           text = paste0("<b>", rownames(ld)[i], "</b>"), font = list(color = ink, size = 12)))
    pct <- pc$sdev^2 / sum(pc$sdev^2) * 100
    style_plotly(p) %>%
      layout(title = list(text = "Biplot PC1 vs PC2", font = list(size = 14)), annotations = ann,
             xaxis = list(title = sprintf("PC1 (%.1f%%)", pct[1])),
             yaxis = list(title = sprintf("PC2 (%.1f%%)", pct[2])),
             legend = list(orientation = "h", y = -0.2))
  })

  output$pca_eig <- renderTable({
    pc <- pca_res(); ev <- pc$sdev^2; pct <- ev / sum(ev) * 100
    data.frame(Komponen = paste0("PC", seq_along(ev)), Eigen = fmt(ev, 3),
               `Varians (%)` = fmt(pct, 1), `Kumulatif (%)` = fmt(cumsum(pct), 1), check.names = FALSE)
  }, width = "100%")

  output$pca_load <- renderTable({
    pc <- pca_res(); r <- pc$rotation[, 1:min(3, ncol(pc$rotation)), drop = FALSE]
    data.frame(Variabel = rownames(r), round(r, 3), check.names = FALSE)
  }, width = "100%", digits = 3)

  km_data <- reactive({
    v <- input$km_vars
    validate(need(length(v) >= 2, "Pilih minimal 2 variabel."))
    d <- dat(); validate(need(nrow(d) > input$km_k + 1, "Observasi terlalu sedikit untuk k ini."))
    X <- d[v]; validate(need(all(sapply(X, sd) > 0), "Ada variabel yang konstan."))
    list(d = d, v = v, Z = scale(as.matrix(X)))
  })
  km_res <- reactive({ kd <- km_data(); set.seed(2026); kmeans(kd$Z, centers = input$km_k, nstart = 25) })

  output$km_legend <- renderUI({
    lv <- paste("Klaster", seq_len(input$km_k)); legend_chips(lv, group_colors(lv))
  })

  output$km_plot <- renderPlotly({
    kd <- km_data(); km <- km_res(); pc <- prcomp(kd$Z); sc <- pc$x[, 1:2, drop = FALSE]
    lv <- paste("Klaster", seq_len(input$km_k)); gc <- group_colors(lv); p <- plot_ly()
    for (i in seq_along(lv)) {
      j <- km$cluster == i
      p <- add_trace(p, x = sc[j, 1], y = sc[j, 2], type = "scatter", mode = "markers", name = lv[i],
                     marker = list(color = gc[[lv[i]]], size = 8, opacity = .75,
                                   line = list(color = surface, width = 1)))
    }
    pct <- pc$sdev^2 / sum(pc$sdev^2) * 100
    style_plotly(p) %>%
      layout(title = list(text = "Klaster pada ruang PCA", font = list(size = 14)),
             xaxis = list(title = sprintf("PC1 (%.1f%%)", pct[1])),
             yaxis = list(title = sprintf("PC2 (%.1f%%)", pct[2])), showlegend = FALSE)
  })

  output$km_elbow <- renderPlotly({
    kd <- km_data(); kmax <- min(8, nrow(kd$Z) - 1); set.seed(2026)
    w <- sapply(1:kmax, function(k) kmeans(kd$Z, k, nstart = 10)$tot.withinss)
    plot_ly(x = 1:kmax, y = w, type = "scatter", mode = "lines+markers",
            line = list(color = blue, width = 3), marker = list(color = blue, size = 9)) %>%
      add_trace(x = input$km_k, y = w[input$km_k], type = "scatter", mode = "markers",
                marker = list(color = orange, size = 16), showlegend = FALSE) %>%
      style_plotly() %>%
      layout(title = list(text = "Elbow method", font = list(size = 14)),
             xaxis = list(title = "k", dtick = 1), yaxis = list(title = "Total within-SS"),
             showlegend = FALSE)
  })

  output$km_profile <- renderTable({
    kd <- km_data(); km <- km_res(); vv <- unique(c(kd$v, "Strength"))
    kd$d %>% mutate(Klaster = paste("Klaster", km$cluster)) %>% group_by(Klaster) %>%
      summarise(N = n(), across(all_of(vv), ~ round(mean(.x), 2)), .groups = "drop") %>%
      rename_with(~ lab(.x), all_of(vv))
  }, width = "100%", digits = 2)

  # ======================= REGRESI =======================
  fit <- reactive({
    v <- input$rg_vars
    validate(need(length(v) >= 1, "Pilih minimal 1 prediktor."))
    d <- dat()
    validate(need(nrow(d) >= length(v) + 10, "Data terlalu sedikit untuk model ini."))
    m <- lm(make_formula(v, isTRUE(input$rg_log)), data = d)
    validate(need(!anyNA(coef(m)),
                  "Ada prediktor konstan/kolinear sempurna pada filter ini. Kurangi prediktor atau longgarkan filter."))
    m
  })

  output$rg_kpis <- renderUI({
    m <- fit(); s <- summary(m); fs <- s$fstatistic
    fp <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      kpi("R-kuadrat", fmt(s$r.squared, 3), orange),
      kpi("R-kuadrat adj.", fmt(s$adj.r.squared, 3), blue),
      kpi("RMSE (MPa)", fmt(sqrt(mean(resid(m)^2)), 2), teal),
      kpi("Uji F (p)", pfmt(fp), purple))
  })

  output$rg_coef <- renderTable({
    m <- fit(); s <- summary(m)$coefficients
    mm <- model.matrix(m)[, -1, drop = FALSE]; y <- model.response(model.frame(m))
    beta <- c(NA, coef(m)[-1] * apply(mm, 2, sd) / sd(y))
    vf <- c(NA, vif_fit(m)[colnames(mm)])
    nm <- rownames(s); nm <- gsub("log\\(Age\\)", "log(Umur)", nm)
    nm <- ifelse(nm == "(Intercept)", "Konstanta", nm)
    data.frame(Variabel = nm, Koefisien = fmt(s[, 1], 4), `Std. Error` = fmt(s[, 2], 4),
               `t hitung` = fmt(s[, 3], 2), `p-value` = pfmt(s[, 4]),
               `Beta std.` = ifelse(is.na(beta), "-", fmt(beta, 3)),
               VIF = ifelse(is.na(vf), "-", fmt(vf, 2)), check.names = FALSE)
  }, width = "100%")

  output$rg_interp <- renderUI({
    m <- fit(); s <- summary(m)
    mm <- model.matrix(m)[, -1, drop = FALSE]; y <- model.response(model.frame(m))
    beta <- coef(m)[-1] * apply(mm, 2, sd) / sd(y)
    top <- names(beta)[which.max(abs(beta))]
    vf <- suppressWarnings(max(vif_fit(m), na.rm = TRUE))
    p(class = "insight", HTML(sprintf(
      "Model menjelaskan <b>%s%%</b> variasi kuat tekan beton (R&sup2; adj. %s). Prediktor paling dominan menurut koefisien terstandar: <b>%s</b>. %s",
      fmt(s$r.squared * 100, 1), fmt(s$adj.r.squared, 3),
      gsub("log\\(Age\\)", "log(Umur)", top),
      if (is.finite(vf) && vf > 10) sprintf("VIF maksimum %s (> 10) menunjukkan multikolinearitas serius.", fmt(vf, 1))
      else if (is.finite(vf) && vf > 5) sprintf("VIF maksimum %s (> 5) menunjukkan multikolinearitas sedang; komponen campuran saling terkait.", fmt(vf, 1))
      else "Tidak ada indikasi multikolinearitas berarti (VIF < 5).")))
  })

  diag_df <- reactive({
    m <- fit()
    data.frame(fitted = fitted(m), res = resid(m), sres = rstandard(m),
               cook = cooks.distance(m), idx = seq_along(resid(m)))
  })

  output$rg_res <- renderPlotly({
    z <- diag_df(); lw <- lowess(z$fitted, z$res)
    plot_ly() %>%
      add_trace(x = z$fitted, y = z$res, type = "scatter", mode = "markers",
                marker = list(color = blue, size = 6, opacity = .55)) %>%
      add_trace(x = lw$x, y = lw$y, type = "scatter", mode = "lines", line = list(color = orange, width = 3)) %>%
      add_trace(x = range(z$fitted), y = c(0, 0), type = "scatter", mode = "lines",
                line = list(color = ink, dash = "dash", width = 1.5)) %>%
      style_plotly() %>% layout(showlegend = FALSE, xaxis = list(title = "Nilai fitted"),
                                yaxis = list(title = "Residual"))
  })

  output$rg_qq <- renderPlotly({
    z <- diag_df(); qq <- qqnorm(z$sres, plot.it = FALSE)
    plot_ly() %>%
      add_trace(x = qq$x, y = qq$y, type = "scatter", mode = "markers",
                marker = list(color = blue, size = 6, opacity = .55)) %>%
      add_trace(x = range(qq$x), y = range(qq$x), type = "scatter", mode = "lines",
                line = list(color = orange, width = 3)) %>%
      style_plotly() %>% layout(showlegend = FALSE, xaxis = list(title = "Kuantil teoretis"),
                                yaxis = list(title = "Residual terstandar"))
  })

  output$rg_scale <- renderPlotly({
    z <- diag_df(); y <- sqrt(abs(z$sres)); lw <- lowess(z$fitted, y)
    plot_ly() %>%
      add_trace(x = z$fitted, y = y, type = "scatter", mode = "markers",
                marker = list(color = blue, size = 6, opacity = .55)) %>%
      add_trace(x = lw$x, y = lw$y, type = "scatter", mode = "lines", line = list(color = orange, width = 3)) %>%
      style_plotly() %>% layout(showlegend = FALSE, xaxis = list(title = "Nilai fitted"),
                                yaxis = list(title = "Akar |residual terstandar|"))
  })

  output$rg_cook <- renderPlotly({
    z <- diag_df(); thr <- 4 / nrow(z)
    plot_ly() %>%
      add_trace(x = z$idx, y = z$cook, type = "bar",
                marker = list(color = ifelse(z$cook > thr, orange, blue))) %>%
      add_trace(x = range(z$idx), y = c(thr, thr), type = "scatter", mode = "lines",
                line = list(color = ink, dash = "dash", width = 1.5)) %>%
      style_plotly() %>% layout(showlegend = FALSE, xaxis = list(title = "Indeks observasi"),
                                yaxis = list(title = "Cook's distance"))
  })

  output$rg_tests <- renderTable({
    m <- fit(); z <- diag_df(); n <- nrow(z)
    sw <- if (n <= 5000) shapiro.test(resid(m)) else NULL
    bp <- bp_test(m); dw <- dw_stat(m); vf <- suppressWarnings(max(vif_fit(m), na.rm = TRUE))
    nc <- sum(z$cook > 4 / n)
    data.frame(
      Asumsi = c("Normalitas residual", "Homoskedastisitas", "Autokorelasi",
                 "Multikolinearitas", "Pengamat berpengaruh"),
      Uji = c("Shapiro-Wilk", "Breusch-Pagan (Koenker)", "Statistik Durbin-Watson",
              "VIF maksimum", "Cook's D > 4/n"),
      Statistik = c(if (!is.null(sw)) fmt(sw$statistic, 3) else "-", fmt(bp[["stat"]], 2),
                    fmt(dw, 3), if (is.finite(vf)) fmt(vf, 2) else "-", paste(nc, "observasi")),
      `p-value` = c(if (!is.null(sw)) pfmt(sw$p.value) else "-", pfmt(bp[["p"]]), "-", "-", "-"),
      Kesimpulan = c(
        if (is.null(sw)) "-" else if (sw$p.value < .05) "Residual tidak normal" else "Residual normal",
        if (bp[["p"]] < .05) "Ragam residual tidak konstan" else "Ragam residual konstan",
        if (dw < 1.5) "Indikasi autokorelasi positif" else if (dw > 2.5) "Indikasi autokorelasi negatif" else "Tidak ada autokorelasi",
        if (!is.finite(vf)) "-" else if (vf > 10) "Multikolinearitas serius" else if (vf > 5) "Multikolinearitas sedang" else "Aman",
        if (nc / n > 0.05) "Cukup banyak, perlu dicek" else "Jumlah wajar"),
      check.names = FALSE)
  }, width = "100%")

  rg_newdata <- reactive({
    vals <- lapply(unname(pred_vars), function(v) input[[paste0("p_", v)]])
    req(!any(sapply(vals, is.null)))
    as.data.frame(setNames(vals, unname(pred_vars)))
  })

  output$rg_pred <- renderUI({
    pr <- predict(fit(), rg_newdata(), interval = "prediction", level = 0.95)
    tagList(
      kpi("Prediksi kuat tekan", paste(fmt(pr[1, "fit"], 1), "MPa"), orange), br(),
      kpi("Rentang prediksi 95%", paste0(fmt(pr[1, "lwr"], 1), " - ", fmt(pr[1, "upr"], 1), " MPa"), blue), br(),
      kpi("Kelas kekuatan", class_label(pr[1, "fit"]), teal))
  })

  output$rg_pred_plot <- renderPlotly({
    pr <- predict(fit(), rg_newdata())[[1]]; d <- dat()
    plot_ly(x = d$Strength, type = "histogram", nbinsx = 25,
            marker = list(color = "rgba(47,111,222,.45)", line = list(color = surface, width = 1))) %>%
      style_plotly() %>%
      layout(xaxis = list(title = "Kuat tekan (MPa)"), yaxis = list(title = "Frekuensi"),
             shapes = list(list(type = "line", x0 = pr, x1 = pr, y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = orange, width = 4))),
             annotations = list(list(x = pr, y = 1, yref = "paper", text = "Prediksi", showarrow = FALSE,
                                     yshift = 10, font = list(color = orange, size = 13))))
  })

  rg_split <- reactive({
    req(input$rg_seed, input$rg_prop)
    d <- dat(); validate(need(nrow(d) >= 40, "Minimal 40 observasi untuk evaluasi train/test."))
    set.seed(input$rg_seed)
    idx <- sample(seq_len(nrow(d)), size = floor(input$rg_prop * nrow(d)))
    list(tr = d[idx, ], te = d[-idx, ])
  })

  rg_models <- reactive({
    v <- input$rg_vars; validate(need(length(v) >= 1, "Pilih minimal 1 prediktor di tab Model."))
    list(A = make_formula(unname(pred_vars), FALSE), B = make_formula(unname(pred_vars), TRUE),
         C = make_formula(v, isTRUE(input$rg_log)))
  })

  output$rg_eval_tbl <- renderTable({
    sp <- rg_split(); ml <- rg_models()
    nm <- c(A = "Linear (8 prediktor)", B = "Linear + log(Age)", C = "Model terpilih (tab Model)")
    res <- lapply(names(ml), function(k) eval_model(ml[[k]], sp$tr, sp$te))
    data.frame(Model = nm[names(ml)],
               `RMSE uji` = sapply(res, function(r) fmt(r[["RMSE"]], 2)),
               `MAE uji` = sapply(res, function(r) fmt(r[["MAE"]], 2)),
               `R2 uji` = sapply(res, function(r) fmt(r[["R2_test"]], 3)),
               `R2 latih` = sapply(res, function(r) fmt(r[["R2_train"]], 3)),
               `n latih / uji` = paste0(nrow(sp$tr), " / ", nrow(sp$te)), check.names = FALSE)
  }, width = "100%")

  output$rg_eval_plot <- renderPlotly({
    sp <- rg_split(); ml <- rg_models()
    m <- lm(ml[[input$rg_eval_model]], data = sp$tr); p <- predict(m, sp$te); a <- sp$te$Strength
    rg <- range(c(a, p))
    plot_ly() %>%
      add_trace(x = a, y = p, type = "scatter", mode = "markers",
                marker = list(color = blue, size = 8, opacity = .6, line = list(color = surface, width = 1))) %>%
      add_trace(x = rg, y = rg, type = "scatter", mode = "lines", line = list(color = orange, width = 3)) %>%
      style_plotly() %>%
      layout(showlegend = FALSE, title = list(text = "Aktual vs Prediksi (data uji)", font = list(size = 14)),
             xaxis = list(title = "Kuat tekan aktual (MPa)"), yaxis = list(title = "Kuat tekan prediksi (MPa)"))
  })

  # ======================= DATA =======================
  output$tbl <- DT::renderDT({
    DT::datatable(dat() %>% select(all_of(unname(pred_vars)), Strength, MixType, AgeGroup, StrengthClass) %>%
                    mutate(Strength = round(Strength, 2)),
                  filter = "top", rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$dl <- downloadHandler(
    filename = function() paste0("betonlens_", Sys.Date(), ".csv"),
    content = function(file) write.csv(dat(), file, row.names = FALSE))
}

shinyApp(ui, server)
