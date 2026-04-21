library(shiny)
library(data.table)

# ══════════════════════════════════════════════════════════════════════════════
#  STAGE DEFINITIONS
# ══════════════════════════════════════════════════════════════════════════════
stages <- data.frame(
  code  = c("L1","D1","D2","D3","C1","C2","C3","R1","R2","R3","R4","E1"),
  sTag  = paste0("S", sprintf("%02d", 1:12)),
  img   = paste0(1:12, ".jpg"),
  title = c(
    "L1 \u2014 Leaf Explant",           "D1 \u2014 Dedifferentiation 1 wk",
    "D2 \u2014 Dedifferentiation 2 wk", "D3 \u2014 Dedifferentiation 5 wk",
    "C1 \u2014 Primary Callus",          "C2 \u2014 Embryogenic Callus",
    "C3 \u2014 Cell Clusters",           "R1 \u2014 Pro-embryogenic Masses",
    "R2 \u2014 Redifferentiation 24 h",  "R3 \u2014 Redifferentiation 72 h",
    "R4 \u2014 Redifferentiation 10 days","E1 \u2014 Globular Embryos"
  ),
  desc = c(
    "Leaves from greenhouse plants (explant)",
    "Explants during dedifferentiation at 1 week",
    "Explants during dedifferentiation at 2 weeks",
    "Explants during dedifferentiation at 5 weeks",
    "Compact primary callus, 3 months after induction",
    "Embryogenic callus, 7 months after induction",
    "Established cell clusters, 4 months in liquid proliferation medium",
    "Pro-embryogenic masses, 1 week in redifferentiation medium after auxin withdrawal",
    "24 h in redifferentiation medium after reducing cell density",
    "72 h in redifferentiation medium after reducing cell density",
    "10 days in redifferentiation medium after reducing cell density",
    "Globular embryos after 3 weeks of culture"
  ),
  stringsAsFactors = FALSE
)

# ══════════════════════════════════════════════════════════════════════════════
#  COLOR SCALE
# ══════════════════════════════════════════════════════════════════════════════
COLOR_STOPS <- list(
  c(0.00,   0,   0, 139), c(0.15,  20,  80, 210),
  c(0.32,   0, 170, 200), c(0.48, 160, 220, 240),
  c(0.60, 255, 240, 110), c(0.75, 255, 165,  30),
  c(0.88, 220,  60,  10), c(1.00, 200,   0,   0)
)

interp_color <- function(t) {
  t <- max(0, min(1, t))
  for (i in 2:length(COLOR_STOPS)) {
    t0 <- COLOR_STOPS[[i-1]][1]; c0 <- COLOR_STOPS[[i-1]][2:4]
    t1 <- COLOR_STOPS[[i]][1];   c1 <- COLOR_STOPS[[i]][2:4]
    if (t <= t1) {
      f <- (t - t0) / (t1 - t0)
      return(sprintf("rgb(%d,%d,%d)",
        round(c0[1] + f*(c1[1]-c0[1])),
        round(c0[2] + f*(c1[2]-c0[2])),
        round(c0[3] + f*(c1[3]-c0[3]))))
    }
  }
  "rgb(200,0,0)"
}

LEGEND_HTML <- paste(sapply(0:59, function(i)
  sprintf('<div style="flex:1;height:100%%;background:%s;"></div>', interp_color(i/59))
), collapse = "")

# ══════════════════════════════════════════════════════════════════════════════
#  LOAD DATA
# ══════════════════════════════════════════════════════════════════════════════
load_gene_data <- function() {
  files    <- paste0("coffee_SE_data_part", 1:4, ".csv")
  existing <- files[file.exists(files)]
  if (!length(existing)) return(NULL)
  dt_list  <- lapply(existing, fread, header = TRUE, sep = "auto", data.table = TRUE)
  rbindlist(dt_list, fill = TRUE, use.names = TRUE)
}

gene_dt <- load_gene_data()

# ══════════════════════════════════════════════════════════════════════════════
#  HELPERS
# ══════════════════════════════════════════════════════════════════════════════
extract_vals <- function(row, sTag) {
  vals <- numeric(0)
  for (r in 1:12) {
    key <- paste0(sTag, "_R", sprintf("%02d", r))
    if (key %in% names(row)) {
      v <- suppressWarnings(as.numeric(row[[key]]))
      if (!is.na(v)) vals <- c(vals, v)
    }
  }
  vals
}

build_tooltip <- function(title, vals) {
  html <- sprintf('<div class="rep-tooltip-title">%s</div>', title)
  for (b in 0:3) {
    idx   <- (b*3+1):min(b*3+3, length(vals))
    spans <- paste(sapply(seq_along(idx), function(ti)
      sprintf('<span>T%d: %.2f</span>', ti, vals[idx[ti]])), collapse = "")
    html  <- paste0(html, sprintf(
      '<div class="bio-group"><span class="bio-label">Biological rep %d</span>
       <div class="bio-vals">%s</div></div>', b+1, spans))
  }
  paste0(html, sprintf(
    '<div class="tooltip-summary">Mean: <em>%.4f</em>&ensp;SD: <em>%.4f</em>&ensp;n = %d</div>',
    mean(vals), sd(vals), length(vals)))
}

reset_all_cards <- function(session) {
  for (s in stages$sTag)
    session$sendCustomMessage("updateCard", list(sTag = s, reset = TRUE))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ══════════════════════════════════════════════════════════════════════════════
#  UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Playfair+Display:ital,wght@0,500;1,400&family=Source+Serif+4:wght@300;400;600&display=swap"),
    tags$style(HTML("
/* ── ROOT ──────────────────────────────────────────────────────────────────── */
:root{
  --bg:#f5f2eb; --surface:#fffefa; --border:#d6cfc0; --text:#2b2318;
  --muted:#6b5e4a; --accent:#5c3d1e; --header-bg:#3b2410; --link:#7a4f28;
}
html { font-size:17px !important; }
*, *::before, *::after { box-sizing:border-box; margin:0; padding:0; }
body {
  font-family:'Source Serif 4',Georgia,serif !important;
  background:var(--bg) !important;
  color:var(--text) !important;
  min-height:100vh;
  font-size:17px !important;
  padding:0 !important;
}
.container-fluid { padding:0 !important; }
.shiny-input-container { margin-bottom:0 !important; }

/* ── HEADER ──────────────────────────────────────────────────────────────── */
#site-header {
  background:var(--header-bg); color:#f5ede0; text-align:center;
  padding:0.3rem 1.8rem 0.2rem; border-bottom:3px solid #7a4f28;
}
#header-inner {
  display:flex;
  align-items:center;
  justify-content:center;
  gap:1rem;
  flex-wrap:wrap;
}
#site-logo {
  height:150px;
  width:150px;
  object-fit:contain;
  border-radius:5px;
  flex-shrink:0;
  display:block;
}
#site-header h1 {
  font-family:'Playfair Display',Georgia,serif;
  font-size:clamp(1.35rem,3vw,2.1rem) !important;
  font-weight:500; letter-spacing:.02em; line-height:1.35;
  color:#f5ede0 !important;
  margin:0;
}
#site-header h1 em { font-style:italic; }

/* ── SEARCH BAR ──────────────────────────────────────────────────────────── */
#search-bar {
  background:#eee8d8; border-bottom:1px solid var(--border);
  padding:.9rem 1rem; display:flex; align-items:center;
  justify-content:center; gap:.65rem; flex-wrap:wrap;
}
#search-bar label {
  font-size:17px !important; font-weight:600; color:var(--accent);
  letter-spacing:.04em; margin-bottom:0;
}
#gene_input {
  border:1px solid #b8a98a !important; border-radius:3px !important;
  padding:.4rem .7rem !important;
  font-family:'Source Serif 4',serif !important;
  font-size:16px !important; width:270px !important;
  background:var(--surface) !important; color:var(--text) !important;
  height:auto !important; box-shadow:none !important;
}
#gene_input:focus { outline:2px solid var(--accent) !important; outline-offset:1px !important; }
#go_btn {
  background:var(--accent) !important; color:#f5ede0 !important;
  border:none !important; border-radius:3px !important;
  padding:.45rem 1.15rem !important;
  font-family:'Source Serif 4',serif !important;
  font-size:16px !important; cursor:pointer !important;
  letter-spacing:.06em !important; height:auto !important;
  transition:background .18s !important;
}
#go_btn:hover { background:#7a4f28 !important; }
#reset_btn {
  background:transparent !important; color:var(--accent) !important;
  border:1px solid #b8a98a !important; border-radius:3px !important;
  padding:.43rem .95rem !important;
  font-family:'Source Serif 4',serif !important;
  font-size:16px !important; cursor:pointer !important;
  letter-spacing:.04em !important; height:auto !important;
}
#reset_btn:hover { background:#d6cfc0 !important; }

/* ── MAIN ────────────────────────────────────────────────────────────────── */
#main-content { max-width:1340px; margin:0 auto; padding:1.2rem 1rem 3rem; }

/* ── INTRO ───────────────────────────────────────────────────────────────── */
#intro-text {
  padding:.8rem 0 1.2rem; font-size:16px !important;
  color:var(--muted); line-height:1.65; font-style:italic; text-align:center;
}

/* ── NOT-FOUND BANNER ────────────────────────────────────────────────────── */
#not-found-banner {
  background:#fdf3f0; border:2px solid #d4826a; border-radius:5px;
  padding:1.2rem 1.5rem; margin-bottom:1.4rem;
  display:none; align-items:flex-start; gap:1rem;
}
.nf-icon { font-size:28px; flex-shrink:0; line-height:1; margin-top:3px; color:#c0392b; }
.nf-title { font-size:18px !important; font-weight:700; color:#8b2a12; margin-bottom:.5rem; letter-spacing:.01em; }
#nf-message {
  font-size:16px !important; color:#5a2010 !important; line-height:1.65;
  font-family:'Source Serif 4',Georgia,serif;
}
#nf-message code {
  background:#f5ddd7; padding:2px 7px; border-radius:3px;
  font-size:15px !important; color:#8b2a12 !important;
  font-family:monospace; font-weight:700;
}

/* ── GENE INFO BAR ───────────────────────────────────────────────────────── */
#gene-info-bar {
  display:flex;
  justify-content:space-between; align-items:baseline;
  margin-bottom:1.1rem; flex-wrap:wrap; gap:.4rem;
  opacity:0;
  pointer-events:none;
  transition:opacity .25s;
  min-height:0;
  overflow:hidden;
  max-height:0;
}
#gene-info-bar.visible {
  opacity:1;
  pointer-events:auto;
  max-height:60px;
}
#gene-id-display {
  font-size:17px !important; font-weight:600;
  color:var(--accent); letter-spacing:.04em;
}

/* ── DOWNLOAD BUTTON styled as a plain text link ─────────────────────────── */
#download_csv {
  font-size:15px !important;
  color:var(--link) !important;
  text-decoration:none !important;
  border:none !important;
  border-bottom:1px dashed var(--link) !important;
  background:none !important;
  padding:0 !important;
  border-radius:0 !important;
  font-family:'Source Serif 4',serif !important;
  font-weight:400 !important;
  cursor:pointer !important;
  box-shadow:none !important;
  outline:none !important;
  display:inline-flex !important;
  align-items:center !important;
  gap:5px !important;
}
#download_csv:hover {
  color:var(--accent) !important;
  border-bottom-color:var(--accent) !important;
  background:none !important;
}

/* ── STAGE GRID ──────────────────────────────────────────────────────────── */
#stage-grid { display:grid; grid-template-columns:repeat(4,1fr); gap:1.1rem 1rem; }
@media(max-width:960px){ #stage-grid { grid-template-columns:repeat(3,1fr); } }
@media(max-width:620px){ #stage-grid { grid-template-columns:repeat(2,1fr); } }

/* ── STAGE CARD ──────────────────────────────────────────────────────────── */
.stage-card {
  background:var(--surface); border:1px solid var(--border);
  border-radius:4px; overflow:visible; display:flex;
  flex-direction:column; position:relative;
}
.stage-img-wrap {
  width:100%; aspect-ratio:4/3; background:#1e1208; display:flex;
  align-items:center; justify-content:center; overflow:hidden;
  position:relative; border-radius:4px 4px 0 0;
}
.stage-img-wrap img { width:100%; height:100%; object-fit:cover; display:block; }
.img-placeholder-text { color:#7a6040; font-size:14px !important; font-style:italic; text-align:center; padding:.5rem; }
.stage-label-badge {
  position:absolute; top:6px; left:8px; background:rgba(0,0,0,.65);
  color:#f5ede0; font-family:'Playfair Display',serif;
  font-size:15px !important; font-weight:500;
  padding:2px 9px; border-radius:2px; letter-spacing:.04em;
}
.bar-row {
  display:flex; align-items:stretch; height:36px;
  border-top:1px solid var(--border); cursor:default; position:relative;
}
.bar-outer {
  width:60%; background:#e8e1d0; border-right:1px solid var(--border);
  overflow:hidden; display:flex; align-items:center;
}
.bar-fill { height:100%; transition:width .5s ease; }
.bar-stats { flex:1; display:flex; flex-direction:column; justify-content:center; padding:0 7px; min-width:0; }
.bar-mean { font-size:14px !important; font-weight:600; color:var(--text); white-space:nowrap; }
.bar-sd   { font-size:12px !important; color:var(--muted); white-space:nowrap; }
.stage-name {
  font-size:14px !important; color:var(--muted);
  padding:7px 10px 9px; line-height:1.5;
  background:#faf7f0; border-top:1px solid #e8e0d0;
  width:100%; word-wrap:break-word; white-space:normal;
}
.stage-name strong { display:block; font-size:15px !important; font-weight:600; color:var(--accent); margin-bottom:3px; }

/* ── TOOLTIP ─────────────────────────────────────────────────────────────── */
.rep-tooltip {
  display:none; position:absolute; bottom:calc(100% + 8px); left:50%;
  transform:translateX(-50%); background:#241a0e; color:#f0e6d0;
  border-radius:5px; padding:11px 13px; font-size:13px !important;
  line-height:1.65; z-index:9999; white-space:nowrap;
  box-shadow:0 6px 22px rgba(0,0,0,.48); pointer-events:none;
  font-family:'Source Serif 4',serif; min-width:250px;
}
.rep-tooltip::after {
  content:''; position:absolute; top:100%; left:50%;
  transform:translateX(-50%); border:7px solid transparent; border-top-color:#241a0e;
}
.rep-tooltip-title { font-weight:600; color:#e8c890; margin-bottom:5px; font-size:14px !important; letter-spacing:.04em; border-bottom:1px solid #4a3620; padding-bottom:4px; }
.bio-group  { margin-bottom:4px; }
.bio-label  { color:#a08060; font-size:12px !important; display:block; margin-bottom:2px; }
.bio-vals   { display:flex; gap:8px; }
.bio-vals span { color:#d4c4a4; font-size:13px !important; }
.tooltip-summary { margin-top:5px; padding-top:5px; border-top:1px solid #4a3620; font-size:13px !important; }
.tooltip-summary em { color:#e8c890; font-style:normal; font-weight:600; }
.stage-card.has-data:hover .rep-tooltip { display:block; }

/* ── LEGEND ──────────────────────────────────────────────────────────────── */
#legend-wrap {
  display:none; margin-top:1.8rem; background:var(--surface);
  border:1px solid var(--border); border-radius:4px;
  padding:.9rem 1.1rem 1.1rem;
}
#legend-scale { display:flex; height:18px; border-radius:2px; overflow:hidden; margin:.5rem 0 .35rem; max-width:580px; }
.legend-labels { display:flex; justify-content:space-between; max-width:580px; font-size:14px !important; color:var(--muted); margin-bottom:.7rem; }
.legend-body { font-size:15px !important; color:var(--muted); line-height:1.6; text-align:justify; }
.legend-body + .legend-body { margin-top:.45rem; }

/* ── REFERENCE ───────────────────────────────────────────────────────────── */
#reference-block {
  margin-top:1.4rem; padding:.9rem 1.1rem; background:#f0ece2;
  border-left:4px solid var(--accent); border-radius:0 4px 4px 0;
  font-size:15px !important; color:var(--muted); line-height:1.65; text-align:justify;
}
#reference-block strong { color:var(--accent); display:block; margin-bottom:.35rem; font-size:16px !important; }
#reference-block a { color:var(--link); text-decoration:none; border-bottom:1px dotted var(--link); }
#reference-block a:hover { color:var(--accent); }
    "))
  ),

  # ── HEADER ──────────────────────────────────────────────────────────────────
  tags$header(id = "site-header",
    tags$div(id = "header-inner",
      tags$img(
        id  = "site-logo",
        src = "logo.png",
        alt = "App logo"
      ),
      tags$h1(HTML("Gene Expression Map of <em>Coffea arabica</em> Somatic Embryogenesis"))
    )
  ),

  # ── SEARCH BAR ──────────────────────────────────────────────────────────────
  tags$div(id = "search-bar",
    tags$label(`for` = "gene_input", "Gene ID:"),
    textInput("gene_input", label = NULL,
              placeholder = "e.g. LOC140003795", width = "270px"),
    actionButton("go_btn",    "GO"),
    actionButton("reset_btn", "RESET")
  ),

  # ── MAIN ────────────────────────────────────────────────────────────────────
  tags$div(id = "main-content",

    tags$div(id = "intro-text", HTML(
      "Enter a Gene ID above and press <strong style='font-style:normal;'>GO</strong>
       to overlay expression values across all 12 developmental stages.
       Hover any stage card to inspect individual replicate values."
    )),

    # Not-found banner
    tags$div(id = "not-found-banner",
      tags$div(class = "nf-icon", HTML("&#9888;")),
      tags$div(
        tags$div(class = "nf-title", "Gene not found in dataset"),
        tags$div(id = "nf-message")
      )
    ),

    # ── GENE INFO BAR ─────────────────────────────────────────────────────────
    tags$div(id = "gene-info-bar",
      tags$span(id = "gene-id-display"),
      downloadButton("download_csv",
        label = "Download normalized data for this gene (CSV)",
        icon  = icon("download")
      )
    ),

    # Stage grid
    uiOutput("stage_grid_ui"),

    # Legend
    tags$div(id = "legend-wrap",
      tags$div(
        style = "font-size:16px;font-weight:600;color:var(--accent);letter-spacing:.04em;",
        "Expression level color scale"),
      tags$div(id = "legend-scale", HTML(LEGEND_HTML)),
      tags$div(class = "legend-labels",
        tags$span("Low"), tags$span("Medium-low"),
        tags$span("Medium-high"), tags$span("High")
      ),
      tags$p(class = "legend-body", HTML(
        "<strong style='color:var(--accent);'>Developmental stages:</strong>
         <strong>L1</strong> &mdash; Leaf explant from greenhouse plants &bull;
         <strong>D1</strong> &mdash; Explants during dedifferentiation at 1 week &bull;
         <strong>D2</strong> &mdash; Explants during dedifferentiation at 2 weeks &bull;
         <strong>D3</strong> &mdash; Explants during dedifferentiation at 5 weeks &bull;
         <strong>C1</strong> &mdash; Compact primary callus obtained 3 months after induction &bull;
         <strong>C2</strong> &mdash; Embryogenic callus obtained 7 months after induction &bull;
         <strong>C3</strong> &mdash; Established cell clusters after 4 months in liquid proliferation medium &bull;
         <strong>R1</strong> &mdash; Pro-embryogenic masses at 1 week in redifferentiation medium after auxin withdrawal &bull;
         <strong>R2</strong> &mdash; 24 h in redifferentiation medium after reducing cell density &bull;
         <strong>R3</strong> &mdash; 72 h in redifferentiation medium after reducing cell density &bull;
         <strong>R4</strong> &mdash; 10 days in redifferentiation medium after reducing cell density &bull;
         <strong>E1</strong> &mdash; Globular embryos obtained after 3 weeks of culture."
      )),
      tags$p(class = "legend-body", HTML(
        "Bar value = mean of 12 replicates (4 biological &times; 3 technical) per stage;
         &plusmn;SD shown below mean. Color scale is relative to the minimum and maximum
         expression across all 12 stages for the queried gene."
      )),
      tags$div(id = "reference-block",
        tags$strong("Image reference"),
        HTML("Awada R, Lepelley M, Breton D, Charpagne A, Campa C, Berry V, Georget F,
          Breitler JC, L&eacute;ran S, Djerrab D, Martinez-Seidel F, Descombes P,
          Crouzillat D, Bertrand B, Etienne H. Global transcriptome profiling reveals
          differential regulatory, metabolic and hormonal networks during somatic
          embryogenesis in <em>Coffea arabica</em>. <em>BMC Genomics.</em>
          2023 Jan 24;24(1):41.
          doi: <a href='https://doi.org/10.1186/s12864-022-09098-z' target='_blank'>
          10.1186/s12864-022-09098-z</a>. PMID: 36694132; PMCID: PMC9875526.")
      )
    )
  ),

  # ── JAVASCRIPT ──────────────────────────────────────────────────────────────
  tags$script(HTML("
    Shiny.addCustomMessageHandler('togglePanel', function(msg) {
      var el = document.getElementById(msg.id);
      if (!el) return;
      if (msg.id === 'gene-info-bar') {
        if (msg.display === 'none' || msg.display === 'hidden') {
          el.classList.remove('visible');
        } else {
          el.classList.add('visible');
        }
      } else {
        el.style.display = msg.display;
      }
    });

    Shiny.addCustomMessageHandler('setNfMessage', function(msg) {
      var el = document.getElementById('nf-message');
      if (el) el.innerHTML = msg.html;
    });

    Shiny.addCustomMessageHandler('setGeneIdDisplay', function(msg) {
      var el = document.getElementById('gene-id-display');
      if (el) el.innerHTML = msg.html;
    });

    Shiny.addCustomMessageHandler('updateCard', function(msg) {
      var fill = document.getElementById('fill-' + msg.sTag);
      var mean = document.getElementById('mean-' + msg.sTag);
      var sd   = document.getElementById('sd-'   + msg.sTag);
      var tip  = document.getElementById('tip-'  + msg.sTag);
      var card = document.getElementById('card-' + msg.sTag);
      if (!fill) return;
      if (msg.reset) {
        fill.style.width      = '0%';
        fill.style.background = '#e0d8c8';
        mean.textContent      = '\u2014';
        mean.style.color      = 'var(--muted)';
        mean.style.fontStyle  = 'italic';
        mean.style.fontWeight = '400';
        sd.textContent = '';
        if (tip) tip.innerHTML = '';
        card.classList.remove('has-data');
      } else {
        fill.style.width      = msg.barW + '%';
        fill.style.background = msg.color;
        mean.textContent      = msg.mean;
        mean.style.color      = 'var(--text)';
        mean.style.fontStyle  = 'normal';
        mean.style.fontWeight = '600';
        sd.textContent = msg.sd;
        if (tip) tip.innerHTML = msg.tipHTML;
        card.classList.add('has-data');
      }
    });

    $(document).on('keydown', '#gene_input', function(e) {
      if (e.key === 'Enter') $('#go_btn').click();
    });
  "))
)

# ══════════════════════════════════════════════════════════════════════════════
#  SERVER
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  rv <- reactiveValues(gene_id = NULL, stage_vals = NULL)

  # Build stage grid once
  output$stage_grid_ui <- renderUI({
    cards <- lapply(seq_len(nrow(stages)), function(i) {
      st <- stages[i, ]
      tags$div(class = "stage-card", id = paste0("card-", st$sTag),
        tags$div(class = "stage-img-wrap",
          tags$img(
            src     = st$img,
            alt     = st$title,
            loading = "lazy",
            onerror = "this.outerHTML='<span class=\"img-placeholder-text\">Image not available</span>'"
          ),
          tags$span(class = "stage-label-badge", st$code)
        ),
        tags$div(class = "bar-row",
          tags$div(class = "bar-outer",
            tags$div(class = "bar-fill",
              id    = paste0("fill-", st$sTag),
              style = "width:0%;background:#e0d8c8;")
          ),
          tags$div(class = "bar-stats",
            tags$span(class = "bar-mean",
              id    = paste0("mean-", st$sTag),
              style = "color:var(--muted);font-style:italic;font-weight:400;",
              "\u2014"),
            tags$span(class = "bar-sd",
              id = paste0("sd-", st$sTag))
          ),
          tags$div(class = "rep-tooltip", id = paste0("tip-", st$sTag))
        ),
        tags$div(class = "stage-name",
          tags$strong(st$title),
          st$desc
        )
      )
    })
    tags$div(id = "stage-grid", do.call(tagList, cards))
  })

  # ── GO ──────────────────────────────────────────────────────────────────────
  observeEvent(input$go_btn, {
    raw_id  <- trimws(input$gene_input)
    gene_id <- toupper(raw_id)

    session$sendCustomMessage("togglePanel",
      list(id = "not-found-banner", display = "none"))

    if (!nzchar(gene_id)) return()

    if (is.null(gene_dt)) {
      showNotification(
        "CSV files not found. Place coffee_SE_data_part1\u20134.csv in the app folder.",
        type = "error", duration = 8)
      return()
    }

    idx <- which(toupper(gene_dt[[1]]) == gene_id)

    if (!length(idx)) {
      safe_id <- htmltools::htmlEscape(raw_id)
      session$sendCustomMessage("setNfMessage", list(html = sprintf(
        'The gene <code>%s</code> was not found in the dataset. Please check the ID and try again.',
        safe_id)))
      session$sendCustomMessage("togglePanel",
        list(id = "not-found-banner", display = "flex"))
      reset_all_cards(session)
      session$sendCustomMessage("togglePanel", list(id = "gene-info-bar", display = "none"))
      session$sendCustomMessage("togglePanel", list(id = "legend-wrap",   display = "none"))
      rv$gene_id <- NULL; rv$stage_vals <- NULL
      return()
    }

    row <- gene_dt[idx[1], ]

    stage_vals <- setNames(
      lapply(stages$sTag, function(s) extract_vals(row, s)),
      stages$sTag)

    means <- sapply(stage_vals, function(v) if (length(v)) mean(v) else 0)
    gMin  <- min(means); gMax <- max(means)
    gRng  <- if (gMax == gMin) 1 else gMax - gMin

    for (i in seq_len(nrow(stages))) {
      st   <- stages[i, ]
      vals <- stage_vals[[st$sTag]]
      if (!length(vals)) next
      m <- mean(vals); s <- sd(vals)
      t <- (m - gMin) / gRng
      session$sendCustomMessage("updateCard", list(
        sTag    = st$sTag,
        reset   = FALSE,
        barW    = round(t * 100),
        color   = interp_color(t),
        mean    = sprintf("%.2f", m),
        sd      = sprintf("\u00b1%.2f", s),
        tipHTML = build_tooltip(st$title, vals)
      ))
    }

    rv$gene_id    <- gene_id
    rv$stage_vals <- stage_vals

    session$sendCustomMessage("setGeneIdDisplay", list(
      html = paste0("Gene ID: <strong>", htmltools::htmlEscape(gene_id), "</strong>")))

    session$sendCustomMessage("togglePanel", list(id = "intro-text",    display = "none"))
    session$sendCustomMessage("togglePanel", list(id = "gene-info-bar", display = "flex"))
    session$sendCustomMessage("togglePanel", list(id = "legend-wrap",   display = "block"))
  })

  # ── RESET ────────────────────────────────────────────────────────────────────
  observeEvent(input$reset_btn, {
    updateTextInput(session, "gene_input", value = "")
    session$sendCustomMessage("togglePanel", list(id = "not-found-banner", display = "none"))
    session$sendCustomMessage("togglePanel", list(id = "gene-info-bar",    display = "none"))
    session$sendCustomMessage("togglePanel", list(id = "legend-wrap",      display = "none"))
    session$sendCustomMessage("togglePanel", list(id = "intro-text",       display = "block"))
    reset_all_cards(session)
    rv$gene_id <- NULL; rv$stage_vals <- NULL
  })

  # ── DOWNLOAD ─────────────────────────────────────────────────────────────────
  output$download_csv <- downloadHandler(
    filename = function() {
      gid <- isolate(rv$gene_id)
      paste0(if (!is.null(gid) && nzchar(gid)) gid else "gene", "_expression.csv")
    },
    content = function(file) {
      gid <- isolate(rv$gene_id)
      sv  <- isolate(rv$stage_vals)

      if (is.null(gid) || is.null(sv)) {
        write.csv(
          data.frame(Error = "No gene data available. Please search for a gene first."),
          file, row.names = FALSE)
        return()
      }

      rows <- do.call(rbind, lapply(seq_len(nrow(stages)), function(i) {
        st   <- stages[i, ]
        vals <- sv[[st$sTag]]
        if (!length(vals)) return(NULL)
        data.frame(
          GeneID      = gid,
          Stage_Code  = st$code,
          Stage_Tag   = st$sTag,
          Stage_Title = st$title,
          Replicate   = seq_along(vals),
          Value       = vals,
          Stage_Mean  = round(mean(vals), 6),
          Stage_SD    = round(sd(vals),   6),
          stringsAsFactors = FALSE
        )
      }))

      if (is.null(rows) || nrow(rows) == 0) {
        write.csv(
          data.frame(Error = "No replicate values found for this gene."),
          file, row.names = FALSE)
        return()
      }

      write.csv(rows, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
}

# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui = ui, server = server)
