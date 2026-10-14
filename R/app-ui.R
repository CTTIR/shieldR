.shield_app_ui <- function() {
  numeric <- function(id, label, value, min = 0, max = NULL, step = 1)
    shiny::numericInput(id, label, value, min = min,
      max = if (is.null(max)) NA else max, step = step)
  choices <- c("P0: initial class, fixed route" = "P0",
    "P1: repeated reassessment" = "P1", "P2: route learning" = "P2",
    "P3: coupled heuristic" = "P3")
  bslib::page_sidebar(
    title = "shieldR | Scenario workbench", fillable = FALSE,
    theme = bslib::bs_theme(version = 5, bg = "#fafafa", fg = "#212121",
      primary = "#292929", secondary = "#626262",
      base_font = bslib::font_collection("system-ui", "Segoe UI", "Arial"),
      "border-radius" = ".25rem"),
    shiny::tags$head(shiny::tags$style(shiny::HTML(
      ".shield-intro{max-width:80ch;margin-bottom:1.4rem}
       .shield-chain{overflow-x:auto;margin:1rem 0 1.5rem}
       .shield-chain svg{min-width:560px;width:100%;height:auto}
       .shield-status{border-left:4px solid #555;padding:.75rem 1rem;background:#eee;margin-bottom:1rem}
       .shield-status p:last-child{margin-bottom:0}
       .shield-downloads{display:flex;flex-wrap:wrap;gap:.6rem;margin:1rem 0}
       .shield-notes{max-width:82ch;line-height:1.6}
       .card{box-shadow:none}.card-header{font-weight:600}
       .bslib-sidebar-layout>.sidebar{background:white}
       .btn-primary{font-weight:600}
       a:focus-visible,button:focus-visible,input:focus-visible,select:focus-visible{outline:3px solid #555;outline-offset:3px}
       .dataTables_wrapper{overflow-x:auto}
       @media(prefers-reduced-motion:reduce){*{scroll-behavior:auto!important;transition:none!important}}"
    ))),
    sidebar = bslib::sidebar(width = 340, title = "Build an experiment",
      shiny::selectInput("preset", "Teaching graph",
        c("Balanced resources" = "balanced", "Constrained resources" = "constrained")),
      shiny::helpText("Presets have zero exposure and item-condition hazards. Edit the graph and assumptions explicitly."),
      shiny::selectInput("design", "Comparison design", c("Single policy" = "single",
        "Independent policy arms" = "policies", "Paired disruptions-disabled intervention" = "intervention")),
      shiny::conditionalPanel("input.design !== 'policies'",
        shiny::selectInput("policy", "Policy", choices, selected = "P3")),
      shiny::conditionalPanel("input.design === 'policies'",
        shiny::selectInput("policies", "Policies to compare", choices,
          selected = c("P0", "P3"), multiple = TRUE)),
      shiny::selectInput("disruption_model", "Disruption model response",
        c("Static" = "static", "Adaptive" = "adaptive")),
      bslib::accordion(open = "Batches and reproducibility",
        bslib::accordion_panel("Batches and reproducibility",
          numeric("N", "Items per batch", 12, 1, 60),
          numeric("n_rep", "Replications per arm", 8, 2, 100),
          numeric("horizon", "Administrative horizon (minutes)", 60, 1, 240),
          numeric("seed", "Master seed", 20260512, 0, .Machine$integer.max - 1000),
          shiny::helpText("Class mix: approximately 1:2:2, with integer rounding. Separate seeds for independent arms; matched seeds for paired intervention.")),
        bslib::accordion_panel("Observation and dispatch",
          numeric("reassess", "Reassessment interval (minutes)", 1, 1, 60),
          numeric("misclass", "Adjacent-class observation error", 0, 0, 1, .01),
          numeric("slot", "Dispatch interval (minutes)", 1, 1, 20),
          numeric("max_hold", "Maximum departure hold (minutes)", 2, 0, 20),
          numeric("t_load", "Loading duration (minutes)", 1, 1, 60),
          numeric("t_unload", "Unloading duration (minutes)", 1, 1, 60),
          numeric("beta", "Policy softmax parameter", 1, 0, NULL, .1)),
        bslib::accordion_panel("Item-condition assumptions",
          shiny::helpText("Synthetic instantaneous rates per minute. Hazards cease after arrival viable at Stage 2."),
          numeric("h1", "Class 1 loss rate", 0, 0, NULL, .001),
          numeric("h2", "Class 2 loss rate", 0, 0, NULL, .001),
          numeric("h3", "Class 3 loss rate", 0, 0, NULL, .001),
          numeric("esc32", "Escalation rate: class 3 to 2", 0, 0, NULL, .001),
          numeric("esc21", "Escalation rate: class 2 to 1", 0, 0, NULL, .001)),
        bslib::accordion_panel("Disruption assumptions",
          shiny::checkboxInput("disruptions_enabled", "Enable disruptions", FALSE),
          shiny::helpText("The paired intervention always runs disruptions on and off. Nonzero disruption_risk and graph exposure are needed for disruption events."),
          numeric("disruption_risk", "Baseline disruption probability", 0, 0, 1, .01),
          numeric("q", "Conditional disruption destructiveity probability", 0, 0, 1, .01),
          numeric("att_delay", "Nondestructive disruption delay (minutes)", 0, 0, 120),
          numeric("lambda", "Adaptive response parameter", 0, 0, NULL, .1),
          numeric("atk_window", "Departure-history window (minutes)", 1, 1, 120))
      ),
      shiny::actionButton("run", "Run experiment", class = "btn-primary w-100 mt-3"),
      shiny::helpText("Interactive cap: 250,000 item-minute steps across all arms. Comparisons use 10,000 bootstrap resamples.")),
    shiny::div(class = "shield-intro",
      shiny::h2("How do assumptions change outcomes?"),
      shiny::p("Build a synthetic logistics chain, compare decision rules, and inspect the complete experiment. Each replication follows one batch through the same five-leg topology.")),
    shiny::div(class = "shield-chain", shiny::HTML(
      '<svg viewBox="0 0 930 100" role="img" aria-labelledby="chain-title chain-description" xmlns="http://www.w3.org/2000/svg"><title id="chain-title">The fixed five-leg logistics chain</title><desc id="chain-description">Source to Collection to two Transfer nodes to two Stage 1 nodes to Stage 2, the primary viability endpoint, and finally Stage 3. Every leg has three route choices.</desc><g fill="none" stroke="#777" stroke-width="2"><path d="M80 45H850"/></g><g fill="#fff" stroke="#333" stroke-width="2"><rect x="5" y="20" width="110" height="50" rx="4"/><rect x="165" y="20" width="110" height="50" rx="4"/><rect x="325" y="20" width="110" height="50" rx="4"/><rect x="485" y="20" width="110" height="50" rx="4"/><rect x="645" y="20" width="110" height="50" rx="4"/><rect x="805" y="20" width="110" height="50" rx="4"/></g><g fill="#222" font-family="system-ui,sans-serif" font-size="16" text-anchor="middle"><text x="60" y="51">Source</text><text x="220" y="51">Collection</text><text x="380" y="51">Transfer A / B</text><text x="540" y="51">Stage 1 a / b</text><text x="700" y="51">Stage 2</text><text x="860" y="51">Stage 3</text><text x="140" y="17" font-size="12">L1</text><text x="300" y="17" font-size="12">L2</text><text x="460" y="17" font-size="12">L3</text><text x="620" y="17" font-size="12">L4</text><text x="780" y="17" font-size="12">L5</text><text x="700" y="93" font-size="12">Primary endpoint</text></g></svg>'
    )),
    shiny::div(role = "status", `aria-live` = "polite", shiny::uiOutput("status")),
    bslib::navset_card_tab(id = "workspace", full_screen = TRUE,
      bslib::nav_panel("Graph",
        shiny::p("Double-click a numeric cell to edit. Vehicle fractions apply to a whole leg; editing one updates all three routes on that leg. Each leg has at least two vehicles after rounding."),
        shiny::fileInput("graph_upload", "Import a five-leg graph CSV", accept = ".csv"),
        DT::DTOutput("graph"),
        shiny::div(class = "shield-downloads",
          shiny::downloadButton("download_graph", "Download current graph CSV")),
        shiny::p("travel_min: positive whole minutes; exposure_weight: 0 to 1; vehicle_fraction: positive vehicles per item. The fixed node and route labels must remain unchanged.")),
      bslib::nav_panel("Outcomes",
        shiny::uiOutput("outcome_note"),
        shiny::plotOutput("viability_plot", height = 340),
        DT::DTOutput("primary"),
        shiny::p("Viable nonarrivals at the administrative horizon count as endpoint failures. A Stage 2 nonarrival is not necessarily a loss.")),
      bslib::nav_panel("Comparisons",
        shiny::uiOutput("comparison_note"), DT::DTOutput("comparisons"),
        shiny::h4("Omnibus comparison"), DT::DTOutput("omnibus")),
      bslib::nav_panel("Diagnostics",
        shiny::h4("Resource use across the chain"),
        shiny::plotOutput("resource_plot", height = 300),
        shiny::h4("Time among observed arrivals"),
        shiny::plotOutput("time_plot", height = 300),
        shiny::p("Time summaries condition on arrival; absent arrivals remain missing. Inspect missing counts alongside viability. Disruptions, dispatches and items viable without Stage 3 arrival at the horizon are separate diagnostics."),
        DT::DTOutput("diagnostics")),
      bslib::nav_panel("Data and export",
        shiny::p("Downloads describe the last completed experiment. The RDS bundle preserves parameters, per-arm results, comparison tables, software and RNG provenance. The replay script reproduces simulated batches and their descriptive summaries."),
        shiny::div(class = "shield-downloads",
          shiny::downloadButton("download_bundle", "Complete experiment RDS"),
          shiny::downloadButton("download_replay", "Replay script R"),
          shiny::downloadButton("download_raw", "Batch results CSV"),
          shiny::downloadButton("download_summary", "Summaries CSV"),
          shiny::downloadButton("download_comparisons", "Comparisons CSV"),
          shiny::downloadButton("download_png", "Viability figure PNG (600 dpi)"),
          shiny::downloadButton("download_pdf", "Viability figure PDF")),
        shiny::verbatimTextOutput("provenance"), DT::DTOutput("raw")),
      bslib::nav_panel("Methods and help", .shield_app_help())
    )
  )
}

.shield_app_help <- function() {
  shiny::div(class = "shield-notes",
    shiny::h3("An explicit model, an inspectable experiment"),
    shiny::p("Start with a teaching graph, edit its numeric cells, choose a design and run. Balanced and constrained presets change only graph vehicle fractions and the suggested horizon; other assumptions remain under your control. The defaults intentionally remove disruption and item-condition hazards so that transport mechanics can be inspected first."),
    shiny::h4("What the policies know"),
    shiny::tags$ul(
      shiny::tags$li("P0 uses initial true class and a fixed route."),
      shiny::tags$li("P1 uses noisy reassessment."),
      shiny::tags$li("P2 learns route risk through observations."),
      shiny::tags$li("P3 couples reassessment, route, destination and departure holds. It assumes the specified disruption_model response model and uses an entropy-regularized heuristic; it does not solve an equilibrium.")),
    shiny::h4("Choose the comparison before running"),
    shiny::p("Independent policy arms receive distinct master seeds. All selected pairwise Stage 2 comparisons form one explicit Benjamini-Hochberg family. The app reports Mann-Whitney U, Cliff's delta and a median difference with 95% percentile bootstrap intervals. With at least three arms it also reports Kruskal-Wallis H and degrees of freedom. These exploratory comparisons are not the manuscript's prespecified hypothesis families."),
    shiny::p("The paired intervention uses the same policy, disruption_model, parameters, master seed and replication count with disruptions enabled and disabled. Its contrast is no disruptions minus disruptions, summarized as a paired mean difference and bootstrap interval. The total modelled effect includes downstream delays and horizon effects; it is not a direct count of disruption losses."),
    shiny::h4("Interpret uncertainty within the model"),
    shiny::p("Intervals use 10,000 bootstrap resamples and describe Monte Carlo sampling uncertainty conditional on the supplied assumptions. The small interactive experiments can have unstable estimates or point intervals, especially for discrete viability proportions. Increasing replication count does not resolve model uncertainty or establish empirical validity. Median and IQR are descriptive; normality testing is not a preliminary step."),
    shiny::h4("Endpoint and resource semantics"),
    shiny::p("All items enter at time zero. Time advances in whole minutes. The primary endpoint is the proportion arriving viable at Stage 2 by the administrative horizon. Item-condition hazard stops after live Stage 2 arrival; Stage 3 is terminal. Resource utilization is occupied vehicle time divided by available fleet time over the horizon. n_pending counts items viable without Stage 3 arrival at the horizon; it is not a direct loss count."),
    shiny::h4("Reproduce and extend"),
    shiny::p("Download the complete experiment and replay script. Use shield_parameters(), simulate_shield(), summarize_shield() and the comparison helpers in scripted analyses for larger experiments. App workload limits are not restrictions on the package API. Parameters not exposed in the form retain the documented neutral package defaults and are recorded in the complete bundle."),
    shiny::h4("Local processing and package status"),
    shiny::p("Graph uploads are processed in this R session and are not sent to an external service. Session state is not shared between users. Deployment on a remote Shiny server changes where processing occurs. shieldR is a academic research package; licensing and institutional release remain pending. Use citation('shieldR') for the installed package citation.")
  )
}
