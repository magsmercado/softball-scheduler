##############################################################################
# Softball League Scheduler + Game-Bag Tracker
#a little helper app by mags mercado
# Builds a season schedule that steers around blackout dates -- giving a
# blacked-out team a bye whenever there's a bye slot free that week -- then
# figures out, week by week, who holds each game bag, which field each
# game is on, and which off-day hand-offs (if any) are needed so exactly
# one bag shows up at every game.
##############################################################################
library(shiny)
library(DT)
library(openxlsx)
# Field pool: fields 10-15 are the default rotation (6 fields, enough for
# 12 teams / 6 simultaneous games); 10-13 are the desirable fields, 14-15
# are the lower-quality fields that get spread evenly across teams. Field
# 9 is held in reserve and only pressed into service for a 7th
# simultaneous game in a week.
GOOD_FIELDS <- c(10, 11, 12, 13)
LOW_FIELDS <- c(14, 15)
OVERFLOW_FIELD <- 9
ALL_FIELDS <- sort(c(GOOD_FIELDS, LOW_FIELDS, OVERFLOW_FIELD))
# Fields treated as "low quality" for fairness tracking and display --
# the regular low fields plus the overflow field on the rare week it's used.
LOW_QUALITY_FIELDS <- sort(c(LOW_FIELDS, OVERFLOW_FIELD))
# 7 fields total (10-15 regularly, 9 as overflow) means at most 7
# simultaneous games in a week.
MAX_GAMES_PER_WEEK <- length(GOOD_FIELDS) + length(LOW_FIELDS) + 1
##############################################################################
## 1. SCHEDULING ENGINE
##############################################################################
# Build the list of calendar dates that will actually host games: start on
# start_date, step weekly, skip any date that falls on a league-wide
# blackout date, until `weeks_needed` valid dates are collected.
build_calendar <- function(start_date, weeks_needed, league_blackouts) {
  dates <- as.Date(character(0))
  d <- as.Date(start_date)
  guard <- 0
  while (length(dates) < weeks_needed && guard < 500) {
    if (!(d %in% league_blackouts)) {
      dates <- c(dates, d)
    }
    d <- d + 7
    guard <- guard + 1
  }
  dates
}
# Build one week package (byes + pairs) per calendar date, with exactly
# `byes_per_week` teams sitting out each date. Each date's byes are chosen
# in two passes:
#   1. Any team with a team-specific blackout on THIS date gets first claim
#      on a bye slot (ties among multiple blacked-out teams broken by
#      whoever has had the fewest byes so far, to keep things fair).
#   2. Any bye slots left over go to whichever teams have had the fewest
#      byes so far overall, so bye weeks still end up spread evenly across
#      the roster over the season.
# Among the teams that ARE playing that date, opponents are matched
# greedily by fewest prior meetings this season, so the schedule works
# toward a full round robin before it ever repeats a matchup.
#
# Returns list(schedule = <one week package per date>, violations =
# <number of games where a team ended up playing despite a blackout,
# because more teams needed a bye that date than there were bye slots>).
build_schedule <- function(teams, byes_per_week, dates, team_blackouts, seed = 1) {
  set.seed(seed)
  n <- length(teams)
  bye_count <- setNames(rep(0L, n), teams)
  pair_count <- matrix(0L, n, n, dimnames = list(teams, teams))
  weeks <- vector("list", length(dates))
  for (w in seq_along(dates)) {
    date_w <- dates[w]
    blacked <- Filter(function(t) {
      bl <- team_blackouts[[t]]
      !is.null(bl) && date_w %in% bl
    }, teams)
    if (length(blacked) > 0) {
      blacked <- blacked[order(bye_count[blacked], runif(length(blacked)))]
    }
    n_from_blackout <- min(length(blacked), byes_per_week)
    bye_teams <- if (n_from_blackout > 0) blacked[seq_len(n_from_blackout)] else character(0)
    remaining_slots <- byes_per_week - n_from_blackout
    if (remaining_slots > 0) {
      pool <- setdiff(teams, bye_teams)
      pool <- pool[order(bye_count[pool], runif(length(pool)))]
      bye_teams <- c(bye_teams, pool[seq_len(min(remaining_slots, length(pool)))])
    }
    active <- setdiff(teams, bye_teams)
    active <- sample(active)  # shuffle so match order isn't biased by input order
    pool <- active
    pairs <- list()
    while (length(pool) >= 2) {
      a <- pool[1]
      rest <- pool[-1]
      counts <- pair_count[a, rest]
      cands <- rest[counts == min(counts)]
      b <- if (length(cands) > 1) sample(cands, 1) else cands
      pairs[[length(pairs) + 1]] <- c(a, b)
      pool <- setdiff(pool, c(a, b))
    }
    for (t in bye_teams) bye_count[t] <- bye_count[t] + 1L
    for (p in pairs) {
      pair_count[p[1], p[2]] <- pair_count[p[1], p[2]] + 1L
      pair_count[p[2], p[1]] <- pair_count[p[2], p[1]] + 1L
    }
    weeks[[w]] <- list(byes = bye_teams, pairs = pairs)
  }
  violations <- 0
  for (w in seq_along(dates)) {
    date_w <- dates[w]
    for (p in weeks[[w]]$pairs) {
      for (t in p) {
        bl <- team_blackouts[[t]]
        if (!is.null(bl) && date_w %in% bl) violations <- violations + 1
      }
    }
  }
  list(schedule = weeks, violations = violations)
}
##############################################################################
## 2. GAME-BAG ROUTER
##############################################################################
# Given the final schedule (list of weeks, each list(byes = <character
# vector>, pairs = <list of c(home, away)>)) and the full team list, work
# out who holds which numbered bag each week, which games need no hand-off
# (bag-holder already faces a non-holder), and which off-day transfers are
# required to fix any mismatched pairs (both holding, or neither holding).
# Also assigns each game a field number (9-15), steering low-quality-field
# duty (9, 14, 15) toward whichever teams have had the fewest low-field
# games so far, so no team is stuck on the bad fields disproportionately
# over the season.
#
# Returns a list with one entry per week:
#   $assignments : list of list(home, away, bag, field)
#   $transfers   : list of list(from, to, bag)  (off-day hand-offs before this week)
#   $byes        : character vector of teams sitting out this week
route_bags <- function(schedule, teams) {
  state <- list()          # team name -> bag label currently held
  next_label <- 1L
  low_field_count <- list()  # team name -> number of low-quality-field games so far
  log <- vector("list", length(schedule))
  get_low_count <- function(t) {
    v <- low_field_count[[t]]
    if (is.null(v)) 0L else v
  }
  for (w in seq_along(schedule)) {
    real <- schedule[[w]]$pairs
    bye_teams <- schedule[[w]]$byes
    if (w == 1) {
      for (p in real) {
        home <- p[1]
        state[[home]] <- next_label
        next_label <- next_label + 1L
      }
    }
    # --- field assignment for this week ---
    # Fill the 4 desirable fields first, then the 2 regular low fields,
    # and only reach for the field-9 overflow slot on a 7-game week where
    # every other field is already spoken for. Among pairs that must take
    # a low-quality field (14/15, or 9 on an overflow week), whichever
    # pairs have had the fewest combined low-field games so far are "due"
    # and get sent there first, so no team gets stuck on the bad fields
    # disproportionately over the season. Ties broken by original pair order.
    n_games <- length(real)
    n_low_needed <- max(0, min(length(LOW_FIELDS), n_games - length(GOOD_FIELDS)))
    n_overflow_needed <- max(0, min(1, n_games - length(GOOD_FIELDS) - length(LOW_FIELDS)))
    field_for_idx <- integer(n_games)
    if (n_games > 0) {
      scores <- vapply(real, function(p) get_low_count(p[1]) + get_low_count(p[2]), numeric(1))
      ord <- order(scores)
      low_idx <- if (n_low_needed > 0) ord[seq_len(n_low_needed)] else integer(0)
      overflow_idx <- if (n_overflow_needed > 0) ord[(n_low_needed + 1):(n_low_needed + n_overflow_needed)] else integer(0)
      good_idx <- setdiff(seq_len(n_games), c(low_idx, overflow_idx))
      if (length(low_idx) > 0) field_for_idx[low_idx] <- LOW_FIELDS[seq_along(low_idx)]
      if (length(overflow_idx) > 0) field_for_idx[overflow_idx] <- OVERFLOW_FIELD
      if (length(good_idx) > 0) field_for_idx[good_idx] <- GOOD_FIELDS[seq_along(good_idx)]
      for (i in c(low_idx, overflow_idx)) {
        p <- real[[i]]
        low_field_count[[p[1]]] <- get_low_count(p[1]) + 1L
        low_field_count[[p[2]]] <- get_low_count(p[2]) + 1L
      }
    }
    hh <- character(0)   # teams that must ship a surplus bag out before this week
    nn <- character(0)   # teams that need an incoming bag before this week
    assignments <- list()
    for (i in seq_along(real)) {
      p <- real[[i]]
      field_i <- field_for_idx[i]
      a <- p[1]; b <- p[2]
      a_has <- !is.null(state[[a]])
      b_has <- !is.null(state[[b]])
      if (a_has && !b_has) {
        assignments[[length(assignments) + 1]] <- list(home = a, away = b, bag = state[[a]], field = field_i)
      } else if (b_has && !a_has) {
        assignments[[length(assignments) + 1]] <- list(home = b, away = a, bag = state[[b]], field = field_i)
      } else if (a_has && b_has) {
        assignments[[length(assignments) + 1]] <- list(home = a, away = b, bag = state[[a]], field = field_i)
        hh <- c(hh, b)
      } else {
        nn <- c(nn, a)
        assignments[[length(assignments) + 1]] <- list(home = a, away = b, bag = NA_integer_, field = field_i)
      }
    }
    # housekeeping: any team sitting out this week shouldn't be sitting on a bag
    for (bt in bye_teams) {
      if (!is.null(state[[bt]]) && !(bt %in% hh)) {
        hh <- c(hh, bt)
      }
    }
    transfers <- list()
    n_pair <- min(length(hh), length(nn))
    if (n_pair > 0) {
      for (k in seq_len(n_pair)) {
        giver <- hh[k]; needer <- nn[k]
        lbl <- state[[giver]]
        state[[giver]] <- NULL
        state[[needer]] <- lbl
        transfers[[length(transfers) + 1]] <- list(from = giver, to = needer, bag = lbl)
        for (idx in seq_along(assignments)) {
          if (identical(assignments[[idx]]$home, needer) && is.na(assignments[[idx]]$bag)) {
            assignments[[idx]]$bag <- lbl
          }
        }
      }
    }
    if (length(nn) > n_pair) {
      for (needer in nn[(n_pair + 1):length(nn)]) {
        state[[needer]] <- next_label
        for (idx in seq_along(assignments)) {
          if (identical(assignments[[idx]]$home, needer) && is.na(assignments[[idx]]$bag)) {
            assignments[[idx]]$bag <- next_label
          }
        }
        next_label <- next_label + 1L
      }
    }
    if (length(hh) > n_pair) {
      for (giver in hh[(n_pair + 1):length(hh)]) {
        state[[giver]] <- NULL
      }
    }
    log[[w]] <- list(assignments = assignments, transfers = transfers, byes = bye_teams)
    # Choose which team keeps each bag heading into next week (free choice,
    # since either team can carry it home after the game) with a one-week
    # lookahead: pick whichever option creates the fewest mismatches next
    # week, while never leaving a bag with a team that's on bye next week.
    if (w < length(schedule)) {
      next_pairs <- schedule[[w + 1]]$pairs
      next_byes <- schedule[[w + 1]]$byes
      if (length(real) > 0) {
        opts <- lapply(real, function(p) p)
        combos <- expand.grid(lapply(opts, function(p) p), stringsAsFactors = FALSE)
        best_cost <- NA
        best_S <- NULL
        for (r in seq_len(nrow(combos))) {
          S <- as.character(unlist(combos[r, ]))
          if (any(next_byes %in% S)) next
          cost <- 0
          for (p in next_pairs) {
            if (p[1] %in% S && p[2] %in% S) cost <- cost + 1
          }
          if (is.na(best_cost) || cost < best_cost) {
            best_cost <- cost
            best_S <- S
          }
        }
        if (is.null(best_S)) {
          # fallback: no combo avoids every bye-holder clash, just minimize cost
          for (r in seq_len(nrow(combos))) {
            S <- as.character(unlist(combos[r, ]))
            cost <- 0
            for (p in next_pairs) {
              if (p[1] %in% S && p[2] %in% S) cost <- cost + 1
            }
            if (is.na(best_cost) || cost < best_cost) {
              best_cost <- cost
              best_S <- S
            }
          }
        }
        old_holders <- intersect(unlist(real), names(state))
        stay <- intersect(best_S, old_holders)
        leave <- setdiff(old_holders, best_S)
        enter <- setdiff(best_S, old_holders)
        leaving_labels <- unlist(lapply(leave, function(t) state[[t]]))
        for (t in leave) state[[t]] <- NULL
        if (length(enter) > 0) {
          for (k in seq_along(enter)) {
            if (k <= length(leaving_labels)) {
              state[[enter[k]]] <- leaving_labels[k]
            } else {
              state[[enter[k]]] <- next_label
              next_label <- next_label + 1L
            }
          }
        }
      }
    }
  }
  log
}
##############################################################################
## 3. SPREADSHEET EXPORT
##############################################################################
# Renders the season as a single sheet laid out the way the league's paper
# schedule has always looked: one two-column block per week (team name +
# a "Bag" column standing in for the old "result" column, since this app
# tracks bag custody rather than scores), one three-row block per field
# (a "Field N" label row, then "away"/"home" rows), a BYE block listing
# every team sitting out that week, and a "Teams" block at the bottom
# showing 1 (played) / 0 (bye) per team per week -- a quick attendance
# check across the season.
# Spells out week numbers ("week one", "week two", ...) up to 52, matching
# the league's existing template.
number_to_word <- function(n) {
  ones <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", "seventeen", "eighteen", "nineteen")
  tens <- c("twenty", "thirty", "forty", "fifty")
  if (n <= 9) return(ones[n])
  if (n <= 19) return(teens[n - 9])
  base <- tens[(n %/% 10) - 1]
  rem <- n %% 10
  if (rem == 0) return(base)
  paste0(base, "-", ones[rem])
}
# "May 15" instead of R's zero-padded "May 15"/"May 05"
fmt_short_date <- function(d) {
  gsub(" 0", " ", format(d, "%b %d"))
}
build_schedule_workbook <- function(teams, dates, bag_log) {
  n_weeks <- length(dates)
  max_byes <- if (n_weeks > 0) max(vapply(bag_log, function(e) length(e$byes), integer(1))) else 0
  max_byes <- max(max_byes, 1)
  n_cols <- 1 + 2 * n_weeks
  col_start <- function(w) 2 + (w - 1) * 2   # team-name column for week w; +1 is the Bag column
  wb <- createWorkbook()
  addWorksheet(wb, "Schedule")
  week_header_style <- createStyle(textDecoration = "bold", halign = "center",
                                    fgFill = "#F2F2F2", border = "TopBottom")
  sub_header_style <- createStyle(textDecoration = "bold", halign = "center")
  section_style <- createStyle(textDecoration = "bold", fgFill = "#DDEBF7")
  label_style <- createStyle(textDecoration = "italic")
  # --- row 1: "week one", "week two", ... merged across each week's 2 cols
  for (w in seq_len(n_weeks)) {
    cs <- col_start(w)
    writeData(wb, "Schedule", paste("week", number_to_word(w)), startRow = 1, startCol = cs)
    if (n_weeks > 0) {
      mergeCells(wb, "Schedule", cols = cs:(cs + 1), rows = 1)
    }
    addStyle(wb, "Schedule", week_header_style, rows = 1, cols = cs:(cs + 1), gridExpand = TRUE)
  }
  # --- row 2: date | "Bag" sub-header
  for (w in seq_len(n_weeks)) {
    cs <- col_start(w)
    writeData(wb, "Schedule", fmt_short_date(dates[w]), startRow = 2, startCol = cs)
    writeData(wb, "Schedule", "Bag", startRow = 2, startCol = cs + 1)
  }
  if (n_cols >= 2) {
    addStyle(wb, "Schedule", sub_header_style, rows = 2, cols = 2:n_cols, gridExpand = TRUE)
  }
  cur_row <- 3
  # --- one 3-row block per field: "Field N" label, then away/home rows
  for (f in ALL_FIELDS) {
    writeData(wb, "Schedule", paste("Field", f), startRow = cur_row, startCol = 1)
    addStyle(wb, "Schedule", section_style, rows = cur_row, cols = 1:n_cols, gridExpand = TRUE)
    away_row <- cur_row + 1
    home_row <- cur_row + 2
    writeData(wb, "Schedule", "away", startRow = away_row, startCol = 1)
    writeData(wb, "Schedule", "home", startRow = home_row, startCol = 1)
    addStyle(wb, "Schedule", label_style, rows = c(away_row, home_row), cols = 1, gridExpand = TRUE)
    for (w in seq_len(n_weeks)) {
      entry <- bag_log[[w]]
      a <- Find(function(x) x$field == f, entry$assignments)
      if (!is.null(a)) {
        cs <- col_start(w)
        bag_txt <- if (is.na(a$bag)) "unassigned" else a$bag
        writeData(wb, "Schedule", a$away, startRow = away_row, startCol = cs)
        writeData(wb, "Schedule", bag_txt, startRow = away_row, startCol = cs + 1)
        writeData(wb, "Schedule", a$home, startRow = home_row, startCol = cs)
        writeData(wb, "Schedule", bag_txt, startRow = home_row, startCol = cs + 1)
      }
    }
    cur_row <- cur_row + 3
  }
  # --- BYE block: one row per bye slot, teams stacked under each week
  bye_row_start <- cur_row
  writeData(wb, "Schedule", "BYE", startRow = bye_row_start, startCol = 1)
  addStyle(wb, "Schedule", createStyle(textDecoration = "bold"), rows = bye_row_start, cols = 1)
  for (k in seq_len(max_byes)) {
    r <- bye_row_start + k - 1
    for (w in seq_len(n_weeks)) {
      byes_w <- bag_log[[w]]$byes
      if (k <= length(byes_w)) {
        writeData(wb, "Schedule", byes_w[k], startRow = r, startCol = col_start(w))
      }
    }
  }
  cur_row <- bye_row_start + max_byes
  # --- Teams block: 1 = played that week, 0 = bye that week
  teams_header_row <- cur_row
  writeData(wb, "Schedule", "Teams", startRow = teams_header_row, startCol = 1)
  addStyle(wb, "Schedule", section_style, rows = teams_header_row, cols = 1:n_cols, gridExpand = TRUE)
  cur_row <- teams_header_row + 1
  for (tm in sort(teams)) {
    writeData(wb, "Schedule", tm, startRow = cur_row, startCol = 1)
    for (w in seq_len(n_weeks)) {
      played <- as.integer(!(tm %in% bag_log[[w]]$byes))
      writeData(wb, "Schedule", played, startRow = cur_row, startCol = col_start(w))
    }
    cur_row <- cur_row + 1
  }
  setColWidths(wb, "Schedule", cols = 1:n_cols, widths = c(16, rep(c(16, 10), n_weeks)))
  freezePane(wb, "Schedule", firstActiveRow = 3, firstActiveCol = 2)
  wb
}
##############################################################################
## 4. SHINY UI
##############################################################################
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body, .form-control, .btn, table.dataTable, .dataTables_wrapper,
      h1, h2, h3, h4, label, .selectize-input {
        font-family: 'Comic Sans MS', 'Comic Neue', cursive, sans-serif !important;
      }
    "))
  ),
  titlePanel("Softball League Scheduler & Game-Bag Tracker"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      numericInput("n_teams", "Number of teams", value = 6, min = 2, max = 30, step = 1),
      numericInput("byes_per_week",
                   paste0("Teams on bye each week (must be 0-", MAX_GAMES_PER_WEEK * 2,
                          " and match the parity of team count, since the rest have to pair up evenly)"),
                   value = 0, min = 0, max = 30, step = 1),
      uiOutput("team_name_inputs"),
      dateInput("start_date", "Season start date (first possible game date)", value = Sys.Date()),
      numericInput("n_weeks", "Number of weeks of games", value = 10, min = 1, max = 52, step = 1),
      textAreaInput(
        "league_blackouts", "League-wide blackout dates (comma-separated, YYYY-MM-DD)",
        value = "", rows = 2,
        placeholder = "e.g. 2026-05-25, 2026-07-04"
      ),
      tags$hr(),
      tags$b("Team-specific blackout dates"),
      uiOutput("team_blackout_inputs"),
      tags$hr(),
      actionButton("generate", "Generate Schedule", class = "btn-primary", width = "100%"),
      tags$br(), tags$br(),
      downloadButton("export_xlsx", "Export Schedule to Excel", style = "width:100%;")
    ),
    mainPanel(
      width = 8,
      uiOutput("conflict_banner"),
      tabsetPanel(
        tabPanel("Schedule & Bags", DTOutput("schedule_table")),
        tabPanel("Off-Day Bag Transfers", DTOutput("transfer_table")),
        tabPanel("Bag Tracker (by bag #)", DTOutput("bag_tracker_table")),
        tabPanel("Field Fairness", DTOutput("field_fairness_table"))
      )
    )
  )
)
##############################################################################
## 5. SHINY SERVER
##############################################################################
server <- function(input, output, session) {
  output$team_name_inputs <- renderUI({
    n <- input$n_teams
    if (is.na(n) || n < 2) return(NULL)
    lapply(seq_len(n), function(i) {
      textInput(paste0("team_", i), paste("Team", i, "name"), value = paste("Team", i))
    })
  })
  # Only rebuild this block when the number of teams changes -- reading the
  # team-name inputs with isolate() keeps a keystroke in a name field from
  # tearing down (and blanking) that team's blackout-date field.
  output$team_blackout_inputs <- renderUI({
    n <- input$n_teams
    if (is.na(n) || n < 2) return(NULL)
    isolate({
      lapply(seq_len(n), function(i) {
        nm <- input[[paste0("team_", i)]]
        if (is.null(nm) || nm == "") nm <- paste("Team", i)
        textInput(
          paste0("blackout_", i),
          paste(nm, "- blackout dates"),
          value = "",
          placeholder = "YYYY-MM-DD, YYYY-MM-DD"
        )
      })
    })
  })
  parse_dates <- function(txt) {
    if (is.null(txt) || trimws(txt) == "") return(as.Date(character(0)))
    parts <- trimws(strsplit(txt, ",")[[1]])
    parts <- parts[parts != ""]
    out <- suppressWarnings(as.Date(parts))
    out[!is.na(out)]
  }
  result <- eventReactive(input$generate, {
    n <- input$n_teams
    validate(need(!is.na(n) && n >= 2, "Enter at least 2 teams."))
    byes <- input$byes_per_week
    validate(need(!is.na(byes) && byes >= 0, "Enter a valid number of byes (0 or more)."))
    validate(need(byes <= n - 2, "You can't bye out more than (teams - 2) -- at least 2 teams need to be left to play a game."))
    validate(need((n - byes) %% 2 == 0,
                   paste0("With ", n, " teams, the number on bye each week must be ",
                          if (n %% 2 == 0) "even" else "odd",
                          " (e.g. ", if (n %% 2 == 0) "0, 2, 4..." else "1, 3, 5...",
                          ") so the rest can pair up evenly.")))
    games_per_week <- (n - byes) / 2
    validate(need(games_per_week <= MAX_GAMES_PER_WEEK,
                   paste0("That's ", games_per_week, " simultaneous games, but fields 9-15 only support ",
                          MAX_GAMES_PER_WEEK, " at once. Increase the byes per week or reduce team count.")))
    teams <- sapply(seq_len(n), function(i) {
      nm <- input[[paste0("team_", i)]]
      if (is.null(nm) || trimws(nm) == "") nm <- paste("Team", i)
      trimws(nm)
    })
    validate(need(length(unique(teams)) == length(teams), "Team names must be unique."))
    team_blackouts <- list()
    for (i in seq_len(n)) {
      bl <- parse_dates(input[[paste0("blackout_", i)]])
      if (length(bl) > 0) team_blackouts[[teams[i]]] <- bl
    }
    league_blackouts <- parse_dates(input$league_blackouts)
    weeks_needed <- input$n_weeks
    validate(need(!is.na(weeks_needed) && weeks_needed >= 1, "Enter a valid number of weeks."))
    dates <- build_calendar(input$start_date, weeks_needed, league_blackouts)
    sched_result <- build_schedule(teams, byes, dates, team_blackouts)
    schedule <- sched_result$schedule
    violations <- sched_result$violations
    bag_log <- route_bags(schedule, teams)
    list(
      teams = teams,
      dates = dates,
      schedule = schedule,
      violations = violations,
      bag_log = bag_log
    )
  })
  output$conflict_banner <- renderUI({
    res <- result()
    if (is.null(res)) return(NULL)
    if (res$violations == 0) {
      div(style = "color: #1a7f37; font-weight: bold; margin-bottom: 10px;",
          "All team blackout dates were successfully avoided.")
    } else {
      div(style = "color: #b42318; font-weight: bold; margin-bottom: 10px;",
          paste0("Heads up: ", res$violations,
                 " game(s) still land on a team's blackout date. This happens when more ",
                 "teams have a blackout on the same date than there are bye slots that ",
                 "week -- increase \"teams on bye each week,\" or check the schedule below ",
                 "and adjust those rows by hand."))
    }
  })
  output$schedule_table <- renderDT({
    res <- result()
    req(res)
    rows <- list()
    for (w in seq_along(res$bag_log)) {
      entry <- res$bag_log[[w]]
      date_w <- res$dates[w]
      for (a in entry$assignments) {
        bag_label <- if (is.na(a$bag)) "unassigned" else paste("Bag", a$bag)
        field_label <- paste0("Field ", a$field, if (a$field %in% LOW_QUALITY_FIELDS) " (low)" else "")
        rows[[length(rows) + 1]] <- data.frame(
          Week = w,
          Date = as.character(date_w),
          Away = a$away,
          At = "@",
          Home = a$home,
          Field = field_label,
          `Bag Carried by Home Team` = bag_label,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }
      for (bt in entry$byes) {
        rows[[length(rows) + 1]] <- data.frame(
          Week = w, Date = as.character(date_w), Away = "", At = "",
          Home = paste(bt, "(BYE)"), Field = "", `Bag Carried by Home Team` = "",
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }
    }
    df <- do.call(rbind, rows)
    datatable(df, rownames = FALSE, options = list(pageLength = 20))
  })
  output$transfer_table <- renderDT({
    res <- result()
    req(res)
    rows <- list()
    for (w in seq_along(res$bag_log)) {
      entry <- res$bag_log[[w]]
      if (length(entry$transfers) > 0) {
        for (t in entry$transfers) {
          rows[[length(rows) + 1]] <- data.frame(
            Week = w,
            Date = as.character(res$dates[w]),
            Instruction = paste0(t$from, " hands Bag ", t$bag, " to ", t$to, " before this week's games"),
            stringsAsFactors = FALSE
          )
        }
      }
    }
    if (length(rows) == 0) {
      df <- data.frame(Week = integer(0), Date = character(0), Instruction = character(0))
    } else {
      df <- do.call(rbind, rows)
    }
    datatable(df, rownames = FALSE, options = list(pageLength = 20))
  })
  output$bag_tracker_table <- renderDT({
    res <- result()
    req(res)
    rows <- list()
    for (w in seq_along(res$bag_log)) {
      entry <- res$bag_log[[w]]
      for (a in entry$assignments) {
        if (!is.na(a$bag)) {
          rows[[length(rows) + 1]] <- data.frame(
            Bag = a$bag, Week = w, Date = as.character(res$dates[w]),
            `Held By (home team)` = a$home, Opponent = a$away,
            check.names = FALSE, stringsAsFactors = FALSE
          )
        }
      }
    }
    df <- do.call(rbind, rows)
    df <- df[order(df$Bag, df$Week), ]
    datatable(df, rownames = FALSE, options = list(pageLength = 25))
  })
  output$field_fairness_table <- renderDT({
    res <- result()
    req(res)
    low_n <- setNames(rep(0L, length(res$teams)), res$teams)
    good_n <- setNames(rep(0L, length(res$teams)), res$teams)
    for (w in seq_along(res$bag_log)) {
      entry <- res$bag_log[[w]]
      for (a in entry$assignments) {
        is_low <- a$field %in% LOW_QUALITY_FIELDS
        for (tm in c(a$home, a$away)) {
          if (is_low) {
            low_n[tm] <- low_n[tm] + 1
          } else {
            good_n[tm] <- good_n[tm] + 1
          }
        }
      }
    }
    total_n <- low_n + good_n
    pct_low <- ifelse(total_n > 0, round(100 * low_n / total_n, 1), 0)
    df <- data.frame(
      Team = res$teams,
      `Games Played` = as.integer(total_n[res$teams]),
      `Low-Quality-Field Games (9, 14, 15)` = as.integer(low_n[res$teams]),
      `Good-Quality-Field Games (10-13)` = as.integer(good_n[res$teams]),
      `% of Games on Low Field` = pct_low[res$teams],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    df <- df[order(-df$`% of Games on Low Field`), ]
    datatable(df, rownames = FALSE, options = list(pageLength = 20))
  })
  output$export_xlsx <- downloadHandler(
    filename = function() {
      paste0("softball_schedule_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
    },
    content = function(file) {
      res <- result()
      req(res)
      wb <- build_schedule_workbook(res$teams, res$dates, res$bag_log)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}
shinyApp(ui = ui, server = server)
