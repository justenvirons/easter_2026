library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinyjs)

# ── Haversine distance (meters) ───────────────────────────────────────────────
haversine_m <- function(lat1, lng1, lat2, lng2) {
  R     <- 6371000
  torad <- pi / 180
  phi1  <- lat1 * torad; phi2 <- lat2 * torad
  dphi  <- (lat2 - lat1) * torad
  dlam  <- (lng2 - lng1) * torad
  a     <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  R * 2 * atan2(sqrt(a), sqrt(1 - a))
}

# ── Location data ─────────────────────────────────────────────────────────────
locs <- list(
  list(
    name      = "Wall Lake Boat Launch",
    lat       = 42.52225, lng = -85.37504,
    haiku     = c("Still water, no wake",
                  "Turtle rests on tangled green",
                  "Sisu slips away"),
    hints     = c(
      "This place sits on a lake not far from where you are right now.",
      "Every spring, a sailboat named Sisu and an electric boat slip into the water here. Turtles bask nearby and the shallows grow thick with green.",
      "The key word, scrambled:"
    ),
    scrambled = "HNAULC", letter = "O",
    view_lat  = 42.521, view_lng = -85.374, view_zoom = 13, tolerance = 1500
  ),
  list(
    name      = "The Point on Wall Lake",
    lat       = 42.52055, lng = -85.38935,
    haiku     = c("Chicken Dude wades in",
                  "Easter haunts the shallow tip",
                  "Summer never ends"),
    hints     = c(
      "This is a shallow part of the same lake as Clue 1.",
      "A legendary character known as the Chicken Dude is said to wade here in summer — and so does a certain Easter bunny.",
      "The key word, scrambled:"
    ),
    scrambled = "WOLLAHS", letter = "M",
    view_lat  = 42.521, view_lng = -85.390, view_zoom = 13, tolerance = 1500
  ),
  list(
    name      = "Gull Lake Yacht Club",
    lat       = 42.40250, lng = -85.42540,
    haiku     = c("Come about, ready!",
                  "Young sailors learn to tack here",
                  "Gulls cry overhead"),
    hints     = c(
      "This is a sailing club on a lake in the area.",
      "You have all taken sailing lessons together here. The lake's name belongs to a large white bird.",
      "The key word, scrambled:"
    ),
    scrambled = "GASINIL", letter = "A",
    view_lat  = 42.403, view_lng = -85.415, view_zoom = 12, tolerance = 2000
  ),
  list(
    name      = "Sprinkle Road, Kalamazoo",
    lat       = 42.25917, lng = -85.53111,
    haiku     = c("Sprinkle lights the sign",
                  "Roundabouts loop twice, then straight",
                  "Sweet name marks the turn"),
    hints     = c(
      "This is a highway exit in Michigan on the route to the lake house.",
      "At this exit, roads spin in circles — roundabouts — before you continue on your way toward the lake.",
      "The key word, scrambled:"
    ),
    scrambled = "LEKSRINP", letter = "S",
    view_lat  = 42.26, view_lng = -85.50, view_zoom = 11, tolerance = 2000
  ),
  list(
    name      = "Fruit Acres Farm Stand",
    lat       = 42.17493, lng = -86.30847,
    haiku     = c("Peaches blush and wait",
                  "Sun dries sweetness into rings",
                  "Acres give their fruit"),
    hints     = c(
      "This is a farm stand in southwest Michigan.",
      "Peaches and apples purchased here are later dried by the parents and turned into sweet snacks for you.",
      "The key word, scrambled:"
    ),
    scrambled = "SCEAHPE", letter = "C",
    view_lat  = 42.17, view_lng = -86.31, view_zoom = 12, tolerance = 2000
  ),
  list(
    name      = "Mackinac Island",
    lat       = 45.86364, lng = -84.62347,
    haiku     = c("No engine, no car",
                  "Horse and bicycle carry",
                  "Bridge calls from the mist"),
    hints     = c(
      "This place is in northern Michigan and can only be reached by ferry.",
      "No cars are allowed on this island. People travel by bicycle and horse. A famous bridge shares its name, and grandparents can see it from their porch.",
      "The key word, scrambled:"
    ),
    scrambled = "DALNIS", letter = "L",
    view_lat  = 46.0, view_lng = -84.5, view_zoom = 9, tolerance = 3000
  ),
  list(
    name      = "Piestewa Peak, Phoenix",
    lat       = 33.54821, lng = -112.01956,
    haiku     = c("She climbed before all",
                  "Desert honors her bright name",
                  "Summit bears her flag"),
    hints     = c(
      "This summit is in a desert city in the American Southwest.",
      "Near your Auntie's house in Phoenix, this peak was renamed to honor a Native American soldier — the first American woman to die in combat in the Iraq War.",
      "The key word, scrambled:"
    ),
    scrambled = "KAEP", letter = "O",
    view_lat  = 33.50, view_lng = -112.00, view_zoom = 11, tolerance = 3000
  ),
  list(
    name      = "Georgioupoli, Crete",
    lat       = 35.36125, lng = 24.26127,
    haiku     = c("Blue water shimmers",
                  "Ancient island holds your laugh",
                  "Together you swam"),
    hints     = c(
      "This location is on a Mediterranean island that belongs to Greece.",
      "You all swam together in the warm sea here many years ago. The island's name sounds like rocky, rugged terrain.",
      "The key word, scrambled:"
    ),
    scrambled = "TRECE", letter = "S",
    view_lat  = 35.2, view_lng = 24.9, view_zoom = 9, tolerance = 10000
  ),
  list(
    name      = "Sufi Bari, Bangladesh",
    lat       = 24.12076, lng = 90.49312,
    haiku     = c("Your name marks the land",
                  "Far past Dhaka, kin await",
                  "Roots run deep as wells"),
    hints     = c(
      "This place is in South Asia, not far from a major capital city.",
      "Look closely at the name of this location — your own family surname is part of it. Some of you have recently made the long journey here.",
      "The key word, scrambled:"
    ),
    scrambled = "ADKAH", letter = "E",
    view_lat  = 23.8, view_lng = 90.4, view_zoom = 9, tolerance = 15000
  ),
  list(
    name      = "Intercultural Montessori",
    lat       = 41.88342, lng = -87.78530,
    haiku     = c("Worlds apart, one room",
                  "Ten years past, your paths first crossed",
                  "Friendship found its home"),
    hints     = c(
      "This is a school in a suburb just west of Chicago, Illinois.",
      "More than ten years ago, all of you were students here at the same time. This is where your friendships first began.",
      "The key word, scrambled:"
    ),
    scrambled = "OHCLSO", letter = "T",
    view_lat  = 41.89, view_lng = -87.72, view_zoom = 12, tolerance = 1500
  )
)

# Letters O M A S C L O S E T -> "OMAS CLOSET"
final_letters_display <- c("T", "O", "L", "S", "E", "C", "O", "M", "A", "S")
final_answer_key      <- "OMASCLOSET"

# ── UI ────────────────────────────────────────────────────────────────────────
# The leafletOutput lives in the static UI (section_clue) so it is never
# destroyed when pages change — this keeps Leaflet click events working.
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Easter Egg Hunt 2026"),
    tags$link(rel = "stylesheet", href = "style.css")
  ),

  # ── Section 1: Landing ──────────────────────────────────────────────────────
  div(id = "section_landing",
    div(class = "landing-wrap",
      div(class = "landing-card",
        h1(class = "hunt-title", "The World Egg Hunt"),
        h2(class = "hunt-year",  "Easter 2026"),
        div(class = "rabbit-frame",
          tags$img(src = "rabbit.jpg", alt = "The Easter Rabbit",
                   class = "rabbit-img",
                   onerror = "this.style.display='none';
                     document.getElementById('rabbit-fallback').style.display='block';")
        ),
        div(id = "rabbit-fallback", class = "rabbit-fallback",
          tags$span("\U0001F430")
        ),
        div(class = "landing-blurb",
          p("Somewhere in this world your Easter baskets are waiting."),
          p("But first — prove you know the places that shaped your friendships."),
          p("Near lakes and distant shores, classroom halls and desert peaks."),
          tags$em("Ten clues. Ten places. Can you find them all?")
        ),
        actionButton("btn_start", "Begin the Hunt \u2192",
                     class = "btn-primary btn-lg btn-start")
      )
    )
  ),

  # ── Section 2: Clue pages ───────────────────────────────────────────────────
  # Hidden initially; leafletOutput is stable here — never re-rendered
  shinyjs::hidden(
    div(id = "section_clue",
      div(class = "clue-wrap",
        # Dynamic: progress bar + haiku + hints
        uiOutput("clue_top"),
        # Stable map block
        div(class = "map-wrap",
          div(class = "map-instruction",
            tags$span("\U0001F5FA  Click the map to mark your best guess, then check your answer.")
          ),
          leafletOutput("clue_map", height = "430px"),
          div(class = "map-btn-row",
            shinyjs::disabled(
              actionButton("btn_check", "Check My Answer",
                           class = "btn-primary btn-check")
            )
          )
        ),
        # Dynamic: feedback + next button
        uiOutput("answer_feedback")
      )
    )
  ),

  # ── Section 3: Final puzzle ─────────────────────────────────────────────────
  shinyjs::hidden(
    div(id = "section_final",
      div(class = "final-wrap",
        div(class = "final-card",
          h1(class = "final-title", "\U0001F95A  All Ten Found!"),
          p(class = "final-intro",
            "You've traveled the world — from Wall Lake to Wall Lake by way of three ",
            "continents. Now unscramble these ten letters to find where your Easter ",
            "baskets are hidden:"),
          div(class = "letter-row",
            lapply(final_letters_display, function(l) tags$span(class = "letter-tile", l))
          ),
          div(class = "final-form",
            textInput("final_answer", label = NULL,
                      placeholder = "Type your answer here \u2026",
                      width = "100%"),
            actionButton("btn_final_check", "\U0001F423  Reveal the Hiding Spot!",
                         class = "btn-primary btn-final")
          ),
          uiOutput("final_feedback")
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    page        = 0,
    hints_shown = 0,
    clicked_lat = NULL,
    clicked_lng = NULL,
    checked     = FALSE,
    correct     = FALSE,
    letters     = character(0)
  )

  # Render map even while section is hidden so it's ready on first show
  outputOptions(output, "clue_map", suspendWhenHidden = FALSE)

  # ── Section visibility ───────────────────────────────────────────────────────
  observeEvent(rv$page, {
    shinyjs::toggle("section_landing", condition = rv$page == 0)
    shinyjs::toggle("section_clue",    condition = rv$page >= 1 && rv$page <= 10)
    shinyjs::toggle("section_final",   condition = rv$page >= 11)
  })

  # ── Landing ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_start, {
    rv$page        <- 1
    rv$hints_shown <- 0
    rv$clicked_lat <- NULL
    rv$clicked_lng <- NULL
    rv$checked     <- FALSE
    rv$correct     <- FALSE
  })

  # ── Dynamic clue top (progress + haiku + hints) ──────────────────────────────
  output$clue_top <- renderUI({
    req(rv$page >= 1, rv$page <= 10)
    n   <- rv$page
    loc <- locs[[n]]
    tagList(
      div(class = "progress-row",
        div(class = "progress-outer",
          div(class = "progress-inner",
              style = paste0("width:", round((n - 1) / 10 * 100), "%;"))
        ),
        span(class = "progress-label", paste0("Clue ", n, " of 10"))
      ),
      div(class = "haiku-card",
        div(class = "haiku-inner",
          tags$p(class = "haiku-line", loc$haiku[[1]]),
          tags$hr(class = "haiku-rule"),
          tags$p(class = "haiku-line", loc$haiku[[2]]),
          tags$hr(class = "haiku-rule"),
          tags$p(class = "haiku-line", loc$haiku[[3]])
        )
      ),
      div(class = "hints-wrap",
        div(class = "hint-btn-row",
          actionButton("btn_hint1", "Hint 1", class = "btn-hint"),
          shinyjs::disabled(actionButton("btn_hint2", "Hint 2", class = "btn-hint")),
          shinyjs::disabled(actionButton("btn_hint3", "Hint 3 \u2605",
                                         class = "btn-hint btn-hint3"))
        ),
        uiOutput("hints_display")
      )
    )
  })

  # ── Hints ────────────────────────────────────────────────────────────────────
  observeEvent(input$btn_hint1, { if (rv$hints_shown < 1) rv$hints_shown <- 1 })
  observeEvent(input$btn_hint2, {
    req(rv$hints_shown >= 1)
    if (rv$hints_shown < 2) rv$hints_shown <- 2
  })
  observeEvent(input$btn_hint3, {
    req(rv$hints_shown >= 2)
    if (rv$hints_shown < 3) rv$hints_shown <- 3
  })

  observe({
    req(rv$page >= 1, rv$page <= 10)
    shinyjs::toggleState("btn_hint2", condition = rv$hints_shown >= 1)
    shinyjs::toggleState("btn_hint3", condition = rv$hints_shown >= 2)
  })

  output$hints_display <- renderUI({
    req(rv$page >= 1, rv$page <= 10)
    if (rv$hints_shown == 0) return(NULL)
    loc   <- locs[[rv$page]]
    items <- lapply(seq_len(rv$hints_shown), function(i) {
      if (i == 3) {
        div(class = "hint-item hint-item-3",
          tags$span(class = "hint-label", "Hint 3:"),
          tags$span(loc$hints[[3]]),
          tags$span(class = "scramble-word", loc$scrambled)
        )
      } else {
        div(class = "hint-item",
          tags$span(class = "hint-label", paste0("Hint ", i, ":")),
          tags$span(loc$hints[[i]])
        )
      }
    })
    div(class = "hints-list", items)
  })

  # ── Map ──────────────────────────────────────────────────────────────────────
  # Initial render (page 1). Subsequent clues update via leafletProxy only —
  # the widget itself is never re-created, so click bindings stay intact.
  output$clue_map <- renderLeaflet({
    loc <- locs[[1]]
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      setView(lng = loc$view_lng, lat = loc$view_lat, zoom = loc$view_zoom) |>
      addSearchOSM(options = searchOptions(
        autoCollapse = FALSE, minLength = 2,
        zoom = 14, collapsed = FALSE, position = "topleft"
      ))
  })

  # Re-centre map and clear markers when page advances
  observeEvent(rv$page, {
    req(rv$page >= 1, rv$page <= 10)
    loc <- locs[[rv$page]]
    leafletProxy("clue_map") |>
      clearMarkers() |>
      clearShapes() |>
      setView(lng = loc$view_lng, lat = loc$view_lat, zoom = loc$view_zoom)
  })

  # Enable/disable Check button
  observe({
    shinyjs::toggleState("btn_check",
      condition = !is.null(rv$clicked_lat) && !rv$correct)
  })

  observeEvent(input$clue_map_click, {
    req(rv$page >= 1, rv$page <= 10, !rv$correct)
    rv$clicked_lat <- input$clue_map_click$lat
    rv$clicked_lng <- input$clue_map_click$lng
    rv$checked     <- FALSE
    leafletProxy("clue_map") |>
      clearMarkers() |>
      addMarkers(lng = rv$clicked_lng, lat = rv$clicked_lat,
                 popup = "Your guess")
  })

  # ── Check answer ─────────────────────────────────────────────────────────────
  observeEvent(input$btn_check, {
    req(!is.null(rv$clicked_lat), rv$page >= 1, rv$page <= 10)
    loc  <- locs[[rv$page]]
    dist <- haversine_m(rv$clicked_lat, rv$clicked_lng, loc$lat, loc$lng)
    rv$checked <- TRUE
    if (dist <= loc$tolerance) {
      rv$correct <- TRUE
      rv$letters <- c(rv$letters, loc$letter)
      leafletProxy("clue_map") |>
        clearMarkers() |>
        addCircleMarkers(
          lng = loc$lng, lat = loc$lat,
          radius = 14, color = "#2e7d32",
          fillColor = "#66bb6a", fillOpacity = 0.8, weight = 3,
          popup = paste0("<b>", loc$name, "</b><br>\U0001F95A Found it!")
        )
    }
  })

  output$answer_feedback <- renderUI({
    req(rv$checked)
    if (rv$correct) {
      loc     <- locs[[rv$page]]
      btn_lbl <- if (rv$page < 10) "Next Clue \u2192" else "See Your Letters! \u2192"
      div(class = "feedback feedback-correct",
        div(class = "feedback-inner",
          tags$span(class = "feedback-icon", "\U0001F95A"),
          div(
            tags$strong("Correct! "),
            tags$span(paste0("You found ", loc$name, ".")),
            div(class = "letter-earned",
              tags$span("Letter earned:"),
              tags$span(class = "letter-badge", loc$letter)
            )
          )
        ),
        actionButton("btn_next", btn_lbl, class = "btn-primary btn-next")
      )
    } else {
      div(class = "feedback feedback-wrong",
        tags$span(class = "feedback-icon", "\U0001F430"),
        tags$span("Not quite — keep looking! Try clicking a different spot.")
      )
    }
  })

  # ── Advance ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_next, {
    if (rv$page < 10) {
      rv$page <- rv$page + 1
    } else {
      rv$page <- 11
      return()
    }
    rv$hints_shown <- 0
    rv$clicked_lat <- NULL
    rv$clicked_lng <- NULL
    rv$checked     <- FALSE
    rv$correct     <- FALSE
  })

  # ── Final puzzle ─────────────────────────────────────────────────────────────
  output$final_feedback <- renderUI(NULL)

  observeEvent(input$btn_final_check, {
    guess <- toupper(gsub("[^A-Za-z]", "", input$final_answer))
    if (guess == final_answer_key) {
      output$final_feedback <- renderUI({
        div(class = "final-winner",
          tags$span(class = "winner-icon", "\U0001F389"),
          h2("Oma's Laundry Closet!"),
          p("Happy Easter! \U0001F430\U0001F338\U0001F95A"),
          p(tags$em("Go find your baskets!"))
        )
      })
    } else {
      output$final_feedback <- renderUI({
        div(class = "feedback feedback-wrong",
          tags$span(class = "feedback-icon", "\U0001F430"),
          tags$span("Not quite — keep rearranging those letters!")
        )
      })
    }
  })
}

shinyApp(ui, server)
