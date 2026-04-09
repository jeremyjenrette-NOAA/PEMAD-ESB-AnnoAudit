# annotator app

library(shiny)
library(dplyr)
library(ggplot2)
library(jpeg)
library(png)
library(readr)
library(tools)
library(magick)

# ---- Specify paths/arguments ----
img_dirname = "/Volumes/PortableSSD/saltnoaa/images/starfish_2024_pred/"
det_path <- "detections/starfish_2024_subset.csv"
IMAGE_SPLIT <- "right"   # options: "none", "left", "right"
class = "starfish"

user_list <- c(
  "Jeremy",
  "Dvora",
  "Han",
  "Conor"
)

# ---- Class-specific type definitions ----
type_options <- list(
  scallop = c("normal", "buried", "clapper", "swimming", "partial"),
  starfish = c("normal", "partial", "buried", "predating"),
  shark = c("normal", "above_water", "below_water")
)

colors <- c(
  normal = "black",
  buried = "orange",
  clapper = "grey89",
  swimming = "cyan",
  partial = "violet",
  predating = "red",
  above_water = "forestgreen",
  below_water = "darkblue"
)

# ---- Output file ----
audit_path <- file.path(
  "annotations",
  paste0(file_path_sans_ext(basename(det_path)), "_annotations.csv")
)

audit_schema <- list(
  annotation_id = "character",
  Imagename = "character",
  TLx = "numeric",
  TLy = "numeric",
  BRx = "numeric",
  BRy = "numeric",
  Conf = "numeric",
  Spname = "character",
  source = "character",
  status = "character",
  audit_datetime = "character",
  user_id = "character",
  type = "character",
  Detectid = "character"
)

empty_audit <- data.frame(
  annotation_id = character(),
  Imagename = character(),
  TLx = numeric(),
  TLy = numeric(),
  BRx = numeric(),
  BRy = numeric(),
  Conf = numeric(),
  Spname = character(),
  source = character(),
  status = character(),
  audit_datetime = character(),
  user_id = character(),
  type = character(),
  stringsAsFactors = FALSE
)

if (!file.exists(audit_path)) {
  write.csv(empty_audit, audit_path, row.names = FALSE)
}

# fallback if class not defined
get_type_choices <- function(class_name) {
  if (class_name %in% names(type_options)) {
    return(type_options[[class_name]])
  } else {
    return(c("type1", "type2", "type3"))
  }
}

safe_read_audit <- function(path) {
  if (!file.exists(path)) {
    return(data.frame(Imagename = character()))
  }
  read_csv(path, show_col_types = FALSE)
}

read_image <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext %in% c("jpg", "jpeg")) {
    return(jpeg::readJPEG(path))
  } else if (ext == "png") {
    return(png::readPNG(path))
  } else {
    stop(paste("Unsupported image format:", ext))
  }
}

load_and_split_image <- function(image_path, split = "none") {
  
  img <- image_read(image_path)
  info <- image_info(img)
  
  w <- info$width
  h <- info$height
  
  if (split == "none") {
    return(img)
    
  } else if (split == "left") {
    return(image_crop(img, geometry = sprintf("%dx%d+0+0", w/2, h)))
    
  } else if (split == "right") {
    return(image_crop(img, geometry = sprintf("%dx%d+%d+0", w/2, h, w/2)))
    
  } else {
    stop("Invalid split option: ", split)
  }
}

coerce_schema <- function(df, schema) {

  # add missing columns
  missing <- setdiff(names(schema), names(df))
  for (m in missing) df[[m]] <- NA

  # enforce types
  for (col in names(schema)) {
    if (schema[[col]] == "character") {
      df[[col]] <- as.character(df[[col]])
    } else if (schema[[col]] == "numeric") {
      df[[col]] <- as.numeric(df[[col]])
    }
  }

  # enforce column order
  df <- df[, names(schema), drop = FALSE]

  return(df)
}

# ---- Read detections + images ----
detections <- read.csv(det_path, stringsAsFactors = FALSE)
all_images <- unique(detections$Imagename)

# ---- UI ----
ui <- fluidPage(
  titlePanel("HabCam Annotator"),
  h5(paste("This app is used to manually validate", class, "detections.")),
  tags$script(HTML("
    $(document).on('keydown', function(e) {
      if (e.key === 'y') { $('#yes').click(); }
      if (e.key === 'n') { $('#no').click(); }
    });

    Shiny.addCustomMessageHandler('click-next', function(message) {
      $('#next_image').click();
    });
  ")),
  sidebarLayout(
sidebarPanel(

  h4("Annotation Review"),
uiOutput("global_progress"),

h4("Navigation"),
fluidRow(
  column(6, actionButton("prev", "Previous", width = "100%")),
  column(6, actionButton("next_image", "Next", width = "100%"))
),

hr(),

selectInput(
  "user_id",
  "Annotator:",
  choices = user_list,
  selected = NULL
),

  uiOutput("progress"),

  fluidRow(
    column(6, actionButton("yes", "✅ Correct", width = "100%")),
    column(6, actionButton("no", "❌ Misclassified", width = "100%"))
  ),
fluidRow(
  column(12, uiOutput("type_inputs"))
),

  hr(),

  h4("Edit"),
    fluidRow(
    column(6, actionButton("add_box", "Add Box", width = "100%")),
    column(6, actionButton("save", " 💾 Save Annotations", width = "100%"))
  ),

  hr(),
downloadButton("export_zip", "📦 Export Annotations"),
hr(),
textOutput("img_name")

  
),

    mainPanel(
      div(
      style = "margin-bottom: 0px;",
      uiOutput("type_legend")
      ),
      plotOutput(
        "image_plot",
        width = "100%",
        height = "700px",
        brush = brushOpts(id = "plot_brush")
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {

  rv <- reactiveValues(
    index = 1,
    boxes = NULL,
    current_box = 1,
    decisions = NULL,
    current_type_val = "normal"
  )
  
  output$type_legend <- renderUI({
    
    choices <- get_type_choices(class)
    
    tags$div(
      style = "display: flex; gap: 15px; align-items: center;",
      
      lapply(choices, function(t) {
        tags$div(
          style = "display: flex; align-items: center; gap: 5px;",
          
          tags$div(
            style = paste0(
              "width: 15px; height: 15px; background-color: ",
              colors[[t]], "; border: 1px solid black;"
            )
          ),
          
          tags$span(t)
        )
      })
    )
  })
  
  output$type_inputs <- renderUI({
    
    boxes <- rv$boxes
    choices <- get_type_choices(class)
    
    model_indices <- which(boxes$source == "model")
    
    current_type <- "normal"
    
    if (length(model_indices) > 0 &&
        rv$current_box <= length(model_indices)) {
      
      idx <- model_indices[rv$current_box]
      
      current_type <- ifelse(
        is.na(boxes$type[idx]),
        "normal",
        boxes$type[idx]
      )
    }
    
    tagList(
      
      # ---- EDIT EXISTING BOX ----
      fluidRow(
        column(12,
               radioButtons(
                 inputId = "current_type",
                 label = "Edit selected box:",
                 choices = choices,
                 selected = current_type,
                 inline = TRUE
               )
        )
      ),
      
      # ---- NEW BOX TYPE ----
      fluidRow(
        column(12,
               radioButtons(
                 inputId = "new_type",
                 label = "New box type:",
                 choices = choices,
                 selected = "normal",
                 inline = TRUE
               )
        )
      )
      
    )
  })
  
output$export_zip <- downloadHandler(
  filename = function() {
    paste0(class,"_annotations_", Sys.Date(), ".zip")
  },
  content = function(file) {
    
    audited <- safe_read_audit(audit_path)
    
    if (nrow(audited) == 0) {
      stop("No annotations to export")
    }
    
    tmp_dir <- tempdir()
    
    # ---- write CSV ----
    csv_path <- file.path(tmp_dir, "annotations.csv")
    write.csv(audited, csv_path, row.names = FALSE)
    
    # ---- process images ----
    img_dir <- img_dirname
    imgs <- unique(audited$Imagename)
    
    exported_imgs <- c()
    
    for (img_name in imgs) {
      
      full_path <- file.path(img_dir, img_name)
      
      if (!file.exists(full_path)) next
      
      # 🔥 APPLY SPLIT HERE
      img <- load_and_split_image(full_path, IMAGE_SPLIT)
      
      # optional: rename to reflect split
      out_name <- paste0(
        tools::file_path_sans_ext(img_name),
        "_", IMAGE_SPLIT, ".png"
      )
      
      out_path <- file.path(tmp_dir, out_name)
      
      magick::image_write(img, out_path)
      
      exported_imgs <- c(exported_imgs, out_name)
    }
    
    # ---- zip ----
    oldwd <- setwd(tmp_dir)
    on.exit(setwd(oldwd))
    
    zip::zip(
      zipfile = file,
      files = c("annotations.csv", exported_imgs)
    )
  }
)

output$global_progress <- renderUI({

  audited <- safe_read_audit(audit_path)

  total <- length(all_images)
  done <- length(unique(audited$Imagename))

  remaining <- total - done

  tagList(
    div(
      style = "
        border: 2px solid #4CAF50;
        padding: 10px;
        border-radius: 6px;
        margin-bottom: 10px;
        text-align: center;
        font-weight: bold;
      ",
      paste0("Progress: ", done, " / ", total)
    ),

    div(
      style = "text-align: center;",
      paste0("Remaining: ", remaining)
    )
  )
})

get_remaining_images <- reactive({

  audited <- safe_read_audit(audit_path)

  if (nrow(audited) == 0) return(all_images)

  remaining <- setdiff(all_images, unique(audited$Imagename))

  return(remaining)
})

observeEvent(input$current_type, {
  rv$current_type_val <- input$current_type
})
  
  # ---- Load image boxes ----
  observeEvent(rv$index, {

    imgs <- get_remaining_images()

    if (length(imgs) == 0) {
      showNotification("All images audited 🎉", type = "message")
      return(NULL)
    }

    img <- imgs[rv$index]

  audited <- safe_read_audit(audit_path)

  if (img %in% audited$Imagename) {
    # ✅ LOAD AUDITED VERSION
    rv$boxes <- audited %>%
      filter(Imagename == img)

  } else {
    # fallback to detections
    rv$boxes <- detections %>%
      filter(Imagename == img) %>%
      mutate(source = "model",
        type = "normal"
      )
    
    rv$boxes$type[is.na(rv$boxes$type)] <- "normal"
  }

    rv$current_box <- 1
    rv$decisions <- rep(NA, nrow(rv$boxes))

    model_indices <- which(rv$boxes$source == "model")
    
    if (length(model_indices) > 0) {
      idx <- model_indices[1]
      rv$current_type_val <- rv$boxes$type[idx]
    }
    
    # 🔥 CLEAR BRUSH ON IMAGE CHANGE
    session$resetBrush("plot_brush")
  })

observeEvent(input$yes, {
  
  model_indices <- which(rv$boxes$source == "model")
  
  if (rv$current_box <= length(model_indices)) {
    
    # 🔥 SAVE TYPE BEFORE MOVING ON
    idx <- model_indices[rv$current_box]
    rv$boxes$type[idx] <- rv$current_type_val
    
    # move forward
    rv$current_box <- rv$current_box + 1
  }
  
  # update next box type
  if (rv$current_box <= length(model_indices)) {
    idx <- model_indices[rv$current_box]
    rv$current_type_val <- rv$boxes$type[idx]
  }
  
})

observeEvent(input$no, {

  model_indices <- which(rv$boxes$source == "model")

  if (rv$current_box <= length(model_indices)) {

    idx <- model_indices[rv$current_box]

    rv$boxes <- rv$boxes[-idx, , drop = FALSE]
  }
})

output$progress <- renderUI({

  model_count <- sum(rv$boxes$source == "model")

  if (model_count == 0) {
    return(div("No model boxes left 🎉"))
  }

  current <- min(rv$current_box, model_count)

  tagList(

    div(
      style = "
        border: 3px solid gold;
        background-color: rgba(255, 255, 0, 0.15);
        padding: 10px;
        border-radius: 6px;
        margin-bottom: 10px;
        font-weight: bold;
        text-align: center;
      ",
      paste0("Reviewing Box ", current)
    ),

    div(
      style = "text-align: center;",
      paste0("Predicted Box ", current, " / ", model_count)
    )
  )
})

  # ---- Navigation ----
  observeEvent(input$next_image, {
  imgs <- get_remaining_images()
  rv$index <- min(rv$index + 1, length(imgs))
})

observeEvent(input$prev, {
  imgs <- get_remaining_images()
  rv$index <- max(rv$index - 1, 1)
})

  # ---- Add box via brush ----
observeEvent(input$add_box, {

  imgs <- get_remaining_images()   # 🔥 ADD THIS

  if (length(imgs) == 0) return()

  b <- input$plot_brush

  if (!is.null(b)) {

    new_box <- data.frame(
      Imagename = imgs[rv$index],
      TLx = b$xmin,
      TLy = b$ymin,
      BRx = b$xmax,
      BRy = b$ymax,
      Conf = 1.0,
      Detectid = paste0("user_", nrow(rv$boxes)),
      source = "user",
      type = input$new_type
    )

    rv$boxes <- bind_rows(rv$boxes, new_box)

    session$resetBrush("plot_brush")
  }
})

observe({
  if (!is.null(input$user_id) && input$user_id != "") {
    updateQueryString(
      paste0("?user=", input$user_id),
      mode = "replace"
    )
  }
})

  # ---- Plot image + boxes ----
  output$image_plot <- renderPlot({

    imgs <- get_remaining_images()

    if (length(imgs) == 0) return(NULL)

    img_name <- imgs[rv$index]

    full_path <- file.path(
      img_dirname,
      img_name
    )

    if (!file.exists(full_path)) {
      print(paste("Missing:", full_path))
      return(NULL)
    }

    # ---- Correct reader for JPG ----
    img <- load_and_split_image(full_path, IMAGE_SPLIT)


    info <- image_info(img)
    w <- info$width
    h <- info$height

    df <- rv$boxes

    model_indices <- which(rv$boxes$source == "model")

    current_box_df <- NULL

    if (length(model_indices) > 0 &&
        rv$current_box <= length(model_indices)) {

      idx <- model_indices[rv$current_box]
      current_box_df <- rv$boxes[idx, ]
    }

    if (rv$current_box > nrow(rv$boxes)) {
      showNotification("All boxes reviewed!", type = "message")
    }
    if (rv$current_box > nrow(rv$boxes)) {
      showNotification("All boxes reviewed! You can now add new ones.", type = "message")
    }

    highlight_layer <- NULL
    highlight_label <- NULL

    if (!is.null(current_box_df) && nrow(current_box_df) > 0) {
      highlight_layer <- geom_rect(
        data = current_box_df,
        aes(xmin = TLx, xmax = BRx, ymin = TLy, ymax = BRy),
        color = "yellow",
        linewidth = 1.5,
        alpha = 0.05
      ) 
      highlight_label <- geom_label(
        data = current_box_df,
        aes(
          x = TLx,
          y = BRy,
          label = paste0(class, "\n", isolate(rv$current_type_val), ": ", round(Conf, 2))
        ),
        fill = "transparent",
        color = "yellow",
        size = 4,
        hjust = 0,
        vjust = 1
      )
    }

    ggplot() +
      annotation_raster(
        img,
        xmin = 0, xmax = w,
        ymin = h, ymax = 0  
      ) +
      geom_rect(
        data = df,
        aes(
          xmin = TLx, xmax = BRx,
          ymin = TLy, ymax = BRy,
          color = type
        ),
        # color = "red",
        alpha = 0.4,
        linewidth = 1
      ) +
      scale_color_manual(values = colors) +
    highlight_layer +
    highlight_label +
      coord_equal(
        xlim = c(0, w),
        ylim = c(h, 0),
        expand = FALSE,
        clip = "off"
      ) +
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank())

  })

  # ---- Save ----
observeEvent(input$save, {

  imgs <- get_remaining_images()

  tryCatch({
    existing <- safe_read_audit(audit_path)
    existing <- coerce_schema(existing, audit_schema)

    existing <- existing %>%
      filter(Imagename != imgs[rv$index])

    model_indices <- which(rv$boxes$source == "model")
    
    if (length(model_indices) > 0 &&
        rv$current_box <= length(model_indices)) {
      
      idx <- model_indices[rv$current_box]
      
      rv$boxes$type[idx] <- rv$current_type_val
    }
    
    if (nrow(rv$boxes) == 0) {
      
      # ---- EMPTY IMAGE CASE ----
      new_data <- data.frame(
        annotation_id = "ann_1",
        Imagename = imgs[rv$index],
        TLx = NA,
        TLy = NA,
        BRx = NA,
        BRy = NA,
        Conf = NA,
        Spname = NA,
        status = "empty",
        audit_datetime = Sys.time(),
        user_id = input$user_id,
        type = "none",
        source = "manual",
        stringsAsFactors = FALSE
      )
      
    } else {
      
      # ---- NORMAL CASE ----
      new_data <- rv$boxes %>%
        mutate(
          annotation_id = paste0("ann_", seq_len(nrow(rv$boxes))),
          Imagename = imgs[rv$index],
          status = "accepted",
          audit_datetime = Sys.time(),
          user_id = input$user_id,
          type = type,
          Spname = class
        )
    }

    new_data <- coerce_schema(new_data, audit_schema)

    out <- bind_rows(existing, new_data)

    write.csv(out, audit_path, row.names = FALSE)

    showNotification("Saved successfully!", type = "message")

  }, error = function(e) {

    showNotification(
      paste("Save failed:", e$message),
      type = "error",
      duration = NULL
    )

  })

  later::later(function() {
    session$sendCustomMessage("click-next", list())
  }, 0.1)

})

  # ---- Display ----
output$img_name <- renderText({
  imgs <- get_remaining_images()
  
  if (length(imgs) == 0) return("No images remaining")
  
  paste(imgs[rv$index])
})

observe({
  rv$index
})

}

shinyApp(ui, server)