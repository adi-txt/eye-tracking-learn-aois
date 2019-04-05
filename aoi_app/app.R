#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jpeg)
library(MASS)

global.eye.data <- read.delim("data/Event Statistics - Single.txt", stringsAsFactors = FALSE)
global.stimuli <- read.csv("data/stimuli.csv", stringsAsFactors = FALSE)
global.colors <- read.csv("data/colors.csv", stringsAsFactors = FALSE)
global.participants <- merge(data.frame("id" = seq_along(unique(global.eye.data$Participant)), 
                                        "Participant" = unique(global.eye.data$Participant)), 
                                        global.colors)
# Give participant eye data R-friendly colors and integer ids
global.eye.data <- merge(global.eye.data, global.participants) 

global.aois <- data.frame()
if (file.exists("data/aois.csv")) {
    global.aois <- read.csv("data/aois.csv", stringsAsFactors = FALSE)
}

ui <- fluidPage(
   
    titlePanel(img(src = "shield-logotype-whitebkgd-RGB-4k.png", height = "38px")),
    
    fluidRow(
        
        column(width = 1,
            radioButtons("select_stimulus", "Stimuli",
                         choices = setNames(seq_along(global.stimuli$Stimulus), global.stimuli$Stimulus),
                         selected = 1),
            hr(),
            
            checkboxGroupInput("select_participants", "Participants", 
                               choiceNames = apply(global.participants, 1, function(p) { 
                                   span(span(p['Participant']), 
                                        span(icon("eye"), style = paste0("color:", p['color.name'])))
                               }),
                               choiceValues = global.participants$Participant,
                               selected = global.participants$Participant)
        ),
        
        column(width = 11,
            
               column(width = 9, 
                      div(plotOutput("main_slide", height = "100%", width = "100%",
                                 brush = brushOpts(id = "aoi_brush", resetOnNew = TRUE)), 
                          style = "margin-top: -54px"),
                      conditionalPanel(
                          condition = "input.select_view_data.indexOf('aois') > -1",
                          tableOutput("aoi_output_table")
                      )
                      
                                 
               ),
               column(width = 3,   
                      checkboxGroupInput("select_view_data", "Visualize Data", 
                                         choices = list("AOIs" = "aois", 
                                                        "Scanpaths" = "scanpaths",
                                                        "Heatmap" = "heatmap"),
                                         selected = c("aois")),
                      div(
                        checkboxInput("scanpath_index", "with fixation index", FALSE),
                        style = "font-size:small;margin-top:-58px;margin-left:100px"
                      ),
                      div(
                        downloadButton("save_image", "Save Stimulus Image"),
                        style = "margin-top:36px"
                      ),
                      br(),
                      p("Download Data", style = "font-weight:bold"),
                      downloadButton("export_aoi_eye_data", "Save Fixation Data"),
                      helpText("One fixation per row."),
                      downloadButton("export_aoi_participant_stats", "Save Participant AOI Stats"),
                      helpText("One participant and aoi per row."),
                      downloadButton("export_aoi_stats", "Save AOI Stats"),
                      helpText("One aoi per row."),
                      br(),
                      conditionalPanel(
                          condition = "input.select_view_data.indexOf('aois') > -1",
                          p("Define AOIs", style = "font-weight:bold"),
                          helpText("Click and drag to draw a rectangle, then click Make AOI."),
                          actionButton("save_aoi", "Make AOI"),
                          uiOutput('aoi_ctls'),
                          downloadButton("export_aoi_template", "Save AOI Template"),
                          br(),br(),
                          fileInput("import_aoi_template", "Upload AOI Template",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          helpText("Uploading a template will clear any AOIs currently displayed on this stimulus.")
                      )
            )
        )
    )
)

# Define server logic 
server <- function(input, output) {
    
    #session.stimuli.idx <- NULL
    session.aois <- global.aois
    
    new_aoi_listener <- reactiveVal()
    
    output$slide_name <- renderText({
        global.stimuli[input$select_stimulus, 'Stimulus']
    })
    
    output$main_slide <- renderPlot({
        
        path <- normalizePath(file.path(paste0('./www/', global.stimuli[input$select_stimulus, 'Content'])))
        jpg <- readJPEG(path, native=T) # read the file
        res <- dim(jpg)[2:1] # get the resolution, [x, y]
        plot(1,1,
             xlim=c(1,res[1]),
             ylim=c(res[2],1),
             asp=1,    # aspect ratio x/y 
             type='n', # no plotting either point or lines
             xaxs='i', # inhibit expansion of axis limits for images 
             yaxs='i',
             xlab='',  # no axis labels
             ylab=''
        )
        rasterImage(jpg, 1, res[2], res[1], 1)
        #print(input$select_view_data)
        if ('heatmap' %in% input$select_view_data) {
            if (length(input$select_participants) > 0) {
                draw_heatmap()
            }
        }
        if ('scanpaths' %in% input$select_view_data) { 
            input$select_participants
            input$scanpath_index
            draw_scanpaths() 
        }
        if ('aois' %in% input$select_view_data) { 
            new_aoi_listener()
            draw_aois()
        }
    }, height = 640, width = 800)
    
    output$save_image <- downloadHandler(
        filename = function() {
            paste0(global.stimuli[isolate(input$select_stimulus), 'Stimulus'], "-",
                   paste(isolate(input$select_view_data), collapse = "-"), 
                   ".png") 
        }, 
        content = function(file) {
            
            path <- normalizePath(file.path(paste0('./www/', global.stimuli[isolate(input$select_stimulus), 'Content'])))
            jpg <- readJPEG(path, native=T) # read the file
            res <- dim(jpg)[2:1] # get the resolution, [x, y]
            png(file, height = res[2], width = res[1])
            plot(1,1,
                 xlim=c(1,res[1]),
                 ylim=c(res[2], 1),
                 asp=1,    # aspect ratio x/y 
                 type='n', # no plotting either point or lines
                 xaxs='i', # inhibit expansion of axis limits for images 
                 yaxs='i', # 
                 xaxt='n', # no x or y axis visualization
                 yaxt='n', # 
                 xlab='',  # no axis labels
                 ylab='',  #
                 bty='n'   # no box border around plot area
            )
            rasterImage(jpg, 1, res[2], res[1], 1)
            
            if ('heatmap' %in% isolate(input$select_view_data)) {
                if (length(isolate(input$select_participants)) > 0) {
                    draw_heatmap()
                }
            }
            if ('scanpaths' %in% isolate(input$select_view_data)) { 
                draw_scanpaths() 
            }
            if ('aois' %in% isolate(input$select_view_data)) { 
                draw_aois()
            }

            dev.off()
        }, contentType = 'image/png')

    
    observeEvent(input$save_aoi, {
        aoi.dims <- isolate(input$aoi_brush)
        if (!is.null(aoi.dims)) {
            
            stimulus.name <- global.stimuli[isolate(input$select_stimulus), 'Stimulus']
            stimulus.aois <- session.aois[session.aois$stimulus.name == stimulus.name, ]
            
            stimulus.aoi.index <- 1
            if (nrow(stimulus.aois) > 0) {
                stimulus.aoi.index <- max(stimulus.aois$aoi.index) + 1
            }
            stimulis.aoi.name <- paste0("AOI", sprintf("%03d", stimulus.aoi.index))
            
            session.aois <<- rbind(session.aois, data.frame(
                stimulus.name = stimulus.name,
                stimulus.filename = global.stimuli[isolate(input$select_stimulus), 'Content'],
                aoi.index = stimulus.aoi.index,
                aoi.name = stimulis.aoi.name,
                xleft =  as.integer(aoi.dims$xmin),
                ybottom	=  as.integer(aoi.dims$ymin),
                xright	=  as.integer(aoi.dims$xmax),
                ytop =  as.integer(aoi.dims$ymax),
                stringsAsFactors = FALSE
            ))
            
            # update plot
            new_aoi_listener(nrow(session.aois))
        }
    })
    
    draw_heatmap <- function() {
        
        stimulus.content <- global.stimuli[isolate(input$select_stimulus), 'Content']
        stimulus.name <- global.eye.data[global.eye.data$Content == stimulus.content, 'Stimulus'][1]
        stimulus.eye.data <- global.eye.data[global.eye.data$Stimulus == stimulus.name & 
                                                 global.eye.data$Category.Group == 'Eye' &
                                                 global.eye.data$Category == 'Fixation' &
                                                 global.eye.data$Participant %in% isolate(input$select_participants), ]
        
        stimulus.eye.data$Fixation.Position.X..px. <- as.numeric(stimulus.eye.data$Fixation.Position.X..px.)
        stimulus.eye.data$Fixation.Position.Y..px. <- as.numeric(stimulus.eye.data$Fixation.Position.Y..px.)
        stimulus.eye.data$Event.Duration..ms. <- as.numeric(stimulus.eye.data$Event.Duration..ms.)
        
        # upsample for kernel density estimation
        hz = 30
        x.samples <- rep(stimulus.eye.data$Fixation.Position.X..px., 
                         as.integer(stimulus.eye.data$Event.Duration..ms. * hz / 1000))
        y.samples <- rep(stimulus.eye.data$Fixation.Position.Y..px., 
                         as.integer(stimulus.eye.data$Event.Duration..ms. * hz / 1000))
        
        k <- kde2d(x.samples, y.samples, n = 300, lims = c(0, 1280, 0, 1024))
        
        heatmap.colors <- c("#FFFFFF00", "#00FF007A", "#48FF007A", "#91FF007A", "#DAFF007A", 
                                         "#FFDA007A", "#FF91007A", "#FF48007A", "#FF00007A")
        image(k, col = heatmap.colors, xlim = c(1,res[1]), ylim=c(res[2],1),
              asp=1, type='n', xaxs='i', yaxs='i', xlab='', ylab='', add = TRUE
        )
        
    }
    
    draw_scanpaths <- function() {
        stimulus.content <- global.stimuli[isolate(input$select_stimulus), 'Content']
        stimulus.name <- global.eye.data[global.eye.data$Content == stimulus.content, 'Stimulus'][1]
        stimulus.eye.data <- global.eye.data[global.eye.data$Stimulus == stimulus.name & 
                                                 global.eye.data$Category.Group == 'Eye' &
                                                 global.eye.data$Category == 'Fixation' &
                                                 global.eye.data$Participant %in% isolate(input$select_participants), ]
        
        stimulus.eye.data$Fixation.Position.Y..px. <- as.numeric(stimulus.eye.data$Fixation.Position.Y..px.)
        stimulus.eye.data$Event.Duration..ms. <- as.numeric(stimulus.eye.data$Event.Duration..ms.)
        
        pch.val <- 1
        if (isolate(input$scanpath_index)) {
            pch.val <- 16
        }
        
        by(stimulus.eye.data, stimulus.eye.data$Participant, FUN = function(data) {
            lines(x = data$Fixation.Position.X..px.,
                  y = data$Fixation.Position.Y..px., 
                  col = data$color.name,
                  cex = data$Event.Duration..ms./80,
                  type = "o",
                  lwd = 2,
                  pch = pch.val
            )
            if (isolate(input$scanpath_index)) {
                text(x = data$Fixation.Position.X..px.,
                      y = data$Fixation.Position.Y..px., 
                     labels = data$Index,
                      col = data$label.color,
                     cex = 1,
                     font = 2
                )
            }
        })
    }
    
    draw_aois <- function() {
        stimulus.name <- global.stimuli[isolate(input$select_stimulus), 'Stimulus']
        stimulus.aois <- session.aois[session.aois$stimulus.name == stimulus.name, ]
        if (nrow(stimulus.aois) > 0) {
            for (aoi in 1:nrow(stimulus.aois)) {
                xleft <- stimulus.aois[aoi, 'xleft']
                ybottom <- stimulus.aois[aoi, 'ybottom']
                xright <- stimulus.aois[aoi, 'xright']
                ytop <- stimulus.aois[aoi, 'ytop']
                rect(
                    xleft = xleft,
                    ybottom = ybottom,
                    xright = xright,
                    ytop = ytop,
                    col = rgb(1, 0, 0, alpha = 0.5)
                )
                center <- c(mean(c(xleft, xright)), mean(c(ybottom, ytop)))
                text(center[1], center[2], labels = stimulus.aois[aoi, 'aoi.name'])
            }
        }
    }
    
    obsList <- list()
    output$aoi_ctls <- renderUI({
        new_aoi_listener()
        stimulus.name <- global.stimuli[input$select_stimulus, 'Stimulus']
        stimulus.aois <- session.aois[session.aois$stimulus.name == stimulus.name, ]
        
        tags$table(class = "table shiny-table table- spacing-s",
            tags$thead(
                tags$tr(
                    tags$th("AOI Name"),
                    tags$th("Delete AOI")
                )
            ),
            tags$tbody(
                tagList(
                    lapply(stimulus.aois$aoi.name, function(a) {
                        button.id <- gsub(" ", "-", paste(stimulus.name, a, "delete"))
                        if (is.null(obsList[[button.id]])) {
                            obsList[[button.id]] <<- observeEvent(input[[button.id]], {
                                session.aois <<- session.aois[!(session.aois$stimulus.name == stimulus.name & session.aois$aoi.name == a), ]
                                new_aoi_listener(nrow(session.aois))
                            })
                        }
                        tags$tr(
                            tags$td(a),
                            tags$td(actionButton(button.id, "Delete"))
                        )
                    })
                )
            )
        )
    })
    
    observeEvent(input$import_aoi_template, {
        file <- input$import_aoi_template
        aois <- read.csv(file$datapath, header = TRUE)
        
        stimulus.name <- global.stimuli[isolate(input$select_stimulus), 'Stimulus']
        session.aois <<- session.aois[session.aois$stimulus.name != stimulus.name, ]
        
        aois$stimulus.name <- stimulus.name
        aois$stimulus.filename = global.stimuli[isolate(input$select_stimulus), 'Content']
        
        session.aois <<- rbind(session.aois, aois)
        new_aoi_listener(0)
        new_aoi_listener(nrow(session.aois))
    })
    
    output$export_aoi_template <- downloadHandler(
        filename = function() {
            paste0(global.stimuli[isolate(input$select_stimulus), 'Stimulus'], "-aoi-template.csv")
        }, 
        content = function(file) {
            aoi.template.cols <- c('aoi.index', 'aoi.name', 'xleft', 'ybottom', 'xright', 'ytop')
            if (nrow(session.aois) > 0) {
                stimulus.name <- global.stimuli[isolate(input$select_stimulus), 'Stimulus']
                write.csv(session.aois[session.aois$stimulus.name == stimulus.name, aoi.template.cols], 
                          file = file, row.names = F)
            } else {
                write.csv(
                    setNames(data.frame(matrix(ncol = length(aoi.template.cols), nrow = 0)), 
                             aoi.template.cols),
                    file = file, row.names = F
                )
            }
            
        },
        contentType = 'text/csv'

    )
    
    output$export_aoi_eye_data <- downloadHandler(
        filename = function() {
            paste0(global.stimuli[isolate(input$select_stimulus), 'Stimulus'], "-data.csv")
        },
        content = function(file) {
            eye.data <- annotate_data_aois()
            eye.data <- eye.data[, c('Participant', 'Stimulus', 'X', 'Y', 'Start', 'End', 
                                     'Duration', 'AOI')]
            names(eye.data) <- c('Participant', 'Stimulus', 'Fixation X', 'Fixation Y', 'Fixation Start Time', 'Fixation End Time', 
                                 'Fixation Duration', 'AOI')
            write.csv(eye.data, file = file, row.names = F)
        },
        contentType = 'text/csv'
    )
    
    output$export_aoi_participant_stats <- downloadHandler(
        filename = function() {
            paste0(global.stimuli[isolate(input$select_stimulus), 'Stimulus'], "-participant-aoi-stats.csv")
        },
        content = function(file) {
            eye.data.stats <- calculate_aoi_p_stats()
            if (nrow(eye.data.stats) > 0) {
                eye.data.stats <- eye.data.stats[, c('Participant', 'Stimulus', 'AOI', 'AOI.Area', 'Start.first', 'Duration.total', 
                                                     'Duration.mean', 'Start.count', 'Glances', 'Revisits')]
                names(eye.data.stats) <-  c('Participant', 'Stimulus', 'AOI', 'AOI Area', 'Time To First Fixation', 'Dwell Time', 
                                            'Average Fixation Time', 'Fixation Count', 'Glances', 'Revisits')
            }
            write.csv(eye.data.stats, file = file, row.names = F)
        },
        contentType = 'text/csv'
    )
    
    
    output$export_aoi_stats <- downloadHandler(
        filename = function() {
            paste0(global.stimuli[isolate(input$select_stimulus), 'Stimulus'], "-aoi-stats.csv")
        },
        content = function(file) {
            eye.data.stats <- calculate_aoi_stats()
            
            if (nrow(eye.data.stats) > 0) {
            
                eye.data.stats <- eye.data.stats[, c('Stimulus', 'AOI', 'AOI.Area', 'Participant.id.uniq', 
                                                     'Duration.total.total', 'Duration.total.mean', 'Start.count.total',
                                                      'Start.first.first', 'Glances.total', 'Revisits.total')]
            
                names(eye.data.stats) <-  c('Stimulus', 'AOI', 'AOI Area', 'Participant Hit Count', 
                                            'Total Dwell Time', 'Average Dwell Time', 'Total Fixation Count', 
                                            'Min Time To First Fixation', 'Total Glances Count', 'Total Revisits Count')
            }
            write.csv(eye.data.stats, file = file, row.names = F)
        },
        contentType = 'text/csv'
    )
    
    output$aoi_output_table <- renderTable({
        input$select_stimulus
        input$select_participants
        new_aoi_listener()
        eye.data.stats <- calculate_aoi_stats()  
        if (nrow(eye.data.stats) == 0) return(data.frame())
        eye.data.stats <- eye.data.stats[, c('Stimulus', 'AOI', 'AOI.Area', 'Participant.id.uniq', 
                                             'Start.count.total', 'Glances.total', 'Revisits.total')]
        
        names(eye.data.stats) <-  c('Stimulus', 'AOI', 'AOI Area', 'Participant Hit Count', 
                                    'Total Fixation Count', 'Total Glances Count', 'Total Revisits Count')
        return(eye.data.stats)
    })
    
    # AOI calculations below
    
    calculate_aoi_stats <- function() {
        eye.data.p.stats <- calculate_aoi_p_stats()
        
        if (nrow(eye.data.p.stats) == 0) return(data.frame())

        eye.data.stats <- do.call(data.frame, 
                                  aggregate(cbind(Duration.total, Start.first, Start.count, Participant.id, Glances, Revisits) ~ AOI + AOI.Area + Stimulus, eye.data.p.stats,
                                            FUN = function(x) { 
                                                c(total=as.integer(sum(x)), 
                                                  first=as.integer(min(x)), 
                                                  mean=as.integer(mean(x)), 
                                                  count=as.integer(length(x)),
                                                  uniq=as.integer(length(unique(x))))
                                            }
                                  ))
        
        eye.data.stats <- eye.data.stats[order(eye.data.stats$AOI), ] 
        
        return(eye.data.stats)
    }
    
    calculate_aoi_p_stats <- function() {
        eye.data <- annotate_data_aois()
        if (nrow(eye.data) == 0) return(data.frame())
        eye.data$Duration <- as.numeric(eye.data$Duration)
        eye.data$Start <- as.numeric(eye.data$Start)
        eye.data.stats <- do.call(data.frame, 
                                  aggregate(cbind(Duration, Start) ~ Glances + AOI + AOI.Area + Participant + Participant.id + Stimulus, eye.data, 
                                            FUN = function(x) { 
                                                    c(total=as.integer(sum(x)), 
                                                      first=as.integer(min(x)), 
                                                      mean=as.integer(mean(x)), 
                                                      count=as.integer(length(x)))
                                                }
                                            ))
        
        eye.data.stats$Revisits <- eye.data.stats$Glances - 1
        
        return(eye.data.stats)
    }
    
    annotate_data_aois <- function() {
        
        stimulus.content <- global.stimuli[isolate(input$select_stimulus), 'Content']
        stimulus.name <- global.eye.data[global.eye.data$Content == stimulus.content, 'Stimulus'][1]
        stimulus.eye.data <- global.eye.data[global.eye.data$Stimulus == stimulus.name & 
                                             global.eye.data$Category.Group == 'Eye' &
                                             global.eye.data$Category == 'Fixation' &
                                             global.eye.data$Participant %in% isolate(input$select_participants), ]
        
        if (nrow(stimulus.eye.data) == 0) return(stimulus.eye.data)
        
        stimulus.aois <- session.aois[session.aois$stimulus.filename == stimulus.content, ]
        stimulus.eye.data$Fixation.Position.X..px. <- as.numeric(as.character(stimulus.eye.data$Fixation.Position.X..px.))
        stimulus.eye.data$Fixation.Position.Y..px. <- as.numeric(as.character(stimulus.eye.data$Fixation.Position.Y..px.))
        stimulus.eye.data$AOI.Name <- ""
        stimulus.eye.data$AOI.Index <- 0
        stimulus.eye.data$AOI.Area <- ""
        
        if (nrow(stimulus.aois) > 0) {
            for (i in 1:nrow(stimulus.aois)) {
                aoi <- stimulus.aois[i, ]
                aoi$aoi.area <- as.integer((aoi$xright - aoi$xleft) * (aoi$ytop - aoi$ybottom))
                stimulus.eye.data[stimulus.eye.data$Fixation.Position.X..px. >= aoi$xleft &
                                      stimulus.eye.data$Fixation.Position.X..px. <= aoi$xright &
                                      stimulus.eye.data$Fixation.Position.Y..px. >= aoi$ybottom &
                                      stimulus.eye.data$Fixation.Position.Y..px. <= aoi$ytop, 'AOI.Name'] <- aoi$aoi.name
                
                stimulus.eye.data[stimulus.eye.data$Fixation.Position.X..px. >= aoi$xleft &
                                      stimulus.eye.data$Fixation.Position.X..px. <= aoi$xright &
                                      stimulus.eye.data$Fixation.Position.Y..px. >= aoi$ybottom &
                                      stimulus.eye.data$Fixation.Position.Y..px. <= aoi$ytop, 'AOI.Index'] <- aoi$aoi.index
                
                stimulus.eye.data[stimulus.eye.data$Fixation.Position.X..px. >= aoi$xleft &
                                      stimulus.eye.data$Fixation.Position.X..px. <= aoi$xright &
                                      stimulus.eye.data$Fixation.Position.Y..px. >= aoi$ybottom &
                                      stimulus.eye.data$Fixation.Position.Y..px. <= aoi$ytop, 'AOI.Area'] <- aoi$aoi.area
            }
        }
        
        df <- stimulus.eye.data[, c('Participant', 'id', 'Stimulus', 'Fixation.Position.X..px.', 'Fixation.Position.Y..px.', 
                                    'Event.Start.Trial.Time..ms.', 'Event.End.Trial.Time..ms.', 'Event.Duration..ms.', 'AOI.Name', 'AOI.Index', 'AOI.Area')]
        
        df$Stimulus <- global.stimuli[isolate(input$select_stimulus), 'Stimulus']
        
        df <- cbind(df, 'Glances' = unlist(by(df, df$Participant, 
                                               function (df) { 
                                                   glance.table <- data.frame(table(rle(df$AOI.Index)$values))
                                                   sapply(df$AOI.Index, function(a) { glance.table[glance.table$Var1 == a, 'Freq'] })
                                               }
        )))
        names(df) <- c('Participant', 'Participant.id', 'Stimulus', 'X', 'Y', 'Start', 'End', 'Duration', 'AOI', 'AOI.Index', 'AOI.Area', 'Glances')
        
        # express area as percent of total
        df[df$AOI.Area != "", 'AOI.Area'] <- sprintf("%.1f%%", 
                                                     (as.integer(df[df$AOI.Area != "", 'AOI.Area']) * 100) / (1289 * 1024))
        
        return(df)
    }
}

# Run the application 
shinyApp(ui = ui, server = server)

