#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# import libraries, shiny == data vis for the web app, jpeg == , MASS ==
library(shiny)
library(jpeg)
library(MASS)

# set global variables
global.eye.data <- read.delim("data/Event Statistics - Single.txt", stringsAsFactors = FALSE) # get eye data
global.stimuli <- read.csv("data/stimuli.csv", stringsAsFactors = FALSE) # get stimuli / images
global.colors <- read.csv("data/colors.csv", stringsAsFactors = FALSE) # get colors

global.participants <- merge(data.frame("id" = seq_along(unique(global.eye.data$Participant)), 
                                        "Participant" = unique(global.eye.data$Participant)), 
                                        global.colors) # get participants, merge by ID?

# Give participant eye data R-friendly colors and integer ids
global.eye.data <- merge(global.eye.data, global.participants) 

# create empty data frame for AOIs
global.aois <- data.frame()

# read AOIS if the file exists
if (file.exists("data/aois.csv")) {
    global.aois <- read.csv("data/aois.csv", stringsAsFactors = FALSE)
}

ui <- fluidPage(
   
    # set title with Penn logo
    titlePanel(img(src = "shield-logotype-whitebkgd-RGB-4k.png", height = "38px")),
    
    fluidRow(
        
        # set up stimuli on the left with a column
        column(width = 1, # set width
            radioButtons("select_stimulus",
                         "Stimuli",
                         choices = setNames(seq_along(global.stimuli$Stimulus), global.stimuli$Stimulus)
                         ),
            
            hr(), # horizontal row?
            
            checkboxGroupInput("select_participants", "Participants", # input id, label
                               choiceNames = apply(global.participants, 1, function(p) { # names
                                  # get participant, get icon, change style, put on one line 
                                  span(span(p['Participant']),
                                        span(icon("eye"), style = paste0("color:", p['color.name'])))
                               }),
                               choiceValues = global.participants$Participant, # choice values = participants
                               selected = global.participants$Participant) # initially selected value
        ),

        # set up main column
        column(width = 11, # set up width for the middle
               
            # column for the image, align to center
            column(width = 9,
                align="center",
                # use div with slide; brushOpts sets up brush for aoi
                div(plotOutput("main_slide", height = "100%", width = "100%",
                           brush = brushOpts(id = "aoi_brush", resetOnNew = TRUE)), 
                    style = "margin-top: -55px"),
                conditionalPanel( # set up table with output IF condition satisfies
                    condition = "input.select_view_data.indexOf('aois') > -1",
                    tableOutput("aoi_output_table") # if satisfied, this is the output
                )
            ),
               
            # column for visualize data, downloading data, AOI stats, defining AOIs, etc.
            column(width = 3,   
                checkboxGroupInput("select_view_data", # input id
                                   "Visualize Data", # label
                                   choices = list("AOIs" = "aois",  
                                                  "Scanpaths" = "scanpaths",
                                                  "Heatmap" = "heatmap"),
                                   selected = c("aois")),
                    
                # set up another checkbox set to FALSE by default to be placed next to scanpaths
                div(
                  checkboxInput("scanpath_index", "with fixation index", FALSE),
                  style = "font-size:small;margin-top:-58px;margin-left:100px"
                ),
                    
                #  set up download button div associated with save_image outputid / download handler
                div(
                  downloadButton("save_image", "Save Stimulus Image"),
                  style = "margin-top:36px"
                ),
                    
                br(), # line break
                
                # download data header
                p("Download Data", style = "font-weight:bold"),
                
                # aoi fixation button div with handler, help text 
                downloadButton("export_aoi_eye_data", "Save Fixation Data"),
                helpText("One fixation per row."),
                
                # aoi participant stats button div with handler, help text 
                downloadButton("export_aoi_participant_stats", "Save Participant AOI Stats"),
                helpText("One participant and aoi per row."),
                
                # aoi stats button div with handler, help text 
                downloadButton("export_aoi_stats", "Save AOI Stats"),
                helpText("One aoi per row."),
                
                br(), # line break
                    
                # set up panel only if condition is satisfied
                conditionalPanel(
                    condition = "input.select_view_data.indexOf('aois') > -1", # condition for panel
                    
                    # header and help text
                    p("Define AOIs", style = "font-weight:bold"),
                    helpText("Click and drag to draw a rectangle, then click Make AOI."),
                    
                    actionButton("save_aoi", "Make AOI"), # action button connected to save_aoi input id
                    uiOutput('aoi_ctls'), # tied to aoi_ctls renderUI in server
                    
                    # download to save AOI template currently created
                    downloadButton("export_aoi_template", "Save AOI Template"),
                    
                    # line breaks
                    br(),
                    br(),
                        
                    # set up file input box tied to import_aoi_template event + help text
                    fileInput("import_aoi_template", "Upload AOI Template",
                              multiple = FALSE, # single file upload
                              # MIME type hints at what to expect
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")
                    ),
                    helpText("Uploading a template will clear any AOIs currently displayed on this stimulus.")
                )
            )
        )
    )
)

# Define server logic 
server <- function(input, output) {
    
    #session.stimuli.idx <- NULL
    
    # session object = environment that can be used to access info + functionality related to the session
    session.aois <- global.aois
    
    # set new_aoi_listener to be a reactive value ()
    new_aoi_listener <- reactiveVal()
    
    # set slide_name output to be stimulus from select_stimulus' radio buttons amongst the global.stimuli options
    output$slide_name <- renderText({
        global.stimuli[input$select_stimulus, 'Stimulus']
    })
    
    # set main_slide's output
    output$main_slide <- renderPlot({
        
        # get file path
        path <- normalizePath(file.path(paste0('./www/', global.stimuli[input$select_stimulus, 'Content'])))
        
        # read the file
        jpg <- readJPEG(path, native=T)
        
        # get the resolution, [x, y]
        res <- dim(jpg)[2:1]
        
        
        plot(1,1,
             xlim=c(1,res[1]), # x-axis limit
             ylim=c(res[2],1), # y-axis limit
             asp=1,    # aspect ratio x/y 
             type='n', # no plotting either point or lines
             xaxs='i', # inhibit expansion of axis limits for images 
             yaxs='i',
             xlab='',  # no axis labels
             ylab=''
        )
        
        # draw image
        rasterImage(jpg, 1, res[2], res[1], 1)
        
        #print(input$select_view_data)
        
        # draw heatmap if heatmap is selected in select_view_data checkbox and select_participants is > 0
        if ('heatmap' %in% input$select_view_data) {
            if (length(input$select_participants) > 0) {
                draw_heatmap()
            }
        }
        
        # draw scanpaths if scan paths is selected in select_view_data
        if ('scanpaths' %in% input$select_view_data) { 
            input$select_participants # get participants
            input$scanpath_index # get with or without fixation index from checkbox
            draw_scanpaths() 
        }
        
        # if aois is selected in select_view_data, get new_aoi_listener reactive value and draw aois
        if ('aois' %in% input$select_view_data) { 
            new_aoi_listener()
            draw_aois()
        }
        
    }, height = 640, width = 800)
    
    # download handler for save_image
    output$save_image <- downloadHandler(
        
        # get filename
        filename = function() {
            paste0(global.stimuli[isolate(input$select_stimulus), 'Stimulus'], "-",
                   paste(isolate(input$select_view_data), collapse = "-"), 
                   ".png") 
        },
        
        # get file content
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

    # if save_aoi action button is clicked
    observeEvent(input$save_aoi, {
        
        # set aoi.dims to the aoi brush selected
        aoi.dims <- isolate(input$aoi_brush)
        
        # if the dims are not null (i.e. an AOI is selected)
        if (!is.null(aoi.dims)) {
            
            # get name from select_stimulus checkbox
            stimulus.name <- global.stimuli[isolate(input$select_stimulus), 'Stimulus']
            
            # get stimulus AOIs for specific stimulus from session/global AOIs
            stimulus.aois <- session.aois[session.aois$stimulus.name == stimulus.name, ]
            
            # change aoi index to 1
            stimulus.aoi.index <- 1
            
            # if there are any AOIs saved for the given stimulus
            if (nrow(stimulus.aois) > 0) {
                # update the aoi index accordingly by finding the last one and adding 1
                stimulus.aoi.index <- max(stimulus.aois$aoi.index) + 1
            }
            
            # update the stimulus AOI name by concatenating AOI with the AOI index
            stimulis.aoi.name <- paste0("AOI", sprintf("%03d", stimulus.aoi.index))
            
            # update session AOIs by updating the data frame (<<- assigns to parent/global environment)
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
    
    # draw heatmap function
    draw_heatmap <- function() {
        
        # get content / image
        stimulus.content <- global.stimuli[isolate(input$select_stimulus), 'Content']
        
        # get stimulus name from global eye data
        stimulus.name <- global.eye.data[global.eye.data$Content == stimulus.content, 'Stimulus'][1]
        
        # get eye data from global eye data, specifically info for given stimulus + eye fixation for participants that are selected
        stimulus.eye.data <- global.eye.data[global.eye.data$Stimulus == stimulus.name & 
                                                 global.eye.data$Category.Group == 'Eye' &
                                                 global.eye.data$Category == 'Fixation' &
                                                 global.eye.data$Participant %in% isolate(input$select_participants), ]
        
        # convert x and y fixation position data + event duration data to numeric values
        stimulus.eye.data$Fixation.Position.X..px. <- as.numeric(stimulus.eye.data$Fixation.Position.X..px.)
        stimulus.eye.data$Fixation.Position.Y..px. <- as.numeric(stimulus.eye.data$Fixation.Position.Y..px.)
        stimulus.eye.data$Event.Duration..ms. <- as.numeric(stimulus.eye.data$Event.Duration..ms.)
        
        
        # upsample for kernel density estimation
        hz = 30
        
        # create x and y samples (fixation positions + upsampled duration converted to hertz + divided by 1000)
        x.samples <- rep(stimulus.eye.data$Fixation.Position.X..px., 
                         as.integer(stimulus.eye.data$Event.Duration..ms. * hz / 1000))
        y.samples <- rep(stimulus.eye.data$Fixation.Position.Y..px., 
                         as.integer(stimulus.eye.data$Event.Duration..ms. * hz / 1000))
        
        # kernel density estimation in 2d with x and y samples,
        # n = number of grid points in each direction, lims = limits of rectangle (size of image)
        k <- kde2d(x.samples, y.samples, n = 300, lims = c(0, 1280, 0, 1024))
        
        # set up heatmap colors
        heatmap.colors <- c("#FFFFFF00", "#00FF007A", "#48FF007A", "#91FF007A", "#DAFF007A", 
                                         "#FFDA007A", "#FF91007A", "#FF48007A", "#FF00007A")
        
        # display image of given x, y variables in k, kernel density with given colors,
        # given ranges of x and y, add = TRUE (adds to current plot)
        image(k, col = heatmap.colors, xlim = c(1,res[1]), ylim=c(res[2],1),
              asp=1, type='n', xaxs='i', yaxs='i', xlab='', ylab='', add = TRUE
        )
        
    }
    
    # draw scanpaths function
    draw_scanpaths <- function() {
        # get stimulus content, name and eye data based on which stimulus and participants are selected
        stimulus.content <- global.stimuli[isolate(input$select_stimulus), 'Content']
        stimulus.name <- global.eye.data[global.eye.data$Content == stimulus.content, 'Stimulus'][1]
        stimulus.eye.data <- global.eye.data[global.eye.data$Stimulus == stimulus.name & 
                                                 global.eye.data$Category.Group == 'Eye' &
                                                 global.eye.data$Category == 'Fixation' &
                                                 global.eye.data$Participant %in% isolate(input$select_participants), ]
        
        # convert fixation position y and event duration to numeric values
        stimulus.eye.data$Fixation.Position.Y..px. <- as.numeric(stimulus.eye.data$Fixation.Position.Y..px.)
        stimulus.eye.data$Event.Duration..ms. <- as.numeric(stimulus.eye.data$Event.Duration..ms.)
        
        # pch = plotting character / symbol to use
        # set pch value to 1 == unfilled circles
        pch.val <- 1
        
        # if fixation index is selected, change pch value to 16 == solid circle without a border (19 would add border)
        if (isolate(input$scanpath_index)) {
            pch.val <- 16
        }
        
        # apply function to stimulus.eye.data for each participant
        by(stimulus.eye.data, stimulus.eye.data$Participant, FUN = function(data) {
          
            # draw line segments for given x, y data with given colors, line width = 2, type = 'o'
            lines(x = data$Fixation.Position.X..px.,
                  y = data$Fixation.Position.Y..px., 
                  col = data$color.name,
                  cex = data$Event.Duration..ms./80, # QUESTION: why divided by .8?
                  type = "o",
                  lwd = 2,
                  pch = pch.val
            )
            
            # if fixation index is selected
            if (isolate(input$scanpath_index)) {
              
                # output text in plot for fixation points with labels == index
                text(x = data$Fixation.Position.X..px.,
                     y = data$Fixation.Position.Y..px., 
                     labels = data$Index,
                     col = data$label.color,
                     cex = 1, # size of labels (cex = character expansion)
                     font = 2
                )
            }
        })
    }
    
    # draw AOIs function
    draw_aois <- function() {
        
        # get stimulus name and existing AOIs
        stimulus.name <- global.stimuli[isolate(input$select_stimulus), 'Stimulus']
        stimulus.aois <- session.aois[session.aois$stimulus.name == stimulus.name, ]
        
        # if the selected stimulus has any AOIs
        if (nrow(stimulus.aois) > 0) {
            
            # for each aoi in aois
            for (aoi in 1:nrow(stimulus.aois)) {
                
                # get all four coordinates
                xleft <- stimulus.aois[aoi, 'xleft']
                ybottom <- stimulus.aois[aoi, 'ybottom']
                xright <- stimulus.aois[aoi, 'xright']
                ytop <- stimulus.aois[aoi, 'ytop']
                
                # draw rectangle
                rect(
                    xleft = xleft,
                    ybottom = ybottom,
                    xright = xright,
                    ytop = ytop,
                    col = rgb(1, 0, 0, alpha = 0.5)
                )
                
                # get center value
                center <- c(mean(c(xleft, xright)), mean(c(ybottom, ytop)))
                
                # output text of center position + label from aoi name
                text(center[1], center[2], labels = stimulus.aois[aoi, 'aoi.name'])
            }
        }
    }
    
    # set up empty list
    obsList <- list()
    
    # this renders the AOI controls if the condition in the conditional panel is satisfied
    output$aoi_ctls <- renderUI({
        new_aoi_listener()  # QUESTION: is this getting the reactive variable? where is it being used?
        
        # get stimulus name + aois for that name
        stimulus.name <- global.stimuli[input$select_stimulus, 'Stimulus']
        stimulus.aois <- session.aois[session.aois$stimulus.name == stimulus.name, ]
        
        # create html table with given class
        tags$table(class = "table shiny-table table- spacing-s",
            
            # with one row and two columns (this is the header)
            tags$thead(
                tags$tr(
                    tags$th("AOI Name"),
                    tags$th("Delete AOI")
                )
            ),
            
            # set up body
            tags$tbody(
              
                # create list with a function applied on aoi name (in this case, a)
                tagList(
                        
                    lapply(stimulus.aois$aoi.name, function(a) {
                      
                        # set up button's id by using gsub to replace all spaces with '-'s
                        button.id <- gsub(" ", "-", paste(stimulus.name, a, "delete"))
                        
                        # QUESTION: not sure what this does?
                        if (is.null(obsList[[button.id]])) {
                            obsList[[button.id]] <<- observeEvent(input[[button.id]], { # if button is clicked, update session AOIs to remove current AOI?
                                session.aois <<- session.aois[!(session.aois$stimulus.name == stimulus.name & session.aois$aoi.name == a), ]
                                new_aoi_listener(nrow(session.aois)) # QUESTION?
                            })
                        }
                        
                        # set up one row with name of AOI and the delete button
                        tags$tr(
                            tags$td(a),
                            tags$td(actionButton(button.id, "Delete"))
                        )
                    })
                )
            )
        )
    })
    
    # event handler for when file is selected to import AOI template
    observeEvent(input$import_aoi_template, {
        
        # read file and AOIs from file
        file <- input$import_aoi_template
        aois <- read.csv(file$datapath, header = TRUE)
        
        # update stimulus name, update session AOIs with only AOIs for stimulus
        stimulus.name <- global.stimuli[isolate(input$select_stimulus), 'Stimulus']
        session.aois <<- session.aois[session.aois$stimulus.name != stimulus.name, ]
        
        # update AOI stimulus name + filename in aois data struct read from csv
        aois$stimulus.name <- stimulus.name
        aois$stimulus.filename = global.stimuli[isolate(input$select_stimulus), 'Content']
        
        # update session AOIs with new AOI read in from file
        session.aois <<- rbind(session.aois, aois)
        
        # clear AOI listener (?)
        new_aoi_listener(0)
        
        # QUESTION: update AOI listener with the new number of AOIs?
        new_aoi_listener(nrow(session.aois))
    })
    
    # download handler for saving an AOI template
    output$export_aoi_template <- downloadHandler(
        
        # create filename
        filename = function() {
            paste0(global.stimuli[isolate(input$select_stimulus), 'Stimulus'], "-aoi-template.csv")
        }, 
        
        # create file content for AOI template
        content = function(file) {
            # set up the AOI's columns
            aoi.template.cols <- c('aoi.index', 'aoi.name', 'xleft', 'ybottom', 'xright', 'ytop')
            
            # check if session AOIs is greater than 0 (ergo there is something to be exported)
            if (nrow(session.aois) > 0) {
                stimulus.name <- global.stimuli[isolate(input$select_stimulus), 'Stimulus']
                write.csv(session.aois[session.aois$stimulus.name == stimulus.name, aoi.template.cols], 
                          file = file, row.names = F)
            } else { # QUESTION: write empty csv in else case?
                write.csv(
                    setNames(data.frame(matrix(ncol = length(aoi.template.cols), nrow = 0)), 
                             aoi.template.cols),
                    file = file, row.names = F
                )
            }
        },
        
        # set content type
        contentType = 'text/csv'
    )
    
    # save fixation data download handler
    output$export_aoi_eye_data <- downloadHandler(
      
        # create filename
        filename = function() {
            paste0(global.stimuli[isolate(input$select_stimulus), 'Stimulus'], "-data.csv")
        },
        
        # create file content
        content = function(file) {
            # get eye data 
            eye.data <- annotate_data_aois()
            
            # only get specific columns from eye data
            eye.data <- eye.data[, c('Participant', 'Stimulus', 'X', 'Y', 'Start', 'End', 
                                     'Duration', 'AOI')]
            
            # set the names of each object (header for the above data)
            names(eye.data) <- c('Participant', 'Stimulus', 'Fixation X', 'Fixation Y', 'Fixation Start Time', 'Fixation End Time', 
                                 'Fixation Duration', 'AOI')
            
            # write to csv file
            write.csv(eye.data, file = file, row.names = F)
        },
        
        # set content type
        contentType = 'text/csv'
    )
    
    # AOI participant stats download handler
    output$export_aoi_participant_stats <- downloadHandler(
        
        # create filename
        filename = function() {
            paste0(global.stimuli[isolate(input$select_stimulus), 'Stimulus'], "-participant-aoi-stats.csv")
        },
        
        # create file content
        content = function(file) {
            eye.data.stats <- calculate_aoi_p_stats()
            
            # if the eye data stats are present
            if (nrow(eye.data.stats) > 0) {
                
              # get eye data stats for specific columns
                eye.data.stats <- eye.data.stats[, c('Participant', 'Stimulus', 'AOI', 'AOI.Area', 'Start.first', 'Duration.total', 
                                                     'Duration.mean', 'Start.count', 'Glances', 'Revisits')]
                
                # header for columns
                names(eye.data.stats) <-  c('Participant', 'Stimulus', 'AOI', 'AOI Area', 'Time To First Fixation', 'Dwell Time', 
                                            'Average Fixation Time', 'Fixation Count', 'Glances', 'Revisits')
            }
            
            # write to csv file
            write.csv(eye.data.stats, file = file, row.names = F)
        },
        # set content type
        contentType = 'text/csv'
    )
    
    # AOI stats download handler
    output$export_aoi_stats <- downloadHandler(
      
        # create filename
        filename = function() {
            paste0(global.stimuli[isolate(input$select_stimulus), 'Stimulus'], "-aoi-stats.csv")
        },
        
        # create file content
        content = function(file) {
            eye.data.stats <- calculate_aoi_stats() 
            
            # if eye data stats exist
            if (nrow(eye.data.stats) > 0) {
            
                # get stats for specific columns
                eye.data.stats <- eye.data.stats[, c('Stimulus', 'AOI', 'AOI.Area', 'Participant.id.uniq', 
                                                     'Duration.total.total', 'Duration.total.mean', 'Start.count.total',
                                                      'Start.first.first', 'Glances.total', 'Revisits.total')]
            
                # set header row
                names(eye.data.stats) <-  c('Stimulus', 'AOI', 'AOI Area', 'Participant Hit Count', 
                                            'Total Dwell Time', 'Average Dwell Time', 'Total Fixation Count', 
                                            'Min Time To First Fixation', 'Total Glances Count', 'Total Revisits Count')
            }
            
            # write to csv
            write.csv(eye.data.stats, file = file, row.names = F)
        },
        
        # set content type
        contentType = 'text/csv'
    )
    
    # table output renderer if an AOI exists
    output$aoi_output_table <- renderTable({
        input$select_stimulus # get stimulus
        input$select_participants # get participants
        new_aoi_listener() # QUESTION: get reactive value?
        
        # calculate current AOI stats 
        eye.data.stats <- calculate_aoi_stats()
        
        # if there are no eye data stats, return empty data frame
        if (nrow(eye.data.stats) == 0) return(data.frame())
        
        # if eye data stats exist, pick specific columns
        eye.data.stats <- eye.data.stats[, c('Stimulus', 'AOI', 'AOI.Area', 'Participant.id.uniq', 
                                             'Start.count.total', 'Glances.total', 'Revisits.total')]
        
        # set up header for specific columns
        names(eye.data.stats) <-  c('Stimulus', 'AOI', 'AOI Area', 'Participant Hit Count', 
                                    'Total Fixation Count', 'Total Glances Count', 'Total Revisits Count')
        
        #return eye data stats
        return(eye.data.stats)
    })
    
    # AOI calculations below
    calculate_aoi_stats <- function() {
        
        # calculate participant statistics
        eye.data.p.stats <- calculate_aoi_p_stats()
        
        # QUESTION: if returned value is null, return empty data frame
        if (nrow(eye.data.p.stats) == 0) return(data.frame())

        
        # create a dataframe takes in the aggregate() as arguments
        eye.data.stats <- do.call(data.frame,
                                  # QUESTION: all within cbind() depends on AOI, AOI.Area, and Stimulus?
                                  aggregate(cbind(Duration.total, Start.first, Start.count, Participant.id, Glances, Revisits) ~ AOI + AOI.Area + Stimulus, eye.data.p.stats,
                                            # QUESTION: for all values here, update the above values
                                            FUN = function(x) { 
                                                c(total=as.integer(sum(x)), 
                                                  first=as.integer(min(x)), 
                                                  mean=as.integer(mean(x)), 
                                                  count=as.integer(length(x)),
                                                  uniq=as.integer(length(unique(x))))
                                            }
                                  ))
        
        # order eye data stats alphabetically
        eye.data.stats <- eye.data.stats[order(eye.data.stats$AOI), ] 
        
        return(eye.data.stats)
    }
    
    # calculate participant AOI stats
    calculate_aoi_p_stats <- function() {
        eye.data <- annotate_data_aois()
        
        # if null data returned, return empty data frame
        if (nrow(eye.data) == 0) return(data.frame())
        
        # convert duration + start to numeric values
        eye.data$Duration <- as.numeric(eye.data$Duration)
        eye.data$Start <- as.numeric(eye.data$Start)
        
        # create a dataframe that takes in the aggregate() as an argument
        eye.data.stats <- do.call(data.frame, 
                                  # QUESTION: all within cbind() depends on Glances, AOI, AOI.Area, Participant, Participant.id, Stimulus?
                                  aggregate(cbind(Duration, Start) ~ Glances + AOI + AOI.Area + Participant + Participant.id + Stimulus, eye.data, 
                                            # QUESTION: for all values here, update the above values
                                            FUN = function(x) { 
                                                    c(total=as.integer(sum(x)), 
                                                      first=as.integer(min(x)), 
                                                      mean=as.integer(mean(x)), 
                                                      count=as.integer(length(x)))
                                                }
                                            ))
        
        # set up revisits variable to be the number of glances for a location - 1 (removing first visit)
        eye.data.stats$Revisits <- eye.data.stats$Glances - 1
        
        return(eye.data.stats)
    }
    
    # initial helper function utilized in calculate_aoi_p_stats and calculate_aoi_stats
    annotate_data_aois <- function() {
        
        # get stimulus content and name
        stimulus.content <- global.stimuli[isolate(input$select_stimulus), 'Content']
        stimulus.name <- global.eye.data[global.eye.data$Content == stimulus.content, 'Stimulus'][1]
        
        #get stimulus eye data from global eye data (picking for spiecific stimulus, categories + participants that are selected)
        stimulus.eye.data <- global.eye.data[global.eye.data$Stimulus == stimulus.name & 
                                             global.eye.data$Category.Group == 'Eye' &
                                             global.eye.data$Category == 'Fixation' &
                                             global.eye.data$Participant %in% isolate(input$select_participants), ]
        
        # if the above == 0 then return out of function
        if (nrow(stimulus.eye.data) == 0) return(stimulus.eye.data)
        
        # update stimulus AOIS from session AOIs (for specific stimulus filename)
        stimulus.aois <- session.aois[session.aois$stimulus.filename == stimulus.content, ]
        
        # update fixation position data to numeric value
        stimulus.eye.data$Fixation.Position.X..px. <- as.numeric(as.character(stimulus.eye.data$Fixation.Position.X..px.))
        stimulus.eye.data$Fixation.Position.Y..px. <- as.numeric(as.character(stimulus.eye.data$Fixation.Position.Y..px.))
        
        # empty index + name + area
        stimulus.eye.data$AOI.Name <- ""
        stimulus.eye.data$AOI.Index <- 0
        stimulus.eye.data$AOI.Area <- ""
        
        # if AOIs exist for this stimuli
        if (nrow(stimulus.aois) > 0) {
          
            # loop through all AOIs
            for (i in 1:nrow(stimulus.aois)) {
                # aoi = current AOI
                aoi <- stimulus.aois[i, ]
                
                # calculate current AOI's area
                aoi$aoi.area <- as.integer((aoi$xright - aoi$xleft) * (aoi$ytop - aoi$ybottom))
                
                # for all values in stimulus.eye.data that satisfy given conditions, set AOI.Name to aoi.name
                stimulus.eye.data[stimulus.eye.data$Fixation.Position.X..px. >= aoi$xleft &
                                      stimulus.eye.data$Fixation.Position.X..px. <= aoi$xright &
                                      stimulus.eye.data$Fixation.Position.Y..px. >= aoi$ybottom &
                                      stimulus.eye.data$Fixation.Position.Y..px. <= aoi$ytop, 'AOI.Name'] <- aoi$aoi.name
                
                # for all values in stimulus.eye.data that satisfy given conditions, set AOI.Index to aoi.index
                stimulus.eye.data[stimulus.eye.data$Fixation.Position.X..px. >= aoi$xleft &
                                      stimulus.eye.data$Fixation.Position.X..px. <= aoi$xright &
                                      stimulus.eye.data$Fixation.Position.Y..px. >= aoi$ybottom &
                                      stimulus.eye.data$Fixation.Position.Y..px. <= aoi$ytop, 'AOI.Index'] <- aoi$aoi.index
                
                # for all values in stimulus.eye.data that satisfy given conditions, set AOI.Area to aoi.area
                stimulus.eye.data[stimulus.eye.data$Fixation.Position.X..px. >= aoi$xleft &
                                      stimulus.eye.data$Fixation.Position.X..px. <= aoi$xright &
                                      stimulus.eye.data$Fixation.Position.Y..px. >= aoi$ybottom &
                                      stimulus.eye.data$Fixation.Position.Y..px. <= aoi$ytop, 'AOI.Area'] <- aoi$aoi.area
            }
        }
        
        # create dataframe that takes specific columns from stimulus.eye.data
        df <- stimulus.eye.data[, c('Participant', 'id', 'Stimulus', 'Fixation.Position.X..px.', 'Fixation.Position.Y..px.', 
                                    'Event.Start.Trial.Time..ms.', 'Event.End.Trial.Time..ms.', 'Event.Duration..ms.', 'AOI.Name', 'AOI.Index', 'AOI.Area')]
        
        # set Stimulus variable of df to the stimulus selected
        df$Stimulus <- global.stimuli[isolate(input$select_stimulus), 'Stimulus']
        
        # QUESTION: add "Glaces" column to dataframe, which is set up by going through each participant and running the below function
        df <- cbind(df, 'Glances' = unlist(by(df, df$Participant, 
                                               function (df) {
                                                   # calculate run length encoding for (fixation count)? in df
                                                   glance.table <- data.frame(table(rle(df$AOI.Index)$values))
                                                   
                                                   # apply given function to df for AOI.Index column
                                                   sapply(df$AOI.Index, function(a) { glance.table[glance.table$Var1 == a, 'Freq'] })
                                               }
        )))
        
        # set up header to dataframe
        names(df) <- c('Participant', 'Participant.id', 'Stimulus', 'X', 'Y', 'Start', 'End', 'Duration', 'AOI', 'AOI.Index', 'AOI.Area', 'Glances')
        
        # express area as percent of total
        df[df$AOI.Area != "", 'AOI.Area'] <- sprintf("%.1f%%", 
                                                     (as.integer(df[df$AOI.Area != "", 'AOI.Area']) * 100) / (1289 * 1024))
        
        return(df)
    }
}

# Run the application 
shinyApp(ui = ui, server = server)

