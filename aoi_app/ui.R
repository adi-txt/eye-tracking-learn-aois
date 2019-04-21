# import libraries, shiny == data vis for the web app, jpeg == , MASS ==
library(shiny)
library(jpeg)
library(MASS)


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