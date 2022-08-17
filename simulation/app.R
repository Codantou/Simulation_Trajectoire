library(argonDash)
library(argonR)
library(dplyr)
library(stringr)
library(GoFKernel)
library(stringr)
library(plotly)
library(gganimate)
library(shinyWidgets)

replace_t = function(expr){
  expr_list = str_split(expr,"") %>% unlist()
  Map(function(x){
    if(x %in% letters[-20]) gsub(x,"t",x)
    else x
  }, expr_list) %>% unlist() %>%
    paste0(collapse = "")
}

shiny::shinyApp(
  ui = argonDashPage(
    title = "Simulation",
    author = "Abdoul Oudouss",
    description = "Argon Dash Test",footer = argonDashFooter(copyrights = "@Abdoul Oudouss DIAKITE",src = "https://github.com/AODiakite"),
    sidebar = argonDashSidebar(
      vertical = T,skin = "dark",
      background = "white",size = "lg",
      side = "left",
      id = "my_sidebar",
      brand_url = "https://github.com/Codantou/Simulation_Trajectoire",
      brand_logo ="logoApp.png",
      dropdownMenus = argonDropNav(),
      argonSidebarHeader(title = "Main Menu"),
      argonSidebarMenu(
        argonSidebarItem(
          tabName = "Accueil",
          icon = argonIcon(name = "app", color = "info"),
          "Accueil"
        ),
        argonSidebarItem(
          tabName = "Simulation",
          icon = argonIcon(name = "settings", color = "green"),
          "Simulation"
        )
        ),
      argonSidebarDivider()
    ),



    #---------- Body
    body = argonDashBody(
      argonTabItems(

        argonTabItem(
          tabName = "Accueil",
          argonH1("Accueil", display = 4),
          argonRow(
            argonCard(

              width = 12,
              src = NULL,
              icon = icon("cogs"),
              status = "success",
              shadow = TRUE,
              border_level = 0,
              hover_shadow = TRUE,
              title = "Introduction",hover_lift = F,
              shadow_size = 160,gradient = F,

              "Guide d'utilisation de l'application"


            )
          )
        ),
        argonTabItem(
          tabName = "Simulation",
          # info cards
          argonH1("Indicateurs", display = 4),
          uiOutput("cards_info"),

          # classic cards
          argonRow(
            argonColumn(width = 12,
                        argonCard(
                          width = 12,collapsed = T,
                          src = NULL,
                          icon = icon("cogs"),
                          status = "info",
                          shadow = TRUE,
                          border_level = 0,
                          hover_shadow = TRUE,
                          title = "Paramètres",hover_lift = F,
                          shadow_size = 120,gradient = F,background_color = "default",

                          argonRow(
                            argonColumn(width = 6,
                                        textInput("x_t","x(t)",value = "60*t*cos(45)")
                                        ),
                            argonColumn(width = 6,
                                       textInput("y_t","y(t)","(-9.8/2)*t^2+60*t*sin(45)")
                            ),
                            actionButton("Simuler","Simuler",
                                       icon = icon( "play"),class = "btn-block")
                          )
                        )
            ),
            argonColumn(width = 12,
                        argonTabSet(
                          id = "tab-1",
                          card_wrapper = TRUE,
                          horizontal = TRUE,
                          circle = FALSE,
                          size = "sm",
                          width = 12,
                          iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
                          argonTab(
                            tabName = "Animation",
                            active = F,
                            imageOutput("gif",height = "700px")
                          ),
                          argonTab(
                            tabName = "Graphique",
                            active = T,
                            plotly::plotlyOutput("plot_ly",height = "600px",width = "100%")
                          )
                          )
            )
          )
        )

      )
    )
  ),


  server = function(input, output) {


    expr_x <- reactive({
      input$x_t
    }) %>% bindEvent(input$Simuler)

    expr_y <- reactive({
      input$y_t
    }) %>% bindEvent(input$Simuler)

    dat <- reactive({

      x = function(t) eval(parse(text =expr_x()))
      y = function(t) eval(parse(text =expr_y()))
      maximum = 100
      while (y(maximum) > 0) {
        maximum = maximum +100
      }

      t = seq(0,maximum,0.1)

      y_t = Map(y,t) %>% unlist()
      x_t = Map(x,t) %>% unlist()
      data.frame(t = t, x = x_t, y= y_t)

    })
    output$plot_ly <- renderPlotly({
      dat() %>% filter(y>=0)%>%
        plot_ly(x =~x,
                y=~y,
                type = "scatter",
                mode = "lines", name = "Trajectoire") %>%
        add_trace(x =~x,
                  y=~y,
                  frame = ~t, type = "scatter",
                  mode = "markers",size =20,alpha = 1,name = "Objet"
        )
    })
    output$gif <- renderImage({
      p = ggplot(dat() %>% filter(y>=0),aes(x,y))+geom_point(size = 8, color = "blue")+
        geom_line()+ hrbrthemes::theme_ipsum(grid = "Y")+
        transition_reveal(x)#+view_follow()
      anim_save("outfile.gif",animate(p))

      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif',
           height = "700px",
           alt = "This is alternate text"
      )
    })
    output$cards_info = renderUI({
      argonRow(
        argonColumn(width = 4,
                    argonInfoCard(width = 12,
                      value = round(max(dat()$y),2),
                      title = "Flèche",
                      icon = argonIcon("planet"),
                      icon_background = "danger",
                      shadow = TRUE,
                      background_color = "default",
                      hover_lift = TRUE
                    )

                    ),
        argonColumn(width = 4,
                    argonInfoCard(width = 12,
                      value = round(max(dat()$x),2),
                      title = "Portée",
                      icon = icon("chart-pie"),
                      icon_background = "warning",
                      background_color = "default",
                      shadow = TRUE,
                      hover_lift = TRUE
                    )

        ),
        argonColumn(width = 4,
                    argonInfoCard(width = 12,
                      value = round(max(dat()$t),2),
                      title = "Durée",
                      icon = icon("users"),
                      icon_background = "yellow",
                      background_color = "default",
                      shadow = TRUE,
                      hover_lift = TRUE
                    )

        )

        )

    })

    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })

    output$plot <- renderPlot({
      dist <- switch(
        input$dist,
        norm = rnorm,
        unif = runif,
        lnorm = rlnorm,
        exp = rexp,
        rnorm
      )

      hist(dist(500))
    })

    # argonTable
    output$argonTable <- renderUI({

      wrap <- if (input$cardWrap == "Enable") TRUE else FALSE

      argonTable(
        cardWrap = wrap,
        headTitles = c(
          "PROJECT",
          "BUDGET",
          "STATUS",
          "USERS",
          "COMPLETION",
          ""
        ),
        argonTableItems(
          argonTableItem("Argon Design System"),
          argonTableItem(dataCell = TRUE, "$2,500 USD"),
          argonTableItem(
            dataCell = TRUE,
            argonBadge(
              text = "Pending",
              status = "danger"
            )
          ),
          argonTableItem(
            argonAvatar(
              size = "sm",
              src = "https://image.flaticon.com/icons/svg/219/219976.svg"
            )
          ),
          argonTableItem(
            dataCell = TRUE,
            argonProgress(value = 60, status = "danger")
          ),
          argonTableItem(
            argonButton(
              name = "Click me!",
              status = "warning",
              icon = "atom",
              size = "sm"
            )
          )
        )
      )
    })

  }
)
