source("FUNCTIONOneinout.R")                #funtion of inoutdegree
source("FUNTIONbig.triangle.numggplot.R")   #funtion of big.trangle.numggplot, the variable of return is "bigallcdata"
source("FUNCTIONtotal.datatable.R")         #funtion of user.Wadj.total.datatable, the variable of return is "build"
source("FUNCTIONDiameterPatH.R")            #funtion of Diameter.PatH 
source("FUNCTIONNeighborhooD.R")            #funtion of Neighborhood of max degree
source("FUNCTIONNode.select.R")             #funtion of Node_Select
source("FUNCTIONtriangle.numggplot.R")      #funtion of triangle.numggplot
source("FUNCTIONTwoside.R")                 #funtion of Twoside
source("FUNCTIONeach.datatable.R")   
ALLPlotlY<-function(MATRIX,group,gname,firstdate,obs,
                    ICON.CODEg="f111",ICON.SIZEg=50,
                    ICON.CODE1=c("f005","f185"),ICON.COLOR1=c("yellow","red"),ICON.SIZE1=c(50,70),icon.sizeL1=17,ICON.LABEL1=c("one link company","company"),
                    ICON.CODE2=rep("f19c",length(unique(group))),ICON.COLOR2=unique(ColoR),ICON.LABEL2=unique(group)){
  
  dataeach=datateach(MATRIX,group,gname,firstdate)
  visGroup=FUNCTIONTwoside(MATRIX,group,gname,firstdate,ICON.CODEg,ICON.COLOR2,ICON.SIZEg)
  visNode_selection=FUNCTIONNode_selection(MATRIX,group,firstdate,ICON.CODE2,ICON.COLOR2)
  ptri=TriangleNumggplot(MATRIX,group,gname,firstdate)
  Oneinout=OneinoutF(MATRIX,group,gname,firstdate)
  bigggplotly=BigtriangleN(MATRIX,group,gname,firstdate,obs)
  build=Totaldatatable(MATRIX,group,firstdate)
  dia.Net=DiameterPatH(MATRIX,group,gname,firstdate,ICON.CODE2,ICON.COLOR2)
  visNeighbor=NeighborhooD(MATRIX,group,gname,ICON.CODE2,ICON.COLOR2,firstdate)            

######
ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      "Network Property!",
      tabPanel("Characteristic of Each Network",
               sidebarPanel(
                 selectInput("Date", "Year-Month:"  ,choices = as.character(Date)),
                 br(),
                 conditionalPanel(condition="input.tabselected==3"
                                  ,selectInput(inputId = "selnodes", label = "Node selection", choices = 1:length(group), multiple = TRUE)),
                 conditionalPanel(condition="input.tabselected==4",
                                  selectInput(inputId = "dg", label =  "In-Degree",
                                              choices=sort(unique(indeg[[i]]))) ),
                 conditionalPanel(condition="input.tabselected==5",
                                  selectInput(inputId = "dgout", label =  "Out-Degree",
                                              choices=sort(unique(outdeg[[i]]))) ),
                 conditionalPanel(condition="input.tabselected==6")
             ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Property Table", value=1,dataTableOutput("table1")),
                   tabPanel("Group", value=2,visNetworkOutput("grou", height = "750px")),
                   tabPanel("Node", value=3,visNetworkOutput("visNode.select", height = "750px")),
                   tabPanel("In-Degree", value=4,visNetworkOutput("Net_deg", height = "750px")), 
                   tabPanel("Out-Degree", value=5,visNetworkOutput("Net_outdeg", height = "750px")), 
                   tabPanel("Triangle Number", value=6, plotlyOutput("Triangle.Number")),
                   id = "tabselected"
                            )
                        )
             ),
      tabPanel("Characteristic of the Overall Network",
               mainPanel(
                 tabsetPanel(
                   tabPanel('Property Table',    
                            dataTableOutput('table')),
                   tabPanel('Density',    
                            plotlyOutput("DENSITY")),
                   tabPanel('Mean Distance',
                            plotlyOutput("MEAN.DISTANCE")),
                   tabPanel('Transitivity',    
                            plotlyOutput("TRANSITIVITY")),
                   tabPanel('Big Triangle Numbers',
                            plotlyOutput("BigTriangle")),
                   tabPanel('Diameter',
                            plotlyOutput("DIAMETER"),visNetworkOutput("DIAMETER.PATH")),
                   tabPanel('Max Degree Numbers',
                            plotlyOutput("NEIGHBOR"),visNetworkOutput("NEIGHBORvisNet")),
                   tabPanel('Company Share Price',                      
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("nodenumk","select node",choices = 1:nodenum)
                              ),
                              mainPanel( plotlyOutput( "INOUTC"), plotlyOutput("SHAREPRICE"))
                            )
                           )
                 )
               )
      )
    )
  )
  
server=function(input, output,session) ({
if (interactive()) {
      observe({
        i = which(as.character(Date)==as.character(input$Date) )[1];
        
        updateSelectInput(session, inputId = "dg", label ="In-Degree",
                          choices=sort(unique(indeg[[i]])))
        updateSelectInput(session,inputId = "dgout", label ="Out-Degree",
                          choices=sort(unique(outdeg[[i]])))
        updateSliderInput(session,"size", "degree", min = 0, max = max(deg[[i]]))
      })
}
    
## Get the value of the MATRIX that is selected by user from the list of datasets
    data <- reactive({
      get(input$MATRIX)
    })
#----------------------------------------- 
##data table
    dataInput <- reactive({
      i = which(as.character(Date)==as.character(input$Date) )[1] 
      dataeach[[i]]
    })
    output$table1 <- renderDataTable({
      datatable(dataInput())
    })
    
#-----------------------------------------    
##使用者可以選擇節點組別(group)
    output$grou <- renderVisNetwork({
      i = which(as.character(Date)==as.character(input$Date) )[1];
      visGroup[[i]]
    })
#-----------------------------------------  
##使用者可以選擇網絡圖中的多個節點(nodes_selection)
    output$visNode.select <- renderVisNetwork({
      i = which(as.character(Date)==as.character(input$Date) )[1];
      visNode_selection[[i]]
    })
    
    observe({
      nodes_selection <- input$selnodes
      visNetworkProxy("visNode.select") %>%
        visSelectNodes(id = nodes_selection)
    })
#-----------------------------------------  
##使用者可以選擇網絡圖中的IN-degree
    output$Net_deg <- renderVisNetwork({
      i = which(as.character(Date)==as.character(input$Date) )[1];
      
      #排序組別，將資料矩陣依照group去做排序
      SSort=sort(paste(group),index.return=TRUE)
      RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
      CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
      #matrix轉成igraph
      Mgfam[[i]]<- graph_from_adjacency_matrix(CMATRIX[[i]])      
      indeg[[i]] <- degree(Mgfam[[i]], mode="in")
      
      dg=input$dg;
      #選擇indegree的節點
      select.edgein[[i]]<-which(indeg[[i]]==dg)
      
      if(dg==0){
        id[[i]]=select.edgein[[i]]    #太陽和星星id
        sunid[[i]]=select.edgein[[i]] #太陽id
        starid[[i]]=id[[i]][-which(id[[i]]==select.edgein[[i]])]#星星id
        
        #設定節點圖示(icon.code),節點顏色(icon.color)的值
        iconcode=ifelse(id[[i]]==sunid[[i]],ICON.CODE1[2],ICON.CODE1[1])
        iconcolor=ifelse(id[[i]]==sunid[[i]],ICON.COLOR1[2],ICON.COLOR1[1])
        iconsize=ifelse(id[[i]]==sunid[[i]],ICON.SIZE1[2],ICON.SIZE1[1])
        #調整legend的圖示
        ADDNodein <- data.frame(label =ICON.LABEL1, shape = "icon",
                                icon.code = ICON.CODE1, icon.size =rep(icon.sizeL1,length(ICON.CODE1)), icon.color =ICON.COLOR1,font.color ="midnightblue")
        #調整節點訊息
        indeg0_node[[i]]=data.frame(id=id[[i]],
                                    label=id[[i]],
                                    group= group[id[[i]]] ,
                                    title = paste0("Node:", id[[i]],"<br>Group:", group[id[[i]]],"<br>Name:",gname[id[[i]]]),
                                    shape = "icon",
                                    icon.code = iconcode,
                                    icon.color =iconcolor,
                                    icon.size=iconsize,
                                    font.size=iconsize-5,
                                    font.color=rep("midnightblue",length(id[[i]])))
        #調整邊訊息
        indeg0_edge[[i]]<- data.frame(from = id[[i]],to = id[[i]])
        #網絡圖
        visNetwork(indeg0_node[[i]],indeg0_edge[[i]],
                   main = list(text = paste("In-degree",dg),
                               style = "ont-family:Georgia;color:#1a2421;font-size:25px;font-weight:bold;text-align:center;"),
                   submain=  list(text = paste("Date : ",Date[i],"<br>Node",paste( select.edgein[[i]], collapse = ", ")),
                                  style = "font-family:Comic;color:#1a2421;font-size:18px;text-align:center;"),
                   width = "100%") %>%
          addFontAwesome() %>%
          addIonicons()%>%
          visOptions(highlightNearest = list(enabled = TRUE, hover = T, 
                                             hideColor = 'rgba(200,200,200,0)', degree = list(to =1)))%>%
          visEdges( hidden=TRUE)%>% 
          visLegend(addNodes =ADDNodein, useGroups = FALSE)
      }else{
        indegposi[[i]]=which(get.edgelist(Mgfam[[i]])[,2] %in% select.edgein[[i]])
        
        indegedgelist[[i]]<-matrix(get.edgelist(Mgfam[[i]])[indegposi[[i]],],ncol=2)  #edgelist
        id[[i]]=unique(c(indegedgelist[[i]][,1],indegedgelist[[i]][,2]))
        sunid[[i]]=select.edgein[[i]] #太陽id
        starid[[i]]=id[[i]][-which(id[[i]] %in% select.edgein[[i]])]#星星id
        
        #設定節點圖示(icon.code),節點顏色(icon.color)的值
        iconcode1=ifelse(id[[i]]%in%sunid[[i]],ICON.CODE1[2],ICON.CODE1[1])
        iconcolor1=ifelse(id[[i]]%in%sunid[[i]],ICON.COLOR1[2],ICON.COLOR1[1])
        iconsize1=ifelse(id[[i]] %in% sunid[[i]],ICON.SIZE1[2],ICON.SIZE1[1])
        #調整legend的圖示
        ADDNodein <- data.frame(label =ICON.LABEL1, shape = "icon",
                                icon.code = ICON.CODE1, icon.size =rep(icon.sizeL1,length(ICON.CODE1)),icon.color =ICON.COLOR1, font.color ="midnightblue")
        #調整邊訊息
        indeg_edge[[i]]<- data.frame(from =indegedgelist[[i]][,1], to = indegedgelist[[i]][,2])
        #調整節點訊息
        indeg_node[[i]]=data.frame(id=id[[i]],
                                   label=id[[i]],
                                   group=group[id[[i]]] ,
                                   title = paste0("Node:", id[[i]],"<br>Group:", group[id[[i]]],"<br>Name:",gname[id[[i]]]),
                                   shape = "icon",
                                   icon.code = iconcode1,
                                   icon.color =iconcolor1,
                                   icon.size=iconsize1,
                                   font.size=iconsize1-15,
                                   font.color=rep("midnightblue",length(id[[i]])))
        #網絡圖 
        visNetwork(indeg_node[[i]],indeg_edge[[i]],
                   main = list(text = paste("In-degree",dg),
                               style = "ont-family:Georgia;color:#1a2421;font-size:25px;font-weight:bold;text-align:center;"),
                   submain=list(text = paste("Date : ",Date[i],"<br>Node",paste( select.edgein[[i]], collapse = ", ")),
                                style = "font-family:serif;color:#1a2421;font-size:18px;text-align:center;"),
                   width = "100%") %>%
          addFontAwesome() %>%
          addIonicons()%>%
          visEdges(color=list(color = "white", highlight = "cyan",opacity=0.15),#hidden=TRUE,  #edge線隱藏
                   arrows =list(to = list(enabled = TRUE, scaleFactor = 1))
                   ,dashes = TRUE
                   ,arrowStrikethrough=FALSE) %>%
          visOptions( highlightNearest = list(enabled = TRUE, algorithm = "hierarchical",
                                              hover = T, hideColor = 'rgba(200,200,200,0)', degree = list(from =1)))  %>%
          visPhysics(solver = "repulsion")%>%
          visInteraction(keyboard = TRUE, dragNodes = T, dragView = T, zoomView = T ) %>%
          visLegend(addNodes = ADDNodein , useGroups = FALSE)
      }
    })
#-----------------------------------------
##使用者可以選擇網絡圖中的OUT-degree
    output$Net_outdeg <- renderVisNetwork({
      i = which(as.character(Date)==as.character(input$Date) )[1];
      
      #排序組別，將資料矩陣依照group去做排序
      SSort=sort(paste(group),index.return=TRUE) 
      RMATRIX[[i]]=MATRIX[[i]][SSort$ix,]
      CMATRIX[[i]]=RMATRIX[[i]][,SSort$ix]
      #matrix轉成igraph    
      Mgfam[[i]]<- graph_from_adjacency_matrix(CMATRIX[[i]])      
      #計算outdegree 
      outdeg[[i]] <- degree(Mgfam[[i]],mode="out")
      
      dg=input$dgout;
      #選擇OUTdegree的節點
      select.edgeout[[i]]<-which(outdeg[[i]]==dg)
      if(dg==0){
        id[[i]]=select.edgeout[[i]]    #太陽和星星id
        sunid[[i]]=select.edgeout[[i]] #太陽id
        starid[[i]]=id[[i]][-which(id[[i]]==select.edgeout[[i]])]#星星id
        
        #設定節點圖示(icon.code),節點顏色(icon.color)的值
        iconcode=ifelse(id[[i]]==sunid[[i]],ICON.CODE1[2],ICON.CODE1[1])
        iconcolor=ifelse(id[[i]]==sunid[[i]],ICON.COLOR1[2],ICON.COLOR1[1])
        iconsize=ifelse(id[[i]]==sunid[[i]],ICON.SIZE1[2],ICON.SIZE1[1])
        #調整legend的圖示
        ADDNodeout <- data.frame(label =ICON.LABEL1, shape = "icon",
                                 icon.code = ICON.CODE1, icon.size =rep(icon.sizeL1,length(ICON.CODE1)), icon.color =ICON.COLOR1,font.color ="midnightblue")
        #調整節點訊息
        outdeg0_node[[i]]=data.frame(id=id[[i]],
                                     label=id[[i]],
                                     group= group[id[[i]]] ,
                                     title = paste0("Node:", id[[i]],"<br>Group:", group[id[[i]]],"<br>Name:",gname[id[[i]]]),
                                     shape = "icon",
                                     icon.code = iconcode,
                                     icon.color =iconcolor,
                                     icon.size=iconsize,
                                     font.size=iconsize-5,
                                     font.color=rep("midnightblue",length(id[[i]])))
        #調整邊訊息
        outdeg0_edge[[i]]<- data.frame(from =id[[i]],to =id[[i]])
        #網絡圖 
        visNetwork(outdeg0_node[[i]], outdeg0_edge[[i]],
                   main = list(text = paste("Out-degree",dg),
                               style = "ont-family:serif;color:	#1a2421;font-size:25px;font-weight:bold;text-align:center;"),
                   submain=  list(text = paste("Date : ",Date[i],"<br>Node",paste( select.edgeout[[i]], collapse = ", ")),
                                  style = "font-family:Comic;color:	#1a2421;font-size:18px;text-align:center;"),    
                   width = "100%") %>%
          addFontAwesome() %>%
          addIonicons()%>%
          visOptions( highlightNearest = list(enabled = TRUE, hover = T, hideColor = 'rgba(200,200,200,0)', 
                                              degree = list(to =1)))%>%
          visEdges( hidden=TRUE)%>%   #edge線隱藏
          visLegend(addNodes =ADDNodeout,useGroups = FALSE)  
      }else{
        outdegposi[[i]]=which(get.edgelist(Mgfam[[i]])[,1] %in% select.edgeout[[i]])
        
        outdegedgelist[[i]]<-matrix(get.edgelist(Mgfam[[i]])[outdegposi[[i]],],ncol=2)  #edgelist
        id[[i]]=unique(c(outdegedgelist[[i]][,1],outdegedgelist[[i]][,2]))
        sunid[[i]]=select.edgeout[[i]] #太陽id
        starid[[i]]=id[[i]][-which(id[[i]] %in% select.edgeout[[i]])]#星星id
        
        #設定節點圖示(icon.code),節點顏色(icon.color)的值
        iconcode1=ifelse(id[[i]]%in%sunid[[i]],ICON.CODE1[2],ICON.CODE1[1])
        iconcolor1=ifelse(id[[i]]%in%sunid[[i]],ICON.COLOR1[2],ICON.COLOR1[1])
        iconsize1=ifelse(id[[i]] %in% sunid[[i]],ICON.SIZE1[2],ICON.SIZE1[1])
        #調整legend的圖示
        ADDNodeout <- data.frame(label =ICON.LABEL1, shape = "icon",
                                 icon.code = ICON.CODE1, icon.size =rep(icon.sizeL1,length(ICON.CODE1)),icon.color =ICON.COLOR1, font.color ="midnightblue")
        #調整邊和節點的訊息
        outdeg_edge[[i]] <- data.frame(from =outdegedgelist[[i]][,1], to = outdegedgelist[[i]][,2])
        outdeg_node[[i]] = data.frame(id=id[[i]],
                                      label=id[[i]],
                                      group=group[id[[i]]] ,
                                      title = paste0("Node:", id[[i]],"<br>Group:", group[id[[i]]],"<br>Name:",gname[id[[i]]]),
                                      shape = "icon",
                                      icon.code = iconcode1,
                                      icon.color =iconcolor1,
                                      icon.size=iconsize1,
                                      font.size=iconsize1-15,
                                      font.color=rep("midnightblue",length(id[[i]])))
        
        #網絡圖
        visNetwork(outdeg_node[[i]],outdeg_edge[[i]],
                   main = list(text = paste("Out-degree",dg),
                               style = "ont-family:serif;color:	#1a2421;font-size:25px;font-weight:bold;text-align:center;"),
                   submain=list(text = paste("Date : ",Date[i],"<br>Node",paste( select.edgeout[[i]], collapse = ", ")),
                                style = "font-family:Comic;color:	#1a2421;font-size:18px;text-align:center;"),    
                   width = "100%") %>%
          addFontAwesome() %>%
          addIonicons()%>%
          visEdges(color=list(color = "white", highlight = "cyan",opacity=0.15),#hidden=TRUE,  #edge線隱藏
                   arrows =list(to = list(enabled = TRUE, scaleFactor = 1))
                   ,dashes = TRUE
                   ,arrowStrikethrough=FALSE) %>%   
          visOptions( highlightNearest = list(enabled = TRUE, algorithm = "hierarchical",
                                              hover = T, hideColor = 'rgba(200,200,200,0)',degree = list(to =1)) ) %>% 
          visPhysics(solver = "repulsion")%>%
          visInteraction(keyboard = TRUE, dragNodes = T, dragView = T, zoomView = T ) %>%
          visLegend(addNodes = ADDNodeout , useGroups = FALSE)
      }
    })
#-----------------------------------------
##節點在網絡中參與的三角形數直方圖
    output$Triangle.Number <- renderPlotly({
      i = which(as.character(Date)==as.character(input$Date) )[1];
      ptri[[i]]
    })
    
## DataTable
    output$table <- renderDataTable(build)
## density 網絡密度
    output$DENSITY <- renderPlotly({
      p <- plot_ly(df) %>%
        add_trace(x = ~Date,  y = ~density, type = 'bar', name = 'density',
                  hoverinfo = "text",
                  text = ~paste('density:', density, sep=" : ") )  %>%
        layout(
              title = "<b>Network Property -- Density",titlefont =list(size = 15),
              xaxis=list(title="",tickangle = -35),
              yaxis=list(title="value"),
              annotations = a1
              )
    })
    
## mean distance   將所有節點兩兩之間的最短距離做平均
    output$MEAN.DISTANCE <- renderPlotly({
      p <- plot_ly(df) %>%
        add_trace(x = ~Date,  y = ~Mean.Distance, type = 'bar', name = 'Mean Distance',
                  hoverinfo = "text",
                  text = ~paste('Mean Distance:', density, sep=" : ") )  %>%
        layout(
               title = "<b>Network Property -- Mean Distance",
               xaxis=list(title="",tickangle = -35),
               yaxis=list(title="value"),
               annotations = a2
        )
    })
## transitivity
    output$TRANSITIVITY <- renderPlotly({

      plot_ly(df) %>%
        add_trace(x = ~Date,  y = ~Transitivity, type = 'bar', name = 'Transitivity',
                  hoverinfo = "text",
                  text = ~paste('Transitivity:', Transitivity, sep=" : ") )  %>%
        layout(
               title = "<b>Network Property -- Transitivity",
               xaxis=list(title="",tickangle = -35),
               yaxis=list(title="value"),
               annotations = a3
              )
    })
    
## Big Triangle 前幾大的三角形折線圖 
    output$BigTriangle <- renderPlotly({
      bigggplotly
    })
    
## Diameter 為節點倆倆之間的最短距離中最長的(最長的最短距離)
    output$DIAMETER <- renderPlotly({
      b <- list(
        x = Date[1] ,
        y = df$diameter[which(df$Date==Date[1])]+5,
        text = paste("The user can click on the bar","<br>and see the diameter path below the histogram "),
        xref = "x",
        yref = "y",
        showarrow = F, 
        ax = 10,
        ay = -40
      )
      
      tmp=paste("p<-plot_ly(df)");
      for(jj in 1:length(unique(group)))
      {
        tmp <- paste(tmp ,"%>% add_trace(x = ~Date,y = ~df[,5+",jj,"],type = 'bar', name =unique(group)[",jj,"])",sep="")
      }
      tmp <- paste(tmp ,"%>% layout(
                   title = \"<b>Network Property -- Diameter\",
                   xaxis=list(title=\"\",tickangle = -35),
                   yaxis=list(title=\"diameter+1\")
                   , barmode = 'stack',annotations = list(a4,b))",sep="")
      eval(parse(text=tmp))
    })
    
    output$selection <- renderPrint({
      s <- event_data("plotly_click")
      if (length(s) == 0) {
        "Click on a bar in the histogram to display a diameter path"
      } else {
        cat("You selected: \n\n")
        as.list(s)
      }
    })
    
## diameter路徑
    output$DIAMETER.PATH <- renderVisNetwork({
      s <- event_data("plotly_click")
      if (length(s)!=0) {
        i = which(as.character(Date)==as.character(unique(s$x)) )[1];
        dia.Net[[i]]
      } else {
        plotly_empty()
      }
    })
    
## Neighbor.num 挑出網絡中最大degree的那些節點畫成折線圖
    output$NEIGHBOR <- renderPlotly({

      p <- plot_ly(df) %>%
        add_trace(x = ~Date,  y = ~Neighbor.num, type = 'scatter', name = 'Neighbor number',
                  hoverinfo = "text",
                  line = list(color = '#45171D'), 
                  hoverinfo = "text"
                  , text =~paste("Total Neighbor numbers = ", Neighbor.num
                                 ,"<br>Max Degree ID:", max.degree) ) %>%
        layout(
          title = "<b>Network Property -- Neighbor Number",
          xaxis=list(title="",tickangle = -35),
          yaxis=list(title="Neighbor number"),
          annotations = a5
        )
    })
    
    output$selection <- renderPrint({
      ed <- event_data("plotly_click")
      if (length(ed) == 0) {
        "Click on a bar in the histogram to display a diameter path"
      } else {
        cat("You selected: \n\n")
        as.list(ed)
      }
    })
## 畫出最大degree的那個節點的鄰居網絡圖
    output$NEIGHBORvisNet <- renderVisNetwork({
      ed <- event_data("plotly_click")
      if (length(ed)!=0) {
        i = which(as.character(Date)==as.character(unique(ed$x)) )[1];
        visNeighbor[[i]]
      } else {
        plotly_empty()
      }
    })
## inoutdegree 一個節點的一步indegree和outdegree
    output$INOUTC <- renderPlotly({
      k = which(c(1:dim(MATRIX[[1]])[1] )==input$nodenumk )[1];
      p <- ggplot(Oneinout[[k]], aes(x=Date, y=ID,
                                     shape=factor(Oneinout[[k]]$Arrow, levels = c("out","in")),  
                                     color=as.factor(group[Oneinout[[k]]$ID]) 
                                     ,text=paste("Date: ",Oneinout[[k]]$Date,
                                                 '<br&gt; Company: ',gdata$abbreviation[Oneinout[[k]]$ID],
                                                 '<br&gt; Arrow: ',Oneinout[[k]]$Arrow) 
                                     ,position = 'jitter')) +
        geom_point() +
        scale_shape_manual(values=c(17,25))+  
        labs(x="", y=paste("Neighbors of ",gname[k]) , shape="",col="") +  
        ggtitle(paste("Neighbors & Share Price of",gname[k])) +
        scale_y_continuous(breaks=seq(0,length(gname), floor(length(gname)/4)))  + 
        theme(axis.text.x = element_blank() 
              #axis.text.x  = element_text(angle=35),  
              # ,plot.margin = unit(c(1,-6,0.1,3),"cm")
        ) 
      
      ggplotly(p, tooltip = c("text"))%>%
        layout(legend = list(orientation = 'h', x =0.01, y =0.5)
               ,yaxis = list(domain = c(0.5, 1),rangemode = "nonnegative")
               ,annotations = list(
                 xref = "paper", yref = "paper", 
                 x = 0.8, y =1.05, showarrow = F, 
                 xanchor = "left", yanchor = "top",
                 align = "left",
                 text =paste(range(Date), collapse = " : "),
                 font = list(size = 8)),
               plot_bgcolor = "#f2f2f2")
    })  
    
##股價
    output$SHAREPRICE <- renderPlotly({
      comd=list();
      comdata=list();
      monthly.prices=list();
      company=paste(gname);
      k = which(c(1:dim(MATRIX[[1]])[1] )==input$nodenumk )[1];
      comd[[k]] <-as.data.frame(getSymbols(Symbols = company[k], 
                                           src = "google", from = firstdate,to=Date0[length(MATRIX)], env = NULL))
      
      monthly.prices[[k]] <- to.monthly(comd[[k]])
      colnames(monthly.prices[[k]]) <-c("Open","High","Low","Close","Volume")
      
      comdata[[k]]<-data.frame(Date=Date,coredata(monthly.prices[[k]]))
      rownames( comdata[[k]])<-c(1:length(Date))
      
# 調整上漲下跌的顏色
      for (co in 1:length(comdata[[k]] [,1])) {
        if (comdata[[k]] $Close[co] >= comdata[[k]]$Open[co]) {
          comdata[[k]] $direction[co] = 'Increasing'
        } else {
          comdata[[k]]$direction[co] = 'Decreasing'
        }
      }
      hovertxt <- Map(function(x, y)paste0(x, ":", y), names(comdata[[k]])[-6], comdata[[k]][-6])
      hovertxt <- Reduce(function(x, y)paste0(x, "<br&gt;", y), hovertxt)
      
# plot candlestick chart
      p <- plot_ly(comdata[[k]],x = ~Date, xend = ~Date,  type="candlestick",
                    hoverinfo = "none",color = ~Close > Open, colors = c("#00b386","#ff6666"),
                    showlegend = F) %>%
           add_segments(y = ~Low, yend = ~High, line = list(width =1, color = "black")) %>%
           add_segments(y = ~Open, yend = ~Close, line = list(width =6)) %>%
           add_markers(y = ~(Low + High)/2, hoverinfo = "text",
                    text = hovertxt, marker = list(color = "transparent")) %>%
           layout(yaxis = list(title = "Price", domain = c(0, 0.9)))
# plot volume bar chart
      pp <- comdata[[k]]  %>%
        plot_ly(x=~Date,y=~Volume, type='bar', showlegend =T,
                hoverinfo = "text",text = ~ paste0('Date: ', Date, "<br&gt;Volume: ",comdata[[k]]$Volume),
                color = ~direction, colors = c("#00b386","#ff6666")) %>%
        layout(yaxis = list(title = "Volume", domain = c(0.25, 1)),xaxis = list(title = "Date"))
      
# subplot with shared x axis
      PPP<- subplot(p, pp, heights = c(0.6,0.3), nrows=2,
                    shareX =TRUE, titleY = TRUE) %>%
        layout(legend = list(orientation = 'h', x = 0.5, y = 0.95,
                        xanchor = 'center', yref = 'paper',
                        font = list(size = 10),
                        bgcolor = 'transparent'),
               annotations = list( list(xref = "paper", yref = "paper", x = 0, y =0.98, showarrow = F, 
                                        xanchor = "left", yanchor = "top", align = "left",font = list(size =16),
                                        text = paste("Share price of ",company[k])),
                                   list(xref = "paper", yref = "paper", x = 0.80, y =0.925, showarrow = F, 
                                        xanchor = "left", yanchor = "top", align = "left",
                                        text =paste(range(Date), collapse = " : "), font = list(size = 8))),
               plot_bgcolor = "#f2f2f2")
    })
  })
  result <-list(server = server,ui=ui)
  Shiny.ResulT<-shinyApp(server = result$server,ui= result$ui)
  return(Shiny.ResulT) 
}