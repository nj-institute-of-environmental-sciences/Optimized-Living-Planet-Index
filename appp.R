rm(list=ls())

library(shiny)

ui = navbarPage(
  "生物多样性指数",
  tabPanel(
    "Input_data",
    sidebarPanel(
      fileInput(
        "file", "导入数据文件", accept=c("text/csv",
                                   "text/comma-separated-values,text/plain",".csv")
      ),
      h5("注意：导入的数据文件是需要预处理之后的，避免不规整数据影响数据进一步分析！"),
      
      checkboxGroupInput(
        "year", "选择需要计算指数的年度", 2011:2030
      ),
      radioButtons(
        "type", "生物类",
        c("鸟类"="鸟类", "两栖"="两栖")
      ),
      textInput(
        "aera_in", "输入自定义区域面积"
      ),
      actionButton(
        "update", "Confirm"
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "inTabset",
        tabPanel(
          "Introduction",
          h3("软件使用说明："),
          h4("(1)目前软件可以计算全国范围内、各省份范围内的生物多样性指数；如果需要计算自然保护区或者自定义区域下的
             生物多样性指数，需要使用者输入相应区域的面积，保证自定义区域指数计算的合理性。计算全国、各省份范围内的
             生物多样性指数，需要输入自定区域面积为-1。"),
          h4("(2)aera文件记录各省面积，鸟类的在aera文件中用编号，两栖在aera文件中用省份，如果鸟类数据中有省份这一列，
             对应aera文件也用省份。"),
          h4("(3)line表示myapp文件夹在本地的路径，在使用前需要做相应的修改。"),
          h4("(4)导入的数据文件是鸟类或者两栖的 .csv文件。"),
          h4("(5)标准数据格式展示面板中，展示了导入数据文件的标准数据格式，导入的数据不能有
             缺失值，针对不同的字段，要先做预处理。例如：将经度120°12'修改为120.2。"),
          h4("(6)计算鸟类的多样性指数时，数据文件需要包含:年份,物种名,长度,省份,物种数量等字段；计算两栖的多样性指数
             时，数据文件需要包含:年份,物种名,长度,宽度,省份,物种数量等字段。"),
          h4("(7)自定义面积单位是平方米！")
          ),
        tabPanel(
          "标准数据格式展示",
          h3("注意，在输入的数据表中，需要包含的基本字段包括："),
          br(),
          tableOutput("view")
        ),
        tabPanel(
          "导入的数据展示",
          br(),
          tableOutput("view1")
        )
          )
    )
    ),
  tabPanel(
    "指数计算结果展示",
    h3("数据表格"),
    tableOutput("out"),
    h3("改进后指数折线图"),
    plotOutput("out1")
  ),
  tabPanel(
    "指数变化显著性检验",
    sidebarPanel(
      checkboxGroupInput(
        "year1", "进行检验的年份", 2011:2030
      ),
      h4("注意：这里选择的年份是Input_data面板中选择计算指数年份中的任意两年！"),
      textInput(
        "pp", "检验随机抽样的次数"
      ),
      actionButton(
        "update1", "Confirm"
      )
    ),
    mainPanel(
      h3("检验结果展示："),
      h4("注：如果p-value<0.05，则拒绝原假设，说明指数变化差异显著;反之，接受原假设，
         说明指数变化差异不显著。"),
      verbatimTextOutput("pri"),
      br(),
      verbatimTextOutput("summary")
      )
  )
  )

options(shiny.maxRequestSize=50*1024^2)
server = function(input, output, session){
  
  line = "C:/Users/LW/Desktop/Index2/"
  file_data = eventReactive(input$update, {
    inFile = input$file
    read.csv(inFile$datapath,sep=',',header=TRUE)
  })
  year_data = eventReactive(input$update, {
    input$year
  })
  type_data = eventReactive(input$update, {
    input$type
  })
  aera_in_data = eventReactive(input$update, {
    input$aera_in
  })
  year_data_jy = eventReactive(input$update1,{
    input$year1
  })
  pp_data = eventReactive(input$update1,{
    input$pp
  })
  
  link_fun = paste0(line,"指数函数封装_加置信区间.r")
  print(link_fun)
  source(link_fun, encoding = "UTF-8")
  conclusion = eventReactive(input$update, {
    fun(file_data(), year_data(), type_data(),line,aera_in_data())
  })
  link_fun1 = paste0(line, "指数检验封装.r")
  source(link_fun1, encoding = "utf-8")
  conclusion1 = eventReactive(input$update1, {
    fun1(file_data(), year_data_jy(), year_data(), pp_data(), type_data(), line, aera_in_data())
  })
  
  print("准备打印标准数据集实例....")
  data_link = paste0(line, "两栖.csv")
  data = read.csv(data_link, sep=',', header=TRUE)
  output$view = renderTable({
    names = c("年份","物种名","样线编码","调查次序","省份","长度","宽度","经度","纬度","物种数量")
    head(data[names],10)
  })
  
  output$view1 = renderTable({
    print("准备打印导入数据集....")
    head(file_data(),5)
  })
  
  output$out = renderTable({
    withProgress(message = 'Calculation in progress',detail = 'This may take a while...', 
                 expr = {
                   conclusion()[[1]]
                 })
    #conclusion()[[1]]
  })
  
  
  output$out1 = renderPlot({
    print("开始绘制结果图!")
    par(mfrow=c(1,2))
    plot(year_data(), conclusion()[[1]][,"改进1"],xlab="year",ylab="number",type='l',
         ylim = c(min(conclusion()[[2]]),max(conclusion()[[3]])),main="New Index one")
    polygon(c(year_data(),rev(year_data())), c(rev(conclusion()[[3]]),conclusion()[[2]]), col=rgb(.7, .7, .7, .5),border = NA)
    plot(year_data(), conclusion()[[1]][,"改进2"],xlab="year",ylab="number",type='l',
         ylim = c(min(conclusion()[[4]]),max(conclusion()[[5]])),main="New Index two")
    polygon(c(year_data(),rev(year_data())), c(rev(conclusion()[[5]]),conclusion()[[4]]), col=rgb(.7, .7, .7, .5),border = NA)
    print("结果图绘制完成!")
  })
  
  output$pri = renderPrint(
    print(paste(pp_data(),"次随机抽样，指数检验计算结果:"))
  )
  output$summary = renderPrint(
    withProgress(message = 'Calculation in progress',detail = 'This may take a while...', 
                 expr = {
                   conclusion1()
                 })
  )
 }

shinyApp(ui, server)
