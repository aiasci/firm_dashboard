
library(plotly)
library(data.table)
library(readxl)

datum<- read.csv("https://raw.githubusercontent.com/aiasci/firms/master/graph_data.csv")
setDT(datum)

colnames(datum)[1]="time"

datum$time = as.numeric(datum$time)
datum$Large = as.numeric(datum$Large)
datum$Medium = as.numeric(datum$Medium)
datum$Micro = as.numeric(datum$Micro)
datum$Total = as.numeric(datum$Total)
datum$Small = as.numeric(datum$Small)

datum[is.na(datum)] <- 0


firmtype <- function(x){
  if(x == "A")
    paste0("Agriculture Forestry and Fishing")
  else if(x == "B")
    paste0("Mining and Quarrying")
  else if(x == "C")
    paste0("Manufacturing")
  else if(x == "D")
    paste0("Electricity, Gas, Steam and Air Conditioning Supply")
  else if(x == "E")
    paste0("Water Supply; Sewerage; Waste Management and Remediation Activities")
  else if(x == "F")
    paste0("Construction")
  else if(x == "G")
    paste0("Trade")
  else if(x == "H")
    paste0("Transporting and Storage")
  else if(x == "I")
    paste0("Accommodation and Food Service Activities")
  else if(x == "J")
    paste0("Information and Communication")
  else if(x == "L")
    paste0("Real Estate Activities")
  else if(x == "M")
    paste0("Professional, Scientific and Technical Activities")
  else if(x == "N")
    paste0("Administrative and Support Service Activities")
  else if(x == "P")
    paste0("Education")
  else if(x == "Q")
    paste0("Human Health and Social Work Activities")
  else if(x == "R")
    paste0("Arts, Entertainment and Recreation")
  else if(x == "S")
    paste0("Other Services Activities")
  
}



# updatemenus component

ind <- list(
  list(
    label="Indicators",active = 0, type= 'dropdown',y=1.08, x=1.3 ,buttons = list(
      list(method = "restyle",args = list(), label = "--Income Statement--"),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[1]), label = unique(datum$code)[1]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[2]), label = unique(datum$code)[2]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[3]), label = unique(datum$code)[3]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[4]), label = unique(datum$code)[4]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[5]), label = unique(datum$code)[5]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[6]), label = unique(datum$code)[6]),
      list(method = "restyle",args = list(), label = "--Liquidity--"),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[7]), label = unique(datum$code)[7]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[8]), label = unique(datum$code)[8]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[19]),label = unique(datum$code)[19]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[20]),label = unique(datum$code)[20]),
      list(method = "restyle",args = list(), label = "--Indebtedness--"),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[9]), label = unique(datum$code)[9]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[10]),label = unique(datum$code)[10]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[11]),label = unique(datum$code)[11]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[12]),label = unique(datum$code)[12]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[13]),label = unique(datum$code)[13]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[14]),label = unique(datum$code)[14]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[15]),label = unique(datum$code)[15]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[16]),label = unique(datum$code)[16]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[17]),label = unique(datum$code)[17]),
      list(method = "restyle",args = list("transforms[0].value", unique(datum$code)[18]),label = unique(datum$code)[18]))),
  list(
    label="Indicators",active = 0, type= 'button',y=1.08, x=0.7,buttons = list(
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[1]), label =firmtype(unique(datum$name)[1])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[2]), label =firmtype(unique(datum$name)[2])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[3]), label =firmtype(unique(datum$name)[3])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[4]), label =firmtype(unique(datum$name)[4])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[5]), label =firmtype(unique(datum$name)[5])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[6]), label =firmtype(unique(datum$name)[6])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[7]), label =firmtype(unique(datum$name)[7])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[8]), label =firmtype(unique(datum$name)[8])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[9]), label =firmtype(unique(datum$name)[9])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[10]), label =firmtype(unique(datum$name)[10])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[11]), label =firmtype(unique(datum$name)[11])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[12]), label =firmtype(unique(datum$name)[12])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[13]), label =firmtype(unique(datum$name)[13])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[14]), label =firmtype(unique(datum$name)[14])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[15]), label =firmtype(unique(datum$name)[15])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[16]), label =firmtype(unique(datum$name)[16])),
      list(method = "restyle",args = list("transforms[1].value", unique(datum$name)[17]), label =firmtype(unique(datum$name)[17])) )))


fig<-datum %>%
  plot_ly(type = 'scatter', mod= 'line',mode = 'markers', 
          transforms = list(list(type = 'filter',target = ~code, operation = '=',value = unique(datum$code)[1]),
                            list(type='filter',target= ~name, operation = '=', value = unique(datum$name)[1])))%>%
  add_trace(x = ~time,y= ~Large, name = 'Large Firms',text=~Large, mode = "lines+markers", color = "#b2abd2") %>%
  add_trace(x = ~time,y= ~Small, name = 'Small-sized Firms',text=~Small, mode = "lines+markers", color="#b35806") %>%
  add_trace(x = ~time,y= ~Medium, name = 'Medium-sized Firms',text=~Medium, mode = "lines+markers", color="#e08214") %>% 
  add_trace(x = ~time,y= ~Micro, name = 'Micro-sized Firms',text=~Micro, mode = "lines+markers", color="#8073ac") %>%
  add_trace(x = ~time,y= ~Total, name = 'All Firms',text=~Total, mode = "lines+markers", color="#542788")%>%
  layout(xaxis=list(title="Year", tickvals= list(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020),fixedrange =T),
         yaxis=list(title="Percentage",fixedrange =T),
         updatemenus=ind,
         annotations = list(x = 0, y = 0, text = "Source: CBRT Company Accounts,Calculated by A. Ismet ASCI", 
                            showarrow = F, xref='paper', yref='paper', 
                            xshift=0, yshift=0,
                            font=list(size=15, color="black")))

fig

