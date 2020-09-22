ggplot(data=zi, mapping = aes(x=avg_rate)) + geom_bar(aes(y=y), stat = "identity") + geom_bar(aes(y=pg), stat = "identity")# + scale_y_continuous(name='First_Axis', sec.axis = sec_axis(name="Second_axis"))

zi<-data%>%
  group_by(pg, pmt)%>%
  summarise(avg_srate=mean(srate))

library(rsconnect)
library(shiny)
library(ggplot2)
library(dplyr)
library(purrr)

data=read.csv(file.choose(), sep=",", header = TRUE)

data<-mutate(data, srate=success*100/t)


x<-data%>%
   group_by(bank)%>%
   summarise(avg_srate=mean(srate))
y<-data%>%
  group_by(pg)%>%
  summarise(avg_srate=mean(srate))
z<-data%>%
  group_by(pmt)%>%
  summarise(avg_srate=mean(srate))
z2<-data%>%
  group_by(sub_type)%>%
  summarise(avg_srate=mean(srate))
ti<-data%>%
  group_by(hr)%>%
  summarise(avg_srate=mean(srate))

ggplot(data=x, mapping=aes(x=reorder(bank, -avg_srate), y=avg_srate)) + geom_bar(stat = "identity")

ggplot(data=y, mapping=aes(x=reorder(pg, -avg_srate), y=avg_srate)) + geom_bar(stat = "identity")

ggplot(data=z, mapping=aes(x=reorder(pmt, -avg_srate), y=avg_srate)) + geom_bar(stat = "identity")

ggplot(data=z2, mapping=aes(x=reorder(sub_type, -avg_srate), y=avg_srate)) + geom_bar(stat = "identity")

ggplot(data=ti, mapping=aes(x=reorder(hr, -avg_srate), y=avg_srate)) + geom_bar(stat = "identity")

ui <- fluidPage(
  
  plotOutput("bank_vs_srate"),
  plotOutput("pmt_vs_srate"),
  plotOutput("pg_vs_srate"),
  plotOutput("subtype_vs_srate"),
  plotOutput("hr_vs_srate")
)
server <- function(input, output, session) {

  output$bank_vs_srate <- renderPlot(ggplot(data=x, mapping = aes(x=reorder(bank, -avg_srate), y=avg_srate)) +  geom_bar(stat='identity'))
  output$pmt_vs_srate <- renderPlot(ggplot(data=z, mapping = aes(x=reorder(pmt, -avg_srate), y=avg_srate)) +  geom_bar(stat='identity'))
  output$pg_vs_srate <- renderPlot(ggplot(data=y, mapping = aes(x=reorder(pg, -avg_srate), y=avg_srate)) +  geom_bar(stat='identity'))
  output$subtype_vs_srate <- renderPlot(ggplot(data=z2, mapping = aes(x=reorder(sub_type, -avg_srate), y=avg_srate)) +  geom_bar(stat='identity'))
  output$hr_vs_srate <- renderPlot(ggplot(data=ti, mapping = aes(x=reorder(hr, -avg_srate), y=avg_srate)) +  geom_bar(stat='identity'))
  
  }
shinyApp(ui, server)

rsconnect::deployApp("C:/Users/avikk/Desktop/")

