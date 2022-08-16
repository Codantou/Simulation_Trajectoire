library(stringr)
library(GoFKernel)
library(stringr)
library(plotly)
library(gganimate)
# prendre t comme inconnu pour inverser la fonction x(t)
replace_t = function(expr){
  expr_list = str_split(expr,"") |> unlist()
  Map(function(x){
    if(x %in% letters[-20]) gsub(x,"t",x)
    else x
  }, expr_list) %>% unlist() %>%
    paste0(collapse = "")
}


expr_x  = "60*t*cos(45)"
expr_y = "(-9.8/2)*t^2+60*t*sin(45)"
exp

t = seq(0,100,0.1)
x = function(t) eval(parse(text =expr_x))
y = function(t) eval(parse(text =expr_y))
y_t = Map(y,t) %>% unlist()
x_t = Map(x,t) %>% unlist()
dat = data.frame(t = t, x = x_t, y= y_t)

plot_ly(dat, x = ~t, y = ~x,z=~y,type = 'scatter3d', mode = 'lines',
        line = list(width = 4))
p = ggplot(dat %>% filter(y>=0),aes(x,y))+geom_point(size = 8, color = "blue")+
  geom_line()+ hrbrthemes::theme_ipsum()+
  transition_reveal(x)#+view_follow()
