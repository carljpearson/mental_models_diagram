library(tidyverse)

#read data
data <- read_csv("/Users/carlpearson/Documents/r_github/mental_models_diagram/example_data.csv",col_names = T)

#subset for test
df <- data %>%
  #test only
  rename(task.tower=`Task tower`,atomic.task=`Atomic task`,mental.space=`Mental Space`) %>%
 
  #create ordering variable for y placement
  group_by(task.tower) %>% 
  mutate(ordering=1:n()) 

#create plot
g <- df %>%
  mutate(atomic.task=str_wrap(atomic.task,width=30)) %>% #wrap text
  ggplot(aes(x=task.tower,y=ordering,fill=Persona)) + #define vars
  geom_tile(color="white") + #add box
  geom_text(aes(label=atomic.task)) + #add text over boxes
  theme_void() + #remove lines 
  theme(
          axis.text.x = element_text() #add in task tower labels
        ) + 
  facet_wrap(~mental.space,scales = "free_x",nrow = 1) #each mental space is a facet

#large png
ggsave(g,
       filename = "/Users/carlpearson/Documents/r_github/mental_models_diagram/test.png",
       width=50,
       height=8,
       device="png",
       limitsize = F)

#test ggiraph

#testing plotlly
p<- plotly::ggplotly(g) 
  p %>%
    layout(autosize = F, width = 6000, height = 800, margin = m)
p  
  