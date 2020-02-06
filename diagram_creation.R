library(tidyverse)
library(grid)

#read data
data <- read_csv("/Users/carlpearson/Documents/r_github/mental_models_diagram/example_data.csv",col_names = T)

#subset for test
df <- data %>%
  #test only
  mutate(task.tower=as.factor(`Task tower`),atomic.task=as.factor(`Atomic task`),mental.space=as.factor(`Mental Space`)) %>%
  
  #create ordering variable for y placement
  group_by(task.tower) %>% 
  mutate(ordering=1:n()) 

#create plot
g <- df %>%
  ungroup() %>%
  mutate(atomic.task=str_wrap(atomic.task,width=30),
         task.tower=str_wrap(task.tower,width=20)
         ) %>% #wrap text
  ggplot(aes(x=task.tower,y=ordering,fill=Persona)) + #define vars
  geom_tile(color="white") + #add box
  geom_text(aes(label=atomic.task)) + #add text over boxes
  theme_void() + #remove lines 
  scale_y_reverse() + #make towers start at the top and go down
  scale_x_discrete(position = "top") + #put labels above towers
  theme(
          axis.text.x = element_text(#add in task tower labels
                                      size= 15
                                     ),
          strip.text.x = element_text( #adjust mental space labels
                                      size= 20,
                                      vjust=-1
            
                                 ),
          legend.position = "top"
        ) + 
  facet_wrap(~mental.space,scales = "free_x",nrow = 1) #each mental space is a facet


gd <- ggplot_gtable(ggplot_build(g))

gtable::gtable_show_layout(gd)

gd$widths[5] 
gd$widths[5] <- 4*gd$widths[5]
gd$widths[5] 

gd.print <- grid.draw(gd)

#large png
ggsave(gd.print,
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
  