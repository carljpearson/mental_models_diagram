library(tidyverse)
library(gridExtra)

#read data
data <- read_csv("/Users/carlpearson/Documents/r_github/mental_models_diagram/example_data.csv",col_names = T)

#subset for test
df <- data %>%
  #test only
  mutate(task.tower=as.factor(`Task tower`),
         atomic.task=as.factor(`Atomic task`),
         mental.space=as.factor(`Mental Space`)) %>%
  filter(`Mental Space` %in% c("Issue Discovery","Issue resolution", "Development","Team management")) %>%
  #create ordering variable for y placement
  group_by(task.tower) %>% 
  mutate(ordering=1:n()) 

#create plot
g <- df %>%
  ungroup() %>%
  mutate(atomic.task=str_wrap(atomic.task,width=25),
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
                                      size= 15,
                                      vjust=-.01
                                     ),
          strip.text.x = element_text( #adjust mental space labels
                                      size= 20,
                                      vjust=2
            
                                 ),
          legend.position = "top" #put the legend on top
        ) + 
  facet_wrap(~mental.space,scales = "free_x",nrow = 1) #each mental space is a facet


gd <- ggplot_gtable(ggplot_build(g)) #get grid drawn object

      #for testing,
      #gtable::gtable_show_layout(gd)  view object
      #4 on edge, target, 3 buffer, target, 3 buffer, target, 4 on edge
      #gd$widths

#get the number of task towers in a mental space
mental.space.widths <- df %>% 
  group_by(mental.space,task.tower) %>% 
  count() %>% 
  group_by(mental.space) %>% 
  count(name="facet_width") 

#get number of mental spaces
mental.spaces.total_n <- nrow(mental.space.widths)

#add number for replacement function in gd based on 4/x/3/x/3/x/4 pattern
mental.space.widths$grid_location <- seq(from=5, to=5+(3*mental.spaces.total_n), by=4)
  
gd$widths[mental.space.widths$grid_location] <- mental.space.widths$facet_width*gd$widths[mental.space.widths$grid_location]

#view test
grid.draw(gd)

#large png
ggsave(plot=gd,
       filename = "/Users/carlpearson/Documents/r_github/mental_models_diagram/test.png",
       width=13.3*mental.spaces.total_n,
       height=max(df$ordering)+1,
       device="png",
       limitsize = F)









#test ggiraph

#testing plotlly
p<- plotly::ggplotly(gd.print) 
  p %>%
    layout(autosize = F, width = 6000, height = 800, margin = m)
p  
  