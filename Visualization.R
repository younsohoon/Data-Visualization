library(dplyr)
library(ggplot2)
library(treemapify)
library(scales)

q3key = read.csv("/Users/sohoon/Desktop/938/A2/q3key.csv")
q3stats = read.csv("/Users/sohoon/Desktop/938/A2/q3stats.csv") 

#[1]  "Title"   "Guest.s."      "Panel"    "Link"  "Notes"   "Description"         
# will -->  Listens. 
# used --> Category, Format, Guest.Location, Venue,Length, Total.Number, Release.Date..m.d.y.  

df = q3stats[,2:length(q3stats)]
sum(is.null(df))

# sequential graph for year.month.date

##########################################################################################
# Categorical variables  ::   Category, Format, Guest.Location, Venue
##########################################################################################
# Categorical variables
unique(q3stats$Category)
unique(q3stats$Format)
unique(q3stats$Guest.Location)
unique(q3stats$Panel)

q3cat = data.frame(q3stats %>% group_by(Category, .drop = FALSE) %>% count())
q3format = data.frame(q3stats %>% group_by(Format, .drop = FALSE) %>% count())
q3guest.location = data.frame(q3stats %>% group_by(Guest.Location, .drop = FALSE) %>% count())
q3venue = data.frame(q3stats %>% group_by(Venue, .drop = FALSE) %>% count())

# q3cat
q3cat_g = ggplot(q3cat, 
       aes(x=n, 
           y=reorder(Category,n))) +
  geom_point(color="red", 
             size = 2) +
  geom_segment(aes(x = 0, 
                   xend = n, 
                   y = Category, 
                   yend = Category),
               color = "black") +
  labs (x = "Counts",
        y = "Category",
        title = "Categories") +
  theme_minimal()
q3cat_g  

# q3format
q3format_g = ggplot(q3format, 
                    aes(x = reorder(Format,n),y=n)) + 
  geom_bar(stat="identity", fill = "cornflowerblue", color="black") +
  geom_text(aes(label = n), 
            vjust=-0.5) +
  labs(x = "Format", 
       y = "Count", 
       title = "Count by Format") + 
  theme_minimal()
q3format_g # mean of length of 3 types of format 

# q3guest.location
q3guest_g = ggplot(q3guest.location, 
       aes(fill = Guest.Location, 
           area = n, 
           label = Guest.Location)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Guest Location") 
q3guest_g

# q3venue
q3venue_g = ggplot(q3venue, 
                    aes(x = reorder(Venue,n),y=n)) + 
  geom_bar(stat="identity", fill = "coral1", color="black") +
  geom_text(aes(label = n), 
            vjust=-0.5) +
  labs(x = "Venue", 
       y = "Count", 
       title = "Count by Venue") + 
  theme_minimal()
q3venue_g

##########################################################################################
# Numerical variables ::  Release.Date..m.d.y. , Length, Listens. , Total.Number , Description 
##########################################################################################

# length 
### Convert time to mins ###
q3stats$Length2 = sapply(q3stats$Length, function(x) {
  val = strsplit(x, split=":")[[1]]
  mn = as.numeric(val[1])
  sc = as.numeric(val[2])
  60 * mn + sc
})
### Convert mins to hours ###
q3stats$Length2_mns = sapply(q3stats$Length2, function(x) {
  round(x / 60)
})
q3stats$Length2_mns

ggplot(q3stats, aes(Length2_mns)) +
#  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(notch = TRUE, 
               fill = "orange", 
               alpha = .7) +
  labs(title = "Length of videos")

ggplot(q3stats, aes(Total.Number)) +
  geom_boxplot(notch = TRUE, 
               fill = "hotpink1", 
               alpha = .7) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + 
  labs(title = "Total Episode Number")


ggplot(q3stats, aes(Listens.)) +
  geom_boxplot(notch = TRUE,
               fill = "mediumorchid1",
               alpha = .7) +
  labs(title = "Total Episode Number") ####### addition : numbers depends on genere

ggplot(q3stats, aes(x=year)) + 
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_histogram(binwidth=1) +
  theme_minimal()

hist(q3stats$year)



