knitr::include_graphics("poster.png")

q3key = read.csv("q3key.csv")
q3stats = read.csv("q3stats.csv")

q3stats$Length2 = sapply(q3stats$Length, function(x) {
  val = strsplit(x, split=":")[[1]]
  mn = as.numeric(val[1])
  sc = as.numeric(val[2])
  round(mn + sc/60,0)
})
q3stats$year = sapply(q3stats$Release.Date..m.d.y., function(x) {
  val = strsplit(x, split="/")[[1]]
  year = as.numeric(val[3])
  year
})

q3cat = data.frame(q3stats %>% group_by(Category, .drop = FALSE) %>% count())
q3format = data.frame(q3stats %>% group_by(Format, .drop = FALSE) %>% count())
q3guest.location = data.frame(q3stats %>% group_by(Guest.Location, .drop = FALSE) %>% count())
q3venue = data.frame(q3stats %>% group_by(Venue, .drop = FALSE) %>% count())
q3cat_yr = data.frame(q3stats %>% group_by(Category,year, .drop = FALSE) %>% count())

cat_length = aggregate(Length2 ~ Category, data = q3stats, FUN = median) 
cat_epnumb = aggregate(Total.Number ~ Category, data = q3stats, FUN = median)
cat_year = aggregate(year ~ Category, data = q3stats, FUN = median)

par(mfrow=c(1,2))
palette = c("deeppink4","gold1","cornflowerblue","coral1","purple","seagreen","sandybrown","azure4","forestgreen","plum1","blue3","violetred","sienna3","lightskyblue3","olivedrab3","wheat2")
# q3guest.location
q3guest_g = ggplot(q3guest.location, 
                   aes(fill = Guest.Location, 
                       area = n, 
                       label = paste0(Guest.Location,"\n", prettyNum(n)))) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", place = "centre") +
  labs(title = "Guest Location") +
  scale_fill_manual(values=palette)
q3guest_g

par(mfrow=c(1,2))
q3cat_g = ggplot(q3cat, 
                 aes(x=n, 
                     y=reorder(Category,n))) +
  geom_point(color="red", 
             size = 2) +
  geom_segment(aes(x = 0, 
                   xend = n, 
                   y = Category, 
                   yend = Category),
               color = "azure4") +
  labs (x = "Episode Counts",
        y = "Category",
        title = "Episode Count by Category") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size= 2.5, color = "red")+
  theme_bw()
q3cat_g  

par(mfrow=c(1,2))
q3format_g = ggplot(q3format, 
                    aes(x = reorder(Format,n),y=n)) + 
  geom_bar(stat="identity", fill = "cornflowerblue", color="black") +
  geom_text(aes(label = n), 
            vjust=-0.5) +
  labs(x = "Format", 
       y = "Count", 
       title = "Episode Count by Format") + 
  ylim(0,250) +
  theme_bw()
q3format_g 

q3cat_format = data.frame(q3stats %>% group_by(Category,Format, .drop = FALSE) %>% count())
palette = distinctColorPalette(3)
ggplot(q3cat_format, aes(x = Category, y = n, fill = Format)) + 
  geom_bar(stat = "identity") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("deepskyblue3","purple1","blue4")) + 
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size= 2, color = "white") +
  labs(title = "Episode Count by Format & Category", y = "Count", x = "Category") 

par(mfrow=c(1,2))
ggplot(q3stats, aes(x=year)) + 
  geom_histogram(color = "#000000", fill = "#0099F8", binwidth = 1) +
  scale_x_continuous(breaks=seq(2013,2023,1)) +
  labs (x = "Release Year",
        y = "count",
        title = "Episode Count by Release Year") +
  stat_bin(binwidth=1, geom='text', color='white', aes(label=..count..),
           position=position_stack(vjust = 0.5)) + 
  theme_bw()

palette = c("hotpink","black","red","wheat3","purple","seagreen","sandybrown","salmon4","coral1","cornflowerblue","blue3")
q3cat_yr[,2] = factor(q3cat_yr[,2])
ggplot(q3cat_yr, aes(x = Category, y = n, fill = year)) + 
  geom_bar(stat = "identity")+
  theme_bw() + 
  geom_text(aes(label = year), position = position_stack(vjust = 0.5), size= 1.4, color = "white") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=palette) + 
  labs(title = "Episode Count by Year & Category", y = "Count", x = "Category")

par(mfrow=c(1,2))
q3venue_g = ggplot(q3venue, 
                   aes(x = reorder(Venue,n),y=n)) + 
  geom_bar(stat="identity", fill = "coral1", color="black") +
  geom_text(aes(label = n), 
            vjust=-0.5) +
  labs(x = "Venue", 
       y = "Count", 
       title = "Episode Count by Venue") + 
  ylim(0, 250) +
  theme_bw()
q3venue_g

q3cat_v = data.frame(q3stats %>% group_by(Category,Venue, .drop = FALSE) %>% count())
ggplot(q3cat_v, aes(x = Category, y = n, fill = Venue)) + 
  geom_bar(stat = "identity") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values=c("darkorange1","magenta4","goldenrod2")) + 
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size= 2, color = "white") +
  labs(title = "Episode Count by Venue & Category", y = "Count", x = "Category") 

par(mfrow=c(1,2))
ggplot(cat_length, aes(x=Category,y=Length2,group=1))+
  geom_line(color='red')+
  geom_point(color='red')+
  theme_bw()+
  ylab("Length (min)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Episode Length by Category", y = "Length(min)", x = "Category")

ggplot(cat_epnumb, aes(x=Category,y=Total.Number,group=1))+
  geom_line(color='blue')+
  geom_point(color='blue')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Total Episode Number by Category", y = "Total Episode Number", x = "Category")

