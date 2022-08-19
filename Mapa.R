#  Cargamos las Librerias ----------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
library(elevatr)
library(ggspatial)
library(tmap)
library(ggpubr)
library(ggrepel)
library(ggforce)
library(grid)
library(png)
library(ggimage)

SurAmerica = st_read("SHP/Sur_america.shp")  %>% st_as_sf()
SurAmeric <- st_transform(SurAmerica  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

library(rgbif)
Theobroma   <- occ_search(scientificName="Theobroma cacao")
cacao           <- subset(Theobroma$data , scientificName == "Theobroma cacao L.")
cacao$image <- "PNG/Imagen.png"


Cacaogg= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black")+
  geom_sf(data = Peru , fill="white", color="black")+
  geom_point(data = cacao, aes( x=decimalLongitude, y = decimalLatitude, 
                                color=scientificName) ,size=4, alpha=0.3)+
  coord_sf(xlim = c(-81.007, -34.86064 ), ylim = c(-55.05167,12.46331))+
  theme_classic()+
  theme(legend.position = c(0.70, 0.20),
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.5,"cm"), #ancho de cuadrados de referencia 
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, family="serif", face = "italic"),
        panel.background = element_rect(fill = "#a9def9"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(color = 'Especie',  x = 'Longitud', y = 'Latitud',
       title = "Mapa de dispersion de la especie Theobroma cacao L.",
       subtitle = "Datos de Rgbif",
       caption = "Codigo en https://github.com/GorkyFlorez/Dis_Cacao_Sur_America")+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -80, y = -20, hjust = 0, vjust = 1, 
           label = "Oeano \nPacifico",size = 3, family="serif", color = 
             "#03045e",  fontface="italic")+
  annotate(geom = "text", x = -50, y = 11, hjust = 0, vjust = 1, 
           label = "Oeano \nAtlantico",size = 3, family="serif", color = 
             "#03045e",  fontface="italic")+
  annotate(geom = "text", x = -80, y = -50, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo      Gorky Florez Castillo  Gorky  Florez Castillo  Gorky Florez Castillo  Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)+
  geom_hline(yintercept  = 0,
             size=1,
             linetype = 2,
             color = "red") +
  annotate(geom = "text",
           x = -60,y = 0,
           color="red",
           label = "Linea ecuatorial",
           fontface = "plain",
           vjust = 1)



ggsave(plot=Cacaogg,"Mapa de dispersion.png",units = "cm",width = 13, #alto
       height = 15, #ancho
       dpi=1200)




