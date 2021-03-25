library(chilemapas)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(readr)
library(lubridate)
library(stringr)

## --
## Input: Mapa 
## --

# Vector de comunas del "Gran Santiago"
comunas_considerar <- c("Huechuraba", "Quilicura", "Pudahuel", "Maipú",
                       "Padre Hurtado", "Cerrillos", "San Bernardo",
                       "La Pintana", "Pirque", "San José de Maipo",
                       "Puente Alto", "La Florida", "Peñalolén",
                       "Las Condes", "Lo Barnechea", "Vitacura",
                       "Conchalí", "Recoleta", "Independencia", "Quinta Normal",
                       "Renca", "Cerro Navia", "Lo Prado", "Estación Central",
                       "Santiago", "Pedro Aguirre Cerda", "Lo Espejo",
                       "El Bosque", "La Cisterna", "San Ramón", "La Granja",
                       "San Miguel", "San Joaquín", "Macul", "La Reina",
                       "Ñuñoa", "Providencia")

# Vector de localidades a no considerar
urbano_noconsiderar <- c("EL PRINCIPAL", "SAN ALFONSO", "SAN JOSÉ DE MAIPO",
                         "CIUDAD DEL VALLE", "VILLA CAMPO ALEGRE", "EL CAMBUCHO",
                         "LO AGUIRRE", "LO HERRERA", "NOVICIADO ALTO", "EL MAITÉN")

# Cargar shape de zonas urbanas (sacar zonas "discontinuas")
RM_urb_shp <- st_read("shapes/Zonas_urbanas_2017_Chile.shp", quiet = TRUE) %>% 
  filter(NOM_COMUNA %in% str_to_upper(comunas_considerar),
         !URBANO %in% urbano_noconsiderar)

# Ver Shape "RM_urb_shp"
ggplot() +
  geom_sf(data = RM_urb_shp) +
  theme_void()

# Unir los shapes de localidad a nivel de comuna (resulta un shape con las divisiones por comuna)
RM_urb_shp_comb <- RM_urb_shp %>% 
  distinct(COMUNA, NOM_COMUNA, geometry) %>% 
  group_by(COMUNA) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup()

# Ver Shape "RM_urb_shp_comb"
ggplot() +
  geom_sf(data = RM_urb_shp_comb) +
  theme_void()

## --
## Input: Datos etapa
## --

# Cargar datos desde github del ministerio de ciencias
pasoapaso <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto74/paso_a_paso.csv")

# Dejar solo datos para región metropolitana/zonas urbanas
# pasar datos de "ancho" a "largo"
# transformar "clase" de algunas variables
pasoapaso_rm <- pasoapaso %>% 
  filter(codigo_region == 13,
         zona %in% c("Total", "Urbana")) %>% 
  select(-zona) %>% 
  pivot_longer(5:(ncol(pasoapaso)-1), names_to = "fecha", values_to = "etapa") %>% 
  mutate(codigo_comuna = as.character(codigo_comuna),
         etapa = as.factor(etapa),
         fecha = ymd(fecha))

## --
## Unir información 
## --

# Anexar a mapa (shapefile) información del paso a paso
RM_urb_shp_pp <- left_join(RM_urb_shp_comb, pasoapaso_rm, by = c("COMUNA" = "codigo_comuna"))

# Probar con un día ("hoy") que esté todo OK
RM_urb_shp_pp %>% 
  filter(fecha == today()) %>% 
  ggplot() +
  geom_sf(aes(fill = etapa), col = NA) +
  scale_fill_manual(values = c("red", "yellow", "orange", "green")) +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  labs(title = "Fecha: XXXX-XX-XX")

## --
## Animación
## --

# Crear animación
animacion_pasopaso <- RM_urb_shp_pp %>% 
  as_tibble() %>%
  ggplot() +
  geom_sf(aes(fill = etapa, geometry = geometry), col = NA) +
  scale_fill_manual(values = c("red", "orange", "yellow", "green")) +
  theme_void() +
  transition_time(fecha) +
  theme(plot.title = element_text(size = 16, face = "bold"))  +
  labs(title = 'Fecha: {frame_time}')

# Mostrar animación
animate(animacion_pasopaso, fps = 5, renderer = gifski_renderer(loop = FALSE))

# Guardar animación como GIF
anim_save("GifPasoaPaso.gif")

