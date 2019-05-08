
# Análisis de texto de un chat de WhatsApp

Este análisis se realiza para obtener información de acerca de la conversación y de sus participantes.
<br>Análisis por: Jorge Roberto Ahumada.

## Preparación del ambiente de trabajo
Carga e instalación de librerías y datos.
* Tidyverse: Librería para la limpieza y trabajo de datos que se compone de múltiples otras librerías, es una colección de paquetes para R.
* Rwhatsapp: Librería para el estudio de conversaciones de WhatsApp
* Wordcloud: Librería que permite formar nubes de palabras para entender de manera visual el uso de las palabras dentro del dataset.
* Stringr: Librería que facilita el trabajo y limpieza de strings.
* TM: Framework para minería de datos. 
* data.table: Librería para manejar con facilidad data sets en formato de tabla.
* Lubridate: Librería que simplifica el manejo de fechas.
* Emoji font: Paquete que permite la visualización de emoji dentro de R, principalmente en gráficas.
* Tidytext: Análisis y extracción de textos con base en el paquete dplyr.
* Stopwords: palabras que se repiten en varias ocasiones y que no afectan el contexto ni significado del mensaje.


```R
devtools::install_github("JBGruber/rwhatsapp")
library(tidyverse) 
library(rwhatsapp)
library(wordcloud)
library(stringr)
library(tm)
library(data.table)
library(lubridate)
library(emojifont)
library(tidytext)
library(stopwords)
```

    Downloading GitHub repo JBGruber/rwhatsapp@master
    

    
    v  checking for file 'C:\Users\Andrés\AppData\Local\Temp\Rtmp4oElBk\remotes10ac62453285\JBGruber-rwhatsapp-b05eb75/DESCRIPTION' (859ms)
    -  preparing 'rwhatsapp':
    v  checking DESCRIPTION meta-information ... 
    -  checking for LF line-endings in source and make files and shell scripts
    -  checking for empty or unneeded directories
    -  looking to see if a 'data/datalist' file should be added
    -  building 'rwhatsapp_0.1.3.9000.tar.gz'
       
    

    Installing package into 'C:/Users/Andrés/Documents/R/win-library/3.5'
    (as 'lib' is unspecified)
    


    Error in i.p(...): (converted from warning) installation of package 'C:/Users/ANDRS~1/AppData/Local/Temp/Rtmp4oElBk/file10ac5cfc2637/rwhatsapp_0.1.3.9000.tar.gz' had non-zero exit status
    Traceback:
    

    1. devtools::install_github("JBGruber/rwhatsapp")

    2. install_remotes(remotes, auth_token = auth_token, host = host, 
     .     dependencies = dependencies, upgrade = upgrade, force = force, 
     .     quiet = quiet, build = build, build_opts = build_opts, repos = repos, 
     .     type = type, ...)

    3. vapply(remotes, install_remote, ..., FUN.VALUE = character(1))

    4. FUN(X[[i]], ...)

    5. install(source, dependencies = dependencies, upgrade = upgrade, 
     .     force = force, quiet = quiet, build = build, build_opts = build_opts, 
     .     repos = repos, type = type, ...)

    6. safe_install_packages(pkgdir, repos = NULL, quiet = quiet, type = "source", 
     .     ...)

    7. with_envvar(c(R_LIBS = lib, R_LIBS_USER = lib, R_LIBS_SITE = lib), 
     .     if (should_error_for_warnings()) {
     .         with_options(list(warn = 2), with_rprofile_user("options(warn = 2)", 
     .             i.p(...)))
     .     } else {
     .         i.p(...)
     .     })

    8. force(code)

    9. with_options(list(warn = 2), with_rprofile_user("options(warn = 2)", 
     .     i.p(...)))

    10. force(code)

    11. with_rprofile_user("options(warn = 2)", i.p(...))

    12. with_envvar(c(R_PROFILE_USER = temp_rprofile), {
      .     force(code)
      . })

    13. force(code)

    14. force(code)

    15. i.p(...)

    16. warning(gettextf("installation of package %s had non-zero exit status", 
      .     sQuote(update[i, 1L])), domain = NA)

    17. .signalSimpleWarning("installation of package 'C:/Users/ANDRS~1/AppData/Local/Temp/Rtmp4oElBk/file10ac5cfc2637/rwhatsapp_0.1.3.9000.tar.gz' had non-zero exit status", 
      .     quote(i.p(...)))

    18. withRestarts({
      .     .Internal(.signalCondition(simpleWarning(msg, call), msg, 
      .         call))
      .     .Internal(.dfltWarn(msg, call))
      . }, muffleWarning = function() NULL)

    19. withOneRestart(expr, restarts[[1L]])

    20. doWithOneRestart(return(expr), restart)



```R
library(rwhatsapp)
base <- rwa_read("https://raw.githubusercontent.com/Manamaster/analisisWhatsApp/master/chatWA.txt")
head(base)
```


    `by` can't contain join column `emoji` which is missing from LHSTraceback:
    

    1. rwa_read("https://raw.githubusercontent.com/Manamaster/analisisWhatsApp/master/WhatsAppChatArkha.txt")

    2. dplyr::bind_cols(tbl, rwa_add_emoji(tbl))

    3. flatten_bindable(dots_values(...))

    4. dots_values(...)

    5. rwa_add_emoji(tbl)

    6. dplyr::left_join(out, rwhatsapp::emojis, by = "emoji")

    7. left_join.tbl_df(out, rwhatsapp::emojis, by = "emoji")

    8. common_by(by, x, y)

    9. common_by.character(by, x, y)

    10. common_by.list(by, x, y)

    11. bad_args("by", "can't contain join column {missing} which is missing from LHS", 
      .     missing = fmt_obj(setdiff(by$x, x_vars)))

    12. glubort(fmt_args(args), ..., .envir = .envir)

    13. .abort(text)


## Se prepara el texto para su posterior análisis.
Primero se cambian los mensajes a mayúsculas para evitar que se tripliquen los mensajes cuando el algoritmo interprete las cadenas (palabras) como únicas.


```R
base$text <- toupper(base$text)
```

Se reemplazan los caracteres acentuados con vocales.


```R
base$text <- chartr('ÁÉÍÓÚ', 'AEIOU', base$text)
```

Se remueven las palabras innecesarias, puntuación, espacios en blanco y los mensajes de que se omitió el archivo adjunto cuando se exportó el chat.<br>(En caso de que en la conversación se encuentren otros tipos de risa, se pueden agregar a la lista usando ' ')


```R
base$text <- removeWords(base$text, c(toupper(stopwords("spanish")),
                                      'OK', 'JAJA', 'JAJAJA', 'JAJAJAJA', 'MEDIA OMITTED'))
base$text <- stripWhitespace(base$text)
```

Eliminar los mensajes en blanco si la columna "mensaje" está vacía (=""), si es un mensaje sobre el chat (mensajes de añadidura o remoción de miembros), etc.


```R
base <- base %>% 
  filter(!str_detect(text,'AÑADIO'),
         !str_detect(text,'ELIMINO'),
         !str_detect(text,'CREO'),
         !str_detect(text,'SALIO'),
         !str_detect(text,'CAMBIO'),
         !base$text %in% ''
  )
length(unique(base$author))
```

Generación de un Top 10 de los participantes del chat.<br>
Se verifica que la columna "author" no esté vacía, se agrupa por autor, se sumariza y se acomodan los autores con estos estándares. <br>
Se unen las dos bases, la original y la generada en el paso anterior para formar una nueva.


```R
base_2 <- base %>%
  filter(!is.na(author)) %>% 
  group_by(author) %>% 
  summarise(cuenta = n()) %>% 
  arrange(-cuenta) %>% 
  top_n(10, cuenta) 

base_3 <- plyr::join(x = base, y = base_2, by = "author")
base_3 <- base_3 %>% 
  filter(!is.na(cuenta))
length(unique(base_3$author))
```

## Visualización de la información
Graficación de los mensajes por día.


```R
base %>% 
  mutate(day = date(time)) %>% 
  count(day) %>% 
  ggplot(aes(x = day,y = n)) +
  geom_bar(stat = "identity") + 
  ggtitle("Mensajes por día") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
```

Gráfica de los mensajes por autor.


```R
base_3 %>% 
  mutate(day = date(time)) %>% 
  count(author)  %>% 
  ggplot(aes(x = reorder(author, n),y = n)) +
  geom_bar(stat = "identity") + 
  ggtitle("Mensajes por autor") +
  coord_flip()
```

Gráfica de los emoji más utilizados dentro de la conversación


```R
base_3 %>% 
  unnest(emoji) %>% 
  count(author, emoji, sort = TRUE) %>% 
  group_by(author) %>% 
  top_n(n = 6) %>% 
  ggplot(aes(x = reorder(emoji, n), 
             y = n, 
             fill = author)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Emoji más utilizados") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

```

Las palabras más utilizadas (favoritas) por integrantes de la conversación.


```R

base_3 %>% 
  unnest_tokens(input = "text",
                output = "word") %>%
  filter(!word %in% c(
    stopwords(language = "es"),
    "media",
    "omitted",
    "ref",
    "hola",
    "gracias",
    "favor"
  )) %>% 
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 5) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = author)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("Most often used words") +
  theme_bw()

```

Días en los que hubo una mayor cantidad de mensajes.


```R
base <- base %>% 
  mutate(weekday = weekdays(time))

base %>% 
  count(weekday, sort = TRUE) %>%
  group_by(weekday) %>%
  ggplot(aes(x = reorder(weekday, n), y = n, fill = weekday)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  ggtitle("Mayor tráfico dentro de la conversación por día de la semana") +
  theme_bw()

```

Información sobre los tipos de documentos adjuntos y la frecuencia con la que son enviados. <br>
Primero se crea una nueva columna dentro de la tabla para que, de acuerdo con los datos que detecta usando la función str_detect, marque el archivo adjunto con su extensión en la columna recién creada.


```R
base_4 <- base %>% 
  filter(str_detect(text, "ADJUNTO" )) %>% 
  count(author, text, sort = TRUE) %>%
  mutate(file = ifelse(test = str_detect(text, ".PDF"),
                       yes = "PDF",
                       no = ifelse(test = str_detect(text, ".JPG"),
                                   yes = "JPG",
                                   no = ifelse(str_detect(text, ".DOC"),
                                               yes = "WORD",
                                               no = ifelse(str_detect(text, ".PPT"),
                                                           yes = "PPT",
                                                           no = ifelse(str_detect(text, "VIDEO"),
                                                                       yes = "VIDEO" ,
                                                                       no = ifelse(str_detect(text, ".VCF"),
                                                                                   yes = "VCF",
                                                                                   no = "OTRO")))))))

base_4 %>% 
  group_by(author) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = reorder(author, n), y = n, fill = author)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  ggtitle("Archivos adjuntos por usuario.") +
  theme_bw()
```


```R
base_4 %>% 
  group_by(file) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = reorder(file, -n), y = n, fill = file)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  ggtitle("Cantidad de archivos por tipo.") +
  theme_bw()
```
