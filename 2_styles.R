# COLOR PALETTE ####
colorpal <- c("#d6ba72", "#0484fb", "#1b7e24", "#c00f0a", 
              "#fbc500", "#1a64c2", "#7cc4a4", "#680514")

# STOP WORDS ####
my_stop_words <- tibble(word=c("jamaica","queens","neighborhood",
                                "street","avenue","block","boulevard",
                                "42nd","11","13",
                                "southeast",
                                "house","home",
                                "remember","time","day","lived",
                                "yeah","lot","started","moved","called",
                                  "um","uh","constantly","whatnot","eventually","dah",
                                "xfriendx","xintervieweex",
                               "eighties","1970s","mid","1980s","1930s"
                              ))
all_stop_words <- bind_rows(stop_words, my_stop_words)

