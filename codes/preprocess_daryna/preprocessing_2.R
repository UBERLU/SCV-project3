get(load("/Users/darynabilodid/Documents/GitHub/SCV-project3/data.RData"))

attach(my_data)

my_data$Genre <- as.factor(Genre)
my_data$Type.of.acquisition <- as.factor(Type.of.acquisition)
my_data$Canton <- as.factor(Canton)
my_data$Year <- as.factor(Year)
my_data$Age <- as.factor(Age)
my_data$Previous.citizenship <- as.factor(Previous.citizenship)

levels(my_data$Genre) <-c("Sex - total","Male","Female")

levels(my_data$Type.of.acquisition) <- c("Type of acquisition - total",
                                 "Ordinary naturalisation",
                                 "Simplified naturalisation",
                                 "Reintegration",
                                 "Confirmation of the Swiss nationality",
                                 "Naturalisation by adoption")

levels(my_data$Canton) <- c("Switzerland","Zürich","Bern / Berne",
                    "Luzern","Uri","Schwyz","Obwalden",
                    "Nidwalden","Glarus","Zug","Fribourg / Freiburg",
                    "Solothurn","Basel-Stadt","Basel-Landschaft",
                    "Schaffhausen","Appenzell Ausserrhoden",
                    "Appenzell Innerrhoden","St. Gallen",
                    "Graubünden / Grigioni / Grischun",
                    "Aargau","Thurgau","Ticino","Vaud",
                    "Valais / Wallis","Neuchâtel",
                    "Genève","Jura","Unknown")

levels(my_data$Previous.citizenship)  <- c("Previous citizenship - total","Switzerland","Albania","Andorra",
                                           "Belgium","Bulgaria","Denmark","Germany",
                                           "Finland","France","Greece","United Kingdom","Ireland","Iceland",
                                           "Italy","Liechtenstein","Luxembourg","Malta","Monaco","Netherlands",
                                           "Norway","Austria","Poland","Portugal","Romania","San Marino",
                                           "Sweden","Spain","Turkey","Hungary","Vatican City State","Cyprus",
                                           "Slovakia","Czechia","Serbia","Croatia","Slovenia","Bosnia and Herzegovina",
                                           "Montenegro","North Macedonia","Kosovo","Estonia","Latvia","Lithuania",
                                           "Moldova","Russia","Ukraine","Belarus","Equatorial Guinea","Ethiopia",
                                           "Djibouti","Algeria","Angola","Botswana","Burundi","Benin","Côte d'Ivoire",
                                           "Gabon","Gambia","Ghana","Guinea-Bissau","Guinea","Cameroon","Cabo Verde",
                                           "Kenya","Comoros","Congo (Brazzaville)","Congo (Kinshasa)","Lesotho",
                                           "Liberia","Libya","Madagascar","Malawi","Mali","Morocco","Mauritania",
                                           "Mauritius","Mozambique","Niger","Nigeria","Burkina Faso","Zimbabwe","Rwanda",
                                           "Zambia","Sao Tomé and Principe","Senegal","Seychelles","Sierra Leone",
                                           "Somalia","South Africa","Sudan","Namibia","Eswatini","Tanzania",
                                           "Togo","Chad","Tunisia","Uganda","Egypt","Central African Republic",
                                           "Eritrea","South Sudan","Western Sahara","Argentina","Bahamas","Barbados",
                                           "Bolivia","Brazil","Chile","Costa Rica","Dominican Republic","Ecuador",
                                           "El Salvador","Guatemala","Guyana","Haiti","Belize","Honduras","Jamaica",
                                           "Canada","Colombia","Cuba","Mexico","Nicaragua","Panama","Paraguay","Peru",
                                           "Suriname","Trinidad and Tobago","Uruguay","Venezuela","United States","Dominica",
                                           "Grenada","Antigua and Barbuda","Saint Lucia","Saint Vincent and the Grenadines",
                                           "Saint Kitts and Nevis","Afghanistan","Bahrain","Bhutan","Brunéi Darussalam","Myanmar",
                                           "Sri Lanka","Taiwan (Chinese Taipei)","China","India","Indonesia","Iraq","Iran","Israel",
                                           "Japan","Yemen","Jordan","Cambodia","Qatar","Kuwait","Laos","Lebanon","Malaysia",
                                           "Maldives","Oman","Mongolia","Nepal","North Korea","United Arab Emirates","Pakistan",
                                           "Philippines","Saudi Arabia","Singapore","South Korea","Syria","Thailand","Vietnam",
                                           "Bangladesh","Timor-Leste","Palestine","Armenia","Azerbaijan","Georgia","Kazakhstan",
                                           "Kyrgyzstan","Tajikistan","Turkmenistan","Uzbekistan","Australia","Fiji","Nauru","Vanuatu",
                                           "New Zealand","Papua New Guinea","Tonga","Samoa","Solomon Islands","Tuvalu","Kiribati",
                                           "Marshall Islands","Micronesia","Palau","Cook Islands","Stateless",
                                           "Not attributable according to current borders","Unknown")

my_data$Age <- as.numeric(gsub(".*?([0-9]+).*", "\\1", my_data$Age))






