my_data <- my_data %>%
  filter(value.Kanton != "Schweiz" & value.Alter != "Alter - Total"
         & value.Geschlecht != "Geschlecht - Total" &
           value.Art.des.Erwerbs != "Art des Erwerbs - Total"
         & value.Staatsangehörigkeit.ehemalig != "Staatsangehörigkeit ehemalig - Total"
         & value.Staatsangehörigkeit.ehemalig != "Schweiz")

my_data <- rename(my_data, Previous.citizenship = value.Staatsangehörigkeit.ehemalig)
my_data <- rename(my_data, Age = value.Alter)
my_data <- rename(my_data, Genre = value.Geschlecht)
my_data <- rename(my_data, Type.of.acquisition = value.Art.des.Erwerbs)
my_data <- rename(my_data, Canton = value.Kanton)
my_data <- rename(my_data, Year = value.Jahr)
my_data <- rename(my_data, Value = value.value)
