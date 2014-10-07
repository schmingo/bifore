BiFoRe scripts
=========

#### Fernerkundliche Vorhersage ausgewählter zoologischer Biodiversitätsparameter. 

<p> Der biologische Datensatz basiert auf Biodiversitätsdaten welche von Claudia Hemp am Kilimanjaro von 1996 - 2012 aufgenommen wurden. Die fernerkundliche Grundlage bildet der MODIS Satellit Aqua. </p>
<p> Zu jeder Observierung im Biodiversitätsdatensatz wird mittels MODIS Cloudmask (MYD35) eine zeitnahe wolkenfreie Szene (MYD02) selektiert. Über die Geokoordinaten werden aus allen 36 Bändern des MODIS Produkts die entsprechenden Grauwerte extrahiert und weiter prozessiert. Mit Hilfe des RandomForest Algorithmus wird anschließend eine Vorhersage erstellt und diese statistisch ausgewertet. </p>
