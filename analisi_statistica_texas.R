install.packages("ggplot2")
install.packages("summarytools")
install.packages("dplyr")
install.packages("moments")
library(moments)
library(summarytools)
library(dplyr)
library(ggplot2)
dati=read.csv("realestate_texas.csv")
head(dati,5)
#vediamo da cosa è composto il dataset con il comando str
str(dati)

#step 1 analisi delle variabili

#Il dataset contiene diverse variabili di natura qualitativa e quantitativa, che permettono di analizzare il mercato immobiliare da più prospettive.

#city: variabile qualitativa nominale, che identifica la città di riferimento. Non è ordinabile e consente analisi tramite distribuzioni di frequenza o confronti tra gruppi.
#year: variabile quantitativa discreta che rappresenta la dimensione temporale annuale del dataset. Permette di analizzare l’evoluzione delle vendite nel tempo, identificando trend, anni con valori massimi o minimi e variazioni nel mercato.
#month: variabile quantitativa discreta con valori da 1 a 12. Rappresenta una dimensione temporale intra-annuale ed è utile per studiare la stagionalità delle vendite, individuando mesi con maggiore o minore attività.
#sales: variabile quantitativa discreta che rappresenta il numero totale di vendite. È una delle variabili principali per l’analisi del mercato, utilizzabile per calcolare medie, variabilità e trend nel tempo.
#volume: variabile quantitativa continua che indica il valore totale delle vendite (in milioni di dollari). Permette di analizzare il peso economico delle transazioni immobiliari.
#median_price: variabile quantitativa continua che rappresenta il prezzo mediano degli immobili. È utile per studiare la distribuzione dei prezzi e confrontare i livelli tra città e periodi.
#listings: variabile quantitativa discreta che indica il numero di annunci attivi. Consente di analizzare l’offerta nel mercato immobiliare.
#months_inventory: variabile quantitativa continua che rappresenta il tempo necessario per vendere tutte le inserzioni attive. È un indicatore dell’equilibrio tra domanda e offerta.


#STEP 2 indici di posizone, variabilità e forma e costruzione tabelle di frequenze

#varibile City
N=dim(dati)[1]
table(dati$city)
freq_ass_city=table(dati$city)
freq_rel_city=table(dati$city)/N
distr_freq_city=cbind(freq_ass_city,freq_rel_city)
distr_freq_city
# Non si calcolano le frequenze cumulate per la variabile city,
# in quanto si tratta di una variabile qualitativa nominale,
# priva di un ordinamento naturale tra le modalità.
# Di conseguenza, le frequenze cumulate non hanno interpretazione statistica.

#varibaile YEAR
table(dati$year)
freq_ass_year=table(dati$year)
freq_rel_year=table(dati$year)/N
freq_ass_cum_year=cumsum(freq_ass_year)
freq_rel_cum_year=cumsum(freq_rel_year)
distr_freq_year=cbind(freq_ass_year,freq_rel_year,freq_ass_cum_year,freq_rel_cum_year)
distr_freq_year

#varibiale month
table(dati$month)
freq_ass_month=table(dati$month)
freq_rel_month=table(dati$month)/N
distr_freq_month=cbind(freq_ass_month,freq_rel_month)
distr_freq_month

# Le distribuzioni di frequenza delle variabili year e month risultano uniformi.
# In particolare, ogni anno presenta lo stesso numero di osservazioni (48),
# pari al 20% del totale, mentre ogni mese compare con la stessa frequenza (20 osservazioni, 8,33%).
# Questa uniformità indica che il dataset è bilanciato nel tempo,
# consentendo confronti coerenti tra anni e mesi senza distorsioni dovute alla numerosità dei dati.


#iniziamo dalla varibaile sales
mean_sales=mean(dati$sales)
median_sales=median(dati$sales)
quantile_sales=quantile(dati$sales)
quantile_sales
min(dati$sales)
max(dati$sales)
range_sales=range(dati$sales)
iqr_sales=IQR(dati$sales)
var_sales=var(dati$sales)
sd_sales=sd(dati$sales)
skew_sales = skewness(dati$sales)
kurt_sales = kurtosis(dati$sales)

skew_sales
kurt_sales

cv=function(x){
  return(sd(x)/mean(x)*100)
}
cv(dati$sales)
var_sales
sd_sales
descr(dati$sales)
#La variabile sales presenta una media di 192,3 e una mediana di 175,5, suggerendo una distribuzione leggermente 
#asimmetrica a destra. La deviazione standard pari a 79,65 e il coefficiente di variazione del 41,42% indicano 
#una variabilità piuttosto elevata nelle vendite mensili, che può essere dovuta a diversi fattori esterni.

#variabile volume
# per gli altri indici utilizzero una libreria esterna ad r, cosi da non rendere il codice ridondante
descr(dati$volume)
# La variabile volume presenta una media superiore alla mediana (31,01 > 27,06),
# indicando una distribuzione leggermente asimmetrica a destra.
# La deviazione standard pari a 16,65 risulta elevata rispetto alla media,
# evidenziando una dispersione significativa dei dati.
# Il coefficiente di variazione pari a circa 54% conferma un’elevata variabilità,
# suggerendo forti differenze nel valore economico delle vendite tra città e periodi.

#VARIABILE MEDIAN_PRICE
descr(dati$median_price)
# La variabile median_price presenta una media (132.665,42) molto vicina alla mediana (134.500),
# indicando una distribuzione sostanzialmente simmetrica e ben centrata.
# La deviazione standard pari a 22.662,15 è moderata rispetto al valore medio,
# mentre il coefficiente di variazione pari a circa 17% evidenzia una bassa variabilità.
# Questo suggerisce una relativa stabilità dei prezzi immobiliari nel periodo analizzato,
# con assenza di forti oscillazioni nel mercato.

#VARIABILE LISTINGS
descr(dati$listings)
# La variabile listings rappresenta il numero di annunci immobiliari attivi.
# La media (1.738,02) è leggermente superiore alla mediana (1.618,50),
# suggerendo una lieve asimmetria positiva dovuta a valori elevati in alcune osservazioni.
# La deviazione standard pari a 752,71 indica una dispersione significativa dei dati,
# mentre il coefficiente di variazione pari a circa 43% evidenzia un’elevata variabilità.
# Questo suggerisce che il mercato immobiliare non è uniforme nel tempo e tra le città,
# con fasi di forte espansione dell’offerta e altre di contrazione.

#VARIABILE MONTH_INVENTORY
descr(dati$months_inventory)
# La variabile months_inventory rappresenta il tempo medio necessario per vendere le inserzioni immobiliari.
# La media (9,19) è molto vicina alla mediana (8,95), indicando una distribuzione abbastanza simmetrica.
# La deviazione standard pari a 2,30 è relativamente contenuta, mentre il coefficiente di variazione pari a circa 25%
# evidenzia una variabilità moderata.
# Questo suggerisce una certa stabilità nei tempi di vendita degli immobili,
# con differenze presenti ma non eccessivamente marcate tra città e periodi.

#STEP 3 Identificazione delle variabili con maggiore variabilità e asimmetria

#mettiamo a confronto tutti i cv delle variabili
cv_sales=cv(dati$sales)
cv_volume=cv(dati$volume)
cv_median_price=cv(dati$median_price)
cv_listings=cv(dati$listings)
cv_months_invetory=cv(dati$months_inventory)
cv_table=cbind(cv_sales,cv_volume,cv_median_price,cv_listings,cv_months_invetory)
cv_table
# Dall’analisi dei coefficienti di variazione emerge che la variabile con maggiore variabilità è volume,
# seguita da listings e sales.
# Questo indica che il valore economico delle transazioni è la componente più instabile del dataset,
# con forti oscillazioni tra città e periodi temporali.

skew_sales=skewness(dati$sales)
skew_volume=skewness(dati$volume)
skew_median_price=skewness(dati$median_price)
skew_listings=skewness(dati$listings)
skew_months_inventory=skewness(dati$months_inventory)

skew_table=cbind(skew_sales, skew_volume, skew_median_price,
                    skew_listings, skew_months_inventory)

skew_table
# L’analisi dell’asimmetria evidenzia che la variabile più asimmetrica è volume,
# con una distribuzione fortemente sbilanciata a destra.
# Ciò indica la presenza di valori estremi elevati che influenzano la distribuzione.

#STEP 4, DIVISONI IN CLASSI

dati$sales_cl=cut(dati$sales, breaks=c(30,80,130,180,230,280,330,380,430))
table(dati$sales_cl)
ni_cl=table(dati$sales_cl)
fi_cl=table(dati$sales_cl)/N
Ni_cl=cumsum(ni_cl)
Fi_cl=Ni_cl/N

cbind(ni_cl,fi_cl,Ni_cl,Fi_cl)

#una volta suddivisa in classe la varibaile possiamo trovare la classe modale,mediana e le classi dei qaurtili che sono:
# classe modale= 80,130 poichè ha la frequenza assoluta più alta
#classe mediana= 130,180 dato che per quella classe FI_ci>0.5
#classe del primo quartile = 80,130 ovvero la classe con Fi_cl > 0.25
#classe del secondo quartile = classe mediana
#classe terzo quartile= 230,280 ovvero la classe con Fi_cl>0.75

#BOX PLOT DISTRIBUZIONI IN CLASSE PER LA VARIBIALE SALES
library(ggplot2)
ggplot(data=dati)+
  geom_bar(aes(x=sales_cl),
           stat="count",
           col="black",
           fill="blue")+
  labs(title="distrubuzione delle classi di lunghezza",
             x="lunghezza in classi, cm",
             y="frequenze assolute")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))


#adesso clacoliamo l'indice di gini
gini_index=function(x){
  ni=table(x)
  fi=ni/length(x)
  fi2=fi^2
  j=length(table(x))
  
  gini=1-sum(fi2)
  gini.normalizzato=gini/((j-1)/j)
  
  return(gini.normalizzato)
}
gini_index(dati$sales_cl)
# La distribuzione delle frequenze mostra una presenza relativamente equilibrata tra le classi centrali,
# con una maggiore concentrazione nelle classi 80–180 e una progressiva diminuzione verso gli estremi.

# L’indice di Gini pari a 0,92 indica un’elevata eterogeneità tra le classi,
# ma tale valore deve essere interpretato tenendo conto della scelta degli intervalli di discretizzazione,
# che può amplificare la disuguaglianza osservata.

# Nel complesso, la distribuzione non appare fortemente concentrata in una sola classe,
# ma piuttosto distribuita su più intervalli centrali.

#STEP 5 CALCOLO DELLE PROBAILITà

#Qual è la probabilità che, presa una riga a caso di questo dataset, essa riporti la città “Beaumont”?
prob_beaumont=sum(dati$city=="Beaumont")/nrow(dati)
prob_beaumont

#E la probabilità che riporti il mese di Luglio?
prob_luglio=sum(dati$month==7)/nrow(dati)
prob_luglio

#E la probabilità che riporti il mese di dicembre 2012?
prob_dic_2012=sum(dati$month==12 & dati$year==2012)/nrow(dati)
prob_dic_2012
# Le probabilità sono state calcolate come frequenze relative sul totale delle osservazioni.
# La città di Beaumont presenta una probabilità pari a 0.25, indicando che rappresenta una quota significativa del dataset.
# Il mese di luglio ha probabilità 0.0833, coerente con una distribuzione uniforme dei mesi nell’anno.
# La probabilità congiunta di dicembre 2012 è bassa (0.0167), come atteso per un evento specifico che combina due condizioni.


# STEP 6. Creazione di nuove variabili
#Crea una nuova colonna che calcoli il prezzo medio degli immobili utilizzando le variabili disponibili.
dati$average_price=(dati$volume/dati$sales)*1000000

#È stata creata la variabile average_price, calcolata come rapporto tra volume (valore totale delle vendite) e 
#sales (numero di immobili venduti). Poiché la variabile volume è espressa in milioni di dollari, il risultato è stato moltiplicato 
#per 1.000.000 per ottenere il prezzo medio degli immobili espresso direttamente in dollari.

#Prova a creare una colonna che misuri l’efficacia degli annunci di vendita. Commenta e discuti i risultati.

dati$efficacia_annunci=dati$sales/dati$listings

#ANDIAMO AD ANALIZZARE I PRIMI 4 MESI E VEDIAMO COME L'FFICACIA AUMENTA
head(dati,4)
#possiamo notare come:
#Mese 1: efficacia bassa (5%), una piccola quota degli immobili in vendita viene effettivamente venduta.
#Mese 2: leggero aumento dell’efficacia (6,8%), segnale di un primo miglioramento nella capacità del mercato di assorbire l’offerta.
#Mese 3: efficacia quasi raddoppiata (10,8%), il mercato appare più dinamico e una quota maggiore di annunci si trasforma in vendite.
#Mese 4: ulteriore crescita dell’efficacia (11,7%), indicando una buona performance degli annunci e una maggiore attività del mercato immobiliare.

#PUNTO 7 Analisi condizionata

#ANALISI CONDIZIONATA PER CITTà
sales_city=dati %>%
  group_by(city) %>%
  summarise(mean_city_sales=mean(sales),
            dv.city_sales=sd(sales))
#grafico
ggplot(sales_city, aes(x=city,y=mean_city_sales,fill=city))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin = mean_city_sales - dv.city_sales,
                    ymax = mean_city_sales + dv.city_sales),
                width = 0.2)+
  labs(
    title = "Media vendite per città",
    x = "città",
    y = "Media",
    fill = "Città"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Il grafico mostra la media delle vendite mensili per città, con barre di errore 
#che rappresentano ±1 deviazione standard, indicando la variabilità dei dati. 
#Tyler si distingue come la città con la media più alta di vendite, circa 270 al mese, ma anche con la maggiore variabilità, segnalata da barre di errore più estese. Al contrario, Wichita Falls presenta la media più bassa, intorno a 120 vendite mensili, e una variabilità molto più contenuta, 
#suggerendo una performance più stabile nel tempo.

#ANALISI CONDIZIONATA PER ANNO
sales_year=dati %>%
  group_by(year) %>%
  summarise(mean_sales_year=mean(sales),
            dv.sales_year=sd(sales))
#grafico
ggplot(sales_year, aes(x=factor(year),y=mean_sales_year))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin = mean_sales_year - dv.sales_year,
                    ymax = mean_sales_year + dv.sales_year),
                width = 0.2)+
  labs(
    title = "Media vendite per anno",
    x = "anno",
    y = "Media",
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Il grafico mostra un trend crescente della media delle vendite annuali 
#in Texas tra il 2010 e il 2014, con valori medi che passano da circa 165 a 230 vendite. Questo suggerisce una ripresa progressiva del mercato immobiliare nel periodo considerato. Le barre di errore, corrispondenti a ±1 deviazione standard, indicano una certa variabilità nella distribuzione delle vendite annuali,
#che però non compromette la chiarezza del trend positivo osservato.

#ANALISI CONDIZIONATA PER MESE
sale_month=dati %>%
  group_by(month) %>%
  summarise(mean_sales_month=mean(sales),
            dv.sales_month=sd(sales))
#grafico
ggplot(sale_month,aes(x=factor(month),y=mean_sales_month))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin = mean_sales_month - dv.sales_month,
                 ymax = mean_sales_month + dv.sales_month),
             width = 0.2)+
  labs(
    title = "Media vendite per mese",
    x = "mese",
    y = "Media",
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Si osserva una chiara stagionalità nelle vendite mensili, 
#che crescono da gennaio (~130) fino a un picco in giugno (~250), 
#per poi diminuire gradualmente fino a novembre (~155), indicando una maggiore attività del mercato nei mesi primaverili ed estivi.


#PUNTO 8 Creazione di visualizzazioni con ggplot2

#boxplot per confrontare la distribuzione del prezzo mediano delle case tra le varie città
ggplot(dati, aes(x = city, y = median_price)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Distribuzione dei prezzi delle case per città",
    x = "Città",
    y = "Prezzo degli immobili"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Il boxplot mostra la distribuzione del prezzo mediano degli immobili per città.
# Bryan-College Station ha i prezzi più alti (~158.000$) e la minore variabilità,
# seguita da Tyler (~143.000$) e Beaumont (~130.000$).
# Wichita Falls presenta i prezzi più bassi (~100.000$) e la maggiore dispersione.
# Si notano outlier in Beaumont e Bryan-College Station, con valori superiori alla norma.

#boxplot per confrontare la distribuzione del valore totale delle vendite tra le varie città ma anche tra i vari anni.
ggplot(dati, aes(x = city, y = volume, fill = factor(year))) +
  geom_boxplot() +
  labs(
    title = "Distribuzione del valore totale delle vendite per città e anno",
    x = "Città",
    y = "Valore totale vendite (milioni USD)",
    fill = "Anno" 
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Il boxplot mostra la distribuzione del valore totale delle vendite (in milioni di USD) 
#per città e anno dal 2010 al 2014. In tutte le città si osserva un trend crescente nel valore delle vendite. Tyler e Bryan-College Station registrano i valori più alti e la più marcata crescita nel periodo, mentre Wichita Falls mantiene valori più bassi e volumi relativamente stabili. 
#Beaumont presenta una crescita moderata, con qualche outlier negli anni iniziali.

#Grafici a barre per confrontare il totale delle vendite per mese e città.
sum_sales_city_month=dati %>%
  group_by(month,city) %>%
  summarise(total_sales = sum(sales))

ggplot(sum_sales_city_month,aes(x=factor(month),y=total_sales,fill=city))+
  geom_bar(stat = "identity", position="dodge")+
  labs(
    title = "Vendite totali per mese e città",
    x = "mese",
    y = "somma delle vendite",
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Il grafico mostra la somma delle vendite per mese e città.
# In tutte le città si conferma la stagionalità estiva, con il picco tra maggio e luglio
# e i valori minimi nei mesi invernali. Tyler domina in quasi tutti i mesi,
# seguita da Bryan-College Station, mentre Wichita Falls rimane costantemente la più bassa.

#line charts per confrontare l’andamento delle vendite in periodi storici differenti

ggplot(dati, aes(x =month, y = sales, color = factor(year))) +
  geom_line(size = 1) +
  facet_wrap(~city)+ #creo grafici separati per vedere l'andamento nel tempo di ogni città
  scale_x_continuous(
    breaks = 1:12)+
  labs(
    title = "Andamento delle vendite nel tempo per città e anno",
    x = "Mese",
    y = "Numero di vendite",
    color = "Anno"
  ) +
  theme_minimal()
  # Il grafico mostra l'andamento mensile delle vendite per città e anno.
  # In tutte le città si conferma la stagionalità estiva con picco tra maggio e luglio.
  # Il 2014 (rosa) tende ad essere l'anno con i valori più alti, mentre il 2010-2011
  # mostrano i valori più bassi, confermando il trend crescente già osservato in precedenza.
  # Wichita Falls rimane la città con i volumi più contenuti e stabili nel tempo.


#STEP 9, COMMENTO FINALE
# CONCLUSIONI

# L’analisi del dataset ha evidenziato diverse caratteristiche rilevanti del mercato immobiliare in Texas.

# In primo luogo, emerge una forte eterogeneità tra le città considerate: alcune, come Tyler e Bryan-College Station,
# presentano livelli più elevati sia in termini di numero di vendite che di valore economico delle transazioni,
# mentre altre, come Wichita Falls, mostrano valori più contenuti e una maggiore stabilità nel tempo.

# Dal punto di vista temporale, si osserva un trend crescente delle vendite nel periodo analizzato (2010–2014),
# che suggerisce una fase di espansione del mercato immobiliare. Inoltre, è evidente una componente stagionale:
# le vendite tendono ad aumentare nei mesi primaverili ed estivi e a diminuire nei mesi invernali.

# L’analisi della variabilità e dell’asimmetria ha evidenziato come la variabile volume sia la più instabile e
# caratterizzata da valori estremi, indicando forti differenze nel valore economico delle vendite tra città e periodi.
# Al contrario, il prezzo mediano degli immobili risulta relativamente stabile, con una distribuzione più simmetrica.

# Le nuove variabili introdotte hanno fornito ulteriori insight: il prezzo medio degli immobili conferma i livelli osservati
# nella distribuzione dei prezzi, mentre l’efficacia degli annunci evidenzia che solo una parte delle inserzioni si traduce
# effettivamente in vendite, suggerendo possibili margini di miglioramento nelle strategie di marketing.

# Dal punto di vista operativo, i risultati suggeriscono che l’azienda dovrebbe:
# - focalizzarsi maggiormente sulle città con maggiore crescita e volume di vendite;
# - sfruttare la stagionalità del mercato per pianificare le strategie commerciali nei periodi più favorevoli;
# - migliorare l’efficacia degli annunci, ottimizzando la gestione delle inserzioni immobiliari.

# In conclusione, l’analisi conferma la presenza di differenze strutturali tra le città e dinamiche temporali rilevanti,
# fornendo una base informativa utile per supportare decisioni strategiche nel mercato immobiliare.




