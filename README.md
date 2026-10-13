# L'uso dell'intelligenza artificiale per lo studio della povertà: analisi del risparmio

Repository: <https://github.com/fdiprima/tesi-poverta-istat>

Codice R della tesi di laurea magistrale di **Federico Di Prima**
Corso di laurea magistrale in Amministrazione, finanza e controllo – Università degli Studi della Tuscia, a.a. 2025-2026
Relatrice: Prof.ssa Rossana Cotroneo – Insegnamento: Statistica multivariata e data mining

## Obiettivo

Prevedere la condizione di **povertà relativa** delle famiglie italiane a partire dalla
**composizione della spesa** (quote sulle 13 divisioni COICOP) e dalle caratteristiche
socio-demografiche e abitative, confrontando cinque modelli di classificazione:
regressione logistica, SVM lineare, rete neurale, random forest e XGBoost.

## Dati

I microdati **non sono inclusi** nel repository. Si scaricano gratuitamente dal sito Istat:

- *Indagine sulle spese delle famiglie – microdati ad uso pubblico*, anno 2024:
  <https://www.istat.it/microdati/indagine-sulle-spese-delle-famiglie-uso-pubblico/>

Dopo il download, copiare nella cartella `data/`:

| File | Contenuto |
|---|---|
| `HBS_Microdati_2024.txt` | microdati (28.054 famiglie, 1.627 variabili) |
| `HBS_crea_divisioni_spese_2024.xlsx_2_2024.xlsx` | file Istat che associa le variabili di spesa alle 13 divisioni COICOP |

## Come eseguire lo script

1. Aprire `tesi-poverta-istat.Rproj` in RStudio (la cartella di lavoro diventa quella del repository).
2. Copiare i dati in `data/`.
3. *Session → Restart R*, poi eseguire `script_tesi.R` dall'inizio (*Ctrl+Shift+S*).

I pacchetti mancanti vengono installati automaticamente. Tutti i passaggi casuali usano
`set.seed(123)`, quindi i risultati sono riproducibili. Tempo di esecuzione: 1-2 ore
(lo schema P_up è il più lento).

## Struttura dello script

| Parte | Contenuto |
|---|---|
| Blocco 0 | Avvio, cartelle, pacchetti, funzioni di servizio |
| A | Preparazione dei dati: valori mancanti, verifica del data leakage, quote di spesa COICOP, variabili socio-demografiche e abitative |
| B | Ridondanze (correlazioni, V di Cramér) e dataset definitivo (57 predittori) |
| C | Analisi esplorativa |
| E | Resampling (tre schemi a confronto), divisione 60/20/20, tuning sul validation set, tre soglie di classificazione, risultati su training, validation e test |
| F | Valutazione: curve ROC con intervalli di confidenza, precision-recall, test sull'accuratezza (No Information Rate), Kappa, rapporto di verosimiglianza, DeLong, McNemar, Brier score, importanza delle variabili |
| G | Sintesi finale |
| H | XGBoost sugli stessi split e confronto con gli altri modelli |

## Output

Tutto viene salvato nella cartella `output/`:

- `output_completo.txt`: testo completo della console;
- `figure/`: grafici in PNG;
- `tabelle/`: tabelle in CSV (separatore `;`, virgola decimale);
- file `.RData` con gli oggetti di R, tra cui `sessione_completa.RData` (l'intero ambiente a fine script).

## Software

R (versione 4.6) con i pacchetti data.table, readxl, dplyr, tidyr, caret, randomForest,
ranger, LiblineaR, nnet, pROC, corrplot, rcompanion, xgboost. Le versioni esatte sono in
`sessionInfo.txt`.
