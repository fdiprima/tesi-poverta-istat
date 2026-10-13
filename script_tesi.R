# ==============================================================================
#  TESI MAGISTRALE - L'uso dell'intelligenza artificiale per lo studio della
#                    povertà: analisi del risparmio
#  Federico Di Prima - Università degli Studi della Tuscia - a.a. 2025-26
#  Relatrice: Prof.ssa Rossana Cotroneo
#  Dati: Istat, Indagine sulle spese delle famiglie 2024 (microdati, 28.054 famiglie)
#  Script finale completo e riproducibile (set.seed(123) in ogni passaggio casuale)
#  Codice: https://github.com/fdiprima/tesi-poverta-istat
# ==============================================================================
#
#  STRUTTURA
#   BLOCCO 0  Avvio: memoria pulita, cartelle, file di output, pacchetti, funzioni
#   PARTE A   Preparazione dei dati
#             A1-A6   Primo approccio (1.627 -> 771 variabili) e scoperta del leakage
#             A7-A11  Secondo approccio: quote COICOP + variabili socio-demografiche
#                     e abitative (47 predittori)
#             A12     + numero di componenti e ripartizione (49)
#             A13     + titolo di godimento, tipo di abitazione, beni durevoli (60)
#   PARTE B   Ridondanze (correlazioni, V di Cramér) e dataset definitivo (57)
#   PARTE C   Analisi esplorativa del dataset definitivo
#   PARTE E   Resampling e modelli di classificazione, in tre schemi a confronto:
#             P_up   resampling (oversampling) sull'intero campione, poi split
#                    60/20/20
#             P_down resampling (undersampling) sull'intero campione, poi split
#             C      split 60/20/20, poi resampling solo sul training
#             Tuning sul validation, tre soglie, prestazioni su training,
#             validation e test, test "depurato" dalle famiglie duplicate,
#             modello di partenza senza resampling, confronto tra gli schemi
#   PARTE F   Valutazione completa di ogni schema: ROC con IC, precision-recall,
#             effetto della soglia, NIR e Kappa, rapporto di verosimiglianza,
#             DeLong, McNemar, Brier, importanza delle variabili, dipendenza
#             parziale, coefficienti della logistica
#   PARTE G   Sintesi finale
#   PARTE H   Boosting (XGBoost) sugli stessi split, confronto con i 4 modelli
#   (G2)      Informazioni sulla sessione
#
#  La numerazione salta la lettera D, che nelle versioni di lavoro conteneva
#  un'analisi di clustering poi esclusa dalla tesi.
#
#  PRIMA DI LANCIARE: copiare i dati in data/ (vedi README.md), menu Session -> Restart R,
#  poi eseguire dall'inizio.
#  OUTPUT (nella cartella output/):
#           output_completo.txt   (tutto il testo della console)
#           figure/               (tutti i grafici in PNG)
#           tabelle/              (tutte le tabelle in CSV, apribili con Excel)
#           *.RData               (oggetti salvati per riprendere il lavoro)
#           sessione_completa.RData (l'intero ambiente a fine script)
#  TEMPO STIMATO: 1h-2h con i tre schemi (P_up è il più lento: training di
#  circa 30.000 righe). Per accorciare: modificare schemi_da_eseguire (PARTE E).
# ==============================================================================


# ##############################################################################
# BLOCCO 0 - AVVIO
# ##############################################################################
rm(list = ls())
# Cartella di lavoro = cartella principale del repository (in RStudio: aprire il file
# tesi-poverta-istat.Rproj, oppure Session -> Set Working Directory -> To Source File Location).
# I microdati Istat vanno messi in data/ (vedi README.md); tutti i risultati finiscono in output/.
cartella_dati   <- "data"
cartella_output <- "output"
dir.create(cartella_output, showWarnings = FALSE)
out <- function(nome) file.path(cartella_output, nome)

# Tutto ciò che compare in console viene scritto anche in output/output_completo.txt
# (nome diverso dal precedente output_completo.txt, che così non viene sovrascritto).
# Se R va riavviato a metà: rilanciare sink con un nuovo nome
# (es. out("output_completo_2.txt")) per non sovrascrivere la prima parte.
sink(out("output_completo.txt"), split = TRUE)
options(width = 140)   # tabelle più larghe, senza andare a capo nel file di output
inizio_script <- Sys.time()

# --- Cartelle di output (si creano da sole) ---
cartella_figure  <- out("figure")
cartella_tabelle <- out("tabelle")
dir.create(cartella_figure,  showWarnings = FALSE)
dir.create(cartella_tabelle, showWarnings = FALSE)

# --- Pacchetti: installa quelli mancanti e li carica ---
pacchetti <- c("data.table", "readxl", "dplyr", "tidyr", "caret", "randomForest",
               "ranger", "LiblineaR", "nnet", "pROC", "corrplot", "rcompanion",
               "xgboost")
mancanti <- pacchetti[!pacchetti %in% rownames(installed.packages())]
if (length(mancanti) > 0) install.packages(mancanti)
invisible(lapply(pacchetti, library, character.only = TRUE))
# Nota: ranger e randomForest hanno entrambi una funzione importance();
# nello script si usa sempre randomForest::importance() per evitare ambiguità.

# --- Funzioni di servizio ---

# Titolo di sezione ben visibile nell'output
titolo <- function(testo) {
  cat("\n\n", strrep("=", 78), "\n ", testo, "\n", strrep("=", 78), "\n", sep = "")
}

# Controllo con messaggio OK / ATTENZIONE (non interrompe lo script)
controlla <- function(condizione, messaggio) {
  cat(if (isTRUE(condizione)) "  [OK]        " else "  [ATTENZIONE] ", messaggio, "\n", sep = "")
}

# Salva il grafico corrente in figure/<nome>.png (resta visibile anche in Plots)
salva_fig <- function(nome, larghezza = 1600, altezza = 1000) {
  file <- file.path(cartella_figure, paste0(nome, ".png"))
  esito <- tryCatch({
    dev.copy(png, filename = file, width = larghezza, height = altezza, res = 150)
    dev.off()
    TRUE
  }, error = function(e) { cat("  Figura NON salvata (", conditionMessage(e), ")\n"); FALSE })
  if (esito) cat("  Figura salvata:", file, "\n")
}

# Salva una tabella in tabelle/<nome>.csv (formato italiano: ; e virgola decimale)
salva_tab <- function(x, nome) {
  file <- file.path(cartella_tabelle, paste0(nome, ".csv"))
  write.csv2(as.data.frame(x), file, row.names = TRUE)
  cat("  Tabella salvata:", file, "\n")
}

# Tempo trascorso dall'inizio dello script
tempo <- function() {
  cat("  [tempo trascorso:", round(as.numeric(difftime(Sys.time(), inizio_script, units = "mins")), 1),
      "minuti]\n")
}

# Rimuove i livelli vuoti solo se la variabile è un factor
dl <- function(x) if (is.factor(x)) droplevels(x) else x

titolo("BLOCCO 0 - Avvio completato")
cat("Cartella di lavoro:", getwd(), "\n")
cat("Data e ora di avvio:", format(inizio_script), "\n")
controlla(file.exists(file.path(cartella_dati, "HBS_Microdati_2024.txt")), "file dei microdati presente")
controlla(file.exists(file.path(cartella_dati, "HBS_crea_divisioni_spese_2024.xlsx_2_2024.xlsx")), "file della mappa COICOP presente")


# ##############################################################################
# PARTE A - PREPARAZIONE DEI DATI
# ##############################################################################

titolo("A1 - Caricamento dei microdati Istat")
dati <- as.data.frame(fread(file.path(cartella_dati, "HBS_Microdati_2024.txt"), na.strings = "."))
dim(dati)
controlla(nrow(dati) == 28054 && ncol(dati) == 1627, "28.054 famiglie x 1.627 variabili")

titolo("A2 - Esplorazione dei valori mancanti")
quota_na <- colMeans(is.na(dati))
summary(quota_na)
table(cut(quota_na, breaks = c(-0.01, 0.05, 0.30, 0.70, 0.95, 1),
          labels = c("0-5%", "5-30%", "30-70%", "70-95%", "95-100%")))

hist(quota_na, breaks = 50, col = "grey70", border = "white",
     main = "Distribuzione della quota di valori mancanti per variabile",
     xlab = "Quota di NA", ylab = "Numero di variabili")
abline(v = 0.30, col = "firebrick", lwd = 2, lty = 2)
legend("top", legend = "soglia 30%", col = "firebrick", lty = 2, lwd = 2, bty = "n")
salva_fig("01_distribuzione_NA")

titolo("A3 - Filtro sulle variabili con più del 30% di NA")
# Soglia scelta sulla distribuzione bimodale della quota di NA (grafico 01)
soglia_na <- 0.30
dati_filtrati <- dati[, quota_na <= soglia_na]
dim(dati_filtrati)

titolo("A4 - Variabili numeriche: costanti e correlazioni elevate")
vars_numeriche <- dati_filtrati[, sapply(dati_filtrati, is.numeric)]
sd_vars <- sapply(vars_numeriche, function(x) sd(x, na.rm = TRUE))
vars_costanti <- names(sd_vars[is.na(sd_vars) | sd_vars == 0])
cat("Variabili a varianza nulla:", length(vars_costanti), "\n")
vars_numeriche2 <- vars_numeriche[, !(colnames(vars_numeriche) %in% vars_costanti)]

mat_cor <- cor(vars_numeriche2, use = "pairwise.complete.obs")
mat_cor[is.na(mat_cor)] <- 0
alta_corr <- findCorrelation(mat_cor, cutoff = 0.85, names = TRUE)
cat("Variabili eliminate per correlazione > 0.85:", length(alta_corr), "\n")
dati_selezionati <- vars_numeriche2[, !(colnames(vars_numeriche2) %in% alta_corr)]
dim(dati_selezionati)

titolo("A5 - Variabile risposta: povertà relativa")
nome_target <- colnames(dati)[1627]
controlla(nome_target == "poveri", paste("la colonna 1627 è", nome_target))
target_factor <- as.factor(dati[[nome_target]])
table(target_factor, useNA = "always")
round(100 * prop.table(table(target_factor)), 2)

predittori <- dati_selezionati[, !(colnames(dati_selezionati) %in% nome_target)]
predittori_imputati <- na.roughfix(predittori)

titolo("A6 - Random forest esplorativa sulle 771 variabili: verifica del leakage")
set.seed(123)
rf_esplorativa <- ranger(x = predittori_imputati, y = target_factor,
                         num.trees = 200, importance = "impurity")
imp_771 <- sort(rf_esplorativa$variable.importance, decreasing = TRUE)
round(head(imp_771, 20), 1)
cat("Errore OOB:", round(rf_esplorativa$prediction.error, 4), "\n")

par(mar = c(4, 9, 3, 1))
barplot(rev(head(imp_771, 20)), horiz = TRUE, las = 1, col = "grey60", border = NA,
        cex.names = 0.75, xlab = "Importanza (impurity)",
        main = "Random forest sulle 771 variabili: prime 20 per importanza")
par(mar = c(5, 4, 4, 2) + 0.1)
salva_fig("02_leakage_rf771", 1600, 1100)

# povassc (povertà assoluta, col. 1548) è costruita sulla stessa spesa del target
table(povassc = dati$povassc, poveri = target_factor)
# CONCLUSIONE: il percorso delle 771 variabili contiene leakage (povassc e gli
# importi di spesa ricostruiscono la soglia di povertà). Viene abbandonato e il
# dataset viene ricostruito da zero (A7-A11) con variabili prive di leakage.

titolo("A7 - Mappa COICOP: divisione di spesa -> variabili elementari")
mappa <- read_excel(file.path(cartella_dati, "HBS_crea_divisioni_spese_2024.xlsx_2_2024.xlsx"),
                    sheet = "2024", col_names = FALSE)
colnames(mappa) <- c("divisione", "gruppo", "variabile")
mappa <- mappa %>% fill(divisione, .direction = "down") %>% filter(!is.na(variabile))
mappa$variabile <- trimws(mappa$variabile)

# I nomi nella mappa differiscono per maiuscole/minuscole: abbinamento case-insensitive
lookup <- setNames(colnames(dati), tolower(colnames(dati)))
mappa$variabile_reale <- lookup[tolower(mappa$variabile)]
cat("Variabili della mappa non trovate nel dataset:\n")
print(mappa[is.na(mappa$variabile_reale), ])   # attese: 2 variabili sanitarie
mappa_valida <- mappa %>% filter(!is.na(variabile_reale))
table(mappa_valida$divisione)

titolo("A8 - Spesa per divisione e quote sul totale")
divisioni <- unique(mappa_valida$divisione)
spesa_divisioni <- as.data.frame(sapply(divisioni, function(d) {
  vars <- mappa_valida$variabile_reale[mappa_valida$divisione == d]
  rowSums(dati[, vars, drop = FALSE], na.rm = TRUE)
}))
colnames(spesa_divisioni) <- divisioni
spesa_totale <- rowSums(spesa_divisioni, na.rm = TRUE)
summary(spesa_totale)
controlla(all(spesa_totale > 0), "spesa totale positiva per tutte le famiglie")

quote_divisioni <- spesa_divisioni / spesa_totale
colnames(quote_divisioni) <- paste0("quota_", divisioni)
summary(quote_divisioni)
controlla(all(abs(rowSums(quote_divisioni) - 1) < 1e-8), "le 13 quote sommano a 1 per ogni famiglia")

# Verifica: le quote da sole non contengono leakage
set.seed(123)
rf_check <- ranger(poveri ~ ., data = data.frame(quote_divisioni, poveri = target_factor),
                   num.trees = 200, importance = "impurity")
round(sort(rf_check$variable.importance, decreasing = TRUE), 1)
cat("Errore OOB con le sole quote:", round(rf_check$prediction.error, 4),
    "(atteso circa 0.09-0.10, nessuna variabile dominante = nessun leakage)\n")

titolo("A9 - Variabili socio-demografiche (col. 3-56) e abitative (col. 219-249)")
demo_cols <- colnames(dati)[c(3:56, 219:249)]
quota_na_demo <- colMeans(is.na(dati[, demo_cols]))
demo_validi <- demo_cols[quota_na_demo <= soglia_na]
cat("Variabili candidate:", length(demo_cols), "- con NA <= 30%:", length(demo_validi), "\n")

# c_Superf (mq) e c_Stanze restano numeriche, le altre diventano factor
demo_validi_cat <- setdiff(demo_validi, c("c_Superf", "c_Stanze"))
vars_demo <- as.data.frame(lapply(dati[, demo_validi_cat], as.factor))
vars_demo$c_Superf <- dati[["c_Superf"]]
vars_demo$c_Stanze <- dati[["c_Stanze"]]

titolo("A10 - Rimozione delle variabili costanti")
n_modalita <- sapply(vars_demo, function(x) length(unique(na.omit(x))))
vars_costanti_demo <- names(n_modalita[n_modalita <= 1])
vars_costanti_demo   # attese: c_relaz_1, Cucina, Bagno, Acquacor
vars_demo <- vars_demo[, !(colnames(vars_demo) %in% vars_costanti_demo)]

titolo("A11 - Dataset base: 13 quote + variabili socio-demografiche e abitative")
dataset_finale_47 <- cbind(quote_divisioni, vars_demo, poveri = target_factor)
dim(dataset_finale_47)
controlla(ncol(dataset_finale_47) == 48, "47 predittori + target")

titolo("A12 - Aggiunta di numero di componenti e ripartizione geografica")
table(rip = dati$rip, useNA = "always")
table(componenti = dati$c_Ncmp_fatto, useNA = "always")
cat("Quota di NA in c_Ncmp_altro:", round(mean(is.na(dati$c_Ncmp_altro)), 3), "-> esclusa\n")
dataset_finale_49 <- cbind(
  dataset_finale_47[, setdiff(names(dataset_finale_47), "poveri")],
  rip          = as.factor(dati$rip),
  c_Ncmp_fatto = dati$c_Ncmp_fatto,
  poveri       = target_factor
)
dim(dataset_finale_49)

titolo("A13 - Titolo di godimento, tipo di abitazione, beni durevoli")
candidate <- c("Titoccup", "Propabit", "tipabitaz_new", "c_n_abit_sec_topcod",
               "Possauto", "Possmoto", "Possbici", "Posstelcell", "Posstv",
               "Posspc", "Possantparabol", "Possdvd", "Possstereo")
round(colMeans(is.na(dati[, candidate])), 3)
vars_aggiunte <- candidate[colMeans(is.na(dati[, candidate])) <= soglia_na]
cat("Aggiunte (NA <= 30%):", vars_aggiunte, "\n")
cat("Escluse  (NA >  30%):", setdiff(candidate, vars_aggiunte), "\n")
cat("max_percettore esclusa: valore 1 per",
    round(100 * mean(dati$max_percettore == 1, na.rm = TRUE), 1), "% delle famiglie\n")

dataset_finale_60 <- cbind(
  dataset_finale_49[, setdiff(names(dataset_finale_49), "poveri")],
  as.data.frame(lapply(dati[, vars_aggiunte], as.factor)),
  poveri = target_factor
)
dim(dataset_finale_60)


# ##############################################################################
# PARTE B - RIDONDANZE E DATASET DEFINITIVO
# ##############################################################################
titolo("B1 - Correlazioni tra le variabili numeriche (dataset a 60 predittori)")
# Imputazione semplice sull'intero campione: solo per analisi descrittive.
# Per i modelli l'imputazione si fa dopo lo split (PARTE E).
dati_imp_60 <- na.roughfix(dataset_finale_60)

quote_vars <- grep("^quota_d_", names(dati_imp_60), value = TRUE)
mat_corr_quote <- cor(dati_imp_60[, quote_vars])
round(mat_corr_quote, 2)
salva_tab(round(mat_corr_quote, 3), "B1_correlazioni_quote")
corrplot(mat_corr_quote, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.6, tl.cex = 0.8, tl.col = "black",
         title = "Correlazione tra le 13 quote di spesa", mar = c(0, 0, 2, 0))
salva_fig("03_correlazione_quote", 1600, 1600)

idx_q <- which(abs(mat_corr_quote) > 0.5 & upper.tri(mat_corr_quote), arr.ind = TRUE)
cat("Coppie di quote con |r| > 0.5:", nrow(idx_q), "\n")
round(cor(dati_imp_60[, c("c_Superf", "c_Stanze", "c_Ncmp_fatto")]), 2)

titolo("B2 - V di Cramér tra le variabili categoriche")
cat_vars <- setdiff(names(dati_imp_60)[sapply(dati_imp_60, is.factor)], "poveri")
n_cat <- length(cat_vars)
cat("Variabili categoriche:", n_cat, "\n")
mat_cramer <- matrix(NA, n_cat, n_cat, dimnames = list(cat_vars, cat_vars))
for (i in 1:n_cat) {
  for (j in i:n_cat) {
    if (i == j) {
      mat_cramer[i, j] <- 1
    } else {
      v <- tryCatch(cramerV(table(dl(dati_imp_60[[cat_vars[i]]]), dl(dati_imp_60[[cat_vars[j]]]))),
                    error = function(e) NA)
      mat_cramer[i, j] <- v
      mat_cramer[j, i] <- v
    }
  }
}
salva_tab(round(mat_cramer, 3), "B2_matrice_v_cramer")
corrplot(mat_cramer, method = "color", type = "upper", tl.cex = 0.6, tl.col = "black",
         is.corr = FALSE, col.lim = c(0, 1),
         title = "V di Cramér tra le variabili categoriche", mar = c(0, 0, 2, 0))
salva_fig("04_v_cramer", 2000, 2000)

idx_c <- which(mat_cramer > 0.5 & upper.tri(mat_cramer), arr.ind = TRUE)
coppie <- data.frame(var1 = rownames(mat_cramer)[idx_c[, 1]],
                     var2 = colnames(mat_cramer)[idx_c[, 2]],
                     V    = round(mat_cramer[idx_c], 2))
coppie <- coppie[order(-coppie$V), ]
coppie
salva_tab(coppie, "B2_coppie_V_maggiore_05")

titolo("B3 - Dataset definitivo: rimozione delle variabili ridondanti (V > 0.85)")
# Stessa soglia usata per le numeriche (A4). Delle coppie si tiene la più informativa:
#   c_c_redd_1    (V = 0.91 con c_cond_1)  -> si tiene c_cond_1
#   pnasc_padre_1 (V = 0.89 con pnasc_1)   -> si tengono pnasc_1 e cittad_1
#   pnasc_madre_1 (V = 0.88 con pnasc_1)
vars_ridondanti <- c("c_c_redd_1", "pnasc_padre_1", "pnasc_madre_1")
print(coppie[coppie$V > 0.85, ])
dataset_finale <- dataset_finale_60[, setdiff(names(dataset_finale_60), vars_ridondanti)]
dim(dataset_finale)
controlla(ncol(dataset_finale) == 58, "57 predittori + target")
cat("Predittori numerici:  ", sum(sapply(dataset_finale, is.numeric)), "\n")
cat("Predittori categorici:", sum(sapply(dataset_finale, is.factor)) - 1, "\n")

# Riepilogo del percorso delle variabili (tabella per la tesi)
riepilogo_variabili <- data.frame(
  passaggio = c("Microdati originali", "Filtro NA <= 30%", "Numeriche a varianza non nulla",
                "Filtro di correlazione (0.85) - poi abbandonato per leakage",
                "Dataset base (13 quote + socio-demografiche/abitative)",
                "+ componenti e ripartizione", "+ godimento, tipo abitazione, beni durevoli",
                "Definitivo (- 3 ridondanti, V > 0.85)"),
  variabili = c(ncol(dati), ncol(dati_filtrati), ncol(vars_numeriche2), ncol(dati_selezionati),
                ncol(dataset_finale_47) - 1, ncol(dataset_finale_49) - 1,
                ncol(dataset_finale_60) - 1, ncol(dataset_finale) - 1)
)
riepilogo_variabili
salva_tab(riepilogo_variabili, "B3_percorso_variabili")

save(dataset_finale, dataset_finale_60, dataset_finale_49, dataset_finale_47,
     file = out("dataset_finali.RData"))
tempo()


# ##############################################################################
# PARTE C - ANALISI ESPLORATIVA DEL DATASET DEFINITIVO
# ##############################################################################
# Dati non imputati (NA esclusi caso per caso). Statistiche sul campione, senza
# pesi campionari Istat. I codici delle modalità vanno letti sul tracciato record.
eda <- dataset_finale
eda$poveri_lab <- factor(eda$poveri, levels = c("0", "1"), labels = c("Non povere", "Povere"))
tasso_medio <- 100 * mean(eda$poveri == "1")

titolo("C1 - Variabile risposta")
tab_target <- table(eda$poveri_lab)
tab_target
round(100 * prop.table(tab_target), 1)
bp <- barplot(100 * prop.table(tab_target), col = c("grey75", "firebrick"), border = NA,
              ylim = c(0, 100), ylab = "% di famiglie",
              main = "Famiglie in povertà relativa (campione Istat 2024)")
text(bp, 100 * prop.table(tab_target), paste0(round(100 * prop.table(tab_target), 1), "%"), pos = 3)
salva_fig("05_eda_target", 1200, 1000)

titolo("C2 - Statistiche descrittive delle 13 quote di spesa")
descrittive <- function(x) c(
  media = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), minimo = min(x, na.rm = TRUE),
  Q1 = unname(quantile(x, 0.25, na.rm = TRUE)), mediana = median(x, na.rm = TRUE),
  Q3 = unname(quantile(x, 0.75, na.rm = TRUE)), massimo = max(x, na.rm = TRUE),
  NA_perc = 100 * mean(is.na(x)))
descr_quote <- t(sapply(eda[, quote_vars], descrittive))
round(descr_quote, 3)
salva_tab(round(descr_quote, 4), "C2_descrittive_quote")

titolo("C3 - Composizione della spesa: famiglie povere e non povere")
quote_medie <- sapply(eda[, quote_vars], function(x) tapply(x, eda$poveri_lab, mean, na.rm = TRUE))
round(100 * quote_medie, 1)
salva_tab(round(100 * t(quote_medie), 2), "C3_quote_medie_per_poverta")

par(mar = c(8, 4, 4, 1))
barplot(100 * quote_medie, beside = TRUE, col = c("grey75", "firebrick"), border = NA,
        las = 2, cex.names = 0.75, ylab = "Quota media sulla spesa totale (%)",
        main = "Composizione della spesa per condizione di povertà",
        legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))
par(mar = c(5, 4, 4, 2) + 0.1)
salva_fig("06_eda_quote_per_poverta", 1800, 1000)

quote_principali <- c("quota_d_01", "quota_d_04_str", "quota_d_07", "quota_d_11")
par(mfrow = c(1, 4))
for (v in quote_principali) {
  boxplot(eda[[v]] ~ eda$poveri_lab, col = c("grey75", "firebrick"), outline = FALSE,
          main = v, xlab = "", ylab = "Quota sulla spesa totale")
}
par(mfrow = c(1, 1))
salva_fig("07_eda_boxplot_quote", 2000, 900)

# Test di Wilcoxon-Mann-Whitney: le quote differiscono tra povere e non povere?
test_quote <- t(sapply(quote_vars, function(v) {
  w <- wilcox.test(eda[[v]] ~ eda$poveri, exact = FALSE)
  c(mediana_non_povere = median(eda[[v]][eda$poveri == "0"], na.rm = TRUE),
    mediana_povere     = median(eda[[v]][eda$poveri == "1"], na.rm = TRUE),
    p_value            = w$p.value)
}))
round(test_quote, 4)
salva_tab(signif(test_quote, 4), "C3_wilcoxon_quote")

titolo("C4 - Superficie, stanze e numero di componenti")
for (v in c("c_Superf", "c_Stanze", "c_Ncmp_fatto")) {
  cat("\n---", v, "---\n")
  print(round(t(sapply(split(eda[[v]], eda$poveri_lab), descrittive)), 2))
  cat("Wilcoxon p-value:", signif(wilcox.test(eda[[v]] ~ eda$poveri, exact = FALSE)$p.value, 3), "\n")
}

titolo("C5 - Tasso di povertà per le principali caratteristiche")
vars_chiave <- c("rip", "c_Ncmp_fatto", "c_c_etacalc_1", "c_titstu_1", "c_cond_1",
                 "staciv_1", "Titoccup", "cittad_1", "Possauto", "Posspc")
tassi_poverta <- lapply(vars_chiave, function(v) {
  tab <- table(dl(eda[[v]]), eda$poveri)
  data.frame(variabile = v, modalita = rownames(tab),
             famiglie  = as.vector(rowSums(tab)),
             perc_famiglie = round(100 * as.vector(rowSums(tab)) / sum(tab), 1),
             tasso_pov = round(100 * tab[, "1"] / rowSums(tab), 1))
})
names(tassi_poverta) <- vars_chiave
tassi_poverta
salva_tab(do.call(rbind, tassi_poverta), "C5_tasso_poverta_per_caratteristica")

par(mfrow = c(2, 5), mar = c(4, 4, 3, 1))
for (v in vars_chiave) {
  tp <- tassi_poverta[[v]]
  barplot(tp$tasso_pov, names.arg = tp$modalita, col = "steelblue", border = NA,
          ylim = c(0, max(tp$tasso_pov) * 1.15), main = v, ylab = "% povere", cex.names = 0.8)
  abline(h = tasso_medio, lty = 2, col = "grey40")
}
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
salva_fig("08_eda_tasso_poverta", 2200, 1100)

titolo("C6 - Associazione di ogni variabile categorica con la povertà")
cat_eda <- setdiff(names(eda)[sapply(eda, is.factor)], c("poveri", "poveri_lab"))
assoc_target <- do.call(rbind, lapply(cat_eda, function(v) {
  tab <- table(dl(eda[[v]]), eda$poveri)
  ct  <- suppressWarnings(chisq.test(tab))
  data.frame(variabile = v, modalita = nrow(tab),
             chi2 = round(unname(ct$statistic), 1), gdl = unname(ct$parameter),
             p_value = signif(ct$p.value, 3), V_Cramer = round(cramerV(tab), 3))
}))
assoc_target <- assoc_target[order(-assoc_target$V_Cramer), ]
rownames(assoc_target) <- NULL
assoc_target
salva_tab(assoc_target, "C6_associazione_con_poverta")

save(descr_quote, quote_medie, test_quote, tassi_poverta, assoc_target,
     file = out("analisi_esplorativa.RData"))
tempo()


# ##############################################################################
# PARTE E - RESAMPLING E MODELLI DI CLASSIFICAZIONE
# ##############################################################################
# Il campione è sbilanciato (10% di famiglie povere): senza correzione i modelli
# classificano quasi tutti come "non poveri". Si applica il resampling secondo
# tre schemi, con gli stessi modelli, le stesse griglie di tuning e gli stessi
# criteri di soglia, così da poterli confrontare:
#
#  P_up   - il campione viene AUMENTATO con il
#           resampling (oversampling casuale con reimmissione delle famiglie povere
#           fino al rapporto 50/50) e POI diviso in training (60%), validation
#           (20%) e test (20%).
#  P_down - stesso ordine (resampling sull'intero campione, poi divisione) ma con
#           undersampling delle famiglie non povere: nessuna famiglia duplicata.
#  C      - divisione 60/20/20 sul campione originale e resampling (undersampling)
#           solo sul training: validation e test conservano la prevalenza reale.
#
# Per ogni schema si riportano le prestazioni su training, validation e test.
# Per P_up si misura inoltre quante famiglie del test sono copie di famiglie del
# training (effetto dell'oversampling) e si valuta il modello sul test "depurato"
# dalle copie, per verificare se l'accuratezza è sovrastimata.
#
# Modelli: random forest (ranger), SVM lineare (LiblineaR), regressione
# logistica (glm), rete neurale (nnet). Tuning sul validation (AUC); soglie di
# classificazione scelte sul validation; valutazione finale sul test.

schemi_da_eseguire <- c("P_up", "P_down", "C")   # togliere uno schema per risparmiare tempo
n_core <- max(1, parallel::detectCores() - 1)    # core usati dalla random forest

titolo("E0 - Impostazioni")
cat("Schemi di resampling da eseguire:", schemi_da_eseguire, "\n")
cat("Core utilizzati dalla random forest:", n_core, "\n")


# ==============================================================================
# E0 - Funzioni
# ==============================================================================

# Imputazione con mediana (numeriche) o moda (categoriche) del set di riferimento
imputa <- function(df, rif) {
  for (v in setdiff(names(df), c("poveri", ".id"))) {
    if (anyNA(df[[v]])) {
      if (is.numeric(rif[[v]])) {
        df[[v]][is.na(df[[v]])] <- median(rif[[v]], na.rm = TRUE)
      } else {
        df[[v]][is.na(df[[v]])] <- names(which.max(table(rif[[v]])))
      }
    }
  }
  df
}

auc_val <- function(prob, vero) {
  as.numeric(auc(roc(vero, prob, levels = c("0", "1"), direction = "<", quiet = TRUE)))
}

# Soglia scelta sul validation: Youden o massima accuratezza
soglia_ottimale <- function(prob, vero, criterio = "youden") {
  r <- roc(vero, prob, levels = c("0", "1"), direction = "<", quiet = TRUE)
  if (criterio == "youden") {
    return(coords(r, "best", best.method = "youden", ret = "threshold")$threshold[1])
  }
  cc <- coords(r, "all", ret = c("threshold", "accuracy"))
  cc <- cc[is.finite(cc$threshold), ]
  cc$threshold[which.max(cc$accuracy)]
}

# Soglia di compromesso: sensitivity >= 70% con specificity massima
soglia_sens <- function(prob, vero, sens_min = 0.70) {
  r  <- roc(vero, prob, levels = c("0", "1"), direction = "<", quiet = TRUE)
  cc <- coords(r, "all", ret = c("threshold", "sensitivity", "specificity"))
  cc <- cc[is.finite(cc$threshold) & cc$sensitivity >= sens_min, ]
  cc$threshold[which.max(cc$specificity)]
}

# Metriche per una data soglia
valuta <- function(prob, vero, soglia) {
  pred <- factor(ifelse(prob >= soglia, "1", "0"), levels = c("0", "1"))
  cm <- confusionMatrix(pred, vero, positive = "1")
  c(Accuracy     = unname(cm$overall["Accuracy"]),
    Sensitivity  = unname(cm$byClass["Sensitivity"]),
    Specificity  = unname(cm$byClass["Specificity"]),
    Precision    = unname(cm$byClass["Pos Pred Value"]),
    F1           = unname(cm$byClass["F1"]),
    Balanced_acc = unname(cm$byClass["Balanced Accuracy"]),
    Kappa        = unname(cm$overall["Kappa"]),
    AUC          = auc_val(prob, vero))
}

# Matrice di dummy (stesse colonne per training, validation e test)
crea_matrice <- function(df) {
  m <- model.matrix(poveri ~ ., data = df)[, -1]
  colnames(m) <- make.names(colnames(m), unique = TRUE)
  m
}

# Random forest (ranger, foresta di probabilità) e sue previsioni
rf_fit <- function(d, mtry, nodo, ntree, importanza = "none") {
  ranger(poveri ~ ., data = d, num.trees = ntree, mtry = mtry, min.node.size = nodo,
         probability = TRUE, importance = importanza, seed = 123, num.threads = n_core)
}
p_rf <- function(m, d) predict(m, data = d, num.threads = n_core)$predictions[, "1"]

# Punteggio SVM orientato verso la classe "1"
punteggio_svm <- function(m, x) {
  pr  <- predict(m, x, decisionValues = TRUE)$decisionValues
  cl1 <- as.character(m$ClassNames[1])
  dv  <- pr[, cl1]
  if (cl1 == "1") dv else -dv
}

# Divisione del campione e resampling secondo lo schema scelto
prepara_schema <- function(ds, schema) {
  ds$.id <- seq_len(nrow(ds))                       # identificativo della famiglia originale
  dividi <- function(d) {                           # split 60/20/20 stratificato su "poveri"
    set.seed(123)
    i_tr  <- createDataPartition(d$poveri, p = 0.6, list = FALSE)
    resto <- d[-i_tr, ]
    set.seed(123)
    i_va  <- createDataPartition(resto$poveri, p = 0.5, list = FALSE)
    list(train = d[i_tr, ], valid = resto[i_va, ], test = resto[-i_va, ])
  }
  if (schema %in% c("P_up", "P_down")) {
    # 1) imputazione sull'intero campione  2) resampling sull'intero campione  3) split
    ds_imp <- imputa(ds, ds)
    x <- ds_imp[, setdiff(names(ds_imp), "poveri")]
    set.seed(123)
    ds_res <- if (schema == "P_up") {
      upSample(x = x, y = ds_imp$poveri, yname = "poveri")
    } else {
      downSample(x = x, y = ds_imp$poveri, yname = "poveri")
    }
    sp <- dividi(ds_res)
    sp$train_sbilanciato <- NULL
    sp$n_dopo_resampling <- nrow(ds_res)
  } else {
    # 1) split sul campione originale  2) imputazione con il training  3) resampling sul training
    sp0 <- dividi(ds)
    train_imp <- imputa(sp0$train, sp0$train)
    valid     <- imputa(sp0$valid, sp0$train)
    test      <- imputa(sp0$test,  sp0$train)
    set.seed(123)
    train <- downSample(x = train_imp[, setdiff(names(train_imp), "poveri")],
                        y = train_imp$poveri, yname = "poveri")
    sp <- list(train = train, valid = valid, test = test, train_sbilanciato = train_imp,
               n_dopo_resampling = nrow(ds))
  }
  sp$schema <- schema
  sp
}

# Composizione dei tre insiemi e famiglie ripetute tra training e test
diagnostica_split <- function(sp) {
  f <- function(d) c(famiglie = nrow(d), povere = sum(d$poveri == "1"),
                     perc_povere = round(100 * mean(d$poveri == "1"), 1),
                     famiglie_distinte = length(unique(d$.id)))
  tab <- rbind(training = f(sp$train), validation = f(sp$valid), test = f(sp$test))
  id_tr <- unique(sp$train$.id)
  list(tab = tab,
       perc_valid_in_train = round(100 * mean(sp$valid$.id %in% id_tr), 1),
       perc_test_in_train  = round(100 * mean(sp$test$.id  %in% id_tr), 1),
       perc_test_povere_in_train = round(100 * mean(sp$test$.id[sp$test$poveri == "1"] %in% id_tr), 1))
}

# Tuning, stima e valutazione dei 4 modelli su uno schema
esegui_modelli <- function(sp) {
  togli_id <- function(d) d[, setdiff(names(d), ".id")]
  tr <- togli_id(sp$train); va <- togli_id(sp$valid); te <- togli_id(sp$test)

  # Matrici per SVM, logistica e rete neurale (standardizzate con il training)
  x_tr <- crea_matrice(tr); x_va <- crea_matrice(va); x_te <- crea_matrice(te)
  stopifnot(identical(colnames(x_tr), colnames(x_va)), identical(colnames(x_tr), colnames(x_te)))
  medie <- colMeans(x_tr); sds <- apply(x_tr, 2, sd); sds[is.na(sds) | sds == 0] <- 1
  scala <- function(m) sweep(sweep(m, 2, medie, "-"), 2, sds, "/")
  x_tr_s <- scala(x_tr); x_va_s <- scala(x_va); x_te_s <- scala(x_te)
  y_tr <- tr$poveri; y_va <- va$poveri; y_te <- te$poveri

  # Random forest: tuning di mtry e min.node.size (300 alberi in griglia, 500 nel finale)
  griglia_rf <- expand.grid(mtry = c(3, 5, 7, 10, 15), nodesize = c(1, 5, 10))
  griglia_rf$auc_valid <- NA
  for (i in seq_len(nrow(griglia_rf))) {
    m <- rf_fit(tr, griglia_rf$mtry[i], griglia_rf$nodesize[i], 300)
    griglia_rf$auc_valid[i] <- auc_val(p_rf(m, va), y_va)
    cat("  [", sp$schema, "] random forest", i, "di", nrow(griglia_rf), "\n")
  }
  best_rf <- griglia_rf[which.max(griglia_rf$auc_valid), ]
  rf_finale <- rf_fit(tr, best_rf$mtry, best_rf$nodesize, 500, importanza = "permutation")

  # SVM lineare (LiblineaR, L2): tuning del costo
  griglia_svm <- data.frame(cost = c(0.001, 0.01, 0.1, 1, 10), auc_valid = NA)
  for (i in seq_len(nrow(griglia_svm))) {
    m <- LiblineaR(data = x_tr_s, target = y_tr, type = 2, cost = griglia_svm$cost[i])
    griglia_svm$auc_valid[i] <- auc_val(punteggio_svm(m, x_va_s), y_va)
  }
  best_svm   <- griglia_svm[which.max(griglia_svm$auc_valid), ]
  svm_finale <- LiblineaR(data = x_tr_s, target = y_tr, type = 2, cost = best_svm$cost)
  cat("  [", sp$schema, "] SVM fatta\n")

  # Regressione logistica (nessun iperparametro: si sceglie la soglia)
  logit_finale <- glm(poveri ~ ., data = data.frame(x_tr, poveri = y_tr), family = binomial)
  cat("  [", sp$schema, "] logistica fatta\n")

  # Rete neurale (un hidden layer): tuning di size e decay
  y_tr_num <- as.numeric(y_tr == "1")
  griglia_nn <- expand.grid(size = c(3, 5, 10), decay = c(0.001, 0.01, 0.1))
  griglia_nn$auc_valid <- NA
  for (i in seq_len(nrow(griglia_nn))) {
    set.seed(123)
    m <- nnet(x = x_tr_s, y = y_tr_num, size = griglia_nn$size[i], decay = griglia_nn$decay[i],
              maxit = 500, entropy = TRUE, MaxNWts = 20000, trace = FALSE)
    griglia_nn$auc_valid[i] <- auc_val(as.numeric(predict(m, x_va_s)), y_va)
    cat("  [", sp$schema, "] rete neurale", i, "di", nrow(griglia_nn), "\n")
  }
  best_nn <- griglia_nn[which.max(griglia_nn$auc_valid), ]
  set.seed(123)
  nn_finale <- nnet(x = x_tr_s, y = y_tr_num, size = best_nn$size, decay = best_nn$decay,
                    maxit = 500, entropy = TRUE, MaxNWts = 20000, trace = FALSE)

  # Probabilità/punteggi su training, validation e test
  prevedi <- function(dati, x_s, x) list(
    RandomForest = p_rf(rf_finale, dati),
    SVM          = punteggio_svm(svm_finale, x_s),
    Logistica    = as.numeric(suppressWarnings(predict(logit_finale, newdata = data.frame(x), type = "response"))),
    ReteNeurale  = as.numeric(predict(nn_finale, x_s)))
  p_tr <- prevedi(tr, x_tr_s, x_tr); p_va <- prevedi(va, x_va_s, x_va); p_te <- prevedi(te, x_te_s, x_te)
  modelli <- lapply(names(p_te), function(n) list(tr = p_tr[[n]], va = p_va[[n]], te = p_te[[n]]))
  names(modelli) <- names(p_te)

  # Soglie scelte sul validation
  soglie <- data.frame(
    Youden      = sapply(modelli, function(m) soglia_ottimale(m$va, y_va, "youden")),
    Accuratezza = sapply(modelli, function(m) soglia_ottimale(m$va, y_va, "accuracy")),
    Compromesso = sapply(modelli, function(m) soglia_sens(m$va, y_va, 0.70)))

  # Risultati sul test con le tre soglie
  ris_test <- function(criterio) t(sapply(names(modelli), function(n)
    valuta(modelli[[n]]$te, y_te, soglie[n, criterio])))
  ris_compromesso <- ris_test("Compromesso")
  ris_youden      <- ris_test("Youden")
  ris_accuracy    <- ris_test("Accuratezza")

  # Prestazioni su training, validation e test (soglia di compromesso)
  tre_insiemi <- do.call(rbind, lapply(names(modelli), function(n) {
    s <- soglie[n, "Compromesso"]
    r <- rbind(training   = valuta(modelli[[n]]$tr, y_tr, s),
               validation = valuta(modelli[[n]]$va, y_va, s),
               test       = valuta(modelli[[n]]$te, y_te, s))
    data.frame(modello = n, insieme = rownames(r), round(r, 4), row.names = NULL)
  }))

  # Test "depurato": solo famiglie del test che non compaiono nel training (e senza copie)
  sovrapposizione <- any(sp$test$.id %in% sp$train$.id)
  nuove <- !(sp$test$.id %in% sp$train$.id) & !duplicated(sp$test$.id)
  ris_depurato <- NULL
  if (sovrapposizione && sum(nuove) > 0 && sum(y_te[nuove] == "1") >= 10 && sum(y_te[nuove] == "0") >= 10) {
    ris_depurato <- t(sapply(names(modelli), function(n)
      valuta(modelli[[n]]$te[nuove], y_te[nuove], soglie[n, "Compromesso"])))
  }

  list(schema = sp$schema, rf = rf_finale, svm = svm_finale, logit = logit_finale, nn = nn_finale,
       griglia_rf = griglia_rf, griglia_svm = griglia_svm, griglia_nn = griglia_nn,
       best_rf = best_rf, best_svm = best_svm, best_nn = best_nn,
       modelli = modelli, soglie = soglie, y_tr = y_tr, y_va = y_va, y_te = y_te,
       train = tr, x_tr = x_tr,
       ris_compromesso = ris_compromesso, ris_youden = ris_youden, ris_accuracy = ris_accuracy,
       tre_insiemi = tre_insiemi,
       test_depurato = list(sovrapposizione = sovrapposizione, famiglie = sum(nuove), povere = sum(y_te[nuove] == "1"), risultati = ris_depurato))
}

# Dipendenza parziale per la random forest (ranger)
pdp_rf <- function(m, dati, var, n_righe = 2000, n_punti = 20) {
  set.seed(123)
  base <- dati[sample(nrow(dati), min(n_righe, nrow(dati))), ]
  x <- dati[[var]]
  griglia <- if (is.numeric(x)) {
    unique(as.numeric(quantile(x, probs = seq(0.02, 0.98, length.out = n_punti), na.rm = TRUE)))
  } else {
    levels(droplevels(x))
  }
  yhat <- sapply(griglia, function(g) {
    b <- base
    b[[var]] <- if (is.factor(x)) factor(rep(g, nrow(b)), levels = levels(x)) else g
    mean(p_rf(m, b))
  })
  list(x = griglia, y = yhat, numerica = is.numeric(x))
}

# Valutazione completa di uno schema: grafici, tabelle e test statistici
valutazione_completa <- function(ris, nome) {
  pref  <- paste0("S_", nome, "_")
  y_va  <- ris$y_va; y_te <- ris$y_te
  col_mod <- c(RandomForest = "forestgreen", SVM = "firebrick", Logistica = "steelblue",
               ReteNeurale = "darkorange")
  col_g <- c("steelblue", "darkorange", "firebrick")

  titolo(paste0("[", nome, "] Tuning (AUC sul validation)"))
  cat("\nRandom forest:\n"); print(ris$griglia_rf[order(-ris$griglia_rf$auc_valid), ])
  cat("\nSVM:\n");           print(ris$griglia_svm)
  cat("\nRete neurale:\n");  print(ris$griglia_nn[order(-ris$griglia_nn$auc_valid), ])
  cat("\nParametri scelti:\n"); print(ris$best_rf); print(ris$best_svm); print(ris$best_nn)
  salva_tab(ris$griglia_rf, paste0(pref, "tuning_random_forest"))
  salva_tab(ris$griglia_svm, paste0(pref, "tuning_svm"))
  salva_tab(ris$griglia_nn, paste0(pref, "tuning_rete_neurale"))

  plot(NA, xlim = range(ris$griglia_rf$mtry), ylim = range(ris$griglia_rf$auc_valid),
       xlab = "mtry", ylab = "AUC sul validation", main = paste0("Tuning della random forest (", nome, ")"))
  for (j in seq_along(unique(ris$griglia_rf$nodesize))) {
    ns <- unique(ris$griglia_rf$nodesize)[j]
    g  <- ris$griglia_rf[ris$griglia_rf$nodesize == ns, ]
    lines(g$mtry, g$auc_valid, type = "b", pch = 19, col = col_g[j])
  }
  legend("bottomright", legend = paste("min.node.size =", unique(ris$griglia_rf$nodesize)),
         col = col_g, pch = 19, lty = 1, bty = "n")
  salva_fig(paste0(pref, "01_tuning_random_forest"), 1500, 1000)

  plot(log10(ris$griglia_svm$cost), ris$griglia_svm$auc_valid, type = "b", pch = 19, col = "steelblue",
       xlab = "log10(costo C)", ylab = "AUC sul validation", main = paste0("Tuning della SVM lineare (", nome, ")"))
  salva_fig(paste0(pref, "02_tuning_svm"), 1500, 1000)

  plot(NA, xlim = range(ris$griglia_nn$size), ylim = range(ris$griglia_nn$auc_valid),
       xlab = "Neuroni nello strato nascosto (size)", ylab = "AUC sul validation",
       main = paste0("Tuning della rete neurale (", nome, ")"))
  for (j in seq_along(unique(ris$griglia_nn$decay))) {
    dc <- unique(ris$griglia_nn$decay)[j]
    g  <- ris$griglia_nn[ris$griglia_nn$decay == dc, ]
    lines(g$size, g$auc_valid, type = "b", pch = 19, col = col_g[j])
  }
  legend("bottomleft", legend = paste("decay =", unique(ris$griglia_nn$decay)),
         col = col_g, pch = 19, lty = 1, bty = "n")
  salva_fig(paste0(pref, "03_tuning_rete_neurale"), 1500, 1000)

  titolo(paste0("[", nome, "] Risultati sul test set con le tre soglie"))
  cat("\nSoglie scelte sul validation:\n"); print(round(ris$soglie, 4))
  cat("\n--- Soglia di COMPROMESSO (sensitivity >= 70%) ---\n"); print(round(ris$ris_compromesso, 3))
  cat("\n--- Soglia di YOUDEN ---\n");                              print(round(ris$ris_youden, 3))
  cat("\n--- Soglia di MASSIMA ACCURATEZZA ---\n");                 print(round(ris$ris_accuracy, 3))
  salva_tab(round(ris$soglie, 4), paste0(pref, "soglie"))
  salva_tab(round(ris$ris_compromesso, 4), paste0(pref, "risultati_compromesso"))
  salva_tab(round(ris$ris_youden, 4), paste0(pref, "risultati_youden"))
  salva_tab(round(ris$ris_accuracy, 4), paste0(pref, "risultati_max_accuratezza"))

  titolo(paste0("[", nome, "] Prestazioni su training, validation e test (soglia di compromesso)"))
  print(ris$tre_insiemi)
  salva_tab(ris$tre_insiemi, paste0(pref, "training_validation_test"))
  cat("Nota: per la random forest le prestazioni sul training sono 'in-sample' e quindi ottimistiche.\n")

  if (!ris$test_depurato$sovrapposizione) {
    cat("\n[", nome, "] Nessuna famiglia del test compare nel training: il test è già 'pulito'.\n")
  } else if (!is.null(ris$test_depurato$risultati)) {
    titolo(paste0("[", nome, "] Test depurato dalle famiglie presenti anche nel training"))
    cat("Famiglie nel test depurato:", ris$test_depurato$famiglie, "- di cui povere:", ris$test_depurato$povere, "\n")
    print(round(ris$test_depurato$risultati, 3))
    salva_tab(round(ris$test_depurato$risultati, 4), paste0(pref, "test_depurato"))
  } else {
    cat("\n[", nome, "] Test depurato: non calcolabile (meno di 10 famiglie povere non presenti nel training)\n")
    cat("Famiglie nel test depurato:", ris$test_depurato$famiglie, "- di cui povere:", ris$test_depurato$povere, "\n")
  }

  titolo(paste0("[", nome, "] Curve ROC e intervalli di confidenza dell'AUC (test)"))
  roc_list <- lapply(ris$modelli, function(m) roc(y_te, m$te, levels = c("0", "1"), direction = "<", quiet = TRUE))
  ic_auc <- t(sapply(roc_list, function(r) as.numeric(ci.auc(r, method = "delong"))))
  colnames(ic_auc) <- c("IC95_inf", "AUC", "IC95_sup")
  print(round(ic_auc, 3))
  salva_tab(round(ic_auc, 4), paste0(pref, "AUC_intervalli_confidenza"))
  plot(roc_list$RandomForest, col = col_mod["RandomForest"], lwd = 2, legacy.axes = TRUE, asp = NA,
       main = paste0("Curve ROC (test set) - schema ", nome),
       xlab = "1 - Specificity (falsi positivi)", ylab = "Sensitivity (poveri individuati)")
  for (n in c("SVM", "Logistica", "ReteNeurale")) lines(roc_list[[n]], col = col_mod[n], lwd = 2)
  legend("bottomright", bty = "n", lwd = 2, col = col_mod,
         legend = paste0(names(col_mod), " (AUC = ", sprintf("%.3f", ic_auc[names(col_mod), "AUC"]), ")"))
  salva_fig(paste0(pref, "04_curve_ROC"))

  titolo(paste0("[", nome, "] Curve precision-recall (test)"))
  plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "Recall (poveri individuati)",
       ylab = "Precision (segnalati davvero poveri)", main = paste0("Curve precision-recall - schema ", nome))
  for (n in names(roc_list)) {
    pr <- coords(roc_list[[n]], "all", ret = c("recall", "precision"))
    pr <- pr[is.finite(pr$precision) & is.finite(pr$recall), ]
    pr <- pr[order(pr$recall), ]
    lines(pr$recall, pr$precision, col = col_mod[n], lwd = 2)
  }
  abline(h = mean(y_te == "1"), lty = 2, col = "grey50")
  legend("bottomleft", legend = c(names(col_mod), "prevalenza nel test"),
         col = c(col_mod, "grey50"), lwd = c(2, 2, 2, 2, 1), lty = c(1, 1, 1, 1, 2), bty = "n")
  salva_fig(paste0(pref, "05_curve_precision_recall"))

  titolo(paste0("[", nome, "] Effetto della soglia sulla random forest (validation)"))
  roc_rf_va <- roc(y_va, ris$modelli$RandomForest$va, levels = c("0", "1"), direction = "<", quiet = TRUE)
  cc_rf <- coords(roc_rf_va, "all", ret = c("threshold", "accuracy", "sensitivity", "specificity"))
  cc_rf <- cc_rf[is.finite(cc_rf$threshold), ]
  plot(cc_rf$threshold, cc_rf$accuracy, type = "l", lwd = 2, col = "black", ylim = c(0, 1),
       xlab = "Soglia di classificazione", ylab = "Valore",
       main = paste0("Random forest: effetto della soglia (validation) - schema ", nome))
  lines(cc_rf$threshold, cc_rf$sensitivity, lwd = 2, col = "firebrick")
  lines(cc_rf$threshold, cc_rf$specificity, lwd = 2, col = "steelblue")
  abline(v = as.numeric(unlist(ris$soglie["RandomForest", ])), lty = 2, col = c("grey40", "grey60", "darkgreen"))
  legend("bottomleft", bty = "n", lwd = c(2, 2, 2, 1, 1, 1), lty = c(1, 1, 1, 2, 2, 2),
         col = c("black", "firebrick", "steelblue", "grey40", "grey60", "darkgreen"),
         legend = c("Accuracy", "Sensitivity (poveri trovati)", "Specificity",
                    "soglia di Youden", "soglia di max accuratezza", "soglia di compromesso"))
  salva_fig(paste0(pref, "06_soglia_tradeoff_random_forest"))

  metriche_graf <- t(ris$ris_compromesso[, c("Accuracy", "Sensitivity", "Specificity", "Balanced_acc", "AUC")])
  barplot(metriche_graf, beside = TRUE, ylim = c(0, 1.1), border = NA,
          col = gray.colors(nrow(metriche_graf), start = 0.2, end = 0.85), ylab = "Valore",
          main = paste0("Metriche sul test (soglia di compromesso) - schema ", nome),
          legend.text = TRUE, args.legend = list(x = "top", ncol = 5, bty = "n", cex = 0.8))
  abline(h = 0.8, lty = 2, col = "firebrick")
  salva_fig(paste0(pref, "07_confronto_metriche"), 1800, 1000)

  titolo(paste0("[", nome, "] Matrici di confusione (test, soglia di compromesso)"))
  pred_comp <- lapply(names(ris$modelli), function(n)
    factor(ifelse(ris$modelli[[n]]$te >= ris$soglie[n, "Compromesso"], "1", "0"), levels = c("0", "1")))
  names(pred_comp) <- names(ris$modelli)
  for (n in names(pred_comp)) {
    cat("\n---", n, "---\n")
    print(confusionMatrix(pred_comp[[n]], y_te, positive = "1")$table)
  }

  titolo(paste0("[", nome, "] Significatività: accuratezza contro No Information Rate e Kappa"))
  p_nir <- t(sapply(names(pred_comp), function(n) {
    cm <- confusionMatrix(pred_comp[[n]], y_te, positive = "1")
    c(Accuracy = unname(cm$overall["Accuracy"]), NIR = unname(cm$overall["AccuracyNull"]),
      p_value = unname(cm$overall["AccuracyPValue"]), Kappa = unname(cm$overall["Kappa"]))
  }))
  print(round(p_nir, 4))
  salva_tab(round(p_nir, 5), paste0(pref, "test_NIR_kappa"))

  titolo(paste0("[", nome, "] Significatività: regressione logistica, rapporto di verosimiglianza"))
  logit_nullo <- glm(poveri ~ 1, data = data.frame(ris$x_tr, poveri = ris$y_tr), family = binomial)
  print(anova(logit_nullo, ris$logit, test = "Chisq"))
  cat("Pseudo-R2 di McFadden:", round(1 - ris$logit$deviance / ris$logit$null.deviance, 3), "\n")
  cat("Coefficienti non stimabili (modalità rare):", sum(is.na(coef(ris$logit))), "\n")

  titolo(paste0("[", nome, "] Significatività: confronto tra le AUC (test di DeLong)"))
  confronti <- list(c("RandomForest", "SVM"), c("RandomForest", "Logistica"),
                    c("RandomForest", "ReteNeurale"), c("Logistica", "SVM"),
                    c("SVM", "ReteNeurale"), c("Logistica", "ReteNeurale"))
  delong <- do.call(rbind, lapply(confronti, function(cp) {
    rt <- roc.test(roc_list[[cp[1]]], roc_list[[cp[2]]], method = "delong")
    data.frame(confronto = paste(cp, collapse = " vs "),
               AUC_1 = round(as.numeric(rt$estimate[1]), 4), AUC_2 = round(as.numeric(rt$estimate[2]), 4),
               Z = round(as.numeric(rt$statistic), 3), p_value = signif(rt$p.value, 3))
  }))
  print(delong)
  salva_tab(delong, paste0(pref, "test_DeLong"))

  titolo(paste0("[", nome, "] Significatività: test di McNemar (test, soglia di compromesso)"))
  corretto <- lapply(pred_comp, function(p) factor(p == y_te, levels = c(FALSE, TRUE)))
  mcnemar <- do.call(rbind, lapply(confronti, function(cp) {
    mt <- mcnemar.test(table(corretto[[cp[1]]], corretto[[cp[2]]]))
    data.frame(confronto = paste(cp, collapse = " vs "),
               chi2 = round(as.numeric(mt$statistic), 2), p_value = signif(mt$p.value, 3))
  }))
  print(mcnemar)
  salva_tab(mcnemar, paste0(pref, "test_McNemar"))

  titolo(paste0("[", nome, "] Brier score (modelli che restituiscono probabilità)"))
  brier <- sapply(c("RandomForest", "Logistica", "ReteNeurale"), function(n)
    mean((ris$modelli[[n]]$te - as.numeric(y_te == "1"))^2))
  print(round(brier, 4))
  cat("Riferimento (previsione costante pari alla prevalenza):",
      round(mean((mean(y_te == "1") - as.numeric(y_te == "1"))^2), 4), "\n")

  titolo(paste0("[", nome, "] Importanza delle variabili (random forest, permutazione)"))
  imp <- sort(ris$rf$variable.importance, decreasing = TRUE)
  print(round(head(imp, 20), 5))
  salva_tab(data.frame(importanza = round(imp, 6)), paste0(pref, "importanza_variabili"))
  par(mar = c(4, 9, 3, 1))
  barplot(rev(head(imp, 15)), horiz = TRUE, las = 1, col = "forestgreen", border = NA, cex.names = 0.8,
          xlab = "Riduzione di accuratezza per permutazione",
          main = paste0("Importanza delle variabili - Random Forest (", nome, ")"))
  par(mar = c(5, 4, 4, 2) + 0.1)
  salva_fig(paste0(pref, "08_importanza_variabili"), 1800, 1000)

  titolo(paste0("[", nome, "] Dipendenza parziale delle variabili più importanti"))
  var_pdp <- names(imp)[1:4]
  par(mfrow = c(2, 2))
  for (v in var_pdp) {
    pd <- tryCatch(pdp_rf(ris$rf, ris$train, v), error = function(e) NULL)
    if (is.null(pd)) { plot.new(); title(paste(v, "- non calcolabile")); next }
    if (pd$numerica) {
      plot(pd$x, pd$y, type = "l", lwd = 2, col = "forestgreen", xlab = v,
           ylab = "Probabilità media di povertà", main = v)
    } else {
      barplot(pd$y, names.arg = pd$x, col = "forestgreen", border = NA, xlab = v,
              ylab = "Probabilità media di povertà", main = v)
    }
  }
  par(mfrow = c(1, 1))
  salva_fig(paste0(pref, "09_dipendenza_parziale"), 1800, 1400)

  titolo(paste0("[", nome, "] Regressione logistica: coefficienti significativi e odds ratio"))
  coef_logit <- as.data.frame(summary(ris$logit)$coefficients)
  colnames(coef_logit) <- c("stima", "errore_std", "z", "p_value")
  coef_logit$odds_ratio <- exp(coef_logit$stima)
  coef_sig <- coef_logit[coef_logit$p_value < 0.05 & rownames(coef_logit) != "(Intercept)", ]
  coef_sig <- coef_sig[order(-abs(coef_sig$z)), ]
  cat("Coefficienti significativi al 5%:", nrow(coef_sig), "su", nrow(coef_logit) - 1, "\n")
  print(round(head(coef_sig, 25), 4))
  salva_tab(signif(coef_logit, 4), paste0(pref, "coefficienti_logistica"))

  invisible(list(ic_auc = ic_auc, p_nir = p_nir, delong = delong, mcnemar = mcnemar,
                 brier = brier, importanza = imp, coef_logit = coef_logit))
}


# ==============================================================================
# E1 - Divisione del campione e resampling nei tre schemi
# ==============================================================================
titolo("E1 - Divisione del campione e resampling")
split_schemi <- lapply(schemi_da_eseguire, function(s) prepara_schema(dataset_finale, s))
names(split_schemi) <- schemi_da_eseguire

diagnostiche <- lapply(split_schemi, diagnostica_split)
for (s in schemi_da_eseguire) {
  cat("\n========== Schema", s, "==========\n")
  cat("Famiglie dopo il resampling:", split_schemi[[s]]$n_dopo_resampling, "\n")
  print(diagnostiche[[s]]$tab)
  cat("Famiglie del validation presenti anche nel training:", diagnostiche[[s]]$perc_valid_in_train, "%\n")
  cat("Famiglie del test presenti anche nel training:      ", diagnostiche[[s]]$perc_test_in_train, "%\n")
  cat("Famiglie POVERE del test presenti anche nel training:", diagnostiche[[s]]$perc_test_povere_in_train, "%\n")
  salva_tab(diagnostiche[[s]]$tab, paste0("E1_composizione_", s))
}
controlla(all(sapply(diagnostiche, function(d) all(d$tab[, "famiglie"] > 0))), "tutti gli insiemi non vuoti")
tempo()


# ==============================================================================
# E2 - Stima, tuning e valutazione dei modelli in ogni schema
# ==============================================================================
risultati <- list()
for (s in schemi_da_eseguire) {
  titolo(paste("E2 - Addestramento dei modelli - schema", s))
  risultati[[s]] <- esegui_modelli(split_schemi[[s]])
  tempo()
}
save(risultati, split_schemi, diagnostiche, file = out("risultati_modelli_schemi.RData"))


# ==============================================================================
# E3 - Modello di partenza senza resampling (random forest, soglia 0.5)
# ==============================================================================
titolo("E3 - Random forest SENZA resampling (motiva la necessità del bilanciamento)")
sp_c <- if ("C" %in% schemi_da_eseguire) split_schemi$C else prepara_schema(dataset_finale, "C")
tr_sbil <- sp_c$train_sbilanciato[, setdiff(names(sp_c$train_sbilanciato), ".id")]
te_c    <- sp_c$test[, setdiff(names(sp_c$test), ".id")]
rf_sbilanciata <- rf_fit(tr_sbil, floor(sqrt(ncol(tr_sbil) - 1)), 10, 300)
ris_sbilanciata <- valuta(p_rf(rf_sbilanciata, te_c), te_c$poveri, 0.5)
cat("Random forest addestrata sul training originale (10% di povere), soglia 0.5:\n")
print(round(ris_sbilanciata, 3))
cat("-> accuratezza alta ma poche famiglie povere individuate: serve il resampling.\n")
salva_tab(round(t(ris_sbilanciata), 4), "E3_random_forest_senza_resampling")


# ==============================================================================
# E4 - Confronto tra gli schemi di resampling
# ==============================================================================
titolo("E4 - Confronto tra gli schemi di resampling (test set, soglia di compromesso)")
confronto_schemi <- do.call(rbind, lapply(schemi_da_eseguire, function(s) {
  r <- risultati[[s]]$ris_compromesso
  data.frame(schema = s, modello = rownames(r),
             famiglie_test = nrow(split_schemi[[s]]$test),
             perc_povere_test = diagnostiche[[s]]$tab["test", "perc_povere"],
             perc_test_in_training = diagnostiche[[s]]$perc_test_in_train,
             round(r, 4), row.names = NULL)
}))
print(confronto_schemi)
salva_tab(confronto_schemi, "E4_confronto_schemi")

# Random forest: confronto grafico tra gli schemi
rf_schemi <- sapply(schemi_da_eseguire, function(s)
  risultati[[s]]$ris_compromesso["RandomForest", c("Accuracy", "Sensitivity", "Specificity", "Balanced_acc", "AUC")])
barplot(rf_schemi, beside = TRUE, ylim = c(0, 1.15), border = NA,
        col = gray.colors(nrow(rf_schemi), start = 0.2, end = 0.85), ylab = "Valore",
        main = "Random forest: metriche sul test nei diversi schemi di resampling",
        legend.text = TRUE, args.legend = list(x = "top", ncol = 5, bty = "n", cex = 0.8))
abline(h = 0.8, lty = 2, col = "firebrick")
salva_fig("E4_confronto_schemi_random_forest", 1800, 1000)

# Schema P_up: test completo contro test depurato dalle famiglie duplicate
if ("P_up" %in% schemi_da_eseguire) {
  titolo("E5 - Schema P_up: effetto delle famiglie duplicate tra training e test")
  cat("Famiglie del test che sono copie di famiglie del training:", diagnostiche$P_up$perc_test_in_train, "%\n")
  cat("Famiglie POVERE del test copie di famiglie del training: ", diagnostiche$P_up$perc_test_povere_in_train, "%\n\n")
  cat("Test completo (soglia di compromesso):\n")
  print(round(risultati$P_up$ris_compromesso, 3))
  if (!is.null(risultati$P_up$test_depurato$risultati)) {
    cat("\nTest depurato (solo famiglie mai viste nel training):\n")
    print(round(risultati$P_up$test_depurato$risultati, 3))
    conf_dep <- rbind("Test completo"  = risultati$P_up$ris_compromesso["RandomForest", ],
                      "Test depurato"  = risultati$P_up$test_depurato$risultati["RandomForest", ])
    print(round(conf_dep, 3))
    salva_tab(round(conf_dep, 4), "E5_P_up_test_completo_vs_depurato")
  } else {
    cat("\nTest depurato non calcolabile: quasi tutte le famiglie povere del test compaiono anche nel training.\n")
    cat("Famiglie nel test depurato:", risultati$P_up$test_depurato$famiglie,
        "- di cui povere:", risultati$P_up$test_depurato$povere, "\n")
  }
}

# Confronto 47 / 49 / 60 / 57 predittori (schema C, facoltativo)
titolo("E6 - Confronto 47 / 49 / 60 / 57 predittori (schema C, facoltativo)")
# Risultati già ottenuti con randomForest (AUC sul test, schema C):
#                47     49     60     57
# RandomForest  0.896  0.940  0.944  0.943
# SVM           0.877  0.923  0.929  0.929
# Logistica     0.877  0.923  0.929  0.929
# ReteNeurale   0.840  0.901  0.895  0.891
esegui_confronto <- FALSE   # TRUE per rifarlo: circa 40 minuti in più
if (esegui_confronto) {
  auc_versione <- function(ds) esegui_modelli(prepara_schema(ds, "C"))$ris_youden[, "AUC"]
  confronto_auc <- data.frame(AUC_47 = round(auc_versione(dataset_finale_47), 3),
                              AUC_49 = round(auc_versione(dataset_finale_49), 3),
                              AUC_60 = round(auc_versione(dataset_finale_60), 3),
                              AUC_57 = round(risultati$C$ris_youden[, "AUC"], 3))
  print(confronto_auc)
  salva_tab(confronto_auc, "E6_confronto_versioni")
} else {
  cat("Confronto non rieseguito (esegui_confronto = FALSE); valori nel commento sopra.\n")
}
tempo()


# ##############################################################################
# PARTE F - VALUTAZIONE COMPLETA DI OGNI SCHEMA
# ##############################################################################
# Per ogni schema: tuning, tre soglie, training/validation/test, ROC con IC,
# precision-recall, effetto della soglia, matrici di confusione, NIR e Kappa,
# rapporto di verosimiglianza, DeLong, McNemar, Brier, importanza delle
# variabili, dipendenza parziale, coefficienti della logistica.
valutazioni <- list()
for (s in schemi_da_eseguire) {
  titolo(paste("PARTE F - VALUTAZIONE COMPLETA - SCHEMA", s))
  valutazioni[[s]] <- valutazione_completa(risultati[[s]], s)
  tempo()
}
save(valutazioni, file = out("risultati_valutazione_schemi.RData"))


# ##############################################################################
# PARTE G - SINTESI FINALE
# ##############################################################################
titolo("G1 - Sintesi dei risultati principali")
cat("Dataset definitivo:", nrow(dataset_finale), "famiglie,", ncol(dataset_finale) - 1, "predittori\n")
cat("Prevalenza della povertà relativa:", round(tasso_medio, 1), "%\n\n")

sintesi <- do.call(rbind, lapply(schemi_da_eseguire, function(s) {
  r <- risultati[[s]]; ic <- valutazioni[[s]]$ic_auc
  data.frame(schema = s, modello = rownames(r$ris_compromesso),
             AUC = round(ic[rownames(r$ris_compromesso), "AUC"], 3),
             IC95 = paste0("[", round(ic[rownames(r$ris_compromesso), "IC95_inf"], 3), "; ",
                           round(ic[rownames(r$ris_compromesso), "IC95_sup"], 3), "]"),
             Accuracy = round(r$ris_compromesso[, "Accuracy"], 3),
             Sensitivity = round(r$ris_compromesso[, "Sensitivity"], 3),
             Specificity = round(r$ris_compromesso[, "Specificity"], 3),
             Balanced_acc = round(r$ris_compromesso[, "Balanced_acc"], 3),
             Kappa = round(r$ris_compromesso[, "Kappa"], 3),
             sopra_80 = ifelse(r$ris_compromesso[, "Accuracy"] >= 0.80, "si", "no"),
             row.names = NULL)
}))
print(sintesi)
salva_tab(sintesi, "G1_sintesi_finale")

for (s in schemi_da_eseguire) {
  cat("\nSchema", s, "- parametri della random forest: mtry =", risultati[[s]]$best_rf$mtry,
      "- min.node.size =", risultati[[s]]$best_rf$nodesize, "- 500 alberi\n")
}
cat("\nLettura dei risultati:\n")
cat("  P_up   : resampling prima della divisione; confrontare test completo e test depurato (E5)\n")
cat("  P_down : stesso ordine senza duplicati; test bilanciato 50/50\n")
cat("  C      : test con la prevalenza reale (10%); stima delle prestazioni su dati nuovi\n")

cat("\nFile prodotti:\n")
cat("  figure: ", length(list.files(cartella_figure)), "file in", cartella_figure, "\n")
cat("  tabelle:", length(list.files(cartella_tabelle)), "file in", cartella_tabelle, "\n")


# ##############################################################################
# PARTE H - BOOSTING (XGBoost) SUGLI STESSI SCHEMI DI RESAMPLING
# ##############################################################################
# Boosting: XGBoost stimato sugli STESSI training / validation / test della
# Parte E per ciascuno schema, con tuning sul validation, tre soglie e confronto
# statistico (DeLong, McNemar) con i quattro modelli già stimati.

togli_id <- function(d) d[, setdiff(names(d), ".id")]
# XGBoost: stima e previsione
xgb_fit <- function(x, y, profondita, eta, n_alberi) {
  set.seed(123)
  parametri <- list(objective = "binary:logistic", eval_metric = "auc",
                    max_depth = profondita, eta = eta,
                    subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 1,
                    nthread = n_core)   # il seme è quello di R (set.seed sopra)
  xgb.train(params = parametri, data = xgb.DMatrix(x, label = as.numeric(y == "1")),
            nrounds = n_alberi, verbose = 0)
}
p_xgb <- function(m, x) as.numeric(predict(m, xgb.DMatrix(x)))

schemi <- schemi_da_eseguire
titolo("H1 - XGBoost: stessi split della Parte E")
cat("Versione di xgboost:", as.character(packageVersion("xgboost")), "\n")
for (s in schemi) {
  controlla(identical(risultati[[s]]$y_te, split_schemi[[s]]$test$poveri),
            paste("schema", s, "- test identico a quello della Parte E"))
}

# ##############################################################################
# H2-H4 - TUNING, SOGLIE E RISULTATI PER OGNI SCHEMA
# ##############################################################################
griglia_base <- expand.grid(max_depth = c(3, 6, 9), eta = c(0.05, 0.1), nrounds = c(200, 500))

risultati_xgb <- list()
for (s in schemi) {
  titolo(paste("H2 - Tuning di XGBoost - schema", s))
  sp <- split_schemi[[s]]
  tr <- togli_id(sp$train); va <- togli_id(sp$valid); te <- togli_id(sp$test)
  x_tr <- crea_matrice(tr); x_va <- crea_matrice(va); x_te <- crea_matrice(te)
  stopifnot(identical(colnames(x_tr), colnames(x_va)), identical(colnames(x_tr), colnames(x_te)))
  y_tr <- tr$poveri; y_va <- va$poveri; y_te <- te$poveri

  griglia <- griglia_base
  griglia$auc_valid <- NA
  for (i in seq_len(nrow(griglia))) {
    m <- xgb_fit(x_tr, y_tr, griglia$max_depth[i], griglia$eta[i], griglia$nrounds[i])
    griglia$auc_valid[i] <- auc_val(p_xgb(m, x_va), y_va)
    cat("  [", s, "] XGBoost", i, "di", nrow(griglia),
        "- depth", griglia$max_depth[i], "eta", griglia$eta[i], "alberi", griglia$nrounds[i],
        "- AUC valid", round(griglia$auc_valid[i], 4), "\n")
  }
  best <- griglia[which.max(griglia$auc_valid), ]
  cat("\nGriglia ordinata per AUC sul validation:\n")
  print(griglia[order(-griglia$auc_valid), ], row.names = FALSE)
  cat("\nParametri scelti:\n"); print(best, row.names = FALSE)
  salva_tab(griglia, paste0("H_", s, "_tuning_xgboost"))

  xgb_finale <- xgb_fit(x_tr, y_tr, best$max_depth, best$eta, best$nrounds)
  p_tr <- p_xgb(xgb_finale, x_tr); p_va <- p_xgb(xgb_finale, x_va); p_te <- p_xgb(xgb_finale, x_te)

  # Grafico del tuning
  col_g <- c("steelblue", "darkorange", "firebrick")
  par(mfrow = c(1, 2))
  for (nr in unique(griglia$nrounds)) {
    g0 <- griglia[griglia$nrounds == nr, ]
    plot(NA, xlim = range(g0$max_depth), ylim = range(griglia$auc_valid),
         xlab = "Profondità degli alberi (max_depth)", ylab = "AUC sul validation",
         main = paste0("XGBoost (", s, ") - ", nr, " alberi"))
    for (j in seq_along(unique(g0$eta))) {
      g <- g0[g0$eta == unique(g0$eta)[j], ]
      lines(g$max_depth, g$auc_valid, type = "b", pch = 19, col = col_g[j])
    }
    legend("bottomright", legend = paste("eta =", unique(g0$eta)), col = col_g, pch = 19, lty = 1, bty = "n")
  }
  par(mfrow = c(1, 1))
  salva_fig(paste0("H_", s, "_01_tuning_xgboost"), 1800, 900)

  # H3 - Soglie sul validation e risultati sul test
  titolo(paste("H3 - Soglie e risultati sul test - schema", s))
  soglie <- c(Compromesso = soglia_sens(p_va, y_va, 0.70),
              Youden      = soglia_ottimale(p_va, y_va, "youden"),
              Accuratezza = soglia_ottimale(p_va, y_va, "accuracy"))
  cat("Soglie scelte sul validation:\n"); print(round(soglie, 4))
  ris_test <- t(sapply(names(soglie), function(k) valuta(p_te, y_te, soglie[k])))
  rownames(ris_test) <- paste("XGBoost - soglia", names(soglie))
  cat("\nRisultati sul test:\n"); print(round(ris_test, 4))
  salva_tab(round(ris_test, 4), paste0("H_", s, "_risultati_test_tre_soglie"))

  # H4 - Training, validation e test (soglia di compromesso)
  titolo(paste("H4 - Training, validation e test (soglia di compromesso) - schema", s))
  tre_insiemi <- rbind(training   = valuta(p_tr, y_tr, soglie["Compromesso"]),
                       validation = valuta(p_va, y_va, soglie["Compromesso"]),
                       test       = valuta(p_te, y_te, soglie["Compromesso"]))
  print(round(tre_insiemi, 4))
  salva_tab(round(tre_insiemi, 4), paste0("H_", s, "_training_validation_test"))

  pred_c <- factor(ifelse(p_te >= soglie["Compromesso"], "1", "0"), levels = c("0", "1"))
  cat("\nMatrice di confusione (test, soglia di compromesso):\n")
  print(table(Prediction = pred_c, Reference = y_te))

  risultati_xgb[[s]] <- list(griglia = griglia, best = best, modello = xgb_finale,
                             p_tr = p_tr, p_va = p_va, p_te = p_te, y_te = y_te, y_va = y_va,
                             soglie = soglie, ris_test = ris_test, tre_insiemi = tre_insiemi,
                             nomi_x = colnames(x_tr))
  tempo()
}


# ##############################################################################
# H5 - CONFRONTO CON I MODELLI DELLA PARTE E
# ##############################################################################
for (s in schemi) {
  titolo(paste("H5 - XGBoost a confronto con gli altri modelli - schema", s))
  rx  <- risultati_xgb[[s]]
  re  <- risultati[[s]]
  y_te <- rx$y_te

  # Previsioni sul test di tutti i modelli (le altre vengono dalla Parte E)
  prob <- c(list(XGBoost = rx$p_te), lapply(re$modelli, function(m) m$te))
  sogl <- c(XGBoost = unname(rx$soglie["Compromesso"]), setNames(re$soglie$Compromesso, rownames(re$soglie)))

  # AUC con intervallo di confidenza (DeLong)
  roc_l <- lapply(prob, function(p) roc(y_te, p, levels = c("0", "1"), direction = "<", quiet = TRUE))
  ic <- t(sapply(roc_l, function(r) as.numeric(ci.auc(r, method = "delong"))))
  colnames(ic) <- c("IC95_inf", "AUC", "IC95_sup")
  cat("AUC sul test con IC 95%:\n"); print(round(ic, 4))

  # Metriche con la soglia di compromesso
  metriche <- t(sapply(names(prob), function(n) valuta(prob[[n]], y_te, sogl[n])))
  cat("\nMetriche sul test (soglia di compromesso):\n"); print(round(metriche, 4))
  salva_tab(round(cbind(metriche, ic[, c(1, 3)]), 4), paste0("H_", s, "_confronto_modelli"))

  # Test di DeLong: XGBoost contro ciascun modello
  delong <- do.call(rbind, lapply(setdiff(names(prob), "XGBoost"), function(n) {
    t <- roc.test(roc_l$XGBoost, roc_l[[n]], method = "delong")
    data.frame(confronto = paste("XGBoost vs", n), AUC_XGB = round(as.numeric(auc(roc_l$XGBoost)), 4),
               AUC_altro = round(as.numeric(auc(roc_l[[n]])), 4),
               Z = round(unname(t$statistic), 3), p_value = signif(t$p.value, 3))
  }))
  cat("\nTest di DeLong:\n"); print(delong, row.names = FALSE)
  salva_tab(delong, paste0("H_", s, "_test_DeLong"))

  # Test di McNemar sugli errori (soglia di compromesso)
  corretto <- lapply(names(prob), function(n)
    factor(ifelse(prob[[n]] >= sogl[n], "1", "0"), levels = c("0", "1")) == y_te)
  names(corretto) <- names(prob)
  mcn <- do.call(rbind, lapply(setdiff(names(prob), "XGBoost"), function(n) {
    tb <- table(factor(corretto$XGBoost, levels = c(TRUE, FALSE)),
                factor(corretto[[n]],     levels = c(TRUE, FALSE)))
    t <- mcnemar.test(tb)
    data.frame(confronto = paste("XGBoost vs", n), chi2 = round(unname(t$statistic), 2),
               p_value = signif(t$p.value, 3))
  }))
  cat("\nTest di McNemar:\n"); print(mcn, row.names = FALSE)
  salva_tab(mcn, paste0("H_", s, "_test_McNemar"))

  # Curve ROC
  col_mod <- c(XGBoost = "purple", RandomForest = "forestgreen", SVM = "firebrick",
               Logistica = "steelblue", ReteNeurale = "darkorange")
  plot(roc_l$XGBoost, col = col_mod["XGBoost"], lwd = 2, legacy.axes = TRUE, asp = NA,
       main = paste0("Curve ROC (test set) - schema ", s, " - con XGBoost"),
       xlab = "1 - Specificity (falsi positivi)", ylab = "Sensitivity (poveri individuati)")
  for (n in setdiff(names(roc_l), "XGBoost")) plot(roc_l[[n]], col = col_mod[n], lwd = 1.5, add = TRUE)
  legend("bottomright", bty = "n", lwd = 2, col = col_mod[names(roc_l)],
         legend = paste0(names(roc_l), " (AUC = ", sprintf("%.3f", ic[names(roc_l), "AUC"]), ")"))
  salva_fig(paste0("H_", s, "_02_curve_ROC_con_xgboost"), 1600, 1000)

  risultati_xgb[[s]]$confronto <- list(ic = ic, metriche = metriche, delong = delong, mcnemar = mcn)
}


# ##############################################################################
# H6 - IMPORTANZA DELLE VARIABILI (XGBoost)
# ##############################################################################
for (s in schemi) {
  titolo(paste("H6 - Importanza delle variabili (XGBoost, gain) - schema", s))
  imp <- as.data.frame(xgb.importance(model = risultati_xgb[[s]]$modello))[, c("Feature", "Gain")]
  # se il modello non conserva i nomi delle colonne (f0, f1, ...), li si ricostruisce
  if (all(grepl("^f[0-9]+$", imp$Feature))) {
    imp$Feature <- risultati_xgb[[s]]$nomi_x[as.integer(sub("^f", "", imp$Feature)) + 1]
  }
  print(head(imp, 20), row.names = FALSE)
  salva_tab(imp, paste0("H_", s, "_importanza_xgboost"))
  top <- head(imp, 15)
  par(mar = c(5, 11, 4, 2))
  barplot(rev(top$Gain), names.arg = rev(top$Feature), horiz = TRUE, las = 1, col = "purple",
          border = NA, cex.names = 0.8, xlab = "Gain (contributo al miglioramento degli alberi)",
          main = paste0("Importanza delle variabili - XGBoost (", s, ")"))
  par(mar = c(5, 4, 4, 2))
  salva_fig(paste0("H_", s, "_03_importanza_xgboost"), 1600, 1000)
  risultati_xgb[[s]]$importanza <- imp
}


# ##############################################################################
# H7 - SINTESI
# ##############################################################################
titolo("H7 - Sintesi: XGBoost e random forest a confronto (test, soglia di compromesso)")
sintesi_xgb <- do.call(rbind, lapply(schemi, function(s) {
  cf <- risultati_xgb[[s]]$confronto
  do.call(rbind, lapply(rownames(cf$metriche), function(n) data.frame(
    schema = s, modello = n,
    AUC = round(cf$ic[n, "AUC"], 3),
    IC95 = paste0("[", round(cf$ic[n, "IC95_inf"], 3), "; ", round(cf$ic[n, "IC95_sup"], 3), "]"),
    Accuracy = round(cf$metriche[n, "Accuracy"], 3),
    Sensitivity = round(cf$metriche[n, "Sensitivity"], 3),
    Specificity = round(cf$metriche[n, "Specificity"], 3),
    Balanced_acc = round(cf$metriche[n, "Balanced_acc"], 3),
    Kappa = round(cf$metriche[n, "Kappa"], 3),
    sopra_80 = ifelse(cf$metriche[n, "Accuracy"] >= 0.80, "si", "no"))))
}))
print(sintesi_xgb, row.names = FALSE)
salva_tab(sintesi_xgb, "H_sintesi_con_xgboost")

cat("\nParametri scelti per XGBoost:\n")
for (s in schemi) {
  b <- risultati_xgb[[s]]$best
  cat(" ", s, ": max_depth =", b$max_depth, "- eta =", b$eta, "- alberi =", b$nrounds,
      "- AUC validation =", round(b$auc_valid, 4), "\n")
}
cat("\nDifferenza di AUC sul test, XGBoost meno random forest:\n")
for (s in schemi) {
  ic <- risultati_xgb[[s]]$confronto$ic
  dlg <- risultati_xgb[[s]]$confronto$delong
  cat(" ", s, ":", sprintf("%+.4f", ic["XGBoost", "AUC"] - ic["RandomForest", "AUC"]),
      "- p DeLong =", dlg$p_value[dlg$confronto == "XGBoost vs RandomForest"], "\n")
}

save(risultati_xgb, sintesi_xgb, file = out("risultati_xgboost.RData"))
cat("\nSalvato: output/risultati_xgboost.RData\n")
tempo()

titolo("G2 - Informazioni sulla sessione (riproducibilità)")
cat("Durata complessiva:", round(as.numeric(difftime(Sys.time(), inizio_script, units = "mins")), 1), "minuti\n\n")
sessionInfo()

# Salvataggio dell'intero ambiente: con load() si recupera tutto senza rigirare
save.image(out("sessione_completa.RData"))
cat("Salvato: output/sessione_completa.RData\n")

# ------------------------------------------------------------------------------
# CHIUSURA DEL FILE DI OUTPUT
# ------------------------------------------------------------------------------
sink()   # chiude output/output_completo.txt
