# biorecur (development version)

## biorecur 0.0.1
Fork (copia-incolla) delle funzioni del pacchetto SPARE. Il pacchetto SPARE (https://github.com/JasperHof/SPARE) contiene un singolo file 'functions.R' che contiene tutte le funzioni. Non ha test ma solo un manuale.

Abbiamo deciso che la strategia sia quella di 'spacchettare' le singole funzioni nel pacchetto e implementarle singolarmente, ognuna con i suoi singoli test ('refactoring').

La prima cosa fatta è stata aggiustare lo stile di coding (spazi, indentazioni, ecc) poi le funzioni singole sono state messe in file singoli.

Sono stati creati (da Daniele Sabbatini) dei file di prova .bim, .bam, .bed per eseguire velocemente i test.

## biorecur 0.1.1.9000
Sono stati creati i test base (non specifici) per tutte le funzioni e abbiamo cominciato a vedere funzione per funzione di creare un test che abbia senso.

- Funzione `SPARE-bed.R` e test `test-SPARE-bed.R` completati
- Funzione `extract-stats.R` e test `extract-strats.R` ci siamo fermati perché non riusciamo a farla andare in errore come specificato dalla funzione originale (vedi https://github.com/UBESP-DCTV/biorecur/issues/2)





