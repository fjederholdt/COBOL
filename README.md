# COBOL

## Opsætning af GnuCOBOL på Windows
Denne vejledning er til Specialisterne COBOL-forløb, så du kan komme hurtigt i gang med
GnuCOBOL, Visual Studio Code og bygge dine COBOL-programmer.

### 1. Download GnuCOBOL All-in-One
1. Gå til: https://superbol.eu/developers/windows/
2. Vælg MSI-pakken til Windows (64-bit anbefales, fx “GnuCOBOL 3.2 All-inOne”).
    gnucobol-3.2-aio-20240402-user.msi
3. Gem filen på din computer.

### 2. Installer GnuCOBOL
1. Dobbeltklik på den downloadede .msi-fil.
2. Følg installationsguiden:
    Installér til mappen: C:\GnuCobol
3. Klik på Finish, når installationen er færdig.
4. Sørg for, at stien C:\GnuCobol\bin er tilføjet til "Path" i miljøvariablerne:
    * Søg efter "miljøvariabler" i Windows' søgefelt.
    * Vælg Rediger miljøvariabler for din konto.
    * Find variablen "Path", klik Rediger, og tjek om C:\GnuCobol\bin allerede
        findes.
    * Hvis ikke, klik på Ny, og tilføj C:\GnuCobol\bin.
    * Afslut med OK.

### 3. Test installationen
1. Åbn Kommandoprompt (cmd).
    * Skriv: cobc -v
    * Du burde nu se GnuCOBOL versionen
2. Kontroller, at ifølgende mapper findes
    * C:\GnuCOBOL\include
    * C:\GnuCOBOL\lib

### 4. Hvis ikke Visual Studio Code er installeret – skal den installeres.
1. Gå til https://code.visualstudio.com/.
2. Download og installer Visual Studio Code.
3. Start Visual Studio Code.
4. Opret en ny mappe i dit workspace, og kald den Cobol.
5. Installer extension til highlight af Cobol syntax
    https://marketplace.visualstudio.com/items?itemName=bitlang.cobol

### 5. Byg dit cobol program.
1. Åben terminalen i Visual Code.
2. Kør følgende kommando:
    .\cobbuild.bat -x Opgave[NUMMER].cob -lcob
Dette laver en Opgave[NUMMER].exe fil i projekt mappen

## Opgave beskrivelser

### Opgave 1 - Hello world program med variabler
Printer "HELLO WORLD" til consolen.

### Opgave 2 - Variabler og MOVE
Bruger MOVE til at sætte variabler og printe til consolen med display.

### Opgave 3 - STRING og strenghåndtering
Bruger STRING til at sammensætte variabler og strenge ind i en anden variable.

### Opgave 4 - COBOL Struktur
Demonstrerer arbejdet med strukturerede data, hierarkisk organisering af variabler og brug af MOVE-kommandoer til at populere datastrukturer. Programmet viser output af hele datastrukturen til konsolen.

### Opgave 5 - COPY-statement og genbrug af kode
Introducerer COPY-statementet til at genbruge datastruktur-definitioner fra eksterne copybooks. Samme funktionalitet som Opgave 4, men med bedre kodeoversigt og vedligeholdelse.

### Opgave 6 - Sekventiel fil I/O med læsning og skrivning
Arbejder med sekventielle filer, læser kunde-data fra input-fil, sammensætter fulde navne med STRING, og skriver data til output-fil. Anvender READ/WRITE kommandoer og EOF-håndtering.

### Opgave 7 - Formatering og strukturering af filoutput
Bygger på Opgave 6 med udvidet funktionalitet. Implementerer flere FORMAT-rutiner (FORMAT-NAME, FORMAT-ADDRESS, FORMAT-CONTACT-INFO, FORMAT-ACCOUNT-INFO) der formatterer data før skrivning til fil. Bruger FUNCTION TRIM til at fjerne mellemrum.

### Opgave 8 - Join-operationer mellem multiple filer
Arbejder med to input-filer (kundedata og kontooversigt) og skriver kombineret data til output-fil. Implementerer indre join-logik ved at sammenligne kunde-ID'er mellem filerne. Anvender OPEN/CLOSE på multiple filer.

### Opgave 9 - In-memory array processing
LæserAccountInfo-data ind i et OCCURS-array i hukommelsen i stedet for at åbne filen gentagne gange. Implementerer PERFORM VARYING for loop-iteration gennem arrays og matcher data baseret på kunde-ID.

### Opgave 10 - SORT, arrays og komplekse beregninger
Implementerer sortering af transaktioner med SORT statement (sorteret på CPR og dato). Arbejder med store arrays (54.715 poster), valutakonvertering (USD til DKK), og beregning af samlede ind- og udgange. Bruger PERFORM UNTIL og PERFORM VARYING.

### Opgave 11 - Avanceret analyse med multiple output-filer
Udvidelse af Opgave 10 med flere analysekapaciteter: Top-3 højeste saldi, årlige cash-flow beregninger, og shop-analyser. Skriver resultater til separate output-filer (Balances, YearlyCashFlow, BestShops).

### Opgave 12 - Sanktionsliste-matching med Levenshtein-distance
Implementerer sanktionsliste-kontrol ved at sammenligne kundedata mod sanktionsliste. Beregner matchingsscore baseret på Levenshtein-distance algoritme for navne, dato-sammenligning og landevalidering. Anvender weighted scoring-system med konfigurerbare vægte.