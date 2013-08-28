Steg 1 - Byt ut streck mot nollor:
  
  Då kommandot i steg 2 inte kommer att klara att addera nummber med streck (t.ex 14 + 17 + -)
  så ska vi byta ut dessa mot nollor med följande kommando:
    sed 's/\ -/0/g' resultat.txt
  Där 
    sed är en applikation som är suveränt på att manipulera text
    's/\ -/0/g' är uttrycket för hur utdata ska genereras från indata
    resultat.txt är filen där vi tar vår indata

  Seduttryck kan förklaras på detta vis:
    's/ TA BORT DETTA / SKRIV DET HÄR ISTÄLLET /g'
    's/\ -/0/g' letar därför upp varje mellanslag (skrivs '\ ') följt av ett streck ('-')
      och byter ut det mot en nolla ('0')
    Anledningen att jag tog med ett mellanrum innan strecket är att vi annars skulle byta namn som
      Kurt-Stefan mot Kurt0Stefan, vilket denna Kurt-Stefan nog inte skulle uppskatta

Steg 2 - Skriv om raderna:
  
  Kommandot vi kommer använda oss av ser ut såhär:
    |while read -a r;do echo $[r[2]+r[3]+r[4]] ${r[0]} ${r[1]};done
    (Tillsammans med Steg 1 ser det ut såhär: sed 's/\ -/0/g' resultat.txt|while read -a r;do echo $[r[2]+r[3]+r[4]] ${r[0]} ${r[1]};done)
  Där
    strecket (|) innebär att vi tar all utdata från det som står vänster om det och
      ger det till programmet/uttrycket på höger sida (alltså while .. ; do .. ; done)
    while read -a r
      läser en rad i taget och sparar alla ord i denna rad i listan $r
      r[0] är då ord 1 (t ex "Mikaela"), r[1] är ord 2 (t ex "Andersson"), osv
    do .. ;done
      tar det kommandot som står mellan do och done och exekverar det på varje rad
    echo $[r[2]+r[3]+r[4]] ${r[0]} ${r[1]}
      adderar och skriver ut r[2], r[3] och r[4] (alltså ord 3, 4 och 5)
      skriver ut r[0]
      skriver ut r[1]

  Kommandot läser in en rad i taget, vilket ser ut på följande sätt:
    Mikaela Andersson       14   -  17
  Och ger utdata som ser följande ut:
    31 Mikaela Andersson
 
Steg 3 - Sortera raderna:

  Då utdata från det föregående kommandot nu ligger i oordning (Raderna ska stå med högst nummer först),
    så måste vi sortera resultatet
    
  Vi kommer därför lägga på |sort -r på det föregående kommandot, så det totalt ser ut såhär:
      sed 's/\ -/0/g' resultat.txt|while read -a r;do echo $[r[2]+r[3]+r[4]] ${r[0]} ${r[1]};done|sort -r
  Där
    strecket (|) innebär att vi tar all utdata från det som står vänster om det och
      ger det till programmet på höger sida (alltså sort)
    sort sorterar varje rad så att den rad med minst nummer står överst och störst längst ner
      då det är helt tvärtemot vad vi vill så ger vi också kommandot -r, 
      vilket gör att sort sorterar åt andra hållet istället
    

Summa summarum:
  sed 's/\ -/0/g' resultat.txt|while read -a r;do echo $[r[2]+r[3]+r[4]] ${r[0]} ${r[1]};done|sort -r
