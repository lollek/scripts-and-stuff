""" Uppgift 0a: """

# a) Hur kan ni avgora ifall koden exekveras som en modul eller som ett program?

Det bästa sättet att avgöra hur koden exekveras är att inspektera variabeln __name__.

# b) Vad har variablen __name__ for varde nar ni kor koden som en modul?

Variabeln __name__ har, i det fall den körs som modul, samma namn som filen har.
Om man till exempel importerar minmodul.py så kommer __name__ att ge "minmodul"

# c) Vad har variabeln __name__ for varde nar ni kor koden som ett program?

Variabeln __name__ har, i det fall den körs som program, värdet "__main__

# d) Hur kan ni dokumentera funktionaliteten hos en funktion?

Det bästa sättet att dokumentera funktionaliteten hos en funktion är att skriva en 
beskrivande kommentar precis efter funktionsdefinitionen. Detta gör att man kan använda sig av
inbyggda funktionen help(funktionsnamn) för att få information om hur man använder det

Exempel:
  
def times_two(number):
  """ Returns given number multiplied by two """
  return number*2
  
  
