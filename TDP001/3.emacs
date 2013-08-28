;; Parsing and sorting a list the emacs way!

;; Step 1: Replace dashes with zeros
; 1.1. Place the cursor on the first name (Mikaela)
; 1.2. Run M-x 'replace-string'
; 1.3. Type in ' - ' (without 's) and press <enter> 
; 1.4. Type in ' 0 ' (without 's) and press <enter> 
; Now all the -'s should have been replaces by 0's

;; Step 2: Add the numbers together
; 2.1 Place the cursor on the first name (Mikaela)
; 2.2 Type C-x (
; 2.3 Type C-e ')' M-b M-b '(+ ' C-e
; 2.4 Type M-x 'ev' <tab> 'p' <tab> <enter>
; 2.5 Type C-p M-d C-p C-y ' ' M-f M-f C-k C-n C-k C-k
; 2.6 Type C-x )
; 2.7 Execute the macro (C-e and then e) until everything look nice

;; Step 4: Sort
; 4.1 Place the cursor on the first name (Mikaela)
; 4.2 Press C-space and mark all the names
; 4.3 Type C-u 1 M-x 'sort-lines' <enter>

Mikaela Andersson       14   -  17
David Fors               8  12  11
Klas Hägglund            -  13   9
Oskar Holmqvist          9  12  10
Gustaf Karlsson         12  14  13
Annica Rundgren         17  10  15
Marcus Carlsson         12   9  12
Johanna Rönnberg         9  15  13
Josefine Carlson        18  14  16
Magnus Ahlsten          13  18  14
Jerker Leo               -   -  11
Jan-Olof Kärrsgård       8  11  13
Siv Tidblom             11   9  12
Mats Karlzon            15  17  10
Göran Johansson         16  19  15
Britt Lidell             -  14   9
Yngve Johanson          10  11   8
Ingrid Lindgren          8  13  13
Kjell Djurstedt         12  12  12
Lennart Andersson        9  10  13
Elisabeth Björklind     15  12  10
