APPLICATION THAT SOLVES HONEYCOMB RIDDLE: http://www.lamiglowkimix.com/modules/honeycomb/honeycomb.php
 
ALGORITHM OF ACTION:
 0. Input data as a .txt file with sample content: Plaster ['BD ..', '.GA.D', '.FEG', 'ABDCF', 'E..B']
 1. Parse input data to a list of lists
 2. Check if the data format is correct
 3. Check if the initial configuration of the letters is correct
 4. Start at the end of the last line by inserting sequentially the letters 'ABCDEFG' in successive funcion calls
 5. Insert the letter and check if surrounding configuration is correct, i.e. if the letter and her surrounding (n+1 and n+2) do not repeat
    and the current combination is not on the list of incorrect combinations:
      5.1. If everything is OK: then proceed with iterating from behind through the successive fields in the rows
      5.2. If NO: try next letter in the order 'ABCDEFG'
      5.3. If no letter matches, save the current combination at this point as incorrect, replace last inserted letter to '.' and go back to
	   the previous field by repeating step 5 from the beginning
 6. After completing the entire array return the solution which is list of lists