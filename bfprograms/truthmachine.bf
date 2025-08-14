+++++++++                Add 9 to the first cell
>>,                      Set the input to the third cell
<<[->+++++<]             Multiply the first cell (9) by 5 in the second cell
>+++                     Add 3 to the second cell
[>-<-]                   Subtract the second cell from the third cell
>>+++++[>++++++++++<-]>- Gets the ASCII code for "1"
<<[>>.<<]>>-.            Checks if the input is "1":
                         If it is move to the output cell print it then go back to the start-loop cell to start the loop again
                         Else moves to the output cell decrements it and print it