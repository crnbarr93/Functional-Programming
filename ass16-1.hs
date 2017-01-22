{- To create a Month in Haskell that could return the number of days the year of the month would also need to be specified
   To achieve this the Month type takes both the number associated with the month of the year along with another value for the year i.e. (2, 2016) is February, 2016.
   Hence the month type is of (Int, Int)
 -}

type Month = (Int, Int)	

{- A date consists of a day, month and year, normally in that order unless you're using the American format. To achieve this in Haskell an Int and Month value are used.
   The first value (Int) is used to signify the day (between 1 and the length of the month) and a Month, to signify what month and year the day belongs to.
 -}
type Date = (Int, Month)

--An event is specified as having 3 properties. A date (as specified above), a form type (given here as a bounded Int) and a comment (String or list of chars).
type Event = (Date, Int, [Char])

--To aid in creating the History type variable a HistoryEvent is specified as (Date, Int, Float). The date signifies the date of the event, the Int signifies the gap since the previous event and the Float is used to hold the average gap at that particular event
type HistoryEvent = (Date, Int, Float)

--Using the HistoryEvent specified above the History type is a list of HistoryEvents which can be updated using the history function below or shown using the showHistory function
type History = [HistoryEvent]

days :: Month -> Int --The days function uses a Month (containing a value for both the month and the year) before comparing these to each possible month (i.e. 1-12 or January to December)
--To return the correct number of days for every possible month the below 'case' statement is used.
--Example: days (2, 2016) returns 29
days (a, b) = if a <= 0 || a > 12 || b < 0 --Validation is used to ensure that the Month and Year are valid possibilities, 1 to 12 due to 12 months and a year greater than 0.
		then error "Invalid Month or Year" --If the input is invalid, alert the reader to that fact
	      else case a of 1 -> 31 --For each possible input (January (1) to December (12) return the number of days in each month
			     2 -> case b `mod` 4 of 0 -> 29 --The exception to this is due to leap years (Every 4 years) in which case the value of the input year is tested to be a leap year, if so then February has 29 days and 28 otherwise
					            _ -> 28
			     3 -> 31
			     4 -> 30
			     5 -> 31
			     6 -> 30
			     7 -> 31
			     8 -> 31
			     9 -> 30
			     10 -> 31
			     11 -> 30
			     12 -> 31

days (_, _) = error "A month uses two int values" --In the case the input is not of the correct format, an error is returned


{- 
 The showd function uses a Date type (as specified in the preamble) in order to print the date in a dd/mm/yyyy format.
 An example use is showd (29, (2, 2016)) would print "29/2/2016" whereas showd (30, (2, 2016)) would print an "Invalid Date" error
 -}
showd :: Date -> String
showd (a, (b, c)) = if a <= 0 || a > days (b, c) --If the date is below or equal to 0 then it is invalid, alternatively if the date is larger than the amount of dates in the given month it is also invalid.
			then error "Invalid date" 
		    else show a ++ "/" ++ show b ++ "/" ++ show c -- To print the date in dd/mm/yyyy format each value is appropriate appended to a string, seperate by a '/'

		    
{-
  The before function is used to check two dates (a, b) and returns a Boolean value dependant on if a is a date previous to b.
  To check if one date is before another each value of both dates must be compared. This is evaluated below.
  Example usage: before ((1, (2, 2016)), (1, (2, 2016))) returns True
 -}
before :: (Date, Date) -> Bool
before ((a, (b, c)), (d, (e,f))) | (a <= 0) || (a > days (b, c)) = error "Invalid First Day" --Normal validation as in previous functions is applied to ensure input dates are valid
				 | (d <= 0) || (d > days (e, f)) = error "Invalid Second Day"
				 | (b <= 0) || (b > 12) = error "Invalid First Month"
				 | (e <= 0) || (e > 12) = error "Invalid Second Month"
				 | (c < 0) = error "Invalid First Year"
				 | (f < 0) = error "Invalid Second Year"
				 | (c > f) = False --First comparison is between the years. In the case the first year is larger than the second then False
				 | (c < f) = True -- If strictly smaller than True
				 | (c == f) && (e > b) = False --If the years are the same then the comparison falls to the months.
				 | (c == f) && (b < e) = True --Similarly to year strict comparisons are used to return True or False
				 | (c == f) && (b == e) && (a <= d) = True --Finally the dates are used in the case both dates fall in the same month
			         | (c == f) && (b == e) && (a > d) = False 


			         
{-
  To achieve the idea of a finite sequence a function is used to limit the possible inputs of the form type inside of an Event type         
  Example usage form 1 returns "F1", form 11 returns an error of "Invalid Form"
    -}
form :: Int -> String
form a = if a <= 0 || a > 10 then error "Invalid Form" --Bound the possible values of form in order to make it a finite sequence
	 else "F" ++ show a --Return a string "F" with the value appended to produce the appropriate result
--The above function is very similar to a show function

{-
  To aid in visualisation of the Event type the below function prints the Event type in a readable format ("Date | Form | Comment")
  Example usage: showEvent ((2, (2, 2016)), 2, "Comment") returns "2/2/2016 | F2 | Comment"
  -}
showEvent :: Event -> String
showEvent (a, b, c) = if b <= 0 || b > 10 then error "Invalid Form" --Validation is used to limit the form input (produces a cleaner error)
		      else  showd a ++ " | " ++ form b ++ " | " ++ c --Similarly to date all values of the input are appended to a string to produce a readable format


{- 
  The History variable uses a value to store the difference between two dates. As this is not a trivial task the below function is applied to the two dates (a, b) to determine the difference in days b is from a (b-a)
  Example usage: dateDiff((1, (1, 2016)), (1, (2, 2016))) returns 31 (the number of days in January)
 -}
dateDiff :: (Date, Date, Int) -> Int --The function takes in a third type of Int in order to produce a count value that will be returned by the function (in this case 'g')
dateDiff((a, (b, c)), (d, (e, f)), g) | before((a, (b, c)), (d, (e, f))) == False = error "First date is after second" --Vaidation is used to ensure that the second date is after the first
				      | (a == d) && (b == e) && (c == f) = g --When/if the dates match the value of the counter is returned
				      | a < d = dateDiff((d, (b, c)), (d, (e,f)), g+(d-a)) --If the days are not the same then they are adjusted to be the same and the counter is appropriately modified
				      | d < a = dateDiff((a, (b, c)), (a, (e,f)), g-(a-d))
				      | b < e = dateDiff((a, (b+1, c)), (d, (e, f)), g+(days(b,c))) --Similarly for the months, if they are not the same they are adjusted to be so and the value of the difference in days is appended to the counter
				      | e < b = dateDiff((a, (b, c)), (d, (e+1, f)), g-(days(e,f))) --The days function is used to return the number of days in each month
				      | (c < f) && (c `mod` 4 == 0) && (b <= 2) = dateDiff((a, (b, c+1)),(d, (e,f)), g+366) --After/if the days/months are equal the value of the years is adjusted to be the same. The first case is if the month is January/February during a leap year, as jumping forward a year will add 366 days instead of 365
				      | (c < f) && ((c+1) `mod` 4 == 0) && (b > 2) = dateDiff((a, (b, c+1)), (d, (e,f)), g+366) --Similarly if the next year when incrementing is a leap year and February will be included in the jump then 366 days must be added
				      | (c < f) = dateDiff((a, (b, c+1)), (d, (e,f)), g+365) --The last case is that a leap year is not involved in any format and hence only 365 days are added

{-
  In order to help visualise the History type (list of history events) the below show function is used. The below function produces a string format representation of a History type.
  To do this the function appends each value in the History type's elements to a string before printing them seperated by "|". In order to seperate each element a ">" is placed between them and "End" is used to signify the end of the sequence
  Example usage: showHistory([((1, (2, 2016)), 0, 0), ((2, (2, 2016)), 1, 1)]) returns "1/2/2016 | 0 | 0.0 > 2/2/2016 | 1 | 1.0 > End"
 -}
showHistory :: History -> String
showHistory [] = "End" --Print end at the end of the sequence
showHistory ((a, gap, av_gap) : xs) = showd a ++ " | " ++ show gap ++ " | " ++ show av_gap ++ " > " ++ showHistory xs --Append values to a string before recursively moving to the next element of the sequence

{-
  As Haskell variables cannot have their state changed (as far as I'm aware) the history variable is in the form of a function which uses a History type, Date and Int as inputs.
  The History input is the input that you want to update, the date input is the event you wish to append and the Int input is a counter uesd in calculating the average gap.
  Example usage: history([((1, (1, 2016)), 0, 0)], (2, (1, 2016)), 0) returns [((1, (1, 2016)), 0, 0), ((2, (1, 2016)), 1, 1.0)]
 -}
history :: (History, Date, Int) -> History
history (((a, gap, av_gap) : xs), d, n) = if xs == [] then [(a, gap, av_gap)] ++ [(d, dateDiff(a, d, 0), ((av_gap*fromIntegral(n))+fromIntegral(dateDiff(a,d, 0)))/(fromIntegral(n+1)))]
					      {- 
						The first case checks if the current element in the recursion is the tail of the list (as this is the only element that matters when appending to a History type)
						If this is the case then the new event is appended with the new calculated values.
						To calculate the second attribute (gap) the dateDiff function defined previously is used to calculate the number of days since the previous event.
						To calculate the average gap (av_gap) the previous av_gap is multiplied by the number of elements currently in the list before being incremented by the new gap value and divided by the new number of elements in the list.
						As av_gap is of type Float and two of the inputs in the calculation are Ints the conversion function "fromIntegral" is used to convert the Ints to type Float.
					      -}
					      else [(a, gap, av_gap)] ++ history(xs, d, n+1) --In the case that the current element is not the tail then append to the output list and move to the next
history(_,_,_) = error "Invalid inputs, please use input a sequence of events, a date to be added and 0 i.e. history(Sequence, Date, 0)" --Validation to ensure correct type


