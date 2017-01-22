{-
CS256 Assignment Part 2 Analysis
================================
The below haskell program was designed around the concept of a 'shaggy' programming language. The program is to take in a shaggy program and
create a parse tree for it. In this case the program takes in a shaggy program in the format of a string, where each line is separated by '\n'.
There are several functions to take in a string and convert it into a list of list of strings where each element of the parent list is a separate line of the input.
Within each element of the list of lists is a list of strings, where each string is a separate word or expression of the original input.
For example the input: "var x\n var y" would be converted to [["var", "x"], ["var", "y"]]
The reasoning behind these steps is to convert from a stream of words and expressions to a parsed version that would be easier to conver to a tree.
Converting into this format allows each 'line' of the original input to be converted to a 'shaggy' format and hence to a 'parse tree'.
A parse tree, in this context, is the idea of a 'binary' tree that symbolises a particular expression or definition. For example "+ 3 4" can be shown as:
      +
    /   \
   3     4
Where the tree would evaluate from the right, i.e. 4 + 3. (This ordering is important in the case of subtraction)
For a 'shaggy' program as an input this haskell program will produce a list of parse trees for each individual definition within a 'shaggy' program, where a 'shaggy' program is defined as a list
of definitions (i.e. a line of 'shaggy' code). A 'shaggy' definition can also have the form "var <string>" which defines a variable type that is assigned to the string, and "eval <string>" that defines
the evaluation of the input variable string. For "eval" and "var" the parse trees are individual nodes, i.e. "var x" and "eval x" are shown as (respectively):

  Variable(x)   Evaluate Variable(x)

Given these structures an input 'shaggy' program such as "var x\nx = 4\nvar y\ny = + x 4\neval y" would be parsed as:

var x
x = 4
var y
y = + x 4
eval y

  Variable(x)   Variable(x)   Variable(y)   Variable(y)     Evaluate Variable(y)
                    |                           |
                    4                           +
                                               /  \
                                               x   4
-}

{-
Explanation of Data and Types
=============================
The first data declaration in this Haskell program is that of an 'Operand', an operand in this case is a possible operation type on two expressions.
In this case the possible operands are multiplication, addition and subtraction, each using a prefix notation.
For example multiplication would be represented as: * 3 4 and using the data type would be Multi 3 4.
The next data declaration 'Expression' is used to define the structures used within a shaggy 'Definition'. In this case they can be a variable (Variable String), a constant (Const Int),
a 'minus expression' (Minus Int), an operation (Ope Operand Expression Expression), a bracketed expression (Brac Expression) and to help with the declaration of a definition they an also be
an operand (Op Operand) or an evaluate statement (Evalu String).
Using the 'Expression' declaration the 'Definition' data type can be declared. The 'Definition' type is what is used to construct a 'shaggy' program. Due to this
a 'Definition' can take three different forms - "var <String>" (Var String), "<variable> = expression" (Ass String Expression) or "eval <String>" (Eval String).
Bringing it all together a 'Program' is a list of 'Definition'.
The final data type is a recursive data type for a binary tree where the three constructs of each are the root node and the left and right subtree.
-}

data Operand = Multi | Add | Subt
data Expression = Op Operand | Minus Int | Const Int | Variable String | Ope Operand Expression Expression | Brac Expression | Evalu String
data Definition = Var String | Ass String Expression | Eval String
type Program = [Definition]
data Tree a = Empty | Node a (Tree a) (Tree a)


{-
Explanation on Checks and Converters
====================================
The first check function takes a String input and checks that it fits the criteria of a 'shaggy' variable, i.e. it begins with an alphabetic character (between 'a' and 'z').
The 'tenPowerOf' function takes an integer input and returns 10 to the power of the input, this is used in converting from a string integer to an integer value.
To convert from a single digit string integer to an integer value the intValOfChar function is used. The intValOfChar function works by associating a character integer with it's appropriate integer value.
Finally, 'stringInt' converts an input string integer into it's correspending integer value, i.e. "42" = 42.
The 'stringInt' function is constructed using another function 'stringToIntList' which takes any string integer value and returns a list of the integers which construct the input string integer.
The 'stringInt' function uses the input integer list and a list of powers of 10 to create a single list of each value multiplied by a power of 10 before summing the list.
I.e. "42" -> [4, 2] -> [4*10^1, 2*10^0] -> [40, 2] -> 40+2 -> 42
-}
varCheck :: String -> Bool
varCheck [] = False
varCheck (x:l) = ('a' <= x) && (x <= 'z')

tenPowerOf :: Int -> Int
tenPowerOf a = 10^a

intValOfChar :: Char -> Int
intValOfChar x = case x of
                  '1' -> 1
                  '2' -> 2
                  '3' -> 3
                  '4' -> 4
                  '5' -> 5
                  '6' -> 6
                  '7' -> 7
                  '8' -> 8
                  '9' -> 9
                  '0' -> 0
                  _ -> error ("Char is not a number")

tenPowers = map tenPowerOf [0..]
stringInt :: String -> Int
stringInt x = sum (reverse [a*b | (a,b) <- (zip (reverse(stringToInt(x))) tenPowers)])

stringToInt :: String -> [Int]
stringToInt [] = []
stringToInt(x:l) = (intValOfChar x) : (stringToInt l)

{-
Explanation on Parsers
======================
The process of parsing a 'shaggy' program from a string involves several steps. The first of which is to divide each line of the input in to a list of lines. The 'lineParser' function is used for this step,
the input of which is a string, a list of strings (to be returned) and another string to hold the current line before being inserted into the list. To help with the number of inputs the 'lineParserShort' function
takes only an input of a string and uses the 'lineParser' function to give a list of strings as an output. The next step of the process involves splitting words and expressions up by dividing them by the spaces that
separate them. This process is handled by the 'spaceParser' function (and similarly the 'spaceParserShort' function) which takes 5 inputs. Similar to the 'lineParser' function the 'spaceParser' function uses several of its
inputs to store what is to be inserted to each list. As the function returns a list of lists there are two extra inputs to store the string to be inserted to the list that is to be inserted into the returned list.
The final step involves compressing bracketed expressions. This intakes a list of strings (each a separated word/expression from the original input) and compresses any words/expressions within brackets in to one whole string.
This function works in a similar fashion to the previous steps and also has a short hand function ('compressBracketsShort'). All of these functions are combined to make the 'convertStringForInput' function, which takes a string and
returns the list of list of strings defined in the steps mentioned previously. This allows a direct input for the 'stringListToProgram' function mentioned further down.
-}
lineParser :: (String, [String], String) -> [String]
lineParser (a, b, (x:xs)) = if x /= '\n'
                                  then lineParser (a++[x], b, xs)
                               else lineParser ([], b ++ [a], xs)
lineParser (a, b, []) = b ++ [a]



spaceParser :: (String, [String], [[String]], [String]) -> [[String]]
spaceParser (a, b, c, ((x:xs):l)) = if (x /= ' ') && (x /= '(') && (x /=')')
                                      then spaceParser (a++[x], b, c, (xs):l)
                                    else if (x == '(')
                                      then spaceParser([], b++["("], c, (xs):l)
                                    else if (x == ')') && a /= ""
                                      then spaceParser([], b++[a]++[")"], c, (xs):l)
                                    else if (x == ')') && a == ""
                                      then spaceParser([], b++[")"], c, (xs):l)
                                    else if (x == ' ') && a == ""
                                      then spaceParser([], b, c, (xs):l)
                                    else spaceParser ([], b ++ [a], c, (xs):l)
spaceParser(a, b, c, ([]:l)) = if a /= ""
                                then spaceParser([], [], c ++ [b ++ [a]], l)
                               else spaceParser([], [], c ++ [b], l)
spaceParser (a, b, c, []) = c

spaceParser2 :: (String, String, [String]) -> [String]
spaceParser2((x:l), a, b) = if x == ' '
                                then spaceParser2(l, [], b++[a])
                               else
                                spaceParser2(l, a++[x], b)
spaceParser2([], a, b) = b ++ [a]

compressBrackets:: (String, [String], Int, [String]) -> [String]

compressBrackets(s, b, a, (x:xs)) = if (a == 0) && (x == "(")
                                then compressBrackets(s, b, 2, xs)
                              else if (a == 1) && ( x == ")")
                                then compressBrackets([], b++[s], 0, xs)
                              else if (a == 2) && (x /= "(") && (x /= ")")
                                then compressBrackets(s++x, b, 1, xs)
                              else if (a == 1) && (x /= "(") && (x /= ")")
                                then compressBrackets(s++" "++x, b, 1, xs)
			                        else compressBrackets([], b++[x], 0, xs)
compressBrackets(a, b, _, []) = b

lineParserShort :: String -> [String]
lineParserShort [] = []
lineParserShort x = lineParser([], [], x)

spaceParserShort :: [String] -> [[String]]
spaceParserShort [] = []
spaceParserShort x = spaceParser([], [], [], x)

compressBracketsShort :: [String] -> [String]
compressBracketsShort [] = []
compressBracketsShort x = compressBrackets([], [], 0, x)

convertStringForInput :: String -> [[String]]
convertStringForInput [] = []
convertStringForInput x = map compressBracketsShort (spaceParserShort(lineParserShort x))

{-
Explanation on Conversions
==========================
The last step of the haskell program is the most complicated as the functions no longer solely operate on strings. The first step of parsing a string into a parse tree involves converting the string
format into a 'shaggy' program format. This step uses the 'stringListToProgram' functions, which taken a list of list of strings as an input returns a 'Program' or list of 'Definition's. This function
operates on two inputs, a 'Program' (blank on first input) that will be returned by the function, and the original input list of list of strings. The core operation of this function revolves around the
fact that the amount of input types is limited. Each input type has its own case and appropriate output, for example an input line of "var x" would be parsed to [["var", "x"]] which then fits the first
case "stringListToProgram(p, ["var", x]:l)" and appropriately the function then adds a definition of type 'Var <string>' to the 'Program' after checking that the input var fits the variable format. Each
input type is covered by this function and the output is a 'shaggy' 'Program'.

However, this functon makes use of two other functions in order to correctly parse operations. As an operation works on two expressions, which in turn can also be operations, the 'expandOpr' function
takes the original operation string and converts it to the appropriate 'Expression', for example [["+", "3", "4"]] will output 'Ope Add (Const 3) (Const 4)' which is in 'Expression' format. Further to this,
if any of the expressions are also bracketed expressions then a second operation is called which expands the bracketed expression ('expandBracExpr') in a similar fashion. A shortcoming of this format is that
the parser cannot handle nested bracketed expressions, i.e. "(+ (+ x 4) 4)", however every other format is covered including multiple bracketed expressions, i.e. "- (+ x 4) (+ y 3)".

The final step of the haskell program is to take the list of 'Definition's and generate a parse tree for each. The first function in this step is the 'oprToSubtree' function, which given an expression
returns a tree for the appropriate expression, i.e. oprToSubtree(Ope Add (Const 10) (Const 9)) returns:

          Add
          / \
  Const 10  Const 9

The tree data type works on a 'Node Subtree Subtree' constructor, hence bracketed expressions within an operation can be recursively parsed. The second function in this step is the 'definitionToTree' function
which given a definition input returns a parse tree in 'Expression' format. All of the functions in this program culminate in the 'shaggy' function, which takes an input string (a user's 'shaggy' code) and creates
appropriate parse trees. Some example uses are provided after the show function.
-}


stringListToProgram :: (Program, [[String]]) -> Program
stringListToProgram(p, ["var", x]: l) = if (varCheck x)
                                      then stringListToProgram(p ++ [Var x], l)
                                    else error ("Not a valid variable declaration")
stringListToProgram(p, ["eval", x]: l) = if (varCheck x)
                                      then stringListToProgram(p++[Eval x], l)
                                     else error ("Not a valid evaluation")
stringListToProgram(p, [x, "=", oper, exp1, exp2]: l) = if (varCheck x)
                                                    then stringListToProgram(p ++ [Ass x (expandOpr oper exp1 exp2)], l)
                                                 else error ("Not a valid variable assignment")
stringListToProgram(p, [x, "=", "-", c]: l) = if (varCheck x)
                                              then stringListToProgram(p ++ [Ass x (Minus (stringInt(c)))], l)
                                             else error ("Not a valid variable assignment")
stringListToProgram(p, [x, "=", c]: l) = if (varCheck x)
                                          then stringListToProgram(p ++ [Ass x (Const (stringInt(c)))], l)
                                        else error ("Not a valid variable assignment")
stringListToProgram(p, []) = p

stringToShaggyProgram :: String -> Program
stringToShaggyProgram [] = []
stringToShaggyProgram x = stringListToProgram ([], convertStringForInput(x))

expandBracExpr :: [String] -> Expression
expandBracExpr(["-", expr1, expr2]) = Brac (Ope Subt (expandBracExpr ([expr1])) (expandBracExpr([expr2])))
expandBracExpr(["+", expr1, expr2]) = Brac (Ope Add (expandBracExpr ([expr1])) (expandBracExpr([expr2])))
expandBracExpr(["*", expr1, expr2]) = Brac (Ope Multi (expandBracExpr ([expr1])) (expandBracExpr([expr2])))
expandBracExpr(["-", const]) = Minus (stringInt(const))
expandBracExpr([expr1]) = if varCheck(expr1)
                        then Variable expr1
                      else
                        Const (stringInt(expr1))


expandOpr :: String -> String -> String -> Expression
expandOpr "+" expr1 expr2 = Ope Add (expandBracExpr (spaceParser2(expr1, [], []))) (expandBracExpr (spaceParser2(expr2, [], [])))
expandOpr "*" expr1 expr2 = Ope Multi (expandBracExpr (spaceParser2(expr1, [], []))) (expandBracExpr (spaceParser2(expr2, [], [])))
expandOpr "-" expr1 expr2 = Ope Subt (expandBracExpr (spaceParser2(expr1, [], []))) (expandBracExpr (spaceParser2(expr2, [], [])))

oprToSubtree :: Expression -> Tree Expression
oprToSubtree (Ope oper exp1 exp2) = Node (Op oper) ((oprToSubtree(exp1))) ((oprToSubtree(exp2)))
oprToSubtree (Const x) = Node (Const x) Empty Empty
oprToSubtree (Variable x) = Node(Variable x) Empty Empty
oprToSubtree (Minus x) = Node (Minus x) Empty Empty
oprToSubtree (Brac exp1) = oprToSubtree(exp1)
oprToSubtree (Evalu x) = Node (Evalu x) Empty Empty

definitionToTree :: Definition -> Tree Expression
definitionToTree (Eval x) = oprToSubtree(Evalu x)
definitionToTree (Var x) = oprToSubtree(Variable x)
definitionToTree (Ass x expr1) = Node (Variable x) ((oprToSubtree(expr1))) Empty

shaggy :: String -> [Tree Expression]
shaggy x = map definitionToTree(stringToShaggyProgram x)

{-
The rest of the haskell program is just show expressions used to give a visual representation (in the form of a string) of each of the data types declared at the beginning of the program
-}

showExpr :: Expression -> String
showExpr (Const x) = " " ++ (show x)
showExpr (Variable x) = "Variable(" ++ x++")"
showExpr (Minus x) = " " ++ "-" ++ show x
showExpr (Ope op exp1 exp2) = showOpr(op) ++ showExpr(exp1) ++ showExpr(exp2)
showExpr (Brac exp1) = "(" ++ showExpr(exp1) ++ ")"
showExpr (Op op) = showOpr(op)
showExpr (Evalu x) = "Evaluate Variable(" ++ x ++ ")"

showOpr :: Operand -> String
showOpr Add = "Add "
showOpr Subt = "Subt "
showOpr Multi = "Multi "

showDef :: Definition -> String
showDef (Var x) = "var " ++ x
showDef (Eval x) = "eval " ++ x
showDef (Ass x exp) = x ++ " = " ++ showExpr(exp)

showProg :: Program -> String
showProg(x:xs) = showDef(x) ++ "\n " ++ showProg(xs)
showProg [] = ""

showTree :: Tree Expression -> String
showTree (Node a Empty Empty) = showExpr(a)
showTree (Node a (b) (c)) = showExpr(a) ++ "( L - " ++ showTree(b) ++ " R - " ++ showTree(c) ++ " )"
showTree Empty = "EMPTY"

showShaggy :: [Tree Expression] -> String
showShaggy [] = ""
showShaggy [x]= showTree(x) ++ " -- END"
showShaggy (x:l) = showTree(x) ++ " > " ++showShaggy(l)

{-
Shaggy Examples
===============
'test1'/'test2'/'test3' - An example of the general output of this haskell program. The output format is Node ( L - <Left Subtree> R - <Right Subtree>) > Next Tree
'stringTest1'/'stringTest2' - An example use of the parser functions
charToIntTest - An example conversion from string integer to int
-}

test1 = showShaggy(shaggy "var x\nx = - 42\nvar y\ny = + x 1\neval y")
test2 = showShaggy(shaggy "var x\nx = 4\nvar y\ny = - 3 x\nx = + (- y 1) 2\neval x")
test3 = showShaggy(shaggy "var x\nx = 4\nvar y\ny = - 3 x\nx = + (- y 1) (+ 1 1)")
stringTest1 = convertStringForInput "var x\nx = -42"
stringTest2 = convertStringForInput "var x\nx = - (- 3 4) 10"
charToIntTest = stringInt "365"
