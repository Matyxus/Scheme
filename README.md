# Scheme

Ascii-art should be used in following way:
(display ((ascii-art width height chars) img)),
where width and height define size of rectangle from image (in pixels), 
you want to be converted into some char (@, #, etc..) from list of chars.
Result of arnold.png (img parameter) can be seen in arnold.txt, 
params used: width = 8, height = 16, chars=“ .,:;ox%#@”


SVGen should be used in following way:
(display (execute width1 height1 example '(call width2 height2))),
where width1 and height1 are written in intial SVG print: "<svg width="400" height="400">",
example is list containing function definitions,
call is initial call of some function from example list, and width2, height2 are its parameters.
Result of test 4, 5 can be seen in raw .txt form and also in .png


Matrix-rotation should be used in following way:
(rotationInstructions m instr),
where m is matrix in 2d array e.g. '((1 2 3)(4 5 6)(7 8 9)),
instr is list containing instructions of 4 types of rotations:
Up, Down, Left, Right, paired with index of what row / column should
be rotated, (Up, Down rotates columns, while Left, Right rotates rows),
first row and column are both indexed at 0.
e.g. (define instr '((left 1)(down 2)(right 2))).




