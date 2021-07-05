# Scheme

###### Ascii-art:
(display ((ascii-art <ins>width</ins> <ins>height</ins> <ins>chars</ins>) <ins>img</ins>)),
where <ins>width</ins> and <ins>height</ins> define size of rectangle from image (in pixels), 
you want to be converted into some char (@, #, etc..) from list of <ins>chars</ins>.
Result of arnold.png (<ins>img</ins> parameter) can be seen in arnold.txt, 
params used: width = 8, height = 16, chars=“ .,:;ox%#@”


###### SVGen:
(display (execute <ins>width1</ins> <ins>height1</ins> <ins>example</ins> '(<ins>call</ins> <ins>width2</ins> <ins>height2</ins>))),
where <ins>width1</ins> and <ins>height1</ins> are written in intial SVG print: `<svg width="400" height="400">`,
<ins>example</ins> is list containing function definitions,
<ins>call</ins> is initial call of some function from example list, and <ins>width2</ins>, <ins>height2</ins> are its parameters.
Result of test 4, 5 can be seen in raw .txt form and also in .png


###### Matrix-rotation:
(rotationInstructions <ins>m</ins> <ins>instr</ins>),
where <ins>m</ins> is matrix in 2d array e.g. (define m '((1 2 3)(4 5 6))),
<ins>instr</ins> is list containing instructions (direction, index) of 4 types of directions:
1. Up, rotates column,   (define instr '((up 1)))    -> '((1 5 3) (4 2 6))
2. Down, rotates column, (define instr '((down 0)))  -> '((4 2 3) (1 5 6))
3. Left, rotates row,    (define instr '((left 0)))  -> '((2 3 1) (4 5 6))
4. Right, rotates row,   (define instr '((right 1))) -> '((1 2 3) (6 4 5))
