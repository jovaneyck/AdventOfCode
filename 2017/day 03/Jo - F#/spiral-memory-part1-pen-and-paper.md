//example: x = 23
// square?
// 3? nope, 9 < 23. 
// 5? (= next uneven) YUP 23 < 25

//top-right element square 5 = 25. width square 5 = 5 (duh)

//what's the position of x = 23 in this square?
//first element = 9 + 1 = 10

//first 4 (5 - 1) elements are in the "right column" (10, 11, 12, 13)
//next 4 elements are in the "top row" (14, 15, 16, 17)
//next 4 elements are in the "left column" (18, 19, 20, 21)
//next 4 elements are in "bottom row" (22, 23 FOUND YOU!)

//Coordinate:
//Bottom row: y coordinate = n - 1th even number. 5 = 3 - 1 = (-2)
//It's the second element in that row, starts at (n - 1 / 2) + 1 = (5 - 1) / 2 + 1= (-1), so this element is (0)
//Coordinate: (0, -2) // Hamming: abs(x) + abs(y) = 2

//For my input: x = 289326
//In square: 539
//top-right element in square 539: 290521, width square = 539
//pown 539 2
//position in this square?
//first element = 288370
//1 + pown 537 2
//first 538 (539 -1) elements are in the "right column": 288370 + 537 = 288907
//So, it's in the top row, 419 offset to the top right!

//Top right coordinate = nth uneven number of the square = (269, 269)
// Offset 419 to the left:
//(-150, 269)

//Hamming distance: 