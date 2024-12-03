USING: kernel vectors math arrays peg sequences.deep sequences tools.test  io.encodings.ascii io.files io namespaces prettyprint ;
IN: day3.factor

! --- Day 3: Mull It Over ---
!
! "Our computers are having issues, so I have no idea if we have any Chief Historians in stock! You're welcome to check the warehouse, though," says the mildly flustered shopkeeper at the North Pole Toboggan Rental Shop. The Historians head out to take a look.
!
! The shopkeeper turns to you. "Any chance you can see why our computers are having issues again?"
!
! The computer appears to be trying to run a program, but its memory (your puzzle input) is corrupted. All of the instructions have been jumbled up!
!
! It seems like the goal of the program is just to multiply some numbers. It does that with instructions like mul(X,Y), where X and Y are each 1-3 digit numbers. For instance, mul(44,46) multiplies 44 by 46 to get a result of 2024. Similarly, mul(123,4) would multiply 123 by 4.
!
! However, because the program's memory has been corrupted, there are also many invalid characters that should be ignored, even if they look like part of a mul instruction. Sequences like mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do nothing.
!
! For example, consider the following section of corrupted memory:
!
! xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
! (mul(2,4) mul(5,5) mul(11,8) mul(8,5))
! Only the four highlighted sections are real mul instructions. Adding up the result of each instruction produces 161 (2*4 + 5*5 + 11*8 + 8*5).
!
! Scan the corrupted memory for uncorrupted mul instructions. What do you get if you add up all of the results of the multiplications?
!

! tiene que haber una mejor forma de parsear un número
! parsing number:
: to-digit ( char -- number ) 48 - ;

: a-digit ( -- parser ) CHAR: 0 CHAR: 9 range [ to-digit ] action ;

: pair-to-stack ( Vec<x,y> -- y x  )
    dup 1 swap nth swap 0 swap nth ;

: triple-to-stack ( Vec<x,y,z> -- z y x )
    dup 2 swap nth swap dup 1 swap nth swap 0 swap nth ;

: a-number ( -- parser )
    a-digit a-digit a-digit 3seq
            [ triple-to-stack 100 * swap 10 * + + ] action
    a-digit a-digit 2seq
            [ pair-to-stack 10 * + ] action
    a-digit
    3array choice ;

: 4seq ( parser parser parser parser -- parser )
    2seq 2seq 2seq ;

: 5seq ( parser parser parser parser parser -- parser )
    4seq 2seq ;

! parsing operations
: mul-parser ( -- parser )
    "mul(" token hide
     a-number
     "," token hide
     a-number
     ")" token hide
     5seq ;

: any-char ( -- parser ) [ drop t ] satisfy ;

: memory-parser ( -- parser )
    mul-parser any-char hide 2array choice repeat1 ;

: scan-memory ( string -- vector )
    memory-parser parse [ vector? ] filter ;

: calculate ( vector -- number )
    0 [ flatten pair-to-stack * + ] reduce ;

! read the file

: process-memory-file ( filepath -- number )
    ascii file-contents scan-memory calculate ;

: solve-part-1 ( filerelpath -- )
    "the first part yields: " print
    process-memory-file pprint ;


! --- Part Two ---
!
! As you scan through the corrupted memory, you notice that some of the conditional statements are also still intact. If you handle some of the uncorrupted conditional statements in the program, you might be able to get an even more accurate result.
!
! There are two new instructions you'll need to handle:
!
!     The do() instruction enables future mul instructions.
!     The don't() instruction disables future mul instructions.
!
! Only the most recent do() or don't() instruction applies. At the beginning of the program, mul instructions are enabled.
!
! For example:
!
! xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
!
! This corrupted memory is similar to the example from before, but this time the mul(5,5) and mul(11,8) instructions are disabled because there is a don't() instruction before them. The other mul instructions function normally, including the one at the end that gets re-enabled by a do() instruction.
!
! This time, the sum of the results is 48 (2*4 + 8*5).
!
! Handle the new instructions; what do you get if you add up all of the results of just the enabled multiplications?
!

: do-dont-parser ( -- parser )
    "do()" token [ drop t ] action
    "don't()" token [ drop f ] action
    2array choice ;

: new-memory-parser ( -- parser )
    mul-parser do-dont-parser
    any-char hide
    3array choice repeat1 ;

: new-scan-memory ( string -- vector )
    new-memory-parser parse [ dup vector? swap boolean? or ] filter
    [ dup vector? [ flatten ] [ ] if ] map ;

: tag-memory-vector ( vector -- vector )
    t "do" set-global
    [ dup vector?
      [ "do" get-global 2array ]
      [ "do" set-global t ]
      if
    ]
    map [ array? ] filter ;

: pair-fst ( pair -- x ) 0 swap nth ;
: pair-snd ( pair -- y ) 1 swap nth ;

: pair-prod ( vector -- number )
    dup pair-fst swap pair-snd * ;

: new-calculate ( vector -- number )
    0 [ dup pair-snd [ pair-fst ] [ drop V{ 0 0 } ] if pair-prod + ] reduce ;

: new-process-memory-file ( filepath -- number )
    ascii file-contents new-scan-memory tag-memory-vector new-calculate ;

: solve-part-2 ( filerelpath -- )
    "the second part yields: " print
    new-process-memory-file pprint ;

: run-aoc ( filerelpath -- )
    dup
    solve-part-1
    nl
    solve-part-2
    nl ;
