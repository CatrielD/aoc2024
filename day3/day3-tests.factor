
: test-string ( -- string )
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" ;
{ 161 } [ test-string scan-memory calculate ] unit-test

: part2-test-string ( -- string )
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" ;
! This time, the sum of the results is 48 (2*4 + 8*5).

{ 48 } [ part2-test-string new-scan-memory tag-memory-vector new-calculate ] unit-test 
