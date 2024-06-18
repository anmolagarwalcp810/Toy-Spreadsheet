# Toy Spreadsheet OCaml
## How to Run
```shell
make
./spreadsheet sheet1.csv commands.txt
```

## Clean
```shell
make clean
```

## Description
Given initial spreadsheet and list of commands as input, commands are read by lexer and processed based on grammar specified by yacc to perform operations like addition, subtraction, average, min, max over range of cells in the spreadsheet. Spreadsheet is printed to console after each operation.

## Sample Output
Initial spreadsheet:
```shell
1.000000        10.000000       100.000000      -               -               -               -               -
2.000000        20.000000       200.000000      -               -               -               -               -
3.000000        30.000000       300.000000      -               -               -               -               -
4.000000        40.000000       14.000000       -               -               -               -               -
5.000000        -               15.000000       -               -               -               -               -
-               60.000000       16.000000       -               -               -               -               -
7.000000        70.000000       17.000000       -               -               -               -               -
8.000000        80.000000       -               -               -               -               -               -
9.000000        90.000000       1.000000        9.000000        -               -               -               -
10.000000       -               20.000000       -               -               -               -               -
11.000000       110.000000      21.000000       -               -               -               -               -
```

Final spreadsheet after performing operations specified in `commands.txt`:
```shell
1.000000        10.000000       100.000000      5.000000        100.000000      110.000000      -               -
2.000000        20.000000       200.000000      20.000000       400.000000      410.000000      -               -
3.000000        30.000000       300.000000      45.000000       900.000000      910.000000      -               -
4.000000        40.000000       14.000000       -               -               -               -               -
5.000000        -               15.000000       -               -               -               -               -
-               60.000000       16.000000       -               -               -               -               -
7.000000        70.000000       17.000000       -               -               -               -               -
8.000000        80.000000       -               -               -               -               -               -
9.000000        90.000000       1.000000        9.000000        -               -               -               -
10.000000       -               20.000000       -               -               -               -               -
11.000000       110.000000      21.000000       -               -               -               -               -
```
