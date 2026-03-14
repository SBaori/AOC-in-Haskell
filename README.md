# Run the following commands in the aoc year directory

```bash
  cd <year>
```

## Execute
**Note: Make sure you have placed your input.txt files in their respective directories.**

Provide day as an argument

```bash
  cabal run aoc.cabal <day>
```

## Run in Repl
```bash
  cabal repl
```

## Execution Times
**Note: CPU - Intel Core Ultra 9 285H**
```bash
2023 - 8.416 sec
2025 - 2.403 sec
```

## Script for Calculating Execution Times
```bash
#!/bin/bash

taskset -p -c 2 $$ > /dev/null

sum=0
runs=10

for i in $(seq 1 $runs); do
	t=$((/usr/bin/time -f "%U %S" cabal run aoc.cabal $*) 2>&1 | tail -1 | awk '{print $1 + $2}')
	sum=$(echo "$sum + $t" | bc)
	echo "$i: $t"
	sleep 1
done

echo "scale=3; $sum / $runs" | bc
```
