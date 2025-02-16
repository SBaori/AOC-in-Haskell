# Run the following commands in the aoc year directory

```bash
  cd <year>
```

## Execute
Provide day as an argument

```bash
  cabal run aoc.cabal <day>
```

## Run in Repl
```bash
  cabal repl
```

## Execution Times
**Note: Single Thread on i5 11th gen unplugged**
```bash
Day 1   | 0.065s |   -
Day 2   | 0.066s |   -
Day 3   | 0.084s |   -
Day 4   | 0.079s |   -
Day 5   | 0.066s |   -
Day 6   | 0.064s |   -
Day 7   | 0.086s |   -
Day 8   | 0.077s |   -
Day 9   | 0.080s |   -
Day 10  | 0.087s |   -
Day 11  | 0.068s |   -
Day 12  | 0.153s |   -
Day 13  | 0.071s |   -
Day 14  | 0.242s |   -
Day 15  | 0.106s |   -
Day 16  | 1.770s |   -
Day 17  | 5.105s | 3.582s
Day 18  | 0.070s |   -
Day 19  | 0.080s |   -
Day 20  | 0.223s |   -
Day 21  | 0.100s |   -
Day 22  | 0.430s |   -
Day 23  | 6.697s | 3.047s
Day 24  | 0.877s |   -
```

## Script for Calculating Execution Times
```bash
sum=0
for i in {1..10}; do
	t=$((time cabal run aoc.cabal $1) 2>&1 | grep real | awk '{split($2, a, "m"); print a[1] * 60 + a[2]}')
	sum=$(echo "$sum + $t" | bc)
	echo $t
done

echo "scale=3; $sum / 10" | bc
```