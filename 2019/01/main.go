// https://adventofcode.com/2019/day/1
package main

import (
	"bufio"
	"log"
	"math"
	"os"
	"strconv"
)

func main() {
	inputs := loadInputs("./input.txt")
	sum := getFuelRequirementsSum(inputs)
	log.Println(sum)
}

func getRequiredFuel(mass int) int {
	return int(math.Floor(float64(mass)/3.0)) - 2
}

func getRecursiveFuel(mass int) int {
	fuel := getRequiredFuel(mass)
	if fuel > 0 {
		return fuel + getRecursiveFuel(fuel)
	}

	return 0
}

func getFuelRequirementsSum(inputs []int) int {
	sum := 0
	for _, v := range inputs {
		sum += getRecursiveFuel(v)
	}

	return sum
}

func loadInputs(path string) []int {
	f, err := os.Open(path)
	if err != nil {
		log.Fatalln("Couldn't open input file")
	}
	defer f.Close()

	inputs := make([]int, 0)
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		value, err := strconv.Atoi(scanner.Text())
		if err != nil {
			log.Fatalln("One of the inputs isn't an int")
		}
		inputs = append(inputs, value)
	}

	return inputs
}
