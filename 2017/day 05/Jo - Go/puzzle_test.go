package puzzle

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func ReadFile(path string) string {
	b, err := ioutil.ReadFile(path)
	if err != nil {
		fmt.Print(err)
	}

	return string(b)
}

func ParseNumbers(lines []string) []int {
	parsed := make([]int, len(lines))

	for i, raw := range lines {
		p, _ := strconv.Atoi(raw)
		parsed[i] = p
	}

	return parsed
}

func Run(offsetter func(int) int, offsets []int) int {
	steps := 0
	location := 0
	for {
		steps++
		offset := offsets[location]
		offsets[location] = offsetter(offset)
		location += offset

		if location < 0 || location >= len(offsets) {
			return steps
		}
	}
}

func PartOne(input []int) int {
	offsetter := func(o int) int { return o + 1 }
	return Run(offsetter, input)
}

func PartTwo(input []int) int {
	offsetter := func(o int) int {
		if o >= 3 {
			return o - 1
		}
		return o + 1
	}

	return Run(offsetter, input)
}

func TestPartOneExample(t *testing.T) {
	exampleInput := []int{0, 3, 0, 1, -3}
	assert.Equal(t, 5, PartOne(exampleInput))
}

func TestPartTwoExample(t *testing.T) {
	exampleInput := []int{0, 3, 0, 1, -3}
	assert.Equal(t, 10, PartTwo(exampleInput))
}

func TestCalculateSolutions(t *testing.T) {
	text := ReadFile("input.txt")
	lines := strings.Split(text, "\r\n")
	parsed := ParseNumbers(lines)

	//fmt.Printf("Part 1: %v", PartTwo(parsed))
	fmt.Printf("Part 1: %v", PartTwo(parsed))
	fmt.Println()
}
