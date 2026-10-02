package aoc2020;

import java.util.Arrays;
import java.util.logging.Logger;

/**
 * Day 01: Report Repair.
 * <p>
 * Finds combinations of numbers that sum to 2020 and returns the product of those numbers.
 * Part one searches for two numbers, while part two searches for three numbers.
 */
public class Day01 {
    private Day01() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day01.class.getName());

    /**
     * Recursively searches for a fixed number of values that sum to the target.
     * <p>
     * Each recursive call chooses one number, subtracts it from the remaining target, and reduces the remaining count by one.
     * The start index advances so the same number is not reused and duplicate orderings are skipped.
     *
     * @param numbers the list of expense report values
     * @param target the remaining sum to find
     * @param count how many numbers still need to be selected
     * @param startIndex the index to start searching from
     * @return the product of the matching numbers, or 0 if no match is found
     */
    private static int findProductForSum(int[] numbers, int target, int count, int startIndex) {
        if (count == 0) return target == 0 ? 1 : 0;

        for (int i = startIndex; i < numbers.length; i++) {
            int product = findProductForSum(numbers, target - numbers[i], count - 1, i + 1);
            if (product != 0) return numbers[i] * product;
        }

        return 0;
    }

    public static void run() {
        String input = Utils.getInputData(1);
        int[] numbers = Arrays.stream(input.strip().split("\\R")).mapToInt(Integer::parseInt).toArray();

        LOGGER.info(() -> "Part One: " + findProductForSum(numbers, 2020, 2, 0));
        LOGGER.info(() -> "Part Two: " + findProductForSum(numbers, 2020, 3, 0));
    }
}