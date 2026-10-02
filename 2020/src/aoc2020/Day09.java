package aoc2020;

import java.util.Arrays;
import java.util.logging.Logger;

/**
 * Day 09: Encoding Error.
 * <p>
 * Decodes an eXchange-Masking Addition System (XMAS) data stream.
 * Part one finds the first number that is not the sum of two of the previous 25 numbers.
 * Part two finds a contiguous range of at least two numbers that sum to the invalid number from part one and adds the smallest and largest numbers in that range.
 */
public class Day09 {
    private Day09() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day09.class.getName());

    /**
     * Finds the first number in the sequence that is not the sum of two distinct numbers from the previous preamble elements.
     * <p>
     * Iterates through sliding windows of length {@code rangeSize} and checks whether any pair of numbers within
     * the window preamble sums to the target number at the end of the window.
     *
     * @param numbers the array of sequence numbers in the data stream
     * @param rangeSize the size of the sliding window including the target value (preamble length + 1)
     * @return the first number that violates the encoding rule
     * @throws IllegalArgumentException if no invalid number is found
     */
    private static long solvePartOne(long[] numbers, int rangeSize) {
        for (int start = 0; start + rangeSize <= numbers.length; start++) {
            long target = numbers[start + rangeSize - 1];
            boolean found = false;

            for (int i = start; i < start + rangeSize - 1; i++) {
                for (int j = i + 1; j < start + rangeSize - 1; j++) {
                    if (numbers[i] + numbers[j] == target) {
                        found = true;
                        break;
                    }
                }

                if (found) {
                    break;
                }
            }

            if (!found) {
                return target;
            }
        }

        throw new IllegalArgumentException("Input is invalid: no invalid number found");
    }

    /**
     * Finds a contiguous range of at least two numbers that sum to the target invalid number, returning the sum of the smallest and largest numbers in that range.
     * <p>
     * Uses a two-pointer sliding window to expand and shrink a contiguous subarray until its sum equals the target,
     * then determines the minimum and maximum values within that range.
     *
     * @param numbers the array of sequence numbers in the data stream
     * @param target the invalid number identified in part one
     * @return the sum of the minimum and maximum numbers within the contiguous range
     * @throws IllegalArgumentException if no contiguous range summing to the target is found
     */
    private static long solvePartTwo(long[] numbers, long target) {
        int start = 0;
        int end = 0;
        long sum = 0;

        while (end < numbers.length) {
            sum += numbers[end];

            while (sum > target && start < end) {
                sum -= numbers[start];
                start++;
            }

            if (sum == target && end - start >= 1) {
                long min = numbers[start];
                long max = numbers[start];

                for (int i = start + 1; i <= end; i++) {
                    min = Math.min(min, numbers[i]);
                    max = Math.max(max, numbers[i]);
                }

                return min + max;
            }

            end++;
        }

        throw new IllegalArgumentException("No contiguous range found.");
    }

    public static void run() {
        String input = Utils.getInputData(9);
        long[] numbers = Arrays.stream(input.strip().split("\\R")).mapToLong(Long::parseLong).toArray();

        long invalidNumber = solvePartOne(numbers, 26);

        LOGGER.info(() -> "Part One: " + invalidNumber);
        LOGGER.info(() -> "Part Two: " + solvePartTwo(numbers, invalidNumber));
    }
}
