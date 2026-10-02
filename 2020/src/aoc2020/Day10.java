package aoc2020;

import java.util.Arrays;
import java.util.logging.Logger;

/**
 * Day 10: Adapter Array.
 * <p>
 * Connects charging adapters to bridge joltage differences between the charging outlet and device.
 * Part one calculates the product of 1-jolt and 3-jolt differences when connecting all adapters in sequence.
 * Part two determines the total number of distinct valid adapter arrangements that can connect the outlet to the device.
 */
public class Day10 {
    private Day10() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day10.class.getName());

    /**
     * Calculates the product of 1-jolt differences and 3-jolt differences when chaining all adapters in ascending order.
     * <p>
     * Starts at the charging outlet (0 jolts) and steps through sorted adapter ratings, tracking differences of 1 and 3 jolts.
     * The built-in device adapter is always 3 jolts higher than the highest adapter.
     *
     * @param numbers the array of adapter joltage ratings
     * @return the product of the number of 1-jolt differences and 3-jolt differences
     */
    private static int solvePartOne(int[] numbers) {
        int[] adapters = Arrays.copyOf(numbers, numbers.length);
        Arrays.sort(adapters);

        int oneJoltDifferences = 0;
        int threeJoltDifferences = 1;
        int currentJoltage = 0;

        for (int adapter : adapters) {
            int difference = adapter - currentJoltage;
            if (difference == 1) oneJoltDifferences++;
            else if (difference == 3) threeJoltDifferences++;

            currentJoltage = adapter;
        }

        return oneJoltDifferences * threeJoltDifferences;
    }

    /**
     * Calculates the total number of distinct valid arrangements of adapters from the charging outlet to the device.
     * <p>
     * Adds the charging outlet (0 jolts) and the device's built-in adapter (max + 3 jolts) to the sorted list, then uses
     * dynamic programming (tabulation) to compute the number of paths to each adapter from reachable predecessor adapters.
     *
     * @param numbers the array of adapter joltage ratings
     * @return the total number of distinct valid adapter configurations
     */
    private static long solvePartTwo(int[] numbers) {
        int[] adapters = new int[numbers.length + 2];
        adapters[0] = 0;

        System.arraycopy(numbers, 0, adapters, 1, numbers.length);
        Arrays.sort(adapters, 1, adapters.length - 1);
        adapters[adapters.length - 1] = adapters[adapters.length - 2] + 3;

        long[] ways = new long[adapters.length];
        ways[0] = 1;

        for (int i = 1; i < adapters.length; i++) {
            for (int j = i - 1; j >= 0 && adapters[i] - adapters[j] <= 3; j--) {
                ways[i] += ways[j];
            }
        }

        return ways[ways.length - 1];
    }

    public static void run() {
        String input = Utils.getInputData(10);
        int[] numbers = Arrays.stream(input.strip().split("\\R")).mapToInt(Integer::parseInt).toArray();

        LOGGER.info(() -> "Part One: " + solvePartOne(numbers));
        LOGGER.info(() -> "Part Two: " + solvePartTwo(numbers));
    }
}
