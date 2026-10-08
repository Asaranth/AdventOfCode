package aoc2020;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Day 15: Rambunctious Recitation.
 * <p>
 * Simulates a memory recitation game where each spoken number depends on the turn history of previously spoken numbers.
 * Part one determines the 2020th number spoken.
 * Part two determines the 30,000,000th number spoken using the same game rules.
 */
public class Day15 {
    private Day15() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day15.class.getName());

    /**
     * Simulates the memory game for a specified number of turns starting with the initial sequence.
     * <p>
     * Tracks the most recent turn each number was spoken using a hash map. For each subsequent turn,
     * if the previous number was spoken before, the difference between the last two turns is spoken;
     * otherwise, 0 is spoken.
     *
     * @param numbers the starting sequence of numbers
     * @param turns the target turn count to simulate
     * @return the number spoken on the specified target turn
     */
    private static int solve(int[] numbers, int turns) {
        Map<Integer, Integer> lastSpoken = new HashMap<>();

        for (int i = 0; i < numbers.length - 1; i++) lastSpoken.put(numbers[i], i + 1);

        int lastNumber = numbers[numbers.length - 1];

        for (int turn = numbers.length + 1; turn <= turns; turn++) {
            int lastSpokenTurn = turn - 1;
            Integer previousTurn = lastSpoken.get(lastNumber);

            int nextNumber;
            if (previousTurn == null) nextNumber = 0;
            else nextNumber = lastSpokenTurn - previousTurn;

            lastSpoken.put(lastNumber, lastSpokenTurn);
            lastNumber = nextNumber;
        }

        return lastNumber;
    }

    public static void run() {
        String input = Utils.getInputData(15);
        int[] numbers = Arrays.stream(input.strip().split(",")).mapToInt(Integer::parseInt).toArray();

        LOGGER.info(() -> "Part One: " + solve(numbers, 2020));
        LOGGER.info(() -> "Part Two: " + solve(numbers, 30000000));
    }
}
