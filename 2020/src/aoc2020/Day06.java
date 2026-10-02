package aoc2020;

import java.util.Arrays;
import java.util.logging.Logger;

/**
 * Day 06: Custom Customs.
 * <p>
 * Processes customs declaration forms for groups of passengers.
 * Part one counts questions to which anyone in each group answered "yes" and computes the total sum.
 * Part two counts questions to which everyone in each group answered "yes" and computes the total sum.
 */
public class Day06 {
    private Day06() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day06.class.getName());

    /**
     * Calculates the sum of counts of questions to which anyone in each group answered "yes".
     * <p>
     * For each group, all answers are concatenated and distinct characters are counted to determine the union
     * of positive answers.
     *
     * @param groups a 2D array where each element represents a group containing individual passenger response strings
     * @return the total sum of "yes" answer counts across all groups
     */
    private static int solvePartOne(String[][] groups) {
        return Arrays.stream(groups).mapToInt(group -> String.join("", group).chars().distinct().toArray().length).sum();
    }

    /**
     * Calculates the sum of counts of questions to which everyone in each group answered "yes".
     * <p>
     * For each group, filters distinct characters from the first person's answers where every person in the group
     * contains that character (the intersection of positive answers).
     *
     * @param groups a 2D array where each element represents a group containing individual passenger response strings
     * @return the total sum of unanimous "yes" answer counts across all groups
     */
    private static int solvePartTwo(String[][] groups) {
        return Arrays.stream(groups).mapToInt(group -> (int) group[0].chars().filter(answer -> Arrays.stream(group).allMatch(person -> person.indexOf(answer) >= 0)).distinct().count()).sum();
    }

    public static void run() {
        String input = Utils.getInputData(6);
        String[][] groups = Arrays.stream(input.strip().split("\\R\\R")).map(group -> group.split("\\R")).toArray(String[][]::new);

        LOGGER.info(() -> "Part One: " + solvePartOne(groups));
        LOGGER.info(() -> "Part Two: " + solvePartTwo(groups));
    }
}
