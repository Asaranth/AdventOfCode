package aoc2020;

import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

/**
 * Day 02: Password Philosophy.
 * <p>
 * Parses password policies and counts how many passwords are valid.
 * Part one treats the two numbers as the minimum and maximum allowed occurrences of the required character.
 * Part two treats the two numbers as one-based positions and requires the character to appear in exactly one of them.
 */
public class Day02 {
    private Day02() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day02.class.getName());

    /**
     * Represents one password policy and its associated password.
     *
     * @param min the minimum occurrence count for part one, or the first one-based position for part two
     * @param max the maximum occurrence count for part one, or the second one-based position for part two
     * @param character the character required by the policy
     * @param password the password to validate
     */
    private record PasswordPolicy(int min, int max, char character, String password) { }

    /**
     * Parses the raw puzzle input into password policy records.
     *
     * @param input the raw puzzle input
     * @return the parsed password policies
     */
    private static List<PasswordPolicy> parseInput(String input) {
        return Arrays.stream(input.strip().split("\\R")).map(Day02::parseLine).toList();
    }

    /**
     * Parses a single input line into a password policy record.
     * <p>
     * Each line is formatted like {@code 1-3 a: abcde}, where {@code 1-3} is the numeric policy,
     * {@code a} is the required character, and {@code abcde} is the password.
     *
     * @param line one line of puzzle input
     * @return the parsed password policy
     */
    private static PasswordPolicy parseLine(String line) {
        String[] parts = line.split(": ");
        String policy = parts[0];
        String password = parts[1];

        String[] policyParts = policy.split(" ");
        String[] rangeParts = policyParts[0].split("-");

        int min = Integer.parseInt(rangeParts[0]);
        int max = Integer.parseInt(rangeParts[1]);
        char character = policyParts[1].charAt(0);

        return new PasswordPolicy(min, max, character, password);
    }

    /**
     * Counts passwords where the required character appears at least the minimum number of times
     * and at most the maximum number of times.
     *
     * @param policies the parsed password policies
     * @return the number of valid passwords for part one
     */
    private static int solvePartOne(List<PasswordPolicy> policies) {
        return (int) policies.stream().filter(policy -> {
            long count = policy.password().chars().filter(c -> c == policy.character()).count();
            return count >= policy.min() && count <= policy.max();
        }).count();
    }

    /**
     * Counts passwords where the required character appears in exactly one of the two specified positions.
     * <p>
     * Policy positions are one-based, so each position is converted to a zero-based Java string index before checking.
     *
     * @param policies the parsed password policies
     * @return the number of valid passwords for part two
     */
    private static int solvePartTwo(List<PasswordPolicy> policies) {
        return (int) policies.stream().filter(policy -> {
            boolean firstPositionMatches = policy.password().charAt(policy.min() - 1) == policy.character();
            boolean secondPositionMatches = policy.password().charAt(policy.max() - 1) == policy.character();
            return firstPositionMatches != secondPositionMatches;
        }).count();
    }

    public static void run() {
        String input = Utils.getInputData(2);
        List<PasswordPolicy> policies = parseInput(input);

        LOGGER.info(() -> "Part One: " + solvePartOne(policies));
        LOGGER.info(() -> "Part Two: " + solvePartTwo(policies));
    }
}