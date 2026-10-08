package aoc2020;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

/**
 * Day 16: Ticket Translation.
 * <p>
 * Parses ticket field rules, your ticket, and nearby tickets.
 * Part one calculates the ticket scanning error rate by summing all nearby ticket values
 * that do not match any field rule.
 * Part two determines field order and multiplies your ticket's departure field values.
 */
public class Day16 {
    private Day16() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day16.class.getName());

    /**
     * Represents an inclusive integer range [min, max].
     *
     * @param min the minimum allowed value (inclusive)
     * @param max the maximum allowed value (inclusive)
     */
    private record Range(int min, int max) {
        /**
         * Checks whether the specified value falls within this range.
         *
         * @param value the value to test
         * @return {@code true} if the value is within [min, max], {@code false} otherwise
         */
        private boolean contains(int value) {
            return value >= min && value <= max;
        }
    }

    /**
     * Represents a ticket field validation rule with two inclusive valid ranges.
     *
     * @param name the field name
     * @param firstRange the first valid range for this field
     * @param secondRange the second valid range for this field
     */
    private record Rule(String name, Range firstRange, Range secondRange) {
        /**
         * Checks whether the specified value satisfies either of this rule's valid ranges.
         *
         * @param value the value to test
         * @return {@code true} if the value is valid according to this rule, {@code false} otherwise
         */
        private boolean isValid(int value) {
            return firstRange.contains(value) || secondRange.contains(value);
        }
    }

    /**
     * Represents the parsed ticket notes containing rules, your ticket, and nearby tickets.
     *
     * @param rules the list of field validation rules
     * @param yourTicket the field values of your ticket
     * @param nearbyTickets the list of nearby tickets and their field values
     */
    private record Notes(List<Rule> rules, int[] yourTicket, List<int[]> nearbyTickets) {}

    /**
     * Parses the puzzle notes into field rules, your ticket values, and nearby ticket values.
     *
     * @param input the raw puzzle input
     * @return parsed ticket notes
     */
    private static Notes parseNotes(String input) {
        String[] sections = input.strip().split("\\R\\s*\\R");

        if (sections.length != 3)
            throw new IllegalArgumentException("Expected rules, your ticket, and nearby tickets sections.");

        List<Rule> rules = Arrays.stream(sections[0].split("\\R"))
                .filter(line -> !line.isBlank())
                .map(line -> {
                    String[] nameAndRanges = line.split(": ");
                    String[] ranges = nameAndRanges[1].split(" or ");
                    String[] firstBounds = ranges[0].split("-");
                    String[] secondBounds = ranges[1].split("-");

                    Range firstRange = new Range(Integer.parseInt(firstBounds[0]), Integer.parseInt(firstBounds[1]));
                    Range secondRange = new Range(Integer.parseInt(secondBounds[0]), Integer.parseInt(secondBounds[1]));

                    return new Rule(nameAndRanges[0], firstRange, secondRange);
                })
                .toList();

        int[] yourTicket = Arrays.stream(sections[1].split("\\R")).filter(line -> !line.isBlank()).skip(1).findFirst()
                .map(line -> Arrays.stream(line.split(",")).mapToInt(Integer::parseInt).toArray())
                .orElseThrow(() -> new IllegalArgumentException("Missing your ticket values."));

        List<int[]> nearbyTickets = Arrays.stream(sections[2].split("\\R")).filter(line -> !line.isBlank()).skip(1)
                .map(line -> Arrays.stream(line.split(",")).mapToInt(Integer::parseInt).toArray()).toList();

        return new Notes(rules, yourTicket, nearbyTickets);
    }

    /**
     * Calculates the ticket scanning error rate by summing all invalid values on nearby tickets.
     *
     * @param notes parsed ticket notes
     * @return the ticket scanning error rate
     */
    private static int solvePartOne(Notes notes) {
        return notes.nearbyTickets.stream().flatMapToInt(Arrays::stream).filter(value -> notes.rules.stream().noneMatch(rule -> rule.isValid(value))).sum();
    }

    /**
     * Determines the field order from valid nearby tickets and multiplies the values on your ticket
     * for every field whose name starts with {@code departure}.
     *
     * @param notes parsed ticket notes
     * @return the product of all departure field values on your ticket
     */
    private static long solvePartTwo(Notes notes) {
        List<int[]> validTickets = notes.nearbyTickets.stream()
                .filter(ticket -> Arrays.stream(ticket).allMatch(value -> notes.rules.stream().anyMatch(rule -> rule.isValid(value))))
                .toList();

        List<Set<Rule>> candidates = new ArrayList<>();
        for (int index = 0; index < notes.yourTicket.length; index++) {
            Set<Rule> possibleRules = new HashSet<>(notes.rules);

            for (int[] ticket : validTickets) {
                int value = ticket[index];
                possibleRules.removeIf(rule -> !rule.isValid(value));
            }

            candidates.add(possibleRules);
        }

        Rule[] resolvedRules = new Rule[notes.yourTicket.length];

        while (Arrays.stream(resolvedRules).anyMatch(rule -> rule == null)) {
            boolean resolvedAnyRule = false;

            for (int index = 0; index < candidates.size(); index++) {
                Set<Rule> possibleRules = candidates.get(index);

                if (resolvedRules[index] == null && possibleRules.size() == 1) {
                    Rule resolvedRule = possibleRules.iterator().next();
                    resolvedRules[index] = resolvedRule;
                    resolvedAnyRule = true;

                    for (Set<Rule> candidateRules : candidates) {
                        candidateRules.remove(resolvedRule);
                    }
                }
            }

            if (!resolvedAnyRule) {
                throw new IllegalStateException("Unable to resolve ticket field order.");
            }
        }

        long product = 1;
        for (int index = 0; index < resolvedRules.length; index++) {
            if (resolvedRules[index].name().startsWith("departure")) product *= notes.yourTicket[index];
        }

        return product;
    }

    public static void run() {
        String input = Utils.getInputData(16);
        Notes notes = parseNotes(input);

        LOGGER.info(() -> "Part One: " + solvePartOne(notes));
        LOGGER.info(() -> "Part Two: " + solvePartTwo(notes));
    }
}