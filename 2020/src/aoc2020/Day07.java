package aoc2020;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

/**
 * Day 07: Handy Haversacks.
 * <p>
 * Parses bag containment rules and processes hierarchical bag dependencies.
 * Part one determines how many bag colours can eventually contain at least one shiny gold bag using BFS on an inverted graph.
 * Part two computes the total number of individual bags required inside a single shiny gold bag using recursive traversal.
 */
public class Day07 {
    private Day07() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day07.class.getName());

    /**
     * Parses raw rule strings into a mapping of outer bag colours to their contained inner bag colours and quantities.
     *
     * @param rules an array of rule description strings
     * @return a map where each key is an outer bag colour and each value is a map of contained bag colours to quantities
     */
    private static Map<String, Map<String, Integer>> parseRules(String[] rules) {
        Map<String, Map<String, Integer>> contains = new HashMap<>();

        for (String rule : rules) {
            String[] parts = rule.split(" bags contain ");
            String outerColour = parts[0];
            String contents = parts[1];
            Map<String, Integer> innerBags = new HashMap<>();

            if (!contents.equals("no other bags.")) {
                for (String containedBag : contents.split(", ")) {
                    String[] words = containedBag.split(" ", 2);
                    int quantity = Integer.parseInt(words[0]);
                    String innerColour = words[1].replaceAll(" bags?\\.?$", "");
                    innerBags.put(innerColour, quantity);
                }
            }

            contains.put(outerColour, innerBags);
        }

        return contains;
    }

    /**
     * Inverts the containment rules into a reverse adjacency graph.
     * <p>
     * Maps each inner bag colour to the set of outer bag colours that can directly contain it.
     *
     * @param contains the forward containment map
     * @return a map from inner bag colours to sets of direct container bag colours
     */
    private static Map<String, Set<String>> buildContainedByMap(Map<String, Map<String, Integer>> contains) {
        Map<String, Set<String>> containedBy = new HashMap<>();

        for (Map.Entry<String, Map<String, Integer>> outerEntry : contains.entrySet()) {
            String outerColour = outerEntry.getKey();

            for (String innerColour : outerEntry.getValue().keySet()) {
                containedBy.computeIfAbsent(innerColour, key -> new HashSet<>()).add(outerColour);
            }
        }

        return containedBy;
    }

    /**
     * Recursively calculates the total number of individual bags required inside a bag of the specified colour.
     *
     * @param colour the colour of the enclosing bag
     * @param contains the forward containment map
     * @return the total number of bags contained within the specified bag
     */
    private static int countBagsInside(String colour, Map<String, Map<String, Integer>> contains) {
        int total = 0;

        for (Map.Entry<String, Integer> entry : contains.getOrDefault(colour, Map.of()).entrySet()) {
            String innerColour = entry.getKey();
            int quantity = entry.getValue();
            total += quantity * (1 + countBagsInside(innerColour, contains));
        }

        return total;
    }

    /**
     * Counts how many bag colours can eventually contain at least one {@code shiny gold} bag.
     * <p>
     * Performs a breadth-first search (BFS) over the inverted containment graph starting from {@code shiny gold}.
     *
     * @param contains the forward containment map
     * @return the number of distinct outer bag colours that can contain a shiny gold bag
     */
    private static int solvePartOne(Map<String, Map<String, Integer>> contains) {
        Map<String, Set<String>> containedBy = buildContainedByMap(contains);
        Set<String> validOuterColours = new HashSet<>();
        Deque<String> queue = new ArrayDeque<>();
        queue.add("shiny gold");

        while (!queue.isEmpty()) {
            String currentColour = queue.removeFirst();
            for (String outerColour : containedBy.getOrDefault(currentColour, Set.of())) {
                if (validOuterColours.add(outerColour)) queue.addLast(outerColour);
            }
        }

        return validOuterColours.size();
    }

    /**
     * Computes the total number of individual bags required inside a single {@code shiny gold} bag.
     *
     * @param contains the forward containment map
     * @return the total number of nested bags inside a shiny gold bag
     */
    private static int solvePartTwo(Map<String, Map<String, Integer>> contains) {
        return countBagsInside("shiny gold", contains);
    }

    public static void run() {
        String input = Utils.getInputData(7);
        String[] rules = input.strip().split("\\R");
        Map<String, Map<String, Integer>> contains = parseRules(rules);

        LOGGER.info(() -> "Part One: " + solvePartOne(contains));
        LOGGER.info(() -> "Part Two: " + solvePartTwo(contains));
    }
}
