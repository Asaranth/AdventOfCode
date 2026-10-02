package aoc2020;

import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

/**
 * Day 03: Toboggan Trajectory.
 * <p>
 * Navigates a horizontally repeating 2D map of open squares ('.') and trees ('#').
 * Part one counts trees encountered using a slope of right 3, down 1.
 * Part two computes the product of tree counts across five distinct slopes.
 */
public class Day03 {
    private Day03() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day03.class.getName());

    private static final int[][] SLOPES = {
            {1, 1},
            {3, 1},
            {5, 1},
            {7, 1},
            {1, 2}
    };

    /**
     * Parses the raw puzzle input into a list of grid rows.
     *
     * @param input the raw puzzle input
     * @return the list of rows representing the map
     */
    private static List<String> parseGrid(String input) {
        return Arrays.stream(input.strip().split("\\R")).toList();
    }

    /**
     * Counts the number of trees encountered while traversing the grid with a given slope.
     *
     * @param grid the map of open squares and trees
     * @param right the number of squares to move to the right per step
     * @param down the number of squares to move down per step
     * @return the total number of trees encountered
     */
    private static int countTrees(List<String> grid, int right, int down) {
        if (grid.isEmpty()) return 0;

        int width = grid.getFirst().length();
        int trees = 0;
        int col = 0;

        for (int row = 0; row < grid.size(); row += down) {
            if (grid.get(row).charAt(col) == '#') trees++;
            col = (col + right) % width;
        }

        return trees;
    }

    /**
     * Counts trees encountered along the slope of right 3, down 1.
     *
     * @param grid the map of open squares and trees
     * @return the number of trees encountered for part one
     */
    private static int solvePartOne(List<String> grid) {
        return countTrees(grid, 3, 1);
    }

    /**
     * Computes the product of trees encountered across all configured slopes.
     *
     * @param grid the map of open squares and trees
     * @return the product of tree encounter counts across all slopes
     */
    private static long solvePartTwo(List<String> grid) {
        return Arrays.stream(SLOPES)
                .mapToLong(slope -> countTrees(grid, slope[0], slope[1]))
                .reduce(1L, (a, b) -> a * b);
    }

    public static void run() {
        String input = Utils.getInputData(3);
        List<String> grid = parseGrid(input);

        LOGGER.info(() -> "Part One: " + solvePartOne(grid));
        LOGGER.info(() -> "Part Two: " + solvePartTwo(grid));
    }
}
