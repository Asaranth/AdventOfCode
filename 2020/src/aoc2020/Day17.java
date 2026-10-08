package aoc2020;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

/**
 * Day 17: Conway Cubes.
 * <p>
 * Simulates multi-dimensional cellular automata (Conway's Game of Life) across 3D and 4D pocket dimensions.
 * Part one simulates the active cube states in 3-dimensional space over six boot cycles.
 * Part two simulates the active cube states in 4-dimensional hyper-space over six boot cycles.
 */
public class Day17 {
    private Day17() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day17.class.getName());

    /**
     * Represents an N-dimensional coordinate in the pocket dimension.
     *
     * @param coordinates the array of coordinate values across each dimension
     */
    private record Point(int[] coordinates) {
        /**
         * Computes a new point by adding the specified coordinate offsets to this point.
         *
         * @param offset the coordinate offsets to add for each dimension
         * @return the new translated {@code Point}
         */
        private Point add(int[] offset) {
            int[] nextCoordinates = new int[coordinates.length];

            for (int i = 0; i < coordinates.length; i++) nextCoordinates[i] = coordinates[i] + offset[i];

            return new Point(nextCoordinates);
        }

        @Override
        public boolean equals(Object other) {
            return other instanceof Point point && Arrays.equals(coordinates, point.coordinates);
        }

        @Override
        public int hashCode() {
            return Arrays.hashCode(coordinates);
        }
    }

    /**
     * Generates all non-zero relative neighbour coordinate offsets for the given number of dimensions.
     *
     * @param dimensions the number of spatial dimensions
     * @return a list of coordinate offset arrays representing all adjacent neighbouring positions
     */
    private static List<int[]> generateNeighborOffsets(int dimensions) {
        List<int[]> offsets = new ArrayList<>();
        buildOffsets(offsets, new int[dimensions], 0);
        return offsets;
    }

    /**
     * Recursively populates neighbour offsets by permuting {-1, 0, 1} across each dimension.
     *
     * @param offsets the accumulator list of non-zero offset arrays
     * @param currentOffset the buffer array holding the current offset vector being built
     * @param dimensionIndex the current dimension index being processed
     */
    private static void buildOffsets(List<int[]> offsets, int[] currentOffset, int dimensionIndex) {
        if (dimensionIndex == currentOffset.length) {
            if (!isAllZero(currentOffset)) offsets.add(Arrays.copyOf(currentOffset, currentOffset.length));

            return;
        }

        for (int offset = -1; offset <= 1; offset++) {
            currentOffset[dimensionIndex] = offset;
            buildOffsets(offsets, currentOffset, dimensionIndex + 1);
        }
    }

    /**
     * Checks whether all components of the given offset array are zero (representing the origin).
     *
     * @param values the array of integer values to check
     * @return {@code true} if every element is zero, {@code false} otherwise
     */
    private static boolean isAllZero(int[] values) {
        for (int value : values) {
            if (value != 0) return false;
        }

        return true;
    }

    /**
     * Simulates a single boot cycle of the cellular automaton.
     * <p>
     * Active cubes contribute neighbour counts to adjacent positions. An active cube remains active
     * if it has 2 or 3 active neighbours; an inactive cube becomes active if it has exactly 3 active neighbours.
     *
     * @param activeCubes the set of currently active cube coordinates
     * @param neighborOffsets the list of adjacent relative offsets
     * @return the new set of active cube coordinates after the cycle
     */
    private static Set<Point> runCycle(Set<Point> activeCubes, List<int[]> neighborOffsets) {
        Map<Point, Integer> neighborCounts = new HashMap<>();

        for (Point cube : activeCubes) {
            for (int[] offset : neighborOffsets) {
                Point neighbor = cube.add(offset);
                neighborCounts.merge(neighbor, 1, Integer::sum);
            }
        }

        Set<Point> nextActiveCubes = new HashSet<>();

        for (Map.Entry<Point, Integer> entry : neighborCounts.entrySet()) {
            Point cube = entry.getKey();
            int activeNeighborCount = entry.getValue();

            if (activeCubes.contains(cube)) {
                if (activeNeighborCount == 2 || activeNeighborCount == 3) nextActiveCubes.add(cube);
            } else if (activeNeighborCount == 3) {
                nextActiveCubes.add(cube);
            }
        }

        return nextActiveCubes;
    }

    /**
     * Solves the Conway Cubes simulation for the specified number of dimensions over 6 boot cycles.
     *
     * @param state the initial 2D slice configuration of cubes ('#' for active, '.' for inactive)
     * @param dimensions the number of spatial dimensions to simulate (e.g. 3 for Part One, 4 for Part Two)
     * @return the total number of active cubes after 6 boot cycles
     */
    private static int solve(char[][] state, int dimensions) {
        Set<Point> activeCubes = new HashSet<>();

        for (int y = 0; y < state.length; y++) {
            for (int x = 0; x < state[y].length; x++) {
                if (state[y][x] == '#') {
                    int[] coordinates = new int[dimensions];
                    coordinates[0] = x;
                    coordinates[1] = y;
                    activeCubes.add(new Point(coordinates));
                }
            }
        }

        List<int[]> neighborOffsets = generateNeighborOffsets(dimensions);

        for (int cycle = 0; cycle < 6; cycle++) activeCubes = runCycle(activeCubes, neighborOffsets);

        return activeCubes.size();
    }

    public static void run() {
        String input = Utils.getInputData(17);
        char[][] state = Arrays.stream(input.split("\\R")).map(String::toCharArray).toArray(char[][]::new);

        LOGGER.info(() -> "Part One: " + solve(state, 3));
        LOGGER.info(() -> "Part Two: " + solve(state, 4));
    }
}