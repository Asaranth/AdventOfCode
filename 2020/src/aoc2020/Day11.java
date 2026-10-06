package aoc2020;

import java.util.Arrays;
import java.util.logging.Logger;

/**
 * Day 11: Seating System.
 * <p>
 * Simulates a cellular automaton model of passenger seating layout changes until reaching equilibrium.
 * Part one simulates seat occupancy based on directly adjacent seats with an overcrowding threshold of four.
 * Part two simulates seat occupancy based on the first visible seat in eight directions with an overcrowding threshold of five.
 */
public class Day11 {
    private Day11() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day11.class.getName());

    /**
     * Relative coordinate offsets for the eight adjacent and directional neighbours (horizontal, vertical, diagonal).
     */
    private static final int[][] DIRECTIONS = {
            {-1, -1}, {-1, 0}, {-1, 1},
            {0, -1}, {0, 1},
            {1, -1}, {1, 0}, {1, 1}
    };

    /**
     * Holds the state of the seating layout after a simulation round and whether any seat changed state.
     *
     * @param seats the new grid of seat states
     * @param changed {@code true} if at least one seat changed state during the round; {@code false} otherwise
     */
    private record SimulationResult(char[][] seats, boolean changed) {}

    /**
     * Strategy interface for counting occupied seats relevant to a given seat coordinate.
     */
    @FunctionalInterface
    private interface OccupiedSeatCounter {
        /**
         * Counts relevant occupied seats for a given seat position.
         *
         * @param seats the 2D grid of seat states
         * @param row the row index of the target seat
         * @param col the column index of the target seat
         * @return the number of occupied seats relevant to the target position
         */
        int count(char[][] seats, int row, int col);
    }

    /**
     * Simulates a single round of seat occupancy changes according to the configured counting strategy and seat limit.
     * <p>
     * An empty seat ({@code 'L'}) becomes occupied ({@code '#'}) if zero relevant seats are occupied.
     * An occupied seat ({@code '#'}) becomes empty ({@code 'L'}) if at least {@code occupiedSeatLimit} relevant seats are occupied.
     * Floor spaces ({@code '.'}) never change.
     *
     * @param seats the current 2D grid of seat states
     * @param occupiedSeatCounter the strategy used to count relevant occupied seats
     * @param occupiedSeatLimit the minimum number of occupied seats required to vacate a seat
     * @return a {@link SimulationResult} containing the new seat grid and a boolean indicating if changes occurred
     */
    private static SimulationResult simulateRound(char[][] seats, OccupiedSeatCounter occupiedSeatCounter, int occupiedSeatLimit) {
        char[][] nextSeats = new char[seats.length][seats[0].length];
        boolean changed = false;

        for (int row = 0; row < seats.length; row++) {
            for (int col = 0; col < seats[row].length; col++) {
                char currentSeat = seats[row][col];
                int occupiedSeats = occupiedSeatCounter.count(seats, row, col);

                if (currentSeat == 'L' && occupiedSeats == 0) nextSeats[row][col] = '#';
                else if (currentSeat == '#' && occupiedSeats >= occupiedSeatLimit) nextSeats[row][col] = 'L';
                else nextSeats[row][col] = currentSeat;

                if (nextSeats[row][col] != currentSeat) changed = true;
            }
        }

        return new SimulationResult(nextSeats, changed);
    }

    /**
     * Counts the number of occupied seats directly adjacent to the given coordinates (up to eight neighbours).
     *
     * @param seats the 2D grid of seat states
     * @param row the row index of the target seat
     * @param col the column index of the target seat
     * @return the count of directly adjacent occupied seats ({@code '#'})
     */
    private static int countOccupiedAdjacentSeats(char[][] seats, int row, int col) {
        int occupiedSeats = 0;

        for (int[] direction : DIRECTIONS) {
            int adjacentRow = row + direction[0];
            int adjacentCol = col + direction[1];

            if (isInBounds(seats, adjacentRow, adjacentCol) && seats[adjacentRow][adjacentCol] == '#') occupiedSeats++;
        }

        return occupiedSeats;
    }

    /**
     * Counts the number of first visible occupied seats in each of the eight cardinal and diagonal directions.
     * <p>
     * For each direction, rays are cast outward until the first seat ({@code 'L'} or {@code '#'}) or grid boundary is encountered.
     * Floor tiles ({@code '.'}) are traversed without blocking line of sight.
     *
     * @param seats the 2D grid of seat states
     * @param row the row index of the target seat
     * @param col the column index of the target seat
     * @return the count of first visible occupied seats ({@code '#'}) in the eight directions
     */
    private static int countVisibleOccupiedSeats(char[][] seats, int row, int col) {
        int occupiedSeats = 0;

        for (int[] direction : DIRECTIONS) {
            int visibleRow = row + direction[0];
            int visibleCol = col + direction[1];

            while (isInBounds(seats, visibleRow, visibleCol)) {
                if (seats[visibleRow][visibleCol] == '#') {
                    occupiedSeats++;
                    break;
                }

                if (seats[visibleRow][visibleCol] == 'L') break;

                visibleRow += direction[0];
                visibleCol += direction[1];
            }
        }

        return occupiedSeats;
    }

    /**
     * Determines whether the specified row and column coordinates are within the bounds of the seating grid.
     *
     * @param seats the 2D grid of seat states
     * @param row the row coordinate to test
     * @param col the column coordinate to test
     * @return {@code true} if the coordinates are within bounds; {@code false} otherwise
     */
    private static boolean isInBounds(char[][] seats, int row, int col) {
        return row >= 0 && row < seats.length && col >= 0 && col < seats[row].length;
    }

    /**
     * Counts the total number of occupied seats across the entire seating grid.
     *
     * @param seats the 2D grid of seat states
     * @return the total number of occupied seats ({@code '#'})
     */
    private static int countOccupiedSeats(char[][] seats) {
        int occupiedSeats = 0;

        for (char[] row : seats) {
            for (char seat : row) {
                if (seat == '#') occupiedSeats++;
            }
        }

        return occupiedSeats;
    }

    /**
     * Repeatedly applies simulation rounds until the seating arrangement reaches equilibrium (no seats change state).
     *
     * @param seats the initial 2D grid of seat states
     * @param occupiedSeatCounter the strategy used to count relevant occupied seats
     * @param occupiedSeatLimit the threshold of occupied seats that causes an occupied seat to become empty
     * @return the total number of occupied seats once the simulation stabilises
     */
    private static int simulateUntilStable(char[][] seats, OccupiedSeatCounter occupiedSeatCounter, int occupiedSeatLimit) {
        SimulationResult result;

        do {
            result = simulateRound(seats, occupiedSeatCounter, occupiedSeatLimit);
            seats = result.seats();
        } while (result.changed());

        return countOccupiedSeats(seats);
    }

    public static void run() {
        String input = Utils.getInputData(11);
        char[][] seats = Arrays.stream(input.strip().split("\\R")).map(String::toCharArray).toArray(char[][]::new);

        LOGGER.info(() -> "Part One: " + simulateUntilStable(seats, Day11::countOccupiedAdjacentSeats, 4));
        LOGGER.info(() -> "Part Two: " + simulateUntilStable(seats, Day11::countVisibleOccupiedSeats, 5));
    }
}