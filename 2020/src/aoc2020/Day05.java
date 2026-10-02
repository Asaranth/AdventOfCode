package aoc2020;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

/**
 * Day 05: Binary Boarding.
 * <p>
 * Decodes boarding passes using binary space partitioning to determine seat rows, columns, and unique seat IDs.
 * Part one finds the highest seat ID on a boarding pass.
 * Part two finds the missing seat ID corresponding to your seat.
 */
public class Day05 {
    private Day05() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day05.class.getName());

    /**
     * Determines a position along an axis using binary space partitioning based on the provided instructions.
     * <p>
     * Iteratively halves the search range {@code [0, upperBound]} where each character indicates whether to keep
     * the lower or upper half.
     *
     * @param instructions the string of partition instructions (e.g. 'F'/'B' for row or 'L'/'R' for column)
     * @param upperBound the maximum index of the range (inclusive, e.g. 127 for rows or 7 for columns)
     * @param lowerHalfInstruction the character that indicates keeping the lower half of the remaining range
     * @return the resolved zero-based index position
     */
    private static int findPosition(String instructions, int upperBound, char lowerHalfInstruction) {
        int low = 0;
        int high = upperBound;

        for (char instruction : instructions.toCharArray()) {
            int middle = (low + high) / 2;
            if (instruction == lowerHalfInstruction) high = middle;
            else low = middle + 1;
        }

        return low;
    }

    /**
     * Calculates the unique seat ID for a given boarding pass string.
     * <p>
     * The first 7 characters specify the row (0 through 127) using 'F' and 'B', and the last 3 characters specify
     * the column (0 through 7) using 'L' and 'R'. The seat ID is computed as {@code row * 8 + column}.
     *
     * @param boardingPass the 10-character boarding pass string
     * @return the unique seat ID
     */
    private static int getSeatId(String boardingPass) {
        int row = findPosition(boardingPass.substring(0, 7), 127, 'F');
        int column = findPosition(boardingPass.substring(7), 7, 'L');
        return row * 8 + column;
    }

    /**
     * Finds the highest seat ID among all boarding passes in the input.
     *
     * @param boardingPasses the array of boarding pass strings
     * @return the maximum seat ID found
     */
    private static int solvePartOne(String[] boardingPasses) {
        int highestSeatId = 0;
        for (String boardingPass : boardingPasses) {
            if (boardingPass.isBlank()) continue;

            int seatId = getSeatId(boardingPass);
            highestSeatId = Math.max(highestSeatId, seatId);
        }

        return highestSeatId;
    }

    /**
     * Finds the missing seat ID corresponding to your seat.
     * <p>
     * Identifies the empty seat ID that is not at the very front or back of the plane, whose immediate neighbours
     * (ID - 1 and ID + 1) are present in the list of boarding passes.
     *
     * @param boardingPasses the array of boarding pass strings
     * @return the missing seat ID
     * @throws IllegalStateException if no missing seat ID is found
     */
    private static int solvePartTwo(String[] boardingPasses) {
        Set<Integer> seatIds = new HashSet<>();
        int lowestSeatId = Integer.MAX_VALUE;
        int highestSeatId = Integer.MIN_VALUE;

        for (String boardingPass : boardingPasses) {
            if (boardingPass.isBlank()) continue;

            int seatId = getSeatId(boardingPass);
            seatIds.add(seatId);
            lowestSeatId = Math.min(lowestSeatId, seatId);
            highestSeatId = Math.max(highestSeatId, seatId);
        }

        for (int seatId = lowestSeatId + 1; seatId < highestSeatId; seatId++) {
            if (!seatIds.contains(seatId) && seatIds.contains(seatId - 1) && seatIds.contains(seatId + 1))
                return seatId;
        }

        throw new IllegalStateException("No missing seat ID found.");
    }

    public static void run() {
        String input = Utils.getInputData(5);
        String[] boardingPasses = input.split("\\R");

        LOGGER.info(() -> "Part One: " + solvePartOne(boardingPasses));
        LOGGER.info(() -> "Part Two: " + solvePartTwo(boardingPasses));
    }
}
