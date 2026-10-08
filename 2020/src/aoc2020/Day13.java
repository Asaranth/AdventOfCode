package aoc2020;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Logger;

/**
 * Day 13: Shuttle Search.
 * <p>
 * Solves bus departure scheduling problems using modular arithmetic and timetable alignment.
 * Part one calculates the earliest departure time for each available bus to find the minimum waiting time.
 * Part two determines the earliest timestamp satisfying indexed bus departure offsets using the Chinese Remainder Theorem sieve method.
 */
public class Day13 {
    private Day13() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day13.class.getName());

    /**
     * Represents a scheduled departure time and the list of bus IDs departing at that time.
     *
     * @param time the departure timestamp
     * @param busses the list of bus IDs departing at this timestamp
     */
    private record Timetable(int time, List<Integer> busses) {}

    /**
     * Represents a bus requirement with its route ID and required departure offset.
     *
     * @param busId the bus route interval / cycle period
     * @param offset the relative minute offset from the base timestamp
     */
    private record BusOffset(int busId, int offset) {}

    /**
     * Solves Part One by finding the earliest bus departure at or after the given earliest departure time.
     *
     * @param earliestDepartureTime the earliest timestamp at which the passenger can depart
     * @param busIds the list of bus IDs (including out-of-service {@code "x"} entries)
     * @return the wait time multiplied by the bus ID of the earliest available bus
     */
    private static int solvePartOne(int earliestDepartureTime, List<String> busIds) {
        List<Integer> activeBusIds = busIds.stream().filter(value -> !value.equals("x")).map(Integer::parseInt).toList();
        Map<Integer, List<Integer>> departuresByTime = new TreeMap<>();

        for (int busId : activeBusIds) {
            int departureTime = earliestDepartureTime;

            while (departureTime % busId != 0) departureTime++;

            departuresByTime.computeIfAbsent(departureTime, ignored -> new ArrayList<>()).add(busId);
        }

        List<Timetable> timetable = departuresByTime.entrySet().stream()
                .map(entry -> new Timetable(entry.getKey(), entry.getValue())).toList();
        Timetable earliestDeparture = timetable.getFirst();
        int busId = earliestDeparture.busses().getFirst();

        return (earliestDeparture.time() - earliestDepartureTime) * busId;
    }

    /**
     * Solves Part Two by finding the earliest timestamp that satisfies all bus departure offset constraints.
     * <p>
     * Applies an incremental step search (sieve algorithm based on the Chinese Remainder Theorem),
     * updating the timestamp until the current bus condition {@code (timestamp + offset) % busId == 0} is met,
     * and multiplying the step size by each coprime bus ID.
     *
     * @param busIds the list of bus IDs from the timetable, where index position defines the departure offset
     * @return the earliest timestamp matching the offset schedule
     */
    private static long solvePartTwo(List<String> busIds) {
        List<BusOffset> busOffsets = new ArrayList<>();

        for (int offset = 0; offset < busIds.size(); offset++) {
            String busId = busIds.get(offset);

            if (!busId.equals("x")) busOffsets.add(new BusOffset(Integer.parseInt(busId), offset));
        }

        long timestamp = 0;
        long step = 1;

        for (BusOffset busOffset : busOffsets) {
            while ((timestamp + busOffset.offset()) % busOffset.busId() != 0) timestamp += step;

            step *= busOffset.busId();
        }

        return timestamp;
    }

    public static void run() {
        String input = Utils.getInputData(13);
        String[] lines = input.split("\\R");
        int earliestDepartureTime = Integer.parseInt(lines[0]);
        List<String> busIds = Arrays.stream(lines[1].split(",")).toList();

        LOGGER.info(() -> "Part One: " + solvePartOne(earliestDepartureTime, busIds));
        LOGGER.info(() -> "Part Two: " + solvePartTwo(busIds));
    }
}