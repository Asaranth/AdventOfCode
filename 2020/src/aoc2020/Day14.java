package aoc2020;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.logging.Logger;

/**
 * Day 14: Docking Data.
 * <p>
 * Simulates the docking computer initialisation program by processing 36-bit bitmask operations and memory writes.
 * Part one applies value bitmasks to data before storing it in memory addresses.
 * Part two simulates a memory address decoder using floating bitmasks to write values simultaneously across multiple memory addresses.
 */
public class Day14 {
    private Day14() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day14.class.getName());

    /**
     * Represents a raw memory write operation with a target address and value.
     *
     * @param address the target memory address
     * @param value the 36-bit integer value to be written
     */
    private record MemoryWrite(long address, long value) {}

    /**
     * Represents a group of memory write operations governed by a specific bitmask string.
     *
     * @param mask the 36-character bitmask string containing '0', '1', or 'X'
     * @param writes the sequence of memory writes to execute under this mask
     */
    private record MaskGroup(String mask, List<MemoryWrite> writes) {}

    /**
     * Applies the version 1 bitmask to a numeric value.
     * <p>
     * Bit '1' in the mask sets the corresponding bit to 1, bit '0' clears the bit to 0,
     * and 'X' leaves the bit unchanged.
     *
     * @param mask the 36-character bitmask string
     * @param value the original 36-bit value
     * @return the value after applying bitwise set and clear operations
     */
    private static long applyMask(String mask, long value) {
        long result = value;

        for (int i = 0; i < mask.length(); i++) {
            char bit = mask.charAt(i);
            int bitPosition = mask.length() - 1 - i;

            if (bit == '1') result |= 1L << bitPosition;
            else if (bit == '0') result &= ~(1L << bitPosition);
        }

        return result;
    }

    /**
     * Decodes a memory address according to the version 2 bitmask rules with floating bits.
     * <p>
     * Bit '1' in the mask overwrites the corresponding address bit with 1, '0' leaves it unchanged,
     * and 'X' marks the bit as floating (evaluating to both 0 and 1). All $2^N$ combinations of floating
     * bits are generated to yield the full set of target memory addresses.
     *
     * @param mask the 36-character bitmask string
     * @param address the base memory address
     * @return a list of all resolved destination memory addresses
     */
    private static List<Long> applyAddressMask(String mask, long address) {
        long baseAddress = address;
        List<Integer> floatingBits = new ArrayList<>();

        for (int i = 0; i < mask.length(); i++) {
            char bit = mask.charAt(i);
            int bitPosition = mask.length() - 1 - i;

            if (bit == '1') baseAddress |= 1L << bitPosition;
            else if (bit == 'X') floatingBits.add(bitPosition);
        }

        List<Long> addresses = new ArrayList<>();
        int combinations = 1 << floatingBits.size();

        for (int combination = 0; combination < combinations; combination++) {
            long resolvedAddress = baseAddress;

            for (int i = 0; i < floatingBits.size(); i++) {
                int bitPosition = floatingBits.get(i);

                if ((combination & (1 << i)) == 0) resolvedAddress &= ~(1L << bitPosition);
                else resolvedAddress |= 1L << bitPosition;
            }

            addresses.add(resolvedAddress);
        }

        return addresses;
    }

    /**
     * Solves Part One by executing all mask groups with version 1 value masking.
     *
     * @param groups the parsed mask groups containing masks and memory write operations
     * @return the sum of all values stored in memory after program execution
     */
    private static long solvePartOne(List<MaskGroup> groups) {
        Map<Long, Long> memory = new HashMap<>();

        for (MaskGroup group : groups) {
            for (MemoryWrite write : group.writes()) {
                long maskedValue = applyMask(group.mask(), write.value());
                memory.put(write.address(), maskedValue);
            }
        }

        return memory.values().stream().mapToLong(Long::longValue).sum();
    }

    /**
     * Solves Part Two by executing all mask groups with version 2 address decoding.
     *
     * @param groups the parsed mask groups containing masks and memory write operations
     * @return the sum of all values stored in memory after program execution
     */
    private static long solvePartTwo(List<MaskGroup> groups) {
        Map<Long, Long> memory = new HashMap<>();

        for (MaskGroup group : groups) {
            for (MemoryWrite write : group.writes()) {
                List<Long> addresses = applyAddressMask(group.mask(), write.address());

                for (long address : addresses) memory.put(address, write.value());
            }
        }

        return memory.values().stream().mapToLong(Long::longValue).sum();
    }

    public static void run() {
        String input = Utils.getInputData(14);
        List<MaskGroup> groups = new ArrayList<>();
        MaskGroup currentGroup = null;

        Pattern memPattern = Pattern.compile("mem\\[(\\d+)] = (\\d+)");

        for (String line : input.split("\\R")) {
            if (line.isBlank()) continue;

            if (line.startsWith("mask = ")) {
                String mask = line.substring("mask = ".length());
                currentGroup = new MaskGroup(mask, new ArrayList<>());
                groups.add(currentGroup);
                continue;
            }

            Matcher matcher = memPattern.matcher(line);
            if (matcher.matches()) {
                long address = Long.parseLong(matcher.group(1));
                long value = Long.parseLong(matcher.group(2));

                if (currentGroup == null)
                    throw new IllegalArgumentException("Memory instruction found before any mask: " + line);

                currentGroup.writes().add(new MemoryWrite(address, value));
            }
        }

        LOGGER.info(() -> "Part One: " + solvePartOne(groups));
        LOGGER.info(() -> "Part Two: " + solvePartTwo(groups));
    }
}
