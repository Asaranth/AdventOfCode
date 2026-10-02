package aoc2020;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

/**
 * Day 08: Handheld Halting.
 * <p>
 * Simulates execution of handheld game console boot code consisting of {@code acc}, {@code jmp}, and {@code nop} instructions.
 * Part one detects infinite loops by running until an instruction is about to be executed a second time.
 * Part two repairs the corrupted boot code by swapping a single {@code jmp} or {@code nop} instruction so the program terminates.
 */
public class Day08 {
    private Day08() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day08.class.getName());

    /**
     * Represents a single boot code instruction with an operation and an integer argument.
     *
     * @param operation the operation name ({@code acc}, {@code jmp}, or {@code nop})
     * @param argument the integer operand for the operation
     */
    private record Instruction(String operation, int argument) {}

    /**
     * Parses the raw boot code input string into a list of instruction records.
     *
     * @param input the raw boot code input
     * @return a list of parsed instructions
     */
    private static List<Instruction> parseInstructions(String input) {
        return input.lines()
                .filter(line -> !line.isBlank())
                .map(line -> {
                    String[] parts = line.split(" ");
                    String operation = parts[0];
                    int argument = Integer.parseInt(parts[1]);

                    return new Instruction(operation, argument);
                })
                .toList();
    }

    /**
     * Represents the internal execution state of the program.
     *
     * @param accumulator the current value of the accumulator
     * @param instructionPointer the index of the next instruction to execute
     */
    private record ProgramState(int accumulator, int instructionPointer) {}

    /**
     * Executes a single instruction against the current program state to produce the next state.
     *
     * @param instruction the instruction to execute
     * @param state the current program state
     * @return the new program state after executing the instruction
     * @throws IllegalArgumentException if the operation is unrecognized
     */
    private static ProgramState executeInstruction(Instruction instruction, ProgramState state) {
        return switch (instruction.operation()) {
            case "acc" -> new ProgramState(state.accumulator() + instruction.argument(), state.instructionPointer() + 1);
            case "jmp" -> new ProgramState(state.accumulator(), state.instructionPointer() + instruction.argument());
            case "nop" -> new ProgramState(state.accumulator(), state.instructionPointer() + 1);
            default -> throw new IllegalArgumentException("Unknown operation: " + instruction.operation());
        };
    }

    /**
     * Represents the result of running a program until it terminates or loops infinitely.
     *
     * @param accumulator the accumulator value upon completion or loop detection
     * @param terminated true if the program terminated normally by attempting to execute past the last instruction; false if an infinite loop was detected
     */
    private record ProgramResult(int accumulator, boolean terminated) {}

    /**
     * Executes instructions until an infinite loop is detected or the program terminates normally.
     * <p>
     * Tracks visited instruction pointers in a set to identify cyclic execution before any instruction is repeated.
     *
     * @param instructions the list of instructions to execute
     * @return the execution result containing the final accumulator value and termination status
     */
    private static ProgramResult runProgram(List<Instruction> instructions) {
        ProgramState state = new ProgramState(0, 0);
        Set<Integer> executedInstructions = new HashSet<>();

        while (state.instructionPointer() >= 0 && state.instructionPointer() < instructions.size() && !executedInstructions.contains(state.instructionPointer())) {
            executedInstructions.add(state.instructionPointer());
            Instruction instruction = instructions.get(state.instructionPointer());
            state = executeInstruction(instruction, state);
        }

        return new ProgramResult(state.accumulator(), state.instructionPointer() == instructions.size());
    }

    /**
     * Determines the value in the accumulator immediately before any instruction is executed a second time.
     *
     * @param instructions the list of boot code instructions
     * @return the accumulator value prior to repeating an instruction
     */
    private static int solvePartOne(List<Instruction> instructions) {
        return runProgram(instructions).accumulator();
    }

    /**
     * Repairs the boot code by changing exactly one {@code jmp} to {@code nop} (or {@code nop} to {@code jmp}) so it terminates.
     * <p>
     * Iterates through candidate instructions, toggles each {@code jmp}/{@code nop} operation, and simulates
     * the modified instruction list until normal termination is achieved.
     *
     * @param instructions the original list of boot code instructions
     * @return the accumulator value after the repaired program terminates normally
     * @throws IllegalStateException if no repair results in normal termination
     */
    private static int solvePartTwo(List<Instruction> instructions) {
        for (int i = 0; i < instructions.size(); i++) {
            Instruction instruction = instructions.get(i);

            if (instruction.operation().equals("acc")) continue;

            List<Instruction> modifiedInstructions = new java.util.ArrayList<>(instructions);
            String modifiedOperation = instruction.operation().equals("jmp") ? "nop" : "jmp";
            modifiedInstructions.set(i, new Instruction(modifiedOperation, instruction.argument()));
            ProgramResult result = runProgram(modifiedInstructions);

            if (result.terminated()) return result.accumulator();
        }

        throw new IllegalStateException("No repair found.");
    }

    public static void run() {
        String input = Utils.getInputData(8);
        List<Instruction> instructions = parseInstructions(input);

        LOGGER.info(() -> "Part One: " + solvePartOne(instructions));
        LOGGER.info(() -> "Part Two: " + solvePartTwo(instructions));
    }
}
