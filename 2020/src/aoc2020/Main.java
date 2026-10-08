package aoc2020;

import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

public class Main {
    private static final Logger LOGGER = Logger.getLogger(Main.class.getName());
    private static final Map<Integer, Runnable> SOLUTIONS = new HashMap<>();

    static {
        configureLogging();
        SOLUTIONS.put(1, Day01::run);
        SOLUTIONS.put(2, Day02::run);
        SOLUTIONS.put(3, Day03::run);
        SOLUTIONS.put(4, Day04::run);
        SOLUTIONS.put(5, Day05::run);
        SOLUTIONS.put(6, Day06::run);
        SOLUTIONS.put(7, Day07::run);
        SOLUTIONS.put(8, Day08::run);
        SOLUTIONS.put(9, Day09::run);
        SOLUTIONS.put(10, Day10::run);
        SOLUTIONS.put(11, Day11::run);
        SOLUTIONS.put(12, Day12::run);
        SOLUTIONS.put(13, Day13::run);
        SOLUTIONS.put(14, Day14::run);
        SOLUTIONS.put(15, Day15::run);
        SOLUTIONS.put(16, Day16::run);
    }

    static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        LOGGER.info("Enter the day number you want to run (1-25): ");

        if (scanner.hasNextInt()) {
            int day = scanner.nextInt();
            if (day >= 1 && day <= 25) runSolution(day);
            else LOGGER.info("Invalid input. Please enter a number between 1 and 25.");
        } else {
            LOGGER.info("Invalid input. Please enter a number between 1 and 25.");
        }
    }

    private static void configureLogging() {
        Logger rootLogger = Logger.getLogger("");

        for (java.util.logging.Handler handler : rootLogger.getHandlers()) {
            rootLogger.removeHandler(handler);
        }

        ConsoleHandler consoleHandler = new ConsoleHandler();
        consoleHandler.setFormatter(new Formatter() {
            @Override
            public String format(LogRecord record) {
                return record.getMessage() + System.lineSeparator();
            }
        });

        rootLogger.addHandler(consoleHandler);
        rootLogger.setUseParentHandlers(false);
    }

    private static void runSolution(int day) {
        Runnable solution = SOLUTIONS.get(day);
        if (solution != null) solution.run();
        else LOGGER.info("Solution for the given day is not implemented yet.");
    }
}