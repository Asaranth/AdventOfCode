package aoc2020;

import java.util.logging.Logger;

/**
 * Day 12: Rain Risk.
 * <p>
 * Simulates ship navigation according to navigational instructions using 2D coordinate translation and rotation.
 * Part one evaluates instructions relative to the ship's current position and facing direction.
 * Part two evaluates instructions relative to a waypoint, moving and rotating the waypoint and translating the ship toward it.
 */
public class Day12 {
    private Day12() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day12.class.getName());

    /**
     * Represents the navigation state comprising the ship's facing direction, ship coordinates, and waypoint coordinates.
     *
     * @param facing the cardinal direction the ship is currently facing ({@code 'E'}, {@code 'S'}, {@code 'W'}, {@code 'N'})
     * @param ship the current [x, y] coordinates of the ship
     * @param waypoint the current [x, y] coordinates of the waypoint relative to the ship (or {@code null} when unused)
     */
    private record ShipState(char facing, int[] ship, int[] waypoint) {}

    /**
     * Represents a single navigational instruction comprising an action code and a magnitude value.
     *
     * @param action the action character indicating translation direction ({@code 'N'}, {@code 'S'}, {@code 'E'}, {@code 'W'}),
     *               rotation ({@code 'L'}, {@code 'R'}), or forward movement ({@code 'F'})
     * @param value the distance to translate or degrees to rotate
     */
    private record Instruction(char action, int value) {
        /**
         * Executes this instruction for Part One, updating ship position and orientation directly.
         *
         * @param state the current {@link ShipState}
         * @return the updated {@link ShipState} after applying the instruction
         */
        ShipState runForShip(ShipState state) {
            int[] ship = state.ship();
            char facing = state.facing();

            return switch (action) {
                case 'N' -> {
                    move(ship, 'N', value);
                    yield new ShipState(facing, ship, state.waypoint());
                }
                case 'S' -> {
                    move(ship, 'S', value);
                    yield new ShipState(facing, ship, state.waypoint());
                }
                case 'E' -> {
                    move(ship, 'E', value);
                    yield new ShipState(facing, ship, state.waypoint());
                }
                case 'W' -> {
                    move(ship, 'W', value);
                    yield new ShipState(facing, ship, state.waypoint());
                }
                case 'L' -> new ShipState(turn(facing, -value), ship, state.waypoint());
                case 'R' -> new ShipState(turn(facing, value), ship, state.waypoint());
                case 'F' -> {
                    move(ship, facing, value);
                    yield new ShipState(facing, ship, state.waypoint());
                }
                default -> throw new IllegalArgumentException("Unknown action: " + action);
            };
        }

        /**
         * Executes this instruction for Part Two, updating waypoint coordinates/orientation or translating the ship by multiples of the waypoint vector.
         *
         * @param state the current {@link ShipState}
         * @return the updated {@link ShipState} after applying the instruction
         */
        ShipState runForWaypoint(ShipState state) {
            int[] ship = state.ship();
            int[] waypoint = state.waypoint();

            return switch (action) {
                case 'N' -> {
                    move(waypoint, 'N', value);
                    yield new ShipState(state.facing(), ship, waypoint);
                }
                case 'S' -> {
                    move(waypoint, 'S', value);
                    yield new ShipState(state.facing(), ship, waypoint);
                }
                case 'E' -> {
                    move(waypoint, 'E', value);
                    yield new ShipState(state.facing(), ship, waypoint);
                }
                case 'W' -> {
                    move(waypoint, 'W', value);
                    yield new ShipState(state.facing(), ship, waypoint);
                }
                case 'L' -> new ShipState(state.facing(), ship, rotateWaypoint(waypoint, -value));
                case 'R' -> new ShipState(state.facing(), ship, rotateWaypoint(waypoint, value));
                case 'F' -> {
                    ship[0] += waypoint[0] * value;
                    ship[1] += waypoint[1] * value;
                    yield new ShipState(state.facing(), ship, waypoint);
                }
                default -> throw new IllegalArgumentException("Unknown action: " + action);
            };
        }

        /**
         * Moves the given 2D coordinate vector in a cardinal direction by the specified distance.
         *
         * @param coords the [x, y] coordinates to translate in place
         * @param direction the cardinal direction ({@code 'N'}, {@code 'S'}, {@code 'E'}, {@code 'W'})
         * @param distance the distance to move
         */
        private static void move(int[] coords, char direction, int distance) {
            switch (direction) {
                case 'N' -> coords[1] += distance;
                case 'S' -> coords[1] -= distance;
                case 'E' -> coords[0] += distance;
                case 'W' -> coords[0] -= distance;
                default -> throw new IllegalArgumentException("Unknown move direction: " + direction);
            }
        }

        /**
         * Calculates the new facing direction after turning by a multiple of 90 degrees.
         *
         * @param facing the current facing direction ({@code 'N'}, {@code 'S'}, {@code 'E'}, {@code 'W'})
         * @param degrees the degrees to turn (positive for clockwise/right, negative for counter-clockwise/left)
         * @return the resulting facing direction
         */
        private static char turn(char facing, int degrees) {
            if (degrees % 90 != 0) throw new IllegalArgumentException("Turn must be a multiple of 90 degrees: " + degrees);

            char[] directions = {'E', 'S', 'W', 'N'};

            int currentIndex = switch (facing) {
                case 'E' -> 0;
                case 'S' -> 1;
                case 'W' -> 2;
                case 'N' -> 3;
                default -> throw new IllegalArgumentException("Unknown facing direction: " + facing);
            };

            int turns = degrees / 90;
            int newIndex = Math.floorMod(currentIndex + turns, directions.length);

            return directions[newIndex];
        }

        /**
         * Rotates a waypoint vector around the origin by a multiple of 90 degrees.
         *
         * @param waypoint the [x, y] coordinates of the waypoint to rotate
         * @param degrees the degrees to rotate (positive for clockwise/right, negative for counter-clockwise/left)
         * @return a new int array containing the rotated [x, y] coordinates
         */
        private static int[] rotateWaypoint(int[] waypoint, int degrees) {
            if (degrees % 90 != 0) throw new IllegalArgumentException("Rotation must be a multiple of 90 degrees: " + degrees);

            int turns = Math.floorMod(degrees / 90, 4);
            int x = waypoint[0];
            int y = waypoint[1];

            return switch (turns) {
                case 0 -> new int[]{x, y};
                case 1 -> new int[]{y, -x};
                case 2 -> new int[]{-x, -y};
                case 3 -> new int[]{-y, x};
                default -> throw new IllegalStateException("Unexpected rotation: " + turns);
            };
        }
    }

    /**
     * Parses the puzzle input lines into an array of {@link Instruction} objects.
     *
     * @param lines the array of instruction strings
     * @return an array of parsed {@link Instruction} records
     */
    private static Instruction[] parseInstructions(String[] lines) {
        Instruction[] instructions = new Instruction[lines.length];

        for (int i = 0; i < lines.length; i++) {
            String line = lines[i];
            char direction = line.charAt(0);
            int distance = Integer.parseInt(line.substring(1));

            instructions[i] = new Instruction(direction, distance);
        }

        return instructions;
    }

    /**
     * Solves Part One by navigating the ship directly and calculating the Manhattan distance from the starting position.
     *
     * @param instructions the list of navigation instructions
     * @return the Manhattan distance from (0, 0) to the ship's final position
     */
    private static int solvePartOne(Instruction[] instructions) {
        ShipState state = new ShipState('E', new int[]{0, 0}, null);

        for (var instruction : instructions) state = instruction.runForShip(state);

        int[] coords = state.ship();
        return Math.abs(coords[0]) + Math.abs(coords[1]);
    }

    /**
     * Solves Part Two by manipulating the waypoint and moving the ship toward it, then calculating the Manhattan distance from the starting position.
     *
     * @param instructions the list of navigation instructions
     * @return the Manhattan distance from (0, 0) to the ship's final position
     */
    private static int solvePartTwo(Instruction[] instructions) {
        ShipState state = new ShipState('E', new int[]{0, 0}, new int[]{10, 1});

        for (var instruction : instructions) state = instruction.runForWaypoint(state);

        int[] coords = state.ship();
        return Math.abs(coords[0]) + Math.abs(coords[1]);
    }

    public static void run() {
        String input = Utils.getInputData(12);
        Instruction[] instructions = parseInstructions(input.strip().split("\\R"));

        LOGGER.info(() -> "Part One: " + solvePartOne(instructions));
        LOGGER.info(() -> "Part Two: " + solvePartTwo(instructions));
    }
}
