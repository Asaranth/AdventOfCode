package aoc2020;

import java.util.Arrays;
import java.util.Map;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/**
 * Day 04: Passport Processing.
 * <p>
 * Parses batch passport data and validates passports according to required fields and formatting rules.
 * Part one checks for the presence of all required passport fields (country ID is optional).
 * Part two validates the specific formats, ranges, and allowed values of each required field.
 */
public class Day04 {
    private Day04() {/* This utility class should not be instantiated */}

    private static final Logger LOGGER = Logger.getLogger(Day04.class.getName());

    /**
     * Represents a passport with its respective field values.
     *
     * @param birthYear the four-digit birth year (byr)
     * @param issueYear the four-digit issue year (iyr)
     * @param expirationYear the four-digit expiration year (eyr)
     * @param height the height string with measurement unit (hgt)
     * @param hairColour the hair colour hex code (hcl)
     * @param eyeColour the three-letter eye colour code (ecl)
     * @param passportId the nine-digit passport ID (pid)
     * @param CountryId the optional country ID (cid)
     */
    private record Passport(String birthYear, String issueYear, String expirationYear, String height, String hairColour, String eyeColour, String passportId, String CountryId) {}

    /**
     * Parses the raw puzzle input into an array of {@link Passport} records.
     * <p>
     * Passports in the input are separated by blank lines, and individual key-value pairs are separated
     * by whitespace or newlines.
     *
     * @param input the raw puzzle input
     * @return an array of parsed passports
     */
    private static Passport[] parseInput(String input) {
        return Arrays.stream(input.strip().split("\\R\\s*\\R")).map(passportText -> {
            Map<String, String> fields = Arrays.stream(passportText.split("\\s+"))
                    .map(field -> field.split(":", 2))
                    .collect(Collectors.toMap(parts -> parts[0], parts -> parts[1]));

            return new Passport(fields.get("byr"), fields.get("iyr"), fields.get("eyr"), fields.get("hgt"), fields.get("hcl"), fields.get("ecl"), fields.get("pid"), fields.get("cid"));
        }).toArray(Passport[]::new);
    }

    /**
     * Validates that a string represents a 4-digit year within the specified inclusive range.
     *
     * @param value the year string to validate
     * @param min the minimum valid year (inclusive)
     * @param max the maximum valid year (inclusive)
     * @return {@code true} if the value is a valid 4-digit year between min and max; {@code false} otherwise
     */
    private static boolean isValidYear(String value, int min, int max) {
        if (value == null || !value.matches("\\d{4}")) return false;

        int year = Integer.parseInt(value);
        return year >= min && year <= max;
    }

    /**
     * Validates that a height string consists of a number followed by either "cm" or "in" and falls within valid ranges.
     * <p>
     * For "cm", the height must be between 150 and 193 (inclusive).
     * For "in", the height must be between 59 and 76 (inclusive).
     *
     * @param value the height string to validate
     * @return {@code true} if the height value is valid; {@code false} otherwise
     */
    private static boolean isValidHeight(String value) {
        if (value == null || !value.matches("\\d+(cm|in)")) return false;

        int amount = Integer.parseInt(value.substring(0, value.length() - 2));
        String unit = value.substring(value.length() - 2);

        return switch (unit) {
            case "cm" -> amount >= 150 && amount <= 193;
            case "in" -> amount >= 59 && amount <= 76;
            default -> false;
        };
    }

    /**
     * Validates that a hair colour string is a '#' followed by exactly six hexadecimal characters (0-9, a-f).
     *
     * @param value the hair colour string to validate
     * @return {@code true} if the hair colour matches the required format; {@code false} otherwise
     */
    private static boolean isValidHairColour(String value) {
        return value != null && value.matches("#[0-9a-f]{6}");
    }

    /**
     * Validates that an eye colour string matches one of the allowed colour codes.
     * <p>
     * Allowed colour codes are: {@code amb}, {@code blu}, {@code brn}, {@code gry}, {@code grn}, {@code hzl}, {@code oth}.
     *
     * @param value the eye colour string to validate
     * @return {@code true} if the eye colour is one of the allowed codes; {@code false} otherwise
     */
    private static boolean isValidEyeColour(String value) {
        return value != null && value.matches("amb|blu|brn|gry|grn|hzl|oth");
    }

    /**
     * Validates that a passport ID string is a nine-digit number, including leading zeroes.
     *
     * @param value the passport ID string to validate
     * @return {@code true} if the passport ID is exactly nine digits; {@code false} otherwise
     */
    private static boolean isValidPassportId(String value) {
        return value != null && value.matches("\\d{9}");
    }

    /**
     * Counts the number of passports containing all required fields.
     * <p>
     * Required fields are: birth year, issue year, expiration year, height, hair colour, eye colour, and passport ID.
     * Country ID is optional.
     *
     * @param passports the array of passports to check
     * @return the number of passports with all required fields present
     */
    private static int solvePartOne(Passport[] passports) {
        return (int) Arrays.stream(passports)
                .filter(passport -> passport.birthYear() != null
                        && passport.issueYear() != null
                        && passport.expirationYear() != null
                        && passport.height() != null
                        && passport.hairColour() != null
                        && passport.eyeColour() != null
                        && passport.passportId() != null)
                .count();
    }

    /**
     * Counts the number of passports where all required fields are present and satisfy their validation rules.
     *
     * @param passports the array of passports to check
     * @return the number of fully valid passports
     */
    private static int solvePartTwo(Passport[] passports) {
        return (int) Arrays.stream(passports)
                .filter(passport -> isValidYear(passport.birthYear(), 1920, 2002)
                        && isValidYear(passport.issueYear(), 2010, 2020)
                        && isValidYear(passport.expirationYear(), 2020, 2030)
                        && isValidHeight(passport.height())
                        && isValidHairColour(passport.hairColour())
                        && isValidEyeColour(passport.eyeColour())
                        && isValidPassportId(passport.passportId()))
                .count();
    }

    public static void run() {
        String input = Utils.getInputData(4);
        Passport[] passports = parseInput(input);

        LOGGER.info(() -> "Part One: " + solvePartOne(passports));
        LOGGER.info(() -> "Part Two: " + solvePartTwo(passports));
    }
}
