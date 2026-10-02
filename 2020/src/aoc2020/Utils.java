package aoc2020;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public final class Utils {

    private static final Logger LOGGER = Logger.getLogger(Utils.class.getName());
    private static final String SESSION_COOKIE_ENV = "AOC_SESSION_COOKIE";
    private static final String YEAR = "2020";

    private Utils() {
        throw new UnsupportedOperationException("Utility class");
    }

    public static String getInputData(int day) {
        Path cacheFile = resolveCacheFile(day);

        if (Files.exists(cacheFile)) return readCachedInput(cacheFile);

        String sessionCookie = getSessionCookie();
        if (sessionCookie == null || sessionCookie.isBlank())
            throw new InputDataException("AOC_SESSION_COOKIE not found in AdventOfCode/.env.");

        return fetchAndCacheInput(day, cacheFile, sessionCookie);
    }

    private static Path resolveCacheFile(int day) {
        return Path.of(YEAR, "data", String.format("%02d.txt", day));
    }

    private static String readCachedInput(Path cacheFile) {
        try {
            return Files.readString(cacheFile);
        } catch (IOException e) {
            throw new InputDataException("Failed to read cached input file: " + cacheFile, e);
        }
    }

    private static String fetchAndCacheInput(int day, Path cacheFile, String sessionCookie) {
        String url = String.format("https://adventofcode.com/%s/day/%d/input", YEAR, day);
        HttpRequest request = HttpRequest.newBuilder().uri(URI.create(url)).header("Cookie", "session=" + sessionCookie).GET().build();

        HttpClient client = HttpClient.newHttpClient();

        try {
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            validateResponse(response);

            String data = response.body();
            writeCacheFile(cacheFile, data);
            return data;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new InputDataException("Interrupted while retrieving input data for Day " + day, e);
        } catch (IOException e) {
            throw new InputDataException("Failed to retrieve or cache input data for Day " + day, e);
        }
    }

    private static void validateResponse(HttpResponse<String> response) {
        if (response.statusCode() != 200)
            throw new InputDataException("Failed to fetch data: HTTP " + response.statusCode() + " - " + response.body());
    }

    private static void writeCacheFile(Path cacheFile, String data) throws IOException {
        Path dataDir = cacheFile.getParent();
        if (dataDir != null) Files.createDirectories(dataDir);
        Files.writeString(cacheFile, data);
    }

    private static String getSessionCookie() {
        Path[] possibleEnvPaths = {
                Path.of("..", ".env"),
                Path.of(".env")
        };

        for (Path envPath : possibleEnvPaths) {
            String cookie = readSessionCookieFromPath(envPath);
            if (cookie != null) return cookie;
        }

        return null;
    }

    private static String readSessionCookieFromPath(Path envPath) {
        if (!Files.exists(envPath)) return null;

        try {
            return findSessionCookie(Files.readAllLines(envPath));
        } catch (IOException ex) {
            LOGGER.log(Level.WARNING, ex, () -> "Failed to read environment file: " + envPath);
            return null;
        }
    }

    private static String findSessionCookie(List<String> lines) {
        for (String line : lines) {
            String trimmed = line.trim();
            if (trimmed.startsWith(SESSION_COOKIE_ENV + "="))
                return cleanCookieValue(trimmed.substring((SESSION_COOKIE_ENV + "=").length()).trim());
        }

        return null;
    }

    private static String cleanCookieValue(String value) {
        String cleanedValue = value;
        if ((cleanedValue.startsWith("\"") && cleanedValue.endsWith("\"")) || (cleanedValue.startsWith("'") && cleanedValue.endsWith("'")))
            cleanedValue = cleanedValue.substring(1, cleanedValue.length() - 1);

        return cleanedValue.isBlank() ? null : cleanedValue;
    }

    public static final class InputDataException extends RuntimeException {

        public InputDataException(String message) {
            super(message);
        }

        public InputDataException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}