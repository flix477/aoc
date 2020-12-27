package com.flix477;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Main {
    private enum Tile {
        EMPTY_SEAT,
        OCCUPIED_SEAT,
        FLOOR;

        public static Optional<Tile> from(int c) {
            return switch (c) {
                case '.' -> Optional.of(FLOOR);
                case 'L' -> Optional.of(EMPTY_SEAT);
                case '#' -> Optional.of(OCCUPIED_SEAT);
                default -> Optional.empty();
            };
        }

        @Override
        public String toString() {
            return switch (this) {
                case FLOOR -> ".";
                case EMPTY_SEAT -> "L";
                case OCCUPIED_SEAT -> "#";
            };
        }
    }

    private static class Point {
        public final int x;
        public final int y;

        public Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        public Point left() {
            return new Point(x - 1, y);
        }

        public Point right() {
            return new Point(x + 1, y);
        }

        public Point top() {
            return new Point(x, y - 1);
        }

        public Point bottom() {
            return new Point(x, y + 1);
        }

        public Point topRight() {
            return new Point(x + 1, y - 1);
        }

        public Point topLeft() {
            return new Point(x - 1, y - 1);
        }

        public Point bottomRight() {
            return new Point(x + 1, y + 1);
        }

        public Point bottomLeft() {
            return new Point(x - 1, y + 1);
        }

        public Stream<Point> adjacent() {
            return rangeAdjacent().map(s -> s.limit(1).findFirst()).flatMap(Optional::stream);
        }

        public Stream<Stream<Point>> rangeAdjacent() {
            Stream<UnaryOperator<Point>> l = Stream.of(
                Point::top,
                Point::bottom,
                Point::left,
                Point::right,
                Point::topLeft,
                Point::topRight,
                Point::bottomLeft,
                Point::bottomRight
            );

            return l.map(f -> Stream.iterate(this, f).skip(1));
        }

        @Override
        public String toString() {
            return "Point{" +
                    "x=" + x +
                    ", y=" + y +
                    '}';
        }
    }

    private static class Tuple<K, V> {
        public final K left;
        public final V right;


        private Tuple(K left, V right) {
            this.left = left;
            this.right = right;
        }

        @Override
        public String toString() {
            return "Tuple{" +
                    "left=" + left +
                    ", right=" + right +
                    '}';
        }
    }

    private static class Matrix<T> {
        public final int width;
        public final int height;
        private final T[][] items;

        private Matrix(int width, int height, T[][] items) {
            this.width = width;
            this.height = height;
            this.items = items;
        }

        public boolean isInside(Point p) {
            return p.y >= 0 && p.y < height && p.x >= 0 && p.x < width;
        }

        public Optional<T> get(Point p) {
            return isInside(p) ? Optional.of(items[p.y][p.x]) : Optional.empty();
        }

        public void set(Point p, T v) {
            items[p.y][p.x] = v;
        }

        public Stream<Point> points() {
            return IntStream.range(0, width * height).mapToObj(i -> new Point(i % width, i / width));
        }

        public Tuple<Point, T> item(Point p) {
            return new Tuple<>(p, get(p).orElseThrow());
        }

        public Stream<Tuple<Point, T>> items() {
            return points().map(this::item);
        }

        public Stream<Tuple<Point, T>> adjacentItems(Point point) {
            return point.adjacent().filter(this::isInside).map(this::item);
        }

        public Stream<Stream<Tuple<Point, T>>> rangedAdjacentItems(Point point) {
            return point.rangeAdjacent().map(s -> s.takeWhile(this::isInside).map(this::item));
        }

        private void debug() {
            for (var row: items) {
                for (var tile: row) {
                    System.out.print(tile);
                    System.out.print(' ');
                }
                System.out.print('\n');
            }
            System.out.print('\n');
        }
    }

    private static Tile[] parseRow(String line) {
        return line.chars()
            .mapToObj(Tile::from)
            .flatMap(Optional::stream)
            .toArray(Tile[]::new);
    }

    private static Matrix<Tile> parseGrid() throws IOException {
        var path = Paths.get("input.txt");
        var tiles = Files.lines(path).map(Main::parseRow).toArray(Tile[][]::new);
        return new Matrix<>(tiles[0].length, tiles.length, tiles);
    }

    private static Optional<Tile> rule1(Matrix<Tile> grid, Tuple<Point, Tile> item) {
        var point = item.left;
        var tile = item.right;

        if (tile == Tile.EMPTY_SEAT && grid.adjacentItems(point).allMatch(i -> i.right != Tile.OCCUPIED_SEAT))
            return Optional.of(Tile.OCCUPIED_SEAT);
        else if (tile == Tile.OCCUPIED_SEAT && countOccupied(grid.adjacentItems(point)) >= 4)
            return Optional.of(Tile.EMPTY_SEAT);

        return Optional.empty();
    }

    private static Optional<Tile> rule2(Matrix<Tile> grid, Tuple<Point, Tile> item) {
        var point = item.left;
        var tile = item.right;
        var firstSeat = grid.rangedAdjacentItems(point)
            .map(s -> s.filter(i -> i.right != Tile.FLOOR).findFirst())
            .flatMap(Optional::stream);

        if (tile == Tile.EMPTY_SEAT && firstSeat.allMatch(i -> i.right != Tile.OCCUPIED_SEAT))
            return Optional.of(Tile.OCCUPIED_SEAT);
        else if (tile == Tile.OCCUPIED_SEAT && countOccupied(firstSeat) >= 5)
            return Optional.of(Tile.EMPTY_SEAT);

        return Optional.empty();
    }

    private static long solveForRule(BiFunction<Matrix<Tile>, Tuple<Point, Tile>, Optional<Tile>> f) throws IOException {
        var grid = parseGrid();

        List<Tuple<Point, Tile>> changes;
        do {
            changes = grid.items()
                .map(i -> f.apply(grid, i).map(t -> new Tuple<>(i.left, t)))
                .flatMap(Optional::stream)
                .collect(Collectors.toList());

            changes.forEach(change -> grid.set(change.left, change.right));
        } while (!changes.isEmpty());

        return countOccupied(grid.items());
    }

    private static long countOccupied(Stream<Tuple<Point, Tile>> s) {
        return s.filter(i -> i.right == Tile.OCCUPIED_SEAT).count();
    }

    public static void main(String[] args) throws IOException {
        System.out.println("Occupied seats with first rule: " + solveForRule(Main::rule1));
        System.out.println("Occupied seats with second rule: " + solveForRule(Main::rule2));
    }
}
