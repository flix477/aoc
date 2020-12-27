using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace _12
{
    enum InstructionType
    {
        MoveNorth,
        MoveEast,
        MoveWest,
        MoveSouth,
        MoveForward,
        TurnLeft,
        TurnRight
    }

    class Instructions
    {
        public static InstructionType From(Direction d) => d switch
        {
            Direction.East => InstructionType.MoveEast,
            Direction.West => InstructionType.MoveWest,
            Direction.North => InstructionType.MoveNorth,
            Direction.South => InstructionType.MoveSouth,
            _ => throw new InvalidOperationException()
        };

        public static (InstructionType, int)? ParseInstruction(string x)
        {
            InstructionType? instructionType = x[0] switch
            {
                'N' => InstructionType.MoveNorth,
                'E' => InstructionType.MoveEast,
                'W' => InstructionType.MoveWest,
                'S' => InstructionType.MoveSouth,
                'F' => InstructionType.MoveForward,
                'R' => InstructionType.TurnRight,
                'L' => InstructionType.TurnLeft,
                _   => null
            };

            if (instructionType == null) {
                return null;
            }

            return (instructionType.Value, Convert.ToInt32(x.Substring(1)));
        }
    }

    enum Direction
    {
        North = 0,
        East = 1,
        South = 2,
        West = 3,
    }

    static class Directions {
        private static int mod(int x, int m) => (x % m + m) % m;

        public static Direction RotateRight(Direction d, int amount) =>
            (Direction) mod((int)d + amount / 90, 4);

        public static Direction RotateLeft(Direction d, int amount) =>
            (Direction) mod((int)d - amount / 90, 4);
    }

    interface IWaypoint
    {
        (int, int) ApplyInstruction((InstructionType, int) instruction);
    }

    class SimpleWaypoint : IWaypoint {
        private Direction direction = Direction.East;

        public (int, int) ApplyInstruction((InstructionType, int) instruction) {
            var (type, value) = instruction;

            return type switch
            {
                InstructionType.MoveEast => (value, 0),
                InstructionType.MoveWest => (-value, 0),
                InstructionType.MoveNorth => (0, -value),
                InstructionType.MoveSouth => (0, value),
                InstructionType.MoveForward => ApplyInstruction((Instructions.From(direction), value)),
                InstructionType.TurnLeft => RotateLeft(value),
                InstructionType.TurnRight => RotateRight(value),
                _ => throw new InvalidOperationException()
            };
        }

        private (int, int) RotateLeft(int amount) {
            direction = Directions.RotateLeft(direction, amount);
            return (0, 0);
        }

        private (int, int) RotateRight(int amount) {
            direction = Directions.RotateRight(direction, amount);
            return (0, 0);
        }
    }

    class Waypoint : IWaypoint {
        private int x = 10;
        private int y = -1;

        public (int, int) ApplyInstruction((InstructionType, int) instruction) {
            var (type, value) = instruction;

            switch (type)
            {
                case InstructionType.MoveEast:
                    x += value;
                    break;
                case InstructionType.MoveWest:
                    x -= value;
                    break;
                case InstructionType.MoveNorth:
                    y -= value;
                    break;
                case InstructionType.MoveSouth:
                    y += value;
                    break;
                case InstructionType.TurnLeft:
                    Rotate(-value);
                    break;
                case InstructionType.TurnRight:
                    Rotate(value);
                    break;
                case InstructionType.MoveForward:
                    return (x * value, y * value);
            };

            return (0, 0);
        }

        public void Rotate(int degrees) {
            var rad = (Math.PI / 180) * degrees;
            int x1 = x, y1 = y;

            x = Convert.ToInt32(Math.Cos(rad) * x1 - Math.Sin(rad) * y1);
            y = Convert.ToInt32(Math.Sin(rad) * x1 + Math.Cos(rad) * y1);
        }
    }

    class Ship
    {
        private int x = 0;
        private int y = 0;
        private IWaypoint waypoint;

        public Ship(IWaypoint waypoint, IEnumerable<(InstructionType, int)> instructions) {
            this.waypoint = waypoint;
            foreach (var instruction in instructions)
                ApplyInstruction(instruction);
        }

        private void ApplyInstruction((InstructionType, int) instruction) {
            var (x, y) = waypoint.ApplyInstruction(instruction);
            this.x += x;
            this.y += y;
        }

        public int ManhattanDistance() => Math.Abs(x) + Math.Abs(y);
    }

    class Program
    {
        public static IEnumerable<T> EnumeratePresent<T>(Nullable<T> x) where T: struct =>
            x == null ? Enumerable.Empty<T>() : Enumerable.Repeat(x.Value, 1);

        public static int solveForWaypoint((InstructionType, int)[] instructions, IWaypoint waypoint) =>
            new Ship(waypoint, instructions).ManhattanDistance();

        static void Main(string[] args)
        {
            var instructions = File.ReadLines("input.txt")
                .Select(Instructions.ParseInstruction)
                .SelectMany(EnumeratePresent)
                .ToArray();

            Console.WriteLine($"First manhattan distance: {solveForWaypoint(instructions, new SimpleWaypoint())}");
            Console.WriteLine($"Second manhattan distance: {solveForWaypoint(instructions, new Waypoint())}");
        }
    }
}
