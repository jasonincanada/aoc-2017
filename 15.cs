/* Advent of Code 2017

   Day: 15 - Dueling Generators
   URL: http://adventofcode.com/2017/day/15

   Parts 1/2
*/

using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2017
{
    class Day15
    {
        const long Modulus = 2147483647; // 2^31-1;
        const long Mask = 65535; // 2^16-1;

        static void Main(string[] args)
        {
            Func<long, bool> dividesBy4 = (x => x % 4 == 0);
            Func<long, bool> dividesBy8 = (x => x % 8 == 0);
            Func<long, bool> alwaysTrue = (x => true);

            // Sample inputs
            Process(65, 8921, 40000000, alwaysTrue, alwaysTrue);
            Process(65, 8921, 5000000, dividesBy4, dividesBy8);

            // Our inputs
            Process(277, 349, 40000000, alwaysTrue, alwaysTrue);
            Process(277, 349, 5000000, dividesBy4, dividesBy8);
        }

        private static IEnumerable<long> Generate(long seed, long init, Func<long, bool> predicate)
        {
            long number = init;

            while (true)
            {
                while (!predicate(number))
                    number = (number * seed) % Modulus;

                yield return number;

                number = (number * seed) % Modulus;
            }
        }

        private static void Process(long a, long b, long count, Func<long, bool> predicateA, Func<long, bool> predicateB)
        {  
            int matches = 0;

            // Hard-coded values given by the problem
            var generateA = Generate(16807, a, predicateA);
            var generateB = Generate(48271, b, predicateB);

            var iterA = generateA.GetEnumerator();
            var iterB = generateB.GetEnumerator();

            for (int i = 0; i < count; i++)
            {
                iterA.MoveNext();
                iterB.MoveNext();

                a = iterA.Current;
                b = iterB.Current;

                long maska = a & Mask;
                long maskb = b & Mask;

                int match = maska == maskb ? 1 : 0;
                matches += match;
            }

            Console.WriteLine("({0}, {1}) after {2} steps, with {3} matches", a, b, count, matches);
        }
    }
}
