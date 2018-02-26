using System;

namespace AdventOfCode2017
{
    class Day15
    {
        const long Modulus = 2147483647; // 2^31-1;
        const long Mask = 65535; // 2^16-1;

        static void Main(string[] args)
        {
            Part1(277, 349, 40000000);           
        }

        private static void Part1(long a, long b, long count)
        {  
            int matches = 0;

            for (int i = 0; i < count; i++)
            {
                long maska = a & Mask;
                long maskb = b & Mask;

                int match = maska == maskb ? 1 : 0;
                matches += match;

                long nexta = (a * 16807) % Modulus;
                long nextb = (b * 48271) % Modulus;

                a = nexta;
                b = nextb;                
            }

            Console.WriteLine("({0}, {1}) after {2} steps, with {3} matches", a, b, count, matches);
        }
    }
}
