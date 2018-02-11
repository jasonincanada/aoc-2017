using System;
using System.IO;
using System.Collections.Generic;

namespace AdventOfCode2017
{
    class Day5_2
    {
        static void Main(string[] args)
        {
            List<int> nums = fromFile("5.input");

            int cursor = 0;
            int steps = 1;
            int length = nums.Count;

            while (true)
            {
                int value = nums[cursor];

                if (value + cursor >= length)
                    break;
            
                nums[cursor] += value >= 3 ? -1 : 1;
                cursor += value;
                steps++;
            }

            Console.WriteLine(steps);
        }

        static List<int> fromFile(string name)
        {
            var list = new List<int>();

            using (TextReader reader = File.OpenText(name))
            {
                string line;

                while ((line = reader.ReadLine()) != null)
                {
                    int number = int.Parse(line);

                    list.Add(number);
                }
            }

            return list;
        }
    }
}
