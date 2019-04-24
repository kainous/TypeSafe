using System;
using System.Linq;
using System.ComponentModel;

namespace ConsoleApp1
{
    class Program
    {
        static void Main(string[] args)
        {
            PropertyChangedEventManager.AddHandler(source, OnCountChanged, nameof(Count));
            //Enumerable.SelectMany
            Console.WriteLine("Hello World!");
        }
    }
}
