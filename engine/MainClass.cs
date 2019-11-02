using System;
using System.IO;

namespace Wasteland
{

	public class MainClass
	{
		public const string EngineVersion = "1.0.7";

		[STAThread]
		static void Main(string[] args)
		{
			#if RELEASE
			try 
			{
			#endif
				string data_file = Path.Combine(Directory.GetCurrentDirectory(), "data.xml");
				string turn_dir = Directory.GetCurrentDirectory();
				string order_to_check = null;

				for (int i = 0; i < args.Length-1; i++)
					if (args[i] == "/data")
						data_file = args[i+1];
					else if (args[i] == "/turn-dir")
						turn_dir = args[i+1];
					else if (args[i] == "/check")
						order_to_check = args[i+1];

				Console.WriteLine("Wasteland " + EngineVersion);
				Console.WriteLine("");
				Console.WriteLine("Loading data file");
				DataFile.LoadData(data_file);
				Console.WriteLine("Loading game");
				DataFile.LoadGame(turn_dir);
			
				if (order_to_check == null) 
				{
					Console.WriteLine("Processing requests");
					Request.Load(turn_dir);
					Console.WriteLine("Loading orders");
					OrdersReader.Load(turn_dir);
					Console.WriteLine("Processing orders");
					OrdersProcessor.Process();
					Console.WriteLine("Generating reports");
					Report.Generate(turn_dir);
					Console.WriteLine("Saving game");
					DataFile.SaveGame(turn_dir);
				}
				else 
				{
					Console.Write("Checking order");
					OrdersReader.Load(order_to_check, true);
				}
			#if RELEASE
			}
			catch (Exception ex) 
			{
				TextWriter tw = new StreamWriter("error.log", true, System.Text.Encoding.GetEncoding(1251));
				tw.WriteLine(ex.Message);
				tw.WriteLine(ex.StackTrace);
				tw.WriteLine();
				tw.Close();
				Environment.ExitCode = 1;
			}
			#endif
		}
	}
}
