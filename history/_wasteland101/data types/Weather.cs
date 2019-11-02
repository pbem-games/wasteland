using System;
using System.Collections;

namespace Wasteland 
{
	class Weather 
	{
		private static Hashtable List = new Hashtable();
		public string Name;
		public string FullNameEn;
		public string FullNameRu;
		public int Radiation = 100;

		public Weather(string name) 
		{
			Name = name;
			List[name] = this;
		}

		public static Weather Get(string name) 
		{
			return (Weather)List[name];
		}

		public string ToString(Lang lng) 
		{
			if (lng == Lang.En) return FullNameEn;
			else return FullNameRu;
		}
	}

	class Month 
	{
		private static ArrayList List = new ArrayList();

		public string NameEn;
		public string NameRu;
		public int Temperature;
		public Weather Weather;

		public Month() 
		{
			List.Add(this);
		}
    
		public static Month Current 
		{
			get { return (Month)List[(Map.Turn-1) % List.Count]; }
		}
	}
}