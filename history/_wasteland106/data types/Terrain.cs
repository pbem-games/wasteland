using System;
using System.Collections;

namespace Wasteland 
{
	public class Terrain : ILocalized
	{
		private static Hashtable List = new Hashtable();
		public string Name;
		public string FullNameEn;
		public string FullNameRu;
		public int MP = 1;
		public bool Walking = true;
		public bool Vehicles = true;
		public bool Ships = false;
		public Hashtable Monsters = new Hashtable();

		public Terrain(string name) 
		{
			Name = name;
			List[name] = this;
		}

		public static Terrain Get(string name) 
		{
			return (Terrain)List[name];
		}

		public string ToString(Lang lng) 
		{
			if (lng == Lang.En) return FullNameEn;
			else return FullNameRu;
		}
	}
}