using System;
using System.Xml;

namespace Wasteland
{
	public class NameGenerator
	{
		private static XmlElement elBase;

		public static void Init(XmlElement el)
		{
			elBase = el;
		}

		public static string Name(string gender) 
		{
			return Name(gender, "");
		}

		public static string Name(string gender, string ancestor) 
		{
			XmlElement elGroup = null;
			string res = "";
			string ancestor_second = "";
			gender = gender.ToLower();
			
			// Select group by ancestor second name
			if (ancestor != "" && ancestor != null) 
			{
				for (int i = ancestor.Length-1; i >= 0; i--)
					if (ancestor[i] == ' ') break;
					else ancestor_second = ancestor[i] + ancestor_second;

				elGroup = (XmlElement)elBase.SelectSingleNode("group[second/name[@value='" +
					ancestor_second + "']]");
			}

			if (elGroup == null) 
			{
				// Select group at random
				ancestor_second = "";
				elGroup = RandomElement.Select(elBase.SelectNodes("group"));
				if (elGroup == null)
					return "";
			}

			// Get first name for gender
			XmlNodeList first = elGroup.SelectNodes("first/name[@" + gender + "]");

			// If none, change gender to "man"
			if (first.Count == 0) 
			{
				gender = "man";
				first = elGroup.SelectNodes("first/name[@" + gender + "]");
				if (first.Count == 0)
					return "";
			}

			XmlElement elFirst = (XmlElement)first[Constants.Random(first.Count)];
			res += elFirst.GetAttribute("value");

			if (ancestor_second != "") 
			{
				return res + " " + ancestor_second;
			}
			else 
			{
				// Get second name
				XmlNodeList second = elGroup.SelectNodes("second/name[@" + gender + "]");
				if (second.Count == 0)
					return res;

				XmlElement elSecond = (XmlElement)second[Constants.Random(second.Count)];
				res += " " + elSecond.GetAttribute("value");
				return res;
			}
		}
	}

	public class RandomElement 
	{
		public static XmlElement Select(XmlNodeList list) 
		{
			int roll = Constants.Random(100);
			foreach (XmlElement el in list) 
			{
				int chance = Convert.ToInt32(el.GetAttribute("chance"));
				roll -= chance;
				if (roll <= 0)
					return el;
			}
			return null;
		}
	}
}
