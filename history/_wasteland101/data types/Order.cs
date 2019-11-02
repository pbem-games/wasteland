using System;
using System.Collections;

namespace Wasteland
{

	class Order 
	{
		public virtual bool IsMonthlong { get { return false; } }
	}

	class ItemTypeListOrder : Order 
	{
		public ItemTypeArrayList What = new ItemTypeArrayList();
	}

	class PersonNumOrder : Order 
	{
		public int PersonNum = 0;
	}


	class AddressOrder : Order
	{
		public string Email = "";
	}

	class AttackOrder : PersonNumOrder 
	{
	}

	class AvoidOrder : Order 
	{
		public bool Flag = false;
	}

	class BuildOrder : Order 
	{
		public BuildingType What;
		public override bool IsMonthlong { get { return true; } }
	}

	class BurnOrder : ItemTypeListOrder 
	{
	}

	class ConsumeOrder : ItemTypeListOrder 
	{
	}

	class CureOrder : Order 
	{
		public ArrayList PatientNums = new ArrayList();
		public override bool IsMonthlong { get { return true; } }
	}

	class DeclareOrder : Order 
	{
		public int FactionNum = 0;
		public Attitude Attitude = Attitude.Neutral;
	}

	class DescribeOrder : Order 
	{
		public enum Target 
		{
			Person,
			Object
		}

		public Target What;
		public string Description;
	}

	class DriveOrder : Order
	{
		public bool Attack = false;
		public ArrayList Directions = new ArrayList();
	}

	class EnterOrder : Order
	{
		public int BuildingNum = 0;
	}

	class EquipmentOrder : ItemTypeListOrder
	{
	}

	class EvictOrder : PersonNumOrder 
	{
	}

	class GiveOrder : Order 
	{
		public int Target = 0;
		public int Amount = 0;
		public ItemType What = null;
	}

	class HideOrder : Order 
	{
		public enum Variants { Not, Faction, Person }
		public Variants Variant = Variants.Not;
	}

	class InstallOrder : Order 
	{
		public ItemType What;
		public override bool IsMonthlong { get { return true; } }
	}

	class KickOrder : Order 
	{
	}

	class LeaveOrder : Order 
	{
	}

	class MaintainOrder : Order 
	{
		public override bool IsMonthlong { get { return true; } }
	}

	class MoveOrder : Order 
	{
		public bool Attack = false;
		public ArrayList Directions = new ArrayList();
		public override bool IsMonthlong { get { return true; } }
	}

	class NameOrder : Order 
	{
		public enum Target 
		{
			Person,
			Faction,
			Object
		}

		public Target What;
		public string Name;
	}

	class OptionOrder : Order 
	{
		public string Option = "";
		public string Val = "";
	}

	class PasswordOrder : Order 
	{
		public string Password = "";
	}

	class PatrolOrder : Order 
	{
		public override bool IsMonthlong { get { return true; } }
	}

	class ProduceOrder : Order 
	{
		public ItemType ItemType = null;
		public override bool IsMonthlong { get { return true; } }
	}

	class QuitOrder : Order 
	{
	}

	class ScavengeOrder : ItemTypeListOrder 
	{
		public override bool IsMonthlong { get { return true; } }
	}

	class ShowOrder : Order 
	{
		public ItemType ItemType = null;
		public SkillType SkillType = null;
		public BuildingType BuildingType = null;
		public int FactionNum = 0;
	}

	class TeamOrder : Order 
	{
		public bool Kick = false;
		public int LeaderNum = 0;
	}

	class TradeOrder : Order 
	{
		public int PersonNum = 0;
		public ItemType BuyWhat = null;
		public int BuyAmount = 0;
		public ItemType SellWhat = null;
		public int SellAmount = 0;
	}

	class UninstallOrder : Order 
	{
		public ItemType What;
		public int Amount;
		public override bool IsMonthlong { get { return true; } }
	}

}
