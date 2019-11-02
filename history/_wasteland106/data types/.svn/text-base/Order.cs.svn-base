using System;
using System.Collections;

namespace Wasteland
{
	public interface IFactionOrder
	{
		void Parse (Faction faction, string orderArguments);
	}

	public interface IOrderArguments
	{
		void Parse (string orderArguments);
	}

	public interface IPersonOrder
	{
		void Validate (Person person);
	}
	
	public abstract class Order
	{
		public virtual bool IsMonthlong { get { return false; } }
	}
	
	public abstract class ItemTypeListOrder : Order, IOrderArguments
	{
		public ItemTypeList What;

		public ItemTypeListOrder ()
		{
			this.What = new ItemTypeList();
		}

		public ItemTypeListOrder (string arguments)
			: this()
		{
			this.Parse(arguments);
			this.Validate();
		}
		 
		public virtual void Parse(string arguments)
		{
			while ( arguments.Trim() != "" )
			{
				string token = MyStrings.GetQuotedToken(ref arguments).ToLower();
				ItemType it = ItemType.GetByAnyName(token);
				if ( it == null )
					throw new Exception("Bad item " + token);
				if ( this.What.Contains(it) )
					throw new Exception("Duplicate item " + token);
				this.What.Add(it);			
			}
		}

		public virtual void Validate ()
		{
		}
	}

	public abstract class PersonNumOrder : Order, IOrderArguments
	{
		public int PersonNum = 0;
		public PersonNumOrder (string arguments)			
		{
			this.Parse(arguments);
		}

		public virtual void Parse (string arguments)
		{
			string token = MyStrings.GetToken(ref arguments);
			if ( !MyStrings.IsNumber(token) )
				throw new Exception("Bad target " + token);
			// TODO: missing check for existance of a person
			this.PersonNum = Convert.ToInt32(token);
		}
	}


	public class AddressOrder : Order, IOrderArguments
	{
		public string Email;
		
		public AddressOrder (string arguments)
		{
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string token = MyStrings.GetQuotedToken(ref arguments);
			if ( token.Trim() == "" )
				throw new Exception("Address can't be empty");			
			this.Email = token;
		}
	}

	public class AttackOrder : PersonNumOrder, IOrderArguments
	{
		public AttackOrder (string arguments)
			: base(arguments)
		{
		}
	}

	public class AvoidOrder : Order, IOrderArguments
	{
		public bool Flag;
		public AvoidOrder (string arguments)
		{
			this.Flag = false;
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string token = MyStrings.GetToken(ref arguments);
			if ( token != "0" && token != "1" )
				throw new Exception("Bad parameter " + token);
			this.Flag = (token == "1");
		}
	}

	public class BuildOrder : Order, IOrderArguments
	{
		public BuildingType What;
		public override bool IsMonthlong { get { return true; } }

		public BuildOrder (string arguments)
		{
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string token = MyStrings.GetQuotedToken(ref arguments).ToLower();
			this.What = BuildingType.GetByAnyName(token);
			if ( this.What == null )
				throw new Exception("Bad object " + token);
			// TODO: missing check if item is buildable
		}
	}

	public class BurnOrder : ItemTypeListOrder, IOrderArguments 
	{
		public BurnOrder (string arguments)
			: base(arguments)
		{
		}

		public override void Validate ()
		{
			foreach ( ItemType itemType in this.What )
			{
				if ( !itemType.Burn )
					throw new Exception(itemType.ToString(Lang.En) + " can't be burned");
			}
		}
	}

	public class ConsumeOrder : ItemTypeListOrder, IPersonOrder
	{
		public ConsumeOrder (Person person, string arguments)
		{
			this.Parse(arguments);
			this.Validate(person);
		}

		public void Validate (Person person)
		{
			foreach ( ItemType itemType in this.What )
			{
				if ( !person.Man.Food.Contains(itemType) )
					throw new Exception("A " + person.Man.ToString(Lang.En) + " can't consume " + itemType.ToString(Lang.En));
			}
		}
	}

	public class CureOrder : Order, IOrderArguments
	{
		public ArrayList PatientNums;
		public override bool IsMonthlong { get { return true; } }

		public CureOrder (string arguments)
		{
			this.PatientNums = new ArrayList();
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			while ( arguments != "" )
			{
				string token = MyStrings.GetToken(ref arguments);
				if ( !MyStrings.IsNumber(token) )
					throw new Exception("Bad target " + token);
				this.PatientNums.Add(Convert.ToInt32(token));
			}
		}
	}

	public class DeclareOrder : Order, IFactionOrder
	{
		public int			FactionNum;
		public Attitude Attitude;

		public DeclareOrder (Faction faction, string arguments)
		{
			this.Parse(faction, arguments);
		}

		public void Parse (Faction faction, string arguments)
		{
			// declare 12 neutral
			string factionToken = MyStrings.GetToken(ref arguments).ToLower();
			if ( factionToken == "default" )
			{
				// TODO: correct the logic
				// the logic here is mangled, it does work but I don't like it
				// it would be better to have class element public bool default instead
				this.FactionNum = faction.Num;
			} else
			{
				if ( !MyStrings.IsNumber(factionToken) )
				{
					throw new Exception("Bad faction");
				}
				// TODO: missing check for existance of faction 
				this.FactionNum = Convert.ToInt32(factionToken);
			}

			string attitudeToken = MyStrings.GetQuotedToken(ref arguments).ToLower();
			Attitude attitude = Attitude.Hostile;
			while ( attitude <= Attitude.Ally && attitude.ToString().ToLower() != attitudeToken )
			{
				attitude++;
			}
			if ( attitude > Attitude.Ally )
			{
				throw new Exception("Bad attitude " + attitudeToken);
			}
			this.Attitude = attitude;
		}

	}

	public class DescribeOrder : Order, IOrderArguments
	{
		public enum Target 
		{
			Person,
			Object
		}

		public Target What;
		public string Description;

		public DescribeOrder (string arguments)
		{
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string targetToken = MyStrings.GetToken(ref arguments).ToLower();
			#region switch target
			switch ( targetToken )
			{
				case "person":
					this.What = DescribeOrder.Target.Person;
					break;
				case "object":
					this.What = DescribeOrder.Target.Object;
					break;
				default:
					throw new Exception("Bad target " + targetToken);
			}
			#endregion
			string desc = MyStrings.GetQuotedToken(ref arguments);
			string descriptionToken = MyStrings.GetValidString(desc);
			if ( desc != descriptionToken )
				throw new Exception("Invalid characters in description");

			this.Description = descriptionToken.Trim();
		}
	}

	public class DriveOrder : MoveOrder, IOrderArguments
	{
		public DriveOrder (string arguments)
			: base(arguments)
		{		
		}

		public override object GetDirection(string token) 
		{
			switch ( token )
			{
				case "n":
					return Direction.North;
				case "nw":
					return Direction.Northwest;
				case "ne":
					return Direction.Northeast;
				case "s":
					return Direction.South;
				case "sw":
					return Direction.Southwest;
				case "se":
					return Direction.Southeast;
			}

			throw new Exception("Bad direction " + token);
		}
	}

	public class EnterOrder : Order, IOrderArguments
	{
		public int BuildingNum;

		public EnterOrder (string arguments)
		{
			this.BuildingNum = 0;
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string token = MyStrings.GetToken(ref arguments);
			if ( !MyStrings.IsNumber(token) )
				throw new Exception("Bad object");
			this.BuildingNum = Convert.ToInt32(token);
		}
	}

	public class EquipmentOrder : ItemTypeListOrder, IOrderArguments
	{
		public EquipmentOrder (string arguments)
			: base(arguments)
		{
		}
	}

	public class EvictOrder : PersonNumOrder, IOrderArguments
	{
		public EvictOrder (string arguments)
			: base(arguments)
		{
		}
	}

	public class GiveOrder : Order, IOrderArguments
	{
		public int			Target;
		public int			Amount;
		public ItemType What;

		public GiveOrder (string arguments)
		{
			this.Parse(arguments);
		}

		public void Parse(string arguments)
		{
			string targetToken = MyStrings.GetToken(ref arguments);
			if ( !MyStrings.IsNumber(targetToken) )
				throw new Exception("Bad target " + targetToken);
			this.Target = Convert.ToInt32(targetToken);

			string amountToken = MyStrings.GetToken(ref arguments).ToLower();
			if ( amountToken == "all" )
			{
				this.Amount = -1;
			} else
			{
				if ( !MyStrings.IsNumber(amountToken) )
				{
					throw new Exception("Bad amount " + amountToken);
				}
				this.Amount = Convert.ToInt32(amountToken);
			}

			string itemToken = MyStrings.GetQuotedToken(ref arguments);
			this.What = ItemType.GetByAnyName(itemToken);
			if ( this.What == null )
			{
				throw new Exception("Bad item " + itemToken);
			}
		}
	}

	public class GreedyOrder : Order, IOrderArguments
	{
		public bool Flag;
		public GreedyOrder (string arguments)
		{
			this.Flag = false;
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string token = MyStrings.GetToken(ref arguments);
			if ( token != "0" && token != "1" )
				throw new Exception("Bad parameter " + token);
			this.Flag = (token == "1");
		}
	}

	public class HideOrder : Order, IOrderArguments
	{
		public enum Variants 
		{ 
			Not, 
			Faction, 
			Person 
		}

		public Variants Variant;

		public HideOrder (string arguments)
		{
			this.Variant = Variants.Not;
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string token = MyStrings.GetToken(ref arguments).ToLower();
			#region switch token
			switch ( token )
			{
				case "":
				case "none":
					this.Variant = HideOrder.Variants.Not;
					break;
				case "faction":
					this.Variant = HideOrder.Variants.Faction;
					break;
				case "person":
					this.Variant = HideOrder.Variants.Person;
					break;
				default:
					throw new Exception("Bad parameter " + token);
			}
			#endregion
		}
	}

	public class InstallOrder : ItemTypeListOrder, IOrderArguments
	{
		public override bool IsMonthlong { get { return true; } }

		public InstallOrder ()
			: base()
		{
		}

		public InstallOrder (string arguments)
			: base(arguments)
		{
		}

		public override void Validate ()
		{
			if ( this.What.Count == 0 )
				throw new Exception("List of item types to install can't be empty");

			foreach ( ItemType itemType in this.What )
			{
				//TODO: missing check for skill and level
				if ( itemType.InstallSkill == null)
					throw new Exception(itemType.ToString(Lang.En) + " can't be installed");
			}
		}

	}

	public class KickOrder : Order 
	{
	}

	public class LeaveOrder : Order 
	{
	}

	public class MoveOrder : Order, IOrderArguments
	{
		public bool				Attack;
		public ArrayList	Directions;

		public MoveOrder (string arguments)
		{
			this.Attack			= false;
			this.Directions = new ArrayList();
			this.Parse(arguments);
		}

		public virtual object GetDirection(string token) 
		{
			switch ( token )
			{
				case "n":
					return Direction.North;
				case "nw":
					return Direction.Northwest;
				case "ne":
					return Direction.Northeast;
				case "s":
					return Direction.South;
				case "sw":
					return Direction.Southwest;
				case "se":
					return Direction.Southeast;
				case "out":
					return 0;
			}

			if (MyStrings.IsNumber(token)) 
				return Convert.ToInt32(token);
			else
				throw new Exception("Bad direction " + token);
		}

		public void Parse (string arguments)
		{
			string token = MyStrings.GetToken(ref arguments).ToLower();
			if ( token == "attack" )
			{
				this.Attack = true;
				token = MyStrings.GetToken(ref arguments).ToLower();
			}
			while ( token != "" )
			{
				Directions.Add(GetDirection(token));
				token = MyStrings.GetToken(ref arguments).ToLower();
			}
		}
	}

	public class NameOrder : Order, IOrderArguments
	{
		public enum Target 
		{
			Person,
			Faction,
			Object
		}

		public Target What;
		public string Name;

		public NameOrder (string arguments)
		{
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string targetToken = MyStrings.GetToken(ref arguments).ToLower();
			switch ( targetToken )
			{
				case "person":
					this.What = NameOrder.Target.Person;
					break;
				case "faction":
					this.What = NameOrder.Target.Faction;
					break;
				case "object":
					this.What = NameOrder.Target.Object;
					break;
				default:
					throw new Exception("Bad target " + targetToken);
			}

			string name = MyStrings.GetQuotedToken(ref arguments);
			string nameToken = MyStrings.GetValidString(name);
			if (nameToken != name)
				throw new Exception("Invalid characters in name");
			if (nameToken.Trim() == "" )
				throw new Exception("Bad name");
			this.Name = nameToken.Trim();
		}
	}

	public class OptionOrder : Order, IOrderArguments 
	{
		public string Option;
		public string Val;

		public OptionOrder (string arguments)
		{
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string valueToken;
			string optionToken = MyStrings.GetToken(ref arguments).ToLower();

			switch ( optionToken )
			{
				case "text-report":
				case "xml-report":
					valueToken = MyStrings.GetToken(ref arguments).ToLower();
					if ( valueToken != "0" && valueToken != "1" )
					{
						throw new Exception("Wrong option value " + valueToken);
					}
					this.Val = valueToken;
					break;
				case "language":
					valueToken = MyStrings.GetToken(ref arguments).ToLower();
					if ( valueToken != "ru" && valueToken != "en" )
					{
						throw new Exception("Wrong option value");
					}
					this.Val = valueToken;
					break;
				case "template":
					valueToken = MyStrings.GetToken(ref arguments).ToLower();
					if (valueToken != "long" && valueToken != "short")
						throw new Exception("Wrong option value");
					this.Val = valueToken;
					break;
				default:
					throw new Exception("No such option");
			}
			this.Option = optionToken;
		}
	}

	public class PasswordOrder : Order, IOrderArguments
	{
		public string Password;

		public PasswordOrder (string arguments)			
		{
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string token = MyStrings.GetQuotedToken(ref arguments);
			if ( token.Trim() == string.Empty )
			{
				throw new Exception("Password can't be empty");
			}
			if ( token != MyStrings.GetValidString(token) )
			{
				throw new Exception("Bad symbols in password");
			}
			this.Password = token;
		}
	}

	public class PatrolOrder : Order 
	{
		public override bool IsMonthlong { get { return true; } }
	}

	public class ProduceOrder : Order, IOrderArguments
	{
		public ItemType ItemType;
		public override bool IsMonthlong { get { return true; } }

		public ProduceOrder (string arguments)
		{
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string token = MyStrings.GetQuotedToken(ref arguments);
			ItemType itemType = ItemType.GetByAnyName(token);
			if ( itemType == null )
			{
				throw new Exception("Bad item " + token);
			}
			this.ItemType = itemType;
		}

	}

	public class QuitOrder : Order, IFactionOrder
	{
		public QuitOrder (Faction faction, string arguments)
		{
			this.Parse(faction, arguments);
		}

		public void Parse (Faction faction, string arguments)
		{
			string passwordToken = MyStrings.GetQuotedToken(ref arguments);
			if ( passwordToken != faction.Password )
			{
				throw new Exception("Wrong password");
			}
		}
	}

	public class ScavengeOrder : ItemTypeListOrder, IOrderArguments 
	{
		public override bool IsMonthlong { get { return true; } }
		
		public ScavengeOrder (string arguments)
			: base(arguments)
		{
		}
	}

	public class ShowOrder : Order, IOrderArguments 
	{
		public ItemType			ItemType;
		public SkillType		SkillType;
		public BuildingType BuildingType;
		public int					FactionNum;

		public ShowOrder (string arguments)
		{
			this.ItemType			= null;
			this.SkillType		= null;
			this.BuildingType = null;
			this.FactionNum		= 0;
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{			
			string targetToken = MyStrings.GetToken(ref arguments).ToLower();
			#region target token switch
			switch ( targetToken )
			{
				case "faction":
					string factionToken = MyStrings.GetToken(ref arguments);
					if ( !MyStrings.IsNumber(factionToken) )
					{
						throw new Exception("Bad faction " + factionToken);
					}
					this.FactionNum = Convert.ToInt32(factionToken);
					break;
				case "item":
					string itemToken = MyStrings.GetQuotedToken(ref arguments).ToLower();
					ItemType itemType = ItemType.GetByAnyName(itemToken);
					if ( itemType == null )
					{
						throw new Exception("Bad item " + itemToken);
					}
					this.ItemType = itemType;
					break;
				case "skill":
					string skillToken = MyStrings.GetQuotedToken(ref arguments).ToLower();
					SkillType skillType = SkillType.GetByAnyName(skillToken);
					if ( skillType == null )
					{
						throw new Exception("Bad skill " + skillToken);
					}
					this.SkillType = skillType;
					break;
				case "object":
					string objectToken = MyStrings.GetQuotedToken(ref arguments).ToLower();
					BuildingType buildingType = BuildingType.GetByAnyName(objectToken);
					if ( buildingType == null )
					{
						throw new Exception("Bad object");
					}
					this.BuildingType = buildingType;
					break;
				default:
					throw new Exception("Bad parameter " + targetToken);
			}
			#endregion
		}
	}

	public class SpoilsOrder : ItemTypeListOrder, IOrderArguments
	{
		public SpoilsOrder (string arguments)
			: base(arguments)
		{
		}
	}

	public class TeamOrder : Order, IOrderArguments 
	{
		public bool Kick = false;
		public int LeaderNum = 0;

		public TeamOrder (string arguments)
		{
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string token = MyStrings.GetToken(ref arguments).ToLower();
			if ( token == "kick" )
			{
				this.Kick = true;
				token = MyStrings.GetToken(ref arguments).ToLower();
			}
			if ( token == string.Empty || token == "none" )
			{
				this.LeaderNum = 0;
			} else
			{
				if ( !MyStrings.IsNumber(token) )
				{
					throw new Exception("Bad target " + token);
				}
				this.LeaderNum = Convert.ToInt32(token);
			}
		}
	}

	public class TradeOrder : Order, IOrderArguments 
	{
		public int			PersonNum;

		public ItemType BuyWhat ;
		public int			BuyAmount;

		public ItemType SellWhat;
		public int			SellAmount;

		public TradeOrder ()
		{
			this.PersonNum = 0;

			this.BuyWhat = null;
			this.BuyAmount = 0;
			this.SellWhat = null;
			this.SellAmount = 0;
		}

		public TradeOrder (string arguments)
			: this()
		{
			this.Parse(arguments);
		}

		public void Parse (string arguments)
		{
			string token = MyStrings.GetToken(ref arguments).ToLower();

			string itemToken;
			ItemType itemType;

			// TODO: refactor needed, this logic isn't very clear
			if ( token == "with" )
			{
				token = MyStrings.GetToken(ref arguments);
				if ( !MyStrings.IsNumber(token) )
				{
					throw new Exception("Bad target " + token);
				}
				this.PersonNum = Convert.ToInt32(token);
				token = MyStrings.GetToken(ref arguments).ToLower();
			} else
			{
				this.PersonNum = 0;
			}

			if ( token == "all" )
			{
				this.SellAmount = -1;
			} else
			{
				if ( !MyStrings.IsNumber(token) )
				{
					throw new Exception("Bad sell amount " + token);
				}
				this.SellAmount = Convert.ToInt32(token);
			}

			itemToken = MyStrings.GetQuotedToken(ref arguments);
			itemType = ItemType.GetByAnyName(itemToken);
			if ( itemType == null )
			{
				throw new Exception("Bad sell item " + itemToken);
			}
			this.SellWhat = itemType;

			token = MyStrings.GetToken(ref arguments).ToLower();
			if ( token == "all" )
			{
				this.BuyAmount = -1;
			} else
			{
				if ( !MyStrings.IsNumber(token) )
				{
					throw new Exception("Bad buy amount " + token);
				}
				this.BuyAmount = Convert.ToInt32(token);
			}

			itemToken = MyStrings.GetQuotedToken(ref arguments);
			itemType = ItemType.GetByAnyName(itemToken);
			if ( itemType == null )
			{
				throw new Exception("Bad buy item " + itemToken);
			}
			this.BuyWhat = itemType;
		}
	}

	public class UninstallOrder : ItemTypeListOrder, IOrderArguments
	{
		public int Amount;
		public override bool IsMonthlong { get { return true; } }

		public UninstallOrder (string arguments)
		{
			this.Parse(arguments);
		}

		public override void Parse (string arguments)
		{
			string argumentsCopy = arguments;
			string token = MyStrings.GetQuotedToken(ref argumentsCopy);
			if ( MyStrings.IsNumber(token) )
			{
				// UNINSTALL 10 BLOC
				this.Amount = Convert.ToInt32(token);
				string		itemToken = MyStrings.GetQuotedToken(ref argumentsCopy);
				ItemType	itemType	= ItemType.GetByAnyName(itemToken);
				if ( itemType == null )
					throw new Exception("Bad item " + itemToken);
				this.What.Add(itemType);
			} else
			{
				// UNINSTALL BLOC CONS
				this.Amount = -1;
				base.Parse(arguments);
			}
		}

		public override void Validate ()
		{
			if ( this.What.Count == 0 )
				throw new Exception("List of item types to uninstall can't be empty");

			foreach ( ItemType itemType in this.What )
			{
				//TODO: missing check for skill and level
				if ( itemType.InstallSkill == null )
					throw new Exception(itemType.ToString(Lang.En) + " can't be uninstalled");
			}
		}
	}

}
