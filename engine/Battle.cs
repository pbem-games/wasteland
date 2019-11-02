using System;
using System.Collections;

namespace Wasteland 
{
	public class Battle
	{
		private ArrayList Soldiers = null;
		private int FirstDefender = 0;
		private ArrayList BattleReport = new ArrayList();
		private int AttackersOOA = 0;
		private int DefendersOOA = 0;
		private ItemList Spoils = new ItemList();
		public Hashtable TakenHwpn = new Hashtable();
		public Hashtable Hits = new Hashtable();
		public bool AttackersWin = false;
		public bool DefendersWin = false;
		public Region region = null;

		public Battle(Person attacker, Person defender, Region region) 
		{
			PersonList attackers = new PersonList();
			attackers.Add(attacker);
			Init(attackers, defender, region);
		}

		public Battle(PersonList attackers, Person defender, Region region) 
		{
			Init(attackers, defender, region);
		}

		private void Init(PersonList attackers, Person defender, Region region) 
		{
			Soldiers = new ArrayList();
			ArrayList check_persons = new ArrayList();
			this.region = region;

			// Add attackers and their teams
			foreach (Person attacker in attackers) 
				foreach (Person atk in attacker.GetTeamAndLeader()) 
				{
					if (check_persons.Contains(atk))
						throw new Exception("Soldier already added");
					check_persons.Add(atk);
          
					Soldiers.Add(new Soldier(this, atk, Side.Attacker));
					FirstDefender++;
				}

			// Add target person and his team
			if (defender.Leader != null)
				foreach (Person def in defender.Leader.GetTeamAndLeader()) 
				{
					if (check_persons.Contains(def))
						throw new Exception("Soldier already added");
					check_persons.Add(def);

					Soldiers.Add(new Soldier(this, def, Side.Defender));
				}
			else
				foreach (Person def in defender.GetTeamAndLeader()) 
				{
					if (check_persons.Contains(def))
						throw new Exception("Soldier already added");
					check_persons.Add(def);

					Soldiers.Add(new Soldier(this, def, Side.Defender));
				}

			// Add region patrol
			foreach (Person p in region.Persons) 
			{
				if (p.Patrolling && p.AttitudeTo(defender) == Attitude.Ally
					&& !check_persons.Contains(p))
					Soldiers.Add(new Soldier(this, p, Side.Defender));
			}

			// Remove from attackers all further ATTACK orders directed on defenders
			for (int i = 0; i < FirstDefender; i++) 
			{
				Person p = ((Soldier)Soldiers[i]).Person;
				foreach (Order ord in p.Orders)
				{
					if (ord.GetType() == typeof(AttackOrder)) 
					{
						AttackOrder aord = (AttackOrder)ord;
						for (int k = FirstDefender; k < Soldiers.Count; k++) 
						{
							if (aord.PersonNum == ((Soldier)Soldiers[k]).Person.Num) 
							{
								aord.PersonNum = -1;
								break;
							}
						}
					}
				}
			}

			BattleReport.Add(
				String.Format("{0} attacks {1} in {2}!",
					attackers[0].ToString(Lang.En),
					defender.ToString(Lang.En),
					region.ToString(Lang.En)) +
				String.Format("|{2}: {0} и {1} вступили в бой!",
					attackers[0].ToString(Lang.Ru),
					defender.ToString(Lang.Ru),
					region.ToString(Lang.Ru)));

			region.BattleReports.Add(BattleReport);

			BattleReport.Add("");
			BattleReport.Add("Attackers:|Атакующие:");
			for (int i = 0; i < FirstDefender; i++)
				PrintSoldier((Soldier)Soldiers[i]);
			BattleReport.Add("");
			BattleReport.Add("Defenders:|Защитники:");
			for (int i = FirstDefender; i < Soldiers.Count; i++)
				PrintSoldier((Soldier)Soldiers[i]);

			this.Run();

			// Promote skills
			foreach (Soldier s in Soldiers) 
				if (!s.Person.Killed && s.Person.Leader == null)
					s.PromoteSkill();

			// Set had-battle flag
			foreach (Soldier s in Soldiers)
				s.Person.HadBattle = true;

			// Set AVOID on incapacitated persons
			foreach (Soldier s in Soldiers)
				if (s.OutOfAction && !s.FleedAway && !s.Person.Killed) 
				{
					s.Person.Avoiding = true;
					s.Person.Patrolling = false;
				}

			BattleReport.Add("");
			if (AttackersOOA == FirstDefender) 
			{
				BattleReport.Add(String.Format("{0} wins.|{1} побеждает.",
					defender.ToString(Lang.En), defender.ToString(Lang.Ru)));
				CollectSpoils(region, false, Side.Defender);
				DefendersWin = true;
			}
			else if (DefendersOOA == Soldiers.Count - FirstDefender) 
			{
				BattleReport.Add(String.Format("{0} wins.|{1} побеждает.",
					attackers[0].ToString(Lang.En), attackers[0].ToString(Lang.Ru)));
				CollectSpoils(region, false, Side.Attacker);
				AttackersWin = true;
			}
			else 
			{
				BattleReport.Add("The battle is a draw.|Бой закончен вничью.");
				CollectSpoils(region, true, Side.Attacker);
			}
			BattleReport.Add("");
		}

		private void PrintSoldier(Soldier soldier) 
		{
			// En
			string s = soldier.Person.ToString(Lang.En);
			if (!soldier.Person.HideFaction && soldier.Person.Faction != null)
				s += ", " + soldier.Person.Faction.ToString(Lang.En);
			if (soldier.Person.Avoiding)
				s += ", avoiding";
			if (soldier.Weapon != null) 
			{
				if (soldier.Weapon.IsMan)
					s += ", unarmed";
				else
					s += ", " + soldier.Weapon.ToString(1, Lang.En);
				if (soldier.Weapon.Ammo != null)
					s += " (ammo: " + soldier.Ammo.ToString() + ")";
			}
			if (soldier.Armor != null && !soldier.Armor.IsMan)
				s += ", " + soldier.Armor.ToString(1, Lang.En);
			s += ", skill " + soldier.SkillLevel.ToString();
			s += ", reflexes " + soldier.Reflexes;
			if (soldier.Building != null)
				s += ", inside " + soldier.Building.ToString(Lang.En) + " (" + 
					Hits[soldier.Building].ToString() + " hp)";
			s += ".";

			// Ru
			s += "|" + soldier.Person.ToString(Lang.Ru);
			if (!soldier.Person.HideFaction && soldier.Person.Faction != null)
				s += ", " + soldier.Person.Faction.ToString(Lang.Ru);
			if (soldier.Person.Avoiding)
				s += ", избегает боя";
			if (soldier.Weapon != null) 
			{
				if (soldier.Weapon.IsMan)
					s += ", без оружия";
				else
					s += ", " + soldier.Weapon.ToString(1, Lang.Ru);
				if (soldier.Weapon.Ammo != null)
					s += " (заряды: " + soldier.Ammo.ToString() + ")";
			}
			if (soldier.Armor != null && !soldier.Armor.IsMan)
				s += ", " + soldier.Armor.ToString(1, Lang.Ru);
			s += ", навык " + soldier.SkillLevel.ToString();
			s += ", рефлексы " + soldier.Reflexes;
			if (soldier.Building != null)
				s += ", объект: " + soldier.Building.ToString(Lang.Ru) + " (" + 
					Hits[soldier.Building].ToString() + " хитов)";
			s += ".";

			BattleReport.Add(s);
		}

		private void Run() 
		{
      ArrayList reflexes = new ArrayList();
			foreach (Soldier s in Soldiers) 
				if (!reflexes.Contains(s.Reflexes))
					reflexes.Add(s.Reflexes);
			reflexes.Sort();

			for (int turn = 1; turn <= 50; turn++) 
			{
				bool shooters = (turn == 1 && ShootersPresent());
				BattleReport.Add("");
				BattleReport.Add(String.Format("Round {0}{1}|Раунд {0}{2}", 
					turn.ToString(), 
					(shooters ? " (ranged only)" : ""),
					(shooters ? " (только стрелки)" : "")
					));

				// Process all soldiers in order of reflexes
				bool attackers_moved = false;
				bool defenders_moved = false;
				for (int i = reflexes.Count-1; i >= 0; i--) 
				{
					ArrayList acting = new ArrayList();
					foreach (Soldier s in Soldiers)
						if (s.Reflexes == (int)reflexes[i])
							acting.Add(s);

					while (acting.Count > 0) 
					{
            int idx = Constants.Random(acting.Count);
						Soldier s = (Soldier)acting[idx];
						if (!s.OutOfAction) 
						{
							try 
							{
								MoveSoldier(s, turn);
								if (s.Side == Side.Attacker) attackers_moved = true;
								else defenders_moved = true;
							}
							catch (NoTargetException) 
							{
								BattleReport.Add(String.Format("{0}: can't attack anybody.|{1}: не может никого атаковать.",
									s.Person.ToString(Lang.En), s.Person.ToString(Lang.Ru)));
							}
						}
						acting.RemoveAt(idx);

						if (BattleEnded())
							return;
					}
				}

				if (turn > 1) 
				{
					if (!attackers_moved) 
					{
						BattleReport.Add("Attackers panicked and ran away.|Атакующие запаниковали и убегают.");
						for (int i = 0; i < FirstDefender; i++)
							((Soldier)Soldiers[i]).Flee = true;
					}
					if (!defenders_moved) 
					{
						BattleReport.Add("Defenders panicked and ran away.|Защитники запаниковали и убегают.");
						for (int i = FirstDefender; i < Soldiers.Count; i++)
							((Soldier)Soldiers[i]).Flee = true;
					}
				}
			}
		}

		private bool ShootersPresent() 
		{
			// Check if shooters present
			foreach (Soldier s in Soldiers)
				if (s.Weapon != null && s.Weapon.Ranged && !s.Flee && !s.OutOfAction)
					return true;
			return false;
		}

		private void MoveSoldier(Soldier s, int round) 
		{
			if (s.Weapon == null || s.SkillLevel == 0 || s.Person.Avoiding)
				s.Flee = true;

			if (s.Flee) 
			{
				OOASoldier(s);
				s.FleedAway = true;
				BattleReport.Add(String.Format("{0} ran away from combat.|{1} убегает из боя.",
					s.Person.ToString(Lang.En), s.Person.ToString(Lang.Ru)));
				return;
			}

			// First turn is for ranged weapon only if shooters on field
			if (round == 1 && ShootersPresent() && !s.Weapon.Ranged)
				return;
			
			for (int i = 0; i < s.Weapon.Attacks; i++) 
			{
				if (OutOfAmmo(s)) 
					break;
			
				for (int j = 0; j < s.Weapon.Targets; j++) 
				{
					MakeHit(s);
					if (BattleEnded()) 
						break;
				}

				if (s.Ammo > 0)
					s.Ammo--;	
				if (s.Weapon.Ammo != null) 
				{
					if (s.Weapon.Ammo.Case != null)
						Spoils.AddItems(s.Weapon.Ammo.Case, 1);
					s.Person.Items.RemoveItems(s.Weapon.Ammo, 1);
				}
				if (OutOfAmmo(s)) 
					break;

				if (BattleEnded())
					break;
			}
		}

		private bool OutOfAmmo(Soldier s) 
		{
			if (s.Weapon.Ammo != null && s.Ammo == 0) 
			{
				BattleReport.Add(String.Format("{0} is out of ammo.|{1}: кончились заряды.",
					s.Person.ToString(Lang.En), s.Person.ToString(Lang.Ru)));
				s.Flee = true;
				return true;
			}
			return false;
		}

		private bool BattleEnded() 
		{
			return (AttackersOOA == FirstDefender || 
							DefendersOOA == Soldiers.Count - FirstDefender);
		}

		private void MakeHit(Soldier s) 
		{
			if (s.Weapon.AntiTank)
				HitTank(s);
			else if (s.Weapon.Explosive)
				ExplodeTank(s);
			else
				HitPerson(s);
		}

		private void ExplodeTank(Soldier s) 
		{
			// Pick target
			Building target = TargetBuilding(s);

			// If no buildings to attack, flee
			if (target == null) 
			{
				s.Flee = true;
				throw new NoTargetException();
			}

			HitEvent evt = new HitEvent(s.Person, target);
			BattleReport.Add(evt);

			// Roll to hit
			if (!RollToHit(s, evt))
				return;

			// Target is hit, explode
			evt.Results.Add(HitEvent.Code.Exploded);
			target.Damage();

			// Hit soldiers
			ArrayList targets = new ArrayList();
			foreach (Soldier s2 in Soldiers)
				if (s2.Building == target) 
					targets.Add(s2);
			int escape_roll = Constants.Random(100);
			if (escape_roll < s.SkillLevel)
				targets.Add(s);

			foreach (Soldier s2 in targets)
			{
				s2.Building = null;

				// Roll to wound
				int towound_roll = Constants.Random(100);
				if (towound_roll == 99)
					continue;
				if (towound_roll > 0 && towound_roll >= s.Weapon.ToWound)
					continue;
        
        // Incapacitate target
				KillTarget(s, s2, evt);
			}

			s.Person.Items.RemoveItems(s.Weapon, 1);
			s.Flee = true;
		}

		private void HitTank(Soldier s) 
		{
      // Pick target
			Building target = TargetBuilding(s);

			// If no buildings to attack, attack persons
			if (target == null) 
			{
				HitPerson(s);
				return;
			}

			HitEvent evt = new HitEvent(s.Person, target);
			BattleReport.Add(evt);

			// Roll to hit
			if (!RollToHit(s, evt))
				return;

			// Roll to wound
			if (!RollToWound(s, evt))
				return;

			// Target is hit, decrease hp
			Hits[target] = (int)Hits[target] - s.Weapon.HP;
			if ((int)Hits[target] > 0) 
			{
				evt.Hits = s.Weapon.HP;
			}
			else 
			{
				evt.Results.Add(HitEvent.Code.Damaged);
				target.Damage();

				// Move soldiers outside
				foreach (Soldier s2 in Soldiers)
					if (s2.Building == target)
						s2.Building = null;
			}
		}

		private Building TargetBuilding(Soldier s) 
		{
			ArrayList targets = new ArrayList();
			int st = 0;
			int en = Soldiers.Count-1;
			if (s.Side == Side.Attacker)
				st = FirstDefender;
			else
				en = FirstDefender - 1;

			for (int j = st; j <= en; j++) 
			{
				Soldier tg = (Soldier)Soldiers[j];
				if (tg.Building != null && !targets.Contains(tg.Building))
					targets.Add(tg.Building);
			}

			if (targets.Count == 0)
				return null;
			else
				return (Building)targets[Constants.Random(targets.Count)];
		}

		private void HitPerson(Soldier s) 
		{
			// Pick target
			ArrayList targets = new ArrayList();
			int st = 0;
			int en = Soldiers.Count-1;
			if (s.Side == Side.Attacker)
				st = FirstDefender;
			else
				en = FirstDefender - 1;

			if (s.Weapon.Ranged) 
			{
				// Ranged weapons
				for (int pass = 1; pass <= 3; pass++) 
				{
					for (int j = st; j <= en; j++) 
					{
						Soldier tg = (Soldier)Soldiers[j];
						
						// Ranged weapons does not target units inside buildings
						if (tg.OutOfAction || tg.Building != null)
							continue;

						// Fleeing persons and chosen attacked last
						if (pass == 1 && (tg.Flee || tg.Person.Chosen))
							continue;
						if (pass == 2 && tg.Person.Chosen)
							continue;
						targets.Add(tg);
					}
					
					if (targets.Count > 0)
						break;
				}
			}
			else 
			{
				// Melee weapons
				// If soldier inside building, go out
				if (s.Building != null) 
				{
					BattleReport.Add(String.Format("{0} goes out.|{1} выходит наружу.",
						s.Person.ToString(Lang.En), s.Person.ToString(Lang.Ru)));
					s.Building = null;
				}

        // Pick target
				for (int pass = 1; pass <= 2; pass++) 
				{
					for (int j = st; j <= en; j++) 
					{
						Soldier tg = (Soldier)Soldiers[j];
						if (tg.OutOfAction)
							continue;
						if (pass == 1 && tg.Person.Chosen)
							continue;
						targets.Add(tg);
					}
					
					if (targets.Count > 0)
						break;
				}
			}

			// Noone to attack
			if (targets.Count == 0) 
				throw new NoTargetException();

			Soldier target = (Soldier)targets[Constants.Random(targets.Count)];

			HitEvent evt = new HitEvent(s.Person, target.Person);
			BattleReport.Add(evt);

			// Roll to hit
			if (!RollToHit(s, evt)) 
				return;

			if (!s.Weapon.Ranged && (target.Weapon != null && !target.Weapon.Ranged)) 
			{
				// Roll to parry
				int parry_roll = Constants.Random(100);
				if (parry_roll == 99) 
				{
					evt.Results.Add(HitEvent.Code.CriticalBlock);
					return;
				}
				if (parry_roll == 0) 
					evt.Results.Add(HitEvent.Code.CriticalBlockFail);
				else
				{
					if (target.SkillLevel > parry_roll) 
					{
						evt.Results.Add(HitEvent.Code.Block);
						return;
					}
				}
			}

			// Anti-tank weapons ignore wound rolls and armor save
			if (!s.Weapon.AntiTank) 
			{
				// Armor save
				int save_roll = Constants.Random(100);
				if (save_roll == 99) 
				{
					evt.Results.Add(HitEvent.Code.CriticalSave);
					return;
				}
				if (save_roll == 0) 
					evt.Results.Add(HitEvent.Code.CriticalSaveFail);
				else
				{
					if (s.Weapon != null)
						save_roll -= s.Weapon.ArmorModifier;
					if (target.Armor != null && save_roll < target.Armor.ArmorSave) 
					{
						evt.Results.Add(HitEvent.Code.ArmorSave);
						return;
					}
				}

				// Roll to wound
				if (!RollToWound(s, evt))
					return;
			}

			// Target is hit
      KillTarget(s, target, evt);
		}

		private void KillTarget(Soldier s, Soldier target, HitEvent evt) 
		{
			OOASoldier(target);

			// Check hit lethality
			bool killed;
			if (s.Weapon.AntiTank) 
				killed = true;
			else 
			{
				int lethality_roll = Constants.Random(100);
				if (s.Weapon != null && lethality_roll < s.Weapon.Lethality)
					killed = true;
				else
					killed = false;
			}

			if (killed) 
			{
				target.Person.Kill(false);
				evt.Results.Add(HitEvent.Code.Killed);
			}
			else
				evt.Results.Add(HitEvent.Code.Incapacitated);
		}

		private bool RollToHit(Soldier s, HitEvent evt) 
		{
			int tohit_roll = Constants.Random(100);
			if (tohit_roll == 0) 
			{
				evt.Results.Add(HitEvent.Code.CriticalHitFail);
				return false;
			}
			if (tohit_roll == 99) 
				evt.Results.Add(HitEvent.Code.CriticalHit);
			else
			{
				tohit_roll -= s.Weapon.HitModifier;
				if (tohit_roll >= s.SkillLevel) 
				{
					evt.Results.Add(HitEvent.Code.NoHit);
					return false;
				}
			}
			return true;
		}

		private bool RollToWound(Soldier s, HitEvent evt) 
		{
			int towound_roll = Constants.Random(100);
			if (towound_roll == 0) 
			{
				evt.Results.Add(HitEvent.Code.CriticalWoundFail);
				return false;
			}
			if (towound_roll == 99) 
				evt.Results.Add(HitEvent.Code.CriticalWound);
			else
			{
				int towound;
				towound = s.Weapon.ToWound;
				if (towound_roll >= towound) 
				{
					evt.Results.Add(HitEvent.Code.NoWound);
					return false;
				}
			}
			return true;
		}

		private void OOASoldier(Soldier target) 
		{
			target.OutOfAction = true;
			if (target.Side == Side.Attacker)
				AttackersOOA++;
			else
				DefendersOOA++;
		}

		private void CollectSpoils(Region r, bool draw, Side winner) 
		{
			ArrayList spoil_lists = new ArrayList();
			spoil_lists.Add(Spoils);
			foreach (Soldier s in Soldiers) 
			{
				if (s.OutOfAction && !s.FleedAway) 
				{
					if (s.Person.Killed)
						// If person killed, add items to Spoils
						for (int i = s.Person.Items.Count-1; i >= 0; i--) 
						{
							Item itm = s.Person.Items[i];
							if (!itm.Type.NoGive) 
							{
								s.Person.Items.RemoveAt(i);
								Spoils.AddItems(itm.Type, itm.Amount);
							}
						}
					else
						if (!draw && s.Side != winner)
							// If person stunned, allow to marauder givable items from it
							spoil_lists.Add(s.Person.Items);
				}
				
				// If person killed, drop "drops" to Spoils
				if (s.OutOfAction && s.Person.Killed && s.Person.Man.Drops.Count > 0) 
				{
					foreach (Item itm in s.Person.Man.Drops)
						Spoils.AddItems(itm.Type, itm.Amount);
				}
			}

			if (!draw) 
			{
				foreach (Soldier s in Soldiers) 
				{
					if (s.OutOfAction) continue;
					int space = s.Person.GetCapacity(Movement.Walk) - s.Person.GetWeight();
					ItemList taken = new ItemList();

					// Get item requested in spoils
					if (s.Person.Spoils.Count > 0) 
					{
						foreach (ItemType wanted in s.Person.Spoils) 
						{
							if (wanted.NoGive)
								continue;

							for (int i = spoil_lists.Count-1; i >= 0; i--)
							{
								ItemList list = (ItemList)spoil_lists[i];
								Item item = list.GetByType(wanted);
								if (item == null) 
									continue;
								int amt;
								if (item.Type.Weight == 0)
									amt = Math.Min(item.Amount, Constants.Random(20));
								else
									amt = Math.Min(item.Type.Weight * item.Amount, space) / item.Type.Weight;
                if (amt == 0)
									continue;
								s.Person.Items.AddItems(item.Type, amt);
								space -= item.Type.Weight * amt;
								list.RemoveItems(item.Type, amt);
								if (list.Count == 0)
									spoil_lists.Remove(list);
								taken.AddItems(item.Type, amt);
							}

							if (taken.Count > 0)
								break;
						}
					}

					// Get random items from spoils (as much as soldier can carry)
					if (taken.Count == 0) 
					{
						int attempt = 0;
						while (attempt < 2)
						{
							if (spoil_lists.Count == 0)
								break;
							int list_idx = Constants.Random(spoil_lists.Count);
							ItemList givable = new ItemList();
							foreach (Item itm in (ItemList)spoil_lists[list_idx])
								if (!itm.Type.NoGive)
									givable.Add(itm);
							if (givable.Count == 0) 
							{
								spoil_lists.RemoveAt(list_idx);
								continue;
							}
							int item_idx = Constants.Random(givable.Count);
							Item item = givable[item_idx];
							int amt;
							if (item.Type.Weight == 0)
								amt = Math.Min(item.Amount, Constants.Random(20));
							else
								amt = Math.Min(item.Type.Weight * item.Amount, space) / item.Type.Weight;
							if (amt > 0) 
							{
								s.Person.Items.AddItems(item.Type, amt);
								space -= item.Type.Weight * amt;
								((ItemList)spoil_lists[list_idx]).RemoveItems(item.Type, amt);
								taken.AddItems(item.Type, amt);
							}
							attempt++;
						}
					}

					if (taken.Count > 0)
						BattleReport.Add(String.Format("{0} takes {1}|{2} берёт: {3}",
							s.Person.ToString(Lang.En), taken.ToString(Lang.En),
							s.Person.ToString(Lang.Ru), taken.ToString(Lang.Ru)));
				}
			}

			// Add untaken spoils to Junk
			for (int i = Spoils.Count-1; i >= 0; i--)
				r.Junk.AddItems(Spoils[i].Type, Spoils[i].Amount);
		}
	}

	public enum Side 
	{
		Attacker,
		Defender
	}

	public class Soldier 
	{
		public Person Person = null;
		public Side Side = Side.Attacker;
		public ItemType Weapon = null;
		public ItemType Armor = null;
		public Building Building = null;
		public int Reflexes = 0;
		public int SkillLevel = 0;
		public int Ammo = 0;
		public bool OutOfAction = false;
		public bool FleedAway = false;
		public bool Flee = false;

		public Soldier(Battle btl, Person p, Side side) 
		{
			Person = p;
			Side = side;

			if (btl != null)
				p.Faction.BattleRegions.Add(btl.region);

			Flee = p.Avoiding;
			if (p.Building != null && p.Building.Type.HP > 0
				&& p.Building.IsPersonInside(p)) 
			{
				Building = p.Building;
				if (btl != null)
					btl.Hits[p.Building] = p.Building.Type.HP;
			}
			
			Skill reflexes = p.Skills.GetByType(Constants.ReflexesSkill);
			if (reflexes != null)
				Reflexes = reflexes.Level;

			// Look for weapons and armor in person inventory
			ItemList equipment = new ItemList();
			foreach (Item itm in p.Items)
				if ((itm.Type.IsWeapon && !itm.Type.Heavy)
					|| itm.Type.IsArmor)
					equipment.Add(itm);

			// Look for heavy weapon installed in building
			if (p.Building != null)
				foreach (Item itm in p.Building.Installed)
					if (itm.Type.IsWeapon && (btl == null || btl.TakenHwpn[itm] == null
						|| (int)btl.TakenHwpn[itm] < itm.Amount)) 
						equipment.Add(itm);

			// Take items by predefined priority
			foreach (ItemType it in p.Equipment)
				TakeItem(btl, this, equipment, it);

			// Take items by default
			if (Weapon == null || Armor == null)
				for (int i = ItemType.List.Count-1; i >= 0; i--)
					TakeItem(btl, this, equipment, ItemType.List[i]);
		}

		public void TakeItem(Battle btl, Soldier s, ItemList equipment, ItemType it) 
		{
      Item itm = equipment.GetByType(it);
			if (itm == null)
				return;

			// Get weapon
			if (itm.Type.IsWeapon && Weapon == null)
			{
				// Check skill
				if (itm.Type.WeaponSkill == null) 
					throw new Exception("Weapon should have WeaponSkill");
				Skill sk = Person.Skills.GetByType(itm.Type.WeaponSkill);
				if (sk == null)
					return;

				// Check ammo
				if (itm.Type.Ammo != null) 
				{
					Item ammo = s.Person.Items.GetByType(itm.Type.Ammo);
					if (ammo != null)
						Ammo = ammo.Amount;
					if (Ammo == 0)
						return;
				}

				// Get the weapon
				Weapon = itm.Type;
				SkillLevel = sk.Level;
				if (itm.Type.Heavy && btl != null) 
				{
					if (btl.TakenHwpn[itm] == null)
						btl.TakenHwpn[itm] = 1;
					else
						btl.TakenHwpn[itm] = (int)btl.TakenHwpn[itm] + 1;
				}

			}

			// Get armor
			if (itm.Type.IsArmor && Armor == null)
				Armor = itm.Type;
		}

		public void PromoteSkill() 
		{
			if (Weapon != null)
				this.Person.PromoteSkill(Weapon.WeaponSkill, true);
			else if (Person.Skills.GetByType(Constants.ReflexesSkill) != null)
				this.Person.PromoteSkill(Constants.ReflexesSkill, true);
		}
	}

	public class NoTargetException : Exception 
	{
	}


}