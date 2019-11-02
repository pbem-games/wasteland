using System;
using System.IO;
using System.Collections;

namespace Wasteland 
{
	class Report 
	{
		private static TextWriter tw;
		private static Faction faction = null;
		private static Lang lng = Lang.En;

		public static void Generate(string folder) 
		{
			// Recalculate resources for next turn
			Map.Turn++;
			Map.CalcTurnResources();
			Map.Turn--;

			foreach (Faction f in Faction.List)
			{
				faction = f;
				lng = f.Options.Lang;
				GenerateFaction(Path.Combine(folder, String.Format("report.{0}", f.Num)));
			}
		}

		private static void GenerateFaction(string filename) 
		{
			Faction f = faction;
			tw = new StreamWriter(filename, false, System.Text.Encoding.GetEncoding(1251));

			// Header
			Write("To: " + f.Email);
			Write("Subject: [Wasteland] Report for turn " + Map.Turn.ToString());
			Write("Content-Disposition: attachment");
			Write("");
			Write("Wasteland Report For:|Отчёт фракции:");
			Write(f.ToString(lng));
			Write(String.Format("Turn {0}, {1}|Ход {0}, {2}",
				Map.Turn, Month.Current.NameEn, Month.Current.NameRu));
			Write("");

			if (f.Options.TextReport) 
			{
				// Engine
				Write("Wasteland Engine Version: " + MainClass.EngineVersion + 
					"|Версия сервера Wasteland: " + MainClass.EngineVersion);
				Write("");
	
				//if (!faction.IsNPC) 
				{
					// Skill reports
					if (f.SkillsToShow.Count > 0) 
					{
						Write("Skill reports:|Описания навыков:");
						Write("");
						foreach (SkillType st in f.SkillsToShow) 
							WriteSkillReport(st);
					}

					// Item reports
					if (f.ItemsToShow.Count > 0) 
					{
						Write("Item reports:|Описания вещей:");
						Write("");
						foreach (ItemType it in f.ItemsToShow) 
							WriteItemReport(it);
					}

					// Object reports
					if (f.BuildingsToShow.Count > 0) 
					{
						Write("Object reports:|Описания объектов:");
						Write("");
						foreach (BuildingType bt in f.BuildingsToShow) 
							WriteBuildingReport(bt);
					}

					// Battles
					foreach (Region r in Map.Regions) 
					{
						if (r.BattleReports.Count == 0 || (!RegionIsVisible(r, f)
							&& !f.BattleRegions.Contains(r)))
							continue;

						Write("Battle reports:|Отчёты о сражениях:");
						Write("");
						foreach (ArrayList BattleReport in r.BattleReports) 
						{
							foreach (string s in BattleReport)
								Write(s);
							Write("");
						}
					}
				}

				// Events
				Write("Events during turn:|События этого хода:");
				foreach (string s in f.Events)
					Write(s);
				Write("");

				// Attitudes
				Write(String.Format("Declared Attitudes (default {0}):|Отношения (по умолчанию {0}):",
					f.DefaultAttitude.ToString()));
				for (Attitude a = Attitude.Hostile; a <= Attitude.Ally; a++) 
					Write(a + " : " + AttitudeListString(f, a));
				Write("");

				// Regions
				foreach (Region r in Map.Regions) 
				{
					if (!faction.IsNPC && !RegionIsVisible(r, f))
						continue;

					// Print region
					Write("");
					Write(r.ToString(lng));
					Write("------------------------------------------------------------");

					// Weather and resources shown is for next turn
					Map.Turn++;
					Write(String.Format("  Weather forecast: {0}, {1}°C, {2} mR/h.|" +
						"  Прогноз погоды: {3}, {1}°C, {2} мР/ч.",
						r.Weather.ToString(Lang.En), r.Temperature,
						r.Radiation, r.Weather.ToString(Lang.Ru)));
					Write( (lng == Lang.En ? "  Resources: " : "  Ресурсы: ") + 
						r.TurnResources.ToString(lng));
					Map.Turn--;

					Write( (lng == Lang.En ? "  Junk: " : "  Мусор: ") + 
						r.Junk.ToString(lng));
				
					Write("");

					// Exits
					Write("Exits:|Выходы:");
					for (Direction i = Direction.North; i <= Direction.Northwest; i++) 
					{
						Region exit = r.RegionInDir((Direction)i);
						if (exit != null) 
						{
							if (lng == Lang.Ru)
								Write("  " + Map.DirectionFullNamesRu[(int)i - 1] + " : " + exit.ToString(lng));
							else
								Write("  " + i.ToString() + " : " + exit.ToString(lng));
						}
					}
					Write("");
				
					// Persons
					WritePersons(r, null, f);
					Write("");

					// Buildings and persons inside
					foreach (Building b in r.Buildings) 
					{
						string s = "+ " + b.ToString(lng) + " : " + b.Type.ToString(lng);
						if (b.Installed.Count > 0)
							s += ", " + b.Installed.ToString(lng);
						else 
							s += ".";
						ItemArrayList needs = b.GetNeeds();
						if (needs.Count > 0)
							s += (lng == Lang.En ? " Needs: " : " Нужно: ") + needs.ToString(lng);
						
						if (b.Description != "") 
						{
							if (lng == Lang.En)
								s = s.Substring(0, s.Length-1) + "; " + MyStrings.Translit(b.Description);
							else
								s = s.Substring(0, s.Length-1) + "; " + b.Description;
						}

						Write(s);
						WritePersons(r, b, f);
						Write("");
					}

				}

				if (faction.IsNPC)
					return;

				// Orders template
				Write("");
				Write("Orders Template:|Шаблон приказов:");
				Write("");
				Write(String.Format("#orders {0} \"{1}\"", f.Num, f.Password));
				Write("");

				foreach (Person p in f.Persons) 
				{
					Write(String.Format("person {0}", p.Num));
					foreach (string s in p.RepeatingLines) 
						Write(s);
					Write("");
				}

				Write("#end");
				Write("");
			}

			if (faction.Options.XmlReport) 
			{
				Write("");
				Write("XML version (for client applications):|XML-версия (для программ-клиентов):");
				Write("");
				Write(XmlReport.Generate(faction), false);

				/*
				TextWriter tw1 = new StreamWriter("report.xml", false, System.Text.Encoding.GetEncoding(1251));
				tw1.Write(XmlReport.Generate(faction));
				tw1.Close();
				*/
			}

			tw.Close();
			f.AllShown();
		}

		private static bool RegionIsVisible(Region r, Faction f) 
		{
			int j = r.Persons.Count-1;
			while (j >= 0 && ((Person)r.Persons[j]).Faction != f) j--;
			return (j >= 0);
		}

		private static void Write(string line) 
		{
			Write(line, true);
		}

		private static void Write(string line, bool indent) 
		{
			// Take part for language
			if (line.IndexOf("|") >= 0) 
			{
				if (faction.Options.Lang == Lang.En)
					line = line.Substring(0, line.IndexOf("|"));
				else
					line = line.Substring(line.IndexOf("|")+1);
			}

			// Count indent to wrap
			string st1 = "";
			if (indent) 
			{
				st1 = "  ";
				int i = 0;
				while (i < line.Length && line[i] == ' ') 
				{
					st1 += " ";
					i++;
				}
			}

			// Write by lines
			while (true) 
			{
				int j = 71;
				while (line.Length > j && line[j] != ' ') j--;
				if (line.Length > j) 
				{
					tw.WriteLine(line.Substring(0, j));
					line = st1 + line.Substring(j+1);
				}
				else 
				{
					tw.WriteLine(line);
					break;
				}
			}
			tw.Flush();
		}

		private static void WritePersons(Region r, Building b, Faction f) 
		{
			// Write team leaders with teams
			int indent = (b != null) ? 1 : 0;
			foreach (Person p in r.Persons) 
			{
				if (p.Building == b && p.Leader == null) 
				{
					WritePerson(p, f, indent);
					foreach (Person subordinate in p.Team)
						WritePerson(subordinate, f, indent + 1);
				}
			}
		}

		private static void WritePerson(Person p, Faction f, int indent) 
		{
			Lang lng = faction.Options.Lang;

			string s = "";
			if (p.Faction == f) s = "* ";
			else s = "- ";
			for (int i = 0; i < indent; i++)
				s = "  " + s;
			s += p.ToString(lng);
			
			if (p.Faction == f || !p.HideFaction)
				s += ", " + p.Faction.ToString(lng);

			if (p.Insanity >= Constants.DangerousInsanity)
				s += (lng == Lang.En ? ", insane" : ", безумен");
			if (p.Chosen)
				s += (lng == Lang.En ? ", chosen one" : ", избранный");
			if (p.Patrolling)
				s += (lng == Lang.En ? ", patrolling" : ", патрулирует");
			if (p.Age <= Constants.ChildTurns) 
				s += (lng == Lang.En ? ", child" : ", ребёнок");

			if (p.Faction == f) 
			{
				if (p.Avoiding)
					s += (lng == Lang.En ? ", avoiding" : ", избегает боя");
				if (p.Hide)
					s += (lng == Lang.En ? ", hiding" : ", скрывается");
				else if (p.HideFaction)
					s += (lng == Lang.En ? ", hiding faction" : ", скрывает фракцию");
			}

			if (p.Faction == faction)
				s += ", " + p.Items.ToString(lng);
			else 
			{
				ItemArrayList items = new ItemArrayList();
				foreach (Item itm in p.Items)
					if (itm.Type.Weight > 0)
						items.Add(itm);
				s += ", " + items.ToString(lng);
			}

			if (p.Faction == f) 
			{
				int weight = p.GetWeight();
				if (p.Man != null)
					weight -= p.Man.Weight;

				s += (lng == Lang.En ? " Load: " : " Груз: ") + 
					weight.ToString() + ((lng == Lang.En) ? " kg." : " кг.");
				s += (lng == Lang.En ? " Skills: " : " Навыки: ") + 
					p.Skills.ToString(lng);

				if (p.Consume.Count > 0)
					s += (lng == Lang.En ? " Consuming: " : " Еда: ") +
						p.Consume.ToString(lng);
				if (p.Burn.Count > 0)
					s += (lng == Lang.En ? " Burning: " : " Топливо: ") +
						p.Burn.ToString(lng);
				if (p.Equipment.Count > 0)
					s += (lng == Lang.En ? " Equipment: " : " Снаряжение: ") +
						p.Equipment.ToString(lng);

				s += (lng == Lang.En ? " Insanity: " : " Безумие: ") + 
					p.Insanity.ToString() + ".";
			
				int hire = p.GetHireAmount();
				if (hire == 1)
					s += (lng == Lang.En ? " Hire demand: day ration." : " Найм: дневной рацион.");
				else if (hire > 1)
					s += String.Format((lng == Lang.En ? " Hire demand: {0} day rations." :
						" Найм: {0} рационов."), hire);

				int rad_danger = Math.Abs(p.RadiationDanger(true));
				int tempr_danger = p.TemperatureDanger(true);

				if (rad_danger > 0 || tempr_danger > 0) 
				{
					s += (lng == Lang.En ? " Danger:" : " Опасность:");
					if (rad_danger > 0)
						s += " " + rad_danger.ToString() + (lng == Lang.En ? " mR/h" : " мР/ч");
					if (tempr_danger > 0) 
					{
						if (rad_danger > 0) s += ",";
						s += " " + tempr_danger.ToString() + (lng == Lang.En ? "°C" : "°C");
					}
					s += ".";
				}
			}

			if (p.TradeOrder != null) 
			{
				Person receiver = null;
				if (p.TradeOrder.PersonNum != 0)
					receiver = p.Region.Persons.GetByNumber(p.TradeOrder.PersonNum);
				if (p.TradeOrder.PersonNum == 0 || (receiver != null && receiver.Faction == f)) 
				{
					if (lng == Lang.En) 
					{
						s += " Trade: sells " + p.TradeOrder.SellWhat.ToString(p.TradeOrder.SellAmount, Lang.En) +
							" for " + p.TradeOrder.BuyWhat.ToString(p.TradeOrder.BuyAmount, Lang.En);
						if (receiver != null)
							s += " to " + receiver.ToString(Lang.En);
						s += ".";
					}
					else 
					{
						s += " Бартер: предлагает: " + p.TradeOrder.SellWhat.ToString(p.TradeOrder.SellAmount, Lang.Ru) +
							", просит: " + p.TradeOrder.BuyWhat.ToString(p.TradeOrder.BuyAmount, Lang.Ru);
						if (receiver != null)
							s += ", покупатель:  " + receiver.ToString(Lang.Ru);
						s += ".";
					}
				}
			}

			if (p.Description != "") 
			{
				if (lng == Lang.En)
					s = s.Substring(0, s.Length-1) + "; " + MyStrings.Translit(p.Description);
				else
					s = s.Substring(0, s.Length-1) + "; " + p.Description;
			}

			Write(s);
		}

		public static void WriteItemReport(ItemType it) 
		{
			string s = it.ToString(1, lng) + " : ";
			if (lng == Lang.En) 
			{
				s += "weight " + it.Weight.ToString() + " kg";
				if (it.Capacity[(int)Movement.Walk] > it.Weight)
					s += ", walking capacity "+ 
						(it.Capacity[(int)Movement.Walk] - it.Weight).ToString() + " kg";

				s += ".";

				if (it.Rations > 0)
					s += " This item counts as " + RationString(it.Rations, lng) + ".";

				if (it.ProduceSkill != null) 
				{
					s += String.Format(" Persons with {0} can PRODUCE this item",
						it.ProduceSkill.ToString(Lang.En));
					if (it.ProduceFrom1 != null) 
					{
						s += " from " + it.ProduceFrom1.ToString(Lang.En);
						if (it.ProduceFrom2 != null)
							s += " and " + it.ProduceFrom2.ToString(Lang.En);
					}
					if (it.ProduceBuilding != null)
						s += " inside a " + it.ProduceBuilding.Name;
					s += String.Format(" at a rate of {0} per turn.",
						it.ProductionRate);
				}

				if (it.ProduceAs != null)
					s += " This item produced as " + it.ProduceAs.ToString(1, Lang.En) + ".";

				if (it.InstallSkill != null) 
				{
					s += String.Format(" Persons with {0} can INSTALL this item",
						it.InstallSkill.ToString(Lang.En));

					BuildingTypeArrayList buildings = new BuildingTypeArrayList();
					foreach (BuildingType bt in BuildingType.List)
						if (bt.Materials.Count > 0 && bt.Materials[0].Type == it)
							buildings.Add(bt);
					if (buildings.Count > 0) 
						s += ", or use it to BUILD " + buildings.ToString(lng);
					else
						s += ".";
				}

				if (it.OwnerRadiation != 0)
					s += String.Format(" This item {0} radiation by {1} for its owner.",
						it.OwnerRadiation > 0 ? "raises":"lowers", Math.Abs(it.OwnerRadiation));

				if (it.RegionRadiation != 0)
					s += String.Format(" This item {0} radiation in region by {1}.",
						it.RegionRadiation > 0 ? "raises":"lowers", Math.Abs(it.RegionRadiation));

				if (it.OwnerTemperature != 0)
					s += String.Format(" This item protects its owner from {0}° cold.",
						it.OwnerTemperature);

				if (it.IsMan) 
				{
					if (it.RadiationFrom > 0 || (it.RadiationTo > 0 && it.RadiationTo != 1000))
						s += String.Format(" Acceptable radiation is between " +
							"{0} and {1}.", it.RadiationFrom, it.RadiationTo);

					if (it.Food.Count > 0)
						s += String.Format(" A person of this type can eat " + 
							it.Food.ToString(Lang.En));

					if (it.NoLearn)
						s += String.Format(" Persons of this type can't learn.");
				}

				if (it.Burn)
					s += " This item may be burned.";

				if (it.IsWeapon) 
				{
          s += " This is a";
					if (it.Ranged) s += " ranged";
					else s += " melee";
					if (it.Heavy) s += " heavy";
					if (it.AntiTank) s += " anti-tank";
					if (it.Explosive) s += " explosive";
					s += " weapon.";
					if (it.Attacks > 1)
						s += " It has " + it.Attacks.ToString() + " attacks.";
					if (it.Targets > 1)
						s += " It hits " + it.Targets.ToString() + " targets with single attack.";
					if (it.HitModifier != 0)
						s += " Hit modifier: " + (it.HitModifier > 0?"+":"") + it.HitModifier.ToString() + "%.";
					if (it.ArmorModifier != 0)
						s += " Armor modifier: " + (it.ArmorModifier > 0?"+":"") + it.ArmorModifier.ToString() + "%.";
					s += " Wound chance: " + it.ToWound.ToString() + "%.";
					if (it.HP > 0)
						s += " " + it.HP.ToString() + " object's HP inflicted by each hit.";
					if (!it.AntiTank) 
						s += " Target has " +	it.Lethality.ToString() + "% chance to die from direct hit.";
					if (it.Ammo != null)
						s += " Ammo required: " +	it.Ammo.ToString(1, Lang.En) + ".";
					if (it.WeaponSkill != null)
						s += " Skill required: " + it.WeaponSkill.ToString(Lang.En) + ".";
				}

				if (it.IsArmor)
					s += String.Format(" This armor provides {0}% armor save in combat.",
						it.ArmorSave);
			}
			else
			{
				s += "вес " + it.Weight.ToString() + " кг";
				if (it.Capacity[(int)Movement.Walk] > it.Weight)
					s += ", может нести " + 
						(it.Capacity[(int)Movement.Walk] - it.Weight).ToString() + " кг";

				s += ".";

				if (it.Rations > 0)
					s += " Питательная ценность " + RationString(it.Rations, lng) + ".";

				if (it.ProduceSkill != null) 
				{
					s += String.Format(" Персонаж с навыком {0} может производить " +
						"этот предмет",
						it.ProduceSkill.ToString(Lang.Ru));
					s += String.Format(" в количестве {0} за ход.",
						it.ProductionRate);
					if (it.ProduceFrom1 != null) 
					{
						s += " Необходимые материалы: " + it.ProduceFrom1.ToString(Lang.Ru);
						if (it.ProduceFrom2 != null)
							s += " и " + it.ProduceFrom2.ToString(Lang.Ru);
						s += ".";
					}
					if (it.ProduceBuilding != null)
						s += " Производство возможно внутри объекта: " + it.ProduceBuilding.Name;
				}

				if (it.ProduceAs != null)
					s += " Предмет производится как " + it.ProduceAs.ToString(1, Lang.Ru) + ".";

				if (it.InstallSkill != null) 
				{
					s += String.Format(" Персонаж с навыком {0} может монтировать и демонтировать " +
						"этот предмет",
						it.InstallSkill.ToString(Lang.Ru));

					BuildingTypeArrayList buildings = new BuildingTypeArrayList();
					foreach (BuildingType bt in BuildingType.List)
						if (!bt.NoBuild && bt.Materials.Count > 0 && bt.Materials[0].Type == it)
							buildings.Add(bt);
					if (buildings.Count > 0) 
						s += ", или использовать его для постройки объектов: " + buildings.ToString(lng);
					else
						s += ".";
				}

				if (it.OwnerRadiation != 0)
					s += String.Format(" Этот предмет {0} радиацию на {1} единиц для своего владельца.",
						it.OwnerRadiation > 0 ? "повышает":"понижает", Math.Abs(it.OwnerRadiation));

				if (it.RegionRadiation != 0)
					s += String.Format(" Этот предмет {0} радиацию региона на {1} единиц.",
						it.RegionRadiation > 0 ? "повышает":"понижает", Math.Abs(it.RegionRadiation));

				if (it.OwnerTemperature != 0)
					s += String.Format(" Предмет защищает владельца от {0}° холода.",
						it.OwnerTemperature);

				if (it.IsMan) 
				{
					if (it.RadiationFrom > 0 || (it.RadiationTo > 0 && it.RadiationTo != 1000))
						s += String.Format(" Приемлемое значение радиации - в промежутке от " +
							"{0} до {1} единиц.", it.RadiationFrom, it.RadiationTo);

					if (it.Food.Count > 0)
						s += String.Format(" Персонажи этого типа могут есть следующие предметы: " + 
							it.Food.ToString(lng));

					if (it.NoLearn)
						s += String.Format(" Персонажи этого типа не могут учиться.");
				}

				if (it.Burn)
					s += " Этот предмет можно сжечь.";

				if (it.IsWeapon) 
				{
					s += " Это";
					if (it.Ranged) s += " стрелковое";
					if (it.Heavy) s += " тяжёлое";
					if (it.AntiTank) s += " противотанковое";
					s += " оружие";
					if (!it.Ranged) s += " ближнего боя";
					if (it.Explosive) s += " взрывного действия";
					s += ".";
					if (it.Attacks > 1)
						s += " Количество атак: " + it.Attacks.ToString() + ".";
					if (it.Targets > 1)
						s += " Оно наносит повреждения " + it.Targets.ToString() + " целям за одну атаку.";
					if (it.HitModifier != 0)
						s += " Модификатор попадания: " + (it.HitModifier > 0?"+":"") + it.HitModifier.ToString() + "%.";
					if (it.ArmorModifier != 0)
						s += " Модификатор брони: " + (it.ArmorModifier > 0?"+":"") + it.ArmorModifier.ToString() + "%.";
					s += " Вероятность нанести ранение: " + it.ToWound.ToString() + "%.";
					if (it.HP > 0)
						s += " " + it.HP.ToString() + " HP выбивается из объекта каждым попаданием.";
					if (!it.AntiTank) 
						s += " Жертва имеет " +	it.Lethality.ToString() + "% шанс умереть после прямого попадания.";
					if (it.Ammo != null)
						s += " Тип зарядов: " +	it.Ammo.ToString(1, Lang.Ru) + ".";
					if (it.WeaponSkill != null)
						s += " Требуемый навык: " + it.WeaponSkill.ToString(Lang.Ru) + ".";
				}

				if (it.IsArmor)
					s += String.Format(" Это броня, которая предоставляет {0}% защиту в бою.",
						it.ArmorSave);
			}

			Write(s);
			Write("");
		}

		public static void WriteSkillReport(SkillType st) 
		{
			string s = st.ToString(lng) + " :";

			if (lng == Lang.En) 
			{
				if (st.DescriptionEn != "")
					s += " " + st.DescriptionEn;

				if (st.BasedOn != null)
					s += " This skill requires " + st.BasedOn.FullNameEn + 
						" [" + st.BasedOn.Name + "] to study.";

				// Production
				ItemTypeArrayList products = new ItemTypeArrayList();
				foreach (ItemType it in ItemType.List)
					if (it.ProduceSkill != null && it.ProduceSkill.Type == st)
						products.Add(it);
				if (products.Count > 0) 
					s += " A person with this skill may PRODUCE " + 
						products.ToString(lng);

				// Installation
				ItemTypeArrayList installs = new ItemTypeArrayList();
				foreach (ItemType it in ItemType.List)
					if (it.InstallSkill != null && it.InstallSkill.Type == st)
						installs.Add(it);
				if (installs.Count > 0)
					s += " A person with this skill can INSTALL and UNINSTALL " +
						installs.ToString(lng);

				// Building
				BuildingTypeArrayList buildings = new BuildingTypeArrayList();
				foreach (ItemType it in installs) 
				{
					foreach (BuildingType bt in BuildingType.List)
						if (!bt.NoBuild && bt.Materials.Count > 0 && bt.Materials[0].Type == it)
							buildings.Add(bt);
				}
				if (buildings.Count > 0) 
					s += " It allows to BUILD: " + buildings.ToString(lng);

				// Driving
				buildings.Clear();
				foreach (BuildingType bt in BuildingType.List)
					if (bt.DriveSkill != null && bt.DriveSkill.Type == st)
						buildings.Add(bt);
				if (buildings.Count > 0)
					s += " A person with this skill can DRIVE " + buildings.ToString(Lang.En);
			}
			else 
			{
				if (st.DescriptionRu != "")
					s += " " + st.DescriptionRu;

				if (st.BasedOn != null)
					s += " Для изучения необходим навык " + 
						st.BasedOn.ToString(lng) + ".";

				// Production
				ItemTypeArrayList products = new ItemTypeArrayList();
				foreach (ItemType it in ItemType.List)
					if (it.ProduceSkill != null && it.ProduceSkill.Type == st)
						products.Add(it);
				if (products.Count > 0) 
					s += " Навык позволяет производить предметы: " + 
						products.ToString(lng);

				// Installation
				ItemTypeArrayList installs = new ItemTypeArrayList();
				foreach (ItemType it in ItemType.List)
					if (it.InstallSkill != null && it.InstallSkill.Type == st)
						installs.Add(it);
				if (installs.Count > 0)
					s += " Навык позволяет монтировать и демонтировать предметы: " +
						installs.ToString(lng);

				// Building
				BuildingTypeArrayList buildings = new BuildingTypeArrayList();
				foreach (ItemType it in installs) 
				{
					foreach (BuildingType bt in BuildingType.List)
						if (!bt.NoBuild && bt.Materials.Count > 0 && bt.Materials[0].Type == it)
							buildings.Add(bt);
				}
				if (buildings.Count > 0) 
					s += " С его помощью могут быть построены: " + buildings.ToString(lng);

				// Driving
				buildings.Clear();
				foreach (BuildingType bt in BuildingType.List)
					if (bt.DriveSkill != null && bt.DriveSkill.Type == st)
						buildings.Add(bt);
				if (buildings.Count > 0)
					s += " Навык позволяет водить машины: " + 
						buildings.ToString(lng);
			}

			Write(s);
			Write("");
		}

		private static void WriteBuildingReport(BuildingType bt) 
		{
			string s = bt.ToString(lng) + ":";

			if (lng == Lang.En) 
			{
				if (bt.DriveSkill != null) 
				{
					s += String.Format(" This is a vehicle with capacity {0} kg. It can" +
						" move for {1} region{2} per turn", bt.Capacity, bt.Speed,
						bt.Speed == 1 ? "" : "s");
					if (bt.Fuel != null)
						s += ", spending 1 " + bt.Fuel.ToString(1, Lang.En) + " per region";
					s += ".";
					if (bt.DriveSkill != null)
						s += String.Format(" A person must know {0} to DRIVE this vehicle.",
							bt.DriveSkill.ToString(Lang.En));
					if (bt.DriveTerrain != null)
						s += " This vehicle can move in " + bt.DriveTerrain.FullNameEn + ".";
				}
				else
					s += " This is a building.";

				if (bt.Defence > 0)
					s += String.Format(" This structure provides protection to first {0} persons inside.",
						bt.Defence);

				if (bt.Radiation != 0)
					s += String.Format(" Radiation inside this structure is {0} by {1}.",
						bt.Radiation < 0 ? "lowered" : "raised", Math.Abs(bt.Radiation));

				if (bt.Temperature != 0)
					s += String.Format(" This structure protects persons inside from {0}° cold.",
						bt.Temperature);

				if (bt.Resource != null)
					s += String.Format(" Structure adds {0} to region resources.", bt.Resource.ToString(lng));

				if (bt.HP > 0)
					s += String.Format(" It provides defence in battle and has {0} hit points.", bt.HP);

				string materials = bt.Materials.ToString(lng);
				materials = materials.Substring(0, materials.Length-1);
				s += String.Format(" This structure requires " + 
					materials + " to complete.");
				if (bt.OptionalMaterials.Count > 0)
					s += " Optional components are: " + bt.OptionalMaterials.ToString(lng);

				if (bt.NoBuild)
					s += String.Format(" A person can't BUILD new objects of this type.");
			}
			else 
			{
				if (bt.DriveSkill != null) 
				{
					s += String.Format(" Это транспорт. Его грузоподъёмность {0} кг, скорость - {1} регионов за ход.", 
						bt.Capacity, bt.Speed);
					if (bt.Fuel != null)
						s += " Для передвижения на 1 регион требуется 1 " + bt.Fuel.ToString(1, Lang.Ru);
					s += ".";
					if (bt.DriveSkill != null)
						s += String.Format(" Для управления требуется навык {0}.",
							bt.DriveSkill.ToString(Lang.Ru));
					if (bt.DriveTerrain != null)
						s += " Машина может въезжать в " + bt.DriveTerrain.FullNameRu + ".";
				}
				else
					s += " Это здание.";

				if (bt.Defence > 0)
					s += String.Format(" Находящимися внутри считаются первые {0} персонажей.",
						bt.Defence);

				if (bt.Radiation != 0)
					s += String.Format(" Радиация внутри {0} на {1}.",
						bt.Radiation < 0 ? "понижена" : "повышена", Math.Abs(bt.Radiation));

				if (bt.Temperature != 0)
					s += String.Format(" Объект защищает от {0}° холода.",
						bt.Temperature);

				if (bt.HP > 0)
					s += String.Format(" Объект предоставляет защиту в бою и имеет {0} хитов.",
						bt.HP);

				if (bt.Resource != null)
					s += String.Format(" Объект добавляет в ресурсы региона {0}.", bt.Resource.ToString(lng));

				string materials = bt.Materials.ToString(lng);
				s += String.Format(" Для завершения требуется: " + 
					materials);
				if (bt.OptionalMaterials.Count > 0)
					s += " Дополнительно можно установить: " + bt.OptionalMaterials.ToString(lng);

				if (bt.NoBuild)
					s += String.Format(" Новый объект этого типа построить невозможно.");
			}

			Write(s);
			Write("");
		}

		private static string AttitudeListString(Faction f, Attitude a) 
		{
			string s = "";
			foreach (int num in f.Attitudes.Keys) 
			{
				Faction target = Faction.Get(num);
				if (((Attitude)f.Attitudes[num]) != a || target == null)
					continue;
				if (s != "")
					s += ", ";
				s = s + target.ToString(lng);
			}
			if (s == "")
				s = "none";
			return s + ".";
		}

		public static string RationString(int amount, Lang lng) 
		{
			if (lng == Lang.Ru) 
			{
				if (amount % 10 == 1)
					return amount.ToString() + " дневной рацион";
				else if (amount % 10 >= 5 || amount % 10 == 0 || (amount >= 10 && amount < 20))
					return amount.ToString() + " дневных рационов";
				else
					return amount.ToString() + " дневных рациона";
			}
			else 
			{
				if (amount == 1)
					return amount.ToString() + " day ration";
				else
					return amount.ToString() + " day rations";
			}
		}

	}
}