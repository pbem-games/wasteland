using System;
using System.Collections;

namespace Wasteland
{
	public interface ILocalized 
	{
		string ToString(Lang lng);
	}

	public class Event : ILocalized
	{
		private static Hashtable _messages = null;
		private Person _person = null;
		private string _order = "";
		private Event.Code _code;
		private object[] _params;

		public Event(Event.Code code) 
		{
			_code = code;
			_params = new object[0];
		}

		public Event(Person person, string order, Event.Code code, object[] parameters) 
		{
			_person = person;
			_order = order;
			_code = code;
			_params = parameters;
		}

		public Person Person 
		{
			get { return _person; }
		}

		public string ToString(Lang lng) 
		{
			return ToString(lng, true);
		}

		public string ToString(Lang lng, bool show_person) 
		{
			string res = "";
			
			if (show_person && _person != null)
				res += _person.ToString(lng) + ": ";

			if (_order != "")
				res += _order + ": ";

			string[] ps = new string[_params.Length];
      for (int i = 0; i < _params.Length; i++)
				if (_params[i].GetType().GetInterface("ILocalized") != null)
					ps[i] = ((ILocalized)_params[i]).ToString(lng);
				else
					ps[i] = _params[i].ToString();
			
			res += String.Format(((EventMessage)Messages[_code]).ToString(lng), ps);

			return res;
		}

		private static Hashtable Messages
		{
			// I decided to hard-code this to avoid incoherency between data.xml
			// and different code versions. If there will be attempts to add more 
			// languages, probably data file should be considered (with a thought
			// about execution speed, because this will be called very rapidly).
			get 
			{
				if (_messages == null) 
				{
					_messages = new Hashtable();

					_messages.Add(Code.OrderForLeader, new EventMessage(
						"This order should be given to leader.",
						"Этот приказ предназначен для командира бригады."
						));
					_messages.Add(Code.NoSuchItem, new EventMessage(
						"No such item.",
						"Нет такой вещи."
						));
					_messages.Add(Code.NoSkill, new EventMessage(
						"No skill to do that.",
						"Нет необходимого навыка."
						));
					_messages.Add(Code.NotSkilledEnough, new EventMessage(
						"Not skilled enough.",
						"Недостаточный уровень навыка."
						));
					_messages.Add(Code.NotInside, new EventMessage(
						"Should be inside object.",
						"Нужно находиться внутри объекта."
						));
					_messages.Add(Code.NotInRegion, new EventMessage(
						"{0} not in region.",
						"{0} не в регионе."
						));
					_messages.Add(Code.CantAttackSameFaction, new EventMessage(
						"Can't attack a person from same faction.",
						"Нельзя атаковать персонажа из своей фракции."
						));
					_messages.Add(Code.CantAttackSameTeam, new EventMessage(
						"Can't attack a person from same team.",
						"Нельзя атаковать персонажа из своей бригады."
						));
					_messages.Add(Code.CantBuildObject, new EventMessage(
						"Can't build this object.",
						"Этот объект нельзя построить."
						));
					_messages.Add(Code.TargetNotFriendly, new EventMessage(
						"{0} is not Friendly.",
						"{0} не относится к вам Дружелюбно."
						));
					_messages.Add(Code.OutOfDrugs, new EventMessage(
						"Out of drugs.",
						"Кончились таблетки."
						));
					_messages.Add(Code.NoSuchFaction, new EventMessage(
						"No such faction.",
						"Нет такой фракции."
						));
					_messages.Add(Code.MustOwnObject, new EventMessage(
						"Must own object to do that.",
						"Нужно быть владельцем объекта."
						));
					_messages.Add(Code.ObjectCantMove, new EventMessage(
						"Object can't move.",
						"Объект не может двигаться."
						));
					_messages.Add(Code.ObjectIncomplete, new EventMessage(
						"Object is not completed.",
						"Объект не достроен."
						));
					_messages.Add(Code.VehicleOverloaded, new EventMessage(
						"Vehicle is overloaded.",
						"Транспорт перегружен."
						));
					_messages.Add(Code.CantDriveThere, new EventMessage(
						"Can't drive there.",
						"Туда машина не проедет."
						));
					_messages.Add(Code.NotEnoughMP, new EventMessage(
						"Insufficient movement points.",
						"Недостаточно очков передвижения."
						));
					_messages.Add(Code.RadiationTooHighToEnter, new EventMessage(
						"Tried to enter {0}, but feeled woozy and turned back.",
						"Пытается войти в {0}, но чувствует слабость и тошноту и возвращается."
						));
					_messages.Add(Code.NoFuel, new EventMessage(
						"No fuel.",
						"Нет горючего."
						));
					_messages.Add(Code.ObjectOwnerNotFriendly, new EventMessage(
						"Object owner is not Friendly.",
						"Владелец объекта не относится к вам дружественно."
						));
					_messages.Add(Code.NotInObject, new EventMessage(
						"{0} not in same object.",
						"{0} не в этом объекте."
						));
					_messages.Add(Code.EvictLeader, new EventMessage(
						"Must EVICT a team leader.",
						"Надо выселять командира бригады."
						));
					_messages.Add(Code.Evicted, new EventMessage(
						"Evicted {0}.",
						"Выселяет персонажа {0}."
						));
					_messages.Add(Code.EvictedBy, new EventMessage(
						"Evicted by owner.",
						"Владелец выселил из объекта."
						));
					_messages.Add(Code.NoParameters, new EventMessage(
						"No parameters given.",
						"Не задано параметров приказа."
						));
					_messages.Add(Code.CuredBy, new EventMessage(
						"Cured by {0}.",
						"Принимает лечение от персонажа {0}."
						));
					_messages.Add(Code.Cures, new EventMessage(
						"Cures {0}.",
						"Лечит персонажа {0}."
						));
					_messages.Add(Code.CuresUsing, new EventMessage(
						"Cures {0} using {1}.",
						"Лечит персонажа {0}; использовано: {1}."
						));
					_messages.Add(Code.Drives, new EventMessage(
						"Drives from {0} to {1}.",
						"Покидает {0}, въезжает в {1}."
						));
					_messages.Add(Code.DrivesSpending, new EventMessage(
						"Drives from {0} to {1} spending 1 {2}.",
						"Покидает {0}, въезжает в {1}. Используется 1 {2}."
						));
					_messages.Add(Code.PersonDrives, new EventMessage(
						"{0} drives from {1} to {2}.",
						"Машина под управлением персонажа {0} покидает {1}, въезжает в {2}."
						));
					_messages.Add(Code.Enters, new EventMessage(
						"Enters {0}.",
						"Входит в {0}."
						));
					_messages.Add(Code.CantHideInTeam, new EventMessage(
						"Can't hide in team.",
						"Нельзя прятаться в бригаде."
						));
					_messages.Add(Code.CantHideInObject, new EventMessage(
						"Can't hide inside object.",
						"Нельзя прятаться в объекте."
						));
					_messages.Add(Code.CantInstall, new EventMessage(
						"Can't install that.",
						"Этот предмет нельзя монтировать."
						));
					_messages.Add(Code.Leaves, new EventMessage(
						"Leaves {0}.",
						"Покидает {0}."
						));
					_messages.Add(Code.Maintains, new EventMessage(
						"Maintains {0}.",
						"Обслуживает {0}."
						));
					_messages.Add(Code.Patrols, new EventMessage(
						"Patrols {0}.",
						"Патрулирует {0}."
						));
					_messages.Add(Code.Produces, new EventMessage(
						"Produces {0}.",
						"Произведено: {0}."
						));
					_messages.Add(Code.Quitted, new EventMessage(
						"{0} committed suicide. Bones was lost in desert, but great deeds will stay in the hearts of survived.",
						"{0} совершает самоубийство. Кости потеряны в песках, но великие дела навсегда останутся в памяти выживших."
						));
					_messages.Add(Code.NoItemInJunk, new EventMessage(
						"No such item in junk.",
						"В мусоре этого нет."
						));
					_messages.Add(Code.NotInTeam, new EventMessage(
						"{0} not in team.",
						"{0} не в бригаде."
						));
					_messages.Add(Code.KickedTeam, new EventMessage(
						"Kicked from team.",
						"Командир выгнал персонажа из бригады."
						));
					_messages.Add(Code.FactionDismissed, new EventMessage(
						"Faction lost Chosen One and was dismissed.",
						"Фракция потеряла Избранного и распалась."
						));
					_messages.Add(Code.GoesInsane, new EventMessage(
						"Goes insane and leaves faction.",
						"Сходит с ума и убегает из фракции."
						));
					_messages.Add(Code.Discards, new EventMessage(
						"Discards {0}.",
						"Выброшено: {0}."
						));
					_messages.Add(Code.Receives, new EventMessage(
						"Receives {0} from {1}.|",
						"Получено от персонажа {1}: {0}."
						));
					_messages.Add(Code.JoinedFaction, new EventMessage(
						"{0} joined your faction.",
						"{0} присоединяется к фракции."
						));
					_messages.Add(Code.CantInstallInObject, new EventMessage(
						"Item can't be installed in this object.",
						"Предмет нельзя установить в этот объект."
						));
					_messages.Add(Code.AnotherOptionInstalled, new EventMessage(
						"Another optional component installed.",
						"Уже установлен другой опциональный компонент."
						));
					_messages.Add(Code.MaxAmountInstalled, new EventMessage(
						"Max amount of this item already installed.",
						"Уже установлено максимальное количество."
						));
					_messages.Add(Code.DifferentInstallSkill, new EventMessage(
						"All materials should require same install skill.",
						"Все компоненты списка должны требовать одинакового навыка для установки."
						));
					_messages.Add(Code.NoItemToInstall, new EventMessage(
						"No {0} to install.",
						"Нет в наличии: {0}."
						));
					_messages.Add(Code.NoSkillToInstall, new EventMessage(
						"Not enough skill to install {0}.",
						"Недостаточно навыка для установки: {0}."
						));
					_messages.Add(Code.Installs, new EventMessage(
						"Performs work on {0}, installs {1}.",
						"В объект {0} установлено: {1}."
						));
					_messages.Add(Code.CantKickChosen, new EventMessage(
						"Can't kick Chosen One.",
						"Нельзя выгнать Избранного."
						));
					_messages.Add(Code.Kicked, new EventMessage(
						"{0} kicked from faction.",
						"{0} исключается из фракции."
						));
					_messages.Add(Code.CantMoveThere, new EventMessage(
						"Can't move there.",
						"Туда не пройти."
						));
					_messages.Add(Code.PersonOverloaded, new EventMessage(
						"Person is overloaded.",
						"Персонаж перегружен."
						));
					_messages.Add(Code.Moves, new EventMessage(
						"Moves from {0} to {1}.",
						"Покидает {0}, входит в {1}."
						));
					_messages.Add(Code.EntryForbidden, new EventMessage(
						"Entry to {0} forbidden by patrol: {1}.",
						"Вход в {0} запрещён патрульным: {1}."
						));
					_messages.Add(Code.EntryForbiddenF, new EventMessage(
						"Entry to {0} forbidden by patrol: {1}, {2}.",
						"Вход в {0} запрещён патрульным: {1}, {2}."
						));
					_messages.Add(Code.PasswordChanged, new EventMessage(
						"Password was changed.",
						"Пароль изменён."
						));
					_messages.Add(Code.CantProduce, new EventMessage(
						"Can't produce that.",
						"Предмет нельзя произвести."
						));
					_messages.Add(Code.MustBeInside, new EventMessage(
						"Must be inside {0}.",
						"Нужно быть внутри объекта: {0}."
						));
					_messages.Add(Code.ObjectIncompleteNamed, new EventMessage(
						"Object {0} is incomplete.",
						"Объект {0} не достроен."
						));
					_messages.Add(Code.NoResource, new EventMessage(
						"No resource in region.",
						"Ресурс отсутствует в регионе."
						));
					_messages.Add(Code.NotEnoughItem, new EventMessage(
						"Not enough {0}.",
						"Требуется {0}."
						));
					_messages.Add(Code.Uninstalls, new EventMessage(
						"Uninstalls {0}.",
						"Демонтировано: {0}."
						));
					_messages.Add(Code.UninstallsCollapsed, new EventMessage(
						"Uninstalls {0}. Object collapsed.",
						"Демонтировано: {0}. Объект разрушен."
						));
					_messages.Add(Code.NoItemInstalled, new EventMessage(
						"No {0} installed.",
						"В объекте нет компонента: {0}."
						));
					_messages.Add(Code.NoSkillToUninstall, new EventMessage(
						"Not enough skill to uninstall {0}.",
						"Навыка недостаточно, чтобы демонтировать: {0}."
						));
					_messages.Add(Code.FactionMustOwnObject, new EventMessage(
						"A person of same faction must own object.",
						"Персонаж вашей фракции должен быть владельцем объекта."
						));
					_messages.Add(Code.IsInTeam, new EventMessage(
						"{0} is in team.",
						"{0} тоже находится в бригаде."
						));
					_messages.Add(Code.CantJoinSelf, new EventMessage(
						"Leader can't be same person.",
						"Не может стать лидером себя."
						));
					_messages.Add(Code.JoinedTeam, new EventMessage(
						"{0} joined team.",
						"{0} присоединяется к бригаде."
						));
					_messages.Add(Code.LeavesTeam, new EventMessage(
						"{0} leaved team.",
						"{0} покидает бригаду."
						));
					_messages.Add(Code.NotEnoughOffer, new EventMessage(
						"Not enough {0} to offer.",
						"Недостаточно предметов: {0}."
						));
					_messages.Add(Code.Trades, new EventMessage(
						"Trades {0} for {1} with {2}.",
						"Персонажу {2} отдано: {0} за: {1}."
						));
					_messages.Add(Code.BornBaby, new EventMessage(
						"Born the baby: {0}, {1}.",
						"Рожает ребёнка: {0}, {1}."
						));
					_messages.Add(Code.HasBabyFrom, new EventMessage(
						"Has baby from {0}.",
						"Заводит ребёнка от персонажа {0}."
						));
					_messages.Add(Code.HadSexWith, new EventMessage(
						"Had sex with {0}.",
						"Спит с персонажем {0}."
						));
					_messages.Add(Code.TeamQuarrel, new EventMessage(
						"Persons in team are quarrelling.",
						"Персонажи в бригаде ссорятся."
						));
					_messages.Add(Code.Scavenges, new EventMessage(
						"Scavenges {0}.",
						"Найдено в мусоре: {0}."
						));
					_messages.Add(Code.Starved, new EventMessage(
						"Starved to death.",
						"Умирает от истощения."
						));
					_messages.Add(Code.Hungry, new EventMessage(
						"Hungry, needs {0} more rations.",
						"Голодает, нужно ещё {0} рационов."
						));
					_messages.Add(Code.Mutated, new EventMessage(
						"Mutated to {0}!",
						"Мутирует - теперь это {0}!"
						));
					_messages.Add(Code.RadiationOverdose, new EventMessage(
						"Died from lethal dose of radiation.",
						"Умирает от летальной дозы радиации."
						));
					_messages.Add(Code.RadiationUnderdose, new EventMessage(
						"Died from lack of radiation.",
						"Умирает от недостатка радиации."
						));
					_messages.Add(Code.RadiationReceived, new EventMessage(
						"Receiving a dose of radiation.",
						"Получает дозу радиации."
						));
					_messages.Add(Code.RadiationNotReceived, new EventMessage(
						"Not receiving a vital dose of radiation.",
						"Не получает необходимой дозы радиации."
						));
					_messages.Add(Code.Frozen, new EventMessage(
						"Frozen to death.",
						"Умирает от переохлаждения."
						));
					_messages.Add(Code.Cold, new EventMessage(
						"Suffers from cold, {0}° below acceptable.",
						"Страдает от холода, {0}° ниже нормы."
						));
					_messages.Add(Code.Burned, new EventMessage(
						"Burned {0}",
						"Сжигает: {0}"
						));
					_messages.Add(Code.Borrows, new EventMessage(
						"Borrows from {0} {1}.",
						"{0} одалживает: {1}."
						));
					_messages.Add(Code.Address, new EventMessage(
						"Address is now {0}.",
						"Адрес изменён на {0}."
						));
					_messages.Add(Code.Rehired, new EventMessage(
						"{0} leaves faction and joins {1}.",
						"{0} покидает фракцию, персонажа перекупает {1}."
						));
					_messages.Add(Code.NotFriendlyHireFail, new EventMessage(
						"{0} is not friendly and does not want it. It was not enough to hire this person.",
						"{0} не относится к вам дружественно и ничего не хочет брать. Этого недостаточно, чтобы перекупить данного персонажа."
						));
					_messages.Add(Code.RefusesReceive, new EventMessage(
						"Refused to receive {0} from {1}.",
						"Отказывается от предмета {0}, предложенного персонажем {1}."
						));
					_messages.Add(Code.GivesNoHire, new EventMessage(
						"Gives {0} to {1}. It was not enough to hire this person.",
						"Отдано персонажу {1}: {0}. Этого недостаточно, чтобы перекупить данного персонажа."
						));
					_messages.Add(Code.Gives, new EventMessage(
						"Gives {0} to {1}.",
						"Отдано персонажу {1}: {0}."
						));
					_messages.Add(Code.Consumed, new EventMessage(
						"Consumes {0}",
						"Съедено: {0}"
						));
				}
				return _messages;
			}
		}

		public enum Code 
		{
			Address,
			AnotherOptionInstalled,
			BornBaby,
			Burned,
			Borrows,
			CantAttackSameFaction,
			CantAttackSameTeam,
			CantBuildObject,
			CantDriveThere,
			CantHideInTeam,
			CantHideInObject,
			CantInstall,
			CantInstallInObject,
			CantJoinSelf,
			CantKickChosen,
			CantMoveThere,
			CantProduce,
			Cold,
			Consumed,
			Cures,
			CuresUsing,
			CuredBy,
			DifferentInstallSkill,
			Discards,
			Drives,
			DrivesSpending,
			Enters,
			EntryForbidden,
			EntryForbiddenF,
			EvictLeader,
			Evicted,
			EvictedBy,
			FactionDismissed,
			FactionMustOwnObject,
			Frozen,
			GoesInsane,
			Gives,
			GivesNoHire,
			HasBabyFrom,
			HadSexWith,
			Hungry,
			Installs,
			IsInTeam,
			JoinedFaction,
			JoinedTeam,
			Kicked,
			KickedTeam,
			Leaves,
			LeavesTeam,
			Maintains,
			MaxAmountInstalled,
			Moves,
			MustBeInside,
			MustOwnObject,
			Mutated,
			NoFuel,
			NoItemToInstall,
			NoResource,
			NoSuchItem,
			NoSkill,
			NoSkillToInstall,
			NoItemInJunk,
			NoItemInstalled,
			NotSkilledEnough,
			NotInside,
			NotInRegion,
			NotInObject,
			NotInTeam,
			NotEnoughMP,
			NotEnoughItem,
			NotEnoughOffer,
			NotFriendlyHireFail,
			NoSkillToUninstall,
			NoSuchFaction,
			NoParameters,
			ObjectCantMove,
			ObjectIncomplete,
			ObjectIncompleteNamed,
			ObjectOwnerNotFriendly,
			OrderForLeader,
			OutOfDrugs,
			PasswordChanged,
			Patrols,
			PersonDrives,
			PersonOverloaded,
			Produces,
			Quitted,
			RadiationTooHighToEnter,
			RadiationOverdose,
			RadiationUnderdose,
			RadiationReceived,
			RadiationNotReceived,
			Receives,
			RefusesReceive,
			Rehired,
			Scavenges,
			Starved,
			TargetNotFriendly,
			TeamQuarrel,
			Trades,
			Uninstalls,
			UninstallsCollapsed,
			VehicleOverloaded
		}
	}

	public class HitEvent : ILocalized 
	{
		private static Hashtable _messages = null;
		private ILocalized _attacker;
		private ILocalized _defender;
		public ArrayList Results = new ArrayList();
		public int Hits = 0;

		public HitEvent(ILocalized attacker, ILocalized defender) 
		{
			_attacker = attacker;
			_defender = defender;
		}

		public string ToString(Lang lng) 
		{
			string s = _attacker.ToString(lng) + " -> " + _defender.ToString(lng) + ": ";
			for (int i = 0; i < Results.Count; i++)
			{
				if (i > 0)
					s += ", ";
				s += ((EventMessage)Messages[(Code)Results[i]]).ToString(lng);
			}

			if (Hits > 0) {
				if (Results.Count > 0)
					s += ", ";
				s += (lng == Lang.En) ? (Hits.ToString() + " hp inflicted") :
					("выбито " + Hits.ToString() + " хитов"); 
			}

			return s;
		}

		private static Hashtable Messages
		{
			get 
			{
				if (_messages == null) 
				{
					_messages = new Hashtable();
					_messages.Add(Code.NoHit, new EventMessage(
						"no hit", "промах"));
					_messages.Add(Code.Block, new EventMessage(
						"block", "удар блокирован"));
					_messages.Add(Code.ArmorSave, new EventMessage(
						"armor save", "защита бронёй"));
					_messages.Add(Code.NoWound, new EventMessage(
						"no wound", "нет ранения"));
					_messages.Add(Code.Killed, new EventMessage(
						"killed", "персонаж убит"));
					_messages.Add(Code.Exploded, new EventMessage(
						"object exploded", "объект взорван"));
					_messages.Add(Code.Damaged, new EventMessage(
						"object damaged", "объект повреждён"));
					_messages.Add(Code.Incapacitated, new EventMessage(
						"incapacitated", "персонаж выведен из строя"));
					_messages.Add(Code.CriticalHit, new EventMessage(
						"critical hit", "крит. попадание"));
					_messages.Add(Code.CriticalBlock, new EventMessage(
						"critical block", "крит. блок"));
					_messages.Add(Code.CriticalSave, new EventMessage(
						"critical save", "крит. защита бронёй"));
					_messages.Add(Code.CriticalWound, new EventMessage(
						"critical wound", "крит. ранение"));
					_messages.Add(Code.CriticalHitFail, new EventMessage(
						"critical hit fail", "крит. промах"));
					_messages.Add(Code.CriticalBlockFail, new EventMessage(
						"critical block fail", "крит. провал блока"));
					_messages.Add(Code.CriticalSaveFail, new EventMessage(
						"critical save fail", "крит. провал брони"));
					_messages.Add(Code.CriticalWoundFail, new EventMessage(
						"critical wound fail", "крит. провал ранения"));
				}
				return _messages;
			}
		}

		public enum Code
		{
			NoHit,
			Block,
			ArmorSave,
			NoWound,
			Killed,
			Exploded,
			Damaged,
			Incapacitated,
			CriticalHit,
			CriticalBlockFail,
			CriticalSaveFail,
			CriticalWound,
			CriticalHitFail,
			CriticalBlock,
			CriticalSave,
			CriticalWoundFail
		}
	}

	public class EventMessage : ILocalized 
	{
		private string _en;
		private string _ru;

		public EventMessage(string en, string ru) 
		{
			_en = en;
			_ru = ru;
		}

		public string ToString(Lang lng) 
		{
			if (lng == Lang.En)
				return _en;
			else
				return _ru;
		}
	}


	#region Lists

	public class EventList : ArrayList
	{
		public new Event this[int index] 
		{
			get { return (Event)base[index]; }
			set { base[index] = value; }
		}

		public override int Add(object value) 
		{
			throw new NotSupportedException();
		}

		public int Add(Event item) 
		{
			return base.Add(item);
		}

	}

	#endregion

}