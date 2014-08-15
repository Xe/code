/*
 * Copyright (C) 2004-2009  See the AUTHORS file for details.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation.
 *
 * Made by Mini
 */

#include "Modules.h"
#include "Chan.h"
#include "User.h"
#include <list>
 
class CIgnoreEntry {
public:

	CIgnoreEntry(const CString& sHostMask, CString& sType, CString& sOption) {
		CNick Nick;
		Nick.Parse(sHostMask);

		m_sHostMask = (Nick.GetNick().size()) ? Nick.GetNick() : "*";
		m_sHostMask += "!";
		m_sHostMask += (Nick.GetIdent().size()) ? Nick.GetIdent() : "*";
		m_sHostMask += "@";
		m_sHostMask += (Nick.GetHost().size()) ? Nick.GetHost() : "*";
		
		m_sType = sType;
		m_sOption = m_sOption;
	}
	
	virtual ~CIgnoreEntry() {}

	bool IsMatch(const CNick& Nick, const CString& sText, const CString& sSource, const CUser* pUser) {
	
		if (!Nick.GetHostMask().AsLower().WildCmp(m_sHostMask.AsLower()))
			return false;
	
		return true;
	}
	
	bool operator ==(const CIgnoreEntry& IgnoreEntry) {
		return (
				GetHostMask().Equals(IgnoreEntry.GetHostMask())
				);
	}

	// Getters
	
	const CString& GetHostMask() const { return m_sHostMask; }
	const CString& GetType() const { return m_sType; }
	const CString& GetOption() const { return m_sOption; }
	
	// Setters
	
	void SetHostMask(const CString& s) { m_sHostMask = s; }
	void SetType(const CString& s) { m_sType = s; }
	void SetOption(const CString& s) { m_sOption = s; }
	
	
private:
protected:
	CString		m_sHostMask;
	CString		m_sType;
	CString		m_sOption;
};
	
class CIgnoreMod : public CModule {
public:
	
	MODCONSTRUCTOR(CIgnoreMod) {
		Load();
	}
	
	virtual ~CIgnoreMod() {}

	virtual bool OnLoad(const CString& sArgs, CString& sMessage) {
	
		Load();
	
		return true;
	
	}
	
	virtual void OnModCommand(const CString& sCommand) {

		CString sCmdName = sCommand.Token(0);
	
		if (sCmdName.Equals("ignore")) {
			
			CString sType = "all";
			CString sOption = "perm";
			
			if (sCommand.Token(2).size()) { 
				
				sType = sCommand.Token(2);
				
				if ((!sType.Equals("private")) && (!sType.Equals("channel")) && (!sType.Equals("notice")) && (!sType.Equals("CTCP")) && (!sType.Equals("invite")) && (!sType.Equals("all"))) { 
				
					PutModule("Invalid ignore type given. (" + sCommand.Token(2) + ")");
					return;

				}
				
			}
			
			if (sCommand.Token(3).size()) { 
				
				sOption = sCommand.Token(3);
				
				if ((!sOption.Equals("nosave")) && (!sOption.Equals("perm"))) {
				
					PutModule("Invalid ignore option given. (" + sCommand.Token(3) + ")");
					return;
					
				}
			
			}
			
			Ignore(sCommand.Token(1), sType, sOption);
		
		} else if (sCmdName.Equals("unignore")) { 
		
			Unignore(sCommand.Token(1));

		} else if (sCmdName.Equals("list")) { 

			List();

		} else if (sCmdName.Equals("help")) { 

			Help();

		}

	}
	
	virtual EModRet OnPrivMsg(CNick& Nick, CString& sMessage) {
		for (list<CIgnoreEntry>::iterator it = m_lsIgnores.begin(); it != m_lsIgnores.end(); it++) {
			CIgnoreEntry& IgnoreEntry = *it;
		
			if (IgnoreEntry.IsMatch(Nick, sMessage, "priv", m_pUser))
				return HALT;
	
		}
		
		return CONTINUE;
	}
	
	virtual EModRet OnChanMsg(CNick& Nick, CChan& Channel, CString& sMessage) {
		for (list<CIgnoreEntry>::iterator it = m_lsIgnores.begin(); it != m_lsIgnores.end(); it++) {
			CIgnoreEntry& IgnoreEntry = *it;
		
			if (IgnoreEntry.IsMatch(Nick, sMessage, Channel.GetName(), m_pUser))
				return HALT;
	
		}
	
		return CONTINUE;
	}
	
	virtual EModRet OnChanNotice(CNick& Nick, CChan& Channel, CString& sMessage) {
		for (list<CIgnoreEntry>::iterator it = m_lsIgnores.begin(); it != m_lsIgnores.end(); it++) {
			CIgnoreEntry& IgnoreEntry = *it;
		
			if (IgnoreEntry.IsMatch(Nick, sMessage, Channel.GetName(), m_pUser))
				return HALT;
	
		}
	
		return CONTINUE;
	}
	
	virtual EModRet OnPrivNotice(CNick& Nick, CString& sMessage) {
		for (list<CIgnoreEntry>::iterator it = m_lsIgnores.begin(); it != m_lsIgnores.end(); it++) {
			CIgnoreEntry& IgnoreEntry = *it;
		
			if (IgnoreEntry.IsMatch(Nick, sMessage, "priv", m_pUser))
				return HALT;
	
		}
	
		return CONTINUE;
	}
	
	virtual EModRet OnPrivCTCP(CNick& Nick, CString& sMessage) {
		for (list<CIgnoreEntry>::iterator it = m_lsIgnores.begin(); it != m_lsIgnores.end(); it++) {
			CIgnoreEntry& IgnoreEntry = *it;
		
			if (IgnoreEntry.IsMatch(Nick, sMessage, "priv", m_pUser))
				return HALT;
	
		}
	
		return CONTINUE;
	}
	
	virtual EModRet OnChanCTCP(CNick& Nick, CChan& Channel, CString& sMessage) {
		for (list<CIgnoreEntry>::iterator it = m_lsIgnores.begin(); it != m_lsIgnores.end(); it++) {
			CIgnoreEntry& IgnoreEntry = *it;
		
			if (IgnoreEntry.IsMatch(Nick, sMessage, Channel.GetName(), m_pUser))
				return HALT;
	
		}
	
		return CONTINUE;
	}
	
private:

	void Help() {
		CTable Table;

		Table.AddColumn("Command");
		Table.AddColumn("Description");

		Table.AddRow();
		Table.SetCell("Command", "Ignore <HostMask>");
		Table.SetCell("Description", "Add the specified hostmask to the ignore list.");

		Table.AddRow();
		Table.SetCell("Command", "Unignore <HostMask>");
		Table.SetCell("Description", "Remove the specified hostmask from the ignore list.");

		Table.AddRow();
		Table.SetCell("Command", "List");
		Table.SetCell("Description", "List all available entries.");
		
		PutModule(Table);
	}
	
	void Ignore (const CString& sHostMask, CString& sType, CString& sOption) { 
	
		CString sMessage;
		
		if (sHostMask.size()) { 
			CIgnoreEntry IgnoreEntry(sHostMask, sType, sOption);
		
	
			bool eExists = false;
			
			for (list<CIgnoreEntry>::iterator it = m_lsIgnores.begin(); it != m_lsIgnores.end(); it++) {
				if (*it == IgnoreEntry) {
					sMessage = "Entry for [" + IgnoreEntry.GetHostMask() + "] already exists.";
					eExists = true;
					break;
				}
			}
		
	
			if (!eExists) {
				sMessage = "Adding entry: [" + IgnoreEntry.GetHostMask() + "] will now be ignored.";
				m_lsIgnores.push_back(IgnoreEntry);
			}

	
			if (sMessage.empty()) { 
				sMessage = "Something went wrong while adding the ignore. Please try again.";
			}
		
		} else {
			sMessage = "Invalid parameters given. Try HELP";
		}
		
		PutModule(sMessage);
	
		Save();
	
	}
	
	void Unignore(const CString& sHostMask) {

		list<CIgnoreEntry>::iterator it;
		bool sFounded = false;
		for (it = m_lsIgnores.begin(); it != m_lsIgnores.end(); it++) {
			CIgnoreEntry& IgnoreEntry = *it;
		
			if (IgnoreEntry.GetHostMask().Equals(sHostMask))
				sFounded = true;
				break;
		
		}
	
		if (sFounded) {
			m_lsIgnores.erase(it);
			PutModule(sHostMask + " Removed.");
			Save();
		} else {
			PutModule(sHostMask + " Not found.");
		}
	}
	
	void List() {
		CTable Table;
		Table.AddColumn("HostMask");
		Table.AddColumn("Type");
		Table.AddColumn("Option");
		
		for (list<CIgnoreEntry>::iterator it = m_lsIgnores.begin(); it != m_lsIgnores.end(); it++) {
			CIgnoreEntry& IgnoreEntry = *it;

			Table.AddRow();
			Table.SetCell("HostMask", IgnoreEntry.GetHostMask());
			Table.SetCell("Type", IgnoreEntry.GetType());
			Table.SetCell("Option", IgnoreEntry.GetOption());
		}

		if (Table.size()) {
			PutModule(Table);
		} else {
			PutModule("You have no entries.");
		}
	}
	
	void Save() {
		ClearNV(false);
		for (list<CIgnoreEntry>::iterator it = m_lsIgnores.begin(); it != m_lsIgnores.end(); it++) {
			CIgnoreEntry& IgnoreEntry = *it;
			CString sSave;

			sSave  = IgnoreEntry.GetHostMask() + "\n";
			sSave += IgnoreEntry.GetType() + "\n";
			sSave += IgnoreEntry.GetOption() + "\n";
			sSave += " ";

			SetNV("Ignore", sSave, true);
		}

		SaveRegistry();
	}
	
	void Load() {
		m_lsIgnores.clear();

		bool bWarn = false;

		for (MCString::iterator it = BeginNV(); it != EndNV(); it++) {
			VCString vList;
			it->first.Split("\n", vList);

			if (vList.size() != 1) {
				bWarn = true;
				continue;
			}

			CIgnoreEntry IgnoreEntry(vList[0], vList[1], vList[2]);
			m_lsIgnores.push_back(IgnoreEntry);
		}

		if (bWarn)
			PutModule("WARNING: malformed entry found while loading");
	}
	
	list<CIgnoreEntry>	m_lsIgnores;
	VCString			m_sIgnores;

};

MODULEDEFS(CIgnoreMod, "Ignore module. Add/Remove hostmasks, to ignore incoming messages.")