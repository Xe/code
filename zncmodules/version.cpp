/*
 * Fuck that Copyright shit. Here is your Code. Do what you want.
 * Made by Nero
 */

#include "User.h"
#include "Nick.h"

class CVersion : public CModule
{
public:
	MODCONSTRUCTOR(CVersion)
	{
	}

	virtual ~CVersion()
	{
	}

	virtual EModRet OnCTCPReply(CNick& Nick, CString& sMessage) {
		if (sMessage.Equals("VERSION", false, 7)) {
			SetNV(Nick.GetNick().AsLower(),sMessage);
			CString sNicks = "";
			for (MCString::iterator it = BeginNV(); it != EndNV(); ++it) {
				if ((it->second).AsLower() == sMessage.AsLower() && it->first != Nick.GetNick().AsLower()) 
				sNicks = sNicks + it->first + " ";
			}
			if (sNicks.size()>2) {
				PutModNotice("VERSION: " + sNicks);
			}
		}
		return CONTINUE;
	}

	virtual EModRet OnStatusCommand(CString& sCommand) {
		CString sCmdName = sCommand.Token(0).AsLower();
		if (sCmdName == "rdata") { // If other Modueles are requesting a VERSION string
			CString sName = sCommand.Token(1).AsLower();
			sCommand = GetNV(sName);
			return HALT;
		}
		return CONTINUE;
	}


	virtual void OnModCommand(const CString& sCommand) {
		CString sCmdName = sCommand.Token(0).AsLower();
		CString sArg = sCommand.Token(1,true).AsLower();
		if (sCmdName == "del") {
			DelNV(sCommand.Token(1).AsLower());
			PutModule("Deleted " + sCommand.Token(1));
		} else if (sCmdName == "get") {
			PutModule(GetNV(sArg));
			PutModule("--- End of List");
		} else if (sCmdName == "clear") {
			int i = 0;
			for (MCString::iterator it = BeginNV(); it != EndNV(); ++it) {
				DelNV(it->first);
				i++;
			}
			PutModule(CString(i) + " entries deleted");
		} else if (sCmdName == "list") {
			for (MCString::iterator it = BeginNV(); it != EndNV(); ++it) {
				if ((it->second).AsLower().find(sArg) != CString::npos || sArg.empty())
				PutModule("\002" + it->first + "\002: " + it->second);
			}
			PutModule("--- End of List");
		} else {
			PutModule("Commands: del <name>, get <name>, list, clear");
		}
	}
};

MODULEDEFS(CVersion, "Stores VERSION-Replies")
