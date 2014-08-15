/*
 * Copyright (C) 2004-2013  See the AUTHORS file for details.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation.
 */

#include <znc/IRCNetwork.h>
#include <znc/Modules.h>

class CPrivMsgPrefixMod : public CModule {
public:
	MODCONSTRUCTOR(CPrivMsgPrefixMod) {}

	virtual EModRet OnUserMsg(CString& sTarget, CString& sMessage) {
		if (m_pNetwork && m_pNetwork->GetIRCSock() && !m_pNetwork->IsChan(sTarget)) {
			m_pNetwork->PutUser(":" + sTarget + "!prefix@privmsg.znc.in PRIVMSG " + m_pNetwork->GetIRCNick().GetNick() + " :-> " + sMessage, NULL, m_pClient);
		}

		return CONTINUE;
	}

	virtual EModRet OnUserAction(CString& sTarget, CString& sMessage) {
		if (m_pNetwork && m_pNetwork->GetIRCSock() && !m_pNetwork->IsChan(sTarget)) {
			m_pNetwork->PutUser(":" + sTarget + "!prefix@privmsg.znc.in PRIVMSG " + m_pNetwork->GetIRCNick().GetNick() + " :\x01" + "ACTION -> " + sMessage + "\x01", NULL, m_pClient);
		}

		return CONTINUE;
	}

};

USERMODULEDEFS(CPrivMsgPrefixMod, "Send outgoing PRIVMSGs and CTCP ACTIONs to other clients using ugly prefixes")

