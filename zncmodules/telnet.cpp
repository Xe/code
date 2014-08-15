/*
 * Copyright (C) 2004-2013  See the AUTHORS file for details.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation.
 */

#include <znc/Modules.h>
#include <znc/User.h>
#include <znc/IRCNetwork.h>

using std::map;

class CTelnetSock : public CSocket {
	CString m_sName;
public:
	CTelnetSock(CModule* pMod, const CString& sName, const CString& sHost, unsigned short uPort, bool bSSL) : CSocket(pMod) {
		m_sName = sName;
		Connect(sHost, uPort, bSSL);
	}

	virtual void ReadLine(const CString& sLine);
	virtual void Connected();
	virtual void Disconnected();
	virtual void Timeout();
	virtual void SockError(int iErrno, const CString& sDescription);
	virtual ~CTelnetSock();
};

class CTelnetModule : public CModule {
	map<CString, CTelnetSock*> m_mSockets;
	CBuffer m_Buffer;

	void Connect(const CString& sLine) {
		CString sHost = sLine.Token(1);
		CString sPort = sLine.Token(2);
		CString sName = sLine.Token(3);
		bool bSSL = sPort.TrimPrefix("+");
		unsigned short uPort = sPort.ToUInt();

		if (sName.empty()) {
			// generate name
			// if an ipv6 address is given, remove all :, because user won't be able to /msg nick containing :
			sName = "tel" + sHost.Replace_n(":", "") + sPort;
			if (m_mSockets.count(sName)) {
				unsigned int i = 0;
				while (m_mSockets.count(sName + CString(i))) ++i;
				sName += CString(i);
			}
		} else if (m_mSockets.count(sName)) {
			PutModule("Connection with such name already exists.");
			return;
		}

		m_mSockets[sName] = new CTelnetSock(this, sName, sHost, uPort, bSSL);

		PutModule("Opening new connection [" + sName + "] to [" + sHost + "]:" + CString(uPort) + (bSSL ? " using SSL" : ""));
	}

	void List(const CString&) {
		CTable Table;
		Table.AddColumn("Name");
		Table.AddColumn("Host");
		Table.AddColumn("Port");
		Table.AddColumn("SSL");
		for (map<CString, CTelnetSock*>::iterator i = m_mSockets.begin(); i != m_mSockets.end(); ++i) {
			Table.AddRow();
			Table.SetCell("Name", i->first);
			Table.SetCell("Host", i->second->GetHostName());
			Table.SetCell("Port", CString(i->second->GetRemotePort()));
			if (i->second->GetSSL()) {
				Table.SetCell("SSL", "Yes");
			}
		}
		PutModule(Table);
	}

	void Close(const CString& sLine) {
		CString sName = sLine.Token(1);
		if (!m_mSockets.count(sName)) {
			PutModule("Connection with such name does not exist.");
			return;
		}
		CTelnetSock* pSock = m_mSockets[sName];
		pSock->Close();
	}

	void Empty(const CString& sLine) {
		CString sName = sLine.Token(1);
		if (!m_mSockets.count(sName)) {
			PutModule("Connection with such name does not exist.");
			return;
		}
		CTelnetSock* pSock = m_mSockets[sName];
		pSock->Write("\r\n");
	}
public:
	MODCONSTRUCTOR(CTelnetModule) {
		m_Buffer.SetLineCount(500);
		AddCommand("Connect", static_cast<CModCommand::ModCmdFunc>(&CTelnetModule::Connect),
				"host [+]port [name]", "Open new connection. If name is missing, it will be generated");
		AddCommand("List", static_cast<CModCommand::ModCmdFunc>(&CTelnetModule::List),
				"", "List active connections");
		AddCommand("Close", static_cast<CModCommand::ModCmdFunc>(&CTelnetModule::Close),
				"name", "Close connection");
		AddCommand("Empty", static_cast<CModCommand::ModCmdFunc>(&CTelnetModule::Empty),
				"name", "Send empty line");
		AddHelpCommand();
	}

	virtual EModRet OnUserRaw(CString& sLine) {
		if (!sLine.Token(0).Equals("PRIVMSG")) {
			return CONTINUE;
		}

		CString sName = sLine.Token(1);
		if (!sName.TrimPrefix(GetUser()->GetStatusPrefix())) {
			return CONTINUE;
		}
		if (!m_mSockets.count(sName)) {
			return CONTINUE;
		}

		CString sMessage = sLine.Token(2, true);
		sMessage.TrimPrefix();

		CTelnetSock* pSock = m_mSockets[sName];
		pSock->Write(sMessage + "\r\n");
		return HALT;
	}

	void RemoveTelnet(const CString& sName) {
		PutModule("Removing telnet " + sName);
		m_mSockets.erase(sName);
	}

	void PutUserOrBuffer(const CString& sName, const CString& sHost, const CString& sLine) {
		CString sSource = GetUser()->GetStatusPrefix() + sName + "!telnet@" + sHost;
		if (GetNetwork()->IsUserAttached()) {
			GetNetwork()->PutUser(":" + sSource + " PRIVMSG " + m_pNetwork->GetCurNick() + " :" + sLine);
		} else {
			m_Buffer.AddLine(":" + _NAMEDFMT(sSource) + " PRIVMSG {target} :{text}", sLine);
		}
	}

	void PutUserOrBufferA(const CString& sName, const CString& sHost, const CString& sLine) {
		CString sSource = GetUser()->GetStatusPrefix() + sName + "!telnet@" + sHost;
		if (GetNetwork()->IsUserAttached()) {
			GetNetwork()->PutUser(":" + sSource + " PRIVMSG " + m_pNetwork->GetCurNick() + " :\001ACTION " + sLine + "\001");
		} else {
			m_Buffer.AddLine(":" + _NAMEDFMT(sSource) + " PRIVMSG {target} :\001ACTION {text}\001", sLine);
		}
	}

	virtual void OnClientLogin() {
		MCString msParams;
		msParams["target"] = GetNetwork()->GetCurNick();

		unsigned int uSize = m_Buffer.Size();
		for (unsigned int uIdx = 0; uIdx < uSize; uIdx++) {
			PutUser(m_Buffer.GetLine(uIdx, *GetClient(), msParams));
		}
		m_Buffer.Clear();
	}
};

void CTelnetSock::ReadLine(const CString& sLine) {
	((CTelnetModule*)GetModule())->PutUserOrBuffer(m_sName, GetHostName(), sLine.Trim_n());
}

void CTelnetSock::Connected() {
	SetTimeout(0);
	((CTelnetModule*)GetModule())->PutUserOrBufferA(m_sName, GetHostName(), "connected");
}

void CTelnetSock::Disconnected() {
	((CTelnetModule*)GetModule())->PutUserOrBufferA(m_sName, GetHostName(), "disconnected");
}

void CTelnetSock::Timeout() {
	((CTelnetModule*)GetModule())->PutUserOrBufferA(m_sName, GetHostName(), "timed out");
}

void CTelnetSock::SockError(int iErrno, const CString& sDescription) {
	((CTelnetModule*)GetModule())->PutUserOrBufferA(m_sName, GetHostName(), "socket error: (" + CString(iErrno) + ") " + sDescription);
}

CTelnetSock::~CTelnetSock() {
	((CTelnetModule*)GetModule())->RemoveTelnet(m_sName);
}

MODULEDEFS(CTelnetModule, "Open persistent telnet connection with a remote host")
