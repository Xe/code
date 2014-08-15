/*
 * Copyright (C) 2004-2012  See the AUTHORS file for details.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as published
 * by the Free Software Foundation.
 */

#include "User.h"
#include "Chan.h"
#include "znc.h"

// Forward Declaration
class CTranslateMod;

class CTranslateSock : public CSocket {
public:
	CTranslateSock(CTranslateMod* pMod, const CString& sText, const CString& sLangFrom, const CString& sLangTo) : CSocket((CModule*)(pMod)) {
		m_pParent = pMod;
		// Really this Escape_n converts from UTF-8, not from ASCII
		CString sRequest = "GET /ajax/services/language/translate?v=1.0&key=ABQIAAAAhwR5TtcQxY9fSuKy7yrBJhQ-sC4I4KvMQ8RG81t2M9sVc21w2xQUb9Dipx99m8XrHBsa3OctXe2rQw&langpair=" + sLangFrom + "%7C" + sLangTo + "&q=" + sText.Escape_n(CString::EASCII, CString::EURL) + " HTTP/1.0\r\n";
		sRequest += "Referer: http://znc.in/\r\n";
		sRequest += "Accept-Charset: UTF-8\r\n";
		sRequest += "Host: ajax.googleapis.com\r\n";
		sRequest += "Connection: Close\r\n";
		sRequest += "\r\n";
		EnableReadLine();
		state = 0;
		displayedError = false;
		DEBUG("TRANSLATE: Connecting to google");
		Connect("ajax.googleapis.com", 80, false);
		Write(sRequest);
	}
	virtual void onTranslated(const CString& sText)=0;
	virtual void onError(const CString& sText)=0;
	void ReadLine(const CString& sData){
		CString sLine = sData;
		sLine.TrimRight();
		DEBUG(sLine);
		switch (state) {
			case 0:
				state = 1;
				if (!sData.Token(1).Equals("200")) {
					if (!displayedError) {
						onError(sLine.Token(1,true));
						displayedError = true;
					}
				}
				break;
			case 1:
				if(sLine.empty()){
					state = 2;
				}
				break;
			case 2:
				// 'n' is begin of 'null'
				if(sLine.length()>17 && 'n'==sLine[17]){
					if(!displayedError){
						onError(sLine.Token(5, false, "\""));
						displayedError = true;
					}
				}else{
					onTranslated(sLine.Token(5, false, "\""));
				}
				state = 3;
		}
	}
	void Disconnected(){
		// If there is some incomplete line in the buffer, read it
		CString &sBuffer = GetInternalReadBuffer();
		if (!sBuffer.empty()) {
			ReadLine(sBuffer);
		}
		if (3!=state && !displayedError) {
			onError("error");
			displayedError = true;
		}
	}
	void Timeout(){
		if (!displayedError) {
			onError("Timeout");
			displayedError = true;
		}
	}
	void ConnectionRefused(){
		if (!displayedError) {
			onError("Connection refused");
			displayedError = true;
		}
	}
	void SockError(int iErrno){
		if (!displayedError) {
			onError(CString(iErrno));
			displayedError = true;
		}
	}

	CTranslateMod*	m_pParent;
	bool PutIRC(const CString& sText);
	bool PutModule(const CString& sText);
	bool PutUser(const CString& sText);
private:
	int state;//0 waiting for status, 1 waiting next header, 2 waiting body, 3 done
	bool displayedError;
};

class CTranslateFromUserSock : public CTranslateSock {
public:
	CTranslateFromUserSock(CTranslateMod* pMod, const CString& sPrefix, const CString& sSuffix,
			const CString& sText, const CString& sLangFrom, const CString& sLangTo)
			: CTranslateSock(pMod, sText, sLangFrom, sLangTo) {
		m_sPrefix = sPrefix;
		m_sSuffix = sSuffix;
		m_sOriginal = sText;
	}
	virtual void onTranslated(const CString& sText){
		PutIRC(m_sPrefix + sText + m_sSuffix);
	};
	virtual void onError(const CString& sText){
		PutIRC(m_sPrefix + m_sOriginal + m_sSuffix);
		PutModule("Error \"" + sText + "\" occured while outgoing translation. Sent non-translated text");
	};
private:
	CString  m_sPrefix;
	CString  m_sSuffix;
	CString  m_sOriginal;
};

class CTranslateToUserSock : public CTranslateSock {
public:
	CTranslateToUserSock(CTranslateMod* pMod, const CString& sPrefix, const CString& sSuffix,
			const CString& sText, const CString& sLangFrom, const CString& sLangTo)
			: CTranslateSock(pMod, sText, sLangFrom, sLangTo) {
		m_sPrefix = sPrefix;
		m_sSuffix = sSuffix;
		m_sOriginal = sText;
	}
	virtual void onTranslated(const CString& sText){
		PutUser(m_sPrefix + sText + m_sSuffix);
	};
	virtual void onError(const CString& sText){
		PutUser(m_sPrefix + m_sOriginal + m_sSuffix);
		PutModule("Error \"" + sText + "\" occured while incoming translation. Sent non-translated text");
	};
private:
	CString m_sPrefix;
	CString m_sSuffix;
	CString m_sOriginal;
};

class CTranslateMod : public CModule {
	struct TargetSettings {
		CString sLang;
		CString sMy;
	};
public:
	MODCONSTRUCTOR(CTranslateMod) {
		m_sMyLang = "en";
	}

	virtual ~CTranslateMod() {}

	virtual bool OnLoad(const CString& sArgs, CString& sMessage){
		m_sMyLang = GetNV("my");
		if(m_sMyLang.empty()){
			m_sMyLang = "en";
		}
		m_Langs.clear();
		// First, convert config from old version - add '/'
		bool bChanged;
		do {
			bChanged = false;
			for(MCString::iterator i = BeginNV(); EndNV() != i; ++i) {
				if (!i->first.empty() && 'x'==i->first[0] && CString::npos==i->first.find('/')) {
					CString sTarget = i->first.substr(1);
					if (!sTarget.empty() && '#'==sTarget[0]) {
						sTarget += '/';
					} else {
						sTarget = '/' + sTarget;
					}
					DelNV(i);
					SetNV("x" + sTarget, i->second);
					bChanged = true;
					break;
				}
			}
		} while (bChanged);
		for(MCString::iterator i = BeginNV(); EndNV() !=i ; ++i) {
			if (!i->first.empty() && 'x'==i->first[0]) {
				m_Langs[i->first.substr(1)].sLang = i->second;
			}
			if (!i->first.empty() && 'y'==i->first[0]) {
				m_Langs[i->first.substr(1)].sMy = i->second;
			}
		}
		return true;
	}

	virtual void OnModCommand(const CString& sLine) {
		CString sCommand = sLine.Token(0);
		sCommand.MakeLower();
		//here to config
		if (sCommand.Equals("my")) {
			m_sMyLang = sLine.Token(1);
			SetNV("my", m_sMyLang);
			PutModule("ok");
		} else if (sCommand.Equals("set")) {
			CString sTarget = sLine.Token(1);
			if (CString::npos==sTarget.find('/')) {
				if (!sTarget.empty() && '#'==sTarget[0]) {
					sTarget += "/";
				} else {
					sTarget = "/" + sTarget;
				}
			}
			m_Langs[sTarget].sLang = sLine.Token(2);
			m_Langs[sTarget].sMy = sLine.Token(3);
			SetNV("x" + sTarget, sLine.Token(2));
			SetNV("y" + sTarget, sLine.Token(3));
			PutModule("ok");
		} else if (sCommand.Equals("unset")) {
			CString sTarget = sLine.Token(1);
			if (CString::npos==sTarget.find('/')) {
				if (!sTarget.empty() && '#'==sTarget[0]) {
					sTarget += "/";
				} else {
					sTarget = "/" + sTarget;
				}
			}
			m_Langs.erase(sTarget);
			DelNV("x" + sTarget);
			DelNV("y" + sTarget);
			PutModule("ok");
		} else if (sCommand.Equals("clear")) {
			m_Langs.clear();
			ClearNV();
			SetNV("my",m_sMyLang);
			PutModule("ok");
		} else if (sCommand.Equals("list")) {
			PutModule("Your default language: " + m_sMyLang);
			CTable Table;
			Table.AddColumn("Channel");
			Table.AddColumn("Nick");
			Table.AddColumn("Their language");
			Table.AddColumn("Your language");
			for(map<CString,TargetSettings>::iterator i = m_Langs.begin(); m_Langs.end() != i ; ++i){
				Table.AddRow();
				CString sChannel = i->first.Token(0, false, "/", true);
				if (sChannel.empty()) {
					sChannel = "(PM)";
				}
				Table.SetCell("Channel", sChannel);
				Table.SetCell("Nick", i->first.Token(1, false, "/", true));
				Table.SetCell("Their language", i->second.sLang);
				Table.SetCell("Your language", i->second.sMy);
			}
			PutModule(Table);
//		} else if (sCommand.Equals("avail")) {
//			PutModule("not implemented");
		} else {
			PutModule("Commands:");
			CTable Table;
			Table.AddColumn("Command");
			Table.AddColumn("Arguments");
			Table.AddColumn("Description");
			
			Table.AddRow();
			Table.SetCell("Command", "my");
			Table.SetCell("Arguments", "<xy>");
			Table.SetCell("Description", "Set your default language");

			Table.AddRow();
			Table.SetCell("Command", "set");
			Table.SetCell("Arguments", "<channel> <xy> [<my>]");
			Table.SetCell("Description", "Set language of channel");

			Table.AddRow();
			Table.SetCell("Command", "set");
			Table.SetCell("Arguments", "<nick> <xy> [<my>]");
			Table.SetCell("Description", "Set language of private messages for nick");

			Table.AddRow();
			Table.SetCell("Command", "set");
			Table.SetCell("Arguments", "<channel>/<nick> <xy> [<my>]");
			Table.SetCell("Description", "Set language for nick on channel");

			Table.AddRow();
			Table.SetCell("Command", "unset");
			Table.SetCell("Arguments", "<channel or nick>");
			Table.SetCell("Description", "Don't translate target anymore");

			Table.AddRow();
			Table.SetCell("Command", "clear");
			Table.SetCell("Description", "Clear all targets");

			Table.AddRow();
			Table.SetCell("Command", "list");
			Table.SetCell("Description", "Show list of configured targets with their languages");

			Table.AddRow();
			Table.SetCell("Command", "help");
			Table.SetCell("Description", "Show this text");

			PutModule(Table);

//			PutModule("avail   - show list of available languages");
		}
	}
	
	EModRet HandleToUser(const CString& sChannel, const CString& sNick, const CString& sMessage, const CString& sPrefix, const CString& sSuffix){
		map<CString,TargetSettings>::const_iterator i = m_Langs.find(sChannel + "/" + sNick);
		if (m_Langs.end() == i) {
			i = m_Langs.find(sChannel + "/");
			if (m_Langs.end() == i) {
				return CONTINUE;
			}
		}
		new CTranslateToUserSock(this, sPrefix, sSuffix, sMessage, i->second.sLang, i->second.sMy.empty()?m_sMyLang:i->second.sMy);
		return HALT;
	}

	EModRet HandleFromUser(CString sTarget, CString sMessage, CString sPrefix, const CString& sSuffix){
		if (sTarget.empty()) {
			return CONTINUE;
		}
		map<CString,TargetSettings>::const_iterator i = m_Langs.end();
		if ('#' == sTarget[0]) {
			sTarget += "/";
			// Evil heuristics to determine, who is receiver of message
			CString sNick = sMessage.Token(0);
			if (sNick.empty()) {
				return CONTINUE;
			}
			sNick.Trim(",:");
			i = m_Langs.find(sTarget + sNick);
			if (m_Langs.end() != i) {
				sPrefix += sMessage.Token(0) + " ";
				sMessage = sMessage.Token(1, true);
			}
		} else {
			sTarget = "/" + sTarget;
		}
		if (m_Langs.end() == i) {
			i = m_Langs.find(sTarget);
			if (m_Langs.end() == i) {
				return CONTINUE;
			}
		}
		new CTranslateFromUserSock(this, sPrefix, sSuffix, sMessage, i->second.sMy.empty()?m_sMyLang:i->second.sMy, i->second.sLang);
		return HALT;
	}

	virtual EModRet OnUserMsg(CString &sTarget, CString &sMessage){
		return HandleFromUser(sTarget, sMessage, "PRIVMSG " + sTarget + " :", "");
	}

	virtual EModRet OnUserNotice(CString &sTarget, CString &sMessage){
		return HandleFromUser(sTarget, sMessage, "NOTICE " + sTarget + " :", "");
	}

	virtual EModRet OnPrivMsg(CNick &Nick, CString &sMessage){
		return HandleToUser("", Nick.GetNick(), sMessage, ":" + Nick.GetNickMask() + " PRIVMSG " + GetUser()->GetCurNick() + " :", "");
	}

	virtual EModRet OnPrivNotice(CNick &Nick, CString &sMessage){
		return HandleToUser("", Nick.GetNick(), sMessage, ":" + Nick.GetNickMask() + " NOTICE " + GetUser()->GetCurNick() + " :", "");
	}

	virtual EModRet OnChanMsg(CNick &Nick, CChan &Channel, CString &sMessage){
		return HandleToUser(Channel.GetName(), Nick.GetNick(), sMessage, ":" + Nick.GetNickMask() + " PRIVMSG " + Channel.GetName() + " :", "");
	}

	virtual EModRet OnChanNotice(CNick &Nick, CChan &Channel, CString &sMessage){
		return HandleToUser(Channel.GetName(), Nick.GetNick(), sMessage, ":" + Nick.GetNickMask() + " NOTICE " + Channel.GetName() + " :", "");
	}

	virtual EModRet OnPrivAction(CNick &Nick, CString &sMessage){
		return HandleToUser("", Nick.GetNick(), sMessage, ":" + Nick.GetNickMask() + " PRIVMSG " + GetUser()->GetCurNick() + " :\01ACTION ", "\01");
	}

	virtual EModRet OnChanAction(CNick &Nick, CChan &Channel, CString &sMessage){
		return HandleToUser(Channel.GetName(), Nick.GetNick(), sMessage, ":" + Nick.GetNickMask() + " PRIVMSG " + Channel.GetName() + " :\01ACTION ", "\01");
	}

	virtual EModRet OnUserAction(CString &sTarget, CString &sMessage){
		return HandleFromUser(sTarget, sMessage, "PRIVMSG " + sTarget + " :\01ACTION ", "\01");
	}

private:
	CString m_sMyLang;
	map<CString,TargetSettings> m_Langs;
};

bool CTranslateSock::PutIRC(const CString& sText) {
	return m_pParent->PutIRC(sText);
}

bool CTranslateSock::PutModule(const CString& sText) {
	return m_pParent->PutModule(sText);
}

bool CTranslateSock::PutUser(const CString& sText) {
	return m_pParent->PutUser(sText);
}

MODULEDEFS(CTranslateMod, "Translation of conversation using Google")

