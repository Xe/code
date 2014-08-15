/*
 * Copyright 2014 Sam Dodrill <shadowh511@gmail.com>
 *
 * You may use this module as long as you follow the terms of the WTFPL.
 */

/*

Configuration:

module {
	name = "hs_nethost"
	suffix = "yolo-swag.com"
}


 */

#include "module.h"

class HSNetHost : public Module
{
 public:
	HSNetHost(const Anope::string &modname, const Anope::string &creator):
		Module(modname, creator)
	{
	}

	void OnNickRegister(User *user, NickAlias *na) {
		const Anope::string &suffix = Config->GetModule(this)->Get<const Anope::string>("suffix");
		Anope::string vhost = user->nick + "." + suffix;
		Anope::string setter = "HostServ";
		Anope::string ident;

		if(!IRCD->IsHostValid(vhost))
			return;

		na->SetVhost(ident, vhost, setter);
		FOREACH_MOD(OnSetVhost, (na));
	}
};

MODULE_INIT(HSNetHost)
