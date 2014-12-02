/*
 * Copyright (c) 2014 Sam Dodrill <xena@yolo-swag.com>
 *
 * Code is WTFPL
 *
 * Set/unset channel mode +P on channel reg/dereg
 */

#include "atheme-compat.h"

DECLARE_MODULE_V1
(
	"contrib/cs_permreg", false, _modinit, _moddeinit,
	PACKAGE_STRING,
	"Sam Dodrill <xena@yolo-swag.com>"
);

static void register_hook(hook_channel_req_t *hdata)
{
	mychan_t *mc = hdata->mc;

	if (mc == NULL || mc->chan == NULL)
		return;

	modestack_mode_simple(chansvs.nick, mc->chan, MTYPE_ADD, CMODE_PERM);
}

static void drop_hook(mychan_t *mc)
{
	if (mc == NULL || mc->chan == NULL)
		return;

	modestack_mode_simple(chansvs.nick, mc->chan, MTYPE_DEL, CMODE_PERM);
}

void
_modinit(module_t *m)
{
	hook_add_event("channel_register");
	hook_add_channel_register(register_hook);

	hook_add_event("channel_drop");
	hook_add_channel_drop(drop_hook);
}

void
_moddeinit(module_unload_intent_t intent)
{
	hook_del_channel_register(register_hook);
	hook_del_channel_drop(drop_hook);
}
