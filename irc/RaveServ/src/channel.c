#include <stdlib.h>
#include <stdio.h>

#include "channel.h"

extern Channel * create_channel() {
	Channel * channel = malloc(sizeof(Channel));

	channel->lists = hash_new();
	channel->clients = hash_new();
	channel->topic = create_topic();

	return channel;
}

extern void destroy_channel(Channel * chan) {
	hash_each(chan->clients, {
		ChanUser * cuser = (ChanUser *) val;
		destroy_chanuser(cuser);
	});

	hash_each(chan->lists, {
		list_destroy((list_t *) val);
	});

	free(chan->name);
	destroy_topic(chan->topic);
	free(chan);
}

extern ChanUser * add_chan_user(Channel * chan, Client * client) {
	ChanUser * cuser = create_chanuser();

	cuser->channel = chan;
	cuser->client = client;

	hash_set(chan->clients, client->uid, client);

	return cuser;
}

extern ChanUser * create_chanuser() {
	ChanUser * chuser = malloc(sizeof(ChanUser));

	return chuser;
}

extern void del_chan_user(ChanUser * chuser) {
	Channel * chan = chuser->channel;
	Client * client = chuser->client;

	hash_del(chan->clients, client->uid);

	free(chuser);
}

extern Topic * create_topic() {
	Topic * t = malloc(sizeof(Topic));

	return t;
}

extern void destroy_topic(Topic * topic) {
	free(topic->topic);
	free(topic->setter);

	free(topic);

}

