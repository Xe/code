#ifndef CHANNEL_H

#define CHANNEL_H

#include "irc.h"
#include "hash.h"
#include "list.h"

typedef struct {
	long ts;
	char * topic;
	char * setter;
	long settime;
} Topic;

typedef struct {
	char * name;
	long ts;
	int modes;

	Topic * topic;
	hash_t * lists;
	hash_t * clients;
} Channel;

typedef struct {
	Client * client;
	Channel * channel;
	int status;
} ChanUser;

extern Channel * create_channel();
extern void destroy_channel(Channel * chan);

extern ChanUser * add_chan_user(Channel * chan, Client * client);
extern ChanUser * create_chanuser();
extern void del_chan_user(ChanUser * chuser);

extern Topic * create_topic();
extern void destroy_topic(Topic * topic);

#endif

