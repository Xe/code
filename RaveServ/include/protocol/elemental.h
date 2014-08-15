#ifndef ELEMENTAL_H

#define ELEMENTAL_H

#define CHFL_PEON	0x0000 /* Normal channel user */
#define CHFL_VOICE	0x0001 /* Voiced channel user (+, +v) */
#define CHFL_HALFOP	0x0002 /* Channel half-operator (%, +h) */
#define CHFL_CHANOP	0x0004 /* Channel operator (@ +o) */
#define CHFL_ADMIN	0x0008 /* Channel admin (! +a) */
#define CHFL_OWNER	0x0010 /* Channel owner (~ +y) */

#define ANY_OP		(CHFL_HALFOP|CHFL_CHANOP|CHFL_ADMIN|CHFL_OWNER)

// The folloing macros check a ChanUser to see if they are at a level
#define is_any_op(x)	((x) && (x)->status & ANY_OP)

// Channel modes
#define CHM_PRIVATE	0x000000001 /* private channel, +p */
#define CHM_SECRET	0x000000002 /* secret channel, +s */
#define CHM_MUTED	0x000000004 /* muted channel, +m */
#define CHM_TOPICRST	0x000000008 /* prevent non-ops from TOPIC, +t */
#define CHM_INVITE	0x000000010 /* invite only, +i */
#define CHM_INTERNAL	0x000000020 /* disallow external messages, +n */
#define CHM_REGONLY	0x000000040 /* registered clients only, +r */
#define CHM_NOCOLOR	0x000000080 /* disallow color codes, +c */
#define CHM_BIGLIST	0x000000100 /* bigger channel lists, +L */
#define CHM_PERSIST	0x000000200 /* don't vanish without users, +P */
#define CHM_OPMOD	0x000000400 /* messages blocked by +bmq go to ops, +z */
#define CHM_ALLINVITE	0x000000800 /* allow any client to invite, +g */
#define CHM_TARGET	0x000001000 /* free forwarding allowance, +F  */
#define CHM_FORWARD	0x000002000 /* forward channel, +f */
#define CHM_THROTTLE	0x000004000 /* join throttle, +j */
#define CHM_NOTARGET	0x000008000 /* disallows forwards, +Q */
#define CHM_NOCTCP	0x000010000 /* blocks CTCP messages, +C */
#define CHM_NONOTICE	0x000020000 /* blocks NOTICE, +T */
#define CHM_NOACTION	0x000040000 /* blocks CTCP ACTION, +D */
#define CHM_NOKICKS	0x000080000 /* blocks KICKs, +E */
#define CHM_NONICKS	0x000100000 /* blocks nick changes, +d */
#define CHM_NOCAPS	0x000200000 /* blocks capital letters, +G */
#define CHM_NOREJOIN	0x000400000 /* blocks rejoin after kick, +J */
#define CHM_NOREPEAT	0x000800000 /* blocks repeat messages, +K */
#define CHM_NOOPERKICK	0x001000000 /* blocks kicks against opers, +M */
#define CHM_HIDEBANS	0x002000000 /* blocks banlists from non-ops, +u */

#endif
