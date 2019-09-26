const unsigned int interval = 1000;

static const char unknown_str[] = "n/a";

#define MAXLEN 2048

static const struct arg args[] = {
	/* function format          argument */
	{ run_command, "%s | ",     "/home/tux/barscripts/wifi" },
	{ battery_perc, "%s%% | ",  "BAT1" },
	{ datetime, "%s",           "%F %T" },
};
