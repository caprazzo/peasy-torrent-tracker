{application, peasy,
	[{description, "Peasy Torrent Tracker"},
	{vsn, "1.0"},
	{modules, [peasy_web, db, db_setup, peasy_supervisor, rest_interface]},
	{registered, [peasy_web, db, announce_manager, rest_interface]},
	{applications, [kernel, stdlib, mnesia]},
	{env, []},
	{mod, {peasy, [8080, 9090]}}]}.