{application, peasy,
	[{description, "Peasy Torrent Tracker"},
	{vsn, "1.0"},
	{modules, [peasy_web, db, db_setup, peasy_supervisor]},
	{registered, [peasy_web, db]},
	{applications, [kernel, stdlib, mnesia]},
	{env, []},
	{mod, {peasy, [8080, 9090]}}]}.