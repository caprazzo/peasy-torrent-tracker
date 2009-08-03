%% This is the application resource file (.app file) for the 'base'
%% application.
{application, log4erl,
[{description, "Logger for erlang in the spirit of Log4J"},
 {vsn, "0.8.6"},
 {modules, [log4erl, log_manager, log_formatter,
 			log4erl_sup, log4erl_lex, file_appender,
 			console_appender, log4erl_parser, log4erl_conf,
 			log4erl_utils, logger_guard]},
 {registered,[log4erl]},
 {applications, [kernel,stdlib]},
 {mod, {log4erl,[default_logger]}},
 {start_phases, []}
]}.
