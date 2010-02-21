{application, etap, [
    {description, "Erlang Test Anything Protocol"},
    {vsn, "0.3.4"},
    {modules, [
		etap, etap_application, etap_can, etap_exception, etap_process,
		etap_report, etap_request, etap_string, etap_web
    ]},
    {registered, []},
    {applications, [kernel, stdlib]}
]}.
