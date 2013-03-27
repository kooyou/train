{application,chat,
[{description,"IRC Lite"},
{vsn,"1.0"},
{module,[chat_app,server,client,chat_data,
         protocol_pro,c_protocol,connector,timer_handler]},
{registered,[server]},
{applications,[kernel,stdlib]},
{mod,{chat_app,[]}},
{start_phases,[]}
]}.
