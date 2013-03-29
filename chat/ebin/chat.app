{application,chat,
[{description,"IRC Lite"},
{vsn,"1.0"},
{module,[chat_app,server,client,chat_data,manager_client,
         protocol_pro,c_protocol,connector,timer_handler]},
{registered,[server,manager_client,chat_data]},
{applications,[kernel,stdlib]},
{mod,{chat_app,[]}},
{start_phases,[]}
]}.
