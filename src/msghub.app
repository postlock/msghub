{application,msghub,
             [{description,"MsgHub"},
              {vsn,"0.01"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{msghub_app,[]}},
              {env,[]},
              {modules,[
                    msghub_app,                 % application module
                    msghub_session_handler,     % handles session events
                    msghub_sup,                 % application supervisor
                    msghub_listener             % code for starting/stopping listeners
                    ]}]}.
