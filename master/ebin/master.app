{application,master,
             [{description,"master server"},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{master_app,[]}},
              {env,[]},
              {modules,[master,master_app,master_ctl,master_dist,master_sup,
                        reloader,term,term_stake,term_station]}]}.
