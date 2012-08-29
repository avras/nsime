{application, nsime,
    [
      {vsn, "0.1"},
      {modules, [
                  nsime_utils,
                  nsime_simulator,
                  nsime_node,
                  nsime_node_list,
                  nsime_time,
                  nsime_data_rate,
                  nsime_scheduler,
                  nsime_gbtrees_scheduler,
                  nsime_ptp_channel,
                  nsime_ptp_netdevice,
                  nsime_drop_tail_queue
                ]
      },
      {registered, [
                      nsime_simulator,
                      nsime_node_list,
                      nsime_gbtrees_scheduler
                   ]
      }
    ]
}.
