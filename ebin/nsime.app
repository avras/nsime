{application, nsime,
    [
      {vsn, "0.1"},
      {modules, [
                  nsime_simulator,
                  nsime_gbtrees_scheduler,
                  nsime_ptp_channel,
                  nsime_ptp_netdevice,
                  nsime_drop_tail_queue
                ]
      },
      {registered, [
                      nsime_simulator,
                      nsime_gbtrees_scheduler
                   ]
      }
    ]
}.
