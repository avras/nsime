{application, nsime,
    [
      {vsn, "0.1"},
      {modules, [
                  nsime_simulator,
                  nsime_node,
                  nsime_node_list,
                  nsime_time,
                  nsime_data_rate,
                  nsime_gbtrees_scheduler,
                  nsime_netdevice,
                  nsime_ptp_channel,
                  nsime_ptp_netdevice,
                  nsime_droptail_queue,
                  nsime_ipv4_address,
                  nsime_ipv4_mask,
                  nsime_ipv4_header,
                  nsime_ipv4_interface_address,
                  nsime_ipv4_interface,
                  nsime_ipv4_routing_table_entry,
                  nsime_ipv4_routing_protocol,
                  nsime_ipv4_static_routing,
                  nsime_ipv4_list_routing,
                  nsime_icmpv4_header,
                  nsime_icmpv4_echo_header,
                  nsime_icmpv4_dest_unreachable_header,
                  nsime_ip_endpoint,
                  nsime_ip_endpoint_demux,
                  nsime_udp_header,
                  nsime_udp_protocol,
                  nsime_udp_socket,
                  nsime_udp_echo_client,
                  nsime_udp_echo_server
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
