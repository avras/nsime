all: src 

touch: 
			touch src/*.erl

tests: src
			ct_run -pa ebin/ -logdir ./logs -dir ./test  -cover ./cover.spec

BUILT=\
   ebin/nsime_simulator.beam \
   ebin/nsime_scheduler.beam \
   ebin/nsime_time.beam \
   ebin/nsime_gbtrees_scheduler.beam \
   ebin/nsime_node.beam \
   ebin/nsime_node_list.beam \
   ebin/nsime_ptp_channel.beam \
   ebin/nsime_queue.beam \
   ebin/nsime_drop_tail_queue.beam \
   ebin/nsime_application.beam 

src: $(BUILT) 

ebin/nsime_simulator.beam: src/nsime_simulator.erl \
                           include/nsime_event.hrl \
                           include/nsime_simulator_state.hrl
			erlc +debug_info -o ebin/ -I include/ $<

ebin/nsime_scheduler.beam: src/nsime_scheduler.erl 
			erlc +debug_info -o ebin/ -I include/ $<

ebin/nsime_time.beam: src/nsime_time.erl include/nsime_types.hrl
			erlc +debug_info -o ebin/ -I include/ $<

ebin/nsime_gbtrees_scheduler.beam: src/nsime_gbtrees_scheduler.erl src/nsime_scheduler.erl
			erlc +debug_info -o ebin/ -I include/ -pa ebin/ $<

ebin/nsime_node.beam: src/nsime_node.erl include/nsime_node_state.hrl
			erlc +debug_info -o ebin/ -I include/ -pa ebin/ $<

ebin/nsime_node_list.beam: src/nsime_node_list.erl
			erlc +debug_info -o ebin/ -I include/ -pa ebin/ $<

ebin/nsime_ptp_channel.beam: src/nsime_ptp_channel.erl include/nsime_ptp_channel_state.hrl
			erlc +debug_info -o ebin/ -I include/ -pa ebin/ $<

ebin/nsime_drop_tail_queue.beam: src/nsime_drop_tail_queue.erl include/nsime_dtq_state.hrl
			erlc +debug_info -o ebin/ -I include/ -pa ebin/ $<
