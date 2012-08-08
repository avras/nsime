all: beam

BUILT=\
   ebin/nsime_simulator.beam \
   ebin/nsime_scheduler.beam \
   ebin/nsime_gbtrees_scheduler.beam \
   ebin/nsime_node.beam \
   ebin/nsime_channel.beam \
   ebin/nsime_queue.beam \
   ebin/nsime_application.beam 

beam: $(BUILT) 

ebin/nsime_simulator.beam: src/nsime_simulator.erl include/nsime_event.hrl
			erlc -o ebin/ -I include/ $<

ebin/nsime_scheduler.beam: src/nsime_scheduler.erl 
			erlc -o ebin/ -I include/ $<

ebin/nsime_gbtrees_scheduler.beam: src/nsime_gbtrees_scheduler.erl src/nsime_scheduler.erl
			erlc -o ebin/ -I include/ -pa ebin/ $<
