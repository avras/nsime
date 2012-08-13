all: src 

touch: 
			touch src/*.erl

tests: src
			ct_run -pa ebin/ -logdir ./logs -dir ./test  -cover ./cover.spec

BUILT=\
   ebin/nsime_simulator.beam \
   ebin/nsime_scheduler.beam \
   ebin/nsime_gbtrees_scheduler.beam \
   ebin/nsime_node.beam \
   ebin/nsime_channel.beam \
   ebin/nsime_queue.beam \
   ebin/nsime_application.beam 

src: $(BUILT) 

ebin/nsime_simulator.beam: src/nsime_simulator.erl include/nsime_event.hrl
			erlc +debug_info -o ebin/ -I include/ $<

ebin/nsime_scheduler.beam: src/nsime_scheduler.erl 
			erlc +debug_info -o ebin/ -I include/ $<

ebin/nsime_gbtrees_scheduler.beam: src/nsime_gbtrees_scheduler.erl src/nsime_scheduler.erl
			erlc +debug_info -o ebin/ -I include/ -pa ebin/ $<


