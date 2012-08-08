all: beam

BUILT=\
   ebin/nsime_simulator.beam \
   ebin/nsime_node.beam \
   ebin/nsime_channel.beam \
   ebin/nsime_queue.beam \
   ebin/nsime_application.beam 

beam: $(BUILT)

ebin/%.beam: src/%.erl
			@mkdir -p ebin
			erlc -o ebin/ $<
