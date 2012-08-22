all: compile

compile: 
			rebar compile

tests: 
			rebar ct

clean: 
			rebar clean
