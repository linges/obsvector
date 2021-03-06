.PHONY: rel deps elm run

all: deps compile elm

compile:
	rebar compile

elm:
	elm-make elm/Main.elm --output=priv/static/index.html

run:
	erl -sname obsvector -pa ../obsvector/ebin ../obsvector/deps/*/ebin ../obsvector -s obsvector

deps:
	rebar get-deps &&\
  elm-make --yes

clean:
	rebar clean &&\
	rm -rf priv &&\
  rm -rf elm-stuff/build-artifacts

clean-all:
	rm -rf priv elm-stuff ebin deps
