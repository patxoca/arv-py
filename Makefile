
tests:
	@cask exec ert-runner --quiet --reporter ert \
		| sed -e 's/^\( *failed\)/\o033[31m\1\o033[0m/' \
		| sed -e 's/^\( *passed\)/\o033[32m\1\o033[0m/' \
