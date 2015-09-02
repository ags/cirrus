PROJECT = cirrus

.PHONY: shell
shell:
	docker run -it \
		--name $(PROJECT)-shell \
		-v $(PWD):/src \
		ags/haskell:7.10.2 \
		bash
