.PHONY: copydemo

all:


NEW_NAME ?= demo2
OLD_NAME = demo
copy_template:
	@$(RM) -r $(NEW_NAME)
	cp $(OLD_NAME) $(NEW_NAME) -r
	@$(RM) $(NEW_NAME)/DONT_REMOVE_THIS_DIRECTORY.md
	@sed 's/(name $(OLD_NAME))/(name $(NEW_NAME))/g' $(NEW_NAME)/dune-project -i
	@sed 's/public_name $(OLD_NAME)/public_name $(NEW_NAME)/g' $(NEW_NAME)/bin/dune -i
	@mv $(NEW_NAME)/$(OLD_NAME).opam $(NEW_NAME)/$(NEW_NAME).opam
	@mv $(NEW_NAME)/$(OLD_NAME).opam.template $(NEW_NAME)/$(NEW_NAME).opam.template
	@echo "\033[5m\033[1mПереименуйте Васю Пупкина в себя\033[22m\033[0m"
	grep -n --color=auto -e FIXME -e 'FIXME Vasya Pupkin' $(NEW_NAME)/dune-project -r

