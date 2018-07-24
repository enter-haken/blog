PACKAGE_MANAGER = 'yarn'
	
.PHONY: default
default: build

.PHONY: build
build:
	if [ ! -d node_modules ]; then $(PACKAGE_MANAGER) install; fi
	PUBLIC_URL=/example/rasmus/alpha $(PACKAGE_MANAGER) run build

.PHONY: clean
clean:
	rm build/ -rf

.PHONY: clean_node_modules 
clean_node_modules:
	rm node_modules/ -rf

.PHONY: deep_clean
deep_clean: clean clean_node_modules

.PHONY: all
all: clean build

.PHONY: run
run: 
	$(PACKAGE_MANAGER) run start
