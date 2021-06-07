.PHONY: help steup venv dependencies freeze lint run clean clean-cache clean-venv

.DEFAULT: help

# ----------------------- #
# Define some variables   #
# ----------------------- #

MAIN = normal_numbers.py
REQUIREMENTS = requirements.txt
VENV ?= venv

VENV_ACTIVATE = $(VENV)/bin/activate
PYTHON = $(VENV)/bin/python3
PIP = $(PYTHON) -m pip
LINTER = $(PYTHON) -m flake8

# ----------------------- #
# Help                    #
# ----------------------- #

help:
	@echo "---------------HELP-----------------"
	@echo "To run the project, type: make run"
	@echo "------------------------------------"

# ----------------------- #
# Setup                   #
# ----------------------- #

setup: clean-venv venv dependencies freeze

venv:
	test -d $(VENV) || python3 -m venv $(VENV)
	. $(VENV_ACTIVATE)
	$(PIP) install --upgrade pip

dependencies: venv $(REQUIREMENTS)
	$(PIP) install -r $(REQUIREMENTS)

freeze: $(REQUIREMENTS)
	$(PIP) freeze > $(basename $(REQUIREMENTS))_freeze.txt

# ----------------------- #
# Lint
# ----------------------- #

lint: venv
	$(PIP) install flake8
	# stop the build if there are Python syntax errors or undefined names
	$(LINTER) . --exclude=$(VENV) --count --select=E9,F63,F7,F82 --show-source --statistics
	# exit-zero treats all errors as warnings. The GitHub editor is 127 chars wide
	$(LINTER) . --exclude=$(VENV) --count --exit-zero --max-complexity=10 --max-line-length=127 --statistics

# ----------------------- #
# Run the project #
# ----------------------- #

run: dependencies
	$(PYTHON) $(MAIN)

# ----------------------- #
# Clean                   #
# ----------------------- #

clean: clean-cache clean-venv

clean-cache:
	rm -rf .pytest_cache

clean-venv:
	rm -rf $(VENV)
