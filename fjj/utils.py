import os

import requests
from retrying import retry


COMMENT_CHAR = '#'
OPTION_CHAR = '='


def parse_config(filename):
    options = {}
    f = open(filename)
    for line in f:
        # First, remove comments:
        if COMMENT_CHAR in line:
            # split on comment char, keep only the part before
            line, comment = line.split(COMMENT_CHAR, 1)
        # Second, find lines with an option=value:
        if OPTION_CHAR in line:
            # split on option char:
            option, value = line.split(OPTION_CHAR, 1)
            # strip spaces:
            option = option.strip()
            value = value.strip()
            # store in dictionary:
            options[option] = value
    f.close()
    return options


def can_read(filename):
    if os.path.isfile(filename) and os.access(filename, os.R_OK):
        return True
    return False


def retry_if_http_error(exception):
    return isinstance(exception, requests.exceptions.HTTPError)


def retry_if_connection_error(exception):
    return isinstance(exception, requests.exceptions.ConnectionError)


@retry(wait_exponential_multiplier=1000, wait_exponential_max=10000,
       retry_on_exception=retry_if_http_error)
def is_build_running(build):
    return build.is_running()


@retry(wait_exponential_multiplier=1000, wait_exponential_max=60000,
       retry_on_exception=retry_if_connection_error)
def build_get_console(build):
    return build.get_console()
