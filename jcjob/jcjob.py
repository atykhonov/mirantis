#!/usr/bin/env python

import argcomplete
import argparse
from jenkinsapi.jenkins import Jenkins

import os
import sys
import time

from utils import can_read, is_build_running, parse_config


system_settings = '/etc/jcjob/jcjobrc'
home_settings = os.path.join(os.path.expanduser("~"), '.jcjobrc')
pwd_settings = os.path.join(os.getcwd(), '.jcjobrc')

settings = {}

if can_read(system_settings):
    settings.update(parse_config(system_settings))

if can_read(home_settings):
    settings.update(parse_config(home_settings))

if can_read(pwd_settings):
    settings.update(parse_config(pwd_settings))

if os.environ.get('JCJOB_USERNAME'):
    settings['username'] = os.environ.get('JCJOB_USERNAME')

if os.environ.get('JCJOB_PASSWORD'):
    settings['password'] = os.environ.get('JCJOB_PASSWORD')

if os.environ.get('JCJOB_JENKINS_URL'):
    settings['jenkins_url'] = os.environ.get('JCJOB_JENKINS_URL')

if os.environ.get('JCJOB_JOB'):
    settings['job'] = os.environ.get('JCJOB_JOB')


def job_params(prefix, parsed_args, **kwargs):
    cached_params_dir = os.path.expanduser('~/.config/fjj/cache')
    cached_params_file = '{0}/{1}'.format(cached_params_dir, settings['job'])
    if os.path.isfile(cached_params_file):
        with open(cached_params_file) as f:
            return f.readlines()
    jenkins = Jenkins(
        settings['jenkins_url'], settings['username'], settings['password'])
    job = jenkins[settings['job']]
    params = [p['name'] for p in job.get_params()]
    if not os.path.exists(cached_params_dir):
        os.makedirs(cached_params_dir)
    with open(cached_params_file, 'w') as f:
        for p in params:
            f.write('{0}\n'.format(p))
    return params

parser = argparse.ArgumentParser(description='jcjob')
parser.add_argument('-j', '--job', help='Jenkins Job to be built')
parser.add_argument(
    '-r', '--url', dest='jenkins_url', nargs='?', help='Jenkins URL')
parser.add_argument('-u', '--username', nargs='?', help='Jenkins username')
parser.add_argument('-w', '--password', nargs='?', help='Jenkins password')
parser.add_argument(
    '-o', '--output', default='.artifacts', nargs='?',
    help='Output dir for the artifacts')
parser.add_argument(
    '-p', '--parameter', nargs='+', help='Jenkins Job Parameter').completer = job_params
parser.add_argument(
    '-P', '--file-parameter', nargs='+', default=[],
    help='Jenkins Job Parameter given from file')

argcomplete.autocomplete(parser)

args = parser.parse_args()

for key, value in args.__dict__.items():
    if value:
        settings[key] = value

print 'Settings: '
print settings

# job_name = 'custom_8.0_iso'

output_dir = os.path.join(os.getcwd(), args.output)

build_params = {}
for parameter in args.parameter:
    parameter = parameter.strip('\'')
    parameter = parameter.strip('"')
    parsed_param = parameter.split('=', 1)
    build_params[parsed_param[0]] = parsed_param[1]

for parameter in args.file_parameter:
    build_params.update(parse_config(parameter))

jenkins = Jenkins(
    settings['jenkins_url'], settings['username'], settings['password'])

job = jenkins[settings['job']]
try:
    job.invoke(build_params=build_params)
except Exception:
    pass

time.sleep(10)

builds = job.get_build_dict()
last_builds_ids = list(reversed(builds.keys()))[:5]

build = None
for build_id in last_builds_ids:
    build = job.get_build(build_id)
    actions = build.get_actions()
    if actions['causes'][0].get('userId') == settings['username']:
        break

if build:
    output = ''
    last_size = 0
    while is_build_running(build):
        time.sleep(1)
        console = build.get_console()
        if last_size == len(console):
            continue
        diff = console[len(output):]
        print diff
        last_size = len(console)
    if build.is_good():
        artifacts_dir = os.path.join(output_dir, build_id)
        if not os.path.exists(artifacts_dir):
            os.makedirs(artifacts_dir)
        for name, artifact in build.get_artifact_dict().items():
            with open(os.path.join(artifacts_dir, name), 'w') as f:
                f.write(artifact.get_data())
    else:
        sys.exit(1)
