jcjob
=====

This little script provides a handy way to build a Jenkins job directly from command line.

For example:

jcjob -j mybuild -p param1=val1 param2=val2 -u username -p password -r http://localhost:8000/

It will start `mybuild` with specific parameters and will be outputed build console log to the stdout. If job fails the script will exit with status 1. If job succeed then it will output artifacts to the $PWD/.artifacts/{build_id}/ directory.

Settings:
username - registered jenkins username
password - password for this username
jenkins_url - base URL to the jenkins
job - job which will be used to build

Settings could be specified in command line and in a setting file as well. The script looks for the settings in the following files: /etc/jcjob/jcjobrc, $HOME/.jcjobrc, $PWD/.jcjobrc. Also they might be passed via environment variables: JCJOB_JENKINS_URL, JCJOB_USERNAME, JCJOB_PASSWORD and JCJOB_JOB.
