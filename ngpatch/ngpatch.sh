#!/bin/bash

set -e

function ngpatch() {
    CONTAINER_ID=$(ssh root@$2 docker inspect -f '{{.Id}}' $(ssh root@$2 docker ps | grep nailgun | awk '{print $1}'))
    ROOTFS=$(ssh root@$2 locate $CONTAINER_ID | grep rootfs | head -n 1)
    SITE_PACKAGES_DIR=$ROOTFS/usr/lib/python2.6/site-packages/
    scp $1 root@$2:$SITE_PACKAGES_DIR/$1
    ssh root@$2 patch -d $SITE_PACKAGES_DIR $3 -p0 < $1
}

PATCH="ngpatch.diff"
REVERSED_PATCH="ngreversedpatch.diff"

if [ -f $REVERSED_PATCH ];
then
    ngpatch $REVERSED_PATCH $1 "-R"
fi

git diff -u --no-prefix --relative=nailgun > $PATCH
ngpatch $PATCH $1 ""

mv $PATCH $REVERSED_PATCH

ssh root@$1 dockerctl shell nailgun supervisorctl restart nailgun
