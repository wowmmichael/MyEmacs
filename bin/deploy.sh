#!/bin/bash

rsync -vr --exclude-from './bin/rsync_exclude_from.txt' ../MyEmacs ~/.emacs.d/
rsync -vb --suffix=bak dotEmacs ~/.emacs
