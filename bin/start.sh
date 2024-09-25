#!/bin/bash

file_name=$1

if [ -z "$file_name" ]; then
	echo "usage: $0 <file-name>"
	exit 1
fi

# start tmux, vim the file on the top pane and chez on the bottom pane
tmux new-session -d -s scheme "vim $file_name" \
    \; split-window -v -l 8 "chez $file_name" \
    \; select-pane -t 0 \
    \; attach
