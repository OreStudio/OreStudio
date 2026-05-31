#!/usr/bin/env bash
# set-ssh-agent.sh — Export SSH_AUTH_SOCK from the project SSH agent directory.
#
# Usage (must be sourced, not executed):
#   source build/scripts/set-ssh-agent.sh
#
# The SSH agent socket lives in /home/marco/.ssh/agent/ and is always the
# sole file in that directory. The filename changes when the agent restarts
# (reboot, crash), so this script derives it mechanically rather than
# hardcoding the path.

AGENT_DIR="/home/marco/.ssh/agent"
AGENT_SOCK=$(ls "$AGENT_DIR" 2>/dev/null | head -1)

if [[ -z "$AGENT_SOCK" ]]; then
    echo "set-ssh-agent.sh: no socket found in $AGENT_DIR" >&2
    return 1 2>/dev/null || exit 1
fi

export SSH_AUTH_SOCK="$AGENT_DIR/$AGENT_SOCK"
