#!/bin/sh

(sleep 60; diskutil unmount "$2" >> /Library/Logs/CCC.log 2>&1) &
(/usr/libexec/PlistBuddy -c "Print" /Library/Logs/CCC.stats | tail -n 9 | mail -s "CCC Scheduled Task Report" mike@hales.ws) &