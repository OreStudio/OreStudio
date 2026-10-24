# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
"""The Logging parameters: the log, its detail and the progress and structured logs."""

from analytic_parameter_descriptions._entry import FLAG, entry

OUTPUT_FILE = "A file name or a path. ORE writes the file in the output path of the Setup block."
SIZE = "A whole number of bytes, for example 104857600 for 100 MiB."

ENTRIES = {
    "logFile": entry(
        "Log file",
        "A file name for the log.",
        "Names the file that ORE writes its log to. The path is relative to the output path. This parameter wins over the "
        "parameter of the same name in the Setup block. For example, log.txt writes the log to the file log.txt in the output "
        "directory.",
        "None. ORE uses the log file of the Setup block.", OUTPUT_FILE),
    "logMask": entry(
        "Log detail",
        "A number that selects the kinds of message to log.",
        "Selects which messages ORE writes to the log. Each kind of message has one bit: Alert is 1, Critical is 2, Error is 4, "
        "Warning is 8, Notice is 16, Debug is 32 and Data is 64. ORE writes a message when its bit is set in the mask. "
        "For example, 31 writes alerts, criticals, errors, warnings and notices. 255 writes everything, the most detail and the "
        "largest file. This parameter wins over the parameter of the same name in the Setup block.",
        "None. ORE uses the mask of the Setup block.", "A whole number from 0 to 255."),
    "progressLogFile": entry(
        "Progress log file",
        "A file name for the progress log.",
        "Names the file that ORE writes the progress log to. The progress log holds short messages in JSON that say how far "
        "a run has come, so a program can show progress. The name can hold a Boost pattern such as %N, which is "
        "the number of the file after a rotation. For example, my_log_progress_%N.json.",
        "log_progress.json in the output path.", OUTPUT_FILE),
    "progressLogRotationSize": entry(
        "Progress log rotation size",
        "The size in bytes at which the progress log starts a new file.",
        "Sets the size a progress log file can grow to. When a file reaches it, ORE starts the next file. "
        "For example, 10485760 is 10 MiB. A value of 0 sets no limit.",
        "0. The size has no limit.", SIZE),
    "progressLogToConsole": entry(
        "Progress log to console",
        "A flag. Y also writes the progress log to the console.",
        "Decides whether ORE writes the progress log to the standard output as well as to its file. "
        "Use Yes when a program starts ORE and reads its output to show progress.",
        "No. ORE writes the progress log to its file only.", FLAG),
    "structuredLogFile": entry(
        "Structured log file",
        "A file name for the structured log.",
        "Names the file that ORE writes the structured log to. The structured log holds messages about the data of a run, "
        "for example a trade that failed to build, in a form a program can read. The name can hold a Boost pattern such as %N, "
        "which is the number of the file after a rotation. For example, my_structured_logs_%N.txt.",
        "log_structured.json in the output path.", OUTPUT_FILE),
    "structuredLogRotationSize": entry(
        "Structured log rotation size",
        "The size in bytes at which the structured log starts a new file.",
        "Sets the size a structured log file can grow to. When a file reaches it, ORE starts the next file. "
        "For example, 10485760 is 10 MiB. A value of 0 sets no limit.",
        "0. The size has no limit.", SIZE),
}
