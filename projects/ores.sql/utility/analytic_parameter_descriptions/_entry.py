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
"""The shape of the text of one analytic parameter."""


def entry(label, help, description, default, format, required=False):
    """The text a screen shows for one analytic parameter.

    label is the name on the screen. help is one line next to the control.
    description says what the parameter decides, what each value does and when to
    change it. default is the default with its meaning. format says how a value is
    written. required is true when the document must carry the parameter.
    """
    return {"label": label, "help": help, "description": description,
            "default_text": default, "format": format, "required": required}


FLAG = "A flag, written Y or N, or true or false."
FILE = "A file name, written without a directory. ORE writes the file in the output path of the Setup block."
INPUT_FILE = "A file name. ORE looks for the file in the input path of the Setup block."
DATE = "A date, written as YYYY-MM-DD or YYYYMMDD."
COUNT = "A whole number of zero or more."
REAL = "A number, written with a decimal point."
