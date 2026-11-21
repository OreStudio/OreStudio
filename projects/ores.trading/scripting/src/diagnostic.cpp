/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.trading.scripting/diagnostic.hpp"
#include <algorithm>
#include <array>

namespace ores::trading::scripting {

namespace {

constexpr std::array codes = {
    diagnostic_code{"SCR1001",
                    severity::error,
                    "The script does not parse",
                    "ORE's grammar stopped at this position. It reports the first syntax error "
                    "only, so a further error is reported once this one is corrected."},
    diagnostic_code{"SCR2001",
                    severity::error,
                    "An input has no value",
                    "The script reads this input, and the trade states no variable for it."},
    diagnostic_code{"SCR2002",
                    severity::warning,
                    "A variable is never read",
                    "The trade states a variable the script does not read. It is ignored, and "
                    "is often a misspelt input."},
    diagnostic_code{"SCR2003",
                    severity::error,
                    "A variable has the wrong type",
                    "The trade states the variable as one type, and the script reads it as "
                    "another: Number, Event, Index, Currency or Daycounter."},
    diagnostic_code{"SCR2004",
                    severity::error,
                    "A variable is a list where one value is read, or the reverse",
                    "The script indexes the input or asks its size, so it reads a list, or "
                    "it reads one value; the trade states the other."},
    diagnostic_code{"SCR3001",
                    severity::error,
                    "The trade's values break a REQUIRE statement",
                    "Evaluated over the numbers the trade states and the sizes of its lists, "
                    "the statement's condition is false, so ORE would stop the run here."},
    diagnostic_code{"SCR4001",
                    severity::warning,
                    "A declared variable is never used",
                    "The script declares the variable with NUMBER and never reads or assigns "
                    "it."},
};

}

std::span<const diagnostic_code> diagnostic_codes() {
    return codes;
}

std::optional<diagnostic_code> code_of(std::string_view code) {
    const auto it = std::ranges::find(codes, code, &diagnostic_code::code);
    if (it == codes.end())
        return std::nullopt;
    return *it;
}

std::string to_compiler_format(const diagnostic& d, std::string_view file) {
    std::string out(file);
    const auto line = d.start ? d.start->line : std::size_t{1};
    out += ":" + std::to_string(line);
    if (d.start && d.start->column)
        out += ":" + std::to_string(*d.start->column);
    out += d.severity == severity::error ? ": error: " : ": warning: ";
    if (d.subject)
        out += *d.subject + ": ";
    out += d.message + " [" + d.code + "]";
    return out;
}

}
