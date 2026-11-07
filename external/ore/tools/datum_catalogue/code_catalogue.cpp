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
#include <ql/time/calendar.hpp>
#include <ql/time/daycounter.hpp>
#include <iostream>
#include <ored/utilities/parsers.hpp>
#include <string>

/**
 * @file code_catalogue.cpp
 * @brief Reads ORE codes and prints the name the QuantLib object reports.
 *
 * ORE's documents name a calendar and a day counter by code, such as
 * UnitedStates.SOFR or ACT/365.FIXED, and the object ORE builds from one
 * reports its own name, which for several calendars is a display name rather
 * than the code. Anything that has to turn a QuantLib name back into an ORE
 * code needs that correspondence, and reading it from ORE is truer than
 * writing it by hand.
 *
 * One entry per input line: the kind, a tab, and the code. Blank lines and
 * lines starting with '#' are skipped. The output is the same line with the
 * name ORE reports appended, or ORE's own error.
 *
 *   printf 'calendar\tTARGET\n' | code_catalogue
 *   calendar	TARGET	TARGET
 */

namespace {

void report(const std::string& kind, const std::string& code) {
    std::cout << kind << '\t' << code;
    try {
        if (kind == "calendar")
            std::cout << '\t' << ore::data::parseCalendar(code).name();
        else if (kind == "daycounter")
            std::cout << '\t' << ore::data::parseDayCounter(code).name();
        else
            std::cout << "\tunknown kind";
    } catch (const std::exception& e) {
        std::cout << "\t!" << e.what();
    }
    std::cout << '\n';
}

}

int main() {
    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.empty() || line.front() == '#')
            continue;
        const auto tab = line.find('\t');
        if (tab == std::string::npos) {
            std::cerr << "not 'kind<TAB>code': " << line << '\n';
            return 1;
        }
        report(line.substr(0, tab), line.substr(tab + 1));
    }
    return 0;
}
