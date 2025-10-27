/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include <charconv>
#include "ores.utility/string/conversion_error.hpp"
#include "ores.utility/string/converter.hpp"

namespace {

const std::string invalid_argument("Invalid argument: ");
const std::string out_of_range("Out of range: ");
const std::string other_conversion_error("Unspecified conversion error: ");

}

namespace ores::utility::string {

using namespace ores::utility::log;

int converter::string_to_int(std::string s, int base) {
    BOOST_LOG_SEV(lg(), debug) << "Converting to int: " << s;
    int r;
    auto [ptr, ec] = std::from_chars(s.data(), s.data() + s.size(), r, base);

     if (ec == std::errc()) {
        BOOST_LOG_SEV(lg(), debug) << "Converted successfully: " << r;
        return r;
    } else if (ec == std::errc::invalid_argument) {
        BOOST_LOG_SEV(lg(), error) << invalid_argument << s;
        BOOST_THROW_EXCEPTION(conversion_error(invalid_argument + s));
    } else if (ec == std::errc::result_out_of_range) {
        BOOST_LOG_SEV(lg(), error) << invalid_argument << s;
        BOOST_THROW_EXCEPTION(conversion_error(out_of_range + s));
     } else {
         BOOST_LOG_SEV(lg(), error) << other_conversion_error << s;
         BOOST_THROW_EXCEPTION(conversion_error(other_conversion_error + s));
     }
}

}
