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
#include "ores.utility/streaming/std_optional.hpp"
#include "ores.console/configuration.hpp"

namespace ores::console {

configuration::configuration(
    std::optional<ores::utility::log::logging_configuration> logging,
    std::optional<importing_configuration> importing)
    : logging_(std::move(logging)), importing_(std::move(importing)) { }

void configuration::swap(configuration& other) noexcept {
    using std::swap;
    swap(logging_, other.logging_);
    swap(importing_, other.importing_);
}

std::ostream& operator<<(std::ostream& s, const configuration& v) {
    s << " { "
      << "\"__type__\": " << "\"ores::console::configuration\"" << ", "
      << "\"logging\": " << v.logging() << ", "
      << "\"importing\": " << v.importing()
      << " }";
    return(s);
}

}
