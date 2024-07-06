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
#include "ores.utility/io/std_optional.hpp"
#include "ores.console/configuration.hpp"

namespace ores::console {

configuration::configuration(configuration&& rhs) noexcept
    : logging_(std::move(rhs.logging_)) { }

configuration::configuration(
    std::optional<ores::utility::log::logging_configuration> logging,
    std::optional<importing_configuration> importing)
    : logging_(std::move(logging)), importing_(std::move(importing)) { }

void configuration::swap(configuration& other) noexcept {
    using std::swap;
    swap(logging_, other.logging_);
}

bool configuration::operator==(const configuration& rhs) const {
    return logging_ == rhs.logging_;
}

configuration& configuration::operator=(configuration other) {
    using std::swap;
    swap(*this, other);
    return *this;
}

std::optional<ores::utility::log::logging_configuration>
configuration::logging() const {
    return logging_;
}

void configuration::logging(std::optional<ores::utility::log::logging_configuration> v) {
    logging_ = std::move(v);
}

std::optional<importing_configuration> configuration::importing() const {
    return importing_;
}

void configuration::importing(std::optional<importing_configuration> v) {
    importing_ = std::move(v);
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
