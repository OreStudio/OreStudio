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
#include "ores.utility/log/logging_configuration.hpp"

namespace ores::utility::log {

logging_configuration::logging_configuration(
    std::string severity, std::string filename,
    bool output_to_console, std::filesystem::path output_directory)
    : severity_(std::move(severity)), filename_(std::move(filename)),
      output_to_console_(output_to_console),
      output_directory_(std::move(output_directory)) { }

void logging_configuration::swap(logging_configuration& other) noexcept {
    using std::swap;
    swap(severity_, other.severity_);
    swap(filename_, other.filename_);
    swap(output_to_console_, other.output_to_console_);
    swap(output_directory_, other.output_directory_);
}

std::ostream& operator<<(std::ostream& s, const logging_configuration& v) {
    s << " { "
      << "\"__type__\": " << "\"ores::console::logging_configuration\"" << ", "
      << "\"severity\": " << "\"" << v.severity() << "\", "
      << "\"filename\": " << "\"" << v.filename() << "\", "
      << "\"output_to_console\": " << v.output_to_console() << ", "
      << "\"output_directory\": " << "\""
      << v.output_directory().generic_string() << "\""
      << " }";
    return(s);
}

void swap(logging_configuration& lhs, logging_configuration& rhs) {
    lhs.swap(rhs);
}

}
