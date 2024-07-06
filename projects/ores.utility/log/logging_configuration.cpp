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

logging_configuration::logging_configuration()
    : output_to_console_(false) { }

logging_configuration::logging_configuration(logging_configuration&& rhs) noexcept
    : severity_(std::move(rhs.severity_)),
      filename_(std::move(rhs.filename_)),
      output_to_console_(rhs.output_to_console_),
      output_directory_(std::move(rhs.output_directory_)) { }

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

bool logging_configuration::operator==(const logging_configuration& rhs) const {
    return severity_ == rhs.severity_ &&
        filename_ == rhs.filename_ &&
        output_to_console_ == rhs.output_to_console_ &&
        output_directory_ == rhs.output_directory_;
}

logging_configuration& logging_configuration::operator=(logging_configuration other) {
    using std::swap;
    swap(*this, other);
    return *this;
}

std::string logging_configuration::severity() const {
    return severity_;
}

void logging_configuration::severity(std::string v) {
    severity_ = std::move(v);
}

std::string logging_configuration::filename() const {
    return filename_;
}

void logging_configuration::filename(std::string v) {
    filename_ = std::move(v);
}

bool logging_configuration::output_to_console() const {
    return output_to_console_;
}

void logging_configuration::output_to_console(bool v) {
    output_to_console_ = v;
}

std::filesystem::path logging_configuration::output_directory() const {
    return output_directory_;
}

void logging_configuration::output_directory(std::filesystem::path v) {
    output_directory_ = std::move(v);
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


}
