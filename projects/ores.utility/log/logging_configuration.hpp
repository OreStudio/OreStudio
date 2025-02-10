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
#ifndef ORES_UTILITY_LOG_LOGGING_CONFIGURATION_HPP
#define ORES_UTILITY_LOG_LOGGING_CONFIGURATION_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <iosfwd>
#include <string>
#include <filesystem>

namespace ores::utility::log {

/**
 * @brief Options related to logging.
 */
class logging_configuration final {
public:
    logging_configuration(const logging_configuration&) = default;
    logging_configuration(logging_configuration&& rhs) noexcept = default;
    ~logging_configuration() = default;

    logging_configuration& operator=(const logging_configuration&) = default;
    logging_configuration& operator=(logging_configuration&&) noexcept = default;

    logging_configuration() : output_to_console_(false) { }
    logging_configuration(std::string severity, std::string filename,
        bool output_to_console, std::filesystem::path output_directory);

    /**
     * @brief Level at which to log.
     */
    /**@{*/
    std::string severity() const { return severity_; }
    void severity(const std::string& v) { severity_ = v; }
    void severity(std::string&& v) { severity_ = std::move(v); }
    /**@}*/

    /**
     * @brief Name of the file to log into.
     *
     * If empty, file logging is disabled.
     */
    /**@{*/
    std::string filename() const { return filename_; }
    void filename(const std::string& v) { filename_ = v; }
    void filename(std::string&& v) { filename_ = std::move(v); }
    /**@}*/

    /**
     * @brief If true, dumps the log into the console.
     */
    /**@{*/
    bool output_to_console() const { return output_to_console_; }
    void output_to_console(bool v) { output_to_console_ = v; }
    /**@}*/

    /**
     * @brief Directory in which to place the output.
     */
    /**@{*/
    std::filesystem::path output_directory() const { return output_directory_; }
    void output_directory(const std::filesystem::path& v) {
        output_directory_ = v;
    }
    void output_directory(std::filesystem::path&& v) {
        output_directory_ = std::move(v);
    }
    /**@}*/

    void swap(logging_configuration& other) noexcept;

private:
    std::string severity_;
    std::string filename_;
    bool output_to_console_;
    std::filesystem::path output_directory_;
};

std::ostream& operator<<(std::ostream& s, const logging_configuration& v);

void swap(logging_configuration& lhs, logging_configuration& rhs);

}

#endif
