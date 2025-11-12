/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_UTILITY_LOG_LIFE_CYCLE_MANAGER_HPP
#define ORES_UTILITY_LOG_LIFE_CYCLE_MANAGER_HPP

#include <optional>
#include <filesystem>
#include <boost/shared_ptr.hpp>
#include <boost/log/sinks.hpp>
#include "ores.utility/log/severity_level.hpp"
#include "ores.utility/log/logging_options.hpp"

namespace ores::utility::log {

/**
 * @brief Manages the starting and stopping of logging for an application.
 *
 * Note: this class uses boost shared_ptr due to legacy reasons (boost log does
 * not support std::shared_ptr).
 */
class lifecycle_manager final {
private:
    using file_sink_type = boost::log::sinks::synchronous_sink<
        boost::log::sinks::text_file_backend>;
    using console_sink_type = boost::log::sinks::synchronous_sink<
        boost::log::sinks::text_ostream_backend>;

public:
    lifecycle_manager() = delete;
    lifecycle_manager(lifecycle_manager&&) = delete;
    lifecycle_manager& operator=(const lifecycle_manager&) = delete;

private:
    /**
     * @brief Creates a boost log file sink.
     *
     * @note path is non-const by ref by design.
     */
  static boost::shared_ptr<file_sink_type>
  make_file_sink(std::filesystem::path path, severity_level severity,
      std::string tag);

    /**
     * @brief Creates a boost log console sink.
     */
    static boost::shared_ptr<console_sink_type> make_console_sink(
        severity_level severity, std::string tag);

public:

    /**
     * @brief Initialise logging for the entire application.
     *
     * If no configuration is supplied, logging is disabled.
     *
     * @note Must be done in a thread-safe context.
     */
    explicit lifecycle_manager(std::optional<logging_options> ocfg);

    /**
     * @brief Shutdown logging for the entire application.
     *
     * Should be done in a thread-safe context.
     */
    ~lifecycle_manager();

private:
    boost::shared_ptr<file_sink_type> file_sink_;
    boost::shared_ptr<console_sink_type> console_sink_;
};

}

#endif
