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

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <optional>
#include <filesystem>
#include "ores.utility/log/severity_level.hpp"
#include "ores.utility/log/logging_options.hpp"

namespace ores::utility::log {

/**
 * @brief Manages the starting and stopping of logging for an application.
 */
class lifecycle_manager final {
public:
    lifecycle_manager(lifecycle_manager&&) = delete;
    lifecycle_manager& operator=(const lifecycle_manager&) = delete;

private:
    /**
     * @brief Creates a boost log file backend.
     *
     * @note path is non-const by ref by design.
     */
    void create_file_backend(std::filesystem::path path,
        severity_level severity);

    /**
     * @brief Creates a boost log console backend.
     */
    void create_console_backend(severity_level severity);

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

    /**
     * @brief Returns true if the logging system has been initialised
     * at least once.
     */
    bool enabled() const { return enabled_; }

private:
    bool enabled_;
};

}

#endif
