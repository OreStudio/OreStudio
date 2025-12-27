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
#ifndef ORES_SHELL_APP_APPLICATION_HPP
#define ORES_SHELL_APP_APPLICATION_HPP

#include <optional>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.comms/net/client_options.hpp"
#include "ores.shell/config/login_options.hpp"

namespace ores::shell::app {

/**
 * @brief Entry point for the ores client application.
 */
class application final {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.application";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct application with configuration.
     */
    explicit application(std::optional<comms::net::client_options> connection_config = std::nullopt,
                         std::optional<config::login_options> login_config = std::nullopt);

    application(const application&) = delete;
    application& operator=(const application&) = delete;

    /**
     * @brief Executes the application.
     *
     * Starts the REPL and blocks until the user exits.
     */
    void run() const;

private:
    std::optional<comms::net::client_options> connection_config_;
    std::optional<config::login_options> login_config_;
};

}

#endif
