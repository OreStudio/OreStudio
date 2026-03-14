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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_COMMS_SHELL_APP_REPL_HPP
#define ORES_COMMS_SHELL_APP_REPL_HPP

#include <iosfwd>
#include <memory>
#include "ores.logging/make_logger.hpp"
#include "ores.comms.shell/service/nats_session.hpp"
#include "ores.comms.shell/app/pagination_context.hpp"

namespace cli {

class Cli;
class CliSession;
class Menu;

}

namespace ores::comms::shell::app {

/**
 * @brief Interactive REPL (Read-Eval-Print Loop) for the ORE Studio client.
 *
 * Provides a command-line interface for interacting with the ORE Studio server,
 * including commands for connection management and data retrieval.
 */
class repl final {
private:
    inline static std::string_view logger_name =
        "ores.comms.shell.app.repl";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct a REPL instance with a NATS session.
     *
     * @param session Reference to a nats_session.
     */
    explicit repl(service::nats_session& session);

    repl(const repl&) = delete;
    repl& operator=(const repl&) = delete;
    repl(repl&&) = delete;
    repl& operator=(repl&&) = delete;

    /**
     * @brief Run the REPL session using std::cin and std::cout.
     */
    void run();

    /**
     * @brief Run the REPL session with custom streams.
     *
     * @param in Input stream to read commands from.
     * @param out Output stream to write responses to.
     */
    void run(std::istream& in, std::ostream& out);

private:
    /**
     * @brief Setup the command menu structure.
     */
    std::unique_ptr<::cli::Cli> setup_menus();

    /**
     * @brief Display the welcome message.
     */
    void display_welcome(std::ostream& out) const;

    /**
     * @brief Perform cleanup on REPL exit.
     */
    void cleanup();

    service::nats_session& session_;
    pagination_context pagination_;
    ::cli::CliSession* active_session_{nullptr};
};

}

#endif
