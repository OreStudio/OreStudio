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
#ifndef ORES_SHELL_APP_COMMAND_FEEDBACK_HPP
#define ORES_SHELL_APP_COMMAND_FEEDBACK_HPP

#include "ores.shell/export.hpp"
#include <iosfwd>

namespace ores::shell::app {

/**
 * @brief Uniform failure signal for shell commands.
 *
 * The cli library's command handlers return void, so a command has no
 * machine-readable way to report failure to callers such as the load
 * command's script runner. Commands mark failure here (normally via
 * fail()); the script runner resets the flag before feeding each line
 * and inspects it afterwards to implement stop-on-error semantics.
 *
 * The flag is thread-local: each shell session processes commands on
 * one thread, and nested script executions share the same flag so an
 * inner abort propagates to the outer script.
 */
class ORES_SHELL_EXPORT command_feedback final {
public:
    command_feedback() = delete;

    /** @brief Clear the failure flag ahead of executing a command. */
    static void reset();

    /** @brief Record that the current command failed. */
    static void mark_failure();

    /** @brief True if a failure was recorded since the last reset. */
    static bool failed();
};

/**
 * @brief Report a command failure: marks the failure flag and emits
 * the conventional "✗ " prefix, returning the stream for the message.
 *
 * Usage: fail(out) << "Request failed: " << e.what() << std::endl;
 */
ORES_SHELL_EXPORT std::ostream& fail(std::ostream& out);

}

#endif
