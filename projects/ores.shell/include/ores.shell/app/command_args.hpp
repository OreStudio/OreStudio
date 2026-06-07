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
#ifndef ORES_SHELL_APP_COMMAND_ARGS_HPP
#define ORES_SHELL_APP_COMMAND_ARGS_HPP

#include "ores.shell/export.hpp"
#include <chrono>
#include <cstdint>
#include <expected>
#include <map>
#include <optional>
#include <string>
#include <vector>

namespace ores::shell::app {

/**
 * @brief Declares one optional --flag accepted by a command.
 *
 * A switch (requires_value == false) is either present ("true") or
 * absent (defaults to "false"). A value flag accepts "--name value"
 * or "--name=value" and falls back to default_value when absent.
 */
struct flag_spec {
    /// Flag name without the leading dashes, e.g. "continue-on-error".
    std::string name;
    /// True when the flag carries a value; false for a boolean switch.
    bool requires_value = false;
    /// Value used when the flag is not supplied. Switches use "false".
    std::string default_value;
};

/**
 * @brief Result of parsing a command's argument tokens.
 */
struct ORES_SHELL_EXPORT parsed_args {
    /// Arguments that are not flags, in order of appearance.
    std::vector<std::string> positionals;
    /// Flag name to effective value; switches hold "true"/"false".
    std::map<std::string, std::string> flags;

    /** @brief True when a boolean switch was supplied. */
    bool flag_set(const std::string& name) const;

    /** @brief Effective value of a flag (supplied or default). */
    const std::string& flag(const std::string& name) const;
};

/**
 * @brief Parse raw argument tokens into positionals and declared flags.
 *
 * Flags may appear anywhere among the positionals. An undeclared flag,
 * or a value flag with no value, yields an error message suitable for
 * direct display to the user. The cli library hands commands plain
 * strings; this is the single place that turns them into options.
 */
ORES_SHELL_EXPORT std::expected<parsed_args, std::string>
parse_args(const std::vector<std::string>& tokens,
           const std::vector<flag_spec>& specs);

/**
 * @brief Parse a duration flag value as a positive whole number of
 * seconds. Rejects partial numbers ("30x"), zero and negatives.
 */
ORES_SHELL_EXPORT std::optional<std::chrono::seconds>
parse_positive_seconds(const std::string& value);

/**
 * @brief Parse a flag value as an unsigned 32-bit integer.
 * Rejects partial numbers, negatives and out-of-range values.
 */
ORES_SHELL_EXPORT std::optional<std::uint32_t> parse_uint32(const std::string& value);

/**
 * @brief Parse a flag value as an unsigned 64-bit integer.
 * Rejects partial numbers, negatives and out-of-range values.
 */
ORES_SHELL_EXPORT std::optional<std::uint64_t> parse_uint64(const std::string& value);

}

#endif
