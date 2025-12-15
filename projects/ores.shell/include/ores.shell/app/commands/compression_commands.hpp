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
#ifndef ORES_SHELL_APP_COMMANDS_COMPRESSION_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_COMPRESSION_COMMANDS_HPP

#include <cstdint>
#include <string>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/messaging/handshake_protocol.hpp"

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Manages compression settings for the shell client.
 *
 * Provides commands to enable/disable compression and select the algorithm.
 * The compression setting is applied when establishing new connections.
 */
class compression_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.compression";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    /**
     * @brief Current compression support bitmask.
     *
     * Defaults to COMPRESSION_SUPPORT_ALL (all algorithms enabled).
     */
    static std::uint8_t supported_compression_;

public:
    /**
     * @brief Register compression management commands.
     *
     * Adds compression command to the root menu.
     *
     * @param root The root menu to add commands to
     */
    static void register_commands(cli::Menu& root);

    /**
     * @brief Get the current compression support bitmask.
     *
     * Used by connection_commands when establishing connections.
     *
     * @return Current compression support bitmask
     */
    static std::uint8_t get_supported_compression() noexcept {
        return supported_compression_;
    }

    /**
     * @brief Process a compression command.
     *
     * Handles "compression off", "compression on", and
     * "compression on <algorithm>" commands.
     *
     * @param out Output stream for user feedback
     * @param enable Whether to enable compression ("on") or disable ("off")
     * @param algorithm Optional algorithm name (zlib, gzip, bzip2)
     */
    static void process_compression(std::ostream& out,
        const std::string& enable, const std::string& algorithm = "");
};

}

#endif
