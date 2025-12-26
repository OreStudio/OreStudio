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
#ifndef ORES_COMMS_ANALYSER_CONFIG_PARSER_HPP
#define ORES_COMMS_ANALYSER_CONFIG_PARSER_HPP

#include <optional>
#include "ores.comms.analyser/config/options.hpp"

namespace ores::comms::analyser::config {

/**
 * @brief Parses command-line arguments.
 *
 * Returns options if parsing succeeded, or std::nullopt if the program
 * should exit (e.g., --help or --version was requested).
 */
class parser {
public:
    /**
     * @brief Parse command-line arguments.
     *
     * @param argc Argument count
     * @param argv Argument values
     * @return Options if parsing succeeded, or std::nullopt to exit
     * @throws std::runtime_error on parsing errors
     */
    static std::optional<options> parse(int argc, const char* argv[]);
};

}

#endif
