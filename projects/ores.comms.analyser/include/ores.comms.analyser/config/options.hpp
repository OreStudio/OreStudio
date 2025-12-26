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
#ifndef ORES_COMMS_ANALYSER_CONFIG_OPTIONS_HPP
#define ORES_COMMS_ANALYSER_CONFIG_OPTIONS_HPP

#include <string>
#include <filesystem>

namespace ores::comms::analyser::config {

/**
 * @brief Command to execute.
 */
enum class command {
    read,   ///< Read and display session file contents
    info    ///< Display session file metadata only
};

/**
 * @brief Configuration options for the analyser.
 */
struct options {
    command cmd = command::read;
    std::filesystem::path input_file;
    bool verbose = false;
};

}

#endif
