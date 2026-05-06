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
#ifndef ORES_CLI_CONFIG_ORE_ROUNDTRIP_OPTIONS_HPP
#define ORES_CLI_CONFIG_ORE_ROUNDTRIP_OPTIONS_HPP

#include <iosfwd>
#include <filesystem>

namespace ores::cli::config {

/**
 * @brief Options for the ore roundtrip command.
 *
 * Walks input_dir recursively, imports every portfolio XML via the
 * ores.ore import pipeline, exports it back to XML, and writes the result
 * to the mirrored path under output_dir. No database or network access.
 */
struct ore_roundtrip_options final {
    std::filesystem::path input_dir;
    std::filesystem::path output_dir;
};

std::ostream& operator<<(std::ostream& s, const ore_roundtrip_options& v);

}

#endif
