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
#ifndef ORES_CLI_CONFIG_IMPORTING_CONFIG_HPP
#define ORES_CLI_CONFIG_IMPORTING_CONFIG_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <iosfwd>
#include <vector>
#include <filesystem>
#include "ores.cli/config/entity.hpp"

namespace ores::cli::config {

/**
 * @brief Configuration related to importing data into the system.
 */
struct import_options final {
    /**
     * @brief Which entity to target.
     */
    entity entity;
    /**
     * @brief Target files containing import data. Format is inferred from
     * extension.
     */
    std::vector<std::filesystem::path> targets;
};

std::ostream& operator<<(std::ostream& s, const import_options& v);

}

#endif
