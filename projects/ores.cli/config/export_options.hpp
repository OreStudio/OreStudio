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
#ifndef ORES_CLI_CONFIG_EXPORT_OPTIONS_HPP
#define ORES_CLI_CONFIG_EXPORT_OPTIONS_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <iosfwd>
#include <string>
#include "ores.cli/config/entity.hpp"
#include "ores.cli/config/format.hpp"

namespace ores::cli::config {

/**
 * @brief Configuration related to exporting data into a supported export
 * format.
 */
struct export_options final {
    /**
     * @brief Which entity to export.
     */
    entity target_entity;
    /**
     * @brief Timepoint to use for the reading. If empty, use latest.
     */
    std::string as_of;
    /**
     * @brief Key to filter by, if any.
     */
    std::string key;
    /**
     * @brief If true, output all versions of this entity.
     */
    bool all_versions;
    /**
     * @brief Format to use for the export.
     */
    format target_format;
};

std::ostream& operator<<(std::ostream& s, const export_options& v);

}

#endif
