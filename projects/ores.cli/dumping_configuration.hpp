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
#ifndef ORES_CLI_DUMPING_CONFIGURATION_HPP
#define ORES_CLI_DUMPING_CONFIGURATION_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <iosfwd>
#include <string>

namespace ores::cli {

/**
 * @brief Configuration related to dumping data from the system.
 */
struct dumping_configuration final {
    /**
     * @brief Whether to dump currency configurations or not.
     */
    bool currency_configurations;
    /**
     * @brief Timepoint to use for the reading.
     */
    std::string as_of;
    /**
     * @brief Key to filter by.
     */
    std::string key;
    /**
     * @brief If true, output all versions.
     */
    bool all_versions;
};

std::ostream& operator<<(std::ostream& s, const dumping_configuration& v);

}

#endif
