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
#ifndef ORES_ORE_SCANNER_SCAN_RESULT_HPP
#define ORES_ORE_SCANNER_SCAN_RESULT_HPP

#include <vector>
#include <filesystem>

namespace ores::ore::scanner {

/**
 * @brief Result of scanning an ORE directory for importable files.
 */
struct scan_result {
    /**
     * @brief Root directory that was scanned.
     */
    std::filesystem::path root;

    /**
     * @brief Currency configuration files (currencyconfig.xml).
     */
    std::vector<std::filesystem::path> currency_files;

    /**
     * @brief Portfolio XML files (portfolio*.xml).
     */
    std::vector<std::filesystem::path> portfolio_files;

    /**
     * @brief Files that did not match any known category or were in excluded dirs.
     */
    std::vector<std::filesystem::path> ignored_files;
};

}

#endif
