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
#ifndef ORES_ORE_SCANNER_ORE_DIRECTORY_SCANNER_HPP
#define ORES_ORE_SCANNER_ORE_DIRECTORY_SCANNER_HPP

#include <filesystem>
#include <string>
#include <unordered_set>
#include <vector>
#include "ores.ore/scanner/scan_result.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::ore::scanner {

/**
 * @brief Scans an ORE directory tree and classifies files by type.
 *
 * Classifies files as:
 * - currency_files:  filename == "currencyconfig.xml"
 * - portfolio_files: filename starts with "portfolio" and ends with ".xml"
 * - ignored_files:   everything else, and files under excluded directories
 */
class ore_directory_scanner {
private:
    inline static std::string_view logger_name =
        "ores.ore.scanner.ore_directory_scanner";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs a scanner for the given root directory.
     *
     * @param root       Directory to scan recursively.
     * @param exclusions Directory name segments to skip (e.g. {"Input"}).
     */
    explicit ore_directory_scanner(
        std::filesystem::path root,
        std::unordered_set<std::string> exclusions = {});

    /**
     * @brief Walks the directory tree and classifies all files.
     *
     * @return Scan result with files split by category.
     */
    scan_result scan();

private:
    /**
     * @brief Returns true if any component of the relative path is excluded.
     */
    bool is_excluded(const std::filesystem::path& relative_path) const;

    std::filesystem::path root_;
    std::unordered_set<std::string> exclusions_;
};

}

#endif
