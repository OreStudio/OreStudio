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
#include "ores.ore.core/scanner/ore_directory_scanner.hpp"
#include "ores.ore.core/xml/document_kind.hpp"

namespace ores::ore::scanner {

using namespace ores::logging;

ore_directory_scanner::ore_directory_scanner(std::filesystem::path root,
                                             std::unordered_set<std::string> exclusions)
    : root_(std::move(root))
    , exclusions_(std::move(exclusions)) {}

bool ore_directory_scanner::is_excluded(const std::filesystem::path& relative_path) const {
    for (const auto& component : relative_path) {
        if (exclusions_.contains(component.string()))
            return true;
    }
    return false;
}

scan_result ore_directory_scanner::scan() {
    BOOST_LOG_SEV(lg(), debug) << "Scanning directory: " << root_;

    scan_result result;
    result.root = root_;

    if (!std::filesystem::exists(root_)) {
        BOOST_LOG_SEV(lg(), warn) << "Directory does not exist: " << root_;
        return result;
    }

    for (const auto& entry : std::filesystem::recursive_directory_iterator(root_)) {
        if (!entry.is_regular_file()) {
            continue;
        }

        const auto relative = std::filesystem::relative(entry.path(), root_);

        if (is_excluded(relative)) {
            result.ignored_files.push_back(entry.path());
            continue;
        }

        if (entry.path().extension() != ".xml") {
            result.ignored_files.push_back(entry.path());
            continue;
        }

        // The round trip asks the same question of the same files, so the
        // rule lives in one place. A substring match here used to miss a
        // root element carrying attributes and match a keyword inside a
        // comment.
        const auto kind = xml::detect_document_kind(entry.path());
        if (kind == xml::document_kind::portfolio) {
            BOOST_LOG_SEV(lg(), debug) << "Portfolio file: " << entry.path();
            result.portfolio_files.push_back(entry.path());
        } else if (kind == xml::document_kind::currency_config) {
            BOOST_LOG_SEV(lg(), debug) << "Currency file: " << entry.path();
            result.currency_files.push_back(entry.path());
        } else {
            result.ignored_files.push_back(entry.path());
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Scan complete: " << result.currency_files.size()
                              << " currency files, " << result.portfolio_files.size()
                              << " portfolio files, " << result.ignored_files.size()
                              << " ignored files";
    return result;
}

}
