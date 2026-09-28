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
#ifndef ORES_MARKETDATA_CORE_TESTS_CORPUS_FILES_HPP
#define ORES_MARKETDATA_CORE_TESTS_CORPUS_FILES_HPP

#include <algorithm>
#include <filesystem>
#include <string>
#include <vector>

namespace ores::marketdata::test {

/**
 * @brief Whether a path under external/ore/examples is a market payload.
 *
 * The corpus ships more than market data, and ORE's own outputs sit beside the
 * inputs, so the selection is stated once here rather than in each test that
 * walks the tree. Two tests already did state it separately, and a definition
 * that is silently different in one of them changes what "the corpus" means
 * without changing any test's name.
 *
 * The exclusions, in order: a fixing file is a different payload; the file must
 * name "market"; it must be text or CSV; todaysmarketcalibration.csv is a
 * calibration dump that would invent a pseudo-type per curve name it lists; and
 * the dated MD_*.csv dumps are the same thing under another name.
 */
inline bool is_market_payload(const std::string& path) {
    const auto name = std::filesystem::path(path).filename().string();
    if (name.find("fixing") != std::string::npos)
        return false;
    if (name.find("market") == std::string::npos)
        return false;
    if (!name.ends_with(".txt") && !name.ends_with(".csv"))
        return false;
    if (path.find("ExpectedOutput") != std::string::npos)
        return false;
    return name.rfind("MD_", 0) != 0;
}

/**
 * @brief Every market payload under @p root, sorted, for a deterministic walk.
 */
inline std::vector<std::filesystem::path> market_payloads(const std::filesystem::path& root) {
    std::vector<std::filesystem::path> found;
    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        if (entry.is_regular_file() && is_market_payload(entry.path().string()))
            found.push_back(entry.path());
    }
    std::sort(found.begin(), found.end());
    return found;
}

} // namespace ores::marketdata::test

#endif
