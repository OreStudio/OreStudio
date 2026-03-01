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
#ifndef ORES_ORE_PLANNER_ORE_IMPORT_PLANNER_HPP
#define ORES_ORE_PLANNER_ORE_IMPORT_PLANNER_HPP

#include <set>
#include <string>
#include "ores.ore/scanner/scan_result.hpp"
#include "ores.ore/planner/import_choices.hpp"
#include "ores.ore/planner/ore_import_plan.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::ore::planner {

/**
 * @brief Produces a fully-stamped ore_import_plan from a scan_result and
 * user-supplied import_choices.
 *
 * This is a pure-logic class (no network calls). It:
 * 1. Filters currencies by iso_code against existing_iso_codes when
 *    currency_mode == missing_only.
 * 2. Uses ore_hierarchy_builder to derive portfolios and books from the
 *    portfolio file paths, optionally prepending a parent portfolio.
 * 3. Calls importer::import_portfolio_with_context for each book's source
 *    files and stamps the resulting trades with context fields.
 */
class ore_import_planner {
private:
    inline static std::string_view logger_name =
        "ores.ore.planner.ore_import_planner";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs the planner.
     *
     * @param scan_result        Result of ore_directory_scanner::scan().
     * @param existing_iso_codes ISO codes already present in the database.
     * @param choices            User-supplied import options.
     */
    explicit ore_import_planner(
        scanner::scan_result scan_result,
        std::set<std::string> existing_iso_codes,
        import_choices choices);

    /**
     * @brief Builds the import plan.
     *
     * Pure function — safe to call multiple times (each call re-runs the
     * hierarchy builder and re-imports trades).
     *
     * @return Fully-populated ore_import_plan ready for batch saves.
     */
    ore_import_plan plan();

private:
    scanner::scan_result scan_result_;
    std::set<std::string> existing_iso_codes_;
    import_choices choices_;
};

}

#endif
