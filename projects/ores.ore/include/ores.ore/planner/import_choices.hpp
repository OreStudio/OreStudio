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
#ifndef ORES_ORE_PLANNER_IMPORT_CHOICES_HPP
#define ORES_ORE_PLANNER_IMPORT_CHOICES_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::ore::planner {

/**
 * @brief Controls how currencies are imported.
 */
enum class currency_import_mode {
    all,           ///< Import all currencies found in scan
    missing_only   ///< Skip ISO codes already present in the database
};

/**
 * @brief Default field values to stamp onto imported trades.
 *
 * Fields left empty are not overridden; the value parsed from the ORE
 * XML is kept as-is.
 */
struct trade_defaults {
    std::string trade_date;
    std::string effective_date;
    std::string termination_date;
    std::string lifecycle_event;
    std::optional<boost::uuids::uuid> default_counterparty_id;
};

/**
 * @brief User-supplied choices that drive the import plan.
 *
 * Collected across the wizard pages and passed to ore_import_planner.
 */
struct import_choices {
    /**
     * @brief How to handle currencies already in the database.
     */
    currency_import_mode currency_mode = currency_import_mode::missing_only;

    /**
     * @brief Name for the optional wrapping parent portfolio.
     */
    std::string parent_portfolio_name;

    /**
     * @brief If true, a single parent portfolio wraps all imported portfolios.
     */
    bool create_parent_portfolio = true;

    /**
     * @brief Directory name segments to skip during the filesystem scan.
     *
     * Files inside these directories are added to scan_result::ignored_files
     * and never classified or imported.  The default is empty — all
     * directories are scanned.
     */
    std::vector<std::string> scan_exclusions;

    /**
     * @brief Directory name segments to strip when building the
     * portfolio/book hierarchy.
     *
     * Default strips "Input" so that the standard ORE input sub-directory
     * (e.g. Example_1/Input/portfolio.xml) does not create an extra "Input"
     * portfolio node.  Files inside these directories are still scanned.
     */
    std::vector<std::string> hierarchy_strip = {"Input"};

    /**
     * @brief Default field values to stamp onto imported trades.
     */
    trade_defaults defaults;

    /**
     * @brief Party that owns the imported entities.
     *
     * Taken from ClientManager::currentPartyId() in the wizard.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Aggregation currency for auto-created portfolios.
     */
    std::string aggregation_ccy = "USD";

    /**
     * @brief Ledger currency for auto-created books.
     */
    std::string ledger_ccy = "USD";

    /**
     * @brief Purpose type for auto-created portfolios.
     */
    std::string purpose_type = "Internal";
};

}

#endif
