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
#ifndef ORES_ORE_CORE_PLANNER_IMPORT_CHOICES_HPP
#define ORES_ORE_CORE_PLANNER_IMPORT_CHOICES_HPP

#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/text_form.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::ore::planner {

/**
 * @brief Controls how currencies are imported.
 */
enum class currency_import_mode {
    all,         ///< Import all currencies found in scan
    missing_only ///< Skip ISO codes already present in the database
};

/**
 * @brief Controls what happens to portfolios and books that already exist
 *        in the database (matched by name) during a re-import.
 */
enum class reimport_mode {
    /// Skip existing portfolios/books; attach new trades to them as-is.
    add_trades_only,
    /// Re-save existing portfolios/books with their current UUID, producing
    /// a new temporal version, then add new trades.
    create_new_versions
};

/**
 * @brief Default field values to stamp onto imported trades.
 *
 * Fields left empty are not overridden; the value parsed from the ORE
 * XML is kept as-is.
 */
struct trade_defaults {
    std::string trade_date;
    std::string activity_type_code;
    std::optional<boost::uuids::uuid> default_counterparty_id;
};

[[nodiscard]] constexpr auto wire_fields(const trade_defaults&) {
    return std::tuple{
        utility::codec::field{"trade_date", &trade_defaults::trade_date},
        utility::codec::field{"activity_type_code", &trade_defaults::activity_type_code},
        utility::codec::field{"default_counterparty_id", &trade_defaults::default_counterparty_id}};
}
static_assert(utility::codec::names_every_member<trade_defaults>());

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
     * @brief How to handle portfolios and books that already exist.
     */
    reimport_mode portfolio_reimport_mode = reimport_mode::add_trades_only;

    /**
     * @brief Name for the optional wrapping parent portfolio.
     *
     * The wrapping portfolio needs this name to be built. A book at the
     * top of the scanned tree has no portfolio above it, and the database
     * rejects a book with no parent portfolio, so an import over such a
     * tree must name the wrapping portfolio.
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
     * @brief UUID of a pre-existing portfolio to use as parent.
     *
     * When set, all imported top-level portfolios are attached under this
     * portfolio instead of creating a new parent portfolio. The
     * create_parent_portfolio flag and parent_portfolio_name are ignored.
     */
    std::optional<boost::uuids::uuid> existing_parent_portfolio_id;

    /**
     * @brief UUID of an existing book to import all trades into.
     *
     * When set, the portfolio/book hierarchy creation step is skipped.
     * All trades are stamped with this book_id and the book's
     * parent_portfolio_id (from existing_parent_portfolio_id) is used for
     * portfolio_id.  create_parent_portfolio and parent_portfolio_name are
     * ignored.
     */
    std::optional<boost::uuids::uuid> existing_target_book_id;

    /**
     * @brief Sandbox the import lands in.
     *
     * When set, every portfolio the import creates belongs to this sandbox and
     * every book it creates is a virtual book of it, so nothing enters the
     * official portfolio tree. The wrapping parent portfolio, if any, is a
     * root of the sandbox. An existing parent portfolio must belong to the
     * sandbox too, which the database enforces (a portfolio and its parent
     * share a sandbox). The choice does not apply when existing_target_book_id
     * is set: the import then creates no portfolio or book. Left empty, the
     * import service picks the party's open sample sandbox, unless official
     * is set or the import names an existing parent portfolio or book.
     */
    std::optional<boost::uuids::uuid> sandbox_id;

    /**
     * @brief Land the import in the official portfolio tree on purpose.
     *
     * Without it, an import that names no sandbox goes to the party's sample
     * sandbox and never creates an official portfolio or book.
     */
    std::optional<bool> official;

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
     * @brief Functional currency for auto-created books.
     */
    std::string functional_currency = "USD";

    /**
     * @brief Purpose type for auto-created portfolios.
     */
    std::string purpose_type = "Internal";
};

[[nodiscard]] constexpr auto wire_fields(const import_choices&) {
    using c = import_choices;
    return std::tuple{
        utility::codec::field{"currency_mode", &c::currency_mode},
        utility::codec::field{"portfolio_reimport_mode", &c::portfolio_reimport_mode},
        utility::codec::field{"parent_portfolio_name", &c::parent_portfolio_name},
        utility::codec::field{"create_parent_portfolio", &c::create_parent_portfolio},
        utility::codec::field{"scan_exclusions", &c::scan_exclusions},
        utility::codec::field{"hierarchy_strip", &c::hierarchy_strip},
        utility::codec::field{"defaults", &c::defaults},
        utility::codec::field{"existing_parent_portfolio_id", &c::existing_parent_portfolio_id},
        utility::codec::field{"existing_target_book_id", &c::existing_target_book_id},
        utility::codec::field{"sandbox_id", &c::sandbox_id},
        utility::codec::field{"official", &c::official},
        utility::codec::field{"party_id", &c::party_id},
        utility::codec::field{"aggregation_ccy", &c::aggregation_ccy},
        utility::codec::field{"functional_currency", &c::functional_currency},
        utility::codec::field{"purpose_type", &c::purpose_type}};
}
static_assert(utility::codec::names_every_member<import_choices>());

}

template <>
struct ores::utility::codec::text_form<ores::ore::planner::currency_import_mode> {
    using mode = ores::ore::planner::currency_import_mode;
    static constexpr std::string_view wire_name = "a currency import mode";

    static std::string to_text(const mode& v) {
        switch (v) {
            case mode::all:
                return "all";
            case mode::missing_only:
                return "missing_only";
        }
        throw std::invalid_argument("Not " + std::string(wire_name));
    }

    static mode from_text(std::string_view text) {
        if (text == "all")
            return mode::all;
        if (text == "missing_only")
            return mode::missing_only;
        throw std::invalid_argument("Not " + std::string(wire_name) + ": " + std::string(text));
    }
};

template <>
struct ores::utility::codec::text_form<ores::ore::planner::reimport_mode> {
    using mode = ores::ore::planner::reimport_mode;
    static constexpr std::string_view wire_name = "a reimport mode";

    static std::string to_text(const mode& v) {
        switch (v) {
            case mode::add_trades_only:
                return "add_trades_only";
            case mode::create_new_versions:
                return "create_new_versions";
        }
        throw std::invalid_argument("Not " + std::string(wire_name));
    }

    static mode from_text(std::string_view text) {
        if (text == "add_trades_only")
            return mode::add_trades_only;
        if (text == "create_new_versions")
            return mode::create_new_versions;
        throw std::invalid_argument("Not " + std::string(wire_name) + ": " + std::string(text));
    }
};

#endif
