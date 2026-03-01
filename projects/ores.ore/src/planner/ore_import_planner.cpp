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
#include "ores.ore/planner/ore_import_planner.hpp"

#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.ore/xml/importer.hpp"
#include "ores.ore/hierarchy/ore_hierarchy_builder.hpp"
#include "ores.database/domain/change_reason_constants.hpp"

namespace ores::ore::planner {

using namespace ores::logging;
namespace reason = ores::database::domain::change_reason_constants;

ore_import_planner::ore_import_planner(
    scanner::scan_result scan_result,
    std::set<std::string> existing_iso_codes,
    import_choices choices)
    : scan_result_(std::move(scan_result)),
      existing_iso_codes_(std::move(existing_iso_codes)),
      choices_(std::move(choices)) {}

ore_import_plan ore_import_planner::plan() {
    ore_import_plan result;
    boost::uuids::random_generator uuid_gen;

    // =========================================================================
    // Step 1: Currencies
    // =========================================================================
    for (const auto& path : scan_result_.currency_files) {
        auto currencies = xml::importer::import_currency_config(path);
        for (auto& c : currencies) {
            if (choices_.currency_mode == currency_import_mode::missing_only &&
                existing_iso_codes_.count(c.iso_code) > 0) {
                BOOST_LOG_SEV(lg(), debug)
                    << "Skipping existing currency: " << c.iso_code;
                continue;
            }
            c.change_reason_code = std::string(reason::codes::new_record);
            c.change_commentary = "Imported from ORE directory";
            result.currencies.push_back(std::move(c));
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Planned " << result.currencies.size()
                              << " currencies";

    // =========================================================================
    // Step 2: Portfolio / Book hierarchy
    // =========================================================================
    hierarchy::ore_hierarchy_builder builder(
        scan_result_.portfolio_files,
        scan_result_.root,
        choices_.exclusions);

    const auto nodes = builder.build();

    // Node index → assigned UUID (covers both portfolios and books)
    std::vector<boost::uuids::uuid> node_uuids(nodes.size());
    for (auto& id : node_uuids) {
        id = uuid_gen();
    }

    // Optional wrapping parent portfolio
    std::optional<boost::uuids::uuid> parent_portfolio_id;
    if (choices_.create_parent_portfolio && !choices_.parent_portfolio_name.empty()) {
        const auto parent_uuid = uuid_gen();
        parent_portfolio_id = parent_uuid;

        refdata::domain::portfolio parent_p;
        parent_p.id = parent_uuid;
        parent_p.party_id = choices_.party_id;
        parent_p.name = choices_.parent_portfolio_name;
        parent_p.aggregation_ccy = choices_.aggregation_ccy;
        parent_p.purpose_type = choices_.purpose_type;
        parent_p.status = "Active";
        parent_p.is_virtual = 0;
        parent_p.change_reason_code = std::string(reason::codes::new_record);
        parent_p.change_commentary = "Imported from ORE directory";
        result.portfolios.push_back(std::move(parent_p));
    }

    // Create portfolio domain objects (in node order — parents before children)
    for (std::size_t i = 0; i < nodes.size(); ++i) {
        const auto& node = nodes[i];
        if (node.type != hierarchy::import_node::node_type::portfolio) {
            continue;
        }

        refdata::domain::portfolio p;
        p.id = node_uuids[i];
        p.party_id = choices_.party_id;
        p.name = node.name;
        p.aggregation_ccy = choices_.aggregation_ccy;
        p.purpose_type = choices_.purpose_type;
        p.status = "Active";
        p.is_virtual = 0;
        p.change_reason_code = std::string(reason::codes::new_record);
        p.change_commentary = "Imported from ORE directory";

        if (node.parent_index) {
            p.parent_portfolio_id = node_uuids[*node.parent_index];
        } else if (parent_portfolio_id) {
            p.parent_portfolio_id = parent_portfolio_id;
        }

        result.portfolios.push_back(std::move(p));
    }

    // Create book domain objects and stamp trades
    for (std::size_t i = 0; i < nodes.size(); ++i) {
        const auto& node = nodes[i];
        if (node.type != hierarchy::import_node::node_type::book) {
            continue;
        }

        // Determine parent portfolio UUID for this book
        boost::uuids::uuid book_parent_id;
        if (node.parent_index) {
            book_parent_id = node_uuids[*node.parent_index];
        } else if (parent_portfolio_id) {
            book_parent_id = *parent_portfolio_id;
        }

        refdata::domain::book b;
        b.id = node_uuids[i];
        b.party_id = choices_.party_id;
        b.name = node.name;
        b.parent_portfolio_id = book_parent_id;
        b.ledger_ccy = choices_.ledger_ccy;
        b.book_status = "Active";
        b.is_trading_book = 1;
        b.change_reason_code = std::string(reason::codes::new_record);
        b.change_commentary = "Imported from ORE directory";
        result.books.push_back(b);

        // Import and stamp trades for this book
        const auto& defs = choices_.defaults;
        for (const auto& source_file : node.source_files) {
            auto items =
                xml::importer::import_portfolio_with_context(source_file);

            for (auto& item : items) {
                item.trade.id = uuid_gen();
                item.trade.book_id = b.id;
                item.trade.portfolio_id = book_parent_id;
                item.trade.party_id = choices_.party_id;

                if (!defs.trade_date.empty())
                    item.trade.trade_date = defs.trade_date;
                if (!defs.effective_date.empty())
                    item.trade.effective_date = defs.effective_date;
                if (!defs.termination_date.empty())
                    item.trade.termination_date = defs.termination_date;
                if (!defs.lifecycle_event.empty())
                    item.trade.lifecycle_event = defs.lifecycle_event;
                if (defs.default_counterparty_id)
                    item.trade.counterparty_id = defs.default_counterparty_id;

                result.trades.push_back(std::move(item));
            }
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Import plan: "
                              << result.currencies.size() << " currencies, "
                              << result.portfolios.size() << " portfolios, "
                              << result.books.size() << " books, "
                              << result.trades.size() << " trades";
    return result;
}

}
