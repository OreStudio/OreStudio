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
#include "ores.ore/xml/importer.hpp"

#include <sstream>
#include <type_traits>
#include <boost/uuid/random_generator.hpp>
#include "ores.platform/filesystem/file.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.ore/domain/domain.hpp"
#include "ores.ore/domain/currency_mapper.hpp"
#include "ores.ore/domain/calendar_adjustment_mapper.hpp"
#include "ores.ore/domain/conventions_mapper.hpp"
#include "ores.ore/domain/trade_mapper.hpp"

namespace ores::ore::xml {

using refdata::domain::currency;
using trading::domain::trade;
using namespace ores::logging;

std::string importer::validate_currency(const currency& c) {
    std::ostringstream errors;

    // Required fields per XSD
    if (c.name.empty())
        errors << "Name is required\n";

    if (c.iso_code.empty())
        errors << "ISO code is required\n";

    // if (c.symbol.empty())
    //     errors << "Symbol is required\n";

    // if (c.fraction_symbol.empty())
    //     errors << "Fraction symbol is required\n";

    if (c.fractions_per_unit <= 0)
        errors << "Fractions per unit must be positive\n";

    if (c.rounding_type.empty())
        errors << "Rounding type is required\n";

    if (c.rounding_precision < 0)
        errors << "Rounding precision must be non-negative\n";

    return errors.str();
}

std::vector<currency>
importer::import_currency_config(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started import: " << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::currencyConfig ccy_cfg;
    domain::load_data(c, ccy_cfg);
    const auto r = domain::currency_mapper::map(ccy_cfg);

    BOOST_LOG_SEV(lg(), debug) << "Finished importing " << r.size()
                               << " currencies. Result: " << r;

    return r;
}

std::string importer::validate_calendar_adjustment(
    const refdata::domain::calendar_adjustment& ca) {
    std::ostringstream errors;

    if (ca.calendar_name.empty())
        errors << "Calendar name is required\n";

    return errors.str();
}

std::vector<refdata::domain::calendar_adjustment>
importer::import_calendar_adjustments(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started import: " << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::calendaradjustment ca;
    domain::load_data(c, ca);
    const auto r = domain::calendar_adjustment_mapper::map(ca);

    BOOST_LOG_SEV(lg(), debug) << "Finished importing " << r.size()
                               << " calendar adjustments.";
    return r;
}

std::string importer::validate_trade(const trade& t) {
    std::ostringstream errors;

    if (t.external_id.empty())
        errors << "External ID is required\n";

    if (t.trade_type.empty())
        errors << "Trade type is required\n";

    return errors.str();
}

std::vector<trade>
importer::import_portfolio(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started portfolio import: "
                               << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::portfolio p;
    domain::load_data(c, p);
    const auto r = domain::trade_mapper::map(p);

    BOOST_LOG_SEV(lg(), debug) << "Finished importing " << r.size()
                               << " trades.";

    return r;
}

std::vector<trade_import_item>
importer::import_portfolio_with_context(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started portfolio import with context: "
                               << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::portfolio p;
    domain::load_data(c, p);

    std::vector<trade_import_item> r;
    r.reserve(p.Trade.size());

    boost::uuids::random_generator gen;
    for (const auto& t : p.Trade) {
        trade_import_item item;
        item.trade = domain::trade_mapper::map(t);
        item.source_file = path;
        if (t.Envelope && t.Envelope->CounterParty) {
            item.ore_counterparty_name = std::string(*t.Envelope->CounterParty);
        }

        try {
            item.instrument = domain::trade_mapper::map_instrument(t);

            // Assign each instrument its own UUID (independent of the trade),
            // wire the soft FKs in both directions, and record the routing
            // discriminator on the trade.
            std::visit([&](auto& result) {
                using T = std::decay_t<decltype(result)>;
                if constexpr (!std::is_same_v<T, std::monostate>) {
                    const auto instr_id = gen();
                    item.trade.instrument_id = instr_id;

                    using ores::trading::domain::product_type;
                    if constexpr (std::is_same_v<T, domain::swap_mapping_result>) {
                        item.trade.product_type = product_type::swap;
                        std::visit([&](auto& instr) {
                            instr.instrument_id = instr_id;
                            instr.trade_id = item.trade.id;
                        }, result.instrument);
                        for (auto& leg : result.legs)
                            leg.instrument_id = instr_id;
                    } else if constexpr (std::is_same_v<T, domain::fx_mapping_result>) {
                        item.trade.product_type = product_type::fx;
                        result.instrument.id = instr_id;
                        result.instrument.trade_id = item.trade.id;
                    } else if constexpr (std::is_same_v<T, domain::bond_mapping_result>) {
                        item.trade.product_type = product_type::bond;
                        result.instrument.id = instr_id;
                        result.instrument.trade_id = item.trade.id;
                    } else if constexpr (std::is_same_v<T, domain::credit_mapping_result>) {
                        item.trade.product_type = product_type::credit;
                        result.instrument.id = instr_id;
                        result.instrument.trade_id = item.trade.id;
                    } else if constexpr (std::is_same_v<T, domain::equity_mapping_result>) {
                        item.trade.product_type = product_type::equity;
                        result.instrument.id = instr_id;
                        result.instrument.trade_id = item.trade.id;
                    } else if constexpr (std::is_same_v<T, domain::commodity_mapping_result>) {
                        item.trade.product_type = product_type::commodity;
                        result.instrument.id = instr_id;
                        result.instrument.trade_id = item.trade.id;
                    } else if constexpr (std::is_same_v<T, domain::composite_mapping_result>) {
                        item.trade.product_type = product_type::composite;
                        result.instrument.id = instr_id;
                        result.instrument.trade_id = item.trade.id;
                    } else if constexpr (std::is_same_v<T, domain::scripted_mapping_result>) {
                        item.trade.product_type = product_type::scripted;
                        result.instrument.id = instr_id;
                        result.instrument.trade_id = item.trade.id;
                    }
                }
            }, item.instrument);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to map instrument for trade "
                << std::string(t.id) << ": " << e.what();
        }

        r.push_back(std::move(item));
    }

    BOOST_LOG_SEV(lg(), debug) << "Finished importing " << r.size()
                               << " trades with context.";
    return r;
}

domain::mapped_conventions
importer::import_conventions(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started import: " << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::conventions conv;
    domain::load_data(c, conv);
    const auto r = domain::conventions_mapper::map(conv);

    BOOST_LOG_SEV(lg(), debug) << "Finished importing conventions from "
                               << path.generic_string();
    return r;
}

}
