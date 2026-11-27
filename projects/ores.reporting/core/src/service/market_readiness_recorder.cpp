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
#include "ores.reporting.core/service/market_readiness_recorder.hpp"
#include "ores.reporting.api/domain/report_address_market_readiness.hpp"
#include "ores.reporting.api/domain/report_run_market_readiness.hpp"
#include "ores.reporting.api/domain/report_trade_market_readiness.hpp"
#include "ores.reporting.core/service/report_address_market_readiness_service.hpp"
#include "ores.reporting.core/service/report_run_market_readiness_service.hpp"
#include "ores.reporting.core/service/report_trade_market_readiness_service.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <vector>

namespace ores::reporting::service {

namespace {

std::optional<boost::uuids::uuid> as_uuid(const std::optional<std::string>& value) {
    if (!value || value->empty())
        return std::nullopt;
    try {
        return boost::uuids::string_generator{}(*value);
    } catch (const std::exception&) {
        // An address answered by a series the caller cannot name is still answered; the row
        // keeps the cause and loses the series rather than failing the run.
        return std::nullopt;
    }
}

}

market_readiness_recorder::market_readiness_recorder(const ores::database::context& ctx)
    : ctx_(ctx) {}

std::string market_readiness_recorder::record(const readiness_subject& run,
                                              const market_readiness& readiness) {
    try {
        boost::uuids::random_generator new_id;
        const auto run_of = [&run](auto row) {
            row.party_id = run.party_id;
            row.report_instance_id = run.report_instance_id;
            return row;
        };

        std::vector<domain::report_address_market_readiness> addresses;
        addresses.reserve(readiness.addresses.size());
        for (const auto& one : readiness.addresses) {
            auto row = run_of(domain::report_address_market_readiness{
                .id = new_id(), .oresmd_uri = one.oresmd_uri, .gap_cause = one.gap_cause});
            row.series_id = as_uuid(one.series_id);
            addresses.push_back(std::move(row));
        }
        if (!addresses.empty())
            report_address_market_readiness_service{ctx_}.save_address_readinesses(addresses);

        std::vector<domain::report_trade_market_readiness> trades;
        trades.reserve(readiness.trades.size());
        for (const auto& one : readiness.trades)
            trades.push_back(run_of(domain::report_trade_market_readiness{
                .id = new_id(),
                .trade_id = one.trade_id,
                .complete = one.complete,
                .gap_count = static_cast<int>(one.gap_count)}));
        if (!trades.empty())
            report_trade_market_readiness_service{ctx_}.save_trade_readinesses(trades);

        report_run_market_readiness_service{ctx_}.save_run_readiness(
            run_of(domain::report_run_market_readiness{
                .id = new_id(),
                .as_of = run.as_of,
                .booked_count = static_cast<int>(readiness.booked),
                .complete_count = static_cast<int>(readiness.complete),
                .excluded_count = static_cast<int>(readiness.excluded)}));
    } catch (const std::exception& e) {
        return e.what();
    }
    return {};
}

}
