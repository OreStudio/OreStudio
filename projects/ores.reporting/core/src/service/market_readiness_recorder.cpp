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
#include "ores.reporting.api/domain/report_trade_market_gap.hpp"
#include "ores.reporting.api/domain/report_trade_market_readiness.hpp"
#include "ores.reporting.api/messaging/report_address_market_readiness_protocol.hpp"
#include "ores.reporting.api/messaging/report_run_market_readiness_protocol.hpp"
#include "ores.reporting.api/messaging/report_trade_market_gap_protocol.hpp"
#include "ores.reporting.api/messaging/report_trade_market_readiness_protocol.hpp"
#include "ores.reporting.core/repository/report_address_market_readiness_repository.hpp"
#include "ores.reporting.core/repository/report_run_market_readiness_repository.hpp"
#include "ores.reporting.core/repository/report_trade_market_gap_repository.hpp"
#include "ores.reporting.core/repository/report_trade_market_readiness_repository.hpp"
#include "ores.reporting.core/service/report_address_market_readiness_service.hpp"
#include "ores.reporting.core/service/report_run_market_readiness_service.hpp"
#include "ores.reporting.core/service/report_trade_market_gap_service.hpp"
#include "ores.reporting.core/service/report_trade_market_readiness_service.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
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

/*
 * Retires every row the run already holds at one grain.
 *
 * A workflow re-dispatches a step that failed after it committed its work, so
 * the record converges rather than accumulates: the run's rows are closed
 * before the new ones are written, and a retry leaves one record.
 */
template <typename Repository, typename Filter>
void retire(const ores::database::context& ctx,
            Repository& repo,
            std::uint32_t total,
            const Filter& filter) {
    if (total == 0)
        return;
    std::vector<std::string> ids;
    ids.reserve(total);
    for (const auto& row : repo.read_latest(ctx, 0, total, {}, filter))
        ids.push_back(boost::uuids::to_string(row.id));
    if (!ids.empty())
        repo.remove(ctx, ids);
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

        repository::report_address_market_readiness_repository address_repo;
        repository::report_trade_market_readiness_repository trade_repo;
        repository::report_trade_market_gap_repository gap_repo;
        repository::report_run_market_readiness_repository run_repo;

        const messaging::report_address_market_readinesses_filter address_filter{
            .report_instance_id = run.report_instance_id};
        const messaging::report_trade_market_readinesses_filter trade_filter{
            .report_instance_id = run.report_instance_id};
        const messaging::report_trade_market_gaps_filter gap_filter{.report_instance_id =
                                                                        run.report_instance_id};
        const messaging::report_run_market_readinesses_filter run_filter{
            .report_instance_id = run.report_instance_id};

        retire(ctx_, gap_repo, gap_repo.get_total_trade_gap_count(ctx_, gap_filter), gap_filter);
        retire(ctx_,
               address_repo,
               address_repo.get_total_address_readiness_count(ctx_, address_filter),
               address_filter);
        retire(ctx_,
               trade_repo,
               trade_repo.get_total_trade_readiness_count(ctx_, trade_filter),
               trade_filter);
        retire(
            ctx_, run_repo, run_repo.get_total_run_readiness_count(ctx_, run_filter), run_filter);

        report_address_market_readiness_service address_service{ctx_};
        report_trade_market_readiness_service trade_service{ctx_};
        report_trade_market_gap_service gap_service{ctx_};
        report_run_market_readiness_service run_service{ctx_};

        std::vector<domain::report_address_market_readiness> addresses;
        addresses.reserve(readiness.addresses.size());
        for (const auto& one : readiness.addresses) {
            auto row = run_of(domain::report_address_market_readiness{
                .id = new_id(), .oresmd_uri = one.oresmd_uri, .gap_cause = one.gap_cause});
            row.series_id = as_uuid(one.series_id);
            addresses.push_back(std::move(row));
        }
        if (!addresses.empty())
            address_service.save_address_readinesses(addresses);

        std::vector<domain::report_trade_market_readiness> trades;
        trades.reserve(readiness.trades.size());
        for (const auto& one : readiness.trades)
            trades.push_back(run_of(domain::report_trade_market_readiness{
                .id = new_id(),
                .trade_id = one.trade_id,
                .complete = one.complete,
                .gap_count = static_cast<int>(one.gap_count)}));
        if (!trades.empty())
            trade_service.save_trade_readinesses(trades);

        std::vector<domain::report_trade_market_gap> gaps;
        gaps.reserve(readiness.gaps.size());
        for (const auto& one : readiness.gaps)
            gaps.push_back(run_of(domain::report_trade_market_gap{
                .id = new_id(), .trade_id = one.trade_id, .oresmd_uri = one.oresmd_uri}));
        if (!gaps.empty())
            gap_service.save_trade_gaps(gaps);

        run_service.save_run_readiness(run_of(domain::report_run_market_readiness{
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
