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
#include "fixing_inputs.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/fixing_read_operations_protocol.hpp"
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.refdata.core/repository/currency_pair_repository.hpp"
#include "ores.refdata.core/repository/cut_code_repository.hpp"
#include "ores.refdata.core/repository/fixing_source_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <memory>
#include <mutex>
#include <optional>
#include <string_view>

namespace ores::synthetic::service {

namespace {

auto& fixing_inputs_lg() {
    static auto instance = ores::logging::make_logger("ores.synthetic.service.fixing_inputs");
    return instance;
}

/// The outcome code the market data service refuses a series that holds nothing with.
constexpr std::string_view not_found_code = "not_found";

/// How wide a window the read of the days already held asks for.
constexpr int held_window_days = 800;

bool is_not_found(const std::string& error) {
    return error == not_found_code ||
           (error.starts_with(not_found_code) && error.size() > not_found_code.size() &&
            error[not_found_code.size()] == ':');
}

std::map<std::pair<std::string, std::string>, fx_fixing_schedule>
read_fx_schedules(const ores::database::context& db_ctx) {
    std::map<std::string, fx_fixing_schedule> by_source;
    for (const auto& source :
         ores::refdata::repository::fixing_source_repository{}.read_latest(db_ctx))
        by_source.emplace(source.code,
                          fx_fixing_schedule{.source = source.code,
                                             .when = {.at = source.publication_time,
                                                      .zone = source.publication_time_zone}});
    std::map<std::string, std::string> cut_sources;
    for (const auto& cut : ores::refdata::repository::cut_code_repository{}.read_latest(db_ctx))
        cut_sources.emplace(cut.code, cut.fixing_source_code);

    std::map<std::pair<std::string, std::string>, fx_fixing_schedule> out;
    for (const auto& pair :
         ores::refdata::repository::currency_pair_repository{}.read_latest(db_ctx)) {
        if (!pair.default_cut_code)
            continue;
        const auto cut = cut_sources.find(*pair.default_cut_code);
        if (cut == cut_sources.end())
            continue;
        const auto source = by_source.find(cut->second);
        if (source == by_source.end())
            continue;
        out.emplace(std::pair{pair.base_currency, pair.quote_currency}, source->second);
    }
    return out;
}

}

fixing_offer_inputs read_fixing_offer_inputs(const ores::database::context& db_ctx,
                                             ores::nats::service::nats_client& svc_nats) {
    fixing_offer_inputs out;
    try {
        out.fx = read_fx_schedules(db_ctx);
    } catch (const std::exception& e) {
        ORES_LOG_SEV(fixing_inputs_lg(), ores::logging::warn)
            << "The FX fixing schedules are not read, so no FX fixing is offered: " << e.what();
    }

    struct system_party_cache final {
        std::mutex mu;
        std::optional<boost::uuids::uuid> id;
    };
    auto party = std::make_shared<system_party_cache>();
    out.held = [db_ctx, &svc_nats, party](const std::string& item, std::chrono::year_month_day to)
        -> std::expected<std::set<std::chrono::year_month_day>, std::string> {
        boost::uuids::uuid party_id;
        {
            std::lock_guard lock(party->mu);
            if (!party->id) {
                const auto system = ores::refdata::repository::party_repository{}.read_system_party(
                    db_ctx, db_ctx.tenant_id().to_string());
                if (system.empty())
                    return std::unexpected("The tenant has no system party.");
                party->id = system.front().id;
            }
            party_id = *party->id;
        }
        // Reads go delegated, as every read of the catalogue does, because a service's own
        // session holds no party.
        auto delegated = svc_nats.with_delegation({});
        ores::marketdata::client::market_data_client md(delegated);
        ores::marketdata::messaging::get_fixing_window_request request;
        request.item = item;
        request.party_id = boost::uuids::to_string(party_id);
        request.to = to;
        request.days = held_window_days;
        const auto window = md.get_fixing_window(request);
        if (!window) {
            // A series the market holds nothing for is refused as not_found, which means no day
            // is held. Any other failure says nothing about the days, so the history waits.
            if (is_not_found(window.error()))
                return std::set<std::chrono::year_month_day>{};
            return std::unexpected(window.error());
        }
        return std::set<std::chrono::year_month_day>(window->dates.begin(), window->dates.end());
    };
    return out;
}

}
