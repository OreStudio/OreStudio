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
#include "ores.reporting.core/service/nats_cut_body_reader.hpp"
#include "ores.compute.api/messaging/app_version_protocol.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.core/repository/configuration_repository.hpp"
#include "ores.reporting.core/repository/engine_settings_repository.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <format>
#include <optional>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>

namespace ores::reporting::service {

using namespace ores::logging;
using ores::platform::time::datetime;

namespace {

/**
 * @brief Calls one operation with the caller's own session and answers its reply.
 *
 * A refusal at the transport or the protocol is thrown, so the caller records
 * the part as a gap with the sentence rather than reading a refused reply as an
 * empty store.
 */
template <typename Req>
std::optional<typename Req::response_type> call_as_caller(ores::nats::service::client& nats,
                                                          const std::string& authorization,
                                                          const Req& request,
                                                          std::string& error) {
    using Resp = typename Req::response_type;
    if (authorization.empty()) {
        error = "The take carried no session, so the part could not be read.";
        return std::nullopt;
    }
    try {
        const auto& codec = ores::nats::default_wire_codec();
        constexpr auto subject = Req::nats_subject;
        auto reply = nats.request_sync(
            subject,
            codec.encode(request),
            std::unordered_map<std::string, std::string>{
                {std::string(ores::nats::headers::authorization), authorization}});
        const auto err = reply.headers.find("X-Error");
        if (err != reply.headers.end()) {
            error = std::format("{} answered {}", subject, err->second);
            return std::nullopt;
        }
        auto decoded = codec.decode<Resp>(reply.data);
        if (!decoded) {
            error = std::format("{} answered a reply this service cannot read", subject);
            return std::nullopt;
        }
        return *decoded;
    } catch (const std::exception& e) {
        error = std::format("{} could not be reached: {}", Req::nats_subject, e.what());
        return std::nullopt;
    }
}

/**
 * @brief The version of the reporting configuration in force, in one line.
 *
 * The reference data the take can answer is this component's own: the report
 * configurations and the engine run settings in force. Each is stated by how
 * many of it stands and by the newest version among them, so the line is a
 * digest a reader can reproduce rather than a number nobody can explain.
 */
std::optional<std::string> reference_data_in_force(const ores::database::context& ctx,
                                                   std::string& refusal) {
    const auto configurations = repository::configuration_repository{}.read_latest(ctx);
    const auto settings = repository::engine_settings_repository{}.read_latest(ctx);
    if (configurations.empty() && settings.empty()) {
        refusal = "Nothing names the reporting configuration in force, so the body would record no "
                  "settings for the run to read.";
        return std::nullopt;
    }

    const auto newest = [](const auto& rows) {
        int highest = 0;
        for (const auto& row : rows)
            highest = std::max(highest, row.version);
        return highest;
    };
    return std::format("{} report configuration(s) in force, newest version {} · {} engine run "
                       "settings in force, newest version {}",
                       configurations.size(),
                       newest(configurations),
                       settings.size(),
                       newest(settings));
}

}

nats_cut_body_reader::nats_cut_body_reader(ores::database::context ctx,
                                           ores::nats::service::client& nats,
                                           std::string authorization)
    : ctx_(std::move(ctx))
    , nats_(nats)
    , authorization_(std::move(authorization)) {}

cut_body_facts nats_cut_body_reader::read(const cut_body_query& query) {
    cut_body_facts facts;

    // The deal population: the versioned trades whose interval holds the
    // instant, counted by the read that serves the list. The count is the size
    // of the whole filtered set, so one row is asked for and the total read.
    {
        ores::trading::messaging::list_trades_request request;
        request.offset = 0;
        request.limit = 1;
        request.as_of = datetime::to_iso8601_utc(query.timepoint);
        std::string error;
        const auto reply = call_as_caller(nats_, authorization_, request, error);
        if (!reply) {
            facts.deals_refusal = error;
        } else if (reply->result.outcome != ores::utility::domain::outcome::ok) {
            facts.deals_refusal = reply->result.message.empty() ?
                                      std::string("The deal population read refused the "
                                                  "instant.") :
                                      reply->result.message;
        } else {
            facts.deal_count = static_cast<int>(reply->total);
        }
        BOOST_LOG_SEV(lg(), debug) << "The deals part answered " << reply.has_value();
    }

    // The market data: the published sets in use at the instant, and the
    // candidates that stand beside them. A candidate nobody approved is the
    // gap the journey names, so both counts are read.
    {
        ores::marketdata::messaging::list_published_curves_request in_use;
        in_use.party_id = boost::uuids::to_string(query.party_id);
        in_use.include_candidates = false;
        in_use.include_ended = false;
        in_use.offset = 0;
        in_use.limit = 50;
        std::string error;
        const auto reply = call_as_caller(nats_, authorization_, in_use, error);
        if (!reply) {
            facts.market_data_refusal = error;
        } else if (!reply->success) {
            facts.market_data_refusal = reply->message.empty() ?
                                            std::string("The published sets read refused the "
                                                        "party.") :
                                            reply->message;
        } else {
            facts.market_data_set_count = static_cast<int>(reply->total);
            for (const auto& set : reply->sets) {
                const auto at = datetime::to_iso8601_utc(set.as_of);
                if (facts.market_data_as_of.empty() || at > facts.market_data_as_of)
                    facts.market_data_as_of = at;
            }

            ores::marketdata::messaging::list_published_curves_request candidates = in_use;
            candidates.include_candidates = true;
            candidates.limit = 200;
            std::string candidate_error;
            const auto with_candidates =
                call_as_caller(nats_, authorization_, candidates, candidate_error);
            if (with_candidates && with_candidates->success) {
                facts.market_data_candidate_count = std::max(
                    0, static_cast<int>(with_candidates->total) - static_cast<int>(reply->total));
            }
        }
    }

    // The reference data and the settings: the reporting component's own.
    facts.reference_data_version = reference_data_in_force(ctx_, facts.reference_data_refusal);

    // The engine and the wrapper the work will run.
    {
        ores::compute::messaging::list_app_versions_request request;
        request.offset = 0;
        request.limit = 200;
        std::string error;
        const auto reply = call_as_caller(nats_, authorization_, request, error);
        if (!reply) {
            facts.engine_refusal = error;
        } else if (reply->result.outcome != ores::utility::domain::outcome::ok) {
            facts.engine_refusal = reply->result.message.empty() ?
                                       std::string("The engine version read refused the call.") :
                                       reply->result.message;
        } else {
            const ores::compute::domain::app_version* newest = nullptr;
            for (const auto& candidate : reply->app_versions) {
                if (candidate.engine_version.empty())
                    continue;
                if (newest == nullptr || candidate.version > newest->version)
                    newest = &candidate;
            }
            if (newest != nullptr) {
                facts.engine_version = newest->engine_version;
                facts.wrapper_version = newest->wrapper_version;
            } else {
                facts.engine_refusal =
                    "No engine version recorded: nothing in the store names the version that "
                    "would compute the result.";
            }
        }
    }

    return facts;
}

}
