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
#include "ores.marketdata.service/app/curve_feed_ingest_loop.hpp"
#include "ores.marketdata.api/domain/asset_class.hpp"
#include "ores.marketdata.api/domain/ir_curve_tick.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <rfl/json.hpp>
#include <stdexcept>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {
constexpr auto* wildcard_subject = "synthetic.v1.curve_family.*";
}

curve_feed_ingest_loop::curve_feed_ingest_loop(ores::nats::service::client& nats,
                                               ores::database::context ctx)
    : nats_(nats)
    , ctx_(std::move(ctx)) {}

void curve_feed_ingest_loop::start() {
    BOOST_LOG_SEV(lg(), info) << "Starting curve feed ingest loop: subscribing to '"
                              << wildcard_subject << "'";

    boost::uuids::random_generator uuid_gen;

    sub_ = nats_.subscribe(
        wildcard_subject, [this, uuid_gen](ores::nats::message msg) mutable {
            auto tick =
                rfl::json::read<domain::ir_curve_tick>(ores::nats::as_string_view(msg.data));
            if (!tick) {
                BOOST_LOG_SEV(lg(), warn) << "Failed to decode ir_curve_tick: " << tick.error().what();
                return;
            }

            try {
                auto tenant_ctx = ctx_.with_tenant(tick->tenant_id, "ores.marketdata.service");

                repository::market_series_repository series_repo;
                auto existing = series_repo.read_latest_by_type(tenant_ctx,
                                                                 tick->series_type,
                                                                 tick->metric,
                                                                 tick->qualifier,
                                                                 boost::uuids::to_string(tick->party_id));
                if (existing.empty()) {
                    BOOST_LOG_SEV(lg(), info)
                        << "Auto-creating market series for " << tick->series_type << "/"
                        << tick->metric << "/" << tick->qualifier;
                    domain::market_series series;
                    series.id = uuid_gen();
                    series.tenant_id = tenant_ctx.tenant_id();
                    series.party_id = tick->party_id;
                    series.series_type = tick->series_type;
                    series.metric = tick->metric;
                    series.qualifier = tick->qualifier;
                    series.asset_class = domain::asset_class::rates;
                    series.series_subclass = tick->subclass;
                    series.is_scalar = false;
                    series.modified_by = ctx_.service_account();
                    series.performed_by = ctx_.service_account();
                    series.change_reason_code = "system.initial_load";
                    series.change_commentary =
                        tick->series_type + "/" + tick->metric + "/" + tick->qualifier +
                        " synthetic curve feed auto-created";
                    series_repo.write(tenant_ctx, series);
                    existing.push_back(std::move(series));
                }

                domain::market_observation obs;
                obs.id = uuid_gen();
                obs.tenant_id = tenant_ctx.tenant_id();
                obs.party_id = tick->party_id;
                obs.series_id = existing.front().id;
                obs.observation_datetime = tick->datetime;
                obs.value = std::to_string(tick->value);
                obs.source = tick->source_name;
                obs.point_id = tick->point_id;

                repository::market_observations_repository obs_repo;
                obs_repo.write(tenant_ctx, obs);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "Failed to persist IR curve observation for "
                                          << tick->series_type << "/" << tick->metric << "/"
                                          << tick->qualifier << " point=" << tick->point_id << ": "
                                          << e.what();
            }
        });
}

} // namespace ores::marketdata::service::app
