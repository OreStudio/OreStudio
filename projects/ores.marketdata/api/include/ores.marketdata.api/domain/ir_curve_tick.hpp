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
#ifndef ORES_MARKETDATA_API_DOMAIN_IR_CURVE_TICK_HPP
#define ORES_MARKETDATA_API_DOMAIN_IR_CURVE_TICK_HPP

#include "ores.marketdata.api/domain/series_subclass.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>

namespace ores::marketdata::domain {

/**
 * @brief One (tenor, rate) point of an IR Curve Template's tick batch, published per-point via
 * NATS on the family subject (synthetic.v1.curve_family.<source>).
 *
 * Unlike fx_spot_tick, which relies on a feed_binding row at the ingest side to supply tenant/
 * party/series identity, an ir_curve_tick is fully self-describing: a curve family has no
 * per-source binding registry (see the tick-batch-publishing task's Analysis section — the
 * family feed is a producer/subject-routing decision, not a per-config toggle), so every field
 * ingest needs to write a market_observation row travels on the wire. One process step yields N
 * of these, one per ir_curve_template_entry, all sharing one datetime — the batch's shared
 * observation_datetime is what lets a future consumer group them into one generation cycle
 * without a synthetic batch/generation identifier (see the task's rejected-batch-id analysis).
 */
struct ir_curve_tick final {
    /**
     * @brief Tenant identifier this observation belongs to.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Owning party (legal entity) this observation belongs to.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Market series type, e.g. "RATES" (first segment of the series' ORE key).
     */
    std::string series_type;

    /**
     * @brief Market series metric, e.g. "YIELD" (second segment of the series' ORE key).
     */
    std::string metric;

    /**
     * @brief Market series qualifier, e.g. "USD/SOFR" (currency/index, third+ segment of the
     * series' ORE key).
     */
    std::string qualifier;

    /**
     * @brief Fine-grained subclass for this point's instrument role (yield for Deposit/Swap,
     * fra for FRA — see ores.refdata.curve_role).
     */
    series_subclass subclass = series_subclass::yield;

    /**
     * @brief Tenor label identifying this point on the curve (references tenor.code, e.g. "3M",
     * "2Y") — becomes market_observation.point_id.
     */
    std::string point_id;

    /**
     * @brief Producer identity (the ir_curve_generation_config's source_name) — becomes
     * market_observation.source.
     */
    std::string source_name;

    /**
     * @brief UTC timestamp shared by every point in this tick batch.
     */
    std::chrono::system_clock::time_point datetime;

    /**
     * @brief The rate this tenor's instrument implies, derived from curve_instrument_pricer.
     */
    double value = 0.0;
};

}

#endif
