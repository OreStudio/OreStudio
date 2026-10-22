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
#ifndef ORES_MARKETDATA_CORE_TESTS_TICK_TEST_SUPPORT_HPP
#define ORES_MARKETDATA_CORE_TESTS_TICK_TEST_SUPPORT_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <string>

namespace ores::marketdata::tests {

/// The session of a series' party, which the party policy of the tick table needs.
inline ores::database::context party_context(ores::testing::database_helper& h,
                                             const boost::uuids::uuid& party_id) {
    return h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());
}

/// Appends one tick as the owner of the series. A statement that returns no row is refused
/// by the read helper, so the insert states one.
inline void insert_tick(ores::database::context ctx,
                        const boost::uuids::uuid& party_id,
                        const boost::uuids::uuid& series_id,
                        int coord,
                        std::chrono::system_clock::time_point ts,
                        double value) {
    auto lg(ores::logging::make_logger("ores.marketdata.core.tests"));
    ores::database::repository::execute_parameterized_multi_column_query(
        ctx,
        "INSERT INTO ores_marketdata_ticks_tbl (tenant_id, party_id, series_id, ts, coord, value) "
        "VALUES ($1::uuid, $2::uuid, $3::uuid, $4::timestamptz, $5::smallint, $6::float8) "
        "RETURNING 1",
        {ctx.tenant_id().to_string(),
         boost::uuids::to_string(party_id),
         boost::uuids::to_string(series_id),
         ores::platform::time::datetime::to_iso8601_utc(ts),
         std::to_string(coord),
         std::format("{}", value)},
        lg,
        "inserting a tick");
}

}

#endif
