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
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.trading.api/domain/range_bound.hpp"
#include "ores.trading.core/repository/parent_scoped_queries.hpp"
#include "ores.trading.core/repository/range_bound_repository.hpp"
#include "trade_parent_seed.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.trading.core.repository.range_bound.tests");
const std::string tags("[repository][range_bound]");

using ores::testing::database_helper;
using ores::trading::domain::range_bound;
using ores::trading::repository::range_bound_repository;
using namespace ores::logging;

/*
 * The example accumulator states two entries, so the rows below carry the
 * levels and the digits that document states. They are written out of order to
 * prove the read orders them by the ordinal and not by the write order.
 */
range_bound make_bound(boost::uuids::uuid trade_id, boost::uuids::uuid activity_id, int sequence) {
    range_bound r;
    r.trade_id = trade_id;
    r.trade_activity_id = activity_id;
    r.sequence_number = sequence;
    if (sequence == 1) {
        r.range_to = ores::utility::decimal::decimal::from_string("122.123456789012").value();
        r.leverage = 2.0;
    } else {
        r.range_from = ores::utility::decimal::decimal::from_string("122").value();
        r.leverage = 1.0;
    }
    return r;
}

} // namespace

TEST_CASE("range_bound_reads_back_in_the_document_order", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party_id = boost::uuids::random_generator()();
    auto ctx = h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());
    const auto trade_id = ores::trading::tests::write_parent_trade(h);
    const auto activity_id = ores::trading::tests::write_parent_activity(h);

    // The second entry is written first, so an unordered read would return it
    // first and the assertion below would fail.
    range_bound_repository repo;
    CHECK_NOTHROW(repo.write(ctx, make_bound(trade_id, activity_id, 2)));
    CHECK_NOTHROW(repo.write(ctx, make_bound(trade_id, activity_id, 1)));

    const auto read = repo.read_latest_by_trade_id(ctx, boost::uuids::to_string(trade_id), 0, 10);
    REQUIRE(read.size() == 2);
    CHECK(read[0].sequence_number == 1);
    CHECK(read[1].sequence_number == 2);
    REQUIRE(read[0].range_to.has_value());
    CHECK(read[0].range_to->to_string() == "122.123456789012");
    CHECK(read[0].leverage == 2.0);
    REQUIRE(read[1].range_from.has_value());
    CHECK(read[1].range_from->to_string() == "122");
    CHECK_FALSE(read[1].strike.has_value());

    // The digest reads every trade's rows in one statement, ordered the same
    // way, and so does the export join.
    const auto batch = ores::trading::repository::read_range_bounds_by_trade_ids(
        ctx, {boost::uuids::to_string(trade_id)});
    REQUIRE(batch.size() == 2);
    CHECK(batch[0].sequence_number == 1);
    CHECK(batch[1].sequence_number == 2);

    const auto exported = repo.read_by_instruments_batch(ctx, {boost::uuids::to_string(trade_id)});
    REQUIRE(exported.size() == 2);
    CHECK(exported[0].sequence_number == 1);
    CHECK(exported[1].sequence_number == 2);
    BOOST_LOG_SEV(lg, debug) << "Read range bounds: " << read.size();
}
