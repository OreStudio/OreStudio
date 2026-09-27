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
#include "ores.reporting.core/repository/report_input_bundle_entity.hpp"
#include "ores.reporting.core/repository/report_input_bundle_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[repository]");

std::string new_uuid() {
    return boost::uuids::to_string(boost::uuids::random_generator()());
}

ores::reporting::repository::report_input_bundle_entity
make_bundle(const std::string& tenant_id,
            const std::string& instance_id,
            const std::string& definition_id) {
    ores::reporting::repository::report_input_bundle_entity bundle;
    bundle.id = new_uuid();
    bundle.tenant_id = tenant_id;
    bundle.report_instance_id = instance_id;
    bundle.definition_id = definition_id;
    bundle.trades_storage_key = instance_id + "/trades.msgpack";
    bundle.market_data_storage_key = instance_id + "/market_data.msgpack";
    bundle.trade_count = 3;
    bundle.series_count = 5;
    bundle.created_at =
        ores::platform::time::datetime::to_db_string(std::chrono::system_clock::now());
    return bundle;
}

}

using namespace ores::logging;
using ores::testing::database_helper;
using ores::reporting::repository::report_input_bundle_repository;

TEST_CASE("create_and_find_report_input_bundle_by_instance_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto tenant_id = h.tenant_id().to_string();
    const auto instance_id = new_uuid();
    const auto definition_id = new_uuid();
    const auto bundle = make_bundle(tenant_id, instance_id, definition_id);

    report_input_bundle_repository repo;
    repo.create(h.context(), bundle);

    const auto found = repo.find_by_instance_id(h.context(), instance_id);

    REQUIRE(found.has_value());
    CHECK(found->id.value() == bundle.id.value());
    CHECK(found->tenant_id == tenant_id);
    CHECK(found->report_instance_id == instance_id);
    CHECK(found->definition_id == definition_id);
    CHECK(found->trades_storage_key == instance_id + "/trades.msgpack");
    CHECK(found->market_data_storage_key == instance_id + "/market_data.msgpack");
    CHECK(found->trade_count == 3);
    CHECK(found->series_count == 5);
}

TEST_CASE("find_report_input_bundle_by_unknown_instance_id_returns_nothing", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    report_input_bundle_repository repo;
    const auto found = repo.find_by_instance_id(h.context(), new_uuid());

    CHECK_FALSE(found.has_value());
}
