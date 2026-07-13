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
#include "ores.analytics.quant/domain/rate_status.hpp"
#include "ores.marketdata.api/domain/crm_driver_pair.hpp"
#include "ores.marketdata.api/domain/crm_enabled_derived_pair.hpp"
#include "ores.marketdata.api/domain/crm_topology_config.hpp"
#include "ores.marketdata.core/repository/crm_driver_pair_repository.hpp"
#include "ores.marketdata.core/repository/crm_enabled_derived_pair_repository.hpp"
#include "ores.marketdata.core/repository/crm_topology_config_repository.hpp"
#include "ores.marketdata.service/app/crm_ingest_bridge.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>

namespace {

const std::string tags("[app][crm]");

using ores::marketdata::domain::crm_driver_pair;
using ores::marketdata::domain::crm_enabled_derived_pair;
using ores::marketdata::domain::crm_topology_config;
using ores::marketdata::repository::crm_driver_pair_repository;
using ores::marketdata::repository::crm_enabled_derived_pair_repository;
using ores::marketdata::repository::crm_topology_config_repository;
using ores::marketdata::service::app::crm_ingest_bridge;
using ores::testing::database_helper;

struct fixture {
    database_helper h;
    boost::uuids::uuid party_id = boost::uuids::random_generator{}();
    boost::uuids::uuid config_id = boost::uuids::random_generator{}();

    crm_topology_config make_config(const std::string& pivot = "USD") {
        crm_topology_config c;
        c.version = 0;
        c.tenant_id = h.tenant_id();
        c.id = config_id;
        c.party_id = party_id;
        c.name = "test";
        c.pivot_currency_code = pivot;
        c.enabled = true;
        c.modified_by = "system.test";
        c.performed_by = "system.test";
        c.change_reason_code = "system.test";
        c.change_commentary = "crm_ingest_bridge test fixture";
        return c;
    }

    crm_driver_pair
    make_driver_pair(const std::string& base, const std::string& quote, bool enabled = true) {
        crm_driver_pair p;
        p.version = 0;
        p.tenant_id = h.tenant_id();
        p.id = boost::uuids::random_generator{}();
        p.party_id = party_id;
        p.config_id = config_id;
        p.base_currency_code = base;
        p.quote_currency_code = quote;
        p.enabled = enabled;
        p.modified_by = "system.test";
        p.performed_by = "system.test";
        p.change_reason_code = "system.test";
        p.change_commentary = "crm_ingest_bridge test fixture";
        return p;
    }

    crm_enabled_derived_pair make_enabled_derived_pair(const std::string& base,
                                                       const std::string& quote) {
        crm_enabled_derived_pair p;
        p.version = 0;
        p.tenant_id = h.tenant_id();
        p.id = boost::uuids::random_generator{}();
        p.party_id = party_id;
        p.config_id = config_id;
        p.base_currency_code = base;
        p.quote_currency_code = quote;
        p.enabled = true;
        p.modified_by = "system.test";
        p.performed_by = "system.test";
        p.change_reason_code = "system.test";
        p.change_commentary = "crm_ingest_bridge test fixture";
        return p;
    }
};

} // namespace

TEST_CASE("refresh builds an engine from persisted config and driver pairs, update/rate work",
          tags) {
    fixture f;
    crm_topology_config_repository config_repo;
    crm_driver_pair_repository driver_repo;

    config_repo.write(f.h.context(), f.make_config());
    driver_repo.write(f.h.context(), f.make_driver_pair("EUR", "USD"));
    driver_repo.write(f.h.context(), f.make_driver_pair("USD", "JPY"));

    crm_ingest_bridge bridge(f.h.context());
    bridge.refresh();

    const auto tenant_id_str = f.h.tenant_id().to_string();
    const auto party_id_str = boost::uuids::to_string(f.party_id);

    // Not yet ticked -- unavailable, not "no engine" (nullopt).
    auto before = bridge.rate(tenant_id_str, party_id_str, "EUR", "USD");
    REQUIRE(before.has_value());
    CHECK(before->status == ores::analytics::quant::domain::rate_status::unavailable);

    const auto now = std::chrono::system_clock::now();
    bridge.update(tenant_id_str, party_id_str, "EUR", "USD", 1.10, now);
    bridge.update(tenant_id_str, party_id_str, "USD", "JPY", 150.0, now);

    auto direct = bridge.rate(tenant_id_str, party_id_str, "EUR", "USD");
    REQUIRE(direct.has_value());
    CHECK(direct->status == ores::analytics::quant::domain::rate_status::fresh);
    CHECK(direct->rate == Catch::Approx(1.10));

    auto derived = bridge.rate(tenant_id_str, party_id_str, "EUR", "JPY");
    REQUIRE(derived.has_value());
    CHECK(derived->status == ores::analytics::quant::domain::rate_status::fresh);
    CHECK(derived->rate == Catch::Approx(1.10 * 150.0));
}

TEST_CASE("rate() returns nullopt when no CRM is configured for the (tenant, party)", tags) {
    fixture f;
    crm_ingest_bridge bridge(f.h.context());
    bridge.refresh(); // nothing persisted -- engines map stays empty

    const auto result =
        bridge.rate(f.h.tenant_id().to_string(), boost::uuids::to_string(f.party_id), "EUR", "USD");
    CHECK_FALSE(result.has_value());
}

TEST_CASE("update() silently ignores a pair that is not a driver edge", tags) {
    fixture f;
    crm_topology_config_repository config_repo;
    crm_driver_pair_repository driver_repo;

    config_repo.write(f.h.context(), f.make_config());
    driver_repo.write(f.h.context(), f.make_driver_pair("EUR", "USD"));

    crm_ingest_bridge bridge(f.h.context());
    bridge.refresh();

    const auto tenant_id_str = f.h.tenant_id().to_string();
    const auto party_id_str = boost::uuids::to_string(f.party_id);

    // GBP/USD is not a configured driver pair -- must not throw.
    CHECK_NOTHROW(bridge.update(
        tenant_id_str, party_id_str, "GBP", "USD", 1.25, std::chrono::system_clock::now()));
}

TEST_CASE("rates() returns every configured pair (driver + enabled derived) in one batch", tags) {
    fixture f;
    crm_topology_config_repository config_repo;
    crm_driver_pair_repository driver_repo;
    crm_enabled_derived_pair_repository derived_repo;

    config_repo.write(f.h.context(), f.make_config());
    driver_repo.write(f.h.context(), f.make_driver_pair("EUR", "USD"));
    driver_repo.write(f.h.context(), f.make_driver_pair("USD", "JPY"));
    derived_repo.write(f.h.context(), f.make_enabled_derived_pair("EUR", "JPY"));

    crm_ingest_bridge bridge(f.h.context());
    bridge.refresh();

    const auto tenant_id_str = f.h.tenant_id().to_string();
    const auto party_id_str = boost::uuids::to_string(f.party_id);
    const auto now = std::chrono::system_clock::now();
    bridge.update(tenant_id_str, party_id_str, "EUR", "USD", 1.10, now);
    bridge.update(tenant_id_str, party_id_str, "USD", "JPY", 150.0, now);

    const auto results = bridge.rates(tenant_id_str, party_id_str);

    // Two driver pairs + one enabled derived pair = three configured pairs.
    REQUIRE(results.size() == 3);
    for (const auto& r : results)
        CHECK(r.status == ores::analytics::quant::domain::rate_status::fresh);
}

TEST_CASE("a second enabled config for the same (tenant, party) is ignored, not merged", tags) {
    fixture f;
    crm_topology_config_repository config_repo;
    crm_driver_pair_repository driver_repo;

    auto first = f.make_config();
    config_repo.write(f.h.context(), first);
    driver_repo.write(f.h.context(), f.make_driver_pair("EUR", "USD"));

    auto second = f.make_config();
    second.id = boost::uuids::random_generator{}();
    second.name = "test-2";
    config_repo.write(f.h.context(), second);

    crm_ingest_bridge bridge(f.h.context());
    CHECK_NOTHROW(bridge.refresh()); // must not crash on the ambiguous config set

    const auto result =
        bridge.rate(f.h.tenant_id().to_string(), boost::uuids::to_string(f.party_id), "EUR", "USD");
    REQUIRE(result.has_value()); // exactly one of the two configs won
}
