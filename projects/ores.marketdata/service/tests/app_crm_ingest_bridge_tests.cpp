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
#include "ores.marketdata.service/app/crm_ingest_bridge.hpp"
#include "ores.refdata.api/domain/crm_driver_pair.hpp"
#include "ores.refdata.api/domain/crm_enabled_derived_pair.hpp"
#include "ores.refdata.api/domain/crm_topology_config.hpp"
#include "ores.refdata.core/repository/crm_driver_pair_repository.hpp"
#include "ores.refdata.core/repository/crm_enabled_derived_pair_repository.hpp"
#include "ores.refdata.core/repository/crm_topology_config_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>

namespace {

const std::string tags("[app][crm]");

using ores::refdata::domain::crm_driver_pair;
using ores::refdata::domain::crm_enabled_derived_pair;
using ores::refdata::domain::crm_topology_config;
using ores::refdata::repository::crm_driver_pair_repository;
using ores::refdata::repository::crm_enabled_derived_pair_repository;
using ores::refdata::repository::crm_topology_config_repository;
using ores::marketdata::service::app::crm_ingest_bridge;
using ores::testing::database_helper;

struct fixture {
    database_helper h;
    boost::uuids::uuid party_id = boost::uuids::random_generator{}();
    boost::uuids::uuid config_id = boost::uuids::random_generator{}();

    crm_topology_config make_config(const std::string& pivot = "USD",
                                    const std::string& name = "test") {
        crm_topology_config c;
        c.version = 0;
        c.tenant_id = h.tenant_id();
        c.id = config_id;
        c.party_id = party_id;
        c.name = name;
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
    auto before = bridge.rate(tenant_id_str, party_id_str, "test", "EUR", "USD");
    REQUIRE(before.has_value());
    CHECK(before->status == ores::analytics::quant::domain::rate_status::unavailable);

    const auto now = std::chrono::system_clock::now();
    bridge.update(tenant_id_str, party_id_str, "EUR", "USD", 1.10, now);
    bridge.update(tenant_id_str, party_id_str, "USD", "JPY", 150.0, now);

    auto direct = bridge.rate(tenant_id_str, party_id_str, "test", "EUR", "USD");
    REQUIRE(direct.has_value());
    CHECK(direct->status == ores::analytics::quant::domain::rate_status::fresh);
    CHECK(direct->rate == Catch::Approx(1.10));

    auto derived = bridge.rate(tenant_id_str, party_id_str, "test", "EUR", "JPY");
    REQUIRE(derived.has_value());
    CHECK(derived->status == ores::analytics::quant::domain::rate_status::fresh);
    CHECK(derived->rate == Catch::Approx(1.10 * 150.0));
}

TEST_CASE("rate() returns nullopt when no CRM is configured for the (tenant, party)", tags) {
    fixture f;
    crm_ingest_bridge bridge(f.h.context());
    bridge.refresh(); // nothing persisted -- engines map stays empty

    const auto result = bridge.rate(
        f.h.tenant_id().to_string(), boost::uuids::to_string(f.party_id), "test", "EUR", "USD");
    CHECK_FALSE(result.has_value());
}

TEST_CASE("rate() returns nullopt for an unknown CRM name when others are configured", tags) {
    fixture f;
    crm_topology_config_repository config_repo;
    crm_driver_pair_repository driver_repo;

    config_repo.write(f.h.context(), f.make_config("USD", "majors"));
    driver_repo.write(f.h.context(), f.make_driver_pair("EUR", "USD"));

    crm_ingest_bridge bridge(f.h.context());
    bridge.refresh();

    const auto result = bridge.rate(
        f.h.tenant_id().to_string(), boost::uuids::to_string(f.party_id), "exotics", "EUR", "USD");
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

    const auto results = bridge.rates(tenant_id_str, party_id_str, "test");

    // Two driver pairs + one enabled derived pair = three configured pairs.
    REQUIRE(results.size() == 3);
    for (const auto& r : results)
        CHECK(r.status == ores::analytics::quant::domain::rate_status::fresh);
}

TEST_CASE("two enabled configs for the same (tenant, party) build two independent named engines",
          tags) {
    fixture f;
    crm_topology_config_repository config_repo;
    crm_driver_pair_repository driver_repo;

    auto majors = f.make_config("USD", "majors");
    config_repo.write(f.h.context(), majors);
    driver_repo.write(f.h.context(), f.make_driver_pair("EUR", "USD"));
    driver_repo.write(f.h.context(), f.make_driver_pair("USD", "JPY"));

    auto exotics = f.make_config("USD", "exotics");
    exotics.id = boost::uuids::random_generator{}();
    config_repo.write(f.h.context(), exotics);
    // exotics driver pairs must reference the exotics config_id.
    auto try_usd = f.make_driver_pair("TRY", "USD");
    try_usd.config_id = exotics.id;
    driver_repo.write(f.h.context(), try_usd);

    crm_ingest_bridge bridge(f.h.context());
    CHECK_NOTHROW(bridge.refresh());

    const auto tenant_id_str = f.h.tenant_id().to_string();
    const auto party_id_str = boost::uuids::to_string(f.party_id);
    const auto now = std::chrono::system_clock::now();

    // A tick on a pair shared by both CRMs feeds both -- but here EUR/USD
    // is only a driver edge of majors, TRY/USD only of exotics.
    bridge.update(tenant_id_str, party_id_str, "EUR", "USD", 1.10, now);
    bridge.update(tenant_id_str, party_id_str, "TRY", "USD", 0.030, now);

    auto majors_rate = bridge.rate(tenant_id_str, party_id_str, "majors", "EUR", "USD");
    REQUIRE(majors_rate.has_value());
    CHECK(majors_rate->status == ores::analytics::quant::domain::rate_status::fresh);

    auto exotics_rate = bridge.rate(tenant_id_str, party_id_str, "exotics", "TRY", "USD");
    REQUIRE(exotics_rate.has_value());
    CHECK(exotics_rate->status == ores::analytics::quant::domain::rate_status::fresh);

    // All-CRMs batch: majors has 2 configured driver pairs (EUR/USD,
    // USD/JPY), exotics has 1 (TRY/USD) -- 3 total, each tagged by name.
    const auto all = bridge.rates(tenant_id_str, party_id_str);
    REQUIRE(all.size() == 3);
    std::size_t majors_count = 0;
    std::size_t exotics_count = 0;
    for (const auto& r : all) {
        if (r.crm_name == "majors")
            ++majors_count;
        else if (r.crm_name == "exotics")
            ++exotics_count;
    }
    CHECK(majors_count == 2);
    CHECK(exotics_count == 1);
}

TEST_CASE("resolved_rates() synthesises a reverse-pair inverse when the reverse isn't configured",
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
    const auto now = std::chrono::system_clock::now();
    bridge.update(tenant_id_str, party_id_str, "EUR", "USD", 1.10, now);
    bridge.update(tenant_id_str, party_id_str, "USD", "JPY", 150.0, now);

    const auto results = bridge.resolved_rates(tenant_id_str, party_id_str, "test", true);

    // Two configured driver pairs (EUR/USD, USD/JPY); inverted=true adds
    // a synthesised reverse for each, since neither EUR/USD's reverse
    // (USD/EUR) nor USD/JPY's reverse (JPY/USD) is itself a configured
    // pair here: 2 direct + 2 synthesised = 4.
    REQUIRE(results.size() == 4);
    std::size_t inverted_count = 0;
    for (const auto& r : results)
        if (r.inverted)
            ++inverted_count;
    CHECK(inverted_count == 2);
}

TEST_CASE("resolved_rates() synthesises no inverse when the reverse pair is itself configured",
          tags) {
    fixture f;
    crm_topology_config_repository config_repo;
    crm_driver_pair_repository driver_repo;
    crm_enabled_derived_pair_repository derived_repo;

    config_repo.write(f.h.context(), f.make_config());
    driver_repo.write(f.h.context(), f.make_driver_pair("EUR", "USD"));
    // EUR/USD's reverse, USD/EUR, is also explicitly a configured
    // (derived) pair -- resolved_rates() must serve its own real rate
    // for USD/EUR, not a synthesised 1/rate inverse of EUR/USD.
    derived_repo.write(f.h.context(), f.make_enabled_derived_pair("USD", "EUR"));

    crm_ingest_bridge bridge(f.h.context());
    bridge.refresh();

    const auto tenant_id_str = f.h.tenant_id().to_string();
    const auto party_id_str = boost::uuids::to_string(f.party_id);
    bridge.update(
        tenant_id_str, party_id_str, "EUR", "USD", 1.10, std::chrono::system_clock::now());

    const auto results = bridge.resolved_rates(tenant_id_str, party_id_str, "test", true);

    // Both directions are already configured pairs -- no synthesis.
    REQUIRE(results.size() == 2);
    for (const auto& r : results)
        CHECK_FALSE(r.inverted);

    const auto usd_eur = std::ranges::find_if(
        results, [](const auto& r) { return r.base_code == "USD" && r.quote_code == "EUR"; });
    REQUIRE(usd_eur != results.end());
    CHECK(usd_eur->rate == Catch::Approx(1.0 / 1.10));
}

TEST_CASE("refresh() resets the per-named-engine delta baseline", tags) {
    fixture f;
    crm_topology_config_repository config_repo;
    crm_driver_pair_repository driver_repo;

    config_repo.write(f.h.context(), f.make_config());
    driver_repo.write(f.h.context(), f.make_driver_pair("EUR", "USD"));

    crm_ingest_bridge bridge(f.h.context());
    bridge.refresh();

    const auto tenant_id_str = f.h.tenant_id().to_string();
    const auto party_id_str = boost::uuids::to_string(f.party_id);

    bridge.update(
        tenant_id_str, party_id_str, "EUR", "USD", 1.10, std::chrono::system_clock::now());
    const auto first = bridge.resolved_rates(tenant_id_str, party_id_str, "test", false);
    REQUIRE(first.size() == 1);
    CHECK_FALSE(first[0].delta_pct.has_value()); // first observation

    bridge.update(
        tenant_id_str, party_id_str, "EUR", "USD", 1.20, std::chrono::system_clock::now());
    const auto second = bridge.resolved_rates(tenant_id_str, party_id_str, "test", false);
    REQUIRE(second.size() == 1);
    REQUIRE(second[0].delta_pct.has_value()); // baseline established by `first`

    // refresh() rebuilds every named engine (and its delta_tracker) from
    // scratch -- even with the exact same config/driver pairs still
    // persisted, the previously-established baseline must be gone, so
    // the next observation looks like a first observation again.
    bridge.refresh();
    bridge.update(
        tenant_id_str, party_id_str, "EUR", "USD", 1.25, std::chrono::system_clock::now());
    const auto after_refresh = bridge.resolved_rates(tenant_id_str, party_id_str, "test", false);
    REQUIRE(after_refresh.size() == 1);
    CHECK_FALSE(after_refresh[0].delta_pct.has_value());
}

TEST_CASE("the DB rejects a second active crm_topology_config sharing a party's CRM name", tags) {
    // crm_ingest_bridge::refresh() relies on this: it would throw
    // std::logic_error if it ever saw two enabled, same-named configs for
    // one party, but that can only happen if this DB invariant is somehow
    // violated -- assert the invariant directly, at the layer that
    // actually enforces it (crm_topology_configs_party_id_name_uniq_idx,
    // a partial unique index on (tenant_id, party_id, name) for active
    // rows), rather than trying to fabricate the impossible state to
    // exercise refresh()'s defensive throw.
    fixture f;
    crm_topology_config_repository config_repo;

    auto first = f.make_config("USD", "majors");
    config_repo.write(f.h.context(), first);

    auto second = f.make_config("USD", "majors"); // same party, same name
    second.id = boost::uuids::random_generator{}();
    CHECK_THROWS(config_repo.write(f.h.context(), second));
}
