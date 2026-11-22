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
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.trading.api/generators/scripted_instrument_generator.hpp"
#include "ores.trading.api/generators/trade_activity_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.core/repository/script_input_repository.hpp"
#include "ores.trading.core/repository/script_repository.hpp"
#include "ores.trading.core/repository/script_variant_repository.hpp"
#include "ores.trading.core/repository/scripted_instrument_repository.hpp"
#include "ores.trading.core/repository/trade_activity_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.core/service/script_publish_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <random>
#include <string>
#include <vector>

using namespace ores::trading;

namespace {

/// A context that writes as a new party of the test tenant, as booking does.
ores::database::context party_context(ores::testing::scoped_database_helper& h,
                                      ores::utility::generation::generation_context& gen) {
    ores::refdata::repository::party_repository parties;
    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& e : parties.read_latest(h.context()))
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    parties.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

/// A payoff whose text differs on each call, so that each publish is a change.
std::string fresh_code() {
    static std::mt19937 rng{std::random_device{}()};
    return "Option = PAY(Notional * " +
           std::to_string(std::uniform_int_distribution<int>(1, 1000000)(rng)) +
           ", Expiry, Expiry, PayCcy);";
}

messaging::publish_script_request
request_of(const std::string& name, std::vector<std::string> purposes, const std::string& code) {
    messaging::publish_script_request r;
    r.name = name;
    r.description = "A tenant's payoff";
    for (auto& purpose : purposes) {
        messaging::published_variant v;
        v.purpose = std::move(purpose);
        v.code = code;
        v.npv_variable = "Option";
        v.results.push_back({.variable = "Notional", .rename = std::nullopt});
        r.variants.push_back(std::move(v));
    }
    return r;
}

/// The tenant's own current rows of a table, for one script.
template <typename Repository>
auto own_rows(Repository repo, ores::testing::scoped_database_helper& h, const std::string& name) {
    auto rows = repo.read_latest(h.context());
    std::erase_if(rows, [&](const auto& r) {
        return !(r.tenant_id == h.tenant_id()) || r.script_name != name;
    });
    return rows;
}

}

TEST_CASE("a tenant's own script is the one the tenant's trades book on",
          "[service][script_library][publish]") {
    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto party_ctx = party_context(h, gen);

    const auto published = service::script_publish_service(h.context())
                               .publish(request_of("Accumulator01", {"default"}, fresh_code()));

    REQUIRE(published.success);
    CHECK(published.diagnostics.empty());
    REQUIRE(published.warnings.size() == 1u);
    CHECK_THAT(published.warnings[0], Catch::Matchers::ContainsSubstring("world library"));

    std::vector<std::string> inputs;
    for (const auto& i : own_rows(repository::script_input_repository(), h, "Accumulator01")) {
        CHECK(i.script_version == published.version);
        inputs.push_back(i.input_name + ":" + i.data_type);
    }
    std::ranges::sort(inputs);
    CHECK(inputs == std::vector<std::string>{"Expiry:Event", "Notional:Number", "PayCcy:Currency"});

    auto trade = generators::generate_synthetic_trade(gen);
    trade.party_id = *party_ctx.party_id();
    repository::trade_repository().write(party_ctx, trade);
    auto activity = generators::generate_synthetic_trade_activity(gen);
    activity.party_id = *party_ctx.party_id();
    repository::trade_activity_repository().write(party_ctx, activity);

    auto instrument = generators::generate_synthetic_scripted_instrument(gen);
    instrument.audit.change_reason_code = "system.test";
    instrument.identity.party_id = *party_ctx.party_id();
    instrument.identity.trade_id = trade.id;
    instrument.identity.trade_activity_id = activity.id;
    instrument.script_name = "Accumulator01";
    instrument.script_body.clear();
    instrument.script_tenant_id.reset();
    instrument.script_version.reset();
    repository::scripted_instrument_repository().write(party_ctx, instrument);

    const auto booked = repository::scripted_instrument_repository().read_latest(
        party_ctx, boost::uuids::to_string(trade.id));
    REQUIRE(booked.size() == 1u);
    REQUIRE(booked[0].script_tenant_id.has_value());
    CHECK(*booked[0].script_tenant_id == h.tenant_id().to_uuid());
    CHECK(booked[0].script_version == published.version);
}

TEST_CASE("a script equal to its version writes none, and a changed one closes what it drops",
          "[service][script_library][publish]") {
    ores::testing::scoped_database_helper h;
    const auto name = "TenantPayoff_" + boost::uuids::to_string(boost::uuids::random_generator()());
    service::script_publish_service publisher(h.context());

    const auto code = fresh_code();
    const auto first = publisher.publish(request_of(name, {"default", "AMC"}, code));
    REQUIRE(first.success);
    CHECK(first.version == 1);
    CHECK(first.warnings.empty());
    CHECK(own_rows(repository::script_variant_repository(), h, name).size() == 2u);

    const auto again = publisher.publish(request_of(name, {"default", "AMC"}, code));
    REQUIRE(again.success);
    CHECK(again.version == 1);

    const auto changed = publisher.publish(request_of(name, {"default"}, code));
    REQUIRE(changed.success);
    CHECK(changed.version == 2);
    const auto variants = own_rows(repository::script_variant_repository(), h, name);
    REQUIRE(variants.size() == 1u);
    CHECK(variants[0].purpose == "default");
    CHECK(variants[0].script_version == 2);
}

TEST_CASE("a script that does not parse is refused with its diagnostics and writes nothing",
          "[service][script_library][publish]") {
    ores::testing::scoped_database_helper h;
    const auto name = "TenantPayoff_" + boost::uuids::to_string(boost::uuids::random_generator()());

    const auto refused = service::script_publish_service(h.context())
                             .publish(request_of(name, {"default"}, "Option = ;"));

    CHECK_FALSE(refused.success);
    REQUIRE_FALSE(refused.diagnostics.empty());
    CHECK(refused.diagnostics[0].severity == "error");
    CHECK(refused.diagnostics[0].purpose == "default");
    CHECK(refused.diagnostics[0].line == 1);
    CHECK_FALSE(refused.diagnostics[0].code.empty());

    auto scripts = repository::script_repository().read_latest(h.context(), name);
    CHECK(scripts.empty());
}

TEST_CASE("a script with no default variant is refused", "[service][script_library][publish]") {
    ores::testing::scoped_database_helper h;
    const auto refused = service::script_publish_service(h.context())
                             .publish(request_of("TenantPayoff_NoDefault", {"AMC"}, fresh_code()));

    CHECK_FALSE(refused.success);
    CHECK_THAT(refused.message, Catch::Matchers::ContainsSubstring("default"));
}

TEST_CASE("a publish in the system tenant is refused, and a variant with no code is refused",
          "[service][script_library][publish]") {
    ores::testing::scoped_database_helper h;
    const auto system_ctx =
        h.context().with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user());

    const auto in_world = service::script_publish_service(system_ctx)
                              .publish(request_of("Accumulator01", {"default"}, fresh_code()));
    CHECK_FALSE(in_world.success);
    CHECK_THAT(in_world.message, Catch::Matchers::ContainsSubstring("dataset"));

    const auto no_code = service::script_publish_service(h.context())
                             .publish(request_of("TenantPayoff_NoCode", {"default"}, ""));
    CHECK_FALSE(no_code.success);
    CHECK(no_code.diagnostics.empty());
    CHECK_THAT(no_code.message, Catch::Matchers::ContainsSubstring("code"));
}
