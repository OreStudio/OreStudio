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
#include "../src/simulated_source_handler.hpp"
#include "../src/source_control.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.core/repository/stopped_simulated_source_repository.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/test_database_manager.hpp"
#include "test_helpers.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace {

namespace source_messaging = ores::synthetic::messaging;
using ores::synthetic::service::simulated_source_handler;
using ores::synthetic::service::source_control;

const std::string source_test_secret("simulated-source-handler-test-source_test_secret");

// The sources the handler sees, and the stops and starts it asked for.
class fake_sources final : public source_control {
public:
    explicit fake_sources(std::string name) {
        all_.push_back({.source_name = std::move(name), .provider_code = "bloomberg"});
    }

    std::vector<source_messaging::simulated_source> sources() const override {
        return all_;
    }
    bool stop(const std::string& source_name) override {
        stops.push_back(source_name);
        all_.front().stopped = true;
        return true;
    }
    bool start(const std::string& source_name) override {
        starts.push_back(source_name);
        all_.front().stopped = false;
        return true;
    }

    std::vector<std::string> stops;
    std::vector<std::string> starts;

private:
    std::vector<source_messaging::simulated_source> all_;
};

std::string source_token_for(const std::string& tenant_id, const std::vector<std::string>& roles) {
    auto signer = ores::security::jwt::jwt_authenticator::create_hs256(
        source_test_secret, test_issuer, test_audience);
    auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
    claims.subject = "simulated-source-handler-test";
    claims.issuer = test_issuer;
    claims.audience = test_audience;
    claims.tenant_id = tenant_id;
    claims.party_id = boost::uuids::to_string(boost::uuids::random_generator()());
    claims.roles = roles;
    return *signer.create_token(claims);
}

// The handler answers on the message's reply subject; with none it takes its no-reply path, and
// the test reads the effects instead.
template <typename Req>
ores::nats::message source_request_message(const Req& req, const std::string& token) {
    ores::nats::message msg;
    msg.subject = "ores.test.request";
    msg.data = ores::nats::default_wire_codec().encode(req);
    msg.headers.emplace(std::string(ores::nats::headers::authorization),
                        std::string(ores::nats::headers::bearer_prefix) + token);
    return msg;
}

struct source_fixture {
    ores::nats::service::client nats;
    ores::database::context ctx;
    std::optional<ores::security::jwt::jwt_authenticator> verifier;
    std::string source = "test-sim-" + boost::uuids::to_string(boost::uuids::random_generator()());
    fake_sources sources{source};

    source_fixture()
        : nats(ores::testing::make_nats_options())
        , ctx(ores::database::context_factory::make_context(
              ores::database::context_factory::configuration{
                  .database_options = ores::testing::test_database_manager::make_database_options(),
                  .pool_min = 1,
                  .pool_max = 1,
                  .pool_idle_seconds = 30,
                  .num_attempts = 1,
                  .wait_time_in_seconds = 1})) {
        verifier = ores::security::jwt::jwt_authenticator::create_hs256(
            source_test_secret, test_issuer, test_audience);
    }

    simulated_source_handler handler() {
        return simulated_source_handler(nats, ctx, verifier, sources);
    }

    std::vector<ores::synthetic::domain::stopped_simulated_source> stored() {
        return ores::synthetic::repository::stopped_simulated_source_repository().read_latest(
            ores::database::service::tenant_context::with_system_tenant(ctx), source);
    }
};

const std::string source_run_permission = std::string(ores::synthetic::service::run_permission);

}

TEST_CASE("a tenant may not stop a simulated source, which serves every tenant",
          "[simulated_source_handler][integration]") {
    source_fixture f;
    const auto tenant = boost::uuids::to_string(boost::uuids::random_generator()());
    f.handler().stop(source_request_message(
        source_messaging::stop_simulated_source_request{
            .source_name = f.source, .reason_code = "system.test", .commentary = "a tenant"},
        source_token_for(tenant, {source_run_permission})));
    CHECK(f.sources.stops.empty());
    CHECK(f.stored().empty());
}

TEST_CASE("a stopped source is recorded with its reason, and a start removes the record",
          "[simulated_source_handler][integration]") {
    source_fixture f;
    const auto system =
        source_token_for(std::string(ores::database::service::tenant_context::system_tenant_id),
                         {source_run_permission});

    // Without the run permission the system tenant is refused too.
    f.handler().stop(source_request_message(
        source_messaging::stop_simulated_source_request{.source_name = f.source,
                                                        .reason_code = "system.test"},
        source_token_for(std::string(ores::database::service::tenant_context::system_tenant_id),
                         {})));
    CHECK(f.sources.stops.empty());

    f.handler().stop(source_request_message(
        source_messaging::stop_simulated_source_request{
            .source_name = f.source, .reason_code = "system.test", .commentary = "for a test"},
        system));
    CHECK(f.sources.stops == std::vector<std::string>{f.source});
    const auto stored = f.stored();
    REQUIRE(stored.size() == 1);
    CHECK(stored.front().provider_code == "bloomberg");
    CHECK(stored.front().change_reason_code == "system.test");
    CHECK(stored.front().change_commentary == "for a test");

    // A stopped source is not stopped twice.
    f.handler().stop(source_request_message(
        source_messaging::stop_simulated_source_request{.source_name = f.source,
                                                        .reason_code = "system.test"},
        system));
    CHECK(f.sources.stops.size() == 1);

    f.handler().start(source_request_message(
        source_messaging::start_simulated_source_request{.source_name = f.source}, system));
    CHECK(f.sources.starts == std::vector<std::string>{f.source});
    CHECK(f.stored().empty());
}
