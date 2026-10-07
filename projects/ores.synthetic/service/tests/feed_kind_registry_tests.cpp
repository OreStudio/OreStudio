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
#include "../src/feed_config_handler.hpp"
#include "../src/feed_controller.hpp"
#include "../src/feed_kind_registry.hpp"
#include "../src/folder_feed_control_handler.hpp"
#include "../src/simulate_handler.hpp"
#include "../src/vintage_validity_handler.hpp"
#include "ores.analytics.quant/domain/i_stochastic_process.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/simulate_fx_spot_paths_protocol.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/test_database_manager.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <atomic>
#include <chrono>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

// Proves the per-kind registry is the one dispatch point: a stub kind over a
// stub factory, with the registry's container and folder reads injected as
// plain lambdas, is reached by start, stop, list, folder start, folder stop,
// vintage validity and simulate -- with no edit to any handler and no
// database. The stub's own closures read no repository, so nothing here needs
// a connection of any kind.

namespace {

const std::string tags("[feed_kind_registry]");

const std::string test_secret("feed-kind-registry-test-secret");
const std::string test_issuer("ores.iam.test");
const std::string test_audience("ores.synthetic.service");

const std::string stub_kind_name("stub");
const std::string stub_permission("synthetic::stub_generation_configs:read");
const std::string stub_container("11111111-1111-1111-1111-111111111111");
const std::string stub_id("22222222-2222-2222-2222-222222222222");
const std::string stub_other_id("33333333-3333-3333-3333-333333333333");
const std::string stub_folder("44444444-4444-4444-4444-444444444444");
const std::string stub_outside_folder("55555555-5555-5555-5555-555555555555");
const std::string stub_source("stub.test.source");
const std::string stub_ignored_source("stub.test.ignored");

boost::uuids::uuid uuid_of(const std::string& s) {
    return boost::uuids::string_generator()(s);
}

// A stub IFeed: the stand-in for a third asset class's producer.
class stub_feed final : public ores::marketdata::domain::IFeed {
public:
    explicit stub_feed(std::string source) : source_(std::move(source)) {}

    const std::string& source_name() const override {
        return source_;
    }
    const std::string& qualifier() const override {
        return qualifier_;
    }
    const std::string& role() const override {
        return role_;
    }
    const std::string& nats_subject() const override {
        return subject_;
    }
    std::string_view kind() const override {
        return stub_kind_name;
    }
    std::string conflict_key() const override {
        return ores::marketdata::domain::feed_conflict_key(qualifier_, role_);
    }
    std::uint64_t publish_count() const override {
        return 0;
    }
    void start() override {
        while (!stop_.load())
            std::this_thread::sleep_for(std::chrono::milliseconds(1));
    }
    void stop() override {
        stop_.store(true);
    }

private:
    std::string source_;
    std::string qualifier_;
    std::string role_;
    std::string subject_;
    std::atomic<bool> stop_{false};
};

// A stub IStochasticProcess: one flat sample, so the envelope's clamps and
// per-path stepping are observable without a real process.
class stub_process final : public ores::analytics::quant::domain::IStochasticProcess {
public:
    double next() override {
        return 42.0;
    }
    double current() const override {
        return 42.0;
    }
};

// A no-op binding store, so a bound stub feed never calls marketdata.
class no_binding_store final : public ores::synthetic::service::feed_binding_store {
public:
    std::expected<void, std::string> save_if_absent(const std::string&,
                                                    const std::string&) override {
        return {};
    }
};

int stub_builds = 0;

ores::synthetic::feed::feed_factory stub_factory() {
    ores::synthetic::feed::feed_factory f;
    f.register_kind(stub_kind_name, [](const auto&, const auto&) {
        ++stub_builds;
        return std::make_shared<stub_feed>(stub_source);
    });
    return f;
}

// One stub config row. The first sits inside the folder and starts; the second
// sits outside it, so the cascade's subtree filter is observable.
ores::synthetic::service::feed_kind_candidate stub_candidate(const std::string& id,
                                                             const std::string& source,
                                                             const std::string& folder) {
    ores::synthetic::service::feed_kind_candidate c;
    c.container_id = uuid_of(stub_container);
    c.feed_config_id = id;
    c.source_name = source;
    c.display_name = "STUB";
    c.enabled = true;
    c.auto_start = true;
    c.folder_id = uuid_of(folder);
    c.vintage_anchor = [] {
        ores::synthetic::service::feed_vintage_anchor a;
        a.applicable = true;
        a.source = "vintage";
        a.date = "2016-02-05";
        a.series_uri = "oresmd://fx/EURUSD?type=quote&instrument=spot";
        a.datum_uri = "oresmd://fx/EURUSD?type=quote&instrument=spot";
        return a;
    };
    c.build_input = [](ores::synthetic::domain::binding_mode) {
        return ores::synthetic::service::feed_build_outcome{
            .input = ores::synthetic::feed::feed_build_input{
                ores::synthetic::feed::fx_spot_feed_build_input{}},
            .skip_reason = {}};
    };
    return c;
}

// The whole extension story in one value: a kind name, its config permission,
// one candidates closure, and two simulate closures.
ores::synthetic::service::feed_kind stub_kind(const std::string& permission) {
    ores::synthetic::service::feed_kind k;
    k.kind = stub_kind_name;
    k.config_permission = permission;
    k.candidates = [](const ores::database::context&, const std::string& id) {
        std::vector<ores::synthetic::service::feed_kind_candidate> out;
        if (id.empty() || id == stub_id)
            out.push_back(stub_candidate(stub_id, stub_source, stub_folder));
        if (id.empty())
            out.push_back(stub_candidate(stub_other_id, stub_ignored_source, stub_outside_folder));
        return out;
    };
    k.simulate_subject = "ores.test.stub.simulate_paths";
    k.decode_simulate = [](const ores::nats::message&) {
        return ores::synthetic::service::feed_simulate_envelope{
            .num_ticks = 3,
            .num_paths = 2,
            .seed = 7,
            .make_process = [](std::uint32_t) {
                return std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess>(
                    std::make_unique<stub_process>());
            }};
    };
    k.reply_simulate = [](ores::nats::service::client&,
                          const ores::nats::message&,
                          const ores::synthetic::service::feed_simulation_result&) {};
    return k;
}

// A registry over the stub factory and one in-memory container. The folder
// subtree is the root plus everything under it, so the stub's in-folder row
// matches and its out-of-folder row does not.
std::shared_ptr<ores::synthetic::service::feed_kind_registry>
make_stub_registry(const ores::synthetic::feed::feed_factory& factory) {
    static std::map<boost::uuids::uuid, ores::synthetic::domain::market_data_generation_config>
        containers;
    containers.clear();
    ores::synthetic::domain::market_data_generation_config container;
    container.id = uuid_of(stub_container);
    container.enabled = true;
    container.binding_mode = ores::synthetic::domain::binding_mode::sandboxed;
    containers.emplace(container.id, container);

    auto registry = std::make_shared<ores::synthetic::service::feed_kind_registry>(
        ores::synthetic::service::feed_kind_registry::deps{
            .factory = &factory,
            .folder_subtree = [](const ores::database::context&, const boost::uuids::uuid& root) {
                std::set<boost::uuids::uuid> ids;
                ids.insert(root);
                return ids;
            },
            .container_by_id =
                [](const ores::database::context&, const boost::uuids::uuid& id) {
                    const auto it = containers.find(id);
                    return it == containers.end()
                               ? std::nullopt
                               : std::optional<
                                     ores::synthetic::domain::market_data_generation_config>(
                                     it->second);
                },
            .containers = [](const ores::database::context&) {
                std::vector<ores::synthetic::domain::market_data_generation_config> out;
                for (const auto& [_, c] : containers)
                    out.push_back(c);
                return out;
            }});
    registry->register_kind(stub_kind(stub_permission));
    return registry;
}

std::string mint_token(const std::vector<std::string>& roles) {
    auto signer = ores::security::jwt::jwt_authenticator::create_hs256(
        test_secret, test_issuer, test_audience);
    auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
    claims.subject = "feed-kind-registry-test";
    claims.issuer = test_issuer;
    claims.audience = test_audience;
    claims.tenant_id = boost::uuids::to_string(boost::uuids::random_generator()());
    claims.party_id = boost::uuids::to_string(boost::uuids::random_generator()());
    claims.roles = roles;
    return *signer.create_token(claims);
}

// A hand-built inbound message. reply_subject stays empty throughout: the
// handler verbs then take their no-reply path, and the test asserts on the
// effects they produce rather than on a reply it would have to receive over
// NATS.
template <typename Req>
ores::nats::message request_message(const Req& req, const std::string& token) {
    ores::nats::message msg;
    msg.subject = "ores.test.request";
    msg.data = ores::nats::default_wire_codec().encode(req);
    msg.headers.emplace(std::string(ores::nats::headers::authorization),
                        std::string(ores::nats::headers::bearer_prefix) + token);
    return msg;
}

// A real database context over the test database, so the handlers can be
// constructed exactly as the registrar constructs them. No verb in this file
// reads it: the stub's candidates closure answers from memory, and the
// registry's container and folder reads are injected lambdas.
ores::database::context make_test_context() {
    const auto opts = ores::testing::test_database_manager::make_database_options();
    return ores::database::context_factory::make_context(
        ores::database::context_factory::configuration{.database_options = opts,
                                                       .pool_size = 1,
                                                       .num_attempts = 1,
                                                       .wait_time_in_seconds = 1,
                                                       .service_account = opts.user});
}

// The registry and the controllers every handler verb below is driven with.
struct stub_fixture {
    ores::synthetic::feed::feed_factory factory = stub_factory();
    std::shared_ptr<ores::synthetic::service::feed_kind_registry> registry =
        make_stub_registry(factory);
    ores::nats::service::client nats;
    ores::nats::service::nats_client auth_nats;
    ores::database::context ctx;
    std::shared_ptr<ores::synthetic::service::feed_controller> ctrl;
    std::optional<ores::security::jwt::jwt_authenticator> verifier;
    std::string token = mint_token({stub_permission});

    stub_fixture()
        : nats(ores::testing::make_nats_options())
        , auth_nats(nats, [](bool) { return std::string(); })
        , ctx(make_test_context()) {
        ctrl = std::make_shared<ores::synthetic::service::feed_controller>(
            nats, auth_nats, std::make_shared<no_binding_store>());
        verifier = ores::security::jwt::jwt_authenticator::create_hs256(
            test_secret, test_issuer, test_audience);
    }
};

}

using namespace ores::synthetic::messaging;
using namespace ores::marketdata::messaging;
using namespace ores::synthetic::service;
using ores::synthetic::service::feed_config_handler;
using ores::synthetic::service::feed_kind_registry;
using ores::synthetic::service::feed_simulate_envelope;
using ores::synthetic::service::feed_start_target;
using ores::synthetic::service::feed_start_attempt;

TEST_CASE("registry_register_kind_validates_what_a_kind_must_supply", tags) {
    auto factory = stub_factory();
    feed_kind_registry registry(feed_kind_registry::deps{.factory = &factory});

    // A kind the factory has no builder for cannot be registered at all, so
    // the factory registration and the control-plane registration cannot
    // drift silently.
    auto unknown = stub_kind(stub_permission);
    unknown.kind = "stub_unknown";
    CHECK_THROWS_AS(registry.register_kind(unknown), std::invalid_argument);

    // An empty kind, a missing permission and missing closures are the other
    // half of the same guard.
    auto empty = stub_kind(stub_permission);
    empty.kind.clear();
    CHECK_THROWS_AS(registry.register_kind(empty), std::invalid_argument);

    auto no_permission = stub_kind(stub_permission);
    no_permission.config_permission.clear();
    CHECK_THROWS_AS(registry.register_kind(no_permission), std::invalid_argument);

    auto no_candidates = stub_kind(stub_permission);
    no_candidates.candidates = {};
    CHECK_THROWS_AS(registry.register_kind(no_candidates), std::invalid_argument);

    auto no_simulate = stub_kind(stub_permission);
    no_simulate.reply_simulate = {};
    CHECK_THROWS_AS(registry.register_kind(no_simulate), std::invalid_argument);

    registry.register_kind(stub_kind(stub_permission));
    REQUIRE(registry.all().size() == 1);
    CHECK(registry.find(stub_kind_name) != nullptr);
    CHECK(registry.find("not_a_kind") == nullptr);

    // A second registration under the same kind is rejected.
    CHECK_THROWS_AS(registry.register_kind(stub_kind(stub_permission)), std::invalid_argument);
}

TEST_CASE("registry_permits_all_configs_is_derived_from_the_registrations", tags) {
    stub_fixture f;
    feed_kind_registry registry(feed_kind_registry::deps{.factory = &f.factory});
    registry.register_kind(stub_kind(stub_permission));

    const auto verifier = ores::security::jwt::jwt_authenticator::create_hs256(
        test_secret, test_issuer, test_audience);
    const auto ctx_with = ores::service::service::make_request_context(
        f.ctx,
        request_message(list_feeds_request{}, mint_token({stub_permission})),
        verifier);
    const auto ctx_without = ores::service::service::make_request_context(
        f.ctx,
        request_message(list_feeds_request{}, mint_token({"synthetic::other:read"})),
        verifier);
    REQUIRE(ctx_with.has_value());
    REQUIRE(ctx_without.has_value());

    CHECK(registry.permits_all_configs(*ctx_with));
    CHECK_FALSE(registry.permits_all_configs(*ctx_without));

    // A context carrying no permission list at all is the service's own: it
    // grants everything.
    CHECK(registry.permits_all_configs(f.ctx));
}

TEST_CASE("registry_make_feed_applies_the_gate_and_builds_through_the_factory", tags) {
    stub_fixture f;
    const auto registry = f.registry;
    const ores::synthetic::feed::feed_build_context bctx{f.nats, f.auth_nats, {}};

    const auto targets = registry->targets(f.ctx);
    REQUIRE(targets.size() == 2);
    for (const auto& target : targets)
        CHECK(target.startable());
    CHECK(targets.front().binding_mode == ores::synthetic::domain::binding_mode::sandboxed);

    stub_builds = 0;
    const auto attempt = registry->make_feed(targets.front(), bctx);
    REQUIRE(attempt.feed != nullptr);
    CHECK(attempt.failure.empty());
    CHECK(stub_builds == 1);
    CHECK(attempt.feed->kind() == stub_kind_name);

    // A target whose container is missing is not startable, and startability
    // is what the one gate reports.
    feed_start_target unstartable;
    unstartable.row.kind = registry->find(stub_kind_name);
    unstartable.row.candidate = stub_candidate(stub_id, stub_source, stub_folder);
    CHECK_FALSE(unstartable.startable());
    const auto refused = registry->make_feed(unstartable, bctx);
    CHECK(refused.feed == nullptr);
    CHECK(refused.failure == "Feed config is not enabled: " + stub_id);
}

TEST_CASE("registry_resolve_and_rows_walk_the_registered_kinds", tags) {
    stub_fixture f;
    const auto registry = f.registry;

    // resolve(): the named row, tagged with the kind that produced it.
    const auto target = registry->resolve(f.ctx, stub_id);
    REQUIRE(target.has_value());
    CHECK(target->row.kind->kind == stub_kind_name);
    CHECK(target->row.candidate.source_name == stub_source);

    // An id no registered kind claims resolves to nothing.
    CHECK_FALSE(registry->resolve(f.ctx, "not-a-config-id").has_value());

    // rows(): every row of every kind, untagged by container state.
    CHECK(registry->rows(f.ctx).size() == 2);
}

TEST_CASE("run_simulate_paths_owns_the_envelope_for_every_kind", tags) {
    const feed_simulate_envelope env{
        .num_ticks = 3,
        .num_paths = 2,
        .seed = 7,
        .make_process = [](std::uint32_t) {
            return std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess>(
                std::make_unique<stub_process>());
        }};

    const auto result = ores::synthetic::service::run_simulate_paths(env);
    REQUIRE(result.success);
    REQUIRE(result.paths.size() == 2);
    CHECK(result.paths[0].size() == 3);
    CHECK(result.paths[1].size() == 3);
    CHECK(result.paths[0][0] == 42.0);

    // The clamp is the shared one, not the request's own field.
    auto clamped = env;
    clamped.num_ticks = 0;
    clamped.num_paths = 0;
    const auto clamped_result = ores::synthetic::service::run_simulate_paths(clamped);
    REQUIRE(clamped_result.success);
    REQUIRE(clamped_result.paths.size() == 1);
    CHECK(clamped_result.paths[0].size() == 1);

    // The per-path seed is the base seed plus the path index.
    std::vector<std::uint32_t> seeds;
    auto recording = env;
    recording.num_paths = 3;
    recording.make_process = [&seeds](std::uint32_t seed) {
        seeds.push_back(seed);
        return std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess>(
            std::make_unique<stub_process>());
    };
    REQUIRE(ores::synthetic::service::run_simulate_paths(recording).success);
    CHECK(seeds == std::vector<std::uint32_t>{7, 8, 9});

    // A process that throws is reported, not propagated.
    auto throwing = env;
    throwing.make_process = [](std::uint32_t) -> std::unique_ptr<
                                       ores::analytics::quant::domain::IStochasticProcess> {
        throw std::runtime_error("no process");
    };
    const auto failed = ores::synthetic::service::run_simulate_paths(throwing);
    CHECK_FALSE(failed.success);
    CHECK(failed.message == "no process");
}

TEST_CASE("every_control_plane_verb_reaches_a_stub_kind", tags) {
    stub_fixture f;

    // start: the verb reaches the stub kind's candidates and build_input, and
    // its producer goes through factory().make("stub", ...) into the
    // controller.
    stub_builds = 0;
    {
        feed_config_handler h(f.nats, f.auth_nats, f.ctrl, f.ctx, f.verifier,
                              *f.registry);
        h.start(request_message(start_feed_request{.config_id = stub_id}, f.token));
    }
    CHECK(stub_builds == 1);
    CHECK(f.ctrl->running_count() == 1);
    CHECK(f.ctrl->list() == std::vector<std::string>{stub_source});

    // A config id the stub kind does not claim starts nothing.
    {
        feed_config_handler h(f.nats, f.auth_nats, f.ctrl, f.ctx, f.verifier,
                              *f.registry);
        h.start(request_message(start_feed_request{.config_id = "not-a-config-id"}, f.token));
    }
    CHECK(f.ctrl->running_count() == 1);

    // stop: the verb resolves the config id through the same registry.
    {
        feed_config_handler h(f.nats, f.auth_nats, f.ctrl, f.ctx, f.verifier,
                              *f.registry);
        h.stop(request_message(stop_feed_request{.config_id = stub_id}, f.token));
    }
    CHECK(f.ctrl->running_count() == 0);

    // folder start: the cascade loops the registered kinds, filters by the
    // subtree the registry supplies, and tallies under the registered kind.
    {
        folder_feed_control_handler h(f.nats, f.ctrl, f.auth_nats, f.ctx,
                                      f.verifier, *f.registry);
        h.start(request_message(
            start_feeds_under_folder_request{.folder_id = stub_folder}, f.token));
    }
    CHECK(f.ctrl->running_count() == 1);
    CHECK(f.ctrl->list() == std::vector<std::string>{stub_source});

    // folder stop: the same walk, over rows rather than targets.
    {
        folder_feed_control_handler h(f.nats, f.ctrl, f.auth_nats, f.ctx,
                                      f.verifier, *f.registry);
        h.stop(request_message(stop_feeds_under_folder_request{.folder_id = stub_folder}, f.token));
    }
    CHECK(f.ctrl->running_count() == 0);

    // vintage validity: the verb iterates the registered kinds and reads the
    // kind's own anchor coordinate. The anchor is applicable, and the lookup
    // itself fails (there is no marketdata here), so this asserts the verb
    // reaches the stub rather than the outcome of a live read.
    {
        vintage_validity_handler h(f.nats, f.auth_nats, f.ctx, f.verifier,
                                   *f.registry);
        h.list(request_message(get_vintage_validity_request{}, f.token));
    }

    // simulate: the verb decodes through the registered kind's closure and
    // replies with the shared envelope. The registry's own kind lookup is the
    // dispatch, so the verb needs no literal.
    CHECK(f.registry->find(stub_kind_name)->simulate_subject == "ores.test.stub.simulate_paths");
    const auto env = f.registry->find(stub_kind_name)->decode_simulate(
        request_message(simulate_fx_spot_paths_request{}, f.token));
    REQUIRE(env.has_value());
    const auto simulated = ores::synthetic::service::run_simulate_paths(*env);
    REQUIRE(simulated.success);
    REQUIRE(simulated.paths.size() == 2);
    CHECK(simulated.paths[0].size() == 3);

    {
        simulate_handler h(f.nats, f.ctx, f.verifier, *f.registry);
        h.simulate(request_message(simulate_fx_spot_paths_request{}, f.token), stub_kind_name);
    }

    // A kind the registry does not know is answered, not dereferenced.
    {
        simulate_handler h(f.nats, f.ctx, f.verifier, *f.registry);
        h.simulate(request_message(simulate_fx_spot_paths_request{}, f.token), "not_a_kind");
    }
}
