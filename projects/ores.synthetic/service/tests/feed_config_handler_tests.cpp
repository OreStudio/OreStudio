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
#include "ores.database/service/tenant_context.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/domain/instrument_code.hpp"
#include "ores.refdata.api/domain/payment_frequency.hpp"
#include "ores.refdata.api/domain/tenor.hpp"
#include "ores.refdata.api/domain/tenor_convention.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include "ores.refdata.core/repository/instrument_code_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/repository/payment_frequency_repository.hpp"
#include "ores.refdata.core/repository/tenor_convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_convention_resolution_repository.hpp"
#include "ores.refdata.core/repository/tenor_repository.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value.hpp"
#include "ores.synthetic.api/domain/ir_curve_template_entry.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_process_parameter_value_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_template_entry_repository.hpp"
#include "ores.synthetic.core/repository/market_data_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/yield_curve_process_parameter_definition_repository.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstdlib>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

// Proves the per-config control plane end to end: one kind-agnostic
// start/stop/list request keyed by config_id over real NATS resolves either
// kind server-side -- the fx_spot_generation_config or the
// ir_curve_generation_config row, its children, and the refdata context --
// checks the resolved kind's permission, and drives the feed controller via
// the factory. The handler resolves everything from the JWT's tenant/party
// claims (RLS), so the test seeds under an isolated per-run tenant exactly
// like the provisioner's do.

namespace {

const std::string tags("[feed_config_handler][integration]");

const std::string test_secret("feed-config-test-secret");
const std::string test_issuer("ores.iam.test");
const std::string test_audience("ores.synthetic.service");

// Test-scoped subjects: the running service subscribes to the real
// start/stop/list subjects on the same queue group, so a request sent on
// them can be answered by the service's own verifier (a different
// secret/algorithm) instead of the test's, and the reply fails to decode.
// The handler is subject-agnostic -- it only logs the subject and replies on
// the request's reply subject -- so the whole handler runs against these
// test subjects unchanged.
const std::string test_start_subject("ores.test.feed_config.start");
const std::string test_stop_subject("ores.test.feed_config.stop");
const std::string test_list_subject("ores.test.feed_config.list");

// Every seeded row is party-scoped and the JWT carries the same party, so
// the handler's RLS reads see what the test wrote. The party must exist in
// the parties table: set_party() and the JWT context both validate it.
boost::uuids::uuid resolve_test_party(ores::testing::scoped_database_helper& h) {
    namespace refdata_repo = ores::refdata::repository;
    const auto parties = refdata_repo::party_repository().read_latest(h.context());
    for (const auto& p : parties)
        if (p.tenant_id == h.tenant_id() && p.party_category == "System")
            return p.id;
    FAIL("No System party for tenant " << h.tenant_id().to_string());
    return {};
}

// Reads NATS connection settings the same way every service resolves them
// at startup -- CMake bakes every .env variable into the ctest process
// environment, so these are populated identically for both `compass build`
// local runs and CI.
ores::nats::config::nats_options test_nats_options() {
    auto env = [](const char* name) -> std::string {
        const char* v = std::getenv(name);
        return v ? std::string(v) : std::string();
    };

    ores::nats::config::nats_options opts;
    opts.url = env("ORES_NATS_URL");
    if (opts.url.empty())
        opts.url = "nats://localhost:4222";
    opts.subject_prefix = env("ORES_NATS_SUBJECT_PREFIX");
    opts.tls_ca_cert = env("ORES_NATS_TLS_CA");
    opts.tls_client_cert = env("ORES_NATS_TLS_CERT");
    opts.tls_client_key = env("ORES_NATS_TLS_KEY");
    return opts;
}

// Mints an HS256 token the handler's verifier accepts, scoped to the test
// tenant and party with the given roles.
std::string mint_token(const std::string& tenant_id,
                       const boost::uuids::uuid& party_id,
                       const std::vector<std::string>& roles) {
    auto signer = ores::security::jwt::jwt_authenticator::create_hs256(
        test_secret, test_issuer, test_audience);
    auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
    claims.subject = "feed-config-test";
    claims.issuer = test_issuer;
    claims.audience = test_audience;
    claims.tenant_id = tenant_id;
    claims.party_id = boost::uuids::to_string(party_id);
    claims.visible_party_ids = {boost::uuids::to_string(party_id)};
    claims.roles = roles;
    return *signer.create_token(claims);
}

// Sends one feed-config request on the given subject with the Bearer header
// and decodes the response.
template <typename Req, typename Resp>
Resp send_request(ores::nats::service::client& nats,
                  const std::string& subject,
                  const std::string& token,
                  const Req& req) {
    auto reply = nats.request_sync(subject,
                                   ores::nats::default_wire_codec().encode(req),
                                   {{std::string(ores::nats::headers::authorization),
                                     std::string(ores::nats::headers::bearer_prefix) + token}});
    auto decoded = ores::nats::default_wire_codec().decode<Resp>(reply.data);
    REQUIRE(decoded);
    return *decoded;
}

struct seeded_configs {
    boost::uuids::uuid container_id;
    boost::uuids::uuid fx_enabled_id;
    boost::uuids::uuid fx_disabled_id;
    boost::uuids::uuid fx_conflicting_id;
    boost::uuids::uuid fx_no_components_id;
    boost::uuids::uuid ir_enabled_id;
    boost::uuids::uuid ir_disabled_id;
    boost::uuids::uuid ir_no_entries_id;
    std::string fx_enabled_source;
    std::string fx_conflicting_source;
    std::string ir_enabled_source;
};

// Seeds four enabled containers holding: an enabled FX config (EUR/USD, with
// its GMM component) and an enabled IR curve config (USD/sofr, with its
// template entry and VASICEK parameter values) sharing one container, plus
// the negative-shape rows each test needs -- a disabled FX config and a
// disabled IR config (same container, distinct identities), an FX config on
// the same EUR/USD pair under its own container (the uniform conflict rule
// lets only one run), an FX config with no components, and an IR config
// with no template entries.
seeded_configs seed_configs(ores::testing::scoped_database_helper& h,
                            const boost::uuids::uuid& test_party) {
    namespace dom = ores::synthetic::domain;
    namespace repo = ores::synthetic::repository;
    using ores::synthetic::domain::binding_mode;
    using ores::synthetic::domain::scope;

    h.set_party(test_party);
    auto& party_ctx = h.context();
    const auto now = std::chrono::system_clock::now();
    const auto db_user = h.db_user();
    auto uuid = boost::uuids::random_generator();

    // The tenant provisioner copies these catalogs into every new tenant
    // (tenor 1M, instrument DEPO, convention RATES_SPOT_FORWARD with its
    // 1M resolution row, payment frequency Annual). Assert the copies so a
    // provisioner change fails here by name instead of as a feed-build skip.
    namespace refdata_repo = ores::refdata::repository;
    {
        const auto tenors = refdata_repo::tenor_repository().read_latest(party_ctx);
        REQUIRE(std::any_of(
            tenors.begin(), tenors.end(), [](const auto& t) { return t.code == "1M"; }));
        const auto instruments = refdata_repo::instrument_code_repository().read_latest(party_ctx);
        REQUIRE(std::any_of(instruments.begin(), instruments.end(), [](const auto& i) {
            return i.code == "DEPO" && i.curve_role == "DEPOSIT";
        }));
        const auto conventions = refdata_repo::tenor_convention_repository().read_latest(party_ctx);
        REQUIRE(std::any_of(conventions.begin(), conventions.end(), [](const auto& c) {
            return c.code == "RATES_SPOT_FORWARD";
        }));
        const auto frequencies =
            refdata_repo::payment_frequency_repository().read_latest(party_ctx);
        REQUIRE(std::any_of(frequencies.begin(), frequencies.end(), [](const auto& f) {
            return f.code == "Annual";
        }));
        refdata_repo::tenor_convention_resolution_repository resolution_repo(party_ctx);
        const auto resolutions = resolution_repo.read_latest_by_convention("RATES_SPOT_FORWARD");
        REQUIRE(std::any_of(resolutions.begin(), resolutions.end(), [](const auto& r) {
            return r.tenor_code == "1M";
        }));
    }

    // The values insert trigger (security definer) and the handler both
    // resolve parameter_definition_id against the system tenant, so the
    // values below reference the system catalogue's ids directly.
    std::vector<dom::yield_curve_process_parameter_definition> definitions;
    {
        repo::yield_curve_process_parameter_definition_repository definition_repo;
        const auto system_ctx =
            ores::database::service::tenant_context::with_system_tenant(h.context());
        for (const auto& d : definition_repo.read_latest(system_ctx))
            if (d.process_type_code == "VASICEK")
                definitions.push_back(d);
        REQUIRE(definitions.size() == 4);
        std::set<std::string> names;
        for (const auto& d : definitions)
            names.insert(d.parameter_name);
        REQUIRE(names == std::set<std::string>({"initial_rate", "kappa", "sigma", "theta"}));
    }

    const auto make_container = [&](const std::string& name) {
        dom::market_data_generation_config container;
        container.tenant_id = h.tenant_id();
        container.id = uuid();
        container.party_id = test_party;
        container.scope = scope::party;
        container.binding_mode = binding_mode::bound;
        container.enabled = true;
        container.name = name;
        container.modified_by = db_user;
        container.performed_by = db_user;
        container.change_reason_code = "system.test";
        container.change_commentary = "feed config test seed";
        container.recorded_at = now;
        return container;
    };

    const auto main_container = make_container("Feed Config Test Container");
    const auto conflict_container = make_container("Feed Config Test Conflict Container");
    const auto no_components_container = make_container("Feed Config Test No-Components Container");
    const auto no_entries_container = make_container("Feed Config Test No-Entries Container");
    repo::market_data_generation_config_repository().write(party_ctx, main_container);
    repo::market_data_generation_config_repository().write(party_ctx, conflict_container);
    repo::market_data_generation_config_repository().write(party_ctx, no_components_container);
    repo::market_data_generation_config_repository().write(party_ctx, no_entries_container);

    const auto make_fx = [&](const dom::market_data_generation_config& container,
                             const std::string& base,
                             const std::string& quote,
                             const std::string& source_name,
                             bool enabled) {
        dom::fx_spot_generation_config fx;
        fx.tenant_id = h.tenant_id();
        fx.id = uuid();
        fx.party_id = test_party;
        fx.config_id = container.id;
        fx.base_currency_code = base;
        fx.quote_currency_code = quote;
        fx.source_name = source_name;
        fx.ore_key = "FX/RATE/" + base + "/" + quote;
        fx.price_source = "fixed";
        fx.gmm_initial_price = 1.08;
        fx.ticks_per_hour = 12;
        fx.process_type = "geometric";
        fx.enabled = enabled;
        fx.auto_start = false;
        fx.vintage_source = "";
        fx.vintage_date = "";
        fx.modified_by = db_user;
        fx.performed_by = db_user;
        fx.change_reason_code = "system.test";
        fx.change_commentary = "feed config test seed";
        fx.recorded_at = now;
        return fx;
    };

    auto fx_enabled = make_fx(main_container, "EUR", "USD", "feed_config.test.fx.eur_usd", true);
    const auto fx_disabled =
        make_fx(main_container, "GBP", "USD", "feed_config.test.fx.gbp_usd", false);
    const auto fx_conflicting =
        make_fx(conflict_container, "EUR", "USD", "feed_config.test.fx.eur_usd_conflict", true);
    const auto fx_no_components =
        make_fx(no_components_container, "CHF", "USD", "feed_config.test.fx.chf_usd", true);

    repo::fx_spot_generation_config_repository().write(party_ctx, fx_enabled);
    repo::fx_spot_generation_config_repository().write(party_ctx, fx_disabled);
    repo::fx_spot_generation_config_repository().write(party_ctx, fx_conflicting);
    repo::fx_spot_generation_config_repository().write(party_ctx, fx_no_components);

    dom::gmm_component comp;
    comp.tenant_id = h.tenant_id();
    comp.id = uuid();
    comp.party_id = test_party;
    comp.fx_spot_config_id = fx_enabled.id;
    comp.component_index = 0;
    comp.mean = 0.0001;
    comp.stdev = 0.001;
    comp.weight = 1.0;
    comp.description = "feed config test component";
    comp.modified_by = db_user;
    comp.performed_by = db_user;
    comp.change_reason_code = "system.test";
    comp.change_commentary = "feed config test seed";
    comp.recorded_at = now;
    repo::gmm_component_repository().write(party_ctx, comp);

    // The conflicting config is otherwise identical to the enabled one -- the
    // handler must reach the controller's conflict check, so it needs its
    // own component row.
    dom::gmm_component conflict_comp = comp;
    conflict_comp.id = uuid();
    conflict_comp.fx_spot_config_id = fx_conflicting.id;
    conflict_comp.description = "feed config test conflicting component";
    repo::gmm_component_repository().write(party_ctx, conflict_comp);

    const auto make_ir = [&](const dom::market_data_generation_config& container,
                             const std::string& currency,
                             const std::string& source_name,
                             bool enabled) {
        dom::ir_curve_generation_config ir;
        ir.tenant_id = h.tenant_id();
        ir.id = uuid();
        ir.party_id = test_party;
        ir.config_id = container.id;
        ir.currency_code = currency;
        ir.index_family = "sofr";
        ir.tenor = "";
        ir.role = "self_discounting";
        ir.process_type = "VASICEK";
        ir.ticks_per_hour = 12;
        ir.enabled = enabled;
        ir.auto_start = false;
        ir.price_source = "fixed";
        ir.vintage_source = "";
        ir.vintage_date = "";
        ir.description = "feed config test IR config";
        ir.fixed_leg_payment_frequency_code = "Annual";
        ir.source_name = source_name;
        ir.modified_by = db_user;
        ir.performed_by = db_user;
        ir.change_reason_code = "system.test";
        ir.change_commentary = "feed config test seed";
        ir.recorded_at = now;
        return ir;
    };

    auto ir_enabled = make_ir(main_container, "USD", "feed_config.test.ir.usd_sofr", true);
    const auto ir_disabled =
        make_ir(main_container, "EUR", "feed_config.test.ir.eur_sofr_disabled", false);
    const auto ir_no_entries =
        make_ir(no_entries_container, "EUR", "feed_config.test.ir.eur_sofr_no_entries", true);

    repo::ir_curve_generation_config_repository().write(party_ctx, ir_enabled);
    repo::ir_curve_generation_config_repository().write(party_ctx, ir_disabled);
    repo::ir_curve_generation_config_repository().write(party_ctx, ir_no_entries);

    dom::ir_curve_template_entry entry;
    entry.tenant_id = h.tenant_id();
    entry.id = uuid();
    entry.party_id = test_party;
    entry.ir_curve_config_id = ir_enabled.id;
    entry.sequence_index = 0;
    entry.start_tenor_code = "SPOT";
    entry.end_tenor_code = "1M";
    entry.instrument_code = "DEPO";
    entry.modified_by = db_user;
    entry.performed_by = db_user;
    entry.change_reason_code = "system.test";
    entry.change_commentary = "feed config test seed";
    entry.recorded_at = now;
    repo::ir_curve_template_entry_repository().write(party_ctx, entry);

    const std::map<std::string, double> value_by_name = {
        {"kappa", 0.05}, {"theta", 0.03}, {"sigma", 0.01}, {"initial_rate", 0.025}};
    for (const auto& d : definitions) {
        dom::ir_curve_generation_config_process_parameter_value v;
        v.tenant_id = h.tenant_id();
        v.id = uuid();
        v.config_id = ir_enabled.id;
        v.parameter_definition_id = d.id;
        v.parameter_value = value_by_name.at(d.parameter_name);
        v.modified_by = db_user;
        v.performed_by = db_user;
        v.change_reason_code = "system.test";
        v.change_commentary = "feed config test seed";
        v.recorded_at = now;
        repo::ir_curve_generation_config_process_parameter_value_repository().write(party_ctx, v);
    }

    return {.container_id = main_container.id,
            .fx_enabled_id = fx_enabled.id,
            .fx_disabled_id = fx_disabled.id,
            .fx_conflicting_id = fx_conflicting.id,
            .fx_no_components_id = fx_no_components.id,
            .ir_enabled_id = ir_enabled.id,
            .ir_disabled_id = ir_disabled.id,
            .ir_no_entries_id = ir_no_entries.id,
            .fx_enabled_source = fx_enabled.source_name,
            .fx_conflicting_source = fx_conflicting.source_name,
            .ir_enabled_source = ir_enabled.source_name};
}

// The handler under test runs on the NATS worker thread, exactly as the
// registrar runs it in the service: one instance per inbound message, with
// the real controllers and the real database context.
struct config_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::nats::service::nats_client auth_nats;
    std::shared_ptr<ores::synthetic::service::feed_controller> ctrl;
    std::optional<ores::security::jwt::jwt_authenticator> verifier;
    std::optional<ores::nats::service::subscription> start_sub;
    std::optional<ores::nats::service::subscription> stop_sub;
    std::optional<ores::nats::service::subscription> list_sub;

    config_fixture()
        : nats(test_nats_options())
        , auth_nats(nats, [](bool) { return std::string(); }) {
        nats.connect();
        REQUIRE(nats.is_connected());

        ctrl = std::make_shared<ores::synthetic::service::feed_controller>(nats, auth_nats);
        verifier = ores::security::jwt::jwt_authenticator::create_hs256(
            test_secret, test_issuer, test_audience);

        start_sub = nats.queue_subscribe(
            test_start_subject, "ores.synthetic.service", [this](ores::nats::message msg) {
                ores::synthetic::service::feed_config_handler h(
                    nats, auth_nats, ctrl, db.context(), verifier);
                h.start(std::move(msg));
            });
        stop_sub = nats.queue_subscribe(
            test_stop_subject, "ores.synthetic.service", [this](ores::nats::message msg) {
                ores::synthetic::service::feed_config_handler h(
                    nats, auth_nats, ctrl, db.context(), verifier);
                h.stop(std::move(msg));
            });
        list_sub = nats.queue_subscribe(
            test_list_subject, "ores.synthetic.service", [this](ores::nats::message msg) {
                ores::synthetic::service::feed_config_handler h(
                    nats, auth_nats, ctrl, db.context(), verifier);
                h.list(std::move(msg));
            });
    }
};

}

using namespace ores::synthetic::messaging;

TEST_CASE("feed_config_start_stops_and_lists_both_kinds_by_config_id", tags) {
    config_fixture f;
    const auto test_party = resolve_test_party(f.db);
    const auto seeds = seed_configs(f.db, test_party);
    const auto token = mint_token(f.db.tenant_id().to_string(),
                                  test_party,
                                  {"synthetic::fx_spot_generation_configs:read",
                                   "synthetic::ir_curve_generation_configs:read"});

    // FX start by config_id: the server resolves the config, its children,
    // and the container, then starts the feed via the factory.
    const auto fx_start = send_request<start_feed_request, start_feed_response>(
        f.nats,
        test_start_subject,
        token,
        start_feed_request{.config_id = boost::uuids::to_string(seeds.fx_enabled_id)});
    REQUIRE(fx_start.success);
    CHECK(fx_start.message == "Feed started: " + seeds.fx_enabled_source);
    CHECK(f.ctrl->running_count() == 1);

    // Repeat: already running is success with the uniform message.
    const auto fx_again = send_request<start_feed_request, start_feed_response>(
        f.nats,
        test_start_subject,
        token,
        start_feed_request{.config_id = boost::uuids::to_string(seeds.fx_enabled_id)});
    REQUIRE(fx_again.success);
    CHECK(fx_again.message == "Feed already running: " + seeds.fx_enabled_source);

    // IR start by config_id: same request shape, different kind.
    const auto ir_start = send_request<start_feed_request, start_feed_response>(
        f.nats,
        test_start_subject,
        token,
        start_feed_request{.config_id = boost::uuids::to_string(seeds.ir_enabled_id)});
    REQUIRE(ir_start.success);
    CHECK(ir_start.message == "Feed started: " + seeds.ir_enabled_source);

    // List: one request returns every kind's running source_names.
    const auto list = send_request<list_feeds_request, list_feeds_response>(
        f.nats, test_list_subject, token, list_feeds_request{});
    REQUIRE(list.success);
    REQUIRE(list.running_source_names.size() == 2);
    CHECK(std::find(list.running_source_names.begin(),
                    list.running_source_names.end(),
                    seeds.fx_enabled_source) != list.running_source_names.end());
    CHECK(std::find(list.running_source_names.begin(),
                    list.running_source_names.end(),
                    seeds.ir_enabled_source) != list.running_source_names.end());

    // Stop the IR feed by config_id: the server resolves it to the
    // config's source_name.
    const auto ir_stop = send_request<stop_feed_request, stop_feed_response>(
        f.nats,
        test_stop_subject,
        token,
        stop_feed_request{.config_id = boost::uuids::to_string(seeds.ir_enabled_id)});
    REQUIRE(ir_stop.success);
    CHECK(ir_stop.message == "1 feed(s) stopped");
    CHECK(f.ctrl->running_count() == 1);

    // Stop the FX feed by source_name (the config_id-less path).
    const auto fx_stop = send_request<stop_feed_request, stop_feed_response>(
        f.nats,
        test_stop_subject,
        token,
        stop_feed_request{.source_name = seeds.fx_enabled_source});
    REQUIRE(fx_stop.success);
    CHECK(fx_stop.message == "1 feed(s) stopped");
    CHECK(f.ctrl->running_count() == 0);
}

TEST_CASE("feed_config_reports_not_found_and_disabled_uniformly", tags) {
    config_fixture f;
    const auto test_party = resolve_test_party(f.db);
    const auto seeds = seed_configs(f.db, test_party);
    const auto token = mint_token(f.db.tenant_id().to_string(),
                                  test_party,
                                  {"synthetic::fx_spot_generation_configs:read",
                                   "synthetic::ir_curve_generation_configs:read"});

    const auto missing_id = boost::uuids::to_string(boost::uuids::random_generator()());
    const auto not_found = send_request<start_feed_request, start_feed_response>(
        f.nats, test_start_subject, token, start_feed_request{.config_id = missing_id});
    CHECK_FALSE(not_found.success);
    CHECK(not_found.message == "Feed config not found: " + missing_id);

    const auto stop_not_found = send_request<stop_feed_request, stop_feed_response>(
        f.nats, test_stop_subject, token, stop_feed_request{.config_id = missing_id});
    CHECK_FALSE(stop_not_found.success);
    CHECK(stop_not_found.message == "Feed config not found: " + missing_id);

    // Disabled configs read the same for both kinds.
    const auto fx_disabled = send_request<start_feed_request, start_feed_response>(
        f.nats,
        test_start_subject,
        token,
        start_feed_request{.config_id = boost::uuids::to_string(seeds.fx_disabled_id)});
    CHECK_FALSE(fx_disabled.success);
    CHECK(fx_disabled.message ==
          "Feed config is not enabled: " + boost::uuids::to_string(seeds.fx_disabled_id));

    const auto ir_disabled = send_request<start_feed_request, start_feed_response>(
        f.nats,
        test_start_subject,
        token,
        start_feed_request{.config_id = boost::uuids::to_string(seeds.ir_disabled_id)});
    CHECK_FALSE(ir_disabled.success);
    CHECK(ir_disabled.message ==
          "Feed config is not enabled: " + boost::uuids::to_string(seeds.ir_disabled_id));

    CHECK(f.ctrl->running_count() == 0);
}

TEST_CASE("feed_config_start_requires_the_resolved_kind_permission", tags) {
    config_fixture f;
    const auto test_party = resolve_test_party(f.db);
    const auto seeds = seed_configs(f.db, test_party);

    // FX-only token: the IR start must be rejected at the resolved-kind
    // permission check, with no feed started.
    const auto fx_only_token = mint_token(
        f.db.tenant_id().to_string(), test_party, {"synthetic::fx_spot_generation_configs:read"});
    const auto ir_reply =
        f.nats.request_sync(test_start_subject,
                            ores::nats::default_wire_codec().encode(start_feed_request{
                                .config_id = boost::uuids::to_string(seeds.ir_enabled_id)}),
                            {{std::string(ores::nats::headers::authorization),
                              std::string(ores::nats::headers::bearer_prefix) + fx_only_token}});
    REQUIRE(ir_reply.headers.contains(std::string(ores::nats::headers::x_error)));
    CHECK(ir_reply.headers.at(std::string(ores::nats::headers::x_error)) == "forbidden");
    CHECK(f.ctrl->running_count() == 0);

    // IR-only token: the FX start must be rejected the same way.
    const auto ir_only_token = mint_token(
        f.db.tenant_id().to_string(), test_party, {"synthetic::ir_curve_generation_configs:read"});
    const auto fx_reply =
        f.nats.request_sync(test_start_subject,
                            ores::nats::default_wire_codec().encode(start_feed_request{
                                .config_id = boost::uuids::to_string(seeds.fx_enabled_id)}),
                            {{std::string(ores::nats::headers::authorization),
                              std::string(ores::nats::headers::bearer_prefix) + ir_only_token}});
    REQUIRE(fx_reply.headers.contains(std::string(ores::nats::headers::x_error)));
    CHECK(fx_reply.headers.at(std::string(ores::nats::headers::x_error)) == "forbidden");
    CHECK(f.ctrl->running_count() == 0);
}

TEST_CASE("feed_config_start_rejects_missing_children", tags) {
    config_fixture f;
    const auto test_party = resolve_test_party(f.db);
    const auto seeds = seed_configs(f.db, test_party);
    const auto token = mint_token(f.db.tenant_id().to_string(),
                                  test_party,
                                  {"synthetic::fx_spot_generation_configs:read",
                                   "synthetic::ir_curve_generation_configs:read"});

    const auto fx_no_comps = send_request<start_feed_request, start_feed_response>(
        f.nats,
        test_start_subject,
        token,
        start_feed_request{.config_id = boost::uuids::to_string(seeds.fx_no_components_id)});
    CHECK_FALSE(fx_no_comps.success);
    CHECK(fx_no_comps.message == "Feed config has no GMM components: " +
                                     boost::uuids::to_string(seeds.fx_no_components_id));

    const auto ir_no_entries = send_request<start_feed_request, start_feed_response>(
        f.nats,
        test_start_subject,
        token,
        start_feed_request{.config_id = boost::uuids::to_string(seeds.ir_no_entries_id)});
    CHECK_FALSE(ir_no_entries.success);
    CHECK(ir_no_entries.message == "Feed config has no Curve Template entries: " +
                                       boost::uuids::to_string(seeds.ir_no_entries_id));

    CHECK(f.ctrl->running_count() == 0);
}

TEST_CASE("feed_config_start_reports_qualifier_conflict", tags) {
    config_fixture f;
    const auto test_party = resolve_test_party(f.db);
    const auto seeds = seed_configs(f.db, test_party);
    const auto token = mint_token(f.db.tenant_id().to_string(),
                                  test_party,
                                  {"synthetic::fx_spot_generation_configs:read",
                                   "synthetic::ir_curve_generation_configs:read"});

    const auto first = send_request<start_feed_request, start_feed_response>(
        f.nats,
        test_start_subject,
        token,
        start_feed_request{.config_id = boost::uuids::to_string(seeds.fx_enabled_id)});
    REQUIRE(first.success);

    // A second FX config on the same EUR/USD pair: the uniform conflict
    // rule lets only one run, reported with the holding source_name.
    const auto second = send_request<start_feed_request, start_feed_response>(
        f.nats,
        test_start_subject,
        token,
        start_feed_request{.config_id = boost::uuids::to_string(seeds.fx_conflicting_id)});
    CHECK_FALSE(second.success);
    CHECK(second.message == "Already running as '" + seeds.fx_enabled_source +
                                "' — stop it first before starting '" +
                                seeds.fx_conflicting_source + "'.");
    CHECK(f.ctrl->running_count() == 1);
}

TEST_CASE("feed_config_list_requires_both_permissions", tags) {
    config_fixture f;
    const auto test_party = resolve_test_party(f.db);
    const auto token = mint_token(
        f.db.tenant_id().to_string(), test_party, {"synthetic::fx_spot_generation_configs:read"});

    // Every kind is listed, so the gate is the same uniform pair the folder
    // cascade requires.
    const auto reply =
        f.nats.request_sync(test_list_subject,
                            ores::nats::default_wire_codec().encode(list_feeds_request{}),
                            {{std::string(ores::nats::headers::authorization),
                              std::string(ores::nats::headers::bearer_prefix) + token}});
    REQUIRE(reply.headers.contains(std::string(ores::nats::headers::x_error)));
    CHECK(reply.headers.at(std::string(ores::nats::headers::x_error)) == "forbidden");
}
