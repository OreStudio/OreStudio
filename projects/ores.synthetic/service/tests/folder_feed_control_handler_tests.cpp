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
#include "../src/feed_controller.hpp"
#include "../src/folder_feed_control_handler.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.marketdata.api/messaging/market_feed_config_protocol.hpp"
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
#include "ores.synthetic.api/domain/folder.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value.hpp"
#include "ores.synthetic.api/domain/ir_curve_template_entry.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.core/repository/folder_repository.hpp"
#include "ores.synthetic.core/repository/fx_spot_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_process_parameter_value_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/ir_curve_template_entry_repository.hpp"
#include "ores.synthetic.core/repository/market_data_generation_config_repository.hpp"
#include "ores.synthetic.core/repository/yield_curve_process_parameter_definition_repository.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

// Proves the folder cascade end to end: a start_feeds_under_folder request
// over real NATS walks the seeded subtree and starts every feed kind under
// it through the factory -- fx_spot and ir_curve alike -- reporting per-kind
// counts, already-running on repeat, skipped for disabled rows, and stop
// counts. The handler resolves everything server-side from the JWT's
// tenant/party claims (RLS), so the test seeds under an isolated per-run
// tenant exactly like the provisioner's do.

namespace {

const std::string tags("[folder_feed_control_handler][integration]");

const std::string test_secret("folder-cascade-test-secret");
const std::string test_issuer("ores.iam.test");
const std::string test_audience("ores.synthetic.service");

// Test-scoped subjects: the running service subscribes to the real
// start/stop_folder subjects on the same queue group, so a request sent
// on them can be answered by the service's own verifier (a different
// secret/algorithm) instead of the test's, and the reply fails to
// decode. The handler is subject-agnostic -- it only logs the subject
// and replies on the request's reply subject -- so the whole handler
// runs against these test subjects unchanged.
const std::string test_start_subject("ores.test.folder_feed_control.start_folder");
const std::string test_stop_subject("ores.test.folder_feed_control.stop_folder");

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

// Mints an HS256 token the handler's verifier accepts, scoped to the test
// tenant and party with the given roles.
std::string mint_token(const std::string& tenant_id,
                       const boost::uuids::uuid& party_id,
                       const std::vector<std::string>& roles) {
    auto signer = ores::security::jwt::jwt_authenticator::create_hs256(
        test_secret, test_issuer, test_audience);
    auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
    claims.subject = "folder-cascade-test";
    claims.issuer = test_issuer;
    claims.audience = test_audience;
    claims.tenant_id = tenant_id;
    claims.party_id = boost::uuids::to_string(party_id);
    claims.visible_party_ids = {boost::uuids::to_string(party_id)};
    claims.roles = roles;
    return *signer.create_token(claims);
}

// Sends one folder request on the given subject with the Bearer header
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

struct seeded_cascade {
    boost::uuids::uuid root_folder_id;
};

// Seeds a root folder with one child collection holding one enabled FX
// config (with its GMM component), one disabled FX config, one conflicting
// enabled FX config (the same EUR/USD pair as the first — the uniform
// conflict rule lets only one of the two run; the cascade skips the
// second), and one enabled IR curve config (with its template entry and
// VASICEK parameter values).
seeded_cascade seed_cascade(ores::testing::scoped_database_helper& h,
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

    dom::folder root;
    root.tenant_id = h.tenant_id();
    root.id = uuid();
    root.party_id = test_party;
    root.parent_id = std::nullopt;
    root.kind = "root";
    root.name = "Cascade Test Root";
    root.modified_by = db_user;
    root.performed_by = db_user;
    root.change_reason_code = "system.test";
    root.change_commentary = "folder cascade test seed";
    root.recorded_at = now;

    dom::folder child;
    child.tenant_id = h.tenant_id();
    child.id = uuid();
    child.party_id = test_party;
    child.parent_id = root.id;
    child.kind = "collection";
    child.name = "Cascade Test Collection";
    child.modified_by = db_user;
    child.performed_by = db_user;
    child.change_reason_code = "system.test";
    child.change_commentary = "folder cascade test seed";
    child.recorded_at = now;

    repo::folder_repository().write(party_ctx, root);
    repo::folder_repository().write(party_ctx, child);

    dom::market_data_generation_config container;
    container.tenant_id = h.tenant_id();
    container.id = uuid();
    container.party_id = test_party;
    container.scope = scope::party;
    container.binding_mode = binding_mode::bound;
    container.enabled = true;
    container.name = "Cascade Test Container";
    container.modified_by = db_user;
    container.performed_by = db_user;
    container.change_reason_code = "system.test";
    container.change_commentary = "folder cascade test seed";
    container.recorded_at = now;
    repo::market_data_generation_config_repository().write(party_ctx, container);

    dom::fx_spot_generation_config fx_enabled;
    fx_enabled.tenant_id = h.tenant_id();
    fx_enabled.id = uuid();
    fx_enabled.party_id = test_party;
    fx_enabled.config_id = container.id;
    fx_enabled.folder_id = child.id;
    fx_enabled.base_currency_code = "EUR";
    fx_enabled.quote_currency_code = "USD";
    fx_enabled.source_name = "cascade.test.fx.eur_usd";
    fx_enabled.ore_key = "FX/RATE/EUR/USD";
    fx_enabled.price_source = "fixed";
    fx_enabled.gmm_initial_price = 1.08;
    fx_enabled.ticks_per_hour = 12;
    fx_enabled.process_type = "geometric";
    fx_enabled.enabled = true;
    // An enabled config under an enabled container is auto-start eligible,
    // like every published FX row.
    fx_enabled.auto_start = true;
    fx_enabled.vintage_source = "";
    fx_enabled.vintage_date = "";
    fx_enabled.modified_by = db_user;
    fx_enabled.performed_by = db_user;
    fx_enabled.change_reason_code = "system.test";
    fx_enabled.change_commentary = "folder cascade test seed";
    fx_enabled.recorded_at = now;

    // The disabled row has the same CHECK-valid shape, so the only
    // difference is the enabled flag the cascade gates on.
    dom::fx_spot_generation_config fx_disabled = fx_enabled;
    fx_disabled.id = uuid();
    fx_disabled.base_currency_code = "GBP";
    fx_disabled.quote_currency_code = "USD";
    fx_disabled.source_name = "cascade.test.fx.gbp_usd";
    fx_disabled.enabled = false;

    // A second enabled config on the same EUR/USD pair: whichever of the
    // two the cascade reaches first starts; the other is conflict-skipped,
    // so the per-kind counts stay kind-stable regardless of read order. It
    // needs its own container — the schema forbids two FX configs on the
    // same pair under the same one.
    dom::market_data_generation_config conflict_container = container;
    conflict_container.id = uuid();
    conflict_container.name = "Cascade Test Conflict Container";
    repo::market_data_generation_config_repository().write(party_ctx, conflict_container);

    dom::fx_spot_generation_config fx_conflicting = fx_enabled;
    fx_conflicting.id = uuid();
    fx_conflicting.config_id = conflict_container.id;
    fx_conflicting.source_name = "cascade.test.fx.eur_usd_conflict";

    repo::fx_spot_generation_config_repository().write(party_ctx, fx_enabled);
    repo::fx_spot_generation_config_repository().write(party_ctx, fx_disabled);
    repo::fx_spot_generation_config_repository().write(party_ctx, fx_conflicting);

    dom::gmm_component comp;
    comp.tenant_id = h.tenant_id();
    comp.id = uuid();
    comp.party_id = test_party;
    comp.fx_spot_config_id = fx_enabled.id;
    comp.component_index = 0;
    comp.mean = 0.0001;
    comp.stdev = 0.001;
    comp.weight = 1.0;
    comp.description = "cascade test component";
    comp.modified_by = db_user;
    comp.performed_by = db_user;
    comp.change_reason_code = "system.test";
    comp.change_commentary = "folder cascade test seed";
    comp.recorded_at = now;
    repo::gmm_component_repository().write(party_ctx, comp);

    dom::gmm_component conflict_comp = comp;
    conflict_comp.id = uuid();
    conflict_comp.fx_spot_config_id = fx_conflicting.id;
    conflict_comp.description = "cascade test conflicting component";
    repo::gmm_component_repository().write(party_ctx, conflict_comp);

    dom::ir_curve_generation_config ir;
    ir.tenant_id = h.tenant_id();
    ir.id = uuid();
    ir.party_id = test_party;
    ir.config_id = container.id;
    ir.folder_id = child.id;
    ir.currency_code = "USD";
    ir.index_family = "sofr";
    ir.tenor = "";
    ir.role = "self_discounting";
    ir.process_type = "VASICEK";
    ir.ticks_per_hour = 12;
    ir.enabled = true;
    ir.auto_start = false;
    ir.price_source = "fixed";
    ir.vintage_source = "";
    ir.vintage_date = "";
    ir.description = "cascade test IR config";
    ir.fixed_leg_payment_frequency_code = "Annual";
    ir.source_name = "cascade.test.ir.usd_sofr";
    ir.modified_by = db_user;
    ir.performed_by = db_user;
    ir.change_reason_code = "system.test";
    ir.change_commentary = "folder cascade test seed";
    ir.recorded_at = now;
    repo::ir_curve_generation_config_repository().write(party_ctx, ir);

    dom::ir_curve_template_entry entry;
    entry.tenant_id = h.tenant_id();
    entry.id = uuid();
    entry.party_id = test_party;
    entry.ir_curve_config_id = ir.id;
    entry.sequence_index = 0;
    entry.start_tenor_code = "SPOT";
    entry.end_tenor_code = "1M";
    entry.instrument_code = "DEPO";
    entry.modified_by = db_user;
    entry.performed_by = db_user;
    entry.change_reason_code = "system.test";
    entry.change_commentary = "folder cascade test seed";
    entry.recorded_at = now;
    repo::ir_curve_template_entry_repository().write(party_ctx, entry);

    const std::map<std::string, double> value_by_name = {
        {"kappa", 0.05}, {"theta", 0.03}, {"sigma", 0.01}, {"initial_rate", 0.025}};
    for (const auto& d : definitions) {
        dom::ir_curve_generation_config_process_parameter_value v;
        v.tenant_id = h.tenant_id();
        v.id = uuid();
        v.config_id = ir.id;
        v.parameter_definition_id = d.id;
        v.parameter_value = value_by_name.at(d.parameter_name);
        v.modified_by = db_user;
        v.performed_by = db_user;
        v.change_reason_code = "system.test";
        v.change_commentary = "folder cascade test seed";
        v.recorded_at = now;
        repo::ir_curve_generation_config_process_parameter_value_repository().write(party_ctx, v);
    }

    return {.root_folder_id = root.id};
}

// The handler under test runs on the NATS worker thread, exactly as the
// registrar runs it in the service: one instance per inbound message, with
// the real controllers and the real database context.
struct cascade_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::nats::service::nats_client auth_nats;
    std::shared_ptr<ores::synthetic::service::feed_controller> ctrl;
    std::optional<ores::security::jwt::jwt_authenticator> verifier;
    std::optional<ores::nats::service::subscription> start_sub;
    std::optional<ores::nats::service::subscription> stop_sub;

    cascade_fixture()
        : nats(ores::testing::make_nats_options())
        , auth_nats(nats, [](bool) { return std::string(); }) {
        nats.connect();
        REQUIRE(nats.is_connected());

        ctrl = std::make_shared<ores::synthetic::service::feed_controller>(nats, auth_nats);
        verifier = ores::security::jwt::jwt_authenticator::create_hs256(
            test_secret, test_issuer, test_audience);

        using ores::marketdata::messaging::start_feeds_under_folder_request;
        using ores::marketdata::messaging::stop_feeds_under_folder_request;
        start_sub = nats.queue_subscribe(
            test_start_subject, "ores.synthetic.service", [this](ores::nats::message msg) {
                ores::synthetic::service::folder_feed_control_handler h(
                    nats, ctrl, auth_nats, db.context(), verifier);
                h.start(std::move(msg));
            });
        stop_sub = nats.queue_subscribe(
            test_stop_subject, "ores.synthetic.service", [this](ores::nats::message msg) {
                ores::synthetic::service::folder_feed_control_handler h(
                    nats, ctrl, auth_nats, db.context(), verifier);
                h.stop(std::move(msg));
            });
    }
};

}

using namespace ores::marketdata::messaging;

TEST_CASE("folder_cascade_starts_and_stops_both_kinds_with_per_kind_counts", tags) {
    cascade_fixture f;
    const auto test_party = resolve_test_party(f.db);
    const auto seeds = seed_cascade(f.db, test_party);
    const auto token = mint_token(f.db.tenant_id().to_string(),
                                  test_party,
                                  {"synthetic::fx_spot_generation_configs:read",
                                   "synthetic::ir_curve_generation_configs:read"});
    const auto folder = boost::uuids::to_string(seeds.root_folder_id);
    const std::string fx_kind(ores::synthetic::feed::fx_spot_feed_kind);
    const std::string ir_kind(ores::synthetic::feed::ir_curve_feed_kind);

    // First pass: one of the two EUR/USD FX configs starts (the uniform
    // conflict rule lets only one run — the second is skipped, proving the
    // qualifier_conflict rejection end to end), the disabled FX row is
    // skipped, and the IR feed starts. Per-kind counts split the aggregate.
    const auto start1 =
        send_request<start_feeds_under_folder_request, start_feeds_under_folder_response>(
            f.nats,
            test_start_subject,
            token,
            start_feeds_under_folder_request{.folder_id = folder});
    REQUIRE(start1.success);
    CHECK(start1.started == 2);
    CHECK(start1.already_running == 0);
    CHECK(start1.skipped == 2);
    REQUIRE(start1.by_kind.contains(fx_kind));
    REQUIRE(start1.by_kind.contains(ir_kind));
    CHECK(start1.by_kind.at(fx_kind).started == 1);
    CHECK(start1.by_kind.at(fx_kind).already_running == 0);
    CHECK(start1.by_kind.at(fx_kind).skipped == 2);
    CHECK(start1.by_kind.at(ir_kind).started == 1);
    CHECK(start1.by_kind.at(ir_kind).already_running == 0);
    CHECK(start1.by_kind.at(ir_kind).skipped == 0);
    CHECK(f.ctrl->running_count() == 2);

    // The collapsed controller lists every kind; the per-kind filter must
    // scope it (which of the two EUR/USD sources runs depends on read order,
    // so the exact source_name is not asserted).
    CHECK(f.ctrl->list().size() == 2);
    CHECK(f.ctrl->list(fx_kind).size() == 1);
    CHECK(f.ctrl->list(ir_kind).size() == 1);

    // Repeat: both are already running; the disabled and conflicting rows
    // are still skipped.
    const auto start2 =
        send_request<start_feeds_under_folder_request, start_feeds_under_folder_response>(
            f.nats,
            test_start_subject,
            token,
            start_feeds_under_folder_request{.folder_id = folder});
    REQUIRE(start2.success);
    CHECK(start2.started == 0);
    CHECK(start2.already_running == 2);
    CHECK(start2.skipped == 2);
    CHECK(start2.by_kind.at(fx_kind).started == 0);
    CHECK(start2.by_kind.at(fx_kind).already_running == 1);
    CHECK(start2.by_kind.at(fx_kind).skipped == 2);
    CHECK(start2.by_kind.at(ir_kind).started == 0);
    CHECK(start2.by_kind.at(ir_kind).already_running == 1);
    CHECK(start2.by_kind.at(ir_kind).skipped == 0);

    // Stop: both stop, split per kind.
    const auto stop =
        send_request<stop_feeds_under_folder_request, stop_feeds_under_folder_response>(
            f.nats, test_stop_subject, token, stop_feeds_under_folder_request{.folder_id = folder});
    REQUIRE(stop.success);
    CHECK(stop.stopped == 2);
    REQUIRE(stop.stopped_by_kind.contains(fx_kind));
    REQUIRE(stop.stopped_by_kind.contains(ir_kind));
    CHECK(stop.stopped_by_kind.at(fx_kind) == 1);
    CHECK(stop.stopped_by_kind.at(ir_kind) == 1);
    CHECK(f.ctrl->running_count() == 0);
}

TEST_CASE("folder_cascade_rejects_without_the_ir_curve_permission", tags) {
    cascade_fixture f;
    const auto test_party = resolve_test_party(f.db);
    const auto seeds = seed_cascade(f.db, test_party);
    const auto token = mint_token(
        f.db.tenant_id().to_string(), test_party, {"synthetic::fx_spot_generation_configs:read"});

    // The request is well-formed; only the missing IR permission can stop
    // it, so the reply carries X-Error: forbidden and no feed starts.
    const auto reply = f.nats.request_sync(
        test_start_subject,
        ores::nats::default_wire_codec().encode(start_feeds_under_folder_request{
            .folder_id = boost::uuids::to_string(seeds.root_folder_id)}),
        {{std::string(ores::nats::headers::authorization),
          std::string(ores::nats::headers::bearer_prefix) + token}});
    REQUIRE(reply.headers.contains(std::string(ores::nats::headers::x_error)));
    CHECK(reply.headers.at(std::string(ores::nats::headers::x_error)) == "forbidden");
    CHECK(f.ctrl->running_count() == 0);
}

TEST_CASE("folder_cascade_rejects_a_malformed_folder_id", tags) {
    cascade_fixture f;
    const auto test_party = resolve_test_party(f.db);
    const auto token = mint_token(f.db.tenant_id().to_string(),
                                  test_party,
                                  {"synthetic::fx_spot_generation_configs:read",
                                   "synthetic::ir_curve_generation_configs:read"});

    const auto resp =
        send_request<start_feeds_under_folder_request, start_feeds_under_folder_response>(
            f.nats,
            test_start_subject,
            token,
            start_feeds_under_folder_request{.folder_id = "not-a-uuid"});
    CHECK_FALSE(resp.success);
}
