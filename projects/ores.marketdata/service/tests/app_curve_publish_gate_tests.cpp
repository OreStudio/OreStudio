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
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote_key.hpp"
#include "ores.marketdata.core/repository/curve_publish_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/service/series_snapshot_reader.hpp"
#include "ores.marketdata.service/app/curve_publish_gate.hpp"
#include "ores.marketdata.service/app/curve_republish_service.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_config.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_pillar.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_pillar_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <algorithm>
#include <chrono>
#include <optional>
#include <stdexcept>
#include <string>
#include <tuple>

constexpr const char* tags = "[curve_publish]";

namespace {

using ores::inbox::service::approval_lifecycle;
using ores::marketdata::service::app::curve_publish_kind_code;
using ores::marketdata::service::app::publish_subject;
using ores::marketdata::service::app::raise_publish_request;
using ores::marketdata::service::app::record_publication;
using ores::marketdata::service::app::resolve_publish_subject;
using ores::marketdata::service::app::verdict_on_publish;

/// The instant the fixture's curve is bootstrapped against.
const auto as_of = std::chrono::sys_days{std::chrono::year{2026} / 3 / 2} + std::chrono::hours{12};

/**
 * @brief An account of the test tenant, and the context that acts as it.
 *
 * The database stamps every row with the account that wrote it, so a context
 * that names no account cannot raise a request or record a publication.
 */
struct person {
    ores::iam::domain::account account;
    ores::database::context ctx;
};

person seed_person(ores::testing::scoped_database_helper& h) {
    auto gctx = ores::testing::make_generation_context(h);
    auto a = ores::iam::generators::generate_synthetic_account(gctx);
    // A person's account written into the shared test tenant would end its
    // bootstrap, so the account is a service account, as the other approval
    // suites make theirs.
    a.account_type = "service";
    a.change_reason_code = "system.test";
    ores::iam::repository::account_repository{}.write(h.context(), a);
    return {.account = a, .ctx = h.context().with_tenant(h.tenant_id(), a.username)};
}

/**
 * @brief One curve a party can publish, with the market behind it.
 *
 * The curve is a two-pillar funding curve on a spot-starting convention whose
 * tenors the seeded convention resolves -- SPOT to 12M and 12M to 2Y -- so the
 * quote keys a feed would write are the keys the bootstrap reads, and the
 * fixture names them without knowing a single date.
 */
class curve_fixture {
public:
    curve_fixture()
        : gctx(ores::testing::make_generation_context(h))
        , party_id(uuid_gen())
        , config_id(uuid_gen())
        , output_series_id(uuid_gen()) {
        write_party();
        party = h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());
        write_series(output_series_id, output_series_uri());
        write_config();
        write_pillars();
        write_market();
    }

    /// @brief The subject the curve configuration states as of a given instant.
    publish_subject subject(std::chrono::system_clock::time_point at = as_of) const {
        return resolve_publish_subject(*party, boost::uuids::to_string(config_id), at);
    }

    /// @brief Every observation the published output series holds.
    std::size_t output_point_count() const {
        ores::marketdata::repository::market_observation_repository repo;
        return repo.read_latest_for_series(*party, output_series_id).size();
    }

    /// @brief Seeds a second curve of the same party, with its own output series.
    boost::uuids::uuid seed_second_curve() {
        const auto id = uuid_gen();
        const auto series_id = uuid_gen();
        write_series(series_id, output_series_uri("USD-SOFR-SECOND"));
        write_config(id, series_id);
        return id;
    }

    /// @brief The output series, as the resolver names it.
    ores::marketdata::messaging::resolve_series_identity_request identity() const {
        ores::marketdata::messaging::resolve_series_identity_request id;
        id.asset = "ir";
        id.scope = "USD";
        id.instrument_type = "discount";
        id.quote_type = "rate";
        id.fields.push_back({"curve_id", "USD-SOFR-FIXTURE"});
        return id;
    }

    /// @brief The snapshot a consumer reads, which states who published the set.
    ores::marketdata::messaging::get_curve_snapshot_response snapshot() const {
        ores::marketdata::messaging::get_curve_snapshot_request req;
        req.identity = identity();
        return ores::marketdata::service::series_snapshot_reader::read(*party, req, as_of);
    }

    /// @brief The published record of the output series, if any.
    std::optional<ores::marketdata::domain::curve_publish> published() const {
        ores::marketdata::repository::curve_publish_repository repo;
        return repo.read_latest_published_for_series(*party, output_series_id);
    }

    ores::testing::scoped_database_helper h;
    ores::utility::generation::generation_context gctx;
    boost::uuids::random_generator uuid_gen;

    boost::uuids::uuid party_id;
    boost::uuids::uuid config_id;
    boost::uuids::uuid output_series_id;

    /// The party's context: a curve, its quotes and its publish record are one party's.
    std::optional<ores::database::context> party;

private:
    static std::string output_series_uri(const std::string& curve_id = "USD-SOFR-FIXTURE") {
        return "oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=" + curve_id;
    }

    void write_party() {
        ores::refdata::repository::party_repository repo;
        auto p = ores::refdata::generators::generate_synthetic_party(gctx);
        p.id = party_id;
        p.version = 0;
        // The tenant already holds its root party, and the schema allows one. A
        // fixture party hangs beneath it, as every ordinary party does.
        p.is_registration_default = false;
        const auto existing = repo.read_latest(h.context());
        for (const auto& e : existing)
            if (e.tenant_id == h.tenant_id() && !e.parent_party_id) {
                p.parent_party_id = e.id;
                break;
            }
        p.change_reason_code = "system.test";
        p.change_commentary = "curve publish gate test";
        repo.write(h.context(), p);
    }

    void write_series(const boost::uuids::uuid& id, const std::string& uri) {
        ores::marketdata::domain::market_series s;
        s.id = id;
        s.version = 0;
        s.tenant_id = h.tenant_id();
        s.party_id = party_id;
        s.oresmd_uri = uri;
        s.series_subclass = "yield";
        s.provider_code = "ore";
        s.derivation_kind = "OBSERVED";
        s.derivation_config_id = boost::uuids::nil_uuid();
        s.derivation_config_version = 0;
        s.modified_by = h.db_user();
        s.performed_by = h.db_user();
        s.change_reason_code = "system.test";
        s.change_commentary = "curve publish gate test";
        ores::marketdata::repository::market_series_repository repo;
        repo.write(h.context(), s);
    }

    void write_config() { write_config(config_id, output_series_id); }

    void write_config(const boost::uuids::uuid& id, const boost::uuids::uuid& series) {
        ores::refdata::domain::ir_curve_bootstrap_config c;
        c.id = id;
        c.version = 0;
        c.tenant_id = h.tenant_id();
        c.party_id = party_id;
        c.currency_code = "USD";
        c.index_tenor = "1D";
        c.curve_family_role = "FUNDING";
        c.discount_curve_config_id = boost::uuids::nil_uuid();
        c.interpolation_method = "FLAT_FORWARD_THEN_LOG_LINEAR";
        c.day_count_convention = "A360";
        c.tenor_convention_code = "RATES_SPOT_FORWARD";
        c.split_tenor_code = "12M";
        c.output_series_id = series;
        c.modified_by = h.db_user();
        c.performed_by = h.db_user();
        c.change_reason_code = "system.test";
        c.change_commentary = "curve publish gate test";
        ores::refdata::repository::ir_curve_bootstrap_config_repository repo;
        repo.write(h.context(), c);
    }

    void write_pillars() {
        ores::refdata::repository::ir_curve_bootstrap_pillar_repository repo;
        int seq = 0;
        for (const auto& [start, end, role] :
             {std::tuple{"SPOT", "12M", "DEPOSIT"}, std::tuple{"12M", "2Y", "SWAP"}}) {
            ores::refdata::domain::ir_curve_bootstrap_pillar p;
            p.id = uuid_gen();
            p.version = 0;
            p.tenant_id = h.tenant_id();
            p.party_id = party_id;
            p.bootstrap_config_id = config_id;
            p.sequence_index = seq++;
            p.start_tenor_code = start;
            p.end_tenor_code = end;
            p.curve_role_code = role;
            p.modified_by = h.db_user();
            p.performed_by = h.db_user();
            p.change_reason_code = "system.test";
            p.change_commentary = "curve publish gate test";
            repo.write(h.context(), p);
        }
    }

    /// @brief The raw grid: one series per pillar, each quoted at the as-of.
    void write_market() {
        ores::marketdata::repository::market_observation_repository obs_repo;
        for (const auto& [start, end, value] :
             {std::tuple{"SPOT", "12M", "0.0432"}, std::tuple{"12M", "2Y", "0.0395"}}) {
            const auto key = ores::marketdata::core::make_pillar_quote_key("USD",
                                                                          "1D",
                                                                          start,
                                                                          std::chrono::year{2026} /
                                                                              1 / 1,
                                                                          std::chrono::year{2026} /
                                                                              3 / 2,
                                                                          end);
            const auto series_id = uuid_gen();
            write_series(series_id,
                         ores::marketdata::core::pillar_series_uri(key));

            ores::marketdata::domain::market_observation o;
            o.id = uuid_gen();
            o.tenant_id = h.tenant_id();
            o.party_id = party_id;
            o.series_id = series_id;
            o.observation_datetime = as_of;
            o.oresmd_uri = ores::marketdata::core::pillar_datum_uri(key);
            o.value = value;
            o.source = "curve publish gate test";
            obs_repo.insert(*party, {o});
        }
    }
};

}

TEST_CASE("a_publish_that_names_no_request_is_refused_and_states_the_kind",
          "[curve_publish][approval]") {
    ores::testing::scoped_database_helper h;
    const auto maker = seed_person(h);

    const auto verdict = verdict_on_publish(maker.ctx, "", publish_subject{});

    CHECK_FALSE(verdict.approved);
    CHECK(verdict.state_code.empty());
    CHECK(verdict.message.find(std::string(curve_publish_kind_code)) != std::string::npos);
}

TEST_CASE("a_raise_without_a_subject_is_refused", tags) {
    ores::testing::scoped_database_helper h;
    const auto maker = seed_person(h);

    // No curve named.
    CHECK_THROWS_AS(resolve_publish_subject(maker.ctx, "", as_of), std::invalid_argument);
    // No as-of named.
    CHECK_THROWS_AS(resolve_publish_subject(
                        maker.ctx,
                        boost::uuids::to_string(boost::uuids::random_generator{}()),
                        std::chrono::system_clock::time_point{}),
                    std::invalid_argument);
    // A curve the store does not hold.
    CHECK_THROWS_AS(resolve_publish_subject(
                        maker.ctx, boost::uuids::to_string(boost::uuids::random_generator{}()), as_of),
                    std::invalid_argument);
}

TEST_CASE("asking_for_a_publish_raises_the_kind_and_records_the_subject", tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto subject = f.subject();

    const auto raised = raise_publish_request(maker.ctx, subject, "The desk prices off this curve");

    CHECK(raised.kind_code == curve_publish_kind_code);
    CHECK(raised.state_code == "waiting");
    CHECK(raised.requested_by == maker.account.id);
    // The kind carries a review window, so a request states when it stops
    // waiting rather than staying in a queue nobody reads.
    CHECK(raised.expires_at.has_value());

    // The subject is the kind's detail, keyed by the request: the curve, the
    // instant and the party the request authorises, and nothing published yet.
    ores::marketdata::repository::curve_publish_repository repo;
    const auto record = repo.read_latest_by_request(maker.ctx, boost::uuids::to_string(raised.id));
    REQUIRE(record.has_value());
    CHECK(record->bootstrap_config_id == f.config_id);
    CHECK(record->output_series_id == f.output_series_id);
    CHECK(record->party_id == f.party_id);
    CHECK(record->as_of == as_of);
    CHECK(record->published_by.empty());
    CHECK_FALSE(record->published_at.has_value());

    // The publish waits: the request it names is not approved yet.
    const auto verdict = verdict_on_publish(maker.ctx, boost::uuids::to_string(raised.id), subject);
    CHECK_FALSE(verdict.approved);
    CHECK(verdict.state_code == "waiting");
}

TEST_CASE("the_market_risk_decider_sees_the_request_and_approving_it_permits_the_publish", tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto subject = f.subject();

    const auto raised = raise_publish_request(maker.ctx, subject, "Today's bootstrapped curve");
    const auto id = boost::uuids::to_string(raised.id);

    approval_lifecycle deciders_view(decider.ctx);
    const auto queue = deciders_view.queue({.kind_codes = {std::string(curve_publish_kind_code)}},
                                           decider.account.id,
                                           0,
                                           500);
    const auto listed =
        std::ranges::any_of(queue.requests, [&](const auto& r) { return r.id == raised.id; });
    CHECK(listed);

    const auto decided =
        deciders_view.decide(id, raised.version, "approve", decider.account.id, "");
    REQUIRE(decided.outcome == "ok");
    CHECK(decided.state_code == "approved");

    const auto verdict = verdict_on_publish(maker.ctx, id, subject);
    CHECK(verdict.approved);
    CHECK(verdict.state_code == "approved");
    CHECK(verdict.message.empty());
    REQUIRE(verdict.record.has_value());
    CHECK(verdict.record->bootstrap_config_id == f.config_id);
}

TEST_CASE("an_approved_subject_publishes_exactly_that_and_records_the_publisher", tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto subject = f.subject();

    const auto raised = raise_publish_request(maker.ctx, subject, "Today's bootstrapped curve");
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(approval_lifecycle(maker.ctx).decide(id, raised.version, "approve", decider.account.id, "")
                .state_code == "approved");

    const auto verdict = verdict_on_publish(maker.ctx, id, subject);
    REQUIRE(verdict.approved);
    REQUIRE(verdict.record.has_value());

    // The gate publishes the subject it checked, and only then records the act.
    ores::marketdata::service::app::curve_republish_service::republish(
        *f.party, subject.bootstrap_config_id, subject.as_of);
    CHECK(f.output_point_count() == 2);

    const auto published = record_publication(*f.party, *verdict.record, maker.account.username);
    CHECK(published.published_by == maker.account.username);
    REQUIRE(published.published_at.has_value());

    // The record is read back: who published it, when, and which request
    // authorised it.
    const auto read = f.published();
    REQUIRE(read.has_value());
    CHECK(read->request_id == raised.id);
    CHECK(read->published_by == maker.account.username);
    REQUIRE(read->published_at.has_value());
    CHECK(*read->published_at == *published.published_at);
    CHECK(read->as_of == as_of);
}

TEST_CASE("a_publish_of_another_curve_is_refused_and_names_both_subjects", tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto subject = f.subject();

    const auto raised = raise_publish_request(maker.ctx, subject, "Today's bootstrapped curve");
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(approval_lifecycle(maker.ctx).decide(id, raised.version, "approve", decider.account.id, "")
                .state_code == "approved");

    // The same person, approved for one curve, tries to publish another.
    const auto other_config = f.seed_second_curve();
    const auto other = resolve_publish_subject(*f.party, boost::uuids::to_string(other_config), as_of);
    const auto verdict = verdict_on_publish(maker.ctx, id, other);

    CHECK_FALSE(verdict.approved);
    // The refusal names the curve the request authorises and the curve asked for.
    CHECK(verdict.message.find("USD-SOFR-FIXTURE") != std::string::npos);
    CHECK(verdict.message.find("USD-SOFR-SECOND") != std::string::npos);
    CHECK(verdict.message.find("authorises") != std::string::npos);
}

TEST_CASE("a_publish_of_another_as_of_is_refused_and_names_both_instants", tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto subject = f.subject();

    const auto raised = raise_publish_request(maker.ctx, subject, "Today's bootstrapped curve");
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(approval_lifecycle(maker.ctx).decide(id, raised.version, "approve", decider.account.id, "")
                .state_code == "approved");

    auto other_as_of = subject;
    other_as_of.as_of = as_of + std::chrono::hours{24};
    const auto verdict = verdict_on_publish(maker.ctx, id, other_as_of);

    CHECK_FALSE(verdict.approved);
    CHECK(verdict.message.find("2026-03-02") != std::string::npos);
    CHECK(verdict.message.find("2026-03-03") != std::string::npos);
}

TEST_CASE("a_publish_of_another_party_is_refused", tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto subject = f.subject();

    const auto raised = raise_publish_request(maker.ctx, subject, "Today's bootstrapped curve");
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(approval_lifecycle(maker.ctx).decide(id, raised.version, "approve", decider.account.id, "")
                .state_code == "approved");

    // The party is the configuration's to say, so a call cannot state another
    // one: the attempt is made here against the gate directly, which is the only
    // path that could carry a party other than the configuration's.
    auto other_party = subject;
    other_party.party_id = boost::uuids::random_generator{}();
    const auto verdict = verdict_on_publish(maker.ctx, id, other_party);

    CHECK_FALSE(verdict.approved);
    CHECK(verdict.message.find("party") != std::string::npos);
}

TEST_CASE("a_request_that_already_published_is_refused_and_names_who_applied_it", tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto subject = f.subject();

    const auto raised = raise_publish_request(maker.ctx, subject, "Today's bootstrapped curve");
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(approval_lifecycle(maker.ctx).decide(id, raised.version, "approve", decider.account.id, "")
                .state_code == "approved");

    const auto first = verdict_on_publish(maker.ctx, id, subject);
    REQUIRE(first.approved);
    record_publication(*f.party, *first.record, maker.account.username);

    // The second attempt with the very same subject: one request publishes once.
    const auto second = verdict_on_publish(maker.ctx, id, subject);
    CHECK_FALSE(second.approved);
    CHECK(second.message.find("already published") != std::string::npos);
    CHECK(second.message.find(maker.account.username) != std::string::npos);
    CHECK(second.message.find("Raise a request for the next publish") != std::string::npos);
}

TEST_CASE("a_refused_publish_request_writes_nothing", tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto subject = f.subject();

    const auto raised = raise_publish_request(maker.ctx, subject, "A curve nobody checked");
    const auto id = boost::uuids::to_string(raised.id);
    const auto refused = approval_lifecycle(maker.ctx).decide(
        id, raised.version, "refuse", decider.account.id, "The fit is wrong");
    REQUIRE(refused.outcome == "ok");
    CHECK(refused.state_code == "refused");

    const auto verdict = verdict_on_publish(maker.ctx, id, subject);
    CHECK_FALSE(verdict.approved);
    CHECK(verdict.state_code == "refused");
    CHECK_FALSE(verdict.record.has_value());

    // A refusal writes neither the published set nor the record of one.
    CHECK(f.output_point_count() == 0);
    CHECK_FALSE(f.published().has_value());
}

TEST_CASE("the_read_that_serves_the_curve_states_the_publisher_and_the_time", tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const auto subject = f.subject();

    // Before anybody publishes, the read serves the observations and says so:
    // a set nobody approved is not presented as a published one.
    CHECK_FALSE(f.snapshot().published_at.has_value());
    CHECK(f.snapshot().published_by.empty());
    CHECK(f.snapshot().approval_request_id.empty());

    const auto raised = raise_publish_request(maker.ctx, subject, "Today's bootstrapped curve");
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(approval_lifecycle(maker.ctx)
                .decide(id, raised.version, "approve", decider.account.id, "")
                .state_code == "approved");

    const auto verdict = verdict_on_publish(maker.ctx, id, subject);
    REQUIRE(verdict.approved);
    ores::marketdata::service::app::curve_republish_service::republish(
        *f.party, subject.bootstrap_config_id, subject.as_of);
    const auto published = record_publication(*f.party, *verdict.record, maker.account.username);

    const auto snapshot = f.snapshot();
    REQUIRE(snapshot.success);
    CHECK(snapshot.published_by == maker.account.username);
    REQUIRE(snapshot.published_at.has_value());
    CHECK(*snapshot.published_at == *published.published_at);
    CHECK(snapshot.published_as_of == subject.as_of);
    CHECK(snapshot.approval_request_id == id);
}

TEST_CASE("an_approved_request_of_another_kind_does_not_permit_a_publish", tags) {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);

    approval_lifecycle lifecycle(maker.ctx);
    const auto kind = lifecycle.kind("iam.role_grant");
    REQUIRE(kind);
    const auto raised = lifecycle.raise(*kind, "Please", maker.account.id);
    const auto id = boost::uuids::to_string(raised.id);
    const auto approved = lifecycle.decide(id, raised.version, "approve", decider.account.id, "");
    REQUIRE(approved.state_code == "approved");

    const auto verdict = verdict_on_publish(maker.ctx, id, f.subject());
    CHECK_FALSE(verdict.approved);
    CHECK(verdict.state_code == "approved");
    CHECK(verdict.message.find("iam.role_grant") != std::string::npos);
}
