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
#include "curve_publish_fixture.hpp"
#include "curve_publish_steps.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_party_repository.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.api/generators/approval_request_generator.hpp"
#include "ores.inbox.api/messaging/notification_operations_protocol.hpp"
#include "ores.inbox.core/repository/approval_kind_repository.hpp"
#include "ores.inbox.core/repository/approval_request_repository.hpp"
#include "ores.inbox.core/repository/approval_request_state_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.inbox.core/service/notification_center.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/generators/fx_forward_publish_generator.hpp"
#include "ores.marketdata.core/repository/fx_forward_publish_repository.hpp"
#include "ores.marketdata.service/app/curve_notices.hpp"
#include "ores.marketdata.service/app/curve_republish_service.hpp"
#include "ores.marketdata.service/app/curve_withdrawal.hpp"
#include "ores.marketdata.service/app/publication_kind.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

/**
 * @file app_curve_notices_tests.cpp
 * @brief The publish notice: one per apply, at the apply, and its own on a
 * withdrawal.
 *
 * The notice names the subject and the act and never the values, so these tests
 * read the arguments the notice carries and the route it points at. A refused
 * publish moves nothing, so it tells nobody.
 */

namespace {

using namespace ores::logging;

using ores::inbox::service::approval_lifecycle;
using ores::inbox::service::notification_center;
using ores::marketdata::service::app::announce_withdrawal;
using ores::marketdata::service::tests::record_publication;
using ores::marketdata::service::tests::verdict_on_publish;
using ores::marketdata::service::tests::curve_fixture;
using ores::marketdata::service::tests::seed_person;

auto& notices_test_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.service.tests.app_curve_notices");
    return instance;
}

/**
 * @brief A person of the fixture's party, whose bell the notice lands on.
 *
 * The fixture's own accounts are service accounts and hold no party, and a
 * notice is addressed to the people who read the curve: the accounts the party
 * subscribes through. So a reader is a person account associated with the party.
 *
 * The account is closed when the test ends. The store lets any actor write while
 * a tenant has no person in it yet -- the bootstrap case -- and the suites that
 * share the test tenant rely on that; a person left behind would close it for
 * every one of them.
 */
class reader_account {
public:
    explicit reader_account(curve_fixture& f)
        : ctx_(f.h.context())
        , account_(seed(f)) {
        ores::iam::domain::account_party link;
        link.tenant_id = f.h.tenant_id().to_string();
        link.account_id = account_.id;
        link.party_id = f.party_id;
        link.modified_by = f.h.db_user();
        link.performed_by = f.h.db_user();
        link.change_reason_code = "system.test";
        ores::iam::repository::account_party_repository links(f.h.context());
        links.write(link);
    }

    ~reader_account() {
        for (const auto* statement :
             {"delete from ores_iam_account_parties_tbl where account_id = $1::uuid",
              "delete from ores_iam_accounts_tbl where id = $1::uuid"}) {
            try {
                ores::database::query_builder::execute_parameterized_string_query(
                    ctx_,
                    statement,
                    {boost::uuids::to_string(account_.id)},
                    notices_test_lg(),
                    "Closing the notice test's reader account");
            } catch (const std::exception& e) {
                ORES_LOG_SEV(notices_test_lg(), warn)
                    << "The notice test's reader account was left behind: " << e.what();
            }
        }
    }

    reader_account(const reader_account&) = delete;
    reader_account& operator=(const reader_account&) = delete;

    [[nodiscard]] const ores::iam::domain::account& account() const {
        return account_;
    }

private:
    static ores::iam::domain::account seed(curve_fixture& f) {
        auto gctx = ores::testing::make_generation_context(f.h);
        auto a = ores::iam::generators::generate_synthetic_account(gctx);
        a.account_type = "user";
        a.change_reason_code = "system.test";
        ores::iam::repository::account_repository{}.write(f.h.context(), a);
        return a;
    }

    ores::database::context ctx_;
    ores::iam::domain::account account_;
};

/// @brief The notices one person was given, newest first.
std::vector<ores::inbox::messaging::inbox_notification>
notices_of(curve_fixture& f, const ores::iam::domain::account& reader) {
    notification_center center(*f.party);
    return center.mine(reader.id, false, 0, 100).notifications;
}

const std::string* argument_of(const ores::inbox::messaging::inbox_notification& n,
                               const std::string& name) {
    const auto found =
        std::ranges::find_if(n.arguments, [&](const auto& a) { return a.name == name; });
    return found == n.arguments.end() ? nullptr : &found->value;
}

/**
 * @brief Applies the approved request as the handler does, minus NATS.
 *
 * The handler's own path is the gate, the republish, the record and the notice;
 * this states the same four steps so the notice is the one a publish raises.
 *
 * The notice is raised as the person who applied the set, because it names who
 * moved it: the service's own account is in another tenant and holds no bell, so
 * the asker's context is the one the notice is written under.
 */
void apply(const ores::database::context& publishing,
           const ores::database::context& as_the_publisher,
           curve_fixture& f,
           const std::string& request_id,
           ores::marketdata::domain::curve_publish& published) {
    const auto subject = f.subject();
    const auto verdict = verdict_on_publish(publishing, request_id, subject);
    REQUIRE(verdict.approved);
    const auto built = ores::marketdata::service::app::curve_republish_service::republish(
        *f.party, subject.bootstrap_config_id, subject.as_of);
    REQUIRE(built.has_value());
    // The record's own write raises the notice, once, so this is the whole of
    // what a publish tells.
    published = record_publication(as_the_publisher, *verdict.record, as_the_publisher.actor());
}

}

TEST_CASE("a_publish_sends_one_notice_that_names_the_subject_and_the_act", "[curve_notices]") {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const reader_account reader(f);

    const auto raised = raise_publish_request(maker.ctx, f.subject(), "Today's bootstrapped curve");
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(approval_lifecycle(*f.party)
                .decide(id, raised.version, "approve", decider.account.id, "")
                .state_code == "approved");

    ores::marketdata::domain::curve_publish published;
    apply(*f.party, maker.ctx, f, id, published);

    const auto notices = notices_of(f, reader.account());
    const auto published_ones = std::ranges::count_if(
        notices, [](const auto& n) { return n.kind_code == "marketdata.curve_published"; });
    CHECK(published_ones == 1);

    const auto found = std::ranges::find_if(
        notices, [](const auto& n) { return n.kind_code == "marketdata.curve_published"; });
    REQUIRE(found != notices.end());
    // The message key is the server's dotted path, so the screen asks for the key
    // it was sent rather than mapping it.
    CHECK(found->message_key == "notification.marketdata.curve_published");

    // The subject and the act, never the values.
    REQUIRE(argument_of(*found, "curve") != nullptr);
    CHECK(argument_of(*found, "curve")->find("oresmd://ir/USD") != std::string::npos);
    REQUIRE(argument_of(*found, "party") != nullptr);
    ores::refdata::repository::party_repository parties;
    const auto party = parties.read_latest(*f.party, boost::uuids::to_string(f.party_id));
    REQUIRE_FALSE(party.empty());
    CHECK(*argument_of(*found, "party") == party.front().short_code);
    REQUIRE(argument_of(*found, "publisher") != nullptr);
    CHECK(*argument_of(*found, "publisher") == maker.account.username);
    REQUIRE(argument_of(*found, "request") != nullptr);
    CHECK(*argument_of(*found, "request") == id);
    REQUIRE(argument_of(*found, "as_of") != nullptr);
    CHECK(argument_of(*found, "as_of")->starts_with("2026-03-02"));
    // The first publish replaced nothing, and the notice says so by carrying an
    // empty previous as-of rather than by carrying a set of values.
    REQUIRE(argument_of(*found, "previous_as_of") != nullptr);
    CHECK(argument_of(*found, "previous_as_of")->empty());
    CHECK(argument_of(*found, "value") == nullptr);
    CHECK(argument_of(*found, "discount_factor") == nullptr);

    // The link opens the published curve's record page, named by the curve.
    CHECK(found->link_route.starts_with("/") == false);
    CHECK(found->link_route.starts_with("market-data/published-curves/"));
}

TEST_CASE("a_withdrawal_sends_its_own_notice_with_the_reason", "[curve_notices]") {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const reader_account reader(f);

    const auto raised = raise_publish_request(maker.ctx, f.subject(), "Today's bootstrapped curve");
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(approval_lifecycle(*f.party)
                .decide(id, raised.version, "approve", decider.account.id, "")
                .state_code == "approved");
    ores::marketdata::domain::curve_publish published;
    apply(*f.party, maker.ctx, f, id, published);

    ores::marketdata::messaging::withdraw_curve_request request;
    request.publish_id = boost::uuids::to_string(published.id);
    request.reason_code = "common.stale_data";
    request.comment = "The 2Y pillar was stale.";
    const auto withdrawn = ores::marketdata::service::app::withdraw_curve(maker.ctx, request);
    REQUIRE(withdrawn.success);

    const auto notices = notices_of(f, reader.account());
    const auto withdrawal = std::ranges::find_if(
        notices, [](const auto& n) { return n.kind_code == "marketdata.curve_withdrawn"; });
    REQUIRE(withdrawal != notices.end());
    CHECK(withdrawal->message_key == "notification.marketdata.curve_withdrawn");

    // The reason is in words on the notice, because a withdrawn curve is a change
    // a reader must act on.
    REQUIRE(argument_of(*withdrawal, "reason") != nullptr);
    CHECK(*argument_of(*withdrawal, "reason") == "common.stale_data");
    REQUIRE(argument_of(*withdrawal, "withdrawn_by") != nullptr);
    CHECK(*argument_of(*withdrawal, "withdrawn_by") == maker.account.username);
    REQUIRE(argument_of(*withdrawal, "request") != nullptr);
    CHECK(*argument_of(*withdrawal, "request") == id);

    // The publish's own notice stands beside it: the two acts are two notices.
    CHECK(1 == std::ranges::count_if(notices, [](const auto& n) {
              return n.kind_code == "marketdata.curve_published";
          }));
}

TEST_CASE("a_refused_publish_sends_nothing", "[curve_notices]") {
    curve_fixture f;
    const auto maker = seed_person(f.h);
    const auto decider = seed_person(f.h);
    const reader_account reader(f);

    const auto raised = raise_publish_request(maker.ctx, f.subject(), "A curve nobody checked");
    const auto id = boost::uuids::to_string(raised.id);
    REQUIRE(approval_lifecycle(*f.party)
                .decide(id, raised.version, "refuse", decider.account.id, "The fit is wrong")
                .state_code == "refused");

    // A refused publish moves nothing, so no notice is raised: the gate refuses
    // and the announcer is never reached.
    const auto verdict = verdict_on_publish(maker.ctx, id, f.subject());
    CHECK_FALSE(verdict.approved);

    CHECK(notices_of(f, reader.account()).empty());
    CHECK(f.output_point_count() == 0);
    CHECK_FALSE(f.published().has_value());
}

namespace {

/**
 * @brief An approved request row for a forward set to hang on.
 *
 * A forward publish names its request, and the store refuses a request id it
 * does not hold, so each row under test needs one. The kind and the state are
 * the store's own, read rather than invented.
 */
boost::uuids::uuid seed_forward_request(curve_fixture& f,
                                        const ores::database::context& system_ctx) {
    auto generation = ores::testing::make_generation_context(f.h);
    auto asker = ores::iam::generators::generate_synthetic_account(generation);
    asker.account_type = "service";
    asker.change_reason_code = "system.test";
    ores::iam::repository::account_repository{}.write(f.h.context(), asker);

    auto request = ores::inbox::generators::generate_synthetic_approval_request(generation);
    request.change_reason_code = "system.test";
    request.requested_by = asker.id;
    const auto kinds = ores::inbox::repository::approval_kind_repository{}.read_latest(system_ctx);
    REQUIRE_FALSE(kinds.empty());
    request.kind_code = kinds.front().code;
    const auto states =
        ores::inbox::repository::approval_request_state_repository{}.read_latest(system_ctx);
    REQUIRE_FALSE(states.empty());
    request.state_code = states.front().code;
    ores::inbox::repository::approval_request_repository{}.write(f.h.context(), request);
    return request.id;
}

}

TEST_CASE("a_second_forward_publish_supersedes_the_first_and_tells_its_reader", "[curve_notices]") {
    using ores::marketdata::service::app::fx_forward_publication_kind;
    using ores::marketdata::service::app::publication_facts;

    curve_fixture f;
    const reader_account reader(f);
    const auto system_ctx =
        f.h.context().with_tenant(ores::utility::uuid::tenant_id::system(), f.h.db_user());
    auto generation = ores::testing::make_generation_context(f.h);
    const auto pair = std::string("EUR/USD");
    const auto first_as_of =
        std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now()) -
        std::chrono::hours{24};
    const auto second_as_of = first_as_of + std::chrono::hours{24};
    const auto published_at =
        std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
    // The store checks every account a row names, so the publisher is the
    // reader's own account: a name the store does not hold is refused at the
    // write, not at the read.
    const auto publisher = reader.account().username;
    // The notice names the account that acted, and the store refuses a name it
    // does not hold, so the apply runs as the reader's account in the party's
    // own context: the same party the fixture publishes under, with an actor.
    const auto publishing =
        f.h.context().with_party(f.h.tenant_id(), f.party_id, {f.party_id}, publisher);

    // The set in use, as the first apply left it, and the candidate the second
    // request authorises.
    auto first = ores::marketdata::generators::generate_synthetic_fx_forward_publish(generation);
    first.request_id = seed_forward_request(f, system_ctx);
    first.party_id = f.party_id;
    first.pair_code = pair;
    first.as_of = first_as_of;
    first.published_at = first_as_of;
    first.published_by = publisher;
    first.modified_by = publisher;
    first.performed_by = publisher;
    first.change_reason_code = "system.test";

    auto second = ores::marketdata::generators::generate_synthetic_fx_forward_publish(generation);
    second.request_id = seed_forward_request(f, system_ctx);
    second.party_id = f.party_id;
    second.pair_code = pair;
    second.as_of = second_as_of;
    second.modified_by = publisher;
    second.performed_by = publisher;
    second.change_reason_code = "system.test";

    ores::marketdata::repository::fx_forward_publish_repository repo;
    repo.write(publishing, first);
    repo.write(publishing, second);

    publication_facts facts;
    facts.subject.kind = ores::marketdata::service::app::derived_kind::fx_forward;
    facts.subject.party_id = f.party_id;
    facts.subject.subject_key = pair;
    facts.subject.as_of = second_as_of;
    facts.publish_id = second.id;
    facts.request_id = second.request_id;
    facts.version = second.version;
    facts.published_at = published_at;
    facts.published_by = publisher;
    {
        ores::database::repository::unit_of_work uow(publishing);
        const auto& tx = uow.ctx();
        facts = fx_forward_publication_kind().publish(tx, facts);
        fx_forward_publication_kind().announce_published(tx, facts);
        uow.commit();
    }

    // The pair carries one set in use, and the set that left use names the one
    // that took its place.
    const auto ended = repo.read_latest(*f.party, boost::uuids::to_string(first.id));
    REQUIRE_FALSE(ended.empty());
    CHECK(ended.front().superseded_at == published_at);
    CHECK(ended.front().superseded_by_request_id == second.request_id);
    CHECK(ended.front().superseded_as_of == second_as_of);
    CHECK(facts.replaced_as_of == first_as_of);

    // The successor is the live set of the pair, and it superseded nothing else.
    const auto live = repo.read_published_for_pair(*f.party, f.party_id, pair);
    const auto in_use =
        std::ranges::find_if(live, [](const auto& s) { return !s.superseded_at.has_value(); });
    REQUIRE(in_use != live.end());
    CHECK(in_use->id == second.id);

    // The reader is told once, in the words the kind states.
    const auto notices = notices_of(f, reader.account());
    const auto found = std::ranges::find_if(
        notices, [](const auto& n) { return n.kind_code == "marketdata.fx_forward_published"; });
    REQUIRE(found != notices.end());
    CHECK(found->message_key == "notification.marketdata.fx_forward_published");
    REQUIRE(argument_of(*found, "pair") != nullptr);
    CHECK(*argument_of(*found, "pair") == pair);
    REQUIRE(argument_of(*found, "request") != nullptr);
    CHECK(*argument_of(*found, "request") == boost::uuids::to_string(second.request_id));
    REQUIRE(argument_of(*found, "publisher") != nullptr);
    CHECK(*argument_of(*found, "publisher") == publisher);
    REQUIRE(argument_of(*found, "previous_as_of") != nullptr);
    CHECK_FALSE(argument_of(*found, "previous_as_of")->empty());
    // The forward set has no record page yet, so the notice names the set and
    // carries no link rather than a link that opens nothing.
    CHECK(found->link_route.empty());
    // The notice names the subject and the act, never the values.
    CHECK(argument_of(*found, "forward_points") == nullptr);
    CHECK(argument_of(*found, "spot_rate") == nullptr);
}
