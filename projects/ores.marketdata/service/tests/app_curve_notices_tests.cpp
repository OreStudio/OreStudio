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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_party_repository.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.api/messaging/notification_operations_protocol.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.inbox.core/service/notification_center.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.service/app/curve_notices.hpp"
#include "ores.marketdata.service/app/curve_publish_gate.hpp"
#include "ores.marketdata.service/app/curve_republish_service.hpp"
#include "ores.marketdata.service/app/curve_withdrawal.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
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
using ores::marketdata::service::app::record_publication;
using ores::marketdata::service::app::verdict_on_publish;
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
                ores::database::repository::execute_parameterized_string_query(
                    ctx_,
                    statement,
                    {boost::uuids::to_string(account_.id)},
                    notices_test_lg(),
                    "Closing the notice test's reader account");
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(notices_test_lg(), warn)
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
    ores::marketdata::service::app::curve_republish_service::republish(
        *f.party, subject.bootstrap_config_id, subject.as_of);
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

    const auto raised = ores::marketdata::service::app::raise_publish_request(
        maker.ctx, f.subject(), "Today's bootstrapped curve");
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

    const auto raised = ores::marketdata::service::app::raise_publish_request(
        maker.ctx, f.subject(), "Today's bootstrapped curve");
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

    const auto raised = ores::marketdata::service::app::raise_publish_request(
        maker.ctx, f.subject(), "A curve nobody checked");
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
