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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.api/domain/account.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/generators/business_centre_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/business_centre_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/repository/cut_approval_detail_repository.hpp"
#include "ores.reporting.core/repository/cut_repository.hpp"
#include "ores.reporting.core/service/cut_authorisation_applier.hpp"
#include "ores.reporting.core/service/cut_state.hpp"
#include "ores.reporting.service/service/cut_take.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

// The decision these pin: a decided cut authorisation reaches the cut by
// reconciliation and not by a second screen. An approval stamps the signer and
// the instant and marks the request applied; a refusal stamps the reason, which
// is terminal for that body; a second run writes nothing; and a request of
// another kind is left alone, because the apply answers the cut kinds' own
// approval kinds and no other.

using namespace ores::reporting;

namespace {

/** A party, an account that takes, and the context the take acts in. */
struct apply_world {
    boost::uuids::uuid party_id;
    /** The taking account: the store refuses a request raised for an account it does not hold. */
    boost::uuids::uuid taker_id;
    std::string taker;
    ores::database::context ctx;

    /** @brief The context the reconciliation runs in: the service's, with no party. */
    [[nodiscard]] ores::database::context service_context() const {
        return ctx.with_tenant(ctx.tenant_id(), "");
    }
};

/** What the store would answer about each part, stated by the test. */
struct apply_stub_reader final : ores::reporting::service::cut_body_reader {
    ores::reporting::service::cut_body_facts answer;

    [[nodiscard]] ores::reporting::service::cut_body_facts
    read(const ores::reporting::service::cut_body_query&) override {
        return answer;
    }
};

/** Facts a store that answers every part gives, at the demo's own sizes. */
ores::reporting::service::cut_body_facts apply_full_answers() {
    ores::reporting::service::cut_body_facts facts;
    facts.deal_count = 1284;
    facts.market_data_set_count = 7;
    facts.market_data_as_of = "2026-10-06 16:00:00.000000Z";
    facts.reference_data_version = "4 report configuration(s) in force, newest version 7";
    facts.engine_version = "1.8.17.0";
    facts.wrapper_version = "1.0.0";
    return facts;
}

ores::iam::domain::account apply_seed_account(ores::testing::scoped_database_helper& h,
                                        const std::string& prefix,
                                        const std::string& suffix) {
    auto gen = ores::testing::make_generation_context(h);
    auto account = ores::iam::generators::generate_synthetic_account(gen);
    account.username = prefix + "-" + suffix;
    account.account_type = "service";
    account.change_reason_code = "system.test";
    ores::iam::repository::account_repository{}.write(h.context(), account);
    return account;
}

apply_world apply_seed_world(ores::testing::scoped_database_helper& h, const std::string& suffix) {
    auto gen = ores::testing::make_generation_context(h);
    auto& base = h.context();

    const auto taker = "cut-taker-" + suffix;
    auto account = ores::iam::generators::generate_synthetic_account(gen);
    account.username = taker;
    account.account_type = "service";
    account.change_reason_code = "system.test";
    ores::iam::repository::account_repository{}.write(base, account);

    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& existing : ores::refdata::repository::party_repository{}.read_latest(base)) {
        if (existing.tenant_id == party.tenant_id) {
            party.parent_party_id = existing.id;
            break;
        }
    }
    ores::refdata::repository::party_repository{}.write(base, party);

    // A cut names the rates centre its scope revalues against, and the store
    // checks the code against the business centres the session may see.
    auto centre = ores::refdata::generators::generate_synthetic_business_centre(gen);
    centre.code = "GBLO";
    centre.change_reason_code = "system.test";
    ores::refdata::repository::business_centre_repository{}.write(base, centre);

    return {.party_id = party.id,
            .taker_id = account.id,
            .taker = taker,
            .ctx = base.with_party(base.tenant_id(), party.id, {party.id}, taker)};
}

/** A take of one kind at an instant a day past, so it is never ahead of now. */
ores::reporting::messaging::take_cut_request apply_take_request(const std::string& kind_code) {
    return {.kind_code = kind_code,
            .scope_code = "ACME-LONDON",
            .business_centre_code = "GBLO",
            .timepoint = ores::platform::time::datetime::to_iso8601_utc(
                std::chrono::system_clock::now() - std::chrono::hours(24)),
            .timepoint_timezone = "Europe/London",
            .cut_time_code = "",
            .reason = "The day is closed and the reports wait on the body",
            .preview = false};
}

/**
 * @brief A body frozen and its authorisation raised, taken through the take.
 *
 * The take is the only writer of a cut and of the detail row a request is about,
 * so a test that decided a hand-written pair would pin a shape the write does
 * not produce.
 */
ores::reporting::messaging::take_cut_response apply_take_eod(const apply_world& w) {
    apply_stub_reader reader;
    reader.answer = apply_full_answers();
    service::cut_take_service take(w.ctx, reader);
    return take.take(apply_take_request("EOD"));
}

std::string raise_a_request_of_kind(const apply_world& w,
                                    const std::string& kind_code,
                                    const boost::uuids::uuid& asker) {
    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto kind = lifecycle.kind(kind_code);
    REQUIRE(kind);
    return boost::uuids::to_string(lifecycle.raise(*kind, "Please apply this", asker).id);
}

}

TEST_CASE("an_approved_cut_authorisation_is_stamped_on_the_cut", "[cut_authorisation_apply]") {

    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto suffix = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
    const auto w = apply_seed_world(h, suffix);
    const auto decider = apply_seed_account(h, "cut-decider", suffix);

    const auto taken = apply_take_eod(w);
    REQUIRE(taken.approval_request_id != "");
    REQUIRE(taken.cut_state == "taken");

    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto request = lifecycle.request(taken.approval_request_id);
    REQUIRE(request);
    const auto decided = lifecycle.decide(taken.approval_request_id,
                                          request->version,
                                          "approve",
                                          decider.id,
                                          "The body is the day's close");
    REQUIRE(decided.outcome == "ok");
    REQUIRE(decided.state_code == "approved");

    // The decision reaches the cut by the reconciliation and by nothing else.
    // The count is not asserted: the reconciliation reads every tenant, so a
    // service running beside this case may have applied the row already, and the
    // facts below are the invariant either way.
    service::cut_authorisation_applier applier(w.service_context());
    applier.apply();

    const auto rows = repository::cut_repository{}.read_latest(w.ctx, taken.cut_id);
    REQUIRE(rows.size() == 1);
    const auto& cut = rows.front();
    CHECK(cut.approved_by == decider.username);
    REQUIRE(cut.approved_at.has_value());
    CHECK_FALSE(cut.refused_at.has_value());
    CHECK(cut.refusal_comment == "");
    // The act is stamped with the decision's own instant and not with the run's
    // clock, so a second run cannot move it.
    const auto decisions = ores::database::repository::execute_parameterized_multi_column_query(
        w.ctx,
        "select decided_at::text from ores_inbox_approval_decisions_tbl"
        " where request_id = $1::uuid and decision_code = 'approve'"
        " and valid_to = ores_utility_infinity_timestamp_fn()",
        {taken.approval_request_id},
        lg,
        "Reading the decision's instant");
    REQUIRE(decisions.size() == 1);
    CHECK(*cut.approved_at ==
          ores::platform::time::datetime::from_db_string(decisions.front()[0].value_or("")));

    // The detail row is the marker that the decision reached the cut.
    repository::cut_approval_detail_repository details;
    const auto detail = details.read_latest_by_request(w.ctx, taken.approval_request_id);
    REQUIRE(detail.has_value());
    REQUIRE(detail->applied_at.has_value());
    CHECK(*detail->applied_at == *cut.approved_at);
    CHECK(detail->applied_by != "");

    // The state is a function of those facts and is stored nowhere.
    CHECK(service::derive_state(cut, 0) == service::cut_state::authorised);
    CHECK(service::is_usable(service::derive_state(cut, 0)));
}

TEST_CASE("a_refused_cut_authorisation_stamps_the_refusal_and_its_reason",
          "[cut_authorisation_apply]") {

    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto suffix = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
    const auto w = apply_seed_world(h, suffix);
    const auto decider = apply_seed_account(h, "cut-decider", suffix);

    const auto taken = apply_take_eod(w);
    REQUIRE(taken.approval_request_id != "");

    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto request = lifecycle.request(taken.approval_request_id);
    REQUIRE(request);
    const auto decided =
        lifecycle.decide(taken.approval_request_id,
                         request->version,
                         "refuse",
                         decider.id,
                         "A book is missing from the population, so the body is not the close");
    REQUIRE(decided.outcome == "ok");
    REQUIRE(decided.state_code == "refused");

    // As above: the facts are the invariant, not which reconciler reached them
    // first.
    service::cut_authorisation_applier applier(w.service_context());
    applier.apply();

    const auto rows = repository::cut_repository{}.read_latest(w.ctx, taken.cut_id);
    REQUIRE(rows.size() == 1);
    const auto& cut = rows.front();
    CHECK(cut.refused_at.has_value());
    CHECK(cut.refusal_comment ==
          "A book is missing from the population, so the body is not the close");
    CHECK(cut.approved_by == "");
    CHECK_FALSE(cut.approved_at.has_value());
    CHECK(service::derive_state(cut, 0) == service::cut_state::refused);
    // A refusal is terminal for that body: nothing may price from it.
    CHECK_FALSE(service::is_usable(service::derive_state(cut, 0)));

    repository::cut_approval_detail_repository details;
    const auto detail = details.read_latest_by_request(w.ctx, taken.approval_request_id);
    REQUIRE(detail.has_value());
    CHECK(detail->applied_at.has_value());
    CHECK(*detail->applied_at == *cut.refused_at);
}

TEST_CASE("a_decision_nobody_announced_is_applied_by_the_next_run",
          "[cut_authorisation_apply]") {

    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto suffix = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
    const auto w = apply_seed_world(h, suffix);
    const auto decider = apply_seed_account(h, "cut-decider", suffix);

    const auto taken = apply_take_eod(w);
    REQUIRE(taken.approval_request_id != "");

    // The decision stands and no event reaches anybody: the reconciliation reads
    // the store, so what a missed event would have lost is applied anyway.
    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto request = lifecycle.request(taken.approval_request_id);
    REQUIRE(request);
    REQUIRE(lifecycle
                .decide(taken.approval_request_id, request->version, "approve", decider.id, "")
                .outcome == "ok");

    // Nothing ran between the decision and this run, which is the case a service
    // that was down leaves behind.
    service::cut_authorisation_applier applier(w.service_context());
    applier.apply();

    const auto rows = repository::cut_repository{}.read_latest(w.ctx, taken.cut_id);
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().approved_by == decider.username);
}

TEST_CASE("a_second_run_applies_nothing_and_leaves_the_cut_alone",
          "[cut_authorisation_apply]") {

    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto suffix = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
    const auto w = apply_seed_world(h, suffix);
    const auto decider = apply_seed_account(h, "cut-decider", suffix);

    const auto taken = apply_take_eod(w);
    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto request = lifecycle.request(taken.approval_request_id);
    REQUIRE(request);
    REQUIRE(lifecycle
                .decide(taken.approval_request_id, request->version, "approve", decider.id, "")
                .outcome == "ok");

    // As above: the facts are the invariant, not which reconciler reached them
    // first.
    service::cut_authorisation_applier applier(w.service_context());
    applier.apply();

    const auto after_first = repository::cut_repository{}.read_latest(w.ctx, taken.cut_id);
    REQUIRE(after_first.size() == 1);

    // The row leaves the answer once it is applied, so the second run finds
    // nothing and writes no new version of the cut.
    CHECK(applier.apply() == 0);
    CHECK(applier.apply() == 0);

    const auto after_second = repository::cut_repository{}.read_latest(w.ctx, taken.cut_id);
    REQUIRE(after_second.size() == 1);
    CHECK(after_second.front().version == after_first.front().version);
    CHECK(after_second.front().approved_by == decider.username);
    CHECK(*after_second.front().approved_at == *after_first.front().approved_at);
}

TEST_CASE("a_request_of_another_kind_is_left_alone", "[cut_authorisation_apply]") {

    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;
    const auto suffix = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
    const auto w = apply_seed_world(h, suffix);
    const auto decider = apply_seed_account(h, "cut-decider", suffix);

    // A body whose authorisation is recorded and a decision on another kind's
    // request that names the same cut: only the cut kind's approval kinds are
    // applied, so the second stamps nothing.
    const auto taken = apply_take_eod(w);
    const auto cut_id = boost::uuids::string_generator{}(taken.cut_id);

    const auto other_request = raise_a_request_of_kind(w, "iam.role_grant", w.taker_id);
    {
        domain::cut_approval_detail detail;
        detail.id = boost::uuids::random_generator{}();
        detail.tenant_id = w.ctx.tenant_id();
        detail.request_id = boost::uuids::string_generator{}(other_request);
        detail.party_id = w.party_id;
        detail.cut_id = cut_id;
        detail.deal_count = 1284;
        detail.market_data_set_count = 7;
        detail.gap_count = 0;
        detail.applied_by = "";
        detail.applied_at = std::nullopt;
        detail.modified_by = w.taker;
        detail.performed_by = w.taker;
        detail.change_reason_code = "system.test";
        detail.change_commentary = "A subject of another kind, for the test";
        repository::cut_approval_detail_repository{}.write(w.ctx, detail);
    }

    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto other = lifecycle.request(other_request);
    REQUIRE(other);
    REQUIRE(lifecycle.decide(other_request, other->version, "approve", decider.id, "").outcome ==
            "ok");

    service::cut_authorisation_applier applier(w.service_context());
    // The cut's own request still waits, so the apply answers nothing that names
    // this cut, and the other kind's request is not the apply's to answer. The
    // count is not asserted for the reason the cases above state.
    applier.apply();

    const auto rows = repository::cut_repository{}.read_latest(w.ctx, taken.cut_id);
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().approved_by == "");
    CHECK_FALSE(rows.front().approved_at.has_value());
    CHECK(service::derive_state(rows.front(), 0) == service::cut_state::taken);

    repository::cut_approval_detail_repository details;
    const auto untouched = details.read_latest_by_request(w.ctx, other_request);
    REQUIRE(untouched.has_value());
    CHECK_FALSE(untouched->applied_at.has_value());
}
