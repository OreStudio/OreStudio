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
#include "ores.inbox.core/messaging/apply_step_handler.hpp"
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.inbox.core/service/notification_center.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[approval][apply_step]");

ores::iam::domain::account seed_account(ores::testing::scoped_database_helper& h) {
    auto ctx = ores::testing::make_generation_context(h);
    auto a = ores::iam::generators::generate_synthetic_account(ctx);
    a.change_reason_code = "system.test";
    ores::iam::repository::account_repository repo;
    repo.write(h.context(), a);
    return a;
}

}

TEST_CASE("an_approved_request_is_told_as_applied_and_any_other_state_keeps_its_name", tags) {
    using ores::inbox::messaging::apply_result_state;
    CHECK(apply_result_state("approved") == "applied");
    CHECK(apply_result_state("apply_failed") == "apply_failed");
}

TEST_CASE("the_person_who_asked_is_told_how_the_apply_ended", tags) {
    ores::testing::scoped_database_helper h;
    const auto asker = seed_account(h);
    // The notice is sent by the account the context acts as, which in the
    // workflow is the service account. The test acts as the seeded account.
    const auto sender = h.context().with_tenant(h.tenant_id(), asker.username);
    ores::inbox::service::approval_lifecycle lifecycle(sender);
    const auto kind = *lifecycle.kind("iam.role_grant");
    const auto raised = lifecycle.raise(kind, "Please", asker.id);

    ores::inbox::service::approval_announcer(sender).tell_asker(
        kind, raised, "apply_failed", "Line 2: the book changed");

    ores::inbox::service::notification_center center(sender);
    const auto page = center.mine(asker.id, false, 0, 200);
    const auto it = std::ranges::find_if(page.notifications, [&](const auto& n) {
        return n.link_id == boost::uuids::to_string(raised.id);
    });
    REQUIRE(it != page.notifications.end());
    CHECK(it->kind_code == "inbox.approval_decided");
    const auto state = std::ranges::find_if(
        it->arguments, [](const auto& a) { return a.name == "state"; });
    REQUIRE(state != it->arguments.end());
    CHECK(state->value == "apply_failed");
}
