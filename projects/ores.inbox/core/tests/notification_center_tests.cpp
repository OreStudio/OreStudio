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
#include "ores.inbox.core/service/notification_center.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[notification]");

ores::iam::domain::account seed_account(ores::testing::scoped_database_helper& h) {
    auto ctx = ores::testing::make_generation_context(h);
    auto a = ores::iam::generators::generate_synthetic_account(ctx);
    a.change_reason_code = "system.test";
    ores::iam::repository::account_repository repo;
    repo.write(h.context(), a);
    return a;
}

ores::inbox::messaging::raise_notification_request waiting_request() {
    return ores::inbox::messaging::raise_notification_request{
        .kind_code = "inbox.approval_waiting",
        .link_route = "requests",
        .link_id = "",
        .arguments = {{.name = "kind", .value = "Role request"},
                      {.name = "reason", .value = "Covers, \"the\" {desk}"}},
        .account_ids = {},
        .audience_permission_code = ""};
}

// The database stamps every row with the account that wrote it, and refuses a
// write that names nobody, as production's signed-in context always names one.
ores::database::context acting(ores::testing::scoped_database_helper& h) {
    return h.context().with_tenant(h.tenant_id(), h.db_user());
}

}

using ores::inbox::service::notification_center;

TEST_CASE("a_raised_notification_reaches_each_recipient_with_its_values", tags) {
    ores::testing::scoped_database_helper h;
    const auto raiser = seed_account(h);
    const auto reader = seed_account(h);
    notification_center center(acting(h));

    const auto me = boost::uuids::to_string(reader.id);
    const auto raised =
        center.raise(waiting_request(), {me, boost::uuids::to_string(raiser.id)}, raiser.id);
    CHECK(raised.recipient_count == 2);

    const auto page = center.mine(reader.id, false, 0, 200);
    const auto it = std::ranges::find_if(
        page.notifications, [&](const auto& n) { return n.id == raised.notification_id; });
    REQUIRE(it != page.notifications.end());
    CHECK(it->kind_code == "inbox.approval_waiting");
    CHECK(it->message_key == "notification.inbox.approval_waiting");
    CHECK(it->link_route == "requests");
    CHECK(it->read_at.empty());
    REQUIRE(it->arguments.size() == 2);
    CHECK(it->arguments[0].name == "kind");
    CHECK(it->arguments[1].value == "Covers, \"the\" {desk}");
}

TEST_CASE("marking_read_and_clearing_move_a_person_s_own_copy", tags) {
    ores::testing::scoped_database_helper h;
    const auto raiser = seed_account(h);
    const auto reader = seed_account(h);
    notification_center center(acting(h));

    const auto raised =
        center.raise(waiting_request(), {boost::uuids::to_string(reader.id)}, raiser.id);
    const auto unread_before = center.unread(reader.id);
    CHECK(unread_before >= 1);

    CHECK(center.mark_read(reader.id, {raised.notification_id}) == 1);
    CHECK(center.unread(reader.id) == unread_before - 1);
    CHECK(center.mark_read(reader.id, {raised.notification_id}) == 0);

    const auto unread_page = center.mine(reader.id, true, 0, 200);
    CHECK(std::ranges::none_of(unread_page.notifications,
                               [&](const auto& n) { return n.id == raised.notification_id; }));

    CHECK(center.clear(reader.id, {raised.notification_id}) == 1);
    const auto page = center.mine(reader.id, false, 0, 200);
    CHECK(std::ranges::none_of(page.notifications,
                               [&](const auto& n) { return n.id == raised.notification_id; }));
}

TEST_CASE("a_person_never_reads_another_person_s_copy", tags) {
    ores::testing::scoped_database_helper h;
    const auto raiser = seed_account(h);
    const auto reader = seed_account(h);
    const auto other = seed_account(h);
    notification_center center(acting(h));

    const auto raised =
        center.raise(waiting_request(), {boost::uuids::to_string(reader.id)}, raiser.id);

    const auto page = center.mine(other.id, false, 0, 200);
    CHECK(std::ranges::none_of(page.notifications,
                               [&](const auto& n) { return n.id == raised.notification_id; }));
    CHECK(center.mark_read(other.id, {raised.notification_id}) == 0);
}
