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
#include "ores.iam.api/domain/account.hpp"
#include "ores.iam.api/domain/permission.hpp"
#include "ores.iam.api/domain/permission_codes.hpp"
#include "ores.iam.api/domain/role.hpp"
#include "ores.iam.api/domain/role_permission.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.api/generators/account_role_generator.hpp"
#include "ores.iam.api/generators/permission_generator.hpp"
#include "ores.iam.api/generators/role_generator.hpp"
#include "ores.iam.core/messaging/authorization_handler.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/account_role_repository.hpp"
#include "ores.iam.core/repository/permission_repository.hpp"
#include "ores.iam.core/repository/role_permission_repository.hpp"
#include "ores.iam.core/repository/role_repository.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <algorithm>
#include <vector>

namespace {

const std::string_view test_suite("ores.iam.tests.authorization_access");
const std::string tags("[authorization]");

using ores::iam::domain::account;
using ores::iam::domain::role;
using ores::iam::generators::generate_synthetic_account;
using ores::iam::generators::generate_synthetic_account_role;
using ores::iam::generators::generate_synthetic_permission;
using ores::iam::generators::generate_synthetic_role;
using ores::iam::repository::account_repository;
using ores::iam::repository::account_role_repository;
using ores::iam::repository::permission_repository;
using ores::iam::repository::role_permission_repository;
using ores::iam::repository::role_repository;
using ores::iam::service::authorization_service;
using ores::testing::database_helper;
using ores::utility::domain::outcome;
using ores::utility::generation::generation_context;
using namespace ores::logging;
namespace permissions = ores::iam::domain::permissions;

/**
 * @brief The access a read reports, without Member.
 *
 * Every person account holds Member from its first version, so the roles a
 * test assigned are what is left once Member is set aside.
 */
ores::iam::service::account_access assigned(ores::iam::service::account_access access) {
    std::erase_if(access.roles, [](const auto& entry) { return entry.role.name == "Member"; });
    return access;
}

account write_account(database_helper& h, generation_context& gen) {
    account_repository repo;
    auto value = generate_synthetic_account(gen);
    repo.write(h.context(), value);
    return value;
}

void assign(database_helper& h, generation_context& gen, const account& a, const role& r) {
    account_role_repository repo(h.context());
    auto assignment = generate_synthetic_account_role(gen);
    assignment.account_id = a.id;
    assignment.role_id = r.id;
    repo.write(assignment);
}

/*
 * A role bundling one permission code, written once per run: the code is the
 * tenant's natural key, so a second test that needs it joins the row the first
 * one wrote rather than colliding with the unique index.
 */
role write_role_bundling(database_helper& h, generation_context& gen, const std::string& code) {
    role_repository roles;
    permission_repository permissions_repo;
    role_permission_repository links(h.context());

    auto r = generate_synthetic_role(gen);
    roles.write(h.context(), r);

    auto existing = permissions_repo.read_latest_by_code(h.context(), code);
    auto p = existing.empty() ? generate_synthetic_permission(gen) : existing.front();
    if (existing.empty()) {
        p.code = code;
        permissions_repo.write(h.context(), p);
    }

    ores::iam::domain::role_permission link;
    link.tenant_id = h.tenant_id();
    link.role_id = r.id;
    link.permission_id = p.id;
    link.assigned_by = h.db_user();
    link.change_reason_code = "system.test";
    link.change_commentary = "Synthetic test data";
    links.write(link);

    return r;
}

/*
 * An area name no earlier run used. The permission catalogue and the roles that
 * bundle it persist between runs, so a fixed name would pick up their rows.
 */
std::string unique_area(const std::string& prefix) {
    auto id = boost::uuids::to_string(boost::uuids::random_generator()());
    id.erase(std::remove(id.begin(), id.end(), '-'), id.end());
    return prefix + id.substr(0, 10);
}

}

TEST_CASE("read_own_access_returns_roles_with_permissions_and_assignment_tail", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    auto r = write_role_bundling(h, gen, std::string(permissions::accounts_read));
    assign(h, gen, caller, r);

    authorization_service svc(h.context());
    const auto access = assigned(svc.read_own_access(caller.id));

    BOOST_LOG_SEV(lg, debug) << "Own access rows: " << access.roles.size();

    REQUIRE(access.result.outcome == outcome::ok);
    REQUIRE(access.roles.size() == 1);

    const auto& entry = access.roles.front();
    CHECK(entry.role.id == r.id);
    CHECK(entry.role.name == r.name);
    REQUIRE(entry.permission_codes.size() == 1);
    CHECK(entry.permission_codes.front() == permissions::accounts_read);
    CHECK(entry.assigned_by == h.db_user());
    CHECK(entry.change_reason_code == "system.test");
    CHECK(entry.change_commentary == "Synthetic test data");
    CHECK(entry.assigned_at.time_since_epoch().count() != 0);
}

TEST_CASE("read_account_access_is_denied_for_a_caller_holding_only_member", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    auto other = write_account(h, gen);
    auto r = write_role_bundling(h, gen, std::string(permissions::accounts_read));
    assign(h, gen, other, r);

    /*
     * The accounts trigger gives this caller Member and nothing else, which is
     * what every person holds. Member bundles no code that reads another
     * account: the role and permission catalogues are open reads, so no grant
     * is needed for them, and another account's access stays out of reach.
     */
    authorization_service svc(h.context());
    const auto access = assigned(svc.read_account_access(caller.id, other.id));

    BOOST_LOG_SEV(lg, debug) << "Denied access rows: " << access.roles.size();

    CHECK(access.result.outcome == outcome::denied);
    CHECK(access.result.code == permissions::roles_read);
    CHECK_FALSE(access.result.message.empty());
    CHECK(access.roles.empty());
}

TEST_CASE("read_account_access_is_denied_without_the_read_permission", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    auto other = write_account(h, gen);
    auto r = write_role_bundling(h, gen, std::string(permissions::accounts_read));
    assign(h, gen, other, r);

    /*
     * A caller that holds no role at all is the state an administrator leaves
     * behind by taking Member back -- the trigger gives Member once and never
     * again -- so the gate is asked about that caller here.
     */
    account_role_repository(h.context()).remove_all_for_account(caller.id);

    authorization_service svc(h.context());
    const auto access = assigned(svc.read_account_access(caller.id, other.id));

    BOOST_LOG_SEV(lg, debug) << "Denied access rows: " << access.roles.size();

    CHECK(access.result.outcome == outcome::denied);
    CHECK(access.result.code == permissions::roles_read);
    CHECK_FALSE(access.result.message.empty());
    CHECK(access.roles.empty());
}

TEST_CASE("read_account_access_returns_the_composed_shape_with_the_read_permission", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    auto caller_role = write_role_bundling(h, gen, std::string(permissions::roles_read));
    assign(h, gen, caller, caller_role);

    auto other = write_account(h, gen);
    auto other_role = write_role_bundling(h, gen, std::string(permissions::accounts_read));
    assign(h, gen, other, other_role);

    authorization_service svc(h.context());
    const auto access = assigned(svc.read_account_access(caller.id, other.id));

    BOOST_LOG_SEV(lg, debug) << "Privileged access rows: " << access.roles.size();

    REQUIRE(access.result.outcome == outcome::ok);
    REQUIRE(access.roles.size() == 1);

    const auto& entry = access.roles.front();
    CHECK(entry.role.id == other_role.id);
    REQUIRE(entry.permission_codes.size() == 1);
    CHECK(entry.permission_codes.front() == permissions::accounts_read);
    CHECK_FALSE(entry.assigned_by.empty());
    CHECK(entry.assigned_at.time_since_epoch().count() != 0);
}

TEST_CASE("read_own_access_for_an_account_holding_nothing_is_ok_and_empty", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);

    authorization_service svc(h.context());
    const auto access = assigned(svc.read_own_access(caller.id));

    BOOST_LOG_SEV(lg, debug) << "Empty own access rows: " << access.roles.size();

    CHECK(access.result.outcome == outcome::ok);
    CHECK(access.roles.empty());
}

/*
 * The request context carries the actor's username, and the permission check
 * is keyed by account id. A read that took the username for an account id
 * failed every live request, so the resolution is tested on its own rather
 * than through a read whose answer would look the same.
 */
TEST_CASE("caller_account_resolves_the_context_actor_to_their_account", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);

    authorization_service svc(h.context().with_tenant(h.tenant_id(), caller.username));
    const auto resolved = svc.caller_account();

    BOOST_LOG_SEV(lg, debug) << "Actor '" << caller.username
                             << "' resolved: " << resolved.has_value();

    REQUIRE(resolved.has_value());
    CHECK(*resolved == caller.id);
}

TEST_CASE("caller_account_is_empty_when_no_account_carries_the_actor", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    authorization_service svc(h.context().with_tenant(h.tenant_id(), "no-such-actor@ores.invalid"));
    const auto resolved = svc.caller_account();

    CHECK_FALSE(resolved.has_value());
}

/*
 * The bundle write. The codes the request names are the whole bundle the role
 * carries afterwards, so a replacement is one call with both an addition and a
 * removal in it.
 */

namespace {

account caller_who_may_shape_roles(database_helper& h, generation_context& gen) {
    auto caller = write_account(h, gen);
    auto role = write_role_bundling(h, gen, std::string(permissions::roles_update));
    assign(h, gen, caller, role);
    return caller;
}

}

TEST_CASE("replace_role_permissions_adds_and_removes_in_one_call", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = caller_who_may_shape_roles(h, gen);
    auto role = write_role_bundling(h, gen, std::string(permissions::accounts_read));

    authorization_service svc(h.context());
    const auto answer = svc.replace_role_permissions(caller.id,
                                                     role.id,
                                                     {std::string(permissions::roles_read)},
                                                     "common.rectification",
                                                     "The role was too broad");

    BOOST_LOG_SEV(lg, debug) << "Bundle after replace: " << answer.permission_codes.size();

    REQUIRE(answer.result.outcome == outcome::ok);
    REQUIRE(answer.permission_codes.size() == 1);
    CHECK(answer.permission_codes.front() == permissions::roles_read);

    role_permission_repository links(h.context());
    const auto stored = svc.get_role_permissions(role.id);
    REQUIRE(stored.size() == 1);
    CHECK(stored.front() == permissions::roles_read);

    const auto rows = links.read_latest_by_role(role.id);
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().change_reason_code == "common.rectification");
    CHECK(rows.front().change_commentary == "The role was too broad");
    CHECK_FALSE(rows.front().assigned_by.empty());
    CHECK(rows.front().assigned_at.time_since_epoch().count() != 0);
}

TEST_CASE("replace_role_permissions_is_denied_without_the_update_permission", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    auto role = write_role_bundling(h, gen, std::string(permissions::accounts_read));

    authorization_service svc(h.context());
    const auto answer = svc.replace_role_permissions(caller.id,
                                                     role.id,
                                                     {std::string(permissions::roles_read)},
                                                     "common.rectification",
                                                     "Should not land");

    CHECK(answer.result.outcome == outcome::denied);
    CHECK(answer.result.code == permissions::roles_update);
    CHECK_FALSE(answer.result.message.empty());

    const auto stored = svc.get_role_permissions(role.id);
    REQUIRE(stored.size() == 1);
    CHECK(stored.front() == permissions::accounts_read);
}

TEST_CASE("replace_role_permissions_refuses_a_code_that_is_not_a_permission", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = caller_who_may_shape_roles(h, gen);
    auto role = write_role_bundling(h, gen, std::string(permissions::accounts_read));

    authorization_service svc(h.context());
    const auto answer =
        svc.replace_role_permissions(caller.id, role.id, {"no::such:permission"}, "", "");

    CHECK(answer.result.outcome == outcome::invalid);
    CHECK(answer.result.code == "no::such:permission");

    const auto stored = svc.get_role_permissions(role.id);
    REQUIRE(stored.size() == 1);
    CHECK(stored.front() == permissions::accounts_read);
}

TEST_CASE("replace_role_permissions_refuses_a_role_that_does_not_exist", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = caller_who_may_shape_roles(h, gen);

    authorization_service svc(h.context());
    const auto answer = svc.replace_role_permissions(
        caller.id, generate_synthetic_role(gen).id, {std::string(permissions::roles_read)}, "", "");

    CHECK(answer.result.outcome == outcome::missing);
    CHECK_FALSE(answer.result.message.empty());
}

TEST_CASE("replace_role_permissions_can_empty_a_bundle", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = caller_who_may_shape_roles(h, gen);
    auto role = write_role_bundling(h, gen, std::string(permissions::accounts_read));

    authorization_service svc(h.context());
    const auto answer = svc.replace_role_permissions(caller.id, role.id, {}, "", "");

    REQUIRE(answer.result.outcome == outcome::ok);
    CHECK(answer.permission_codes.empty());
    CHECK(svc.get_role_permissions(role.id).empty());
}

/*
 * Giving a role records why. The reason is the administrator's, from the
 * access category, and the grant row carries it beside who gave the role.
 */
TEST_CASE("assign_role_records_the_reason_it_was_given_for", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto member = write_account(h, gen);
    auto r = write_role_bundling(h, gen, std::string(permissions::accounts_read));

    authorization_service svc(h.context());
    svc.assign_role(
        member.id, r.id, h.db_user(), "Covers the settlement desk", "access.cover_for_absence");

    const auto access = assigned(svc.read_own_access(member.id));
    REQUIRE(access.roles.size() == 1);
    CHECK(access.roles.front().assigned_by == h.db_user());
    CHECK(access.roles.front().change_reason_code == "access.cover_for_absence");
    CHECK(access.roles.front().change_commentary == "Covers the settlement desk");
}

TEST_CASE("assign_role_without_a_reason_records_a_new_record", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto member = write_account(h, gen);
    auto r = write_role_bundling(h, gen, std::string(permissions::accounts_read));

    authorization_service svc(h.context());
    svc.assign_role(member.id, r.id, h.db_user());

    const auto access = assigned(svc.read_own_access(member.id));
    REQUIRE(access.roles.size() == 1);
    CHECK(access.roles.front().change_reason_code == "system.new_record");
}

TEST_CASE("assign_role_refuses_a_reason_nobody_defined", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto member = write_account(h, gen);
    auto r = write_role_bundling(h, gen, std::string(permissions::accounts_read));

    authorization_service svc(h.context());
    CHECK_THROWS_WITH(svc.assign_role(member.id, r.id, h.db_user(), "", "access.because_i_said_so"),
                      Catch::Matchers::ContainsSubstring("change_reason_code"));
    CHECK(assigned(svc.read_own_access(member.id)).roles.empty());
}

TEST_CASE("nobody_takes_a_role_away_from_themselves", tags) {
    using ores::iam::messaging::authorization_revoke_refusal;
    const auto me = boost::uuids::random_generator()();
    const auto colleague = boost::uuids::random_generator()();

    CHECK(authorization_revoke_refusal(me, me) == "You cannot take a role away from yourself.");
    CHECK_FALSE(authorization_revoke_refusal(me, colleague).has_value());
}

/*
 * What an account's roles let it do is paged by the server, one area at a
 * time. The area here is made up, so the rows are exactly those the test wrote
 * whatever else the database's catalogue holds.
 */
TEST_CASE("read_own_permissions_pages_the_resources_of_one_area", tags) {
    database_helper h;
    const auto zzpage_name = unique_area("zzpage");
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    const auto a = write_role_bundling(h, gen, zzpage_name + "::alpha_one:read");
    const auto b = write_role_bundling(h, gen, zzpage_name + "::alpha_two:read");
    const auto c = write_role_bundling(h, gen, zzpage_name + "::alpha_three:read");
    assign(h, gen, caller, a);
    assign(h, gen, caller, b);
    assign(h, gen, caller, c);

    authorization_service svc(h.context());
    const auto first = svc.read_own_permissions(
        caller.id, {.area = zzpage_name, .search = "", .offset = 0, .limit = 2});
    const auto second = svc.read_own_permissions(
        caller.id, {.area = zzpage_name, .search = "", .offset = 2, .limit = 2});

    CHECK(first.result.outcome == outcome::ok);
    CHECK(first.area == zzpage_name);
    CHECK(first.total_count == 3);
    CHECK(first.rows.size() == 2);
    CHECK(second.total_count == 3);
    CHECK(second.rows.size() == 1);
    // Rows run in name order, so the pages do not overlap.
    CHECK(first.rows.front().resource == "alpha_one");
    CHECK(second.rows.front().resource == "alpha_two");
}

TEST_CASE("read_own_permissions_names_the_actions_held_and_the_role_behind_them", tags) {
    database_helper h;
    const auto zzheld_name = unique_area("zzheld");
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    const auto r = write_role_bundling(h, gen, zzheld_name + "::beta_one:read");
    assign(h, gen, caller, r);

    authorization_service svc(h.context());
    const auto page = svc.read_own_permissions(caller.id, {.area = zzheld_name});

    REQUIRE(page.rows.size() == 1);
    CHECK(page.rows.front().held == std::vector<std::string>{"read"});
    CHECK(page.rows.front().roles == std::vector<std::string>{r.name});
}

TEST_CASE("read_own_permissions_lists_only_areas_the_account_holds_something_in", tags) {
    database_helper h;
    const auto zzonly_name = unique_area("zzonly");
    const auto zznothing_name = unique_area("zznothing");
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    const auto r = write_role_bundling(h, gen, zzonly_name + "::gamma_one:read");
    assign(h, gen, caller, r);

    authorization_service svc(h.context());
    const auto page = svc.read_own_permissions(caller.id, {});

    const auto holds = [&](const std::string& component) {
        return std::any_of(page.areas.begin(), page.areas.end(), [&](const auto& area) {
            return area.component == component;
        });
    };
    CHECK(holds(zzonly_name));
    // A made-up area nobody was given is not offered, and the chosen area is
    // the first one listed when none was asked for.
    CHECK(!holds(zznothing_name));
    REQUIRE(!page.areas.empty());
    CHECK(page.area == page.areas.front().component);
}

TEST_CASE("read_account_permissions_is_refused_without_roles_read", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    auto target = write_account(h, gen);

    authorization_service svc(h.context());
    const auto page = svc.read_account_permissions(caller.id, target.id, {});

    CHECK(page.result.outcome == outcome::denied);
    CHECK(page.rows.empty());
}

/*
 * The role editor must offer every permission to tick, so its page carries the
 * rows the role does not grant too, and says which of them it holds.
 */
TEST_CASE("read_role_permissions_includes_unheld_rows_for_the_editor", tags) {
    database_helper h;
    const auto zzedit_name = unique_area("zzedit");
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    const auto reader = write_role_bundling(h, gen, std::string(permissions::roles_read));
    assign(h, gen, caller, reader);
    const auto subject = write_role_bundling(h, gen, zzedit_name + "::delta_one:read");
    write_role_bundling(h, gen, zzedit_name + "::delta_two:read");

    authorization_service svc(h.context());
    const auto held_only = svc.read_role_permissions(
        caller.id, subject.id, {.area = zzedit_name, .include_unheld = false});
    const auto everything = svc.read_role_permissions(
        caller.id, subject.id, {.area = zzedit_name, .include_unheld = true});

    CHECK(held_only.result.outcome == outcome::ok);
    CHECK(held_only.total_count == 1);
    CHECK(everything.total_count == 2);
    const auto held_rows = std::count_if(
        everything.rows.begin(), everything.rows.end(), [](const auto& row) {
            return !row.held.empty();
        });
    CHECK(held_rows == 1);
}

TEST_CASE("read_role_permissions_is_refused_without_roles_read", tags) {
    database_helper h;
    const auto zzedit_name = unique_area("zzedit");
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    const auto subject = write_role_bundling(h, gen, zzedit_name + "::echo_one:read");

    authorization_service svc(h.context());
    const auto page = svc.read_role_permissions(caller.id, subject.id, {});

    CHECK(page.result.outcome == outcome::denied);
}

TEST_CASE("read_roles_page_pages_filters_by_area_and_leaves_service_roles_out", tags) {
    database_helper h;
    const auto zznowhere_name = unique_area("zznowhere");
    const auto zzlist_name = unique_area("zzlist");
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    const auto reader = write_role_bundling(h, gen, std::string(permissions::roles_read));
    assign(h, gen, caller, reader);
    const auto mine = write_role_bundling(h, gen, zzlist_name + "::foxtrot_one:read");

    authorization_service svc(h.context());
    const auto in_area = svc.read_roles_page(caller.id, {.area = zzlist_name});
    const auto none = svc.read_roles_page(caller.id, {.area = zznowhere_name});
    const auto one = svc.read_roles_page(
        caller.id, {.role_id = boost::uuids::to_string(mine.id)});

    CHECK(in_area.result.outcome == outcome::ok);
    // A role that grants everything grants something in every area, so the
    // roles in an area are the one that names it and those that grant it all.
    const auto named = std::find_if(in_area.roles.begin(),
                                    in_area.roles.end(),
                                    [&](const auto& row) { return row.name == mine.name; });
    REQUIRE(named != in_area.roles.end());
    CHECK(named->permission_count == 1);
    CHECK(std::all_of(in_area.roles.begin(), in_area.roles.end(), [&](const auto& row) {
        return row.name == mine.name || row.everything;
    }));
    // An area nobody was given lists only the roles that grant everything.
    CHECK(std::all_of(none.roles.begin(), none.roles.end(), [](const auto& row) {
        return row.everything;
    }));
    // The areas on offer are those some listed role grants, whatever the filter.
    CHECK(std::any_of(none.areas.begin(), none.areas.end(), [&](const auto& area) {
        return area.component == zzlist_name;
    }));
    CHECK(one.total_count == 1);
}

TEST_CASE("read_roles_page_is_refused_without_roles_read", tags) {
    database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    auto caller = write_account(h, gen);
    authorization_service svc(h.context());

    CHECK(svc.read_roles_page(caller.id, {}).result.outcome == outcome::denied);
}
