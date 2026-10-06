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
#include "ores.iam.api/generators/permission_generator.hpp"
#include "ores.iam.api/generators/role_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/permission_repository.hpp"
#include "ores.iam.core/repository/role_permission_repository.hpp"
#include "ores.iam.core/repository/role_repository.hpp"
#include "ores.iam.core/repository/run_token_issue_repository.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.iam.core/service/run_grant_operations_service.hpp"
#include "ores.iam.core/service/run_grant_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <chrono>
#include <functional>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.iam.tests.run_grant_exchange");
const std::string tags("[service][run_grant][exchange]");

using ores::database::context;
using ores::iam::domain::account;
using ores::iam::domain::role;
using ores::iam::messaging::create_run_grant_request;
using ores::iam::messaging::exchange_run_grant_request;
using ores::iam::messaging::revoke_run_grant_request;
using ores::iam::service::authorization_service;
using ores::iam::service::run_grant_operations_service;
using ores::iam::service::run_grant_service;
using ores::testing::database_helper;
using ores::utility::generation::generation_context;
using Catch::Matchers::ContainsSubstring;
using namespace ores::logging;

const std::string granted_code("iam::accounts:read");
const std::string exchange_code("iam::run_grants:exchange");
const std::string reporting_service("ReportingService");
const std::string compute_service("ComputeService");
const std::string audience("ReportingService,OreService,ComputeService");

/// A run token is signed and verified by the same object in IAM.
ores::security::jwt::jwt_authenticator signer_for_tests() {
    return ores::security::jwt::jwt_authenticator::create_hs256("ores-iam-run-token-tests");
}

account write_account(database_helper& h, generation_context& gen, const std::string& status) {
    ores::iam::repository::account_repository repo;
    auto value = ores::iam::generators::generate_synthetic_account(gen);
    value.account_status = status;
    repo.write(h.context(), value);
    return value;
}

/// A role named @p name that bundles @p code, so a grant names a role a
/// service can hold.
role write_role(database_helper& h,
                generation_context& gen,
                const std::string& name,
                const std::string& code) {
    ores::iam::repository::role_repository roles;
    ores::iam::repository::permission_repository permissions_repo;
    ores::iam::repository::role_permission_repository links(h.context());

    auto r = ores::iam::generators::generate_synthetic_role(gen);
    r.name = name;
    roles.write(h.context(), r);

    auto existing = permissions_repo.read_latest_by_code(h.context(), code);
    auto p = existing.empty() ? ores::iam::generators::generate_synthetic_permission(gen) :
                                existing.front();
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

/// The role named @p name if the tenant already has one, and a new role that
/// bundles @p code otherwise. A provisioned tenant copies the service roles, so
/// a fixture that inserted one would collide on the name index.
role existing_or_new_role(database_helper& h,
                          generation_context& gen,
                          const std::string& name,
                          const std::string& code) {
    authorization_service auth(h.context());
    if (const auto found = auth.find_role_by_name(name))
        return *found;
    return write_role(h, gen, name, code);
}

/// A step service's own context: it acts as its service account, in the
/// tenant the request names, carrying only the exchange permission.
context service_context(database_helper& h,
                        const account& service,
                        const boost::uuids::uuid& party) {
    return h.context()
        .with_party(h.tenant_id(), party, {party}, service.username)
        .with_roles({exchange_code});
}

create_run_grant_request grant_request(const role& r, const std::string& resource) {
    create_run_grant_request req;
    req.resource = resource;
    req.role = r.name;
    req.audience = audience;
    return req;
}

std::string resource_name() {
    return "reporting.report_definition/" +
           boost::uuids::to_string(boost::uuids::random_generator()());
}

boost::uuids::uuid id_of(const std::string& s) {
    return boost::uuids::string_generator()(s);
}

exchange_run_grant_request exchange_request(const std::string& grant_id,
                                            const std::string& tenant_id,
                                            const std::string& run_id) {
    exchange_run_grant_request req;
    req.grant_id = grant_id;
    req.run_id = run_id;
    req.tenant_id = tenant_id;
    return req;
}

/**
 * @brief A person's live grant, and the step service that exchanges it.
 *
 * The grantor holds the granted role, the service account holds the
 * ReportingService role, and the grant's audience names that role.
 */
struct exchange_fixture {
    database_helper h;
    generation_context gen;
    account grantor;
    account service_account;
    role grantor_role;
    role service_role;
    boost::uuids::uuid party;
    std::string tenant_id;
    std::string grant_id;
    std::optional<context> caller_context;

    /// The step service's own context.
    context caller() const { return *caller_context; }

    explicit exchange_fixture(const std::string& grantor_status = "active")
        : h()
        , gen(ores::testing::make_generation_context(h))
        , party(boost::uuids::random_generator()())
        , tenant_id(h.tenant_id().to_string()) {
        grantor = write_account(h, gen, grantor_status);
        service_account = write_account(h, gen, "active");
        grantor_role = existing_or_new_role(h, gen, "ReportRunViewer", granted_code);
        service_role = existing_or_new_role(h, gen, reporting_service, granted_code);

        authorization_service auth(h.context());
        auth.assign_role(
            grantor.id, grantor_role.id, h.db_user(), "Synthetic test data", "system.test");
        auth.assign_role(
            service_account.id, service_role.id, h.db_user(), "Synthetic test data", "system.test");
        caller_context = service_context(h, service_account, party);

        const auto person_ctx = h.context()
                                    .with_party(h.tenant_id(), party, {party}, grantor.username)
                                    .with_roles({granted_code});
        grant_id = run_grant_operations_service(person_ctx)
                       .create_run_grant(grant_request(grantor_role, resource_name()))
                       .grant_id;
    }

    /// The grant as stored, read as the grantor reads it.
    std::optional<ores::iam::domain::run_grant> stored_grant() {
        return run_grant_service(h.context().with_tenant(h.tenant_id(), grantor.username))
            .find_grant(id_of(grant_id));
    }

    std::optional<ores::iam::domain::run_grant> save_grant(
        const std::function<void(ores::iam::domain::run_grant&)>& change) {
        auto grants = run_grant_service(h.context().with_tenant(h.tenant_id(), grantor.username));
        auto grant = grants.find_grant(id_of(grant_id));
        if (!grant)
            return std::nullopt;
        change(*grant);
        grant->change_reason_code = "system.test";
        grants.save_grant(*grant);
        return grant;
    }
};

}

TEST_CASE("exchange_run_grant_issues_a_token_naming_the_grant_the_run_and_the_service", tags) {
    auto lg(make_logger(test_suite));
    exchange_fixture f;
    REQUIRE_FALSE(f.grant_id.empty());

    const auto signer = signer_for_tests();
    const auto resp = run_grant_operations_service(f.caller(), signer)
                          .exchange_run_grant(exchange_request(f.grant_id, f.tenant_id, "run-1"));
    BOOST_LOG_SEV(lg, info) << "Response: " << resp.message;

    REQUIRE(resp.success);
    REQUIRE_FALSE(resp.token.empty());
    const auto claims = signer.validate(resp.token);
    REQUIRE(claims);
    CHECK(claims->subject == boost::uuids::to_string(f.grantor.id));
    CHECK(claims->username == f.grantor.username);
    CHECK(claims->tenant_id == f.tenant_id);
    CHECK(claims->party_id == boost::uuids::to_string(f.party));
    REQUIRE(claims->visible_party_ids.size() == 1);
    CHECK(claims->visible_party_ids.front() == boost::uuids::to_string(f.party));
    CHECK(claims->act == reporting_service);
    CHECK(claims->audience == reporting_service);
    CHECK(claims->grant_id == f.grant_id);
    CHECK(claims->run_id == "run-1");
    CHECK_FALSE(claims->session_id.has_value());
    CHECK(claims->expires_at - claims->issued_at == std::chrono::seconds(300));
    CHECK(claims->roles == std::vector<std::string>{granted_code});
    CHECK(resp.expires_at ==
          std::chrono::duration_cast<std::chrono::seconds>(claims->expires_at.time_since_epoch())
              .count());

    // Each issue appends one row, and the grant has served one run.
    ores::iam::repository::run_token_issue_repository issues(f.h.context());
    CHECK(issues.distinct_runs(f.grant_id) == 1);
    CHECK(issues.exists(f.grant_id, "run-1"));
}

TEST_CASE("exchange_run_grant_refuses_a_caller_without_the_exchange_permission", tags) {
    exchange_fixture f;
    const auto without = f.caller().with_roles({"iam::accounts:read"});
    const auto resp = run_grant_operations_service(without, signer_for_tests())
                          .exchange_run_grant(exchange_request(f.grant_id, f.tenant_id, "run-1"));
    CHECK_FALSE(resp.success);
    CHECK_THAT(resp.message, ContainsSubstring("not_permitted"));
}

TEST_CASE("exchange_run_grant_refuses_an_account_that_holds_no_service_role", tags) {
    exchange_fixture f;
    const auto plain = write_account(f.h, f.gen, "active");
    const auto resp = run_grant_operations_service(
                          service_context(f.h, plain, f.party), signer_for_tests())
                          .exchange_run_grant(
                              exchange_request(f.grant_id, f.tenant_id, "run-1"));
    CHECK_FALSE(resp.success);
    CHECK_THAT(resp.message, ContainsSubstring("no_service_identity"));
}

TEST_CASE("exchange_run_grant_refuses_a_service_outside_the_audience", tags) {
    exchange_fixture f;
    const auto other_role = existing_or_new_role(f.h, f.gen, compute_service, granted_code);
    const auto other = write_account(f.h, f.gen, "active");
    authorization_service(f.h.context())
        .assign_role(other.id, other_role.id, f.h.db_user(), "Synthetic test data", "system.test");
    // The grant's audience names ReportingService and OreService alone, so the
    // compute service is outside it.
    REQUIRE(f.save_grant([](auto& g) { g.audience = "ReportingService,OreService"; }));

    const auto resp = run_grant_operations_service(
                          service_context(f.h, other, f.party), signer_for_tests())
                          .exchange_run_grant(
                              exchange_request(f.grant_id, f.tenant_id, "run-1"));
    CHECK_FALSE(resp.success);
    CHECK_THAT(resp.message, ContainsSubstring("outside_audience"));
}

TEST_CASE("exchange_run_grant_refuses_an_unknown_grant", tags) {
    exchange_fixture f;
    const auto missing = boost::uuids::to_string(boost::uuids::random_generator()());
    const auto resp = run_grant_operations_service(f.caller(), signer_for_tests())
                          .exchange_run_grant(exchange_request(missing, f.tenant_id, "run-1"));
    CHECK_FALSE(resp.success);
    CHECK_THAT(resp.message, ContainsSubstring("grant_unknown"));
}

TEST_CASE("exchange_run_grant_refuses_a_revoked_grant", tags) {
    exchange_fixture f;
    revoke_run_grant_request revoke;
    revoke.grant_id = f.grant_id;
    const auto person_ctx = f.h.context()
                                .with_party(f.h.tenant_id(), f.party, {f.party}, f.grantor.username)
                                .with_roles({granted_code});
    REQUIRE(run_grant_operations_service(person_ctx).revoke_run_grant(revoke).success);

    const auto resp = run_grant_operations_service(f.caller(), signer_for_tests())
                          .exchange_run_grant(exchange_request(f.grant_id, f.tenant_id, "run-1"));
    CHECK_FALSE(resp.success);
    CHECK_THAT(resp.message, ContainsSubstring("grant_revoked"));
}

TEST_CASE("exchange_run_grant_refuses_a_grant_past_its_not_after", tags) {
    exchange_fixture f;
    REQUIRE(f.save_grant([](auto& g) {
        g.not_after = std::chrono::system_clock::now() - std::chrono::hours(1);
    }));

    const auto resp = run_grant_operations_service(f.caller(), signer_for_tests())
                          .exchange_run_grant(exchange_request(f.grant_id, f.tenant_id, "run-1"));
    CHECK_FALSE(resp.success);
    CHECK_THAT(resp.message, ContainsSubstring("grant_expired"));
}

TEST_CASE("exchange_run_grant_admits_one_run_and_refuses_the_next", tags) {
    exchange_fixture f;
    REQUIRE(f.save_grant([](auto& g) { g.max_runs = 1; }));
    run_grant_operations_service sut(f.caller(), signer_for_tests());

    REQUIRE(sut.exchange_run_grant(exchange_request(f.grant_id, f.tenant_id, "run-1")).success);
    // A second exchange of the same run is a retry of the same work.
    CHECK(sut.exchange_run_grant(exchange_request(f.grant_id, f.tenant_id, "run-1")).success);

    const auto second = sut.exchange_run_grant(exchange_request(f.grant_id, f.tenant_id, "run-2"));
    CHECK_FALSE(second.success);
    CHECK_THAT(second.message, ContainsSubstring("grant_exhausted"));
}

TEST_CASE("exchange_run_grant_refuses_a_tenant_that_is_not_a_tenant_id", tags) {
    exchange_fixture f;
    const auto resp = run_grant_operations_service(f.caller(), signer_for_tests())
                          .exchange_run_grant(exchange_request(f.grant_id, "not-a-tenant", "run-1"));
    CHECK_FALSE(resp.success);
    CHECK_THAT(resp.message, ContainsSubstring("tenant_unreadable"));
}

TEST_CASE("exchange_run_grant_refuses_an_inactive_grantor", tags) {
    exchange_fixture f("pending");
    const auto resp = run_grant_operations_service(f.caller(), signer_for_tests())
                          .exchange_run_grant(exchange_request(f.grant_id, f.tenant_id, "run-1"));
    CHECK_FALSE(resp.success);
    CHECK_THAT(resp.message, ContainsSubstring("grantor_inactive"));
}

TEST_CASE("exchange_run_grant_refuses_a_grantor_who_holds_none_of_the_role", tags) {
    exchange_fixture f;
    // The grantor loses every role, so the intersection is empty.
    const auto person_ctx = f.h.context()
                                .with_party(f.h.tenant_id(), f.party, {f.party}, f.grantor.username)
                                .with_roles({granted_code});
    authorization_service auth(person_ctx);
    for (const auto& r : auth.get_account_roles(f.grantor.id))
        auth.revoke_role(f.grantor.id, r.id);

    const auto resp = run_grant_operations_service(f.caller(), signer_for_tests())
                          .exchange_run_grant(exchange_request(f.grant_id, f.tenant_id, "run-1"));
    CHECK_FALSE(resp.success);
    CHECK_THAT(resp.message, ContainsSubstring("grant_lapsed"));
}

TEST_CASE("exchange_run_grant_refuses_a_caller_over_its_rate_limit", tags) {
    exchange_fixture f;
    const run_grant_operations_service::exchange_limits limits{
        .per_service = 1, .per_tenant = 1, .burst = 1, .period = std::chrono::seconds(60)};
    run_grant_operations_service sut(f.caller(), signer_for_tests(), limits);

    REQUIRE(sut.exchange_run_grant(exchange_request(f.grant_id, f.tenant_id, "run-1")).success);
    const auto refused =
        sut.exchange_run_grant(exchange_request(f.grant_id, f.tenant_id, "run-2"));
    CHECK_FALSE(refused.success);
    CHECK_THAT(refused.message, ContainsSubstring("unavailable"));
    CHECK_THAT(refused.message, ContainsSubstring("retry after"));
}
