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
#include "ores.iam.api/domain/agent_key_format.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.api/generators/permission_generator.hpp"
#include "ores.iam.api/generators/role_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/permission_repository.hpp"
#include "ores.iam.core/repository/role_permission_repository.hpp"
#include "ores.iam.core/repository/role_repository.hpp"
#include "ores.iam.core/repository/session_repository.hpp"
#include "ores.iam.core/service/agent_grant_operations_service.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.logging/logger.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>

namespace {

using ores::database::context;
using ores::iam::domain::account;
using ores::iam::domain::role;
using ores::iam::service::agent_grant_operations_service;
using ores::iam::service::authorization_service;
using ores::testing::database_helper;
using ores::utility::generation::generation_context;
using namespace ores::logging;

const std::string agent_exchange_read_code("refdata::currencies:read");
const std::string agent_exchange_write_code("refdata::currencies:write");
const std::string agent_exchange_code("iam::agent_keys:exchange");
const std::string agent_create_code("iam::agent_grants:create");

ores::security::jwt::jwt_authenticator agent_exchange_signer() {
    return ores::security::jwt::jwt_authenticator::create_hs256("ores-iam-agent-key-tests");
}

account
agent_exchange_write_account(database_helper& h, generation_context& gen, const std::string& type) {
    ores::iam::repository::account_repository repo;
    auto value = ores::iam::generators::generate_synthetic_account(gen);
    value.account_status = "active";
    value.account_type = type;
    repo.write(h.context(), value);
    return value;
}

ores::iam::domain::permission
agent_exchange_permission(database_helper& h, generation_context& gen, const std::string& code) {
    ores::iam::repository::permission_repository repo;
    auto existing = repo.read_latest_by_code(h.context(), code);
    if (!existing.empty())
        return existing.front();
    auto p = ores::iam::generators::generate_synthetic_permission(gen);
    p.code = code;
    repo.write(h.context(), p);
    return p;
}

role agent_exchange_role(database_helper& h,
                         generation_context& gen,
                         const std::string& name,
                         const std::vector<std::string>& codes) {
    ores::iam::repository::role_repository roles;
    ores::iam::repository::role_permission_repository links(h.context());
    auto r = ores::iam::generators::generate_synthetic_role(gen);
    r.name = name;
    roles.write(h.context(), r);
    for (const auto& code : codes) {
        ores::iam::domain::role_permission link;
        link.tenant_id = h.tenant_id().to_string();
        link.role_id = r.id;
        link.permission_id = agent_exchange_permission(h, gen, code).id;
        link.modified_by = h.db_user();
        link.change_reason_code = "system.test";
        link.change_commentary = "Synthetic test data";
        links.write(link);
    }
    return r;
}

/**
 * @brief A person who reads and writes currencies, a grant of a read-only role
 * to their agent, and the gateway that exchanges its key.
 */
struct agent_exchange_fixture {
    database_helper h;
    generation_context gen;
    account person;
    account gateway;
    boost::uuids::uuid party;
    role person_role;
    role agent_role;
    std::optional<context> person_context;
    std::optional<context> gateway_context;
    std::string grant_id;
    std::string key;

    agent_exchange_fixture()
        : h()
        , gen(ores::testing::make_generation_context(h))
        , party(boost::uuids::random_generator()()) {
        person = agent_exchange_write_account(h, gen, "user");
        gateway = agent_exchange_write_account(h, gen, "service");
        const auto suffix =
            boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 8);
        person_role = agent_exchange_role(h,
                                          gen,
                                          "AgentTestTrader" + suffix,
                                          {agent_exchange_read_code, agent_exchange_write_code});
        agent_role =
            agent_exchange_role(h, gen, "AgentTestReader" + suffix, {agent_exchange_read_code});
        authorization_service(h.context())
            .assign_role(
                person.id, person_role.id, h.db_user(), "Synthetic test data", "system.test");

        person_context = h.context()
                             .with_party(h.tenant_id(), party, {party}, person.username)
                             .with_roles({agent_create_code, agent_exchange_read_code});
        gateway_context = h.context()
                              .with_party(h.tenant_id(), party, {party}, gateway.username)
                              .with_roles({agent_exchange_code});

        ores::iam::messaging::create_agent_grant_request req;
        req.agent_name = "test-agent";
        req.role = agent_role.name;
        req.valid_days = 30;
        const auto made = agent_grant_operations_service(*person_context).create_agent_grant(req);
        INFO(made.message);
        REQUIRE(made.success);
        grant_id = made.grant_id;
        key = made.key;
    }

    ores::iam::messaging::exchange_agent_key_response exchange(const std::string& with_key) {
        ores::iam::messaging::exchange_agent_key_request req;
        req.key = with_key;
        req.client_address = "203.0.113.7";
        return agent_grant_operations_service(*gateway_context, agent_exchange_signer())
            .exchange_agent_key(req);
    }
};

}

TEST_CASE("agent_key_names_the_tenant_its_grant_was_made_in", "[service][agent_grants]") {
    agent_exchange_fixture f;
    const auto parts = ores::iam::domain::parse_agent_key(f.key);
    REQUIRE(parts);
    CHECK(boost::uuids::to_string(parts->tenant_id) == f.h.tenant_id().to_string());
}

TEST_CASE("agent_key_exchange_issues_the_person_s_token_made_by_the_agent",
          "[service][agent_grants]") {
    auto lg(make_logger(test_suite));
    agent_exchange_fixture f;

    const auto resp = f.exchange(f.key);
    ORES_LOG_SEV(lg, info) << "Response: " << resp.message;
    REQUIRE(resp.success);
    const auto claims = agent_exchange_signer().validate(resp.token);
    REQUIRE(claims);
    CHECK(claims->subject == boost::uuids::to_string(f.person.id));
    CHECK(claims->username == f.person.username);
    CHECK(claims->tenant_id == f.h.tenant_id().to_string());
    CHECK(claims->act == "agent:" + f.grant_id);
    CHECK(claims->grant_id == f.grant_id);
    CHECK(claims->audience == "HttpService");
    CHECK(claims->expires_at - claims->issued_at == std::chrono::seconds(300));
    // The role's read, which the person holds; never the person's write.
    CHECK(claims->roles == std::vector<std::string>{agent_exchange_read_code});
    REQUIRE(claims->session_id.has_value());

    // The agent's session names the grant, the agent and MCP.
    const auto sid = boost::uuids::string_generator()(*claims->session_id);
    const auto session = ores::iam::repository::session_repository().read(
        f.h.context().with_tenant(f.h.tenant_id(), f.person.username), sid);
    REQUIRE(session);
    CHECK(session->protocol == "mcp");
    CHECK(session->client_identifier == "test-agent");
    REQUIRE(session->agent_grant_id.has_value());
    CHECK(boost::uuids::to_string(*session->agent_grant_id) == f.grant_id);

    // A second exchange reuses the open session.
    const auto again = f.exchange(f.key);
    REQUIRE(again.success);
    const auto again_claims = agent_exchange_signer().validate(again.token);
    REQUIRE(again_claims);
    CHECK(again_claims->session_id == claims->session_id);
}

TEST_CASE("agent_key_exchange_refuses_a_wrong_secret_an_unknown_tenant_and_a_revoked_key",
          "[service][agent_grants]") {
    agent_exchange_fixture f;
    const auto parts = ores::iam::domain::parse_agent_key(f.key);
    REQUIRE(parts);

    auto wrong = *parts;
    wrong.secret.back() = wrong.secret.back() == '0' ? '1' : '0';
    const auto wrong_secret = f.exchange(ores::iam::domain::format_agent_key(wrong));
    CHECK_FALSE(wrong_secret.success);
    CHECK(wrong_secret.message == "invalid key");

    auto elsewhere = *parts;
    elsewhere.tenant_id = boost::uuids::random_generator()();
    const auto unknown_tenant = f.exchange(ores::iam::domain::format_agent_key(elsewhere));
    CHECK_FALSE(unknown_tenant.success);
    CHECK(unknown_tenant.message == wrong_secret.message);

    ores::iam::messaging::revoke_agent_key_request revoke;
    revoke.key_id = boost::uuids::to_string(parts->key_id);
    revoke.reason = "test";
    REQUIRE(agent_grant_operations_service(*f.person_context).revoke_agent_key(revoke).success);
    const auto revoked = f.exchange(f.key);
    CHECK_FALSE(revoked.success);
    CHECK(revoked.message == wrong_secret.message);
}

TEST_CASE("an_agent_cannot_make_a_grant", "[service][agent_grants]") {
    agent_exchange_fixture f;
    const auto token = f.exchange(f.key);
    REQUIRE(token.success);
    const auto agent_ctx = f.person_context->with_delegated_actor("agent:" + f.grant_id);
    ores::iam::messaging::create_agent_grant_request req;
    req.agent_name = "nested";
    req.role = f.agent_role.name;
    req.valid_days = 1;
    CHECK_FALSE(agent_grant_operations_service(agent_ctx).create_agent_grant(req).success);
}
