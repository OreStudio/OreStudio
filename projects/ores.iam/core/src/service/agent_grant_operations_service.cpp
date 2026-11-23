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
#include "ores.iam.core/service/agent_grant_operations_service.hpp"
#include "ores.iam.api/domain/agent_key.hpp"
#include "ores.iam.api/domain/agent_key_format.hpp"
#include "ores.iam.api/domain/session.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/agent_call_log.hpp"
#include "ores.iam.core/repository/agent_token_issue_log.hpp"
#include "ores.iam.core/repository/session_repository.hpp"
#include "ores.iam.core/service/agent_grant_service.hpp"
#include "ores.iam.core/service/agent_key_secrets.hpp"
#include "ores.iam.core/service/agent_key_service.hpp"
#include "ores.iam.core/service/agent_permissions.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.security/authorization/grants.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/asio/ip/address.hpp>
#include <boost/container_hash/hash.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cctype>
#include <iterator>
#include <unordered_set>

namespace ores::iam::service {

using namespace ores::logging;
using ores::service::messaging::change_reasons::new_record;
using ores::service::messaging::change_reasons::update;
using ores::service::messaging::has_permission;

namespace {

/// The one answer to every refused key, so a caller learns nothing of which
/// tenants or keys exist.
constexpr std::string_view invalid_key = "invalid key";

constexpr std::chrono::seconds agent_limit_period{60};
constexpr int agent_exchanges_per_key = 120;
constexpr int agent_exchanges_per_tenant = 6000;

template <typename Response>
Response refuse_agent_request(std::string message) {
    Response r;
    r.success = false;
    r.message = std::move(message);
    return r;
}

bool is_active(const domain::agent_grant& grant, std::chrono::system_clock::time_point now) {
    return grant.revoked_at == std::chrono::system_clock::time_point{} && now < grant.not_after;
}

bool is_revoked(const domain::agent_key& key) {
    return key.revoked_at != std::chrono::system_clock::time_point{};
}

/// An agent's name is short and printable, so it reads as a column of the
/// sessions window and a claim cannot carry anything else.
bool is_valid_agent_name(std::string_view name) {
    if (name.empty() || name.size() > 64)
        return false;
    return std::ranges::all_of(
        name, [](unsigned char c) { return std::isalnum(c) || c == '-' || c == '_' || c == '.'; });
}

std::optional<boost::uuids::uuid> parse_uuid(const std::string& text) {
    try {
        return boost::uuids::string_generator()(text);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

/*
 * An agent's session ends with its key, because a revoked key never exchanges
 * again and so would leave the session open for ever.
 */
void end_agent_session_row(const ores::database::context& ctx,
                           const std::string& session_id,
                           std::chrono::system_clock::time_point now) {
    const auto id = parse_uuid(session_id);
    if (!id)
        return;
    repository::session_repository sessions;
    const auto open = sessions.read(ctx, *id);
    if (open && !open->end_time)
        sessions.end_session(
            ctx, open->id, open->start_time, now, open->bytes_sent, open->bytes_received);
}

void end_agent_key_session(const ores::database::context& ctx,
                           const std::string& key_id,
                           std::chrono::system_clock::time_point now) {
    if (const auto last = repository::agent_token_issue_log(ctx).last_issue(key_id))
        end_agent_session_row(ctx, last->session_id, now);
}

std::vector<std::string> codes_of(const std::vector<domain::permission>& permissions) {
    std::vector<std::string> out;
    out.reserve(permissions.size());
    for (const auto& p : permissions)
        out.push_back(p.code);
    return out;
}

}

agent_grant_operations_service::limiters::limiters()
    : per_key(agent_exchanges_per_key, agent_limit_period, agent_exchanges_per_key)
    , per_tenant(agent_exchanges_per_tenant, agent_limit_period, agent_exchanges_per_tenant) {}

const std::shared_ptr<agent_grant_operations_service::limiters>&
agent_grant_operations_service::shared_limiters() {
    static const auto instance = std::make_shared<limiters>();
    return instance;
}

agent_grant_operations_service::agent_grant_operations_service(
    const ores::database::context& ctx, std::optional<security::jwt::jwt_authenticator> signer)
    : ctx_(ctx)
    , signer_(std::move(signer))
    , limiters_(shared_limiters()) {}

std::string agent_grant_operations_service::mint_key(const ores::database::context& grant_ctx,
                                                     const domain::agent_grant& grant,
                                                     boost::uuids::uuid& key_id) {
    const auto secret = generate_agent_key_secret();
    domain::agent_key key;
    key.id = boost::uuids::random_generator()();
    key.grant_id = grant.id;
    key.secret_hash = hash_agent_key_secret(secret);
    key.change_reason_code = std::string(new_record);
    key.change_commentary = "Made for the agent " + grant.agent_name + ".";
    agent_key_service(grant_ctx).save_key(key);
    key_id = key.id;

    // The key names the tenant it was saved in. A grant made in this request
    // carries no tenant until the repository stamps one on the row.
    boost::uuids::uuid tenant{};
    if (const auto parsed = parse_uuid(grant_ctx.tenant_id().to_string()))
        tenant = *parsed;
    return domain::format_agent_key({.tenant_id = tenant, .key_id = key.id, .secret = secret});
}

messaging::create_agent_grant_response agent_grant_operations_service::create_agent_grant(
    const messaging::create_agent_grant_request& request) {
    using response = messaging::create_agent_grant_response;

    if (ctx_.acts_for_agent())
        return refuse_agent_request<response>("An agent cannot make an agent grant.");
    const auto& held = ctx_.roles();
    if (!held)
        return refuse_agent_request<response>("An agent grant needs the person's own session.");
    if (!security::authorization::grants(*held, create_permission))
        return refuse_agent_request<response>("Delegating to an agent needs " +
                                              std::string(create_permission) + ".");
    const auto party = ctx_.party_id();
    if (!party || party->is_nil())
        return refuse_agent_request<response>(
            "An agent grant needs a party: act for the party the agent works in.");
    if (!is_valid_agent_name(request.agent_name))
        return refuse_agent_request<response>(
            "An agent's name is one to sixty-four letters, digits, dots, dashes or underscores.");
    if (request.valid_days < 1 || request.valid_days > max_valid_days)
        return refuse_agent_request<response>("A grant lasts from one to " +
                                              std::to_string(max_valid_days) + " days.");

    authorization_service auth(ctx_);
    const auto grantor = auth.caller_account();
    if (!grantor)
        return refuse_agent_request<response>("No account matches the session's user, " +
                                              ctx_.actor() + ".");
    const auto role = auth.find_role_by_name(request.role);
    if (!role)
        return refuse_agent_request<response>("No role is named " + request.role + ".");
    const auto refusal = agent_role_refusal(auth.get_role_permissions(role->id), *held);
    if (!refusal.empty())
        return refuse_agent_request<response>("The role " + request.role +
                                              " cannot be granted to an agent. " + refusal);

    const auto now = std::chrono::system_clock::now();
    agent_grant_service grants(ctx_);
    std::optional<domain::agent_grant> existing;
    for (const auto& g :
         grants.list_grants_by_grantor_account_id(boost::uuids::to_string(*grantor), 0, 1000))
        if (g.party_id == *party && g.agent_name == request.agent_name)
            existing = g;
    if (existing && is_active(*existing, now))
        return refuse_agent_request<response>(
            "The agent " + request.agent_name +
            " already has an active grant: make it another key, or revoke it "
            "first.");

    domain::agent_grant grant;
    if (existing) {
        // A grant that ended is made again with the new consent; every key it
        // had is revoked first, so no old key comes back to life with it.
        grant = *existing;
        agent_key_service keys(ctx_);
        for (auto k : keys.list_keys_by_grant_id(boost::uuids::to_string(grant.id), 0, 1000))
            if (!is_revoked(k)) {
                k.revoked_at = now;
                k.change_reason_code = std::string(update);
                k.change_commentary = "Revoked: the grant was made again.";
                keys.save_key(k);
            }
        grant.change_reason_code = std::string(update);
        grant.change_commentary = "Granted again.";
    } else {
        grant.id = boost::uuids::random_generator()();
        grant.change_reason_code = std::string(new_record);
        grant.change_commentary = "Granted.";
    }
    grant.party_id = *party;
    grant.grantor_account_id = *grantor;
    grant.agent_name = request.agent_name;
    grant.role_id = role->id;
    grant.not_after = now + std::chrono::hours(24 * request.valid_days);
    grant.revoked_at = {};
    grants.save_grant(grant);

    boost::uuids::uuid key_id;
    response r;
    r.key = mint_key(ctx_, grant, key_id);
    r.success = true;
    r.grant_id = boost::uuids::to_string(grant.id);
    ORES_LOG_SEV(lg(), info) << "Agent grant " << grant.id << " for " << grant.agent_name
                             << " made by " << ctx_.actor() << " under role " << request.role
                             << " until the grant's expiry; key " << key_id;
    return r;
}

messaging::create_agent_key_response agent_grant_operations_service::create_agent_key(
    const messaging::create_agent_key_request& request) {
    using response = messaging::create_agent_key_response;

    if (ctx_.acts_for_agent())
        return refuse_agent_request<response>("An agent cannot make a key.");
    const auto& held = ctx_.roles();
    if (!held || !security::authorization::grants(*held, create_permission))
        return refuse_agent_request<response>("Making a key needs " +
                                              std::string(create_permission) + ".");
    const auto id = parse_uuid(request.grant_id);
    if (!id)
        return refuse_agent_request<response>("Not a grant id: " + request.grant_id + ".");
    auto grant = agent_grant_service(ctx_).find_grant(*id);
    const auto caller = authorization_service(ctx_).caller_account();
    if (!grant || !caller || grant->grantor_account_id != *caller)
        return refuse_agent_request<response>("You have no agent grant with the id " +
                                              request.grant_id + ".");
    if (!is_active(*grant, std::chrono::system_clock::now()))
        return refuse_agent_request<response>("The grant has ended; make a new grant.");

    boost::uuids::uuid key_id;
    response r;
    r.key = mint_key(ctx_, *grant, key_id);
    r.key_id = boost::uuids::to_string(key_id);
    r.success = true;
    ORES_LOG_SEV(lg(), info) << "Key " << key_id << " made for agent grant " << grant->id;
    return r;
}

messaging::revoke_agent_grant_response agent_grant_operations_service::revoke_agent_grant(
    const messaging::revoke_agent_grant_request& request) {
    using response = messaging::revoke_agent_grant_response;

    if (ctx_.acts_for_agent())
        return refuse_agent_request<response>("An agent cannot revoke a grant.");
    const auto id = parse_uuid(request.grant_id);
    if (!id)
        return refuse_agent_request<response>("Not a grant id: " + request.grant_id + ".");
    auto grant = agent_grant_service(ctx_).find_grant(*id);
    if (!grant)
        return refuse_agent_request<response>("No agent grant has the id " + request.grant_id +
                                              ".");
    const auto caller = authorization_service(ctx_).caller_account();
    const bool own = caller && grant->grantor_account_id == *caller;
    if (!own && !has_permission(ctx_, revoke_permission))
        return refuse_agent_request<response>("Only the grant's person, or a holder of " +
                                              std::string(revoke_permission) + ", may revoke it.");

    response r;
    r.success = true;
    if (grant->revoked_at != std::chrono::system_clock::time_point{}) {
        r.message = "The grant was already revoked.";
        return r;
    }

    // A save stamps the session's party on the row, so an administrator acting
    // for another party saves through a context narrowed to the grant's party.
    const auto grant_ctx =
        ctx_.with_party(ctx_.tenant_id(), grant->party_id, ctx_.visible_party_ids(), ctx_.actor());
    const auto now = std::chrono::system_clock::now();
    const auto reason = request.reason.empty() ? std::string("revoked") : request.reason;
    agent_key_service keys(grant_ctx);
    for (auto k : keys.list_keys_by_grant_id(boost::uuids::to_string(grant->id), 0, 1000))
        if (!is_revoked(k)) {
            k.revoked_at = now;
            k.change_reason_code = std::string(update);
            k.change_commentary = "Revoked with its grant: " + reason;
            keys.save_key(k);
            end_agent_key_session(grant_ctx, boost::uuids::to_string(k.id), now);
        }
    grant->revoked_at = now;
    grant->change_reason_code = std::string(update);
    grant->change_commentary = "Revoked: " + reason;
    agent_grant_service(grant_ctx).save_grant(*grant);

    ORES_LOG_SEV(lg(), info) << "Agent grant " << grant->id << " revoked by " << ctx_.actor()
                             << ": " << reason;
    return r;
}

messaging::revoke_agent_key_response agent_grant_operations_service::revoke_agent_key(
    const messaging::revoke_agent_key_request& request) {
    using response = messaging::revoke_agent_key_response;

    if (ctx_.acts_for_agent())
        return refuse_agent_request<response>("An agent cannot revoke a key.");
    const auto id = parse_uuid(request.key_id);
    if (!id)
        return refuse_agent_request<response>("Not a key id: " + request.key_id + ".");
    auto key = agent_key_service(ctx_).find_key(*id);
    if (!key)
        return refuse_agent_request<response>("No agent key has the id " + request.key_id + ".");
    auto grant = agent_grant_service(ctx_).find_grant(key->grant_id);
    const auto caller = authorization_service(ctx_).caller_account();
    const bool own = grant && caller && grant->grantor_account_id == *caller;
    if (!own && !has_permission(ctx_, revoke_permission))
        return refuse_agent_request<response>("Only the grant's person, or a holder of " +
                                              std::string(revoke_permission) +
                                              ", may revoke its key.");

    response r;
    r.success = true;
    if (is_revoked(*key)) {
        r.message = "The key was already revoked.";
        return r;
    }
    key->revoked_at = std::chrono::system_clock::now();
    key->change_reason_code = std::string(update);
    key->change_commentary =
        "Revoked: " + (request.reason.empty() ? std::string("revoked") : request.reason);
    agent_key_service(ctx_).save_key(*key);
    end_agent_key_session(ctx_, boost::uuids::to_string(key->id), key->revoked_at);
    ORES_LOG_SEV(lg(), info) << "Agent key " << key->id << " revoked by " << ctx_.actor();
    return r;
}

messaging::revoke_agent_key_response
agent_grant_operations_service::end_agent_session(const std::string& session_id) {
    using response = messaging::revoke_agent_key_response;
    const auto key_id = repository::agent_token_issue_log(ctx_).key_of_session(session_id);
    if (!key_id)
        return refuse_agent_request<response>("No agent key opened session " + session_id + ".");
    messaging::revoke_agent_key_request request;
    request.key_id = *key_id;
    request.reason = "Its session " + session_id + " was ended.";
    auto r = revoke_agent_key(request);
    if (r.success)
        end_agent_session_row(ctx_, session_id, std::chrono::system_clock::now());
    return r;
}

messaging::list_own_agent_grants_response agent_grant_operations_service::list_own_agent_grants(
    const messaging::list_own_agent_grants_request&) {
    using response = messaging::list_own_agent_grants_response;

    if (ctx_.acts_for_agent())
        return refuse_agent_request<response>("An agent cannot read agent grants.");
    const auto& held = ctx_.roles();
    if (!held || !security::authorization::grants(*held, create_permission))
        return refuse_agent_request<response>("Reading your agent grants needs " +
                                              std::string(create_permission) + ".");
    const auto caller = authorization_service(ctx_).caller_account();
    if (!caller)
        return refuse_agent_request<response>("No account matches the session's user, " +
                                              ctx_.actor() + ".");

    response r;
    r.success = true;
    r.grants = agent_grant_service(ctx_).list_grants_by_grantor_account_id(
        boost::uuids::to_string(*caller), 0, 1000);
    agent_key_service keys(ctx_);
    for (const auto& g : r.grants)
        for (auto k : keys.list_keys_by_grant_id(boost::uuids::to_string(g.id), 0, 1000)) {
            // The hash proves nothing to a reader and is no business of the page.
            k.secret_hash.clear();
            r.keys.push_back(std::move(k));
        }
    return r;
}

messaging::list_own_agent_activity_response agent_grant_operations_service::list_own_agent_activity(
    const messaging::list_own_agent_activity_request&) {
    using response = messaging::list_own_agent_activity_response;

    if (ctx_.acts_for_agent())
        return refuse_agent_request<response>("An agent cannot read agent activity.");
    if (!has_permission(ctx_, create_permission))
        return refuse_agent_request<response>("Reading your agents' activity needs " +
                                              std::string(create_permission) + ".");
    const auto caller = authorization_service(ctx_).caller_account();
    if (!caller)
        return refuse_agent_request<response>("No account matches the session's user, " +
                                              ctx_.actor() + ".");
    response r;
    r.success = true;
    r.calls = repository::agent_call_log(ctx_).calls_for(boost::uuids::to_string(*caller), 500);
    return r;
}

messaging::list_every_agent_grant_response agent_grant_operations_service::list_every_agent_grant(
    const messaging::list_every_agent_grant_request&) {
    using response = messaging::list_every_agent_grant_response;

    if (ctx_.acts_for_agent())
        return refuse_agent_request<response>("An agent cannot read agent grants.");
    if (!has_permission(ctx_, read_permission))
        return refuse_agent_request<response>("Reading every agent grant needs " +
                                              std::string(read_permission) + ".");

    // Read in pages until the store runs out, so an administrator's overview
    // never silently stops short; keys come in one paged read, not one per grant.
    constexpr std::uint32_t page = 1000;
    response r;
    r.success = true;
    agent_grant_service grants(ctx_);
    for (std::uint32_t offset = 0;; offset += page) {
        auto batch = grants.list_grants(offset, page);
        const auto read = batch.size();
        r.grants.insert(r.grants.end(),
                        std::make_move_iterator(batch.begin()),
                        std::make_move_iterator(batch.end()));
        if (read < page)
            break;
    }
    std::unordered_set<boost::uuids::uuid, boost::hash<boost::uuids::uuid>> listed;
    for (const auto& g : r.grants)
        listed.insert(g.id);
    agent_key_service keys(ctx_);
    for (std::uint32_t offset = 0;; offset += page) {
        auto batch = keys.list_keys(offset, page);
        const auto read = batch.size();
        for (auto& k : batch)
            if (listed.contains(k.grant_id)) {
                k.secret_hash.clear();
                r.keys.push_back(std::move(k));
            }
        if (read < page)
            break;
    }
    return r;
}

messaging::exchange_agent_key_response agent_grant_operations_service::exchange_agent_key(
    const messaging::exchange_agent_key_request& request) {
    using response = messaging::exchange_agent_key_response;

    if (!signer_)
        return refuse_agent_request<response>("This deployment cannot issue agent tokens.");
    if (ctx_.acts_for_agent() || !has_permission(ctx_, exchange_permission))
        return refuse_agent_request<response>("The caller needs " +
                                              std::string(exchange_permission) + ".");

    const auto now = std::chrono::system_clock::now();
    repository::agent_token_issue_log log(ctx_);
    const auto refuse_key = [&](const std::string& tenant,
                                const std::string& party,
                                const std::string& grant,
                                const std::string& key,
                                const std::string& account,
                                std::string_view reason) {
        try {
            log.record({.issued_at = now,
                        .tenant_id = tenant,
                        .party_id = party,
                        .grant_id = grant,
                        .key_id = key,
                        .session_id = "",
                        .grantor_account_id = account,
                        .client_address = request.client_address,
                        .outcome = std::string(repository::agent_token_issue_log::refused),
                        .reason = std::string(reason)});
        } catch (const std::exception& e) {
            ORES_LOG_SEV(lg(), warn)
                << "Could not record a refused agent key exchange: " << e.what();
        }
        ORES_LOG_SEV(lg(), info) << "Refused an agent key exchange from " << request.client_address
                                 << ": " << reason;
        return refuse_agent_request<response>(std::string(invalid_key));
    };

    // The form alone is checked before anything is read, and a key that is not
    // well formed names no tenant whose log could hold its refusal.
    const auto parts = domain::parse_agent_key(request.key);
    if (!parts)
        return refuse_key("", "", "", "", "", "malformed");
    const auto tenant =
        utility::uuid::tenant_id::from_string(boost::uuids::to_string(parts->tenant_id));
    if (!tenant)
        return refuse_key("", "", "", "", "", "tenant_unreadable");
    const auto tenant_text = tenant->to_string();
    const auto key_text = boost::uuids::to_string(parts->key_id);
    const auto tctx = ctx_.with_tenant(*tenant, ctx_.actor());

    const auto key = agent_key_service(tctx).find_key(parts->key_id);
    if (!key)
        return refuse_key("", "", "", "", "", "key_unknown");

    // Budgets are spent only on a key that exists in a tenant that exists, so
    // naming a tenant in bad keys costs nothing and locks no tenant out. The
    // budget is the key's from one address: a key's id is not a secret, so
    // wrong secrets sent from elsewhere must not lock its agent out.
    const auto per_key = limiters_->per_key.allow(key_text + "@" + request.client_address);
    if (!per_key.allowed)
        return refuse_key(tenant_text, "", "", key_text, "", "rate_limited");
    if (!agent_key_secret_matches(parts->secret, key->secret_hash))
        return refuse_key(tenant_text, "", "", key_text, "", "wrong_secret");
    if (is_revoked(*key))
        return refuse_key(tenant_text, "", "", key_text, "", "key_revoked");

    const auto grant = agent_grant_service(tctx).find_grant(key->grant_id);
    if (!grant)
        return refuse_key(tenant_text, "", "", key_text, "", "grant_unknown");
    const auto grant_text = boost::uuids::to_string(grant->id);
    const auto party_text = boost::uuids::to_string(grant->party_id);
    const auto account_text = boost::uuids::to_string(grant->grantor_account_id);
    if (!is_active(*grant, now))
        return refuse_key(tenant_text,
                          party_text,
                          grant_text,
                          key_text,
                          account_text,
                          grant->revoked_at != std::chrono::system_clock::time_point{} ?
                              "grant_revoked" :
                              "grant_expired");

    const auto accounts = repository::account_repository().read_latest(tctx, account_text);
    if (accounts.empty() || accounts.front().account_status != "active")
        return refuse_key(
            tenant_text, party_text, grant_text, key_text, account_text, "person_inactive");
    const auto& person = accounts.front();

    authorization_service auth(tctx);
    const auto catalogue = codes_of(auth.list_permissions());
    const auto permissions = agent_permissions(catalogue,
                                               auth.get_role_permissions(grant->role_id),
                                               auth.get_effective_permissions(person.id));
    if (permissions.empty())
        return refuse_key(
            tenant_text, party_text, grant_text, key_text, account_text, "grant_lapsed");

    const auto per_tenant = limiters_->per_tenant.allow(tenant_text);
    if (!per_tenant.allowed)
        return refuse_agent_request<response>(
            "[unavailable] Too many agent exchanges for the tenant; retry after " +
            std::to_string(per_tenant.retry_after.count()) + " ms.");

    // The key's session: the one its last issue named, while it is open and
    // the key has not been idle past the limit; otherwise a new one.
    repository::session_repository sessions;
    std::optional<domain::session> session;
    if (const auto last = log.last_issue(key_text)) {
        if (const auto sid = parse_uuid(last->session_id)) {
            auto open = sessions.read(tctx, *sid);
            if (open && !open->end_time) {
                if (now - last->issued_at <= session_idle_limit)
                    session = open;
                else
                    sessions.end_session(tctx,
                                         open->id,
                                         open->start_time,
                                         last->issued_at,
                                         open->bytes_sent,
                                         open->bytes_received);
            }
        }
    }
    if (!session) {
        boost::system::error_code ec;
        auto address = boost::asio::ip::make_address(request.client_address, ec);
        if (ec)
            address = boost::asio::ip::make_address("0.0.0.0");
        domain::session s;
        s.tenant_id = *tenant;
        s.id = boost::uuids::random_generator()();
        s.start_time = now;
        s.account_id = person.id;
        s.client_ip = address;
        s.client_identifier = grant->agent_name;
        s.protocol = "mcp";
        s.agent_grant_id = grant->id;
        sessions.write(tctx, s);
        session = s;
    }

    security::jwt::jwt_claims claims;
    claims.subject = account_text;
    claims.username = person.username;
    claims.tenant_id = tenant_text;
    claims.party_id = party_text;
    claims.visible_party_ids = {party_text};
    claims.roles = permissions;
    claims.act = "agent:" + grant_text;
    claims.grant_id = grant_text;
    claims.session_id = boost::uuids::to_string(session->id);
    claims.session_start_time = session->start_time;
    claims.audience = "HttpService";
    claims.issued_at = now;
    claims.expires_at = now + token_lifetime;
    const auto token = signer_->create_token(claims);
    if (!token)
        return refuse_agent_request<response>("The agent token could not be signed.");

    // The row is appended before the token is handed over, so no agent token
    // exists that the log does not name.
    log.record({.issued_at = now,
                .tenant_id = tenant_text,
                .party_id = party_text,
                .grant_id = grant_text,
                .key_id = key_text,
                .session_id = boost::uuids::to_string(session->id),
                .grantor_account_id = account_text,
                .client_address = request.client_address,
                .outcome = std::string(repository::agent_token_issue_log::issued),
                .reason = ""});

    response r;
    r.success = true;
    r.token = *token;
    r.expires_at =
        std::chrono::duration_cast<std::chrono::seconds>(claims.expires_at.time_since_epoch())
            .count();
    return r;
}

}
