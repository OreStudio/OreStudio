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
#ifndef ORES_IAM_MESSAGING_AUTH_HANDLER_HPP
#define ORES_IAM_MESSAGING_AUTH_HANDLER_HPP

#include <chrono>
#include <stdexcept>
#include <boost/asio/ip/address.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include <span>
#include <rfl/json.hpp>
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.iam.api/messaging/signup_protocol.hpp"
#include "ores.iam.api/domain/role.hpp"
#include "ores.iam.api/domain/session.hpp"
#include "ores.iam.core/repository/account_party_repository.hpp"
#include "ores.iam.core/repository/session_repository.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.iam.core/service/account_service.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.iam.core/service/account_setup_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.variability/service/system_settings_service.hpp"
#include "ores.iam.core/domain/token_settings.hpp"
#include "ores.iam.core/repository/auth_event_repository.hpp"

namespace ores::iam::messaging {

namespace {

inline auto& auth_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.iam.messaging.auth_handler");
    return instance;
}

inline std::string auth_extract_bearer_token(const ores::nats::message& msg) {
    auto it = msg.headers.find("Authorization");
    if (it == msg.headers.end())
        return {};
    const auto& val = it->second;
    constexpr std::string_view prefix = "Bearer ";
    if (!val.starts_with(prefix))
        return {};
    return val.substr(prefix.size());
}

inline std::vector<boost::uuids::uuid> auth_compute_visible_party_ids(
    const ores::database::context& ctx,
    const boost::uuids::uuid& party_id) {
    try {
        refdata::repository::party_repository repo(ctx);
        auto ids = repo.read_descendants(party_id);
        if (ids.empty())
            return {party_id};
        return ids;
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), warn)
            << "Failed to compute visible party IDs: " << e.what();
        return {party_id};
    }
}

inline std::optional<refdata::domain::party> auth_lookup_party(
    const ores::database::context& ctx,
    const boost::uuids::uuid& party_id) {
    try {
        refdata::repository::party_repository repo(ctx);
        auto parties = repo.read_latest(party_id);
        if (!parties.empty())
            return parties.front();
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), warn)
            << "Failed to look up party: " << e.what();
    }
    return std::nullopt;
}

inline std::string auth_lookup_tenant_name(
    const ores::database::context& ctx,
    const boost::uuids::uuid& tenant_id) {
    try {
        repository::tenant_repository repo(ctx);
        auto tenants = repo.read_latest(tenant_id);
        if (!tenants.empty())
            return tenants.front().name;
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), warn)
            << "Failed to look up tenant name: " << e.what();
    }
    return {};
}

inline std::optional<ores::iam::domain::tenant> auth_lookup_tenant_by_hostname(
    const ores::database::context& ctx, const std::string& hostname) {
    try {
        repository::tenant_repository repo(ctx);
        auto tenants = repo.read_latest_by_hostname(hostname);
        if (!tenants.empty())
            return tenants.front();
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), warn)
            << "Failed to look up tenant by hostname: " << e.what();
    }
    return std::nullopt;
}

inline bool auth_is_tenant_bootstrap_mode(
    const ores::database::context& ctx,
    const std::string& tenant_id_str) {
    try {
        auto tid_result = ores::utility::uuid::tenant_id::from_string(tenant_id_str);
        if (!tid_result) return false;
        auto tenant_ctx = ctx.with_tenant(*tid_result, "");
        variability::service::system_settings_service sfs(tenant_ctx, tenant_id_str);
        sfs.refresh();
        return sfs.is_bootstrap_mode_enabled();
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), warn)
            << "Failed to check tenant bootstrap mode: " << e.what();
    }
    return false;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;

class auth_handler {
public:
    auth_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer)
        : nats_(nats), ctx_(std::move(ctx)), signer_(std::move(signer)) {
        reload_token_settings();
    }

    void reload_token_settings() {
        try {
            variability::service::system_settings_service svc(ctx_);
            svc.refresh();
            token_settings_ = domain::token_settings::load(svc);
        } catch (const std::exception& e) {
            using namespace ores::logging;
            BOOST_LOG_SEV(auth_handler_lg(), warn)
                << "Failed to load token settings, using defaults: " << e.what();
        }
    }

    void signup(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), debug) << "Handling " << msg.subject;
        namespace svc_acct = ores::iam::domain::service_accounts;
        auto req = decode<signup_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(auth_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            service::account_service acct_svc(ctx_);
            auto auth_svc =
                std::make_shared<service::authorization_service>(ctx_);
            service::account_setup_service setup_svc(acct_svc, auth_svc);
            auto acct = setup_svc.create_account(
                req->principal, req->email, req->password, svc_acct::iam);
            BOOST_LOG_SEV(auth_handler_lg(), debug)
                << "Completed " << msg.subject;
            record_auth_event(ctx_, "signup_success", [&](auto& ev_repo) {
                ev_repo.record_signup_success(
                    std::chrono::system_clock::now(),
                    acct.tenant_id.to_string(),
                    boost::uuids::to_string(acct.id),
                    acct.username);
            });
            reply(nats_, msg, signup_response{
                .success = true,
                .account_id = boost::uuids::to_string(acct.id)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auth_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            record_auth_event(ctx_, "signup_failure", [&](auto& ev_repo) {
                ev_repo.record_signup_failure(
                    std::chrono::system_clock::now(),
                    "", req->principal, e.what());
            });
            reply(nats_, msg, signup_response{
                .success = false, .message = e.what()});
        }
    }

    void login(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<login_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(auth_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            // Parse principal: split username@hostname for tenant routing
            std::string username = req->principal;
            ores::database::context login_ctx = ctx_;
            const auto at_pos = req->principal.rfind('@');
            if (at_pos != std::string::npos) {
                username = req->principal.substr(0, at_pos);
                const auto hostname = req->principal.substr(at_pos + 1);
                if (auto t = auth_lookup_tenant_by_hostname(ctx_, hostname)) {
                    auto tid_result =
                        ores::utility::uuid::tenant_id::from_uuid(t->id);
                    if (tid_result)
                        login_ctx = ctx_.with_tenant(*tid_result, "");
                }
            }

            service::account_service svc(login_ctx);
            auto ip = boost::asio::ip::address_v4::loopback();
            auto acct = svc.login(username, req->password, ip);

            // Check if this non-system tenant needs its provisioning wizard
            const bool in_tenant_bootstrap =
                !acct.tenant_id.is_system() &&
                auth_is_tenant_bootstrap_mode(
                    login_ctx, acct.tenant_id.to_string());

            repository::account_party_repository ap_repo(login_ctx);
            auto account_parties =
                ap_repo.read_latest_by_account(acct.id);

            // Every fully-provisioned account must have at least one party.
            // A missing party is a misconfiguration — reject the login so the
            // issue surfaces immediately rather than producing a confusing
            // partial session.
            if (account_parties.empty()) {
                BOOST_LOG_SEV(auth_handler_lg(), warn)
                    << "Login rejected for " << username
                    << ": account has no party assignment";
                throw std::runtime_error(
                    "Account has no party assignment. "
                    "Please contact your administrator.");
            }

            // Create a session record so that analytics, session listings,
            // and logout end-time tracking all work correctly.
            const auto now = std::chrono::system_clock::now();
            boost::uuids::random_generator uuid_gen;
            domain::session sess;
            sess.id = uuid_gen();
            sess.account_id = acct.id;
            sess.tenant_id = acct.tenant_id;
            sess.start_time = now;
            sess.username = acct.username;
            sess.protocol = domain::session_protocol::http;
            sess.client_ip = ip;
            // party_id set below once we know which party is active
            try {
                repository::session_repository sess_repo(login_ctx);
                sess_repo.create(sess);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(auth_handler_lg(), warn)
                    << "Failed to create session record: " << e.what();
            }
            const auto session_id_str = boost::uuids::to_string(sess.id);

            if (account_parties.size() == 1) {
                const auto& party_id =
                    account_parties.front().party_id;
                auto visible = auth_compute_visible_party_ids(
                    login_ctx, party_id);

                security::jwt::jwt_claims claims;
                claims.subject = boost::uuids::to_string(acct.id);
                claims.issued_at = now;
                claims.expires_at = now + std::chrono::seconds(
                    token_settings_.access_lifetime_s);
                claims.username = acct.username;
                claims.email = acct.email;
                claims.tenant_id = acct.tenant_id.to_string();
                claims.party_id = boost::uuids::to_string(party_id);
                claims.session_id = session_id_str;
                claims.session_start_time = now;
                for (const auto& vid : visible)
                    claims.visible_party_ids.push_back(
                        boost::uuids::to_string(vid));
                auto token = signer_.create_token(claims).value_or("");

                login_response resp;
                resp.success = true;
                resp.token = token;
                resp.account_id = boost::uuids::to_string(acct.id);
                resp.tenant_id = acct.tenant_id.to_string();
                resp.username = acct.username;
                resp.email = acct.email;
                resp.selected_party_id = boost::uuids::to_string(party_id);
                resp.tenant_bootstrap_mode = in_tenant_bootstrap;
                resp.access_lifetime_s = token_settings_.access_lifetime_s;
                for (const auto& ap : account_parties) {
                    auto p = auth_lookup_party(login_ctx, ap.party_id);
                    resp.available_parties.push_back(party_summary{
                        .id = boost::uuids::to_string(ap.party_id),
                        .name = p ? p->full_name : std::string{},
                        .party_category =
                            p ? p->party_category : std::string{}
                    });
                }
                BOOST_LOG_SEV(auth_handler_lg(), debug)
                    << "Completed " << msg.subject;
                record_auth_event(login_ctx, "login_success", [&](auto& ev_repo) {
                    ev_repo.record_login_success(
                        now,
                        acct.tenant_id.to_string(),
                        boost::uuids::to_string(acct.id),
                        acct.username,
                        session_id_str,
                        boost::uuids::to_string(party_id));
                });
                reply(nats_, msg, resp);
            } else {
                // Multiple parties: issue a short-lived select-party token.
                security::jwt::jwt_claims claims;
                claims.subject = boost::uuids::to_string(acct.id);
                claims.issued_at = now;
                claims.expires_at = now + std::chrono::seconds(
                    token_settings_.party_selection_lifetime_s);
                claims.audience = "select_party_only";
                claims.username = acct.username;
                claims.email = acct.email;
                claims.tenant_id = acct.tenant_id.to_string();
                claims.session_id = session_id_str;
                claims.session_start_time = now;
                auto token = signer_.create_token(claims).value_or("");

                login_response resp;
                resp.success = true;
                resp.token = token;
                resp.account_id = boost::uuids::to_string(acct.id);
                resp.tenant_id = acct.tenant_id.to_string();
                resp.username = acct.username;
                resp.email = acct.email;
                resp.tenant_bootstrap_mode = in_tenant_bootstrap;
                resp.access_lifetime_s = token_settings_.party_selection_lifetime_s;
                for (const auto& ap : account_parties) {
                    auto p = auth_lookup_party(login_ctx, ap.party_id);
                    resp.available_parties.push_back(party_summary{
                        .id = boost::uuids::to_string(ap.party_id),
                        .name = p ? p->full_name : std::string{},
                        .party_category =
                            p ? p->party_category : std::string{}
                    });
                }
                BOOST_LOG_SEV(auth_handler_lg(), debug)
                    << "Completed " << msg.subject;
                // Multi-party: login_success recorded after party selection
                reply(nats_, msg, resp);
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auth_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            record_auth_event(ctx_, "login_failure", [&](auto& ev_repo) {
                ev_repo.record_login_failure(
                    std::chrono::system_clock::now(),
                    "", req->principal, e.what());
            });
            login_response resp;
            resp.success = false;
            resp.error_message = e.what();
            reply(nats_, msg, resp);
        }
    }

    void public_key(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), debug) << "Handling " << msg.subject;
        if (msg.reply_subject.empty()) return;
        try {
            auto pub_key = signer_.get_public_key_pem();
            if (pub_key.empty())
                throw std::runtime_error(
                    "No RSA private key configured for JWT signing. "
                    "Run generate_keys.sh in publish/bin/ to generate "
                    "the key, then restart the IAM service.");
            const auto json =
                std::string("{\"public_key\":") + rfl::json::write(pub_key) + "}";
            const auto* p = reinterpret_cast<const std::byte*>(json.data());
            nats_.publish(msg.reply_subject,
                std::span<const std::byte>(p, json.size()));
            BOOST_LOG_SEV(auth_handler_lg(), debug)
                << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auth_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
        }
    }

    void logout(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), debug) << "Handling " << msg.subject;
        auto token = auth_extract_bearer_token(msg);
        try {
            if (!token.empty()) {
                auto claims_result = signer_.validate(token);
                if (claims_result) {
                    boost::uuids::string_generator sg;
                    // Update the login_info online flag
                    try {
                        auto account_id = sg(claims_result->subject);
                        service::account_service svc(ctx_);
                        svc.logout(account_id);
                    } catch (const std::exception& e) {
                        BOOST_LOG_SEV(auth_handler_lg(), warn)
                            << "Failed to update logout state: " << e.what();
                    }
                    // Persist session end time using the IDs embedded in
                    // the JWT at login.
                    if (claims_result->session_id &&
                            claims_result->session_start_time) {
                        try {
                            const auto session_id =
                                sg(*claims_result->session_id);
                            repository::session_repository sess_repo(ctx_);
                            sess_repo.end_session(
                                session_id,
                                *claims_result->session_start_time,
                                std::chrono::system_clock::now(),
                                0, 0);
                        } catch (const std::exception& e) {
                            BOOST_LOG_SEV(auth_handler_lg(), warn)
                                << "Failed to end session record: "
                                << e.what();
                        }
                    }
                }
            }
            if (!token.empty()) {
                // Record logout event from the validated claims.
                auto claims_result = signer_.validate_allow_expired(token);
                if (claims_result) {
                    record_auth_event(ctx_, "logout", [&](auto& ev_repo) {
                        ev_repo.record_logout(
                            std::chrono::system_clock::now(),
                            claims_result->tenant_id.value_or(""),
                            claims_result->subject,
                            claims_result->username.value_or(""),
                            claims_result->session_id.value_or(""));
                    });
                }
            }
            BOOST_LOG_SEV(auth_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, logout_response{
                .success = true, .message = "Logged out"});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auth_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, logout_response{
                .success = false, .message = e.what()});
        }
    }

    void refresh(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), debug) << "Handling " << msg.subject;

        const auto token = auth_extract_bearer_token(msg);
        if (token.empty()) {
            reply(nats_, msg, refresh_response{
                .success = false, .message = "Missing Authorization header"});
            return;
        }

        try {
            // Validate token ignoring expiry — we still verify the signature.
            auto claims_result = signer_.validate_allow_expired(token);
            if (!claims_result) {
                reply(nats_, msg, refresh_response{
                    .success = false, .message = "Invalid token"});
                return;
            }

            // Enforce max session ceiling using session_start_time embedded
            // in the token at login.
            const auto now = std::chrono::system_clock::now();
            if (claims_result->session_start_time) {
                const auto session_age = now - *claims_result->session_start_time;
                const auto max_session = std::chrono::seconds(
                    token_settings_.max_session_s);
                if (session_age >= max_session) {
                    BOOST_LOG_SEV(auth_handler_lg(), info)
                        << "Max session exceeded for subject: "
                        << claims_result->subject;
                    record_auth_event(ctx_, "max_session_exceeded", [&](auto& ev_repo) {
                        ev_repo.record_max_session_exceeded(
                            now,
                            claims_result->tenant_id.value_or(""),
                            claims_result->subject,
                            claims_result->username.value_or(""),
                            claims_result->session_id.value_or(""));
                    });
                    reply(nats_, msg, refresh_response{
                        .success = false, .message = "max_session_exceeded"});
                    return;
                }
            }

            // Issue a fresh token carrying the same identity claims.
            security::jwt::jwt_claims new_claims;
            new_claims.subject          = claims_result->subject;
            new_claims.issued_at        = now;
            new_claims.expires_at       = now + std::chrono::seconds(
                                            token_settings_.access_lifetime_s);
            new_claims.username         = claims_result->username;
            new_claims.email            = claims_result->email;
            new_claims.tenant_id        = claims_result->tenant_id;
            new_claims.party_id         = claims_result->party_id;
            new_claims.session_id       = claims_result->session_id;
            new_claims.session_start_time = claims_result->session_start_time;
            new_claims.roles            = claims_result->roles;
            new_claims.visible_party_ids = claims_result->visible_party_ids;

            const auto new_token = signer_.create_token(new_claims).value_or("");
            if (new_token.empty()) {
                reply(nats_, msg, refresh_response{
                    .success = false, .message = "Token creation failed"});
                return;
            }

            BOOST_LOG_SEV(auth_handler_lg(), debug)
                << "Completed " << msg.subject << " for subject: "
                << claims_result->subject;
            record_auth_event(ctx_, "token_refresh", [&](auto& ev_repo) {
                ev_repo.record_token_refresh(
                    now,
                    claims_result->tenant_id.value_or(""),
                    claims_result->subject,
                    claims_result->username.value_or(""),
                    claims_result->session_id.value_or(""));
            });
            reply(nats_, msg, refresh_response{
                .success = true,
                .token = new_token,
                .access_lifetime_s = token_settings_.access_lifetime_s});

        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auth_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, refresh_response{
                .success = false, .message = e.what()});
        }
    }

private:
    /**
     * @brief Records an auth telemetry event, swallowing any exceptions.
     *
     * Used to avoid boilerplate try/catch around every event recording site.
     */
    template <typename Func>
    void record_auth_event(const ores::database::context& ctx,
        const char* event_name, Func&& fn) {
        try {
            repository::auth_event_repository ev_repo(ctx);
            fn(ev_repo);
        } catch (const std::exception& ev_err) {
            using namespace ores::logging;
            BOOST_LOG_SEV(auth_handler_lg(), warn)
                << "Failed to record " << event_name
                << " event: " << ev_err.what();
        }
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
    domain::token_settings token_settings_;
};

} // namespace ores::iam::messaging
#endif
