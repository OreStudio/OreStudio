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

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.api/domain/session.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.iam.api/messaging/password_policy_protocol.hpp"
#include "ores.iam.api/messaging/signup_protocol.hpp"
#include "ores.iam.core/domain/token_settings.hpp"
#include "ores.iam.core/messaging/principal.hpp"
#include "ores.iam.core/repository/account_party_repository.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/auth_event_repository.hpp"
#include "ores.iam.core/repository/session_repository.hpp"
#include "ores.iam.core/repository/tenant_lookups.hpp"
#include "ores.iam.core/service/account_operations_service.hpp"
#include "ores.iam.core/service/account_setup_service.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.iam.core/service/cache/party_cache.hpp"
#include "ores.iam.core/service/service_session_service.hpp"
#include "ores.iam.core/service/signup_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.platform/concurrency/atomic_shared_ptr.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.security/validation/password_validator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.variability.core/service/system_settings_service.hpp"
#include <boost/asio/ip/address.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <memory>
#include <rfl/json.hpp>
#include <stdexcept>

namespace ores::iam::messaging {

namespace {

inline auto& auth_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.iam.messaging.auth_handler");
    return instance;
}

inline std::string auth_extract_bearer_token(const ores::nats::message& msg) {
    auto it = msg.headers.find(std::string(ores::nats::headers::authorization));
    if (it == msg.headers.end())
        return {};
    const auto& val = it->second;
    if (!val.starts_with(ores::nats::headers::bearer_prefix))
        return {};
    return val.substr(ores::nats::headers::bearer_prefix.size());
}

inline std::vector<boost::uuids::uuid>
auth_compute_visible_party_ids(const service::cache::party_cache& cache,
                               const std::string& tenant_id,
                               const boost::uuids::uuid& party_id) {
    return cache.compute_visible_party_ids(tenant_id, party_id);
}

inline std::optional<refdata::domain::party>
auth_lookup_party(const service::cache::party_cache& cache,
                  const std::string& tenant_id,
                  const boost::uuids::uuid& party_id) {
    return cache.lookup(tenant_id, party_id);
}

inline void
auth_ensure_parties_cached(service::cache::party_cache& cache,
                           const std::string& tenant_id,
                           const std::vector<ores::iam::domain::account_party>& account_parties) {
    for (const auto& ap : account_parties) {
        if (!cache.lookup(tenant_id, ap.party_id)) {
            // Cache miss: reload from refdata (handles bootstrap and startup race).
            (void)cache.load(tenant_id);
            break;
        }
    }
}

inline std::string auth_lookup_tenant_name(const ores::database::context& ctx,
                                           const boost::uuids::uuid& tenant_id) {
    try {
        const auto tenants = repository::read_active_tenant_by_id(ctx, tenant_id);
        if (!tenants.empty())
            return tenants.front().name;
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), warn) << "Failed to look up tenant name: " << e.what();
    }
    return {};
}

inline std::optional<ores::iam::domain::tenant>
auth_lookup_tenant_by_hostname(const ores::database::context& ctx, const std::string& hostname) {
    try {
        const auto tenants = repository::read_active_tenant_by_hostname(ctx, hostname);
        if (!tenants.empty())
            return tenants.front();
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), warn)
            << "Failed to look up tenant by hostname: " << e.what();
    }
    return std::nullopt;
}

inline bool auth_is_tenant_bootstrap_mode(const ores::database::context& ctx,
                                          const std::string& tenant_id_str) {
    try {
        auto tid_result = ores::utility::uuid::tenant_id::from_string(tenant_id_str);
        if (!tid_result)
            return false;
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

// Reads onboarding.party directly from the DB (never the party cache), so
// it is immune to cache staleness after a heavy import — unlike the
// party.status check it replaces, which conflated the party's own domain
// status with whether its provisioner wizard has run.
inline bool auth_is_party_onboarding_complete(const ores::database::context& ctx,
                                              const std::string& tenant_id_str,
                                              const std::string& party_id_str) {
    try {
        auto tid_result = ores::utility::uuid::tenant_id::from_string(tenant_id_str);
        if (!tid_result)
            return false;
        auto tenant_ctx = ctx.with_tenant(*tid_result, "");
        variability::service::system_settings_service sfs(tenant_ctx, tenant_id_str, party_id_str);
        sfs.refresh();
        return sfs.is_onboarding_party_complete();
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), warn)
            << "Failed to check party onboarding completion: " << e.what();
    }
    return false;
}

/**
 * @brief The deployment's answer to a registration, given its two flags.
 *
 * Nothing when the door is open, and the code and the sentence when it is shut.
 * Split from the read so the decision can be tested without a database, and so
 * the handler asks in one place.
 */
inline std::optional<std::pair<std::string, std::string>>
auth_registration_refusal(bool signups_enabled, bool authorization_required) {
    if (!signups_enabled) {
        return std::make_pair(std::string("signup_disabled"),
                              std::string("User registration is currently disabled."));
    }
    if (authorization_required) {
        return std::make_pair(
            std::string("signup_requires_authorization"),
            std::string("This deployment approves new accounts by hand, and the approval step "
                        "does not exist yet. Ask an administrator to create your account."));
    }
    return std::nullopt;
}

/**
 * @brief Reads the deployment's registration flags and answers with the decision.
 *
 * The switch is the deployment's answer rather than a tenant's, so it is read at
 * system scope. A read that fails closes the door: a deployment that cannot say
 * whether it accepts registrations does not accept them.
 */
inline std::optional<std::pair<std::string, std::string>>
auth_registration_refusal(const ores::database::context& ctx) {
    try {
        variability::service::system_settings_service flags(
            ctx, database::service::tenant_context::system_tenant_id);
        flags.refresh();
        return auth_registration_refusal(flags.is_user_signups_enabled(),
                                         flags.is_signup_requires_authorization_enabled());
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(auth_handler_lg(), error)
            << "Failed to read the registration flags, closing the door: " << e.what();
        return std::make_pair(
            std::string("signup_disabled"),
            std::string("The deployment could not say whether it accepts registrations, so it "
                        "does not."));
    }
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class auth_handler {
public:
    auth_handler(ores::nats::service::client& nats,
                 ores::database::context ctx,
                 ores::security::jwt::jwt_authenticator signer,
                 std::shared_ptr<service::cache::party_cache> party_cache)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer))
        , party_cache_(std::move(party_cache)) {
        reload_token_settings();
    }

    /**
     * @brief The token settings as of the last reload.
     *
     * A handler holds the snapshot it read for the whole of its work, so a
     * reload landing mid-request cannot change the lifetimes the request is
     * already answering with.
     */
    [[nodiscard]] std::shared_ptr<const domain::token_settings> token_settings() const {
        return token_settings_.load();
    }

    void reload_token_settings() {
        try {
            variability::service::system_settings_service svc(
                ctx_, database::service::tenant_context::system_tenant_id);
            svc.refresh();
            token_settings_.store(
                std::make_shared<const domain::token_settings>(domain::token_settings::load(svc)));
        } catch (const std::exception& e) {
            using namespace ores::logging;
            BOOST_LOG_SEV(auth_handler_lg(), warn)
                << "Failed to load token settings, using defaults: " << e.what();
        }
    }

    void signup(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(auth_handler_lg(), msg);
        auto req = decode<signup_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(auth_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }

        /*
         * The deployment's gate, read before anything is created. A deployment
         * that has turned self-registration off refuses here, over NATS and over
         * the HTTP gateway alike, and the refusal carries the code a screen
         * branches on.
         */
        if (const auto refusal = auth_registration_refusal(ctx_)) {
            BOOST_LOG_SEV(auth_handler_lg(), warn)
                << "Signup refused for " << req->principal << ": " << refusal->first;
            record_auth_event(ctx_, "signup_failure", [&](auto& ev_repo) {
                ev_repo.record_signup_failure(
                    std::chrono::system_clock::now(), "", req->principal, refusal->second);
            });
            reply(nats_,
                  msg,
                  signup_response{.success = false,
                                  .message = refusal->second,
                                  .error_code = refusal->first});
            return;
        }

        try {
            /*
             * The registration is the self-registration service's, not the
             * administrator path's. That service checks the username and the
             * email for uniqueness and the password against the policy, and
             * answers with a code for each refusal; the administrator path
             * checks none of them. It re-reads the flags the gate above already
             * read, which is deliberate: it must refuse for every caller and not
             * only for this one.
             */
            auto flags = std::make_shared<variability::service::system_settings_service>(
                ctx_, database::service::tenant_context::system_tenant_id);
            auto auth_svc = std::make_shared<service::authorization_service>(ctx_);
            service::signup_service signup_svc(ctx_, flags, auth_svc);
            const auto result = signup_svc.register_user(req->principal, req->email, req->password);

            if (!result.success) {
                const auto code = ores::utility::serialization::to_string(result.error_code);
                BOOST_LOG_SEV(auth_handler_lg(), warn)
                    << "Signup refused for " << req->principal << ": " << code;
                record_auth_event(ctx_, "signup_failure", [&](auto& ev_repo) {
                    ev_repo.record_signup_failure(std::chrono::system_clock::now(),
                                                  "",
                                                  req->principal,
                                                  result.error_message);
                });
                reply(nats_,
                      msg,
                      signup_response{.success = false,
                                      .message = result.error_message,
                                      .error_code = code});
                return;
            }

            BOOST_LOG_SEV(auth_handler_lg(), debug) << "Completed " << msg.subject;
            record_auth_event(ctx_, "signup_success", [&](auto& ev_repo) {
                ev_repo.record_signup_success(std::chrono::system_clock::now(),
                                              ctx_.tenant_id().to_string(),
                                              boost::uuids::to_string(result.account_id),
                                              result.username);
            });
            reply(nats_,
                  msg,
                  signup_response{.success = true,
                                  .account_id = boost::uuids::to_string(result.account_id)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auth_handler_lg(), error) << msg.subject << " failed: " << e.what();
            record_auth_event(ctx_, "signup_failure", [&](auto& ev_repo) {
                ev_repo.record_signup_failure(
                    std::chrono::system_clock::now(), "", req->principal, e.what());
            });
            reply(nats_,
                  msg,
                  signup_response{.success = false,
                                  .message = e.what(),
                                  .error_code = "invalid_request"});
        }
    }

    void login(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(auth_handler_lg(), msg);
        auto req = decode<login_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(auth_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            // The hostname routes the request to a tenant; the username is what
            // the account row is stored under.
            const auto principal = split_principal(req->principal);
            const auto& username = principal.username;
            ores::database::context login_ctx = ctx_;
            if (principal.has_hostname) {
                if (auto t = auth_lookup_tenant_by_hostname(ctx_, principal.hostname)) {
                    auto tid_result = ores::utility::uuid::tenant_id::from_uuid(t->id);
                    if (tid_result)
                        login_ctx = ctx_.with_tenant(*tid_result, "");
                }
            }

            service::account_operations_service svc(login_ctx);
            auto ip = boost::asio::ip::address_v4::loopback();
            const auto outcome = svc.login(username, req->password, ip);
            const auto& acct = outcome.account;

            // Check if this non-system tenant needs its provisioning wizard
            const bool in_tenant_bootstrap =
                !acct.tenant_id.is_system() &&
                auth_is_tenant_bootstrap_mode(login_ctx, acct.tenant_id.to_string());

            repository::account_party_repository ap_repo(login_ctx);
            auto account_parties = ap_repo.read_latest_by_account(acct.id);

            if (account_parties.empty()) {
                BOOST_LOG_SEV(auth_handler_lg(), warn)
                    << "Login rejected for " << username << ": account has no party assignment";
                throw std::runtime_error("Account has no party assignment. "
                                         "Please contact your administrator.");
            }

            // Ensure party details are in the cache. Bootstrap parties are
            // created directly via SQL (no NATS event), so the cache may be
            // cold even though the account-party association exists in the DB.
            auth_ensure_parties_cached(*party_cache_, acct.tenant_id.to_string(), account_parties);

            // Create a session record so that analytics, session listings,
            // and logout end-time tracking all work correctly.
            const auto now = std::chrono::system_clock::now();
            boost::uuids::random_generator uuid_gen;
            domain::session sess;
            sess.id = uuid_gen();
            sess.account_id = acct.id;
            sess.tenant_id = acct.tenant_id;
            sess.start_time = now;
            sess.protocol = "http";
            sess.client_ip = ip;
            // party_id set below once we know which party is active
            try {
                repository::session_repository sess_repo;
                sess_repo.write(login_ctx, sess);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(auth_handler_lg(), warn)
                    << "Failed to create session record: " << e.what();
            }
            const auto session_id_str = boost::uuids::to_string(sess.id);

            if (account_parties.size() == 1) {
                const auto& party_id = account_parties.front().party_id;
                auto visible = auth_compute_visible_party_ids(
                    *party_cache_, acct.tenant_id.to_string(), party_id);

                security::jwt::jwt_claims claims;
                claims.subject = boost::uuids::to_string(acct.id);
                claims.issued_at = now;
                claims.expires_at = now + std::chrono::seconds(token_settings()->access_lifetime_s);
                claims.username = acct.username;
                claims.email = acct.email;
                claims.tenant_id = acct.tenant_id.to_string();
                claims.party_id = boost::uuids::to_string(party_id);
                claims.session_id = session_id_str;
                claims.session_start_time = now;
                for (const auto& vid : visible)
                    claims.visible_party_ids.push_back(boost::uuids::to_string(vid));
                auto token = signer_.create_token(claims).value_or("");

                login_response resp;
                resp.success = true;
                resp.token = token;
                resp.account_id = boost::uuids::to_string(acct.id);
                resp.tenant_id = acct.tenant_id.to_string();
                resp.tenant_name = auth_lookup_tenant_name(login_ctx, acct.tenant_id.to_uuid());
                /*
                 * A session is a session with a deployment, so the answer that
                 * opens one says which build it was opened against.
                 */
                resp.version = utility::version::full_version_string();
                resp.username = acct.username;
                resp.email = acct.email;
                resp.selected_party_id = boost::uuids::to_string(party_id);
                resp.tenant_bootstrap_mode = in_tenant_bootstrap;
                resp.password_reset_required = outcome.password_reset_required;
                resp.access_lifetime_s = token_settings()->access_lifetime_s;
                resp.session_id = session_id_str;
                for (const auto& ap : account_parties) {
                    auto p =
                        auth_lookup_party(*party_cache_, acct.tenant_id.to_string(), ap.party_id);
                    if (ap.party_id == party_id) {
                        const bool onboarding_complete =
                            auth_is_party_onboarding_complete(login_ctx,
                                                              acct.tenant_id.to_string(),
                                                              boost::uuids::to_string(party_id));
                        resp.party_setup_required = !onboarding_complete;
                        if (resp.party_setup_required) {
                            BOOST_LOG_SEV(auth_handler_lg(), info)
                                << "login: party_setup_required=true for party "
                                << boost::uuids::to_string(party_id);
                        } else if (p && p->status == "Inactive") {
                            resp.party_setup_warning =
                                "Party setup completed, but the party is still marked Inactive.";
                            BOOST_LOG_SEV(auth_handler_lg(), warn)
                                << "login: onboarding.party complete but party "
                                << boost::uuids::to_string(party_id) << " still Inactive";
                        }
                    }
                    resp.available_parties.push_back(party_summary{
                        .id = boost::uuids::to_string(ap.party_id),
                        .name = p ? p->full_name : std::string{},
                        .party_category = p ? p->party_category : std::string{},
                        .business_center_code = p ? p->business_center_code : std::string{}});
                }
                BOOST_LOG_SEV(auth_handler_lg(), debug) << "Completed " << msg.subject;
                record_auth_event(login_ctx, "login_success", [&](auto& ev_repo) {
                    ev_repo.record_login_success(now,
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
                claims.expires_at =
                    now + std::chrono::seconds(token_settings()->party_selection_lifetime_s);
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
                resp.tenant_name = auth_lookup_tenant_name(login_ctx, acct.tenant_id.to_uuid());
                /*
                 * A session is a session with a deployment, so the answer that
                 * opens one says which build it was opened against.
                 */
                resp.version = utility::version::full_version_string();
                resp.username = acct.username;
                resp.email = acct.email;
                resp.tenant_bootstrap_mode = in_tenant_bootstrap;
                resp.password_reset_required = outcome.password_reset_required;
                resp.access_lifetime_s = token_settings()->party_selection_lifetime_s;
                resp.session_id = session_id_str;
                if (acct.default_party_id) {
                    const auto default_id = *acct.default_party_id;
                    const bool is_available =
                        std::any_of(account_parties.begin(),
                                    account_parties.end(),
                                    [&](const auto& ap) { return ap.party_id == default_id; });
                    if (is_available)
                        resp.default_party_id = boost::uuids::to_string(default_id);
                }
                for (const auto& ap : account_parties) {
                    auto p =
                        auth_lookup_party(*party_cache_, acct.tenant_id.to_string(), ap.party_id);
                    resp.available_parties.push_back(party_summary{
                        .id = boost::uuids::to_string(ap.party_id),
                        .name = p ? p->full_name : std::string{},
                        .party_category = p ? p->party_category : std::string{},
                        .business_center_code = p ? p->business_center_code : std::string{}});
                }
                BOOST_LOG_SEV(auth_handler_lg(), debug) << "Completed " << msg.subject;
                // Multi-party: login_success recorded after party selection
                reply(nats_, msg, resp);
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auth_handler_lg(), error) << msg.subject << " failed: " << e.what();
            record_auth_event(ctx_, "login_failure", [&](auto& ev_repo) {
                ev_repo.record_login_failure(
                    std::chrono::system_clock::now(), "", req->principal, e.what());
            });
            login_response resp;
            resp.success = false;
            resp.error_message = e.what();
            /*
             * A refusal still says which build refused, so a client can state
             * the deployment's version without having signed in at all.
             */
            resp.version = utility::version::full_version_string();
            reply(nats_, msg, resp);
        }
    }

    /**
     * @brief Serves iam.v1.auth.password-policy.
     *
     * The rules the server enforces, answered from the validator that enforces
     * them, so a screen states the server's rules rather than keeping a copy.
     * A caller reaches this before it has a session, because the screen that
     * shows the rules is the one a person signs in on.
     */
    void password_policy(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(auth_handler_lg(), msg);

        const auto rules = ores::security::validation::password_validator::policy();
        get_password_policy_response resp;
        resp.success = true;
        resp.min_length = static_cast<int>(rules.min_length);
        resp.require_uppercase = rules.require_uppercase;
        resp.require_lowercase = rules.require_lowercase;
        resp.require_digit = rules.require_digit;
        resp.require_special = rules.require_special;
        resp.special_chars = rules.special_chars;
        reply(nats_, msg, resp);
    }

    void public_key(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(auth_handler_lg(), msg);
        if (msg.reply_subject.empty())
            return;
        try {
            auto pub_key = signer_.get_public_key_pem();
            if (pub_key.empty())
                throw std::runtime_error("No RSA private key configured for JWT signing. "
                                         "Run generate_keys.sh in publish/bin/ to generate "
                                         "the key, then restart the IAM service.");
            const auto json = std::string("{\"public_key\":") + rfl::json::write(pub_key) + "}";
            nats_.publish(msg.reply_subject, ores::nats::as_bytes(json));
            BOOST_LOG_SEV(auth_handler_lg(), debug) << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auth_handler_lg(), error) << msg.subject << " failed: " << e.what();
        }
    }

    void logout(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(auth_handler_lg(), msg);
        auto token = auth_extract_bearer_token(msg);
        try {
            if (!token.empty()) {
                auto claims_result = signer_.validate(token);
                if (claims_result) {
                    boost::uuids::string_generator sg;
                    // Update the login_info online flag
                    try {
                        auto account_id = sg(claims_result->subject);
                        service::account_operations_service svc(ctx_);
                        svc.logout(account_id);
                    } catch (const std::exception& e) {
                        BOOST_LOG_SEV(auth_handler_lg(), warn)
                            << "Failed to update logout state: " << e.what();
                    }
                    // Persist session end time using the IDs embedded in
                    // the JWT at login.
                    if (claims_result->session_id && claims_result->session_start_time) {
                        try {
                            const auto session_id = sg(*claims_result->session_id);
                            repository::session_repository sess_repo;
                            sess_repo.end_session(ctx_,
                                                  session_id,
                                                  *claims_result->session_start_time,
                                                  std::chrono::system_clock::now(),
                                                  0,
                                                  0);
                        } catch (const std::exception& e) {
                            BOOST_LOG_SEV(auth_handler_lg(), warn)
                                << "Failed to end session record: " << e.what();
                        }
                    }
                }
            }
            if (!token.empty()) {
                // Record logout event from the validated claims.
                auto claims_result = signer_.validate_allow_expired(token);
                if (claims_result) {
                    record_auth_event(ctx_, "logout", [&](auto& ev_repo) {
                        ev_repo.record_logout(std::chrono::system_clock::now(),
                                              claims_result->tenant_id.value_or(""),
                                              claims_result->subject,
                                              claims_result->username.value_or(""),
                                              claims_result->session_id.value_or(""));
                    });
                }
            }
            BOOST_LOG_SEV(auth_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, logout_response{.success = true, .message = "Logged out"});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auth_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, logout_response{.success = false, .message = e.what()});
        }
    }

    void refresh(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(auth_handler_lg(), msg);

        const auto token = auth_extract_bearer_token(msg);
        if (token.empty()) {
            reply(nats_,
                  msg,
                  refresh_response{.success = false, .message = "Missing Authorization header"});
            return;
        }

        try {
            // Validate token ignoring expiry — we still verify the signature.
            auto claims_result = signer_.validate_allow_expired(token);
            if (!claims_result) {
                reply(nats_, msg, refresh_response{.success = false, .message = "Invalid token"});
                return;
            }

            // Enforce max session ceiling using session_start_time embedded
            // in the token at login.
            const auto now = std::chrono::system_clock::now();
            if (claims_result->session_start_time) {
                const auto session_age = now - *claims_result->session_start_time;
                const auto max_session = std::chrono::seconds(token_settings()->max_session_s);
                if (session_age >= max_session) {
                    BOOST_LOG_SEV(auth_handler_lg(), info)
                        << "Max session exceeded for subject: " << claims_result->subject;
                    record_auth_event(ctx_, "max_session_exceeded", [&](auto& ev_repo) {
                        ev_repo.record_max_session_exceeded(now,
                                                            claims_result->tenant_id.value_or(""),
                                                            claims_result->subject,
                                                            claims_result->username.value_or(""),
                                                            claims_result->session_id.value_or(""));
                    });
                    reply(nats_,
                          msg,
                          refresh_response{.success = false, .message = "max_session_exceeded"});
                    return;
                }
            }

            // Issue a fresh token carrying the same identity claims.
            security::jwt::jwt_claims new_claims;
            new_claims.subject = claims_result->subject;
            new_claims.issued_at = now;
            new_claims.expires_at = now + std::chrono::seconds(token_settings()->access_lifetime_s);
            new_claims.username = claims_result->username;
            new_claims.email = claims_result->email;
            new_claims.tenant_id = claims_result->tenant_id;
            new_claims.party_id = claims_result->party_id;
            new_claims.session_id = claims_result->session_id;
            new_claims.session_start_time = claims_result->session_start_time;
            new_claims.roles = claims_result->roles;
            new_claims.visible_party_ids = claims_result->visible_party_ids;

            const auto new_token = signer_.create_token(new_claims).value_or("");
            if (new_token.empty()) {
                reply(nats_,
                      msg,
                      refresh_response{.success = false, .message = "Token creation failed"});
                return;
            }

            BOOST_LOG_SEV(auth_handler_lg(), debug)
                << "Completed " << msg.subject << " for subject: " << claims_result->subject;
            record_auth_event(ctx_, "token_refresh", [&](auto& ev_repo) {
                ev_repo.record_token_refresh(now,
                                             claims_result->tenant_id.value_or(""),
                                             claims_result->subject,
                                             claims_result->username.value_or(""),
                                             claims_result->session_id.value_or(""));
            });
            reply(nats_,
                  msg,
                  refresh_response{.success = true,
                                   .token = new_token,
                                   .access_lifetime_s = token_settings()->access_lifetime_s});

        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auth_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, refresh_response{.success = false, .message = e.what()});
        }
    }

    void service_login(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(auth_handler_lg(), msg);
        auto req = decode<service_login_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(auth_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(nats_,
                  msg,
                  service_login_response{.success = false, .message = "Failed to decode request"});
            return;
        }
        try {
            repository::account_repository account_repo;
            const auto account_id =
                account_repo.check_service_credentials(ctx_, req->username, req->password);
            if (!account_id) {
                BOOST_LOG_SEV(auth_handler_lg(), warn)
                    << "Service login failed: invalid credentials for " << req->username;
                reply(nats_,
                      msg,
                      service_login_response{.success = false, .message = "Invalid credentials"});
                return;
            }

            service::service_session_service sess_svc(ctx_);
            auto sess = sess_svc.start_service_session(req->username, "ores.service.binary");
            if (!sess) {
                BOOST_LOG_SEV(auth_handler_lg(), error)
                    << "Failed to start service session for " << req->username;
                reply(nats_,
                      msg,
                      service_login_response{.success = false,
                                             .message = "Failed to create session"});
                return;
            }

            const auto now = std::chrono::system_clock::now();
            security::jwt::jwt_claims claims;
            claims.subject = boost::uuids::to_string(sess->account_id);
            claims.issued_at = now;
            claims.expires_at = now + std::chrono::seconds(token_settings()->access_lifetime_s);
            claims.username = req->username;
            claims.tenant_id = sess->tenant_id.to_string();
            claims.session_id = boost::uuids::to_string(sess->id);
            claims.session_start_time = sess->start_time;

            // Embed effective permission codes so handlers can enforce
            // permissions without a round-trip to the database.
            try {
                service::authorization_service auth_svc(ctx_);
                claims.roles = auth_svc.get_effective_permissions(sess->account_id);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(auth_handler_lg(), warn)
                    << "Failed to load permissions for service account " << req->username << ": "
                    << e.what();
            }

            auto token = signer_.create_token(claims).value_or("");
            if (token.empty()) {
                reply(nats_,
                      msg,
                      service_login_response{.success = false, .message = "Token creation failed"});
                return;
            }

            BOOST_LOG_SEV(auth_handler_lg(), info)
                << "Service login successful for " << req->username;
            reply(nats_,
                  msg,
                  service_login_response{.success = true,
                                         .token = std::move(token),
                                         .access_lifetime_s = token_settings()->access_lifetime_s});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(auth_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, service_login_response{.success = false, .message = e.what()});
        }
    }

private:
    /**
     * @brief Records an auth telemetry event, swallowing any exceptions.
     *
     * Used to avoid boilerplate try/catch around every event recording site.
     */
    template <typename Func>
    void record_auth_event(const ores::database::context& ctx, const char* event_name, Func&& fn) {
        try {
            repository::auth_event_repository ev_repo(ctx);
            fn(ev_repo);
        } catch (const std::exception& ev_err) {
            using namespace ores::logging;
            BOOST_LOG_SEV(auth_handler_lg(), warn)
                << "Failed to record " << event_name << " event: " << ev_err.what();
        }
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
    std::shared_ptr<service::cache::party_cache> party_cache_;
    // Written by reload_token_settings, which the settings-change event now
    // calls from a NATS dispatch thread, and read by every request handler on
    // its own thread. Handlers take a snapshot through token_settings(), so a
    // reader sees one whole settings object rather than a half-written one.
    // Initialised rather than left empty, because the reload can fail and a
    // handler that reads it anyway must find the defaults, not a null pointer.
    platform::concurrency::atomic_shared_ptr<const domain::token_settings> token_settings_{
        std::make_shared<const domain::token_settings>()};
};

} // namespace ores::iam::messaging
#endif
