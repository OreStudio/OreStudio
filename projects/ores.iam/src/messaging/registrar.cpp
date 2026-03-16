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
#include "ores.iam/messaging/registrar.hpp"

#include <chrono>
#include <memory>
#include <span>
#include <string_view>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/asio/ip/address.hpp>
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.iam/messaging/signup_protocol.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/messaging/account_protocol.hpp"
#include "ores.iam/service/account_service.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.iam/service/bootstrap_mode_service.hpp"
#include "ores.iam/repository/account_party_repository.hpp"
#include "ores.iam/domain/session.hpp"
#include "ores.iam/domain/role.hpp"

namespace ores::iam::messaging {

namespace svc_acct = ores::iam::domain::service_accounts;

namespace {

template<typename Resp>
void reply(ores::nats::service::client& nats,
           const ores::nats::message& msg,
           const Resp& resp) {
    if (msg.reply_subject.empty())
        return;
    const auto json = rfl::json::write(resp);
    const auto* p = reinterpret_cast<const std::byte*>(json.data());
    nats.publish(msg.reply_subject, std::span<const std::byte>(p, json.size()));
}

template<typename Req>
std::optional<Req> decode(const ores::nats::message& msg) {
    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto r = rfl::json::read<Req>(sv);
    if (!r)
        return std::nullopt;
    return *r;
}

std::string extract_bearer_token(const ores::nats::message& msg) {
    auto it = msg.headers.find("Authorization");
    if (it == msg.headers.end())
        return {};
    const auto& val = it->second;
    constexpr std::string_view prefix = "Bearer ";
    if (!val.starts_with(prefix))
        return {};
    return val.substr(prefix.size());
}

// Compute visible party IDs for a given party.
// For now, returns just the user's own party.
// TODO: implement recursive CTE for full party hierarchy.
static std::vector<boost::uuids::uuid> compute_visible_party_ids(
    const database::context& /*ctx*/,
    const std::string& /*tenant_id_str*/,
    const boost::uuids::uuid& party_id) {
    return {party_id};
}

} // namespace

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    ores::security::jwt::jwt_authenticator signer) {

    std::vector<ores::nats::service::subscription> subs;
    subs.push_back(nats.queue_subscribe(
        "iam.v1.>", "ores.iam.service",
        [&nats, ctx, signer](ores::nats::message msg) mutable {
            const auto& subj = msg.subject;

            // --- Auth ---

            if (subj.ends_with(".auth.login")) {
                if (auto req = decode<login_request>(msg)) {
                    try {
                        service::account_service svc(ctx);
                        auto ip = boost::asio::ip::address_v4::loopback();
                        auto acct = svc.login(req->principal, req->password, ip);

                        // Get available parties for this account
                        repository::account_party_repository ap_repo(ctx);
                        auto account_parties =
                            ap_repo.read_latest_by_account(acct.id);

                        if (account_parties.empty()) {
                            // No parties - issue full JWT with system defaults
                            security::jwt::jwt_claims claims;
                            claims.subject = boost::uuids::to_string(acct.id);
                            claims.issued_at = std::chrono::system_clock::now();
                            claims.expires_at =
                                claims.issued_at + std::chrono::hours(8);
                            claims.username = acct.username;
                            claims.email = acct.email;
                            claims.tenant_id = acct.tenant_id.to_string();
                            auto token = signer.create_token(claims).value_or("");

                            login_response resp;
                            resp.success = true;
                            resp.token = token;
                            resp.account_id = boost::uuids::to_string(acct.id);
                            resp.tenant_id = acct.tenant_id.to_string();
                            resp.username = acct.username;
                            resp.email = acct.email;
                            reply(nats, msg, resp);
                        } else if (account_parties.size() == 1) {
                            // Single party - compute visible_party_ids and
                            // issue full JWT
                            const auto& party_id =
                                account_parties.front().party_id;
                            auto visible = compute_visible_party_ids(
                                ctx, acct.tenant_id.to_string(), party_id);

                            security::jwt::jwt_claims claims;
                            claims.subject = boost::uuids::to_string(acct.id);
                            claims.issued_at = std::chrono::system_clock::now();
                            claims.expires_at =
                                claims.issued_at + std::chrono::hours(8);
                            claims.username = acct.username;
                            claims.email = acct.email;
                            claims.tenant_id = acct.tenant_id.to_string();
                            claims.party_id =
                                boost::uuids::to_string(party_id);
                            for (const auto& vid : visible)
                                claims.visible_party_ids.push_back(
                                    boost::uuids::to_string(vid));
                            auto token =
                                signer.create_token(claims).value_or("");

                            login_response resp;
                            resp.success = true;
                            resp.token = token;
                            resp.account_id = boost::uuids::to_string(acct.id);
                            resp.tenant_id = acct.tenant_id.to_string();
                            resp.username = acct.username;
                            resp.email = acct.email;
                            resp.selected_party_id =
                                boost::uuids::to_string(party_id);
                            for (const auto& ap : account_parties) {
                                resp.available_parties.push_back(party_summary{
                                    .id = boost::uuids::to_string(ap.party_id)
                                });
                            }
                            reply(nats, msg, resp);
                        } else {
                            // Multiple parties - issue interim JWT
                            // (audience restricted)
                            security::jwt::jwt_claims claims;
                            claims.subject = boost::uuids::to_string(acct.id);
                            claims.issued_at = std::chrono::system_clock::now();
                            claims.expires_at =
                                claims.issued_at + std::chrono::minutes(5);
                            claims.audience = "select_party_only";
                            claims.username = acct.username;
                            claims.email = acct.email;
                            claims.tenant_id = acct.tenant_id.to_string();
                            auto token =
                                signer.create_token(claims).value_or("");

                            login_response resp;
                            resp.success = true;
                            resp.token = token; // interim JWT
                            resp.account_id = boost::uuids::to_string(acct.id);
                            resp.tenant_id = acct.tenant_id.to_string();
                            resp.username = acct.username;
                            resp.email = acct.email;
                            for (const auto& ap : account_parties) {
                                resp.available_parties.push_back(party_summary{
                                    .id = boost::uuids::to_string(ap.party_id)
                                });
                            }
                            reply(nats, msg, resp);
                        }
                    } catch (const std::exception& e) {
                        login_response resp;
                        resp.success = false;
                        resp.error_message = e.what();
                        reply(nats, msg, resp);
                    }
                }

            } else if (subj.ends_with(".auth.logout")) {
                auto token = extract_bearer_token(msg);
                try {
                    if (!token.empty()) {
                        auto claims_result = signer.validate(token);
                        if (claims_result) {
                            boost::uuids::string_generator sg;
                            try {
                                auto account_id = sg(claims_result->subject);
                                service::account_service svc(ctx);
                                svc.logout(account_id);
                            } catch (...) {}
                        }
                    }
                    reply(nats, msg, logout_response{
                        .success = true, .message = "Logged out"});
                } catch (const std::exception& e) {
                    reply(nats, msg, logout_response{
                        .success = false, .message = e.what()});
                }

            } else if (subj.ends_with(".auth.jwks")) {
                try {
                    auto pub_key = signer.get_public_key_pem();
                    std::string json =
                        "{\"public_key\":" + rfl::json::write(pub_key) + "}";
                    const auto* p =
                        reinterpret_cast<const std::byte*>(json.data());
                    nats.publish(msg.reply_subject,
                        std::span<const std::byte>(p, json.size()));
                } catch (const std::exception& e) {
                    reply(nats, msg, login_response{
                        .success = false, .error_message = e.what()});
                }

            } else if (subj.ends_with(".auth.signup")) {
                if (auto req = decode<signup_request>(msg)) {
                    try {
                        service::account_service svc(ctx);
                        auto acct = svc.create_account(
                            req->principal, req->email, req->password,
                            svc_acct::iam);
                        reply(nats, msg, signup_response{
                            .success = true,
                            .account_id = boost::uuids::to_string(acct.id)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, signup_response{
                            .success = false, .message = e.what()});
                    }
                }

            // --- Bootstrap ---

            } else if (subj.ends_with(".bootstrap.status")) {
                try {
                    auto auth_svc =
                        std::make_shared<service::authorization_service>(ctx);
                    service::bootstrap_mode_service bms(
                        ctx, ctx.tenant_id().to_string(), auth_svc);
                    reply(nats, msg, bootstrap_status_response{
                        .is_in_bootstrap_mode = bms.is_in_bootstrap_mode()});
                } catch (const std::exception& e) {
                    reply(nats, msg, bootstrap_status_response{
                        .is_in_bootstrap_mode = false,
                        .message = e.what()});
                }

            } else if (subj.ends_with(".bootstrap.create-admin")) {
                if (auto req = decode<create_initial_admin_request>(msg)) {
                    try {
                        service::account_service svc(ctx);
                        auto acct = svc.create_account(
                            req->principal, req->email, req->password,
                            svc_acct::iam);
                        auto auth_svc =
                            std::make_shared<service::authorization_service>(ctx);
                        service::bootstrap_mode_service bms(
                            ctx, ctx.tenant_id().to_string(), auth_svc);
                        bms.exit_bootstrap_mode();
                        reply(nats, msg, create_initial_admin_response{
                            .success = true,
                            .account_id = boost::uuids::to_string(acct.id),
                            .tenant_id = acct.tenant_id.to_string()});
                    } catch (const std::exception& e) {
                        reply(nats, msg, create_initial_admin_response{
                            .success = false, .error_message = e.what()});
                    }
                }

            // --- Accounts ---

            } else if (subj.ends_with(".accounts.list")) {
                try {
                    service::account_service svc(ctx);
                    auto accounts = svc.list_accounts();
                    get_accounts_response resp;
                    resp.total_available_count =
                        static_cast<int>(accounts.size());
                    resp.accounts = std::move(accounts);
                    reply(nats, msg, resp);
                } catch (...) {
                    reply(nats, msg, get_accounts_response{});
                }

            } else if (subj.ends_with(".accounts.save")) {
                if (auto req = decode<save_account_request>(msg)) {
                    try {
                        service::account_service svc(ctx);
                        auto acct = svc.create_account(
                            req->principal, req->email, req->password, "admin");
                        reply(nats, msg, save_account_response{
                            .success = true,
                            .account_id = boost::uuids::to_string(acct.id)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_account_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".accounts.delete")) {
                if (auto req = decode<delete_account_request>(msg)) {
                    try {
                        service::account_service svc(ctx);
                        boost::uuids::string_generator sg;
                        svc.delete_account(sg(req->account_id));
                        reply(nats, msg,
                            delete_account_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_account_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".accounts.lock")) {
                if (auto req = decode<lock_account_request>(msg)) {
                    lock_account_response resp;
                    service::account_service svc(ctx);
                    boost::uuids::string_generator sg;
                    for (const auto& id : req->account_ids) {
                        try {
                            svc.lock_account(sg(id));
                            resp.results.push_back({.success = true});
                        } catch (const std::exception& e) {
                            resp.results.push_back({
                                .success = false, .message = e.what()});
                        }
                    }
                    reply(nats, msg, resp);
                }

            } else if (subj.ends_with(".accounts.unlock")) {
                if (auto req = decode<unlock_account_request>(msg)) {
                    unlock_account_response resp;
                    service::account_service svc(ctx);
                    boost::uuids::string_generator sg;
                    for (const auto& id : req->account_ids) {
                        try {
                            svc.unlock_account(sg(id));
                            resp.results.push_back({.success = true});
                        } catch (const std::exception& e) {
                            resp.results.push_back({
                                .success = false, .message = e.what()});
                        }
                    }
                    reply(nats, msg, resp);
                }

            } else if (subj.ends_with(".accounts.login-info")) {
                try {
                    service::account_service svc(ctx);
                    auto infos = svc.list_login_info();
                    reply(nats, msg,
                        list_login_info_response{
                            .login_infos = std::move(infos)});
                } catch (...) {
                    reply(nats, msg, list_login_info_response{});
                }

            } else if (subj.ends_with(".accounts.select-party")) {
                if (auto req = decode<select_party_request>(msg)) {
                    try {
                        // Validate interim JWT from Bearer header
                        auto token = extract_bearer_token(msg);
                        if (token.empty()) {
                            reply(nats, msg, select_party_response{
                                .success = false,
                                .message = "Missing authorization token"});
                            return;
                        }

                        auto claims_result = signer.validate(token);
                        if (!claims_result) {
                            reply(nats, msg, select_party_response{
                                .success = false,
                                .message = "Invalid or expired token"});
                            return;
                        }

                        boost::uuids::string_generator sg;
                        auto account_id = sg(claims_result->subject);

                        // Validate user is member of requested party
                        boost::uuids::uuid requested_party_id =
                            sg(req->party_id);
                        repository::account_party_repository ap_repo(ctx);
                        auto parties =
                            ap_repo.read_latest_by_account(account_id);

                        bool is_member = false;
                        for (const auto& ap : parties) {
                            if (ap.party_id == requested_party_id) {
                                is_member = true;
                                break;
                            }
                        }

                        if (!is_member) {
                            reply(nats, msg, select_party_response{
                                .success = false,
                                .message =
                                    "User is not a member of requested party"});
                            return;
                        }

                        // Compute visible_party_ids
                        auto tenant_id_str = claims_result->tenant_id.value_or(
                            ctx.tenant_id().to_string());
                        auto visible = compute_visible_party_ids(
                            ctx, tenant_id_str, requested_party_id);

                        // Issue full JWT
                        security::jwt::jwt_claims new_claims;
                        new_claims.subject = claims_result->subject;
                        new_claims.issued_at = std::chrono::system_clock::now();
                        new_claims.expires_at =
                            new_claims.issued_at + std::chrono::hours(8);
                        new_claims.username = claims_result->username;
                        new_claims.email = claims_result->email;
                        new_claims.tenant_id = tenant_id_str;
                        new_claims.party_id =
                            boost::uuids::to_string(requested_party_id);
                        for (const auto& vid : visible)
                            new_claims.visible_party_ids.push_back(
                                boost::uuids::to_string(vid));

                        auto new_token =
                            signer.create_token(new_claims).value_or("");

                        reply(nats, msg, select_party_response{
                            .success = true,
                            .message = "Party selected",
                            .token = new_token,
                            .username = claims_result->username.value_or(""),
                            .tenant_name = tenant_id_str,
                            .party_name = req->party_id
                        });
                    } catch (const std::exception& e) {
                        reply(nats, msg, select_party_response{
                            .success = false, .message = e.what()});
                    }
                }
            }
        }));

    return subs;
}

}
