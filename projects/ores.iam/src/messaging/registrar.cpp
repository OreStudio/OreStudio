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
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.iam/messaging/signup_protocol.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/messaging/account_protocol.hpp"
#include "ores.iam/service/account_service.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.iam/service/bootstrap_mode_service.hpp"
#include "ores.iam/service/auth_session_service.hpp"
#include "ores.iam/domain/session.hpp"

namespace ores::iam::messaging {

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

} // namespace

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx) {

    auto auth_sessions = std::make_shared<service::auth_session_service>();

    std::vector<ores::nats::service::subscription> subs;
    subs.push_back(nats.queue_subscribe(
        "iam.v1.>", "ores.iam.service",
        [&nats, ctx, auth_sessions](ores::nats::message msg) mutable {
            const auto& subj = msg.subject;

            // --- Auth ---

            if (subj.ends_with(".auth.login")) {
                if (auto req = decode<login_request>(msg)) {
                    try {
                        service::account_service svc(ctx);
                        auto ip = boost::asio::ip::address_v4::loopback();
                        auto acct = svc.login(req->principal, req->password, ip);

                        thread_local boost::uuids::random_generator tok_gen;
                        boost::uuids::uuid tok_id = tok_gen();
                        std::string token = boost::uuids::to_string(tok_id);

                        domain::session sess;
                        sess.id = tok_id;
                        sess.account_id = acct.id;
                        sess.tenant_id = acct.tenant_id;
                        sess.start_time = std::chrono::system_clock::now();
                        sess.username = acct.username;
                        auth_sessions->add_session(token, sess);

                        login_response resp;
                        resp.success = true;
                        resp.token = token;
                        resp.account_id = boost::uuids::to_string(acct.id);
                        resp.tenant_id = acct.tenant_id.to_string();
                        resp.username = acct.username;
                        resp.email = acct.email;
                        reply(nats, msg, resp);
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
                        if (auto sess = auth_sessions->find_session(token)) {
                            service::account_service svc(ctx);
                            svc.logout(sess->account_id);
                        }
                        auth_sessions->remove_session(token);
                    }
                    reply(nats, msg, logout_response{
                        .success = true, .message = "Logged out"});
                } catch (const std::exception& e) {
                    reply(nats, msg, logout_response{
                        .success = false, .message = e.what()});
                }

            } else if (subj.ends_with(".auth.signup")) {
                if (auto req = decode<signup_request>(msg)) {
                    try {
                        service::account_service svc(ctx);
                        auto acct = svc.create_account(
                            req->principal, req->email, req->password, "system");
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
                            req->principal, req->email, req->password, "system");
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
                // Party selection: acknowledge success (full implementation deferred)
                reply(nats, msg, select_party_response{
                    .success = true, .message = "Party selected"});
            }
        }));

    return subs;
}

}
