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

#include <memory>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.iam/messaging/auth_handler.hpp"
#include "ores.iam/messaging/bootstrap_handler.hpp"
#include "ores.iam/messaging/account_handler.hpp"
#include "ores.iam/messaging/account_party_handler.hpp"
#include "ores.iam/messaging/session_handler.hpp"
#include "ores.iam/messaging/role_handler.hpp"
#include "ores.iam/messaging/tenant_handler.hpp"
#include "ores.iam/messaging/tenant_status_handler.hpp"
#include "ores.iam/messaging/tenant_type_handler.hpp"
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.iam/messaging/signup_protocol.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/messaging/account_protocol.hpp"
#include "ores.iam/messaging/account_history_protocol.hpp"
#include "ores.iam/messaging/account_party_protocol.hpp"
#include "ores.iam/messaging/session_protocol.hpp"
#include "ores.iam/messaging/session_samples_protocol.hpp"
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"
#include "ores.iam/messaging/tenant_status_protocol.hpp"
#include "ores.iam/messaging/tenant_type_protocol.hpp"

namespace ores::iam::messaging {

namespace {

using namespace ores::logging;
inline static std::string_view logger_name = "ores.iam.messaging.registrar";
static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

} // namespace

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    ores::security::jwt::jwt_authenticator signer) {

    std::vector<ores::nats::service::subscription> subs;
    constexpr auto qg = "ores.iam.service";

    // --- Auth ---
    auto ah = std::make_shared<auth_handler>(nats, ctx, signer);
    subs.push_back(nats.queue_subscribe(
        signup_request::nats_subject, qg,
        [ah](ores::nats::message msg) { ah->signup(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        login_request::nats_subject, qg,
        [ah](ores::nats::message msg) { ah->login(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        logout_request::nats_subject, qg,
        [ah](ores::nats::message msg) { ah->logout(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        public_key_request::nats_subject, qg,
        [ah](ores::nats::message msg) { ah->public_key(std::move(msg)); }));

    // --- Bootstrap ---
    auto bh = std::make_shared<bootstrap_handler>(nats, ctx, signer);
    subs.push_back(nats.queue_subscribe(
        bootstrap_status_request::nats_subject, qg,
        [bh](ores::nats::message msg) { bh->status(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        create_initial_admin_request::nats_subject, qg,
        [bh](ores::nats::message msg) { bh->create_admin(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        provision_tenant_request::nats_subject, qg,
        [bh](ores::nats::message msg) { bh->provision_tenant(std::move(msg)); }));

    // --- Accounts ---
    auto acth = std::make_shared<account_handler>(nats, ctx, signer);
    subs.push_back(nats.queue_subscribe(
        get_accounts_request_typed::nats_subject, qg,
        [acth](ores::nats::message msg) { acth->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_account_request::nats_subject, qg,
        [acth](ores::nats::message msg) { acth->save(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_account_request::nats_subject, qg,
        [acth](ores::nats::message msg) { acth->del(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        lock_account_request::nats_subject, qg,
        [acth](ores::nats::message msg) { acth->lock(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        unlock_account_request::nats_subject, qg,
        [acth](ores::nats::message msg) { acth->unlock(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        list_login_info_request::nats_subject, qg,
        [acth](ores::nats::message msg) { acth->login_info(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        reset_password_request::nats_subject, qg,
        [acth](ores::nats::message msg) { acth->reset_password(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        change_password_request_typed::nats_subject, qg,
        [acth](ores::nats::message msg) { acth->change_password(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        update_account_request::nats_subject, qg,
        [acth](ores::nats::message msg) { acth->update(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        update_my_email_request::nats_subject, qg,
        [acth](ores::nats::message msg) { acth->update_email(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        select_party_request::nats_subject, qg,
        [acth](ores::nats::message msg) { acth->select_party(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_account_history_request::nats_subject, qg,
        [acth](ores::nats::message msg) { acth->history(std::move(msg)); }));

    // --- Account parties ---
    auto aph = std::make_shared<account_party_handler>(nats, ctx, signer);
    subs.push_back(nats.queue_subscribe(
        get_account_parties_request::nats_subject, qg,
        [aph](ores::nats::message msg) { aph->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_account_parties_by_account_request::nats_subject, qg,
        [aph](ores::nats::message msg) { aph->by_account(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_account_party_request::nats_subject, qg,
        [aph](ores::nats::message msg) { aph->save(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_account_party_request::nats_subject, qg,
        [aph](ores::nats::message msg) { aph->del(std::move(msg)); }));

    // --- Sessions ---
    auto sh = std::make_shared<session_handler>(nats, ctx, signer);
    subs.push_back(nats.queue_subscribe(
        list_sessions_request::nats_subject, qg,
        [sh](ores::nats::message msg) { sh->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_active_sessions_request::nats_subject, qg,
        [sh](ores::nats::message msg) { sh->active(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_session_samples_request::nats_subject, qg,
        [sh](ores::nats::message msg) { sh->samples(std::move(msg)); }));

    // --- Roles ---
    auto rh = std::make_shared<role_handler>(nats, ctx, signer);
    subs.push_back(nats.queue_subscribe(
        list_roles_request::nats_subject, qg,
        [rh](ores::nats::message msg) { rh->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        assign_role_request::nats_subject, qg,
        [rh](ores::nats::message msg) { rh->assign(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        revoke_role_request::nats_subject, qg,
        [rh](ores::nats::message msg) { rh->revoke(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_account_roles_request::nats_subject, qg,
        [rh](ores::nats::message msg) { rh->by_account(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        assign_role_by_name_request::nats_subject, qg,
        [rh](ores::nats::message msg) { rh->assign_by_name(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        revoke_role_by_name_request::nats_subject, qg,
        [rh](ores::nats::message msg) { rh->revoke_by_name(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        suggest_role_commands_request::nats_subject, qg,
        [rh](ores::nats::message msg) { rh->suggest_commands(std::move(msg)); }));

    // --- Tenants ---
    auto th = std::make_shared<tenant_handler>(nats, ctx, signer);
    subs.push_back(nats.queue_subscribe(
        get_tenants_request::nats_subject, qg,
        [th](ores::nats::message msg) { th->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_tenant_request::nats_subject, qg,
        [th](ores::nats::message msg) { th->save(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_tenant_request::nats_subject, qg,
        [th](ores::nats::message msg) { th->del(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_tenant_history_request::nats_subject, qg,
        [th](ores::nats::message msg) { th->history(std::move(msg)); }));

    // --- Tenant statuses ---
    auto tsh = std::make_shared<tenant_status_handler>(nats, ctx, signer);
    subs.push_back(nats.queue_subscribe(
        get_tenant_statuses_request::nats_subject, qg,
        [tsh](ores::nats::message msg) { tsh->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_tenant_status_request::nats_subject, qg,
        [tsh](ores::nats::message msg) { tsh->save(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_tenant_status_request::nats_subject, qg,
        [tsh](ores::nats::message msg) { tsh->del(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_tenant_status_history_request::nats_subject, qg,
        [tsh](ores::nats::message msg) { tsh->history(std::move(msg)); }));

    // --- Tenant types ---
    auto tth = std::make_shared<tenant_type_handler>(nats, ctx, signer);
    subs.push_back(nats.queue_subscribe(
        get_tenant_types_request::nats_subject, qg,
        [tth](ores::nats::message msg) { tth->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_tenant_type_request::nats_subject, qg,
        [tth](ores::nats::message msg) { tth->save(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_tenant_type_request::nats_subject, qg,
        [tth](ores::nats::message msg) { tth->del(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_tenant_type_history_request::nats_subject, qg,
        [tth](ores::nats::message msg) { tth->history(std::move(msg)); }));

    BOOST_LOG_SEV(lg(), debug) << "Registered " << subs.size()
        << " IAM message handlers.";

    return subs;
}

} // namespace ores::iam::messaging
