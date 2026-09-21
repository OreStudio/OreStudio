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
#include "ores.iam.core/messaging/registrar.hpp"
#include "ores.iam.api/messaging/account_contact_information_protocol.hpp"
#include "ores.iam.api/messaging/account_history_protocol.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.iam.api/messaging/bootstrap_protocol.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.iam.api/messaging/reset_protocol.hpp"
#include "ores.iam.api/messaging/session_protocol.hpp"
#include "ores.iam.api/messaging/session_operations_protocol.hpp"
#include "ores.iam.core/messaging/session_registrar.hpp"
#include "ores.iam.api/messaging/session_samples_protocol.hpp"
#include "ores.iam.api/messaging/signup_protocol.hpp"
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.iam.api/messaging/tenant_provisioning_protocol.hpp"
#include "ores.iam.api/messaging/tenant_status_protocol.hpp"
#include "ores.iam.api/messaging/tenant_type_protocol.hpp"
#include "ores.iam.client/client/service_token_provider.hpp"
#include "ores.iam.core/messaging/account_contact_information_registrar.hpp"
#include "ores.iam.core/messaging/account_operations_handler.hpp"
#include "ores.iam.core/messaging/account_registrar.hpp"
#include "ores.iam.core/messaging/account_party_handler.hpp"
#include "ores.iam.core/messaging/auth_handler.hpp"
#include "ores.iam.core/messaging/bootstrap_handler.hpp"
#include "ores.iam.core/messaging/publish_from_dq_handler.hpp"
#include "ores.iam.core/messaging/reset_handler.hpp"
#include "ores.iam.core/messaging/authorization_handler.hpp"
#include "ores.iam.core/messaging/role_registrar.hpp"
#include "ores.iam.core/messaging/account_type_registrar.hpp"
#include "ores.iam.core/messaging/login_info_registrar.hpp"
#include "ores.iam.core/messaging/permission_registrar.hpp"
#include "ores.iam.core/messaging/session_operations_handler.hpp"
#include "ores.iam.core/messaging/tenant_provisioning_handler.hpp"
#include "ores.iam.core/messaging/tenant_registrar.hpp"
#include "ores.iam.core/messaging/tenant_status_registrar.hpp"
#include "ores.iam.core/messaging/tenant_type_registrar.hpp"
#include "ores.iam.core/repository/tenant_lookups.hpp"
#include "ores.iam.core/service/cache/party_cache.hpp"
#include "ores.iam.core/service/cache/party_cache_registrar.hpp"
#include "ores.iam.core/service/internal_impersonation_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <array>
#include <memory>
#include <string>
#include <vector>

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
                             ores::security::jwt::jwt_authenticator signer,
                             std::string service_password) {

    std::vector<ores::nats::service::subscription> subs;
    constexpr auto qg = "ores.iam.service";

    // === Phase 1: Wiring & Readiness ===
    // Construct and subscribe every handler, including this service's own
    // (e.g. auth_handler owns service-login, which Phase 2 below calls on
    // itself). See "Service Bootstrap Phases" in the architecture docs: a
    // subject is answerable from the moment it is subscribed (NATS dispatch
    // runs on its own thread, independent of this function returning), so
    // nothing below may be called until every subject is registered.

    // party_cache is constructed here (token-less) only because auth_handler
    // et al. take a shared_ptr to it; it is not usable until Phase 2 arms it
    // with a token and warms it.
    auto pc = std::make_shared<service::cache::party_cache>(nats);

    // --- Auth ---
    auto ah = std::make_shared<auth_handler>(nats, ctx, signer, pc);
    subs.push_back(
        nats.queue_subscribe(signup_request::nats_subject, qg, [ah](ores::nats::message msg) {
            ah->signup(std::move(msg));
        }));
    subs.push_back(
        nats.queue_subscribe(login_request::nats_subject, qg, [ah](ores::nats::message msg) {
            ah->login(std::move(msg));
        }));
    subs.push_back(
        nats.queue_subscribe(logout_request::nats_subject, qg, [ah](ores::nats::message msg) {
            ah->logout(std::move(msg));
        }));
    subs.push_back(
        nats.queue_subscribe(public_key_request::nats_subject, qg, [ah](ores::nats::message msg) {
            ah->public_key(std::move(msg));
        }));
    subs.push_back(
        nats.queue_subscribe(refresh_request::nats_subject, qg, [ah](ores::nats::message msg) {
            ah->refresh(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        service_login_request::nats_subject, qg, [ah](ores::nats::message msg) {
            ah->service_login(std::move(msg));
        }));

    // --- Bootstrap ---
    auto bh = std::make_shared<bootstrap_handler>(nats, ctx, signer, pc);
    subs.push_back(nats.queue_subscribe(
        bootstrap_status_request::nats_subject, qg, [bh](ores::nats::message msg) {
            bh->status(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        create_initial_admin_request::nats_subject, qg, [bh](ores::nats::message msg) {
            bh->create_admin(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        provision_tenant_request::nats_subject, qg, [bh](ores::nats::message msg) {
            bh->provision_tenant(std::move(msg));
        }));

    // --- Accounts: the derived reads ---
    for (auto& sub : register_account_handlers(nats, ctx, signer))
        subs.push_back(std::move(sub));

    // --- Accounts: the operations a table's columns cannot state ---
    auto acth = std::make_shared<account_operations_handler>(nats, ctx, signer, pc);
    subs.push_back(nats.queue_subscribe(
        save_account_request::nats_subject, qg, [acth](ores::nats::message msg) {
            acth->save(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_account_request::nats_subject, qg, [acth](ores::nats::message msg) {
            acth->remove(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        lock_account_request::nats_subject, qg, [acth](ores::nats::message msg) {
            acth->lock(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        unlock_account_request::nats_subject, qg, [acth](ores::nats::message msg) {
            acth->unlock(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        reset_password_request::nats_subject, qg, [acth](ores::nats::message msg) {
            acth->reset_password(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        change_password_request_typed::nats_subject, qg, [acth](ores::nats::message msg) {
            acth->change_password(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        update_account_request::nats_subject, qg, [acth](ores::nats::message msg) {
            acth->update(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        update_my_email_request::nats_subject, qg, [acth](ores::nats::message msg) {
            acth->update_email(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        select_party_request::nats_subject, qg, [acth](ores::nats::message msg) {
            acth->select_party(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        switch_party_request::nats_subject, qg, [acth](ores::nats::message msg) {
            acth->switch_party(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        set_my_default_party_request::nats_subject, qg, [acth](ores::nats::message msg) {
            acth->set_default_party(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_account_history_request::nats_subject, qg, [acth](ores::nats::message msg) {
            acth->history(std::move(msg));
        }));

    // --- Account parties ---
    // The associations are stored in a junction the codegen owns, but their
    // messaging layer is hand-written, so their subjects are wired here. One
    // subscription per canonical operation, in the order the protocol states
    // them.
    auto aph = std::make_shared<account_party_handler>(nats, ctx, signer);
    subs.push_back(nats.queue_subscribe(
        list_account_parties_request::nats_subject, qg, [aph](ores::nats::message msg) {
            aph->list_account_parties(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        list_by_account_id_account_parties_request::nats_subject,
        qg,
        [aph](ores::nats::message msg) {
            aph->list_by_account_id_account_parties(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_account_party_request::nats_subject, qg, [aph](ores::nats::message msg) {
            aph->get_account_party(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_many_account_parties_request::nats_subject, qg, [aph](ores::nats::message msg) {
            aph->get_many_account_parties(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_account_party_request::nats_subject, qg, [aph](ores::nats::message msg) {
            aph->put_account_party(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        put_many_account_parties_request::nats_subject, qg, [aph](ores::nats::message msg) {
            aph->put_many_account_parties(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_account_party_request::nats_subject, qg, [aph](ores::nats::message msg) {
            aph->delete_account_party(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        delete_many_account_parties_request::nats_subject, qg, [aph](ores::nats::message msg) {
            aph->delete_many_account_parties(std::move(msg));
        }));

    // --- Sessions: the derived CRUD protocol ---
    for (auto& sub : register_session_handlers(nats, ctx, signer))
        subs.push_back(std::move(sub));

    // --- Sessions: the operations a session's CRUD verbs cannot state ---
    auto sh = std::make_shared<session_operations_handler>(nats, ctx, signer);
    subs.push_back(nats.queue_subscribe(
        get_active_sessions_request::nats_subject, qg, [sh](ores::nats::message msg) {
            sh->active(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_session_samples_request::nats_subject, qg, [sh](ores::nats::message msg) {
            sh->samples(std::move(msg));
        }));

    // --- Roles: the derived CRUD protocol ---
    for (auto& sub : register_role_handlers(nats, ctx, signer))
        subs.push_back(std::move(sub));

    // --- Authorization: the operations a role's CRUD verbs cannot state ---
    auto rh = std::make_shared<authorization_handler>(nats, ctx, signer);
    subs.push_back(
        nats.queue_subscribe(assign_role_request::nats_subject, qg, [rh](ores::nats::message msg) {
            rh->assign(std::move(msg));
        }));
    subs.push_back(
        nats.queue_subscribe(revoke_role_request::nats_subject, qg, [rh](ores::nats::message msg) {
            rh->revoke(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_account_roles_request::nats_subject, qg, [rh](ores::nats::message msg) {
            rh->by_account(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_account_permissions_request::nats_subject, qg, [rh](ores::nats::message msg) {
            rh->account_permissions(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        get_role_permissions_request::nats_subject, qg, [rh](ores::nats::message msg) {
            rh->permissions(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        assign_role_by_name_request::nats_subject, qg, [rh](ores::nats::message msg) {
            rh->assign_by_name(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        revoke_role_by_name_request::nats_subject, qg, [rh](ores::nats::message msg) {
            rh->revoke_by_name(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        suggest_role_commands_request::nats_subject, qg, [rh](ores::nats::message msg) {
            rh->suggest_commands(std::move(msg));
        }));

    // --- Tenants ---
    service::internal_impersonation_service impersonation(
        signer, [pc](const std::string& tenant_id, const boost::uuids::uuid& party_id) {
            return pc->compute_visible_party_ids(tenant_id, party_id);
        });
    // --- Tenant provisioning ---
    // The provisioning workflow is hand-written, so its two commands are
    // wired here beside the handler that serves them.
    auto tph =
        std::make_shared<tenant_provisioning_handler>(nats, ctx, signer, std::move(impersonation));
    subs.push_back(nats.queue_subscribe(
        complete_tenant_provisioning_command::nats_subject, qg, [tph](ores::nats::message msg) {
            tph->complete_provisioning(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        provision_acme_tenant_command::nats_subject, qg, [tph](ores::nats::message msg) {
            tph->provision_acme(std::move(msg));
        }));

    // --- Tenants, tenant statuses and tenant types ---
    // The generated registrars own these subjects, so the component registrar
    // asks each of them for its subscriptions rather than naming the subjects
    // again here. A subject added to the protocol is then wired by the same
    // change that declares it.
    for (auto& sub : register_tenant_handlers(nats, ctx, signer))
        subs.push_back(std::move(sub));
    for (auto& sub : register_tenant_status_handlers(nats, ctx, signer))
        subs.push_back(std::move(sub));
    for (auto& sub : register_tenant_type_handlers(nats, ctx, signer))
        subs.push_back(std::move(sub));

    // The rest of the derived resource protocols. Each entity already had a
    // generated header, service and registrar; only the wiring was missing, so
    // the subject had no handler and the shell reached nothing.
    for (auto& sub : register_account_type_handlers(nats, ctx, signer))
        subs.push_back(std::move(sub));
    for (auto& sub : register_login_info_handlers(nats, ctx, signer))
        subs.push_back(std::move(sub));
    for (auto& sub : register_permission_handlers(nats, ctx, signer))
        subs.push_back(std::move(sub));

    // --- System reset ---
    auto rsh = std::make_shared<reset_handler>(nats, ctx, signer);
    subs.push_back(nats.queue_subscribe(
        reset_tenant_command::nats_subject, qg, [rsh](ores::nats::message msg) {
            rsh->reset_tenant(std::move(msg));
        }));
    subs.push_back(nats.queue_subscribe(
        reset_system_command::nats_subject, qg, [rsh](ores::nats::message msg) {
            rsh->reset_system(std::move(msg));
        }));

    // --- Reload token settings on variability change ---
    // Subscribe to the system_setting_changed event so that any update to
    // iam.token.* settings takes effect without restarting the IAM service.
    constexpr std::string_view settings_changed_subject = "ores.variability.system_setting_changed";
    subs.push_back(
        nats.queue_subscribe(settings_changed_subject, qg, [ah, acth](ores::nats::message) {
            ah->reload_token_settings();
            acth->reload_token_settings();
        }));

    BOOST_LOG_SEV(lg(), debug) << "Registered " << subs.size() << " IAM message handlers.";

    // === Phase 2: Post-Readiness Activities ===
    // Every IAM subject (including service-login, above) is now live, so it
    // is safe to mint a token against ourselves and warm party_cache.
    // Generated by the nats-event-cache facet (ores.cpp.nats-event-cache).
    // read_parties_for_cache requires a signed JWT; authenticate as IAM's
    // own service account so party_cache's calls carry one.
    pc->set_token_provider(ores::iam::client::make_service_token_provider(
        nats, ctx.service_account(), std::move(service_password)));

    std::vector<std::string> tenant_ids;
    try {
        const auto tenants = repository::read_all_active_tenants(ctx);
        tenant_ids.reserve(tenants.size());
        for (const auto& t : tenants)
            tenant_ids.push_back(boost::uuids::to_string(t.id));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Party cache warm-up failed: " << e.what();
    }
    subs.push_back(service::cache::warm_and_subscribe_party_cache(nats, pc, tenant_ids));

    // --- Account contact information ---
    auto aci_subs = register_account_contact_information_handlers(nats, ctx, signer);
    subs.insert(subs.end(),
                std::make_move_iterator(aci_subs.begin()),
                std::make_move_iterator(aci_subs.end()));

    // --- Publish-from-DQ workflow step handlers ---
    {
        auto h = std::make_shared<publish_from_dq_handler>(nats, ctx);
        // The subjects are the protocol's own. The DQ artefact-type rows that
        // publish onto them carry the same strings in SQL, so a subscription
        // that falls behind the model is a compile-time name, not a literal a
        // reviewer has to compare against a table.
        static constexpr std::array publish_subjects{
            publish_accounts_from_dq_request::nats_subject,
            publish_account_contact_informations_from_dq_request::nats_subject,
        };
        for (const auto subject : publish_subjects) {
            subs.push_back(nats.queue_subscribe(
                subject, qg, [h](ores::nats::message msg) { h->handle(std::move(msg)); }));
        }
    }

    return subs;
}

} // namespace ores::iam::messaging
