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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/connection.hpp"
#include "ores.database/pq/pool.hpp"
#include "ores.database/pq/session_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <stdexcept>
#include <utility>

namespace ores::database {

/**
 * @brief The two halves a context carries: the database, which every copy
 * shares, and the identity, which a copy that narrows the request owns alone.
 */
struct context::impl {
    impl(std::shared_ptr<connection> database,
         utility::uuid::tenant_id tenant_id,
         std::optional<boost::uuids::uuid> party_id,
         std::vector<boost::uuids::uuid> visible_party_ids,
         std::string actor,
         std::string service_account)
        : database(std::move(database))
        , tenant_id(std::move(tenant_id))
        , party_id(party_id)
        , visible_party_ids(std::move(visible_party_ids))
        , actor(std::move(actor))
        , service_account(std::move(service_account)) {}

    std::shared_ptr<connection> database;
    utility::uuid::tenant_id tenant_id;
    std::optional<boost::uuids::uuid> party_id;
    std::vector<boost::uuids::uuid> visible_party_ids;
    std::string actor;
    std::string service_account;
    std::optional<std::vector<std::string>> roles;
    pq::transaction* active_transaction = nullptr;
};

context::context(pq::connection_options options,
                 utility::uuid::tenant_id tenant_id,
                 std::string actor,
                 std::string service_account)
    : impl_(std::make_shared<impl>(std::make_shared<connection>(std::move(options)),
                                   std::move(tenant_id),
                                   std::nullopt,
                                   std::vector<boost::uuids::uuid>{},
                                   std::move(actor),
                                   std::move(service_account))) {}

context::context(pq::connection_options options,
                 utility::uuid::tenant_id tenant_id,
                 std::string actor,
                 std::string service_account,
                 pq::open_policy policy)
    : impl_(std::make_shared<impl>(
          std::make_shared<connection>(std::move(options), std::move(policy)),
          std::move(tenant_id),
          std::nullopt,
          std::vector<boost::uuids::uuid>{},
          std::move(actor),
          std::move(service_account))) {}

context::context(pq::connection_options options,
                 utility::uuid::tenant_id tenant_id,
                 boost::uuids::uuid party_id,
                 std::vector<boost::uuids::uuid> visible_party_ids,
                 std::string actor,
                 std::string service_account)
    : impl_(std::make_shared<impl>(std::make_shared<connection>(std::move(options)),
                                   std::move(tenant_id),
                                   party_id,
                                   std::move(visible_party_ids),
                                   std::move(actor),
                                   std::move(service_account))) {}

context::context(pq::connection_options options,
                 utility::uuid::tenant_id tenant_id,
                 boost::uuids::uuid party_id,
                 std::vector<boost::uuids::uuid> visible_party_ids,
                 std::string actor,
                 std::string service_account,
                 pq::open_policy policy)
    : impl_(std::make_shared<impl>(
          std::make_shared<connection>(std::move(options), std::move(policy)),
          std::move(tenant_id),
          party_id,
          std::move(visible_party_ids),
          std::move(actor),
          std::move(service_account))) {}

context::context(std::shared_ptr<impl> p)
    : impl_(std::move(p)) {}

context::~context() = default;
context::context(const context&) = default;
context& context::operator=(const context&) = default;

pq::transaction* context::active_transaction() const {
    return impl_->active_transaction;
}

context context::with_transaction(pq::transaction& transaction) const {
    auto copy = std::make_shared<impl>(*impl_);
    copy->active_transaction = &transaction;
    return context(std::move(copy));
}

const pq::connection_options& context::connection_options() const {
    return impl_->database->options();
}

const utility::uuid::tenant_id& context::tenant_id() const {
    return impl_->tenant_id;
}

std::optional<boost::uuids::uuid> context::party_id() const {
    return impl_->party_id;
}

const std::vector<boost::uuids::uuid>& context::visible_party_ids() const {
    return impl_->visible_party_ids;
}

const std::string& context::actor() const {
    return impl_->actor;
}

const std::string& context::service_account() const {
    return impl_->service_account;
}

const std::optional<std::vector<std::string>>& context::roles() const {
    return impl_->roles;
}

context context::with_roles(std::vector<std::string> roles) const {
    auto copy = std::make_shared<impl>(*impl_);
    copy->roles = std::move(roles);
    return context(std::move(copy));
}

context context::with_tenant(utility::uuid::tenant_id tenant_id, std::string actor) const {
    auto scoped = std::make_shared<impl>(*impl_);
    scoped->tenant_id = std::move(tenant_id);
    scoped->party_id.reset();
    scoped->visible_party_ids.clear();
    scoped->actor = std::move(actor);
    refuse_change_inside_transaction(*scoped);
    return context(std::move(scoped));
}

context context::with_party(utility::uuid::tenant_id tenant_id,
                            boost::uuids::uuid party_id,
                            std::vector<boost::uuids::uuid> visible_party_ids,
                            std::string actor) const {
    auto scoped = std::make_shared<impl>(*impl_);
    scoped->tenant_id = std::move(tenant_id);
    scoped->party_id = party_id;
    scoped->visible_party_ids = std::move(visible_party_ids);
    scoped->actor = std::move(actor);
    refuse_change_inside_transaction(*scoped);
    return context(std::move(scoped));
}

pq::pool& context::pq_pool() const {
    return impl_->database->pool();
}

void context::set_pq_pool_settings(pq::pool_settings settings) {
    impl_->database->set_pool_settings(settings);
}

void context::set_listener_endpoint(std::string host, std::uint16_t port) {
    impl_->database->set_listener_endpoint(std::move(host), port);
}

pq::connection_options context::listener_connection_options() const {
    return impl_->database->listener_options();
}

pq::session_context context::pq_session() const {
    return {.tenant = impl_->tenant_id,
            .party = impl_->party_id,
            .visible_parties = impl_->visible_party_ids,
            .actor = impl_->actor,
            .service = impl_->service_account};
}

void context::refuse_change_inside_transaction(const impl& to) const {
    if (impl_->active_transaction == nullptr)
        return;
    if (to.tenant_id == impl_->tenant_id && to.party_id == impl_->party_id &&
        to.visible_party_ids == impl_->visible_party_ids)
        return;
    throw std::logic_error(
        "A context bound to a transaction cannot change its tenant or party: the "
        "transaction set them when it opened. Make the read before the unit of work opens.");
}

}
