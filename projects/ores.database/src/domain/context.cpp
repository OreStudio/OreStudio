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
#include "ores.database/pq/pool.hpp"
#include "ores.database/pq/transaction.hpp"
#include <mutex>

namespace ores::database {

struct context::pq_slot {
    std::mutex mutex;
    std::unique_ptr<pq::pool> pool;
    std::size_t size = 2;
};

context::context(pq::connection_options options,
                 utility::uuid::tenant_id tenant_id,
                 std::string actor,
                 std::string service_account,
                 pq::open_policy policy)
    : options_(std::move(options))
    , policy_(std::move(policy))
    , tenant_id_(std::move(tenant_id))
    , actor_(std::move(actor))
    , service_account_(std::move(service_account))
    , pq_(std::make_shared<pq_slot>()) {}

context::context(pq::connection_options options,
                 utility::uuid::tenant_id tenant_id,
                 boost::uuids::uuid party_id,
                 std::vector<boost::uuids::uuid> visible_party_ids,
                 std::string actor,
                 std::string service_account,
                 pq::open_policy policy)
    : options_(std::move(options))
    , policy_(std::move(policy))
    , tenant_id_(std::move(tenant_id))
    , party_id_(party_id)
    , visible_party_ids_(std::move(visible_party_ids))
    , actor_(std::move(actor))
    , service_account_(std::move(service_account))
    , pq_(std::make_shared<pq_slot>()) {}

void context::set_pq_pool_size(std::size_t size) {
    pq_->size = size;
}

pq::pool& context::pq_pool() const {
    std::scoped_lock lock(pq_->mutex);
    if (!pq_->pool)
        pq_->pool = std::make_unique<pq::pool>(options_, pq_->size, policy_);
    return *pq_->pool;
}

pq::session_context context::pq_session() const {
    return {.tenant = tenant_id_,
            .party = party_id_,
            .visible_parties = visible_party_ids_,
            .actor = actor_,
            .service = service_account_};
}

}
