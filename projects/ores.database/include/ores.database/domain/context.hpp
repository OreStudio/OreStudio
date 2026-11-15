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
#ifndef ORES_DATABASE_CONTEXT_HPP
#define ORES_DATABASE_CONTEXT_HPP

#include "ores.database/export.hpp"
#include "ores.database/pq/options.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::database {

namespace pq {
class pool;
class transaction;
struct session_context;
}

/**
 * @brief Context for the operations on a postgres database.
 *
 * The context names who a statement acts for: the tenant and, optionally, the
 * party and the parties it sees, which the row-level security policies read;
 * the actor, the end user behind the request, which modified_by records; and
 * the service account, the service performing the write, which performed_by
 * records. Every transaction it opens sets them as transaction-local settings,
 * so one pool serves every tenant and party.
 *
 * Copies share one libpq pool, opened on first use. A context bound to a unit
 * of work runs every call on that unit of work's transaction.
 */
class ORES_DATABASE_EXPORT context {
public:
    /**
     * @brief Constructs a tenant-only context.
     */
    explicit context(pq::connection_options options,
                     utility::uuid::tenant_id tenant_id,
                     std::string actor = "",
                     std::string service_account = "",
                     pq::open_policy policy = {});

    /**
     * @brief Constructs a tenant-and-party-aware context.
     */
    explicit context(pq::connection_options options,
                     utility::uuid::tenant_id tenant_id,
                     boost::uuids::uuid party_id,
                     std::vector<boost::uuids::uuid> visible_party_ids,
                     std::string actor = "",
                     std::string service_account = "",
                     pq::open_policy policy = {});

    /**
     * @brief The transaction of the unit of work this context is bound to, if
     * any.
     */
    [[nodiscard]] pq::transaction* active_transaction() const {
        return active_transaction_;
    }

    /**
     * @brief A copy of this context bound to @p transaction.
     *
     * Every call made with the copy runs on that transaction, which must
     * outlive it: a unit of work hands out such a context for its own lifetime.
     */
    [[nodiscard]] context with_transaction(pq::transaction& transaction) const {
        auto copy = *this;
        copy.active_transaction_ = &transaction;
        return copy;
    }

    /**
     * @brief The options a connection of this context opens with.
     */
    [[nodiscard]] const pq::connection_options& connection_options() const {
        return options_;
    }

    [[nodiscard]] const utility::uuid::tenant_id& tenant_id() const {
        return tenant_id_;
    }

    [[nodiscard]] std::optional<boost::uuids::uuid> party_id() const {
        return party_id_;
    }

    [[nodiscard]] const std::vector<boost::uuids::uuid>& visible_party_ids() const {
        return visible_party_ids_;
    }

    [[nodiscard]] const std::string& actor() const {
        return actor_;
    }

    [[nodiscard]] const std::string& service_account() const {
        return service_account_;
    }

    /**
     * @brief The roles the caller holds, when the request named them.
     */
    [[nodiscard]] const std::optional<std::vector<std::string>>& roles() const {
        return roles_;
    }

    [[nodiscard]] context with_roles(std::vector<std::string> roles) const {
        auto copy = *this;
        copy.roles_ = std::move(roles);
        return copy;
    }

    /**
     * @brief A context for another tenant, with no party.
     *
     * The service account, the pool, the open policy and the roles are kept, so
     * narrowing a request's context never widens what it may do. A context bound
     * to a transaction refuses a change of tenant or party, because the
     * transaction set them when it opened and a copy would still run under them.
     * A read for another tenant or party is made before the unit of work opens:
     * a unit of work holds its connection from the start, and a second one taken
     * while it does can wait on a bounded pool for ever.
     */
    [[nodiscard]] context with_tenant(utility::uuid::tenant_id tenant_id, std::string actor) const {
        auto scoped = *this;
        scoped.tenant_id_ = std::move(tenant_id);
        scoped.party_id_.reset();
        scoped.visible_party_ids_.clear();
        scoped.actor_ = std::move(actor);
        refuse_change_inside_transaction(scoped);
        return scoped;
    }

    /**
     * @brief A context for a tenant and a party, with the parties it sees.
     *
     * Keeps and refuses what with_tenant() keeps and refuses.
     */
    [[nodiscard]] context with_party(utility::uuid::tenant_id tenant_id,
                                     boost::uuids::uuid party_id,
                                     std::vector<boost::uuids::uuid> visible_party_ids,
                                     std::string actor) const {
        auto scoped = *this;
        scoped.tenant_id_ = std::move(tenant_id);
        scoped.party_id_ = party_id;
        scoped.visible_party_ids_ = std::move(visible_party_ids);
        scoped.actor_ = std::move(actor);
        refuse_change_inside_transaction(scoped);
        return scoped;
    }

    /**
     * @brief The libpq pool every copy of this context shares, opened on its
     * first use under the context's open policy.
     */
    pq::pool& pq_pool() const;

    /**
     * @brief The number of connections the pool opens, when it opens.
     */
    void set_pq_pool_size(std::size_t size);

    /**
     * @brief The session a transaction of this context sets.
     */
    pq::session_context pq_session() const;

private:
    void refuse_change_inside_transaction(const context& to) const {
        if (active_transaction_ == nullptr)
            return;
        if (to.tenant_id_ == tenant_id_ && to.party_id_ == party_id_ &&
            to.visible_party_ids_ == visible_party_ids_)
            return;
        throw std::logic_error(
            "A context bound to a transaction cannot change its tenant or party: the "
            "transaction set them when it opened. Make the read before the unit of work opens.");
    }

    pq::connection_options options_;
    pq::open_policy policy_;
    utility::uuid::tenant_id tenant_id_;
    std::optional<boost::uuids::uuid> party_id_;
    std::vector<boost::uuids::uuid> visible_party_ids_;
    std::string actor_;
    std::string service_account_;
    std::optional<std::vector<std::string>> roles_;
    pq::transaction* active_transaction_ = nullptr;

    /**
     * @brief The slot that holds the shared pool and its lock.
     *
     * Declared here and defined in the implementation, so that this header
     * names neither =libpq=, =<mutex>= nor the pool. Every copy of a context
     * shares one slot, which is why the slot is shared and not copied.
     */
    struct pq_slot;

    std::shared_ptr<pq_slot> pq_;
};

}

#endif
