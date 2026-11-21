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
#include "ores.utility/uuid/tenant_id_fwd.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace ores::database {

namespace pq {
struct connection_options;
struct open_policy;
struct pool_settings;
class pool;
class transaction;
struct session_context;
}

class connection;

/**
 * @brief Who a request acts for, and the database it acts on.
 *
 * The context names the tenant and, optionally, the party and the parties it
 * sees, which the row-level security policies read; the actor, the end user
 * behind the request, which modified_by records; and the service account, the
 * service performing the write, which performed_by records. Every transaction it
 * opens sets them as transaction-local settings, so one pool serves every tenant
 * and party.
 *
 * It carries both halves of the database in one value, and neither half is
 * defined here: the connection lives behind a shared pointer and the identity
 * behind an implementation the translation unit owns. Copies share one libpq
 * pool, opened on first use, and a copy that changes the identity copies it,
 * because two requests never share an identity.
 *
 * A context bound to a unit of work runs every call on that unit of work's
 * transaction.
 */
class ORES_DATABASE_EXPORT context final {
public:
    /**
     * @brief Constructs a tenant-only context.
     */
    explicit context(pq::connection_options options,
                     utility::uuid::tenant_id tenant_id,
                     std::string actor = "",
                     std::string service_account = "");

    /**
     * @brief Constructs a tenant-only context that opens its pool under
     * @p policy.
     */
    context(pq::connection_options options,
            utility::uuid::tenant_id tenant_id,
            std::string actor,
            std::string service_account,
            pq::open_policy policy);

    /**
     * @brief Constructs a tenant-and-party-aware context.
     */
    explicit context(pq::connection_options options,
                     utility::uuid::tenant_id tenant_id,
                     boost::uuids::uuid party_id,
                     std::vector<boost::uuids::uuid> visible_party_ids,
                     std::string actor = "",
                     std::string service_account = "");

    /**
     * @brief Constructs a tenant-and-party-aware context that opens its pool
     * under @p policy.
     */
    context(pq::connection_options options,
            utility::uuid::tenant_id tenant_id,
            boost::uuids::uuid party_id,
            std::vector<boost::uuids::uuid> visible_party_ids,
            std::string actor,
            std::string service_account,
            pq::open_policy policy);

    ~context();

    /**
     * @brief A context is copied, never moved: copying one is a shared pointer
     * increment, and a moved-from context would be a null one.
     */
    context(const context&);
    context& operator=(const context&);

    /**
     * @brief The transaction of the unit of work this context is bound to, if
     * any.
     */
    [[nodiscard]] pq::transaction* active_transaction() const;

    /**
     * @brief A copy of this context bound to @p transaction.
     *
     * Every call made with the copy runs on that transaction, which must
     * outlive it: a unit of work hands out such a context for its own lifetime.
     */
    [[nodiscard]] context with_transaction(pq::transaction& transaction) const;

    /**
     * @brief The options a connection of this context opens with.
     */
    [[nodiscard]] const pq::connection_options& connection_options() const;

    [[nodiscard]] const utility::uuid::tenant_id& tenant_id() const;

    [[nodiscard]] std::optional<boost::uuids::uuid> party_id() const;

    [[nodiscard]] const std::vector<boost::uuids::uuid>& visible_party_ids() const;

    [[nodiscard]] const std::string& actor() const;

    [[nodiscard]] const std::string& service_account() const;

    /**
     * @brief The roles the caller holds, when the request named them.
     */
    [[nodiscard]] const std::optional<std::vector<std::string>>& roles() const;

    [[nodiscard]] context with_roles(std::vector<std::string> roles) const;

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
    [[nodiscard]] context with_tenant(utility::uuid::tenant_id tenant_id, std::string actor) const;

    /**
     * @brief A context for a tenant and a party, with the parties it sees.
     *
     * Keeps and refuses what with_tenant() keeps and refuses.
     */
    [[nodiscard]] context with_party(utility::uuid::tenant_id tenant_id,
                                     boost::uuids::uuid party_id,
                                     std::vector<boost::uuids::uuid> visible_party_ids,
                                     std::string actor) const;

    /**
     * @brief The libpq pool every copy of this context shares, opened on its
     * first use under the context's open policy.
     */
    pq::pool& pq_pool() const;

    /**
     * @brief The connections the pool keeps and the ones a burst may reach,
     * when it opens.
     */
    void set_pq_pool_settings(pq::pool_settings settings);

    /**
     * @brief The endpoint the notification listener connects to, when it is not
     * the one this context's service uses.
     */
    void set_listener_endpoint(std::string host, std::uint16_t port);

    /**
     * @brief The options a listener's connection opens with: this context's
     * endpoint, or the listener's own when one is named.
     */
    [[nodiscard]] pq::connection_options listener_connection_options() const;

    /**
     * @brief The session a transaction of this context sets.
     */
    pq::session_context pq_session() const;

private:
    struct impl;

    explicit context(std::shared_ptr<impl> p);

    void refuse_change_inside_transaction(const impl& to) const;

    std::shared_ptr<impl> impl_;
};

}

#endif
