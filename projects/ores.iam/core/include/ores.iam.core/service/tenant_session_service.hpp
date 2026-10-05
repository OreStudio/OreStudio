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
#ifndef ORES_IAM_CORE_SERVICE_TENANT_SESSION_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_TENANT_SESSION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.eventing.core/service/cache/partition_token_cache.hpp"
#include "ores.iam.api/messaging/tenant_session_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Who asks to enter or leave a tenant, as their token states it.
 *
 * The permissions are the caller's effective permissions read from the
 * database, sorted, and not the token's: a person's token carries none, so a
 * check against it would pass every caller.
 */
struct tenant_session_caller {
    boost::uuids::uuid account_id;
    std::string username;
    std::string session_id;
    utility::uuid::tenant_id tenant_id;
    std::optional<std::string> party_id;
    std::optional<std::string> acting_from_tenant_id;
    std::vector<std::string> permissions;
};

/**
 * @brief A service that reads inside a tenant with no caller, such as IAM
 * loading one tenant's partition of its party cache.
 *
 * The permissions are the scope the read needs and nothing more; the token
 * carries them and no others.
 */
struct tenant_reader {
    boost::uuids::uuid account_id;
    std::string username;
    std::vector<std::string> permissions;
};

/**
 * @brief Why an entry or an exit is refused.
 */
enum class tenant_session_refusal {
    outside_system_administration,
    already_inside_a_tenant,
    not_permitted,
    unreadable_tenant_id,
    system_tenant,
    unknown_tenant,
    no_system_party,
    no_read_permissions,
    not_inside_a_tenant
};

/**
 * @brief The words a caller reads for each refusal.
 */
ORES_IAM_CORE_EXPORT std::string_view describe(tenant_session_refusal refusal);

/**
 * @brief Enters and leaves a tenant as a system administrator.
 *
 * A system administrator reads a tenant's data from inside it. Entering
 * issues a token scoped to the tenant, acting as the tenant's system party,
 * with every party of the tenant visible. The token carries only the read
 * permissions the administrator holds and names the tenant they act from, so
 * a write handler refuses it and a reader can tell it is not a member's.
 * Entering and leaving are recorded in the tenant's own audit trail.
 */
class ORES_IAM_CORE_EXPORT tenant_session_service {
public:
    using context = ores::database::context;

    /// The permission a caller needs to enter a tenant.
    static constexpr std::string_view impersonate_permission = "iam::tenants:impersonate";

    /**
     * @param ctx The service's own context; the tenant and the system tenant
     * are reached from it.
     * @param signer Signs the tenant session's token.
     * @param lifetime How long a tenant session lasts.
     */
    tenant_session_service(context ctx,
                           security::jwt::jwt_authenticator signer,
                           std::chrono::seconds lifetime);

    messaging::enter_tenant_response enter(const tenant_session_caller& caller,
                                           const messaging::enter_tenant_request& request);

    messaging::leave_tenant_response leave(const tenant_session_caller& caller);

    /**
     * @brief A token for @p reader acting inside @p tenant_id, by Token
     * Exchange with delegation semantics.
     *
     * The token names the tenant, the tenant's system party and the parties
     * it sees, and carries only the reader's permissions. Outside the system
     * tenant it names the system tenant the reader acts from, as an entry
     * does. Each issue is logged. Called in-process only: no subject serves
     * it. A tenant that is unknown or has no system party yields no token.
     */
    eventing::service::cache::partition_token read_inside(const tenant_reader& reader,
                                                          const std::string& tenant_id,
                                                          std::chrono::seconds lifetime);

    /**
     * @brief The read permissions among those granted.
     *
     * A read permission is a catalogue code whose verb is read. A grant of
     * every permission yields every read code in the catalogue.
     *
     * @param granted The caller's effective permissions, sorted.
     * @param catalogue Every permission code the deployment defines.
     */
    static std::vector<std::string> read_only(const std::vector<std::string>& granted,
                                              const std::vector<std::string>& catalogue);

private:
    /**
     * @brief The claims of a session inside @p target: its tenant, its system
     * party and the parties that party sees. Empty when the tenant has no
     * system party.
     */
    struct inside_party {
        std::string party_id;
        std::string party_name;
        std::vector<std::string> visible;
    };
    std::optional<inside_party> party_inside(const utility::uuid::tenant_id& target,
                                             const std::string& username) const;

    context ctx_;
    security::jwt::jwt_authenticator signer_;
    std::chrono::seconds lifetime_;
};

}

#endif
