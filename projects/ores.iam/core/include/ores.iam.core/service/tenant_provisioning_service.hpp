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
#ifndef ORES_IAM_SERVICE_TENANT_PROVISIONING_SERVICE_HPP
#define ORES_IAM_SERVICE_TENANT_PROVISIONING_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/service/account_operations_service.hpp"
#include "ores.iam.core/service/account_party_service.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::service {

/// The role a provisioned tenant's own administrator takes. It carries the
/// permissions the tenant's administrator holds, as against SuperAdmin, which
/// holds the platform-level tenant verbs.
inline constexpr std::string_view tenant_admin_role = "TenantAdmin";

/**
 * @brief Creates a tenant and its first administrator.
 *
 * The sequence every provisioning verb runs, in one place so that a verb
 * retiring and its successor cannot drift apart while both are reachable:
 *
 *  1. The SQL provisioner creates the tenant row and copies the system
 *     tenant's registered data into it -- roles, permissions and the lookup
 *     tables -- seeds the world business centre, and creates the tenant's
 *     system party. It requires system tenant context.
 *  2. The administrator account is created in the new tenant's context. The
 *     tenant does not exist before the first call, so the account cannot be
 *     made first.
 *  3. The account is associated with the system party the provisioner
 *     returned, which is the party a person starts in.
 *  4. The TenantAdmin role is assigned. The provisioner copies role
 *     *definitions* into the new tenant but assigns none of them, and a role
 *     is what carries the permissions: without this the administrator can sign
 *     in and do nothing.
 */
class ORES_IAM_CORE_EXPORT tenant_provisioning_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.tenant_provisioning_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /// What a successful provisioning left behind.
    struct provisioned_tenant {
        std::string tenant_id;
        std::string account_id;
    };

    explicit tenant_provisioning_service(context ctx)
        : ctx_(std::move(ctx)) {}

    /**
     * @brief Creates the tenant and its administrator.
     *
     * The audit columns of every write take the IAM service account rather
     * than the new administrator: that username is not a known account until
     * this call has created it, and the validation on those columns refuses a
     * name it cannot resolve.
     *
     * @param tenant_type Tenant type, as @c ores_iam_tenant_types_tbl states
     *                    it. The tenant row refuses a type that no row declares.
     * @param code        Unique tenant code.
     * @param name        Tenant display name.
     * @param hostname    Unique tenant hostname.
     * @param description Optional tenant description.
     * @param admin_username Administrator's username, with no hostname part.
     * @param admin_email    Administrator's email address.
     * @param admin_password Administrator's initial password, in the clear.
     *
     * @throws std::runtime_error When the provisioner answers incompletely.
     */
    [[nodiscard]] provisioned_tenant provision(const std::string& tenant_type,
                                               const std::string& code,
                                               const std::string& name,
                                               const std::string& hostname,
                                               const std::string& description,
                                               const std::string& admin_username,
                                               const std::string& admin_email,
                                               const std::string& admin_password) const {
        using ores::database::repository::execute_parameterized_multi_column_query;
        using ores::database::service::tenant_context;

        auto sys_ctx = tenant_context::with_system_tenant(ctx_);
        const auto rows = execute_parameterized_multi_column_query(
            sys_ctx,
            "SELECT tenant_id::text, system_party_id::text"
            " FROM ores_iam_provision_tenant_fn($1, $2, $3, $4, $5, $6)",
            {tenant_type, code, name, hostname, description, ctx_.service_account()},
            lg(),
            "Provisioning tenant");

        if (rows.empty() || rows[0].size() < 2 || !rows[0][0] || !rows[0][1])
            throw std::runtime_error("Provisioner returned incomplete result");

        const auto& tenant_id = *rows[0][0];
        const auto& system_party_id = *rows[0][1];
        BOOST_LOG_SEV(lg(), ores::logging::info)
            << "Provisioned tenant " << code << " (id: " << tenant_id
            << ", system party: " << system_party_id << ")";

        auto tenant_ctx = tenant_context::with_tenant(ctx_, tenant_id);
        account_operations_service accounts(tenant_ctx);
        auto account = accounts.create_account(
            admin_username, admin_email, admin_password, ctx_.service_account());

        domain::account_party link;
        link.account_id = account.id;
        link.party_id = boost::uuids::string_generator{}(system_party_id);
        link.tenant_id = tenant_id;
        link.modified_by = admin_username;
        link.performed_by = admin_username;
        link.change_reason_code = "system.initial_load";
        link.change_commentary = "Provision tenant: associate admin with system party";
        account_party_service parties(tenant_ctx);
        parties.save_account_party(link);
        BOOST_LOG_SEV(lg(), ores::logging::info)
            << "Associated " << admin_username << " with system party " << system_party_id;

        authorization_service authorization(tenant_ctx);
        if (auto role = authorization.find_role_by_name(std::string(tenant_admin_role))) {
            authorization.assign_role(account.id, role->id, ctx_.service_account());
            BOOST_LOG_SEV(lg(), ores::logging::info)
                << "Assigned " << tenant_admin_role << " to " << admin_username;
        } else {
            BOOST_LOG_SEV(lg(), ores::logging::error)
                << "Tenant " << code << " has no " << tenant_admin_role << " role, so "
                << admin_username << " was created without one and holds no permissions";
        }

        return {tenant_id, boost::uuids::to_string(account.id)};
    }

private:
    context ctx_;
};

}

#endif
