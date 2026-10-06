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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_API_DOMAIN_RUN_GRANT_HPP
#define ORES_IAM_API_DOMAIN_RUN_GRANT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief A person's consent that runs of one resource act in one tenant and one party with one
 * role.
 *
 * A run grant records that a person allows the runs of one resource, such as a
 * scheduled report definition, to act in one tenant and one party with one role.
 * It holds no secret: using it needs a step service's own token and the token
 * exchange, and IAM re-checks the grant and its grantor at every use. It is IAM's
 * form of the Grant Management pattern, and
 * [[id:78B82824-0BBB-4442-A9FA-E1F265DF57BD][Run grants and run tokens]] states
 * its checks and its lifecycle.
 *
 * The table is readable over the wire and written only by IAM's own create and
 * revoke operations, which check that the grantor holds the role in the party:
 * :client_read_only: removes the generic write verbs from the wire and keeps
 * the repository writable for those operations.
 */
struct run_grant final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this run grant.
     *
     * The grant id. It is random, and it is not a credential.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party the runs act in. Enforced by row level security, so a party sees only its
     * own grants.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief What the grant serves, as <component>.<entity>/<id>, for example
     * reporting.report_definition/2d6fee63-....
     */
    std::string resource;

    /**
     * @brief The account that gave the consent. A second create for the same party, resource and
     * grantor returns this grant rather than a new one.
     */
    boost::uuids::uuid grantor_account_id;

    /**
     * @brief The role a run token carries. At create and at every exchange, the grantor must hold
     * every permission of this role in the party.
     */
    boost::uuids::uuid role_id;

    /**
     * @brief The service names that may exchange the grant, separated by commas, named by the
     * service role each one's account holds, for example
     * ReportingService,OreService,ComputeService.
     */
    std::string audience;

    /**
     * @brief The number of runs the grant serves; zero for a standing grant. A manual run's grant
     * serves one.
     */
    int max_runs = 0;

    /**
     * @brief The instant after which the grant serves no run; empty for a standing grant.
     */
    std::chrono::system_clock::time_point not_after = {};

    /**
     * @brief When the grant was revoked; empty while it is active.
     */
    std::chrono::system_clock::time_point revoked_at = {};

    /**
     * @brief Who revoked the grant: a username, or a service account.
     */
    std::string revoked_by;

    /**
     * @brief Why the grant was revoked: unscheduled, deleted, party_changed or a person's reason.
     */
    std::string revoke_reason;

    /**
     * @brief Username of the person who last modified this run grant.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const run_grant&, const run_grant&) = default;
};

/**
 * @brief Dispatch-key identifier for run_grant, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const run_grant&) {
    return "ores.iam.run_grant";
}

}

#endif
