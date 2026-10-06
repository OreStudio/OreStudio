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
#ifndef ORES_IAM_API_DOMAIN_ROLE_GRANT_REQUEST_HPP
#define ORES_IAM_API_DOMAIN_ROLE_GRANT_REQUEST_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief Who would hold the roles an approval request asks for.
 *
 * The IAM detail of an iam.role_grant approval request. The request itself, who
 * asked, when, why and where it stands, is the inbox's record; this row names
 * the account that would hold the roles, keyed by the request it details. The
 * roles asked for are rows of
 * [[id:E07B21AB-F875-4549-BDFC-04EE98C246D0][ores.iam.role_grant_request_role]], one each, so a
 * request may ask for many.
 *
 * The account is usually the person who asked, but a system may ask on a
 * person's behalf, so it is a column of its own rather than the request's
 * requested_by.
 *
 * Who asked for which role is not every member's business, so the generated
 * reads require iam::role_grant_requests:read, as every generated read
 * requires its resource's read code. The person who asked reads their own requests
 * through the inbox's inbox.v1.approval-requests.mine. See
 * [[id:804C7048-DBBF-4B39-8737-BFB4949884C4][Authorised reads]].
 */
struct role_grant_request final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The approval request this row details. FK reference to
     * ores_inbox_approval_requests_tbl.
     */
    boost::uuids::uuid request_id;

    /**
     * @brief The account that would hold the roles. FK reference to ores_iam_accounts_tbl.
     */
    boost::uuids::uuid account_id;

    /**
     * @brief Username of the person who last modified this role grant request.
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
    friend bool operator==(const role_grant_request&, const role_grant_request&) = default;
};

/**
 * @brief Dispatch-key identifier for role_grant_request, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const role_grant_request&) {
    return "ores.iam.role_grant_request";
}

}

#endif
