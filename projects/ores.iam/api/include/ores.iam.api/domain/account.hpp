/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_IAM_DOMAIN_ACCOUNT_HPP
#define ORES_IAM_DOMAIN_ACCOUNT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>

namespace ores::iam::domain {

/**
 * @brief Represents an account for an entity in the system.
 */
struct account final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Unique identifier for the account.
     */
    boost::uuids::uuid id;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Username of the person who recorded this version in the system.
     */
    std::string modified_by;

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
     * @brief Username of the account that performed this operation.
     */
    std::string performed_by;

    /**
     * @brief Account type classification.
     *
     * Determines account capabilities. 'user' accounts can login with password,
     * while 'service', 'algorithm', and 'llm' accounts authenticate via sessions.
     */
    std::string account_type = "user";

    /**
     * @brief Unique username for login purposes.
     */
    std::string username;

    /**
     * @brief Hashed password for secure authentication.
     */
    std::string password_hash;

    /**
     * @brief Salt used in password hashing for additional security.
     */
    std::string password_salt;

    /**
     * @brief Time-based One-Time Password secret for two-factor authentication.
     */
    std::string totp_secret;

    /**
     * @brief Email address associated with the account.
     */
    std::string email;

    /**
     * @brief Party to log into automatically when quick-login is enabled.
     *
     * Soft reference to a party owned by ores.refdata; unset means the
     * account must always go through the party picker.
     */
    std::optional<boost::uuids::uuid> default_party_id;

    /**
     * @brief Profile picture for this account.
     *
     * Soft reference to an image owned by ores.assets. A nil UUID
     * (boost::uuids::nil_uuid()) means the account has no profile picture.
     *
     * A second std::optional<boost::uuids::uuid> field on this struct (in
     * addition to default_party_id) triggers a reflect-cpp aggregate
     * field-count miscount that corrupts JSON/table serialization for
     * multi-element vectors (segfault/duplicate-field exceptions) — hence
     * the nil-sentinel plain-uuid representation instead of optional.
     */
    boost::uuids::uuid image_id = boost::uuids::nil_uuid();

    /**
     * @brief Timestamp when this version of the record was recorded in the system.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
