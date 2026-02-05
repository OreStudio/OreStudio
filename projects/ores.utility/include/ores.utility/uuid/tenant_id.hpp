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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_UTILITY_UUID_TENANT_ID_HPP
#define ORES_UTILITY_UUID_TENANT_ID_HPP

#include <string>
#include <expected>
#include <string_view>
#include <boost/uuid/uuid.hpp>

namespace ores::utility::uuid {

/**
 * @brief String representation of the nil UUID (all zeros).
 *
 * The nil UUID is the default value for a default-constructed
 * boost::uuids::uuid. This constant is used to detect uninitialized or
 * legacy tenant IDs.
 */
inline constexpr char nil_uuid_str[] = "00000000-0000-0000-0000-000000000000";

/**
 * @brief String representation of the max UUID (all ones).
 *
 * The max UUID is defined in RFC 9562 as a sentinel value. It is used
 * as the system tenant identifier because it cannot be accidentally
 * created through default construction.
 */
inline constexpr char max_uuid_str[] = "ffffffff-ffff-ffff-ffff-ffffffffffff";

/**
 * @class tenant_id
 * @brief A strongly-typed wrapper around a UUID representing a tenant identifier.
 *
 * @details This class provides type safety for tenant identifiers by:
 * - Preventing accidental use of nil UUIDs (default boost::uuids::uuid value)
 * - Providing factory methods that enforce valid construction
 * - Using the max UUID (RFC 9562) as the system tenant identifier
 *
 * The class is designed to make it impossible to accidentally create an
 * uninitialized tenant ID that would be confused with the system tenant.
 *
 * @note Construction is only possible through factory methods. Direct
 * construction is not allowed.
 *
 * @see <a href="https://www.rfc-editor.org/rfc/rfc9562.html">RFC 9562 - UUIDs</a>
 */
class tenant_id final {
public:
    /**
     * @brief Creates a tenant_id representing the system tenant.
     *
     * @details The system tenant uses the max UUID (all ones) as defined in
     * RFC 9562. This value cannot be accidentally created through default
     * construction.
     *
     * @return A tenant_id representing the system tenant.
     */
    static tenant_id system();

    /**
     * @brief Creates a tenant_id from a boost UUID.
     *
     * @details This factory method validates the UUID and rejects nil UUIDs
     * to prevent accidental use of uninitialized values.
     *
     * @param uuid The UUID to create the tenant_id from.
     * @return A tenant_id if successful, or an error message if the UUID is nil.
     */
    static std::expected<tenant_id, std::string>
    from_uuid(const boost::uuids::uuid& uuid);

    /**
     * @brief Creates a tenant_id from a string representation.
     *
     * @details This factory method parses the string as a UUID and validates
     * it. Nil UUIDs are rejected.
     *
     * @param str The string representation of the UUID.
     * @return A tenant_id if successful, or an error message if parsing fails
     *         or the UUID is nil.
     */
    static std::expected<tenant_id, std::string>
    from_string(std::string_view str);

    /**
     * @brief Checks if this tenant_id represents the system tenant.
     *
     * @return true if this is the system tenant (max UUID), false otherwise.
     */
    bool is_system() const noexcept;

    /**
     * @brief Checks if the underlying UUID is nil.
     *
     * @details This method is useful for detecting legacy or uninitialized
     * tenant IDs during migration. Under normal circumstances, a tenant_id
     * should never be nil since factory methods reject nil UUIDs.
     *
     * @return true if the underlying UUID is nil, false otherwise.
     */
    bool is_nil() const noexcept;

    /**
     * @brief Returns the underlying boost UUID.
     *
     * @return A const reference to the underlying UUID.
     */
    const boost::uuids::uuid& to_uuid() const noexcept;

    /**
     * @brief Converts the tenant_id to its string representation.
     *
     * @return The string representation of the underlying UUID.
     */
    std::string to_string() const;

    /**
     * @brief Equality comparison operator.
     */
    bool operator==(const tenant_id& other) const noexcept = default;

private:
    /**
     * @brief Private constructor to enforce factory method usage.
     *
     * @param uuid The UUID to wrap.
     */
    explicit tenant_id(boost::uuids::uuid uuid);

    boost::uuids::uuid uuid_;
};

}

#endif
