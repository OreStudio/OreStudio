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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_API_MESSAGING_RESET_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_RESET_PROTOCOL_HPP

#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::iam::messaging {

// Reset a single tenant to bootstrap state.
// Soft-deletes admin-created data, flips Operational parties to Inactive,
// and re-enables system.bootstrap_mode so both TenantProvisioningWizard
// and PartyProvisioningWizard fire on next login.
// Requires: iam::system:reset-tenant permission (SuperAdmin only).
struct reset_tenant_command {
    using response_type = struct reset_tenant_result;
    static constexpr std::string_view nats_subject = "iam.v1.ops.reset_tenant";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string tenant_code;

    friend bool operator==(const reset_tenant_command&, const reset_tenant_command&) = default;
};

struct reset_tenant_result {
    bool success = false;
    std::string message;

    friend bool operator==(const reset_tenant_result&, const reset_tenant_result&) = default;
};

// Reset the entire system to pre-bootstrap state.
// Purges all non-system tenants (hard delete), removes system admin accounts,
// and re-enables system.bootstrap_mode so SystemProvisionerWizard fires on
// next startup.
// Requires: iam::system:reset permission (SuperAdmin only).
struct reset_system_command {
    using response_type = struct reset_system_result;
    static constexpr std::string_view nats_subject = "iam.v1.ops.reset_system";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const reset_system_command&, const reset_system_command&) = default;
};

struct reset_system_result {
    bool success = false;
    std::string message;

    friend bool operator==(const reset_system_result&, const reset_system_result&) = default;
};

/**
 * @brief The members of reset_tenant_command, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<reset_tenant_command>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"tenant_code", &reset_tenant_command::tenant_code},
    };
}

/**
 * @brief The members of reset_tenant_result, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<reset_tenant_result>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &reset_tenant_result::success},
        ::ores::utility::codec::field{"message", &reset_tenant_result::message},
    };
}

/**
 * @brief The members of reset_system_command, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<reset_system_command>) {
    return ::ores::utility::codec::field_list{};
}

/**
 * @brief The members of reset_system_result, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<reset_system_result>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &reset_system_result::success},
        ::ores::utility::codec::field{"message", &reset_system_result::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::reset_tenant_command> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::reset_tenant_command& v);
    static ores::iam::messaging::reset_tenant_command read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::reset_tenant_command& v);
    static ores::iam::messaging::reset_tenant_command read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::reset_tenant_result> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::reset_tenant_result& v);
    static ores::iam::messaging::reset_tenant_result read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::reset_tenant_result& v);
    static ores::iam::messaging::reset_tenant_result read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::reset_system_command> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::reset_system_command& v);
    static ores::iam::messaging::reset_system_command read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::reset_system_command& v);
    static ores::iam::messaging::reset_system_command read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::reset_system_result> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::reset_system_result& v);
    static ores::iam::messaging::reset_system_result read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::reset_system_result& v);
    static ores::iam::messaging::reset_system_result read_msgpack(std::span<const std::byte> data);
};
}


#endif
