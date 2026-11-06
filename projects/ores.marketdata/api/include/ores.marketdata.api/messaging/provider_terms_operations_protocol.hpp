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
#ifndef ORES_MARKETDATA_API_MESSAGING_PROVIDER_TERMS_OPERATIONS_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_PROVIDER_TERMS_OPERATIONS_PROTOCOL_HPP

#include "ores.marketdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <chrono>
#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::marketdata::messaging {

/**
 * @brief Accepts the terms of a provider as they stand.
 *
 * The caller names the provider and nothing else: what is accepted is the terms
 * the service holds, by whom is the session's account, and when is the service's
 * clock. Terms that change afterwards no longer match, so they must be accepted
 * again.
 */
struct accept_provider_terms_request {
    using response_type = struct accept_provider_terms_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.accept_provider_terms";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The provider whose terms are accepted.
     */
    std::string provider_code;
    /**
     * @brief The version of the provider the person read, or zero for the latest.
     *
     * A screen sends the version it showed, so the terms accepted are the terms the
     * person read: when the provider changed in between, the acceptance is refused
     * and the person reads them again. Zero accepts the terms as they stand, which is
     * what a person at the shell does after reading the provider.
     */
    int provider_version = 0;

    friend bool operator==(const accept_provider_terms_request&,
                           const accept_provider_terms_request&) = default;
};

struct accept_provider_terms_response {
    /**
     * @brief Whether the acceptance was recorded.
     */
    bool success = false;
    /**
     * @brief Why it was not, or what was recorded.
     */
    std::string message;
    /**
     * @brief The account recorded as accepting.
     */
    std::string accepted_by;
    /**
     * @brief When the acceptance was recorded.
     */
    std::optional<std::chrono::system_clock::time_point> accepted_at;

    friend bool operator==(const accept_provider_terms_response&,
                           const accept_provider_terms_response&) = default;
};

/**
 * @brief Withdraws the acceptance of a provider's terms, so the adapter no longer fetches it.
 *
 * A system administrator who declines a provider, or changes their mind, withdraws its
 * acceptance. The service clears the digest, the account and the time; a provider that was
 * never accepted is left as it is.
 */
struct withdraw_provider_terms_request {
    using response_type = struct withdraw_provider_terms_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.withdraw_provider_terms";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The provider whose acceptance is withdrawn.
     */
    std::string provider_code;

    friend bool operator==(const withdraw_provider_terms_request&,
                           const withdraw_provider_terms_request&) = default;
};

struct withdraw_provider_terms_response {
    /**
     * @brief Whether the provider is now not accepted.
     */
    bool success = false;
    /**
     * @brief Why it was not, or what was recorded.
     */
    std::string message;

    friend bool operator==(const withdraw_provider_terms_response&,
                           const withdraw_provider_terms_response&) = default;
};

/**
 * @brief The members of accept_provider_terms_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<accept_provider_terms_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"provider_code",
                                      &accept_provider_terms_request::provider_code},
        ::ores::utility::codec::field{"provider_version",
                                      &accept_provider_terms_request::provider_version},
    };
}

/**
 * @brief The members of accept_provider_terms_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<accept_provider_terms_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &accept_provider_terms_response::success},
        ::ores::utility::codec::field{"message", &accept_provider_terms_response::message},
        ::ores::utility::codec::field{"accepted_by", &accept_provider_terms_response::accepted_by},
        ::ores::utility::codec::field{"accepted_at", &accept_provider_terms_response::accepted_at},
    };
}

/**
 * @brief The members of withdraw_provider_terms_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<withdraw_provider_terms_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"provider_code",
                                      &withdraw_provider_terms_request::provider_code},
    };
}

/**
 * @brief The members of withdraw_provider_terms_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<withdraw_provider_terms_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &withdraw_provider_terms_response::success},
        ::ores::utility::codec::field{"message", &withdraw_provider_terms_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::accept_provider_terms_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::accept_provider_terms_request& v);
    static ores::marketdata::messaging::accept_provider_terms_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::accept_provider_terms_request& v);
    static ores::marketdata::messaging::accept_provider_terms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::accept_provider_terms_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::accept_provider_terms_response& v);
    static ores::marketdata::messaging::accept_provider_terms_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::accept_provider_terms_response& v);
    static ores::marketdata::messaging::accept_provider_terms_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::withdraw_provider_terms_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::withdraw_provider_terms_request& v);
    static ores::marketdata::messaging::withdraw_provider_terms_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::withdraw_provider_terms_request& v);
    static ores::marketdata::messaging::withdraw_provider_terms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::withdraw_provider_terms_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::withdraw_provider_terms_response& v);
    static ores::marketdata::messaging::withdraw_provider_terms_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::withdraw_provider_terms_response& v);
    static ores::marketdata::messaging::withdraw_provider_terms_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
