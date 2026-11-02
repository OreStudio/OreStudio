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
#ifndef ORES_MARKETDATA_API_MESSAGING_PROVIDER_TERMS_LIST_OPERATIONS_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_PROVIDER_TERMS_LIST_OPERATIONS_PROTOCOL_HPP

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
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief One provider whose terms a person accepts, at the version they read.
 */
struct provider_terms_choice {
    /**
     * @brief The provider whose terms are accepted.
     */
    std::string provider_code;
    /**
     * @brief The version of the provider the person read, or zero for the latest.
     */
    int provider_version = 0;
};

/**
 * @brief What became of one provider in a list accept.
 */
struct provider_terms_outcome {
    /**
     * @brief The provider the outcome is for.
     */
    std::string provider_code;
    /**
     * @brief Whether its acceptance was recorded.
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
};

/**
 * @brief Accepts the terms of several providers in one request.
 *
 * Each provider is accepted as the single accept would accept it, with the same
 * checks, so one refusal does not stop the others: a setup that accepts every
 * public source, or a screen that accepts all it shows, learns which were recorded.
 */
struct accept_many_provider_terms_request {
    using response_type = struct accept_many_provider_terms_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.accept_many_provider_terms";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The providers whose terms are accepted, each at the version read.
     */
    std::vector<provider_terms_choice> providers;
};

/**
 * @brief One outcome a provider, in the order they were named.
 */
struct accept_many_provider_terms_response {
    /**
     * @brief Whether every provider's acceptance was recorded.
     */
    bool success = false;
    /**
     * @brief How many were recorded, and how many were not.
     */
    std::string message;
    /**
     * @brief What became of each provider, in the order they were named.
     */
    std::vector<provider_terms_outcome> outcomes;
};

/**
 * @brief The members of provider_terms_choice, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const provider_terms_choice&) {
    return std::tuple{
        ::ores::utility::codec::field{"provider_code", &provider_terms_choice::provider_code},
        ::ores::utility::codec::field{"provider_version", &provider_terms_choice::provider_version},
    };
}

/**
 * @brief The members of provider_terms_outcome, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const provider_terms_outcome&) {
    return std::tuple{
        ::ores::utility::codec::field{"provider_code", &provider_terms_outcome::provider_code},
        ::ores::utility::codec::field{"success", &provider_terms_outcome::success},
        ::ores::utility::codec::field{"message", &provider_terms_outcome::message},
        ::ores::utility::codec::field{"accepted_by", &provider_terms_outcome::accepted_by},
        ::ores::utility::codec::field{"accepted_at", &provider_terms_outcome::accepted_at},
    };
}

/**
 * @brief The members of accept_many_provider_terms_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const accept_many_provider_terms_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"providers", &accept_many_provider_terms_request::providers},
    };
}

/**
 * @brief The members of accept_many_provider_terms_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const accept_many_provider_terms_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &accept_many_provider_terms_response::success},
        ::ores::utility::codec::field{"message", &accept_many_provider_terms_response::message},
        ::ores::utility::codec::field{"outcomes", &accept_many_provider_terms_response::outcomes},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::provider_terms_choice> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::provider_terms_choice& v);
    static ores::marketdata::messaging::provider_terms_choice read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::provider_terms_choice& v);
    static ores::marketdata::messaging::provider_terms_choice
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::provider_terms_outcome> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::provider_terms_outcome& v);
    static ores::marketdata::messaging::provider_terms_outcome read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::provider_terms_outcome& v);
    static ores::marketdata::messaging::provider_terms_outcome
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::accept_many_provider_terms_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::accept_many_provider_terms_request& v);
    static ores::marketdata::messaging::accept_many_provider_terms_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::accept_many_provider_terms_request& v);
    static ores::marketdata::messaging::accept_many_provider_terms_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::accept_many_provider_terms_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::accept_many_provider_terms_response& v);
    static ores::marketdata::messaging::accept_many_provider_terms_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::accept_many_provider_terms_response& v);
    static ores::marketdata::messaging::accept_many_provider_terms_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
