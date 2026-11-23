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
#ifndef ORES_MARKETDATA_API_MESSAGING_PARTY_DEMAND_OPERATIONS_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_PARTY_DEMAND_OPERATIONS_PROTOCOL_HPP

#include "ores.marketdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief What one route of the party demands of its provider.
 */
struct demanded_route {
    /**
     * @brief The provider the route is fed by.
     */
    std::string provider_code;
    /**
     * @brief The name of the route's source, which is the id of its adapter.
     */
    std::string adapter_id;
    /**
     * @brief Whether the party may receive the provider's data.
     */
    bool entitled = false;
    /**
     * @brief Why the route is not entitled, such as not_entitled or terms_not_accepted.
     * Empty when it is.
     */
    std::string refusal;
    /**
     * @brief The items the route demands, as oresmd URIs, sorted. Empty when the
     * route is not entitled.
     */
    std::vector<std::string> items;

    friend bool operator==(const demanded_route&, const demanded_route&) = default;
};

/**
 * @brief Reads what one party of the caller's tenant demands of each of its routes.
 */
struct get_party_demand_request {
    using response_type = struct get_party_demand_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_party_demand";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The party whose routes are read.
     */
    std::string party_id;

    friend bool operator==(const get_party_demand_request&,
                           const get_party_demand_request&) = default;
};

/**
 * @brief The demand of each route of the party.
 */
struct get_party_demand_response {
    /**
     * @brief Whether the catalogue was read.
     */
    ores::utility::domain::result result;
    /**
     * @brief One entry for each route of the party whose source the catalogue holds. A
     * party with no route has none.
     */
    std::vector<demanded_route> routes;

    friend bool operator==(const get_party_demand_response&,
                           const get_party_demand_response&) = default;
};

/**
 * @brief The members of demanded_route, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<demanded_route>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"provider_code", &demanded_route::provider_code},
        ::ores::utility::codec::field{"adapter_id", &demanded_route::adapter_id},
        ::ores::utility::codec::field{"entitled", &demanded_route::entitled},
        ::ores::utility::codec::field{"refusal", &demanded_route::refusal},
        ::ores::utility::codec::field{"items", &demanded_route::items},
    };
}

/**
 * @brief The members of get_party_demand_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_party_demand_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"party_id", &get_party_demand_request::party_id},
    };
}

/**
 * @brief The members of get_party_demand_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_party_demand_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &get_party_demand_response::result},
        ::ores::utility::codec::field{"routes", &get_party_demand_response::routes},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::demanded_route> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::demanded_route& v);
    static ores::marketdata::messaging::demanded_route read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::demanded_route& v);
    static ores::marketdata::messaging::demanded_route
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_party_demand_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_party_demand_request& v);
    static ores::marketdata::messaging::get_party_demand_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_party_demand_request& v);
    static ores::marketdata::messaging::get_party_demand_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_party_demand_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_party_demand_response& v);
    static ores::marketdata::messaging::get_party_demand_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_party_demand_response& v);
    static ores::marketdata::messaging::get_party_demand_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
