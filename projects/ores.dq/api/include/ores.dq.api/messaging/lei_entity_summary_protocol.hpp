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
#ifndef ORES_DQ_API_MESSAGING_LEI_ENTITY_SUMMARY_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_LEI_ENTITY_SUMMARY_PROTOCOL_HPP

#include "ores.dq.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::dq::messaging {

/**
 * @brief One legal entity, reduced to the fields the shell lists it by.
 */
struct lei_entity_summary {
    /**
     * @brief The entity's LEI.
     */
    std::string lei;
    /**
     * @brief The entity's registered legal name.
     */
    std::string entity_legal_name;
    /**
     * @brief The entity's category, as the registry classifies it.
     */
    std::string entity_category;
    /**
     * @brief The country of the entity's legal address.
     */
    std::string country;

    friend bool operator==(const lei_entity_summary&, const lei_entity_summary&) = default;
};

/**
 * @brief Asks for the active root legal entities, optionally of one country.
 */
struct get_lei_entities_summary_request {
    using response_type = struct get_lei_entities_summary_response;
    static constexpr std::string_view nats_subject = "dq.v1.ops.get_lei_entities_summary";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The country to list, or empty for every country's count.
     */
    std::string country_filter = "";
    /**
     * @brief How many rows to skip.
     */
    int offset = 0;
    /**
     * @brief How many rows to return.
     */
    int limit = 1000;

    friend bool operator==(const get_lei_entities_summary_request&,
                           const get_lei_entities_summary_request&) = default;
};

/**
 * @brief Reports the summary rows, or why they could not be read.
 */
struct get_lei_entities_summary_response {
    /**
     * @brief Whether the read completed.
     */
    bool success = false;
    /**
     * @brief Why it failed, when it did.
     */
    std::string error_message;
    /**
     * @brief The summarised entities.
     */
    std::vector<lei_entity_summary> entities;

    friend bool operator==(const get_lei_entities_summary_response&,
                           const get_lei_entities_summary_response&) = default;
};

/**
 * @brief One legal entity a search matched, and the work it would bring.
 */
struct lei_entity_match {
    /**
     * @brief The entity's LEI.
     */
    std::string lei;
    /**
     * @brief The entity's registered legal name.
     */
    std::string entity_legal_name;
    /**
     * @brief The entity's category, as the registry classifies it.
     */
    std::string entity_category;
    /**
     * @brief The country of the entity's legal address.
     */
    std::string country;
    /**
     * @brief How many parties importing this entity's hierarchy would create,
     * counting the entity itself.
     *
     * A person choosing the entity a tenant is built around is choosing that much
     * work, so the match states it before the choice is made. It is the size of the
     * hierarchy the deployment holds under the entity, which is what the
     * publication walks.
     */
    std::int64_t party_count = 0;

    friend bool operator==(const lei_entity_match&, const lei_entity_match&) = default;
};

/**
 * @brief Asks for the root legal entities that match a search.
 *
 * The read a search uses, as against the country browser: a name or an LEI is
 * what somebody looking for a particular entity knows, and the deployment holds
 * far more entities than one answer can carry.
 */
struct search_lei_entities_request {
    using response_type = struct search_lei_entities_response;
    static constexpr std::string_view nats_subject = "dq.v1.ops.search_lei_entities";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The text to match, or empty for every entity.
     *
     * A name matches anywhere in the legal name; an LEI matches from its start.
     */
    std::string search;
    /**
     * @brief The country to restrict the matches to, or empty for every country.
     */
    std::string country_filter = "";
    /**
     * @brief How many matches to skip.
     */
    int offset = 0;
    /**
     * @brief How many matches to return.
     */
    int limit = 20;

    friend bool operator==(const search_lei_entities_request&,
                           const search_lei_entities_request&) = default;
};

/**
 * @brief Reports the matches, or why they could not be read.
 */
struct search_lei_entities_response {
    /**
     * @brief Whether the read completed.
     */
    bool success = false;
    /**
     * @brief Why it failed, when it did.
     */
    std::string error_message;
    /**
     * @brief The matches.
     */
    std::vector<lei_entity_match> entities;

    friend bool operator==(const search_lei_entities_response&,
                           const search_lei_entities_response&) = default;
};

/**
 * @brief The members of lei_entity_summary, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<lei_entity_summary>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"lei", &lei_entity_summary::lei},
        ::ores::utility::codec::field{"entity_legal_name", &lei_entity_summary::entity_legal_name},
        ::ores::utility::codec::field{"entity_category", &lei_entity_summary::entity_category},
        ::ores::utility::codec::field{"country", &lei_entity_summary::country},
    };
}

/**
 * @brief The members of get_lei_entities_summary_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_lei_entities_summary_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"country_filter",
                                      &get_lei_entities_summary_request::country_filter},
        ::ores::utility::codec::field{"offset", &get_lei_entities_summary_request::offset},
        ::ores::utility::codec::field{"limit", &get_lei_entities_summary_request::limit},
    };
}

/**
 * @brief The members of get_lei_entities_summary_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_lei_entities_summary_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &get_lei_entities_summary_response::success},
        ::ores::utility::codec::field{"error_message",
                                      &get_lei_entities_summary_response::error_message},
        ::ores::utility::codec::field{"entities", &get_lei_entities_summary_response::entities},
    };
}

/**
 * @brief The members of lei_entity_match, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<lei_entity_match>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"lei", &lei_entity_match::lei},
        ::ores::utility::codec::field{"entity_legal_name", &lei_entity_match::entity_legal_name},
        ::ores::utility::codec::field{"entity_category", &lei_entity_match::entity_category},
        ::ores::utility::codec::field{"country", &lei_entity_match::country},
        ::ores::utility::codec::field{"party_count", &lei_entity_match::party_count},
    };
}

/**
 * @brief The members of search_lei_entities_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<search_lei_entities_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"search", &search_lei_entities_request::search},
        ::ores::utility::codec::field{"country_filter",
                                      &search_lei_entities_request::country_filter},
        ::ores::utility::codec::field{"offset", &search_lei_entities_request::offset},
        ::ores::utility::codec::field{"limit", &search_lei_entities_request::limit},
    };
}

/**
 * @brief The members of search_lei_entities_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<search_lei_entities_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &search_lei_entities_response::success},
        ::ores::utility::codec::field{"error_message",
                                      &search_lei_entities_response::error_message},
        ::ores::utility::codec::field{"entities", &search_lei_entities_response::entities},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::lei_entity_summary> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::lei_entity_summary& v);
    static ores::dq::messaging::lei_entity_summary read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::lei_entity_summary& v);
    static ores::dq::messaging::lei_entity_summary read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_lei_entities_summary_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_lei_entities_summary_request& v);
    static ores::dq::messaging::get_lei_entities_summary_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_lei_entities_summary_request& v);
    static ores::dq::messaging::get_lei_entities_summary_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_lei_entities_summary_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_lei_entities_summary_response& v);
    static ores::dq::messaging::get_lei_entities_summary_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_lei_entities_summary_response& v);
    static ores::dq::messaging::get_lei_entities_summary_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::lei_entity_match> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::lei_entity_match& v);
    static ores::dq::messaging::lei_entity_match read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::lei_entity_match& v);
    static ores::dq::messaging::lei_entity_match read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::search_lei_entities_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::search_lei_entities_request& v);
    static ores::dq::messaging::search_lei_entities_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::search_lei_entities_request& v);
    static ores::dq::messaging::search_lei_entities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::search_lei_entities_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::search_lei_entities_response& v);
    static ores::dq::messaging::search_lei_entities_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::search_lei_entities_response& v);
    static ores::dq::messaging::search_lei_entities_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
