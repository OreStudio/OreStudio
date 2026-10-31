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
#ifndef ORES_REFDATA_API_MESSAGING_COUNTERPARTY_CONTACT_INFORMATION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COUNTERPARTY_CONTACT_INFORMATION_PROTOCOL_HPP

#include "ores.refdata.api/domain/counterparty_contact_information.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::refdata::messaging {

struct counterparty_contact_information_key {
    std::string contact_type;
};

struct counterparty_contact_information_write {
    boost::uuids::uuid id;
    boost::uuids::uuid counterparty_id;
    std::string contact_type;
    std::string street_line_1;
    std::string street_line_2;
    std::string city;
    std::string state;
    std::string country_code;
    std::string postal_code;
    std::string phone;
    std::string email;
    std::string web_page;
};

struct counterparty_contact_information_change {
    counterparty_contact_information_write write;
    ores::utility::domain::precondition precondition;
};

struct counterparty_contact_information_removal {
    counterparty_contact_information_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct counterparty_contact_information_lookup {
    counterparty_contact_information_key key;
    std::optional<ores::refdata::domain::counterparty_contact_information>
        counterparty_contact_information;
};

struct counterparty_contact_informations_filter {
    std::optional<boost::uuids::uuid> counterparty_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> counterparty_id_one_of;
};

struct counterparty_contact_information_event {
    boost::uuids::uuid event_id;
    counterparty_contact_information_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct counterparty_contact_information_version_key {
    counterparty_contact_information_key counterparty_contact_information;
    std::uint32_t version;
};

struct counterparty_contact_information_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_counterparty_contact_informations_request {
    using response_type = struct list_counterparty_contact_informations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_contact_informations.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<counterparty_contact_informations_filter> filter;
    std::optional<std::string> as_of;
};

struct list_counterparty_contact_informations_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_contact_information>
        counterparty_contact_informations;
    std::uint64_t total;
};

struct get_counterparty_contact_information_request {
    using response_type = struct get_counterparty_contact_information_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_contact_informations.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_contact_information_key key;
};

struct get_counterparty_contact_information_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty_contact_information>
        counterparty_contact_information;
};

struct get_many_counterparty_contact_informations_request {
    using response_type = struct get_many_counterparty_contact_informations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_contact_informations.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_contact_information_key> keys;
};

struct get_many_counterparty_contact_informations_response {
    ores::utility::domain::result result;
    std::vector<counterparty_contact_information_lookup> entries;
};

struct put_counterparty_contact_information_request {
    using response_type = struct put_counterparty_contact_information_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_contact_informations.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_contact_information_change change;
    ores::utility::domain::change_intent intent;
};

struct put_counterparty_contact_information_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty_contact_information>
        counterparty_contact_information;
};

struct put_many_counterparty_contact_informations_request {
    using response_type = struct put_many_counterparty_contact_informations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_contact_informations.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_contact_information_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_counterparty_contact_informations_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_contact_information>
        counterparty_contact_informations;
};

struct delete_counterparty_contact_information_request {
    using response_type = struct delete_counterparty_contact_information_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_contact_informations.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_contact_information_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_counterparty_contact_information_response {
    ores::utility::domain::result result;
};

struct delete_many_counterparty_contact_informations_request {
    using response_type = struct delete_many_counterparty_contact_informations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_contact_informations.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_contact_information_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_counterparty_contact_informations_response {
    ores::utility::domain::result result;
};

struct list_by_counterparty_id_counterparty_contact_informations_request {
    using response_type = struct list_by_counterparty_id_counterparty_contact_informations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_contact_informations.list_by_counterparty_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid counterparty_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<counterparty_contact_informations_filter> filter;
};

struct list_by_counterparty_id_counterparty_contact_informations_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_contact_information>
        counterparty_contact_informations;
    std::uint64_t total;
};

struct list_counterparty_contact_information_versions_request {
    using response_type = struct list_counterparty_contact_information_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_contact_informations_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_contact_information_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<counterparty_contact_information_versions_filter> filter;
};

struct list_counterparty_contact_information_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_contact_information> versions;
    std::uint64_t total;
};

struct get_counterparty_contact_information_version_request {
    using response_type = struct get_counterparty_contact_information_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_contact_informations_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_contact_information_version_key key;
};

struct get_counterparty_contact_information_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty_contact_information> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace counterparty_contact_information_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.counterparty_contact_informations_events.created";
inline constexpr std::string_view updated =
    "refdata.v1.counterparty_contact_informations_events.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.counterparty_contact_informations_events.deleted";
}
/**
 * @brief The members of counterparty_contact_information_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_contact_information_key&) {
    return std::tuple{
        utility::codec::field{"contact_type", &counterparty_contact_information_key::contact_type},
    };
}

/**
 * @brief The members of counterparty_contact_information_write, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_contact_information_write&) {
    return std::tuple{
        utility::codec::field{"id", &counterparty_contact_information_write::id},
        utility::codec::field{"counterparty_id",
                              &counterparty_contact_information_write::counterparty_id},
        utility::codec::field{"contact_type",
                              &counterparty_contact_information_write::contact_type},
        utility::codec::field{"street_line_1",
                              &counterparty_contact_information_write::street_line_1},
        utility::codec::field{"street_line_2",
                              &counterparty_contact_information_write::street_line_2},
        utility::codec::field{"city", &counterparty_contact_information_write::city},
        utility::codec::field{"state", &counterparty_contact_information_write::state},
        utility::codec::field{"country_code",
                              &counterparty_contact_information_write::country_code},
        utility::codec::field{"postal_code", &counterparty_contact_information_write::postal_code},
        utility::codec::field{"phone", &counterparty_contact_information_write::phone},
        utility::codec::field{"email", &counterparty_contact_information_write::email},
        utility::codec::field{"web_page", &counterparty_contact_information_write::web_page},
    };
}

/**
 * @brief The members of counterparty_contact_information_change, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_contact_information_change&) {
    return std::tuple{
        utility::codec::field{"write", &counterparty_contact_information_change::write},
        utility::codec::field{"precondition",
                              &counterparty_contact_information_change::precondition},
    };
}

/**
 * @brief The members of counterparty_contact_information_removal, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_contact_information_removal&) {
    return std::tuple{
        utility::codec::field{"key", &counterparty_contact_information_removal::key},
        utility::codec::field{"precondition",
                              &counterparty_contact_information_removal::precondition},
    };
}

/**
 * @brief The members of counterparty_contact_information_lookup, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_contact_information_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &counterparty_contact_information_lookup::key},
        utility::codec::field{
            "counterparty_contact_information",
            &counterparty_contact_information_lookup::counterparty_contact_information},
    };
}

/**
 * @brief The members of counterparty_contact_informations_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_contact_informations_filter&) {
    return std::tuple{
        utility::codec::field{"counterparty_id",
                              &counterparty_contact_informations_filter::counterparty_id},
        utility::codec::field{"id_one_of", &counterparty_contact_informations_filter::id_one_of},
        utility::codec::field{"counterparty_id_one_of",
                              &counterparty_contact_informations_filter::counterparty_id_one_of},
    };
}

/**
 * @brief The members of counterparty_contact_information_event, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_contact_information_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &counterparty_contact_information_event::event_id},
        utility::codec::field{"key", &counterparty_contact_information_event::key},
        utility::codec::field{"action", &counterparty_contact_information_event::action},
        utility::codec::field{"version", &counterparty_contact_information_event::version},
        utility::codec::field{"occurred_at", &counterparty_contact_information_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &counterparty_contact_information_event::correlation_id},
    };
}

/**
 * @brief The members of counterparty_contact_information_version_key, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_contact_information_version_key&) {
    return std::tuple{
        utility::codec::field{
            "counterparty_contact_information",
            &counterparty_contact_information_version_key::counterparty_contact_information},
        utility::codec::field{"version", &counterparty_contact_information_version_key::version},
    };
}

/**
 * @brief The members of counterparty_contact_information_versions_filter, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_contact_information_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version",
                              &counterparty_contact_information_versions_filter::version},
        utility::codec::field{"from_version",
                              &counterparty_contact_information_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &counterparty_contact_information_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_counterparty_contact_informations_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_counterparty_contact_informations_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_counterparty_contact_informations_request::offset},
        utility::codec::field{"limit", &list_counterparty_contact_informations_request::limit},
        utility::codec::field{"order", &list_counterparty_contact_informations_request::order},
        utility::codec::field{"filter", &list_counterparty_contact_informations_request::filter},
        utility::codec::field{"as_of", &list_counterparty_contact_informations_request::as_of},
    };
}

/**
 * @brief The members of list_counterparty_contact_informations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_counterparty_contact_informations_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_counterparty_contact_informations_response::result},
        utility::codec::field{
            "counterparty_contact_informations",
            &list_counterparty_contact_informations_response::counterparty_contact_informations},
        utility::codec::field{"total", &list_counterparty_contact_informations_response::total},
    };
}

/**
 * @brief The members of get_counterparty_contact_information_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_counterparty_contact_information_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_counterparty_contact_information_request::key},
    };
}

/**
 * @brief The members of get_counterparty_contact_information_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_counterparty_contact_information_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_counterparty_contact_information_response::result},
        utility::codec::field{
            "counterparty_contact_information",
            &get_counterparty_contact_information_response::counterparty_contact_information},
    };
}

/**
 * @brief The members of get_many_counterparty_contact_informations_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_many_counterparty_contact_informations_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_counterparty_contact_informations_request::keys},
    };
}

/**
 * @brief The members of get_many_counterparty_contact_informations_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_many_counterparty_contact_informations_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_many_counterparty_contact_informations_response::result},
        utility::codec::field{"entries",
                              &get_many_counterparty_contact_informations_response::entries},
    };
}

/**
 * @brief The members of put_counterparty_contact_information_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_counterparty_contact_information_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_counterparty_contact_information_request::change},
        utility::codec::field{"intent", &put_counterparty_contact_information_request::intent},
    };
}

/**
 * @brief The members of put_counterparty_contact_information_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_counterparty_contact_information_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_counterparty_contact_information_response::result},
        utility::codec::field{
            "counterparty_contact_information",
            &put_counterparty_contact_information_response::counterparty_contact_information},
    };
}

/**
 * @brief The members of put_many_counterparty_contact_informations_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const put_many_counterparty_contact_informations_request&) {
    return std::tuple{
        utility::codec::field{"changes",
                              &put_many_counterparty_contact_informations_request::changes},
        utility::codec::field{"intent",
                              &put_many_counterparty_contact_informations_request::intent},
    };
}

/**
 * @brief The members of put_many_counterparty_contact_informations_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const put_many_counterparty_contact_informations_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &put_many_counterparty_contact_informations_response::result},
        utility::codec::field{"counterparty_contact_informations",
                              &put_many_counterparty_contact_informations_response::
                                  counterparty_contact_informations},
    };
}

/**
 * @brief The members of delete_counterparty_contact_information_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_counterparty_contact_information_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_counterparty_contact_information_request::removal},
        utility::codec::field{"intent", &delete_counterparty_contact_information_request::intent},
    };
}

/**
 * @brief The members of delete_counterparty_contact_information_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_counterparty_contact_information_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_counterparty_contact_information_response::result},
    };
}

/**
 * @brief The members of delete_many_counterparty_contact_informations_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_counterparty_contact_informations_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_counterparty_contact_informations_request::removals},
        utility::codec::field{"intent",
                              &delete_many_counterparty_contact_informations_request::intent},
    };
}

/**
 * @brief The members of delete_many_counterparty_contact_informations_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const delete_many_counterparty_contact_informations_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &delete_many_counterparty_contact_informations_response::result},
    };
}

/**
 * @brief The members of list_by_counterparty_id_counterparty_contact_informations_request, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_counterparty_id_counterparty_contact_informations_request&) {
    return std::tuple{
        utility::codec::field{
            "counterparty_id",
            &list_by_counterparty_id_counterparty_contact_informations_request::counterparty_id},
        utility::codec::field{
            "scope", &list_by_counterparty_id_counterparty_contact_informations_request::scope},
        utility::codec::field{
            "offset", &list_by_counterparty_id_counterparty_contact_informations_request::offset},
        utility::codec::field{
            "limit", &list_by_counterparty_id_counterparty_contact_informations_request::limit},
        utility::codec::field{
            "order", &list_by_counterparty_id_counterparty_contact_informations_request::order},
        utility::codec::field{
            "filter", &list_by_counterparty_id_counterparty_contact_informations_request::filter},
    };
}

/**
 * @brief The members of list_by_counterparty_id_counterparty_contact_informations_response, named
 * as the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_counterparty_id_counterparty_contact_informations_response&) {
    return std::tuple{
        utility::codec::field{
            "result", &list_by_counterparty_id_counterparty_contact_informations_response::result},
        utility::codec::field{"counterparty_contact_informations",
                              &list_by_counterparty_id_counterparty_contact_informations_response::
                                  counterparty_contact_informations},
        utility::codec::field{
            "total", &list_by_counterparty_id_counterparty_contact_informations_response::total},
    };
}

/**
 * @brief The members of list_counterparty_contact_information_versions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_counterparty_contact_information_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_counterparty_contact_information_versions_request::key},
        utility::codec::field{"offset",
                              &list_counterparty_contact_information_versions_request::offset},
        utility::codec::field{"limit",
                              &list_counterparty_contact_information_versions_request::limit},
        utility::codec::field{"order",
                              &list_counterparty_contact_information_versions_request::order},
        utility::codec::field{"filter",
                              &list_counterparty_contact_information_versions_request::filter},
    };
}

/**
 * @brief The members of list_counterparty_contact_information_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_counterparty_contact_information_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_counterparty_contact_information_versions_response::result},
        utility::codec::field{"versions",
                              &list_counterparty_contact_information_versions_response::versions},
        utility::codec::field{"total",
                              &list_counterparty_contact_information_versions_response::total},
    };
}

/**
 * @brief The members of get_counterparty_contact_information_version_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_counterparty_contact_information_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_counterparty_contact_information_version_request::key},
    };
}

/**
 * @brief The members of get_counterparty_contact_information_version_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_counterparty_contact_information_version_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_counterparty_contact_information_version_response::result},
        utility::codec::field{"version",
                              &get_counterparty_contact_information_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_contact_information_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_contact_information_key& v);
    static ores::refdata::messaging::counterparty_contact_information_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_contact_information_key& v);
    static ores::refdata::messaging::counterparty_contact_information_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_contact_information_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_contact_information_write& v);
    static ores::refdata::messaging::counterparty_contact_information_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_contact_information_write& v);
    static ores::refdata::messaging::counterparty_contact_information_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_contact_information_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_contact_information_change& v);
    static ores::refdata::messaging::counterparty_contact_information_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_contact_information_change& v);
    static ores::refdata::messaging::counterparty_contact_information_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_contact_information_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_contact_information_removal& v);
    static ores::refdata::messaging::counterparty_contact_information_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_contact_information_removal& v);
    static ores::refdata::messaging::counterparty_contact_information_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_contact_information_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_contact_information_lookup& v);
    static ores::refdata::messaging::counterparty_contact_information_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_contact_information_lookup& v);
    static ores::refdata::messaging::counterparty_contact_information_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_contact_informations_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_contact_informations_filter& v);
    static ores::refdata::messaging::counterparty_contact_informations_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_contact_informations_filter& v);
    static ores::refdata::messaging::counterparty_contact_informations_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_contact_information_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_contact_information_event& v);
    static ores::refdata::messaging::counterparty_contact_information_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_contact_information_event& v);
    static ores::refdata::messaging::counterparty_contact_information_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_contact_information_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_contact_information_version_key& v);
    static ores::refdata::messaging::counterparty_contact_information_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_contact_information_version_key& v);
    static ores::refdata::messaging::counterparty_contact_information_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_contact_information_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_contact_information_versions_filter& v);
    static ores::refdata::messaging::counterparty_contact_information_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::counterparty_contact_information_versions_filter& v);
    static ores::refdata::messaging::counterparty_contact_information_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_contact_informations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_counterparty_contact_informations_request& v);
    static ores::refdata::messaging::list_counterparty_contact_informations_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_counterparty_contact_informations_request& v);
    static ores::refdata::messaging::list_counterparty_contact_informations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_contact_informations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_counterparty_contact_informations_response& v);
    static ores::refdata::messaging::list_counterparty_contact_informations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_counterparty_contact_informations_response& v);
    static ores::refdata::messaging::list_counterparty_contact_informations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_contact_information_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_contact_information_request& v);
    static ores::refdata::messaging::get_counterparty_contact_information_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_contact_information_request& v);
    static ores::refdata::messaging::get_counterparty_contact_information_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_contact_information_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_contact_information_response& v);
    static ores::refdata::messaging::get_counterparty_contact_information_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_contact_information_response& v);
    static ores::refdata::messaging::get_counterparty_contact_information_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_counterparty_contact_informations_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_many_counterparty_contact_informations_request& v);
    static ores::refdata::messaging::get_many_counterparty_contact_informations_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_counterparty_contact_informations_request& v);
    static ores::refdata::messaging::get_many_counterparty_contact_informations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_counterparty_contact_informations_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_many_counterparty_contact_informations_response& v);
    static ores::refdata::messaging::get_many_counterparty_contact_informations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_counterparty_contact_informations_response& v);
    static ores::refdata::messaging::get_many_counterparty_contact_informations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_counterparty_contact_information_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_counterparty_contact_information_request& v);
    static ores::refdata::messaging::put_counterparty_contact_information_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_counterparty_contact_information_request& v);
    static ores::refdata::messaging::put_counterparty_contact_information_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_counterparty_contact_information_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_counterparty_contact_information_response& v);
    static ores::refdata::messaging::put_counterparty_contact_information_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_counterparty_contact_information_response& v);
    static ores::refdata::messaging::put_counterparty_contact_information_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_counterparty_contact_informations_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::put_many_counterparty_contact_informations_request& v);
    static ores::refdata::messaging::put_many_counterparty_contact_informations_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_counterparty_contact_informations_request& v);
    static ores::refdata::messaging::put_many_counterparty_contact_informations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_counterparty_contact_informations_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::put_many_counterparty_contact_informations_response& v);
    static ores::refdata::messaging::put_many_counterparty_contact_informations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::put_many_counterparty_contact_informations_response& v);
    static ores::refdata::messaging::put_many_counterparty_contact_informations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_counterparty_contact_information_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_counterparty_contact_information_request& v);
    static ores::refdata::messaging::delete_counterparty_contact_information_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_counterparty_contact_information_request& v);
    static ores::refdata::messaging::delete_counterparty_contact_information_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_counterparty_contact_information_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_counterparty_contact_information_response& v);
    static ores::refdata::messaging::delete_counterparty_contact_information_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_counterparty_contact_information_response& v);
    static ores::refdata::messaging::delete_counterparty_contact_information_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_counterparty_contact_informations_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_counterparty_contact_informations_request& v);
    static ores::refdata::messaging::delete_many_counterparty_contact_informations_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_counterparty_contact_informations_request& v);
    static ores::refdata::messaging::delete_many_counterparty_contact_informations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_counterparty_contact_informations_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::delete_many_counterparty_contact_informations_response& v);
    static ores::refdata::messaging::delete_many_counterparty_contact_informations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::delete_many_counterparty_contact_informations_response& v);
    static ores::refdata::messaging::delete_many_counterparty_contact_informations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_counterparty_id_counterparty_contact_informations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_counterparty_id_counterparty_contact_informations_request& v);
    static ores::refdata::messaging::
        list_by_counterparty_id_counterparty_contact_informations_request
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_counterparty_id_counterparty_contact_informations_request& v);
    static ores::refdata::messaging::
        list_by_counterparty_id_counterparty_contact_informations_request
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_counterparty_id_counterparty_contact_informations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::
                   list_by_counterparty_id_counterparty_contact_informations_response& v);
    static ores::refdata::messaging::
        list_by_counterparty_id_counterparty_contact_informations_response
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::
                      list_by_counterparty_id_counterparty_contact_informations_response& v);
    static ores::refdata::messaging::
        list_by_counterparty_id_counterparty_contact_informations_response
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_contact_information_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_counterparty_contact_information_versions_request& v);
    static ores::refdata::messaging::list_counterparty_contact_information_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_counterparty_contact_information_versions_request& v);
    static ores::refdata::messaging::list_counterparty_contact_information_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_contact_information_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_counterparty_contact_information_versions_response& v);
    static ores::refdata::messaging::list_counterparty_contact_information_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_counterparty_contact_information_versions_response& v);
    static ores::refdata::messaging::list_counterparty_contact_information_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_contact_information_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_counterparty_contact_information_version_request& v);
    static ores::refdata::messaging::get_counterparty_contact_information_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_counterparty_contact_information_version_request& v);
    static ores::refdata::messaging::get_counterparty_contact_information_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_contact_information_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_counterparty_contact_information_version_response& v);
    static ores::refdata::messaging::get_counterparty_contact_information_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_counterparty_contact_information_version_response& v);
    static ores::refdata::messaging::get_counterparty_contact_information_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
